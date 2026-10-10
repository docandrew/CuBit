pragma Ada_2022;

package body CuBit.GPU_Plane_Protocol with SPARK_Mode is
   use type DPL.Local_Coordinate;
   Half : constant Unsigned_64 := 2 ** 32;

   function Half_Word (Value : DPL.Local_Coordinate) return Unsigned_64 is
     (if Value >= 0 then Unsigned_64 (Value)
      else Half - Unsigned_64 (-Value))
     with Post => Half_Word'Result < Half;

   function Half_Valid (Word : Unsigned_64) return Boolean is
     (Word <= Unsigned_64 (DPL.Local_Coordinate'Last) or else
      (Word < Half and then
       Word >= Half - Unsigned_64 (-DPL.Local_Coordinate'First)));

   function From_Half (Word : Unsigned_64) return DPL.Local_Coordinate
     with Pre => Half_Valid (Word),
          Post => Half_Word (From_Half'Result) = Word
   is
      Magnitude : Unsigned_64;
      Result : DPL.Local_Coordinate;
   begin
      if Word <= Unsigned_64 (DPL.Local_Coordinate'Last) then
         return DPL.Local_Coordinate (Word);
      end if;
      Magnitude := Half - Word;
      pragma Assert (Magnitude in 1 .. Unsigned_64 (-DPL.Local_Coordinate'First));
      Result := -DPL.Local_Coordinate (Magnitude);
      pragma Assert (Unsigned_64 (-Result) = Magnitude);
      return Result;
   end From_Half;

   function Pack (Item : Position) return Unsigned_64 is
     (Half_Word (Item.X) + Half_Word (Item.Y) * Half);

   function Packed_Valid (Word : Unsigned_64) return Boolean is
     (Half_Valid (Word mod Half) and then Half_Valid (Word / Half));

   function Unpack (Word : Unsigned_64) return Position is
     ((From_Half (Word mod Half), From_Half (Word / Half)));

   function Header_Valid (Wire : D.Wire_Message; Kind : Operation)
      return Boolean is
     (Wire.Label = Code (Kind) and then Wire.Length = Payload_Words and then
      Wire.Flags = 0 and then Wire.Reserved = 0);

   function Plane_Word (Word : Unsigned_64) return Boolean is
     (Word in 1 .. DPL.Plane_Capacity);

   --  [status, count | kinds << 8 | formats << 16 | scaler << 24 | z << 32
   --   | host pointer << 40,
   --   max width | max height << 32, 0]
   Field : constant Unsigned_64 := 2 ** 8;
   function Kind_Bits (Set : DPL.Kind_Set) return Unsigned_64 is
     ((if Set (DPL.Cursor) then 1 else 0) +
      (if Set (DPL.Overlay) then 2 else 0) +
      (if Set (DPL.Primary) then 4 else 0));
   function Format_Bits (Set : DPL.Format_Set) return Unsigned_64 is
     ((if Set (DPL.ARGB8888) then 1 else 0) +
      (if Set (DPL.XRGB8888) then 2 else 0) +
      (if Set (DPL.NV12) then 4 else 0) +
      (if Set (DPL.P010) then 8 else 0));
   Kind_Limit : constant Unsigned_64 := 8;
   Format_Limit : constant Unsigned_64 := 16;

   function Encode (Item : Description) return D.Wire_Message is
     (Label => Code (Query), Length => Payload_Words,
      Words => [Status'Enum_Rep (Accepted),
                Unsigned_64 (Item.Count) +
                  Kind_Bits (Item.Descriptor.Kinds) * Field +
                  Format_Bits (Item.Descriptor.Formats) * Field ** 2 +
                  Boolean'Pos (Item.Descriptor.Scaler) * Field ** 3 +
                  Unsigned_64 (Item.Descriptor.Z) * Field ** 4 +
                  Boolean'Pos (Item.Descriptor.Host_Pointer) * Field ** 5,
                Unsigned_64 (Item.Descriptor.Max_Width) +
                  Unsigned_64 (Item.Descriptor.Max_Height) * Half, 0],
      others => <>);

   function Decode_Description (Wire : D.Wire_Message)
      return Description_Decoding
   is
      Packed  : constant Unsigned_64 := Wire.Words (1);
      Count   : constant Unsigned_64 := Packed mod Field;
      Kinds   : constant Unsigned_64 := Packed / Field mod Field;
      Formats : constant Unsigned_64 := Packed / Field ** 2 mod Field;
      Scaler  : constant Unsigned_64 := Packed / Field ** 3 mod Field;
      Z       : constant Unsigned_64 := Packed / Field ** 4 mod Field;
      Host    : constant Unsigned_64 := Packed / Field ** 5;
      Width   : constant Unsigned_64 := Wire.Words (2) mod Half;
      Height  : constant Unsigned_64 := Wire.Words (2) / Half;
   begin
      if not Header_Valid (Wire, Query) or else
        Wire.Words (0) /= Status'Enum_Rep (Accepted) or else
        Count not in 1 .. DPL.Plane_Capacity or else
        Kinds >= Kind_Limit or else Formats >= Format_Limit or else
        Scaler > Boolean'Pos (Boolean'Last) or else
        Z > Unsigned_64 (DPL.Plane_Z'Last) or else
        Host > Boolean'Pos (Boolean'Last) or else
        Width not in 1 .. DPL.Surface_Extent_Limit or else
        Height not in 1 .. DPL.Surface_Extent_Limit or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Description :=
           (Count => DPL.Plane_Number (Count),
            Descriptor =>
              (Kinds => [DPL.Cursor => (Kinds and 1) /= 0,
                         DPL.Overlay => (Kinds and 2) /= 0,
                         DPL.Primary => (Kinds and 4) /= 0],
               Formats => [DPL.ARGB8888 => (Formats and 1) /= 0,
                           DPL.XRGB8888 => (Formats and 2) /= 0,
                           DPL.NV12 => (Formats and 4) /= 0,
                           DPL.P010 => (Formats and 8) /= 0],
               Max_Width => DPL.Surface_Extent (Width),
               Max_Height => DPL.Surface_Extent (Height),
               Scaler => Scaler /= 0,
               Z => DPL.Plane_Z (Z),
               Host_Pointer => Host /= 0));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Description;

   function Decode_Plane (Kind : Plane_Operation; Wire : D.Wire_Message)
      return Plane_Decoding is
   begin
      if not Header_Valid (Wire, Kind) or else
        not Plane_Word (Wire.Words (0)) or else Wire.Words (1) /= 0 or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant DPL.Plane_Number := DPL.Plane_Number (Wire.Words (0));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Kind, Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Plane;

   function Encode (Item : Buffer) return D.Wire_Message is
     (Label => Code (Map_Plane), Length => Payload_Words,
      Words => [Item.Grant.slot, Item.Grant.generation,
                Unsigned_64 (Item.Width) + Unsigned_64 (Item.Height) * Half,
                Pitch (Item)],
      others => <>);

   function Decode_Buffer (Wire : D.Wire_Message) return Buffer_Decoding is
      Width  : constant Unsigned_64 := Wire.Words (2) mod Half;
      Height : constant Unsigned_64 := Wire.Words (2) / Half;
   begin
      if not Header_Valid (Wire, Map_Plane) or else
        Wire.Words (0) > CuBit.Grant_References.Maximum_Slot or else
        Wire.Words (1) not in CuBit.Grant_References.Generation or else
        Width not in 1 .. DPL.Cursor_Extent_Limit or else
        Height not in 1 .. DPL.Cursor_Extent_Limit or else
        Wire.Words (3) /= Width * Bytes_Per_Pixel
      then
         return (Valid => False);
      end if;
      pragma Assert (Width in 1 .. DPL.Cursor_Extent_Limit);
      pragma Assert (Height in 1 .. DPL.Cursor_Extent_Limit);
      declare
         Result : constant Buffer :=
           ((Wire.Words (0), Wire.Words (1)),
            DPL.Cursor_Extent (Width), DPL.Cursor_Extent (Height));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Buffer;

   function Encode (Item : Show) return D.Wire_Message is
     (Label => Code (Show_Plane), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Plane),
                Unsigned_64 (Item.Hot_X) + Unsigned_64 (Item.Hot_Y) * Half,
                Pack (Item.At_Position), 0],
      others => <>);

   function Decode_Show (Wire : D.Wire_Message) return Show_Decoding is
      Hot_X : constant Unsigned_64 := Wire.Words (1) mod Half;
      Hot_Y : constant Unsigned_64 := Wire.Words (1) / Half;
   begin
      if not Header_Valid (Wire, Show_Plane) or else
        not Plane_Word (Wire.Words (0)) or else
        Hot_X > Unsigned_64 (DPL.Hotspot_Coordinate'Last) or else
        Hot_Y > Unsigned_64 (DPL.Hotspot_Coordinate'Last) or else
        not Packed_Valid (Wire.Words (2)) or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Show :=
           (DPL.Plane_Number (Wire.Words (0)), DPL.Hotspot_Coordinate (Hot_X),
            DPL.Hotspot_Coordinate (Hot_Y), Unpack (Wire.Words (2)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Show;

   function Encode (Item : Move) return D.Wire_Message is
     (Label => Code (Move_Plane), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Plane), Pack (Item.At_Position), 0, 0],
      others => <>);

   function Decode_Move (Wire : D.Wire_Message) return Move_Decoding is
   begin
      if not Header_Valid (Wire, Move_Plane) or else
        not Plane_Word (Wire.Words (0)) or else
        not Packed_Valid (Wire.Words (1)) or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Move :=
           (DPL.Plane_Number (Wire.Words (0)), Unpack (Wire.Words (1)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Move;
end CuBit.GPU_Plane_Protocol;
