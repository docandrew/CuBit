pragma Ada_2022;

package body CuBit.Display_Plane_Protocol with SPARK_Mode is
   use type DPL.Space_Coordinate, DPL.Request_Count;
   Field_Bits : constant := 16;
   Field_Limit : constant Unsigned_64 := 2 ** Field_Bits;
   Half_Shift : constant := 32;

   function Decode_Operation (Label : Unsigned_32) return Operation_Decoding
   is
   begin
      for Kind in Operation loop
         if Code (Kind) = Label then
            return (True, Kind);
         end if;
      end loop;
      return (Valid => False);
   end Decode_Operation;

   --  Signed coordinates: two's complement in a 64-bit word.
   function To_Word (Value : DPL.Space_Coordinate) return Unsigned_64 is
     (if Value >= 0 then Unsigned_64 (Value)
      else 0 - Unsigned_64 (-Value));
   function Coordinate_Word (Word : Unsigned_64) return Boolean is
     (Word <= DPL.Space_Limit or else Word >= 0 - DPL.Space_Limit);
   function From_Word (Word : Unsigned_64) return DPL.Space_Coordinate
     with Pre => Coordinate_Word (Word),
          Post => To_Word (From_Word'Result) = Word
   is
      Magnitude : Unsigned_64;
      Result : DPL.Space_Coordinate;
   begin
      if Word <= DPL.Space_Limit then
         return DPL.Space_Coordinate (Word);
      end if;
      Magnitude := 0 - Word;
      pragma Assert (Magnitude in 1 .. DPL.Space_Limit);
      Result := -DPL.Space_Coordinate (Magnitude);
      pragma Assert (Unsigned_64 (-Result) = Magnitude);
      return Result;
   end From_Word;

   function Header_Valid (Wire : D.Wire_Message; Kind : Operation)
      return Boolean is
     (Wire.Label = Code (Kind) and then Wire.Length = Payload_Words and then
      Wire.Flags = 0 and then Wire.Reserved = 0);

   function Request_Word (Word : Unsigned_64) return Boolean is
     (Word in 1 .. DPL.Request_Capacity);

   --  [request, kind, priority, absolute]. Set_Priority carries zeros.
   function Encode (Kind : Identity_Operation; Item : Identity)
      return D.Wire_Message is
     (Label => Code (Kind), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Request),
                DPL.Plane_Kind'Pos (Item.Kind),
                Unsigned_64 (Item.Priority), Boolean'Pos (Item.Absolute)],
      others => <>);

   function Decode_Identity (Kind : Identity_Operation; Wire : D.Wire_Message)
      return Identity_Decoding is
   begin
      if not Header_Valid (Wire, Kind) or else
        not Request_Word (Wire.Words (0)) or else
        Wire.Words (1) > DPL.Plane_Kind'Pos (DPL.Plane_Kind'Last) or else
        Wire.Words (2) > Unsigned_64 (DPL.Request_Priority'Last) or else
        Wire.Words (3) > Boolean'Pos (Boolean'Last)
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Identity :=
           (DPL.Request_Id (Wire.Words (0)),
            DPL.Plane_Kind'Val (Wire.Words (1)),
            DPL.Request_Priority (Wire.Words (2)),
            Wire.Words (3) /= 0);
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Kind, Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Identity;

   function Encode_Destroy (Request : DPL.Request_Id) return D.Wire_Message is
     (Label => Code (Destroy_Request), Length => Payload_Words,
      Words => [Unsigned_64 (Request), 0, 0, 0], others => <>);

   function Decode_Destroy (Wire : D.Wire_Message) return Request_Decoding is
   begin
      if not Header_Valid (Wire, Destroy_Request) or else
        not Request_Word (Wire.Words (0)) or else
        Wire.Words (1) /= 0 or else Wire.Words (2) /= 0 or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant DPL.Request_Id :=
           DPL.Request_Id (Wire.Words (0));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode_Destroy (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Destroy;

   --  Width | Height << 16 | Hot_X << 32 | Hot_Y << 48.
   function Encode (Item : Cursor_Image) return D.Wire_Message is
     (Label => Code (Set_Cursor_Image), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Request), Item.Grant.slot,
                Item.Grant.generation,
                Unsigned_64 (Item.Width) +
                Unsigned_64 (Item.Height) * Field_Limit +
                Unsigned_64 (Item.Hot_X) * Field_Limit ** 2 +
                Unsigned_64 (Item.Hot_Y) * Field_Limit ** 3],
      others => <>);

   function Decode_Image (Wire : D.Wire_Message) return Image_Decoding is
      Packed : constant Unsigned_64 := Wire.Words (3);
      Width  : constant Unsigned_64 := Packed mod Field_Limit;
      Height : constant Unsigned_64 := Packed / Field_Limit mod Field_Limit;
      Hot_X  : constant Unsigned_64 := Packed / Field_Limit ** 2 mod Field_Limit;
      Hot_Y  : constant Unsigned_64 := Packed / Field_Limit ** 3;
      Result : Cursor_Image;
   begin
      if not Header_Valid (Wire, Set_Cursor_Image) or else
        not Request_Word (Wire.Words (0)) or else
        Wire.Words (1) > CuBit.Grant_References.Maximum_Slot or else
        Wire.Words (2) not in CuBit.Grant_References.Generation or else
        Width not in 1 .. DPL.Cursor_Extent_Limit or else
        Height not in 1 .. DPL.Cursor_Extent_Limit or else
        Hot_X >= Width or else Hot_Y >= Height
      then
         return (Valid => False);
      end if;
      Result := (Request => DPL.Request_Id (Wire.Words (0)),
                 Grant => (Wire.Words (1), Wire.Words (2)),
                 Width => DPL.Cursor_Extent (Width),
                 Height => DPL.Cursor_Extent (Height),
                 Hot_X => DPL.Hotspot_Coordinate (Hot_X),
                 Hot_Y => DPL.Hotspot_Coordinate (Hot_Y));
      --  Canonical form: re-encoding must reproduce every bit.
      if Encode (Result) /= Wire then
         return (Valid => False);
      end if;
      return (True, Result);
   end Decode_Image;

   function Encode (Item : Anchor) return D.Wire_Message is
     (Label => Code (Set_Anchor), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Request), Unsigned_64 (Item.X),
                Unsigned_64 (Item.Y), 0],
      others => <>);

   function Decode_Anchor (Wire : D.Wire_Message) return Anchor_Decoding is
   begin
      if not Header_Valid (Wire, Set_Anchor) or else
        not Request_Word (Wire.Words (0)) or else
        Wire.Words (1) > Unsigned_64 (DPL.Hotspot_Coordinate'Last) or else
        Wire.Words (2) > Unsigned_64 (DPL.Hotspot_Coordinate'Last) or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Anchor :=
           (DPL.Request_Id (Wire.Words (0)),
                     DPL.Hotspot_Coordinate (Wire.Words (1)),
                     DPL.Hotspot_Coordinate (Wire.Words (2)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Anchor;

   function Encode (Item : Move) return D.Wire_Message is
     (Label => Code (Move_Request), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Request), To_Word (Item.X), To_Word (Item.Y),
                0],
      others => <>);

   function Decode_Move (Wire : D.Wire_Message) return Move_Decoding is
   begin
      if not Header_Valid (Wire, Move_Request) or else
        not Request_Word (Wire.Words (0)) or else
        not Coordinate_Word (Wire.Words (1)) or else
        not Coordinate_Word (Wire.Words (2)) or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Move :=
           (DPL.Request_Id (Wire.Words (0)),
                     From_Word (Wire.Words (1)), From_Word (Wire.Words (2)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Move;

   function Encode (Item : Visibility) return D.Wire_Message is
     (Label => Code (Set_Visibility), Length => Payload_Words,
      Words => [Unsigned_64 (Item.Request), Boolean'Pos (Item.Visible), 0, 0],
      others => <>);

   function Decode_Visibility (Wire : D.Wire_Message)
      return Visibility_Decoding is
   begin
      if not Header_Valid (Wire, Set_Visibility) or else
        not Request_Word (Wire.Words (0)) or else
        Wire.Words (1) > Boolean'Pos (Boolean'Last) or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Visibility :=
           (DPL.Request_Id (Wire.Words (0)), Wire.Words (1) /= 0);
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Visibility;

   function Encode (Item : Origin) return D.Wire_Message is
     (Label => Code (Place_Output), Length => Payload_Words,
      Words => [To_Word (Item.X), To_Word (Item.Y), 0, 0], others => <>);

   function Decode_Origin (Wire : D.Wire_Message) return Origin_Decoding is
   begin
      if not Header_Valid (Wire, Place_Output) or else
        not Coordinate_Word (Wire.Words (0)) or else
        not Coordinate_Word (Wire.Words (1)) or else
        Wire.Words (2) /= 0 or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Origin :=
           (From_Word (Wire.Words (0)), From_Word (Wire.Words (1)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Origin;

   --  [status, cursor | overlay << 8 | primary << 16 | host-pointer << 24,
   --   w | h << 32, capacity]
   Count_Bits : constant := 8;
   Count_Limit : constant Unsigned_64 := 2 ** Count_Bits;
   function Encode (Item : Capability) return D.Wire_Message is
     (Label => Code (Query_Output), Length => Payload_Words,
      Words => [Status_Code'Enum_Rep (D.DP.Success),
                Unsigned_64 (Item.Planes (DPL.Cursor)) +
                  Unsigned_64 (Item.Planes (DPL.Overlay)) * Count_Limit +
                  Unsigned_64 (Item.Planes (DPL.Primary)) * Count_Limit ** 2 +
                  Unsigned_64 (Item.Host_Pointer_Cursors) * Count_Limit ** 3,
                Unsigned_64 (Item.Max_Width) +
                  Unsigned_64 (Item.Max_Height) * 2 ** Half_Shift,
                Unsigned_64 (Item.Capacity)],
      others => <>);

   function Decode_Capability (Wire : D.Wire_Message)
      return Capability_Decoding
   is
      Width  : constant Unsigned_64 := Wire.Words (2) mod 2 ** Half_Shift;
      Height : constant Unsigned_64 := Wire.Words (2) / 2 ** Half_Shift;
      Counts : constant Unsigned_64 := Wire.Words (1);
      Cursors  : constant Unsigned_64 := Counts mod Count_Limit;
      Overlays : constant Unsigned_64 := Counts / Count_Limit mod Count_Limit;
      Primaries : constant Unsigned_64 := Counts / Count_Limit ** 2 mod Count_Limit;
      Host_Pointers : constant Unsigned_64 := Counts / Count_Limit ** 3;
   begin
      if not Header_Valid (Wire, Query_Output) or else
        Wire.Words (0) /= Status_Code'Enum_Rep (D.DP.Success) or else
        Cursors > DPL.Plane_Capacity or else
        Overlays > DPL.Plane_Capacity or else
        Primaries > DPL.Plane_Capacity or else
        Host_Pointers > DPL.Plane_Capacity or else
        Width not in 1 .. DPL.Cursor_Extent_Limit or else
        Height not in 1 .. DPL.Cursor_Extent_Limit or else
        Wire.Words (3) > DPL.Request_Capacity
      then
         return (Valid => False);
      end if;
      declare
         Result : constant Capability :=
           (Planes => [DPL.Cursor => DPL.Plane_Count (Cursors),
                       DPL.Overlay => DPL.Plane_Count (Overlays),
                       DPL.Primary => DPL.Plane_Count (Primaries)],
            Host_Pointer_Cursors => DPL.Plane_Count (Host_Pointers),
            Max_Width => DPL.Cursor_Extent (Width),
            Max_Height => DPL.Cursor_Extent (Height),
            Capacity => DPL.Request_Count (Wire.Words (3)));
      begin
         --  Canonical form: re-encoding must reproduce every bit.
         if Encode (Result) /= Wire then
            return (Valid => False);
         end if;
         return (True, Result);
      end;
   end Decode_Capability;

   function To_Bits (Set : Request_Set) return Unsigned_64 is
      Result : Unsigned_64 := 0;
   begin
      for C in DPL.Request_Id loop
         if Set (C) then
            Result := Result or Bit (C);
         end if;
         pragma Loop_Invariant (Result < Shift_Left (1, Natural (C)));
         pragma Loop_Invariant
           (for all E in DPL.Request_Id =>
              ((Result and Bit (E)) /= 0) = (E <= C and then Set (E)));
      end loop;
      return Result;
   end To_Bits;

   Hardware_Shift : constant := 0;
   Software_Shift : constant := Set_Field_Bits;
   Committed_Hardware_Shift : constant := 2 * Set_Field_Bits;
   Committed_Software_Shift : constant := 3 * Set_Field_Bits;
   Field_Mask : constant Unsigned_64 := 2 ** Set_Field_Bits - 1;

   function Encode (Kind : Operation; Item : Report) return D.Wire_Message is
     (Label => Code (Kind), Length => Payload_Words,
      Words => [Status_Code'Enum_Rep (Item.Status), Unsigned_64 (Item.Epoch),
                Shift_Left (To_Bits (Item.Proposed_Hardware), Hardware_Shift)
                or Shift_Left (To_Bits (Item.Proposed_Composited), Software_Shift)
                or Shift_Left (To_Bits (Item.Committed_Hardware),
                               Committed_Hardware_Shift)
                or Shift_Left (To_Bits (Item.Committed_Composited),
                               Committed_Software_Shift),
                0],
      others => <>);

   function Decode_Report (Kind : Operation; Wire : D.Wire_Message)
      return Report_Decoding
   is
      Sets : constant Unsigned_64 := Wire.Words (2);
      Status : Status_Code;
   begin
      if not Header_Valid (Wire, Kind) or else
        Sets >= 2 ** (4 * Set_Field_Bits) or else Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      case Wire.Words (0) is
         when 0 => Status := D.DP.Success;
         when 1 => Status := D.DP.Denied;
         when 2 => Status := D.DP.Bad_Object;
         when 3 => Status := D.DP.Bad_State;
         when 4 => Status := D.DP.Invalid_Request;
         when 5 => Status := D.DP.Unsupported;
         when 6 => Status := D.DP.Resources_Exhausted;
         when others => return (Valid => False);
      end case;
      return (True,
        (Status => Status,
         Epoch => DPL.Plan_Epoch (Wire.Words (1)),
         Proposed_Hardware =>
           From_Bits (Shift_Right (Sets, Hardware_Shift) and Field_Mask),
         Proposed_Composited =>
           From_Bits (Shift_Right (Sets, Software_Shift) and Field_Mask),
         Committed_Hardware =>
           From_Bits (Shift_Right (Sets, Committed_Hardware_Shift) and Field_Mask),
         Committed_Composited =>
           From_Bits (Shift_Right (Sets, Committed_Software_Shift) and Field_Mask)));
   end Decode_Report;
   package Pool renames CuBit.Display_Pool_Protocol;

   function Encode (Item : Plan_Frame) return D.Wire_Message is
     (Pool.Encode (Item.Item) with delta Label => Submit_Plan_Frame,
      Words => [Pool.Encode (Item.Item).Words (0),
                Pool.Encode (Item.Item).Words (1),
                Pool.Encode (Item.Item).Words (2), Unsigned_64 (Item.Epoch)]);

   function Decode_Plan_Frame (Wire : D.Wire_Message)
      return Plan_Frame_Decoding
   is
      Decoded : Pool.Frame_Decoding;
   begin
      if Wire.Label /= Submit_Plan_Frame or else Wire.Words (3) = 0 then
         return (Valid => False);
      end if;
      Decoded := Pool.Decode_Frame
        ((Wire with delta Label => Pool.Submit_Frame,
          Words => [Wire.Words (0), Wire.Words (1), Wire.Words (2), 0]));
      if not Decoded.Valid then
         return (Valid => False);
      end if;
      return (True, (Decoded.Value, Live_Epoch (Wire.Words (3))));
   end Decode_Plan_Frame;
end CuBit.Display_Plane_Protocol;
