pragma Ada_2022;
package body CuBit.Desktop_Protocol.Publication with SPARK_Mode is
   Half : constant Unsigned_64 := 2 ** 16;
   Word : constant Unsigned_64 := 2 ** 32;
   Byte : constant Unsigned_64 := 2 ** 8;
   procedure Equal_Fields (Left, Right : Unsigned_64)
     with Ghost, Pre => Left = Right and then Left < Half,
          Post => Pixel_Coordinate (Left) = Pixel_Coordinate (Right) and then
            Pixel_Extent (Left) = Pixel_Extent (Right);
   procedure Equal_Fields (Left, Right : Unsigned_64) is
   begin
      null;
   end Equal_Fields;
   procedure Equal_Naturals (Left, Right : Unsigned_64)
     with Ghost,
          Pre => Left = Right and then Left <= Unsigned_64 (Natural'Last),
          Post => Natural (Left) = Natural (Right);
   procedure Equal_Naturals (Left, Right : Unsigned_64) is
   begin
      null;
   end Equal_Naturals;
   function Bytes (Low, High : Unsigned_64) return Unsigned_64
     with Pre => Low < Byte and High < Byte,
          Post => Bytes'Result < Half and then
            (Bytes'Result and (Byte - 1)) = Low and then
            Shift_Right (Bytes'Result, 8) = High;
   function Bytes (Low, High : Unsigned_64) return Unsigned_64 is
   begin
      return Low or Shift_Left (High, 8);
   end Bytes;
   function Pair (Low, High : Unsigned_64) return Unsigned_64
     with Pre => Low < Half and High < Half,
          Post => Pair'Result < Word and then
            (Pair'Result and (Half - 1)) = Low and then
            Shift_Right (Pair'Result, 16) = High;
   function Pair (Low, High : Unsigned_64) return Unsigned_64 is
   begin
      return Low or Shift_Left (High, 16);
   end Pair;
   function Words (Low, High : Unsigned_64) return Unsigned_64
     with Pre => Low < Word and High < Word,
          Post => (Words'Result and (Word - 1)) = Low and then
            Shift_Right (Words'Result, 32) = High;
   function Words (Low, High : Unsigned_64) return Unsigned_64 is
   begin
      return Low or Shift_Left (High, 32);
   end Words;
   function Header (Wire : Wire_Message; Label : Unsigned_32) return Boolean is
     (Wire.Label = Label and Wire.Length = 4 and Wire.Flags = 0 and
      Wire.Reserved = 0);
   function Status_Of (Wire : Wire_Message) return Status_Code is
     (if Wire.Words (0) <= Status_Code'Enum_Rep (Resources_Exhausted)
      then Status_Code'Val (Wire.Words (0)) else Invalid_Request);
   function Failure (Status : Status_Code; Label : Unsigned_32)
      return Wire_Message is
     (Label, 4, 0, 0, [Status_Code'Enum_Rep (Status), 0, 0, 0]);
   function Empty_Tail (Wire : Wire_Message) return Boolean is
     (Wire.Words (1) = 0 and Wire.Words (2) = 0 and Wire.Words (3) = 0);

   function Configuration_Words_Valid (Wire : Wire_Message) return Boolean is
     (Wire.Words (1) in Identity and then
      Shift_Right (Shift_Right (Wire.Words (2), 32), 16) = 0 and then
      ((Wire.Words (2) and (Word - 1)) and (Half - 1)) /= 0 and then
      Shift_Right (Wire.Words (2) and (Word - 1), 16) /= 0 and then
      (Shift_Right (Wire.Words (2), 32) and (Byte - 1)) in 1 .. 16 and then
      (Shift_Right (Shift_Right (Wire.Words (2), 32), 8) and (Byte - 1))
        in 1 .. 16 and then
      ((Wire.Words (3) and (Word - 1)) and (Half - 1)) /= 0 and then
      Shift_Right (Wire.Words (3) and (Word - 1), 16) /= 0 and then
      Shift_Right (Wire.Words (3), 32) <= Maximum_Buffer_Bytes);
   function Configuration_Values (Wire : Wire_Message)
      return Configuration is
     (Wire.Words (1),
      Positive_Extent (((Wire.Words (2) and (Word - 1)) and (Half - 1))),
      Positive_Extent (Shift_Right (Wire.Words (2) and (Word - 1), 16)),
      Scale_Component ((Shift_Right (Wire.Words (2), 32) and (Byte - 1))),
      Scale_Component
        (Shift_Right (Shift_Right (Wire.Words (2), 32), 8) and (Byte - 1)),
      (Positive_Extent (((Wire.Words (3) and (Word - 1)) and (Half - 1))),
       Positive_Extent (Shift_Right (Wire.Words (3) and (Word - 1), 16)),
       Buffer_Pitch (Shift_Right (Wire.Words (3), 32))))
     with Pre => Configuration_Words_Valid (Wire),
          Annotate => (GNATprove, Inline_For_Proof);
   function Configuration_Fields (Wire : Wire_Message)
      return Configuration_Result is
     (if not Header (Wire, Configuration_Label)
      then (Status => Invalid_Request)
      elsif Status_Of (Wire) /= Success then
        (Status => Failure_Status
          (if Empty_Tail (Wire) then Status_Of (Wire) else Invalid_Request))
      elsif not Configuration_Words_Valid (Wire)
      then (Status => Invalid_Request)
      elsif Valid (Configuration_Values (Wire))
      then (Success, Configuration_Values (Wire))
      else (Status => Invalid_Request));
   function Decode_Configuration (Wire : Wire_Message)
      return Configuration_Result is (Configuration_Fields (Wire));

   function Encode_Configuration (Item : Configuration_Result)
      return Wire_Message is
   begin
      if Item.Status /= Success then
         declare
            Result : constant Wire_Message :=
              Failure (Item.Status, Configuration_Label);
         begin
            pragma Assert (Decode_Configuration (Result) = Item);
            return Result;
         end;
      end if;
      declare
         Logical_Size : constant Unsigned_64 :=
           Pair (Unsigned_64 (Item.Value.Width),
                 Unsigned_64 (Item.Value.Height));
         Density : constant Unsigned_64 :=
           Bytes (Unsigned_64 (Item.Value.Numerator),
                  Unsigned_64 (Item.Value.Denominator));
         Logical : constant Unsigned_64 := Words (Logical_Size, Density);
         Physical_Size : constant Unsigned_64 :=
           Pair (Unsigned_64 (Item.Value.Layout.Width),
                 Unsigned_64 (Item.Value.Layout.Height));
         Physical : constant Unsigned_64 :=
           Words (Physical_Size, Unsigned_64 (Item.Value.Layout.Pitch));
         Result : constant Wire_Message :=
           (Configuration_Label, 4, 0, 0,
            [0, Item.Value.Epoch, Logical, Physical]);
      begin
         Equal_Fields
           ((Logical and (Word - 1)) and (Half - 1),
            Unsigned_64 (Item.Value.Width));
         Equal_Fields
           (Shift_Right (Logical and (Word - 1), 16),
            Unsigned_64 (Item.Value.Height));
         Equal_Naturals
           (Shift_Right (Logical, 32) and (Byte - 1),
            Unsigned_64 (Item.Value.Numerator));
         Equal_Naturals
           (Shift_Right (Shift_Right (Logical, 32), 8) and (Byte - 1),
            Unsigned_64 (Item.Value.Denominator));
         Equal_Fields
           ((Physical and (Word - 1)) and (Half - 1),
            Unsigned_64 (Item.Value.Layout.Width));
         Equal_Fields
           (Shift_Right (Physical and (Word - 1), 16),
            Unsigned_64 (Item.Value.Layout.Height));
         Equal_Naturals
           (Shift_Right (Physical, 32), Unsigned_64 (Item.Value.Layout.Pitch));
         pragma Assert (Configuration_Words_Valid (Result));
         declare
            Recovered : constant Configuration :=
              Configuration_Values (Result) with Ghost;
         begin
            pragma Assert (Recovered = Item.Value);
            pragma Assert (Valid (Recovered));
            pragma Assert
              (Configuration_Fields (Result) = (Success, Recovered));
            pragma Assert (Item = (Success, Recovered));
            declare
               Decoded : constant Configuration_Result :=
                 Decode_Configuration (Result) with Ghost;
            begin
               pragma Assert (Decoded = Configuration_Fields (Result));
               pragma Assert (Decoded = Item);
               return Result;
            end;
         end;
      end;
   end Encode_Configuration;

   function Decode_Query (Wire : Wire_Message; Retirement : Boolean)
      return Query_Decoding is
     (if not Header
        (Wire, (if Retirement then Retirement_Label else Configuration_Label))
        or else Wire.Words (0) = 0 or else Wire.Words (2) /= 0 or else
          Wire.Words (3) /= 0
        or else (if Retirement then Wire.Words (1) not in Identity
                 else Wire.Words (1) /= 0)
      then (Valid => False)
      else (True, (Live_Surface_Name (Wire.Words (0)), Wire.Words (1))));
   function Encode_Query (Item : Query; Retirement : Boolean)
      return Wire_Message is
     ((if Retirement then Retirement_Label else Configuration_Label), 4, 0, 0,
      [Unsigned_64 (Item.Surface), Item.Ticket, 0, 0]);

   function Decode_Stage (Wire : Wire_Message) return Stage_Decoding is
     (if not Header (Wire, Stage_Label) or else Wire.Words (0) = 0 or else
        Wire.Words (1) not in Identity or else Wire.Words (3) /= 0 or else
        not CuBit.Grant_References.Valid_Wire (Wire.Words (2))
      then (Valid => False)
      else (True, (Live_Surface_Name (Wire.Words (0)), Wire.Words (1),
        CuBit.Grant_References.Decode (Wire.Words (2)))));
   function Encode_Stage (Item : Stage_Request) return Wire_Message is
     (Stage_Label, 4, 0, 0,
      [Unsigned_64 (Item.Surface), Item.Epoch,
       CuBit.Grant_References.Encode (Item.Grant), 0]);

   function Decode_Receipt (Wire : Wire_Message; Expected : Receipt_Label)
      return Receipt is
     (if not Header (Wire, Expected) then (Status => Invalid_Request)
      elsif Status_Of (Wire) /= Success then
        (Status => Failure_Status
           (if Empty_Tail (Wire) then Status_Of (Wire) else Invalid_Request))
      elsif Wire.Words (1) not in Identity or else
        Wire.Words (2) not in Identity or else Wire.Words (3) /= 0
      then (Status => Invalid_Request)
      else (Success, Wire.Words (1), Wire.Words (2)));
   function Encode_Receipt (Item : Receipt; Label : Receipt_Label)
      return Wire_Message is
     (if Item.Status = Success then
        (Label, 4, 0, 0, [0, Item.Epoch, Item.Ticket, 0])
      else Failure (Item.Status, Label));

   function Damage_Fields (Packed : Unsigned_64) return Rectangle is
     (Pixel_Coordinate (((Packed and (Word - 1)) and (Half - 1))),
      Pixel_Coordinate (Shift_Right (Packed and (Word - 1), 16)),
      Pixel_Extent ((Shift_Right (Packed, 32) and (Half - 1))),
      Pixel_Extent (Shift_Right (Shift_Right (Packed, 32), 16)))
     with Post =>
       Damage_Fields'Result.X =
         Pixel_Coordinate ((Packed and (Word - 1)) and (Half - 1)) and then
       Damage_Fields'Result.Y =
         Pixel_Coordinate (Shift_Right (Packed and (Word - 1), 16)) and then
       Damage_Fields'Result.Width =
         Pixel_Extent (Shift_Right (Packed, 32) and (Half - 1)) and then
       Damage_Fields'Result.Height =
         Pixel_Extent (Shift_Right (Shift_Right (Packed, 32), 16));
   function Publication_Fields (Wire : Wire_Message) return Publish_Decoding is
     (if not Header (Wire, Publish_Label) or else Wire.Words (0) = 0 or else
       (Wire.Words (1) and (Word - 1)) not in Identity or else
       Shift_Right (Wire.Words (1), 32) not in Identity or else
       not Valid_Damage (Damage_Fields (Wire.Words (3)))
      then (Valid => False)
      else (True, (Live_Surface_Name (Wire.Words (0)),
                   Wire.Words (1) and (Word - 1),
                   Shift_Right (Wire.Words (1), 32),
                   Damage_Fields (Wire.Words (3)), Wire.Words (2))));
   function Decode_Publish (Wire : Wire_Message) return Publish_Decoding is
     (Publication_Fields (Wire));

   function Encode_Publish (Item : Publish_Request) return Wire_Message is
      Low : constant Unsigned_64 :=
        Pair (Unsigned_64 (Item.Area.X), Unsigned_64 (Item.Area.Y));
      High : constant Unsigned_64 :=
        Pair (Unsigned_64 (Item.Area.Width), Unsigned_64 (Item.Area.Height));
      Packed : constant Unsigned_64 := Words (Low, High);
      Recovered : constant Rectangle := Damage_Fields (Packed) with Ghost;
   begin
      Equal_Fields
        ((Packed and (Word - 1)) and (Half - 1), Unsigned_64 (Item.Area.X));
      Equal_Fields
        (Shift_Right (Packed and (Word - 1), 16), Unsigned_64 (Item.Area.Y));
      Equal_Fields
        (Shift_Right (Packed, 32) and (Half - 1),
         Unsigned_64 (Item.Area.Width));
      Equal_Fields
        (Shift_Right (Shift_Right (Packed, 32), 16),
         Unsigned_64 (Item.Area.Height));
      pragma Assert (Recovered = Item.Area);
      return (Publish_Label, 4, 0, 0,
        [Unsigned_64 (Item.Surface), Words (Item.Epoch, Item.Ticket),
         Item.Input_After, Packed]);
   end Encode_Publish;
end CuBit.Desktop_Protocol.Publication;
