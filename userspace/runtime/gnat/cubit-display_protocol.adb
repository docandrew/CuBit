pragma Ada_2022;
package body CuBit.Display_Protocol with SPARK_Mode is
   function Decode_Attachment (Wire : Wire_Message)
      return Attachment_Decoding
   is
      Width : constant Unsigned_64 := Wire.Words (2) mod 2 ** 32;
      Height : constant Unsigned_64 := Wire.Words (2) / 2 ** 32;
      Layout : Buffer_Layout;
   begin
      if Wire.Label /= Code (Attach_Buffer) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) > Grant_References.Maximum_Slot or else
        Wire.Words (1) not in 1 .. Grant_References.Maximum_Generation or else
        Width not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Height not in 1 .. Unsigned_64 (DP.Positive_Extent'Last) or else
        Wire.Words (3) > Unsigned_64 (DP.Buffer_Pitch'Last)
      then
         return (Valid => False);
      end if;
      Layout := (DP.Positive_Extent (Width), DP.Positive_Extent (Height),
                 DP.Buffer_Pitch (Wire.Words (3)));
      if not DP.Valid_Layout (Layout) then
         return (Valid => False);
      end if;
      return (True, ((Wire.Words (0), Wire.Words (1)), Layout));
   end Decode_Attachment;

   function Encode_Attachment (Item : Attachment_Request) return Wire_Message
   is
     (Label => Code (Attach_Buffer), Length => 4,
      Words => [Item.Grant.slot, Item.Grant.generation,
                Unsigned_64 (Item.Layout.Width) +
                  Unsigned_64 (Item.Layout.Height) * 2 ** 32,
                Unsigned_64 (Item.Layout.Pitch)], others => <>);
   function Encode_Frame (Item : Frame_Request) return Wire_Message is
     (Label => Code (Submit_Frame), Length => 4,
      Words => [Item.Session, Item.Frame,
        Unsigned_64 (Item.Area.X) + Unsigned_64 (Item.Area.Y) * 2 ** 16 +
        Unsigned_64 (Item.Area.Width) * 2 ** 32 +
        Unsigned_64 (Item.Area.Height) * 2 ** 48, 0], others => <>);

   function Decode_Frame (Wire : Wire_Message) return Frame_Decoding is
      Packed : constant Unsigned_64 := Wire.Words (2);
   begin
      if Wire.Label /= Code (Submit_Frame) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) = 0 or else Wire.Words (1) = 0 or else
        Wire.Words (3) /= 0
      then
         return (Valid => False);
      end if;
      return (True, (Wire.Words (0), Wire.Words (1),
        (DP.Pixel_Coordinate (Packed mod 2 ** 16),
         DP.Pixel_Coordinate (Packed / 2 ** 16 mod 2 ** 16),
         DP.Pixel_Extent (Packed / 2 ** 32 mod 2 ** 16),
         DP.Pixel_Extent (Packed / 2 ** 48))));
   end Decode_Frame;

   function Encode_Frame_Result (Item : Frame_Result) return Wire_Message is
      Packed : constant CuBit.Present_Timing.Packed_Pair :=
        CuBit.Present_Timing.Pack
          (if CuBit.Present_Timing.Valid (Item.Timing) then Item.Timing
           else CuBit.Present_Timing.No_Compact);
   begin
      return (Label => Code (Submit_Frame), Length => 4,
              Words => [Item.Session, Item.Frame,
                Frame_Outcome'Enum_Rep (Item.Outcome) or Packed.Outcome_High,
                Buffer_Disposition'Enum_Rep (Item.Buffer_State) or Packed.Buffer_High],
              others => <>);
   end Encode_Frame_Result;

   function Decode_Frame_Result (Wire : Wire_Message)
      return Frame_Result_Decoding
   is
      Mask : constant Unsigned_64 := CuBit.Present_Timing.Enumeration_Mask;
      Outcome_Rep : constant Unsigned_64 := Wire.Words (2) and Mask;
      Buffer_Rep : constant Unsigned_64 := Wire.Words (3) and Mask;
      High : constant CuBit.Present_Timing.Packed_Pair :=
        (Wire.Words (2) and not Mask, Wire.Words (3) and not Mask);
      Timing : constant CuBit.Present_Timing.Compact := CuBit.Present_Timing.Unpack (High);
      use type CuBit.Present_Timing.Packed_Pair;
   begin
      --  One encoding per value: high bits that are not a timing's packing
      --  are refused, as any other malformed field.
      if Wire.Label /= Code (Submit_Frame) or else Wire.Length /= 4 or else
        Wire.Flags /= 0 or else Wire.Reserved /= 0 or else
        Wire.Words (0) = 0 or else Wire.Words (1) = 0 or else
        Outcome_Rep > 2 or else Buffer_Rep > 2 or else
        CuBit.Present_Timing.Pack (Timing) /= High
      then
         return (Valid => False);
      end if;
      return (True, (Wire.Words (0), Wire.Words (1),
        Frame_Outcome'Val (Outcome_Rep),
        Buffer_Disposition'Val (Buffer_Rep), Timing));
   end Decode_Frame_Result;
end CuBit.Display_Protocol;
