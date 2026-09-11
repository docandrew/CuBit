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
        Wire.Words (3) > DP.Maximum_Buffer_Bytes
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
end CuBit.Display_Protocol;
