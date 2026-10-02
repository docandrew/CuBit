pragma Ada_2022;
with Interfaces; use Interfaces;
package body CuBit.Display_Pool_Protocol with SPARK_Mode is
   function Encode (Item : Attachment) return D.Wire_Message is
     (D.Encode_Attachment (Item.Source) with delta Label => Attach_Buffer,
      Flags => Unsigned_8 (Item.Buffer));
   function Decode_Attachment (Wire : D.Wire_Message) return Attachment_Result is
      Decoded : D.Attachment_Decoding;
   begin
      if Wire.Label /= Attach_Buffer or else Wire.Flags not in 1 .. 3 then
         return (Valid => False);
      end if;
      Decoded := D.Decode_Attachment
        ((Wire with delta Label => D.Code (D.Attach_Buffer), Flags => 0));
      if not Decoded.Valid then return (Valid => False); end if;
      return (True, (Buffer_Slot (Wire.Flags), Decoded.Value));
   end Decode_Attachment;
   function Encode (Item : Frame) return D.Wire_Message is
     (D.Encode_Frame (Item.Request) with delta Label => Submit_Frame,
      Flags => Unsigned_8 (Item.Buffer));
   function Decode_Frame (Wire : D.Wire_Message) return Frame_Decoding is
      Decoded : D.Frame_Decoding;
   begin
      if Wire.Label /= Submit_Frame or else Wire.Flags not in 1 .. 3 then
         return (Valid => False);
      end if;
      Decoded := D.Decode_Frame
        ((Wire with delta Label => D.Code (D.Submit_Frame), Flags => 0));
      if not Decoded.Valid then return (Valid => False); end if;
      return (True, (Buffer_Slot (Wire.Flags), Decoded.Value));
   end Decode_Frame;
   function Encode (Item : Completion) return D.Wire_Message is
     (D.Encode_Frame_Result (Item.Result) with delta Label => Submit_Frame,
      Flags => Unsigned_8 (Item.Buffer));
   function Decode_Completion (Wire : D.Wire_Message) return Completion_Decoding is
      Decoded : D.Frame_Result_Decoding;
   begin
      if Wire.Label /= Submit_Frame or else Wire.Flags not in 1 .. 3 then
         return (Valid => False);
      end if;
      Decoded := D.Decode_Frame_Result
        ((Wire with delta Label => D.Code (D.Submit_Frame), Flags => 0));
      if not Decoded.Valid then return (Valid => False); end if;
      return (True, (Buffer_Slot (Wire.Flags), Decoded.Value));
   end Decode_Completion;
end CuBit.Display_Pool_Protocol;
