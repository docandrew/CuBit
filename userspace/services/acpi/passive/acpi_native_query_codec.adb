pragma Ada_2022;
with Interfaces;
package body ACPI_Native_Query_Codec with SPARK_Mode => Off is
   function Copy_Header (Value : CuBit.Messages.Message) return Transport.Packet is
     (Label => Transport.Message_Label (Value.tag.label),
      Length => Transport.Header_Byte (Value.tag.length),
      Flags => Transport.Header_Byte (CuBit.Messages.Flag_Byte (Value.tag)),
      Reserved => Transport.Reserved_Field (Value.tag.reserved),
      Data => Transport.Words (Value.words));
   function Message (Value : Transport.Packet) return CuBit.Messages.Message is
      Result : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      -- Only canonical packets from the pure encoders reach this helper.
      Result.tag.label := Interfaces.Unsigned_32 (Value.Label);
      Result.tag.length := Interfaces.Unsigned_8 (Value.Length);
      Result.words := CuBit.Messages.MessageWords (Value.Data);
      return Result;
   end Message;
   procedure Decode
     (Packet : CuBit.Messages.Message; Selected, Decoded : out Boolean;
      Reference_Word, Declared_Length : out ACPI_Query_Admission.Wire.Word) is
      Result : constant Transport.Request_Decode := Transport.Decode_Request (Copy_Header (Packet));
   begin
      Selected := Result.Selected; Decoded := Result.Decoded;
      Reference_Word := Result.Reference_Word; Declared_Length := Result.Declared_Length;
   end Decode;
   function Request
     (Loan : CuBit.Grant_References.Reference; Declared_Length : Transport.Word)
      return CuBit.Messages.Message is
     (Message (Transport.Encode_Request (CuBit.Grant_References.Encode (Loan), Declared_Length)));
   function Reply (Value : Transport.Reply_Details) return CuBit.Messages.Message is
     (Message (Transport.Encode_Reply (Value)));
   function Decode_Reply
     (Packet : CuBit.Messages.Message; Declared_Length : Transport.Word)
      return Transport.Reply_Decode is
     (Transport.Decode_Reply (Copy_Header (Packet), Declared_Length));
end ACPI_Native_Query_Codec;
