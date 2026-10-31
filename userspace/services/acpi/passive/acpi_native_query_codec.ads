pragma Ada_2022;
with CuBit.Messages;
with CuBit.Grant_References;
with ACPI_Query_Transport;
with ACPI_Query_Admission;
-- Native ABI projection only; no receive, acquisition, IPC, AML or authority.
package ACPI_Native_Query_Codec with SPARK_Mode => Off is
   package Transport renames ACPI_Query_Transport;
   -- Exact formal signature for ACPI_Native_Query_Input.Decode.
   procedure Decode
     (Packet : CuBit.Messages.Message; Selected, Decoded : out Boolean;
      Reference_Word, Declared_Length : out ACPI_Query_Admission.Wire.Word);
   function Request
     (Loan : CuBit.Grant_References.Reference; Declared_Length : Transport.Word)
      return CuBit.Messages.Message;
   -- Exact generic binding for ACPI_Runtime_Query_Replies(Reply_Details,Reply).
   function Reply (Value : Transport.Reply_Details) return CuBit.Messages.Message;
   function Decode_Reply
     (Packet : CuBit.Messages.Message; Declared_Length : Transport.Word)
      return Transport.Reply_Decode;
end ACPI_Native_Query_Codec;
