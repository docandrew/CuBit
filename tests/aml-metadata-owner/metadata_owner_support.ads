with AML_Decode;
with Firmware_Tables;
package Metadata_Owner_Support is
   function Enc (Text : String) return AML_Decode.Bytes;
   function Block (Opcode, Payload : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Method (Name : String; Code : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Pressure return AML_Decode.Bytes;
   function Fixture return AML_Decode.Bytes;
   function Table (Code : AML_Decode.Bytes; Revision : Positive) return Firmware_Tables.Bytes;
end Metadata_Owner_Support;
