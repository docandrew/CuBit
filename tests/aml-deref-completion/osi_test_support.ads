pragma Ada_2022;
with Firmware_Tables;
with AML_Interface_Policy;
package Osi_Test_Support is
   subtype Bytes is Firmware_Tables.Bytes;
   function Enc (Text : String) return Bytes;
   function Configured return AML_Interface_Policy.Policy;
   function Block (Opcode, Payload : Bytes) return Bytes;
   function Method (Name : String; Code : Bytes) return Bytes;
   function String_Operand (Text : String) return Bytes;
   function Table (Payload : Bytes) return Bytes;
end Osi_Test_Support;
