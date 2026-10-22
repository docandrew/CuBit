with AML_Decode;
with AML_Names;
package Root_Test_Support is
   function Enc (Text : String) return AML_Decode.Bytes;
   function Path (Data : AML_Decode.Bytes) return AML_Names.Name_Result;
   function Path (Text : String) return AML_Names.Name_Result;
   function Block (Opcode, Payload : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Method (Name : String; Code : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Alias_Of (Source, Destination : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Alias_Of (Source, Destination : String) return AML_Decode.Bytes;
end Root_Test_Support;
