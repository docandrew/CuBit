with AML_Decode;
package Capacity_Fixture is
   function Text (S : String) return AML_Decode.Bytes;
   function Method (Name : String; Code : AML_Decode.Bytes) return AML_Decode.Bytes;
   function Body_Of_Size (Size : Natural) return AML_Decode.Bytes;
end Capacity_Fixture;
