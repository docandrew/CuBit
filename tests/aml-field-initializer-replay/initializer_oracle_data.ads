pragma Ada_2022;
with AML_Decode;
with AML_Execute;
package Initializer_Oracle_Data is
   subtype Case_Number is Positive range 1 .. 8;
   subtype Step_Number is Positive range 1 .. 14;
   subtype Method_Name is String (1 .. 4);
   type Expectation is record
      Name : Method_Name;
      In_Device : Boolean;
      Status : AML_Execute.Execution_Status;
      Value : AML_Decode.Integer_Value;
      Buffer_Value : AML_Decode.Bytes (1 .. 2);
   end record;
   function Table_Bytes (Item : Case_Number) return AML_Decode.Bytes;
   function Direct_Status (Item : Case_Number) return Boolean;
   function Expected (Item : Case_Number; Step : Step_Number) return Expectation;
end Initializer_Oracle_Data;
