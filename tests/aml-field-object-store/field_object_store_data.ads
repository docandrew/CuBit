pragma Ada_2022;
with AML_Decode;
with AML_Execute;
with ACPI_FADT.Transactions;
package Field_Object_Store_Data is
   subtype Case_Number is Positive range 1 .. 6;
   subtype Step_Number is Positive range 1 .. 20;
   type Expectation is record
      Name : String (1 .. 4);
      Status : AML_Execute.Execution_Status;
      Value : AML_Decode.Integer_Value;
   end record;
   function Table_Bytes (Item : Case_Number) return AML_Decode.Bytes;
   function Access_Width_Of (Item : Case_Number) return ACPI_FADT.Transactions.Access_Width;
   function Expected (Item : Case_Number; Step : Step_Number) return Expectation;
end Field_Object_Store_Data;
