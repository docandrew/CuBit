pragma Ada_2022;
with AML_Decode;
with AML_Execute;
with ACPI_FADT.Transactions;
package Multiple_Oracle_Data is
   subtype Case_Number is Positive range 1 .. 6;
   subtype Step_Number is Positive range 1 .. 20;
   subtype Method_Name is String (1 .. 4);
   type Expectation is record
      Name : Method_Name;
      Status : AML_Execute.Execution_Status;
      Value : AML_Decode.Integer_Value;
   end record;
   function Table_Bytes (Item : Case_Number) return AML_Decode.Bytes;
   function Width (Item : Case_Number) return ACPI_FADT.Transactions.Access_Width;
   function Expected (Item : Case_Number; Step : Step_Number) return Expectation;
end Multiple_Oracle_Data;
