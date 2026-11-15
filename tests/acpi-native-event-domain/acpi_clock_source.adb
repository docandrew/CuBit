with ACPI_One_Service;
package body ACPI_Clock_Source with SPARK_Mode=>Off is
   procedure Read(Value:out AML_Decode.Integer_Value;Available:out Boolean) is
   begin Value:=ACPI_One_Service.Now;Available:=True;end Read;
end ACPI_Clock_Source;
