with Delay_Model;
package body ACPI_Clock_Source with SPARK_Mode => Off is
   procedure Read (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
   begin
      Delay_Model.Read (Value, Available);
   end Read;
end ACPI_Clock_Source;
