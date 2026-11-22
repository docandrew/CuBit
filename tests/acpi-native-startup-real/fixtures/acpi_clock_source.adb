with Owner_Test_Control;
package body ACPI_Clock_Source with SPARK_Mode => Off is
   use type Owner_Test_Control.Scenario;
   procedure Read (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      package T renames Owner_Test_Control;
   begin
      T.Clock_Reads := T.Clock_Reads + 1;
      Available := T.Selected /= T.Clock_Unavailable;
      Value := (if not Available then 0 elsif T.Selected = T.Clock_Overflow
                then AML_Decode.Integer_Value'Last else 10);
   end Read;
end ACPI_Clock_Source;
