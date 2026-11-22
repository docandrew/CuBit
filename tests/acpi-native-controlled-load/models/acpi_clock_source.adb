with Native_Test_Control;
package body ACPI_Clock_Source is
   procedure Read (Value : out AML_Decode.Integer_Value; Available : out Boolean) is
      package T renames Native_Test_Control;
   begin
      T.Clock_Reads := T.Clock_Reads + 1;
      Available := T.Mode /= "clock-unavailable";
      Value := (if T.Mode = "early-deadline" or else
        (T.Mode = "late-deadline" and then T.Complete_Seen) then 11 else 0);
   end Read;
end ACPI_Clock_Source;
