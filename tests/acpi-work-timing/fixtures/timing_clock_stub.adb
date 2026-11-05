pragma Ada_2022;
with Timing_Test_Control;
package body ACPI_Clock_Source is
   procedure Read (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Value := Timing_Test_Control.Now; Available := Timing_Test_Control.Known; end Read;
end ACPI_Clock_Source;
