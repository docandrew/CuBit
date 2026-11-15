with Delay_Test_Clock; package body ACPI_Clock_Source is
 procedure Read (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
 begin Delay_Test_Clock.Reads := Delay_Test_Clock.Reads + 1; Available := Delay_Test_Clock.Available;
 Value := (if Available then Delay_Test_Clock.Now else 0); end Read; end ACPI_Clock_Source;
