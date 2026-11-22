with Delay_Test_Clock; with ACPI_Native_Delays; with AML_Delays;
package body CuBit.Kernel_Calls is
 function Call (Code : CuBit.Kernel_ABI.Operation; Target : Interfaces.Unsigned_64 := 0) return Interfaces.Unsigned_64 is
  Result : AML_Delays.Outcome;
 begin
  case Code is
   when CuBit.Kernel_ABI.Yield => Delay_Test_Clock.Yields := Delay_Test_Clock.Yields + 1;
   when CuBit.Kernel_ABI.Sleep_Until_Monotonic_Microsecond =>
    Delay_Test_Clock.Sleeps := Delay_Test_Clock.Sleeps + 1;
    if Delay_Test_Clock.Reenter then
      Delay_Test_Clock.Reenter := False;
      ACPI_Native_Delays.Perform ((AML_Delays.Stall_Delay, 0), Result);
    elsif Delay_Test_Clock.Close_In_Sleep then ACPI_Native_Delays.Close_Startup;
    end if;
    Delay_Test_Clock.Now := Target;
  end case; return 0;
 end Call; end CuBit.Kernel_Calls;
