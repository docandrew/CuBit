pragma Ada_2022;
with ACPI_EC_Types;
with ACPI_EC_Clock_Policy;
--  Backend clock only: raw kernel microsecond read/sleep share one epoch.
--  Never convert published/fallback Microseconds by division into IPC ms time.
package ACPI_EC_Native_Clock is
   procedure Read (Value : out ACPI_EC_Types.Microseconds; Available : out Boolean);
   function Last_Status return ACPI_EC_Clock_Policy.Sample_Status;
   procedure Pause_Until
     (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report);
end ACPI_EC_Native_Clock;
