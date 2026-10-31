pragma Ada_2022;
with Interfaces;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with ACPI_EC_Clock_Adapter;
package body ACPI_EC_Native_Clock is
   package K renames CuBit.Kernel_ABI;
   use type Interfaces.Unsigned_64;
   procedure Sample (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin
      Value := CuBit.Kernel_Calls.Call (K.Read_Monotonic_Microseconds);
      Available := Value /= Interfaces.Unsigned_64'Last;
   end Sample;
   procedure Pause (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean) is
      Result : constant Interfaces.Unsigned_64 :=
        CuBit.Kernel_Calls.Call (K.Sleep_Until_Monotonic_Microsecond, Until_Time);
   begin
      Accepted := Result = 0;
   end Pause;
   package Clock is new ACPI_EC_Clock_Adapter (Sample, Pause);
   procedure Read (Value : out ACPI_EC_Types.Microseconds; Available : out Boolean)
     renames Clock.Read;
   function Last_Status return ACPI_EC_Clock_Policy.Sample_Status renames Clock.Last_Status;
   procedure Pause_Until
     (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report) renames Clock.Pause_Until;
end ACPI_EC_Native_Clock;
