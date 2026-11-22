pragma Ada_2022;
with Interfaces;
with ACPI_EC_Types;
with ACPI_EC_Clock_Policy;
--  Serialized trusted callbacks. Raw read and pause share one microsecond
--  clock epoch,return normally,remain bounded,and never reenter this instance.
generic
   with procedure Read_Raw (Value : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Pause_Raw (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean);
   Maximum_Slice : ACPI_EC_Clock_Policy.Positive_Microseconds :=
     ACPI_EC_Clock_Policy.Default_Maximum_Slice;
package ACPI_EC_Clock_Adapter with SPARK_Mode is
   use type ACPI_EC_Types.Microseconds, ACPI_EC_Types.Work_Budget;
   procedure Read (Value : out ACPI_EC_Types.Microseconds; Available : out Boolean)
     with Post => (if not Available then Value = 0);
   function Last_Status return ACPI_EC_Clock_Policy.Sample_Status;
   --  One bounded pause request at most. Every positive-budget call consumes
   --  one attempt,even for unavailable/stopped clocks. Never reset Remaining
   --  inside a wait episode. Exhaustion needs explicit owner supervision.
   --  Pause_Returned does not attest the target was reached; resample and let
   --  the engine enforce its own time/ownership/budget guards before any IO.
   procedure Pause_Until
     (Target : ACPI_EC_Types.Microseconds;
      Remaining : in out ACPI_EC_Types.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report)
     with Post => Remaining = (if Remaining'Old = 0 then 0 else Remaining'Old - 1);
private
   Status : ACPI_EC_Clock_Policy.Sample_Status := ACPI_EC_Clock_Policy.Unavailable;
end ACPI_EC_Clock_Adapter;
