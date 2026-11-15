pragma Ada_2022;
with Interfaces;
with ACPI_Local_Runtime_Status;
with ACPI_Local_Sample_Status;
with ACPI_Local_Sample_Events;
generic
   with function Runtime_Current return ACPI_Local_Runtime_Status.Phase;
   with procedure Begin_Event (Server : State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Server : State; Completed : Boolean; Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure Read_Clock (Now : out Samples.Tick; Available : out Boolean);
   with procedure Report_Publication (Server : State; Kind : ACPI_Local_Sample_Status.Notice;
      Value : Samples.View; Flags : Interfaces.Unsigned_64);
   with function Events_Healthy return Boolean;
   Fuel_Budget : Positive := ACPI_Local_Sample_Status.Default_Fuel;
   Window_Microseconds : ACPI_Local_Sample_Status.Window_Allowance := ACPI_Local_Sample_Status.Default_Window_Microseconds;
-- Fixed-family adapter for the parent's same-Core evaluator/cache. It adds no
-- interpreter, mapping, wire method selector, hardware authority or logger.
-- Bind runtime/clock/health to the same native owner; route every consumer
-- before the broker ACK. Report_Publication is a fast non-IPC copied-fact sink.
package ACPI_Requests.Power_Sources.With_Local_Events is
   package Status renames ACPI_Local_Sample_Status;
   package Samples renames ACPI_Requests.Power_Sources.Samples;
   type Collection_Diagnostic is record
      Search : Controller.Discovery_Report;
      Refresh : Cached.Refresh_Report;
      Charged : Natural := 0;
   end record;
   -- Copied last attempt, not a live/fresh sample or native-health assertion.
   -- Contains no retained handles, endpoints, Core aliases or register pointers.
   function Collection_Report return Collection_Diagnostic;
   procedure Start (Server : State; Accepted : out Boolean) with Pre => Valid(Server);
   function Started return Boolean;
   function Last_Failure return Status.Failure;
   procedure Route (Server : State; Routed : out Boolean) with Pre => Valid(Server);
   procedure Advance (Server : in out State; Result : out Status.Outcome) with Pre => Valid(Server);
   function Read (Server : State; Now : Samples.Clock_Reading; Maximum_Age : Samples.Tick) return Samples.View
     with Pre => Valid(Server);
   -- Stop this consumer only. Backing/shared-owner retirement stays separate.
   procedure Stop;
private
   Snapshot : Collection_Diagnostic;
   procedure Collect (Data : in out Cache; Server : in out State; Budget : Natural;
      Healthy,Collected : out Boolean);
   procedure Publish_Collected (Data : in out Cache; Server : State; At_Time : Samples.Clock_Reading;
      Published : out Boolean; Flags : out Interfaces.Unsigned_64);
   function Read_Cache (Data : Cache; Server : State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View;
   package Owner is new ACPI_Local_Sample_Events
     (Cached.Sample,Cached.Empty_Sample,Samples,Cache,Observe_Claimed,Invalidate,
      Collect,Publish_Collected,Read_Cache,Runtime_Current,Begin_Event,End_Event,
      Read_Clock,Report_Publication,Events_Healthy,Fuel_Budget,Window_Microseconds);
end ACPI_Requests.Power_Sources.With_Local_Events;
