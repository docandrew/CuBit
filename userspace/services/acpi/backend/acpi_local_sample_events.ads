pragma Ada_2022;
with Interfaces;
with ACPI_Service;
with ACPI_Requests;
with ACPI_Sample_Cache;
with ACPI_Local_Runtime_Status;
with ACPI_Local_Sample_Status;
generic
   type Sample_Value is private;
   Empty_Sample : Sample_Value;
   with package Model is new ACPI_Sample_Cache (Sample_Value,Empty_Sample);
   type Cache_State is limited private;
   with procedure Observe_Claimed (Data : in out Cache_State; Server : ACPI_Requests.State; Observed : out Boolean);
   with procedure Invalidate (Data : in out Cache_State);
   -- Fixed family operations only. Collect may run AML but must preserve the
   -- Core/catalog and leave retained-result cleanup for End_Event to verify.
   -- No dequeue, receipt claim/ACK, reentry or other producer interleaving.
   with procedure Collect (Data : in out Cache_State; Server : in out ACPI_Requests.State;
      Budget : Natural; Healthy,Collected : out Boolean);
   with procedure Publish (Data : in out Cache_State; Server : ACPI_Requests.State;
      At_Time : Model.Clock_Reading; Published : out Boolean; Flags : out Interfaces.Unsigned_64);
   with function Cached_Read (Data : Cache_State; Server : ACPI_Requests.State;
      Now : Model.Clock_Reading; Maximum_Age : Model.Tick) return Model.View;
   with function Runtime_Current return ACPI_Local_Runtime_Status.Phase;
   with procedure Begin_Event (Server : ACPI_Requests.State; Deadline_Microseconds : Interfaces.Unsigned_64;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Server : ACPI_Requests.State; Completed : Boolean;
      Result : out ACPI_Local_Runtime_Status.Outcome);
   with procedure Read_Clock (Now : out Model.Tick; Available : out Boolean);
   -- Called only after a successful window close, no pending notifications,
   -- and an idle runtime. May use the existing logger. No AML/IPC/reentry or
   -- Server mutation; consume only this copied observation and return normally.
   with procedure Report_Publication (Server : ACPI_Requests.State; Kind : ACPI_Local_Sample_Status.Notice;
      Value : Model.View; Flags : Interfaces.Unsigned_64);
   -- Includes source stop/loss/overflow/dependency failure. Read-only, no IPC
   -- or reentry. Main binds the actual enrolled-source health, not a constant.
   with function Events_Healthy return Boolean;
   Fuel_Budget : Positive := ACPI_Local_Sample_Status.Default_Fuel;
   Window_Microseconds : ACPI_Local_Sample_Status.Window_Allowance := ACPI_Local_Sample_Status.Default_Window_Microseconds;
-- One serialized process-lived event consumer. It owns no interpreter,
-- provider, mapping or backing. Bind the same local request/runtime instance
-- as startup. All callbacks are trusted, bounded and normally returning.
package ACPI_Local_Sample_Events is
   package Status renames ACPI_Local_Sample_Status;
   package Samples renames Model;
   procedure Start (Server : ACPI_Requests.State; Accepted : out Boolean)
     with Pre => ACPI_Requests.Valid(Server);
   function Started return Boolean;
   function Last_Failure return Status.Failure;
   procedure Route (Server : ACPI_Requests.State; Routed : out Boolean)
     with Pre => ACPI_Requests.Valid(Server);
   -- At most one collection or publication per turn. Route/ACK all consumers
   -- between turns. Query-generated notifications invalidate publication but
   -- cannot create an automatic resampling loop. Later external receipts can
   -- coalesce one new collection even while publication is pending.
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Status.Outcome)
     with Pre => ACPI_Requests.Valid(Server);
   function Read (Server : ACPI_Requests.State; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View
     with Pre => ACPI_Requests.Valid(Server);
   -- Closes this consumer only; no cancellation, result release, provider
   -- close or backing retirement. Native Main separately stops the shared owner.
   procedure Stop;
private
   type Phase is (Fresh, Ready, Publishing, Stopped);
   State : Phase := Fresh;
   Failure : Status.Failure := Status.None;
   Attempted, Working, Needed, Collected, Resample : Boolean := False;
   Have_Clock : Boolean := False;
   Last_Clock : Samples.Tick := 0;
   Collection_Time : Samples.Clock_Reading;
   Anchor : ACPI_Service.Context_Key := ACPI_Service.No_Context;
   Last_Receipt : ACPI_Requests.Receipt_Number := 0;
   Self_Notifications : ACPI_Service.Notification_Count := 0;
   Data : Cache_State;
   procedure Fail (Why : Status.Failure; Result : out Status.Outcome);
end ACPI_Local_Sample_Events;
