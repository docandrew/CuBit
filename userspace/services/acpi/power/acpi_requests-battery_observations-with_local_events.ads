pragma Ada_2022;
with Interfaces;
with ACPI_Battery_Inventory;
with ACPI_Local_Runtime_Status;
with ACPI_Local_Sample_Status;
with ACPI_Local_Sample_Events;
generic
   with package Inventory is new ACPI_Battery_Inventory (Collector=>Collector,others=><>);
   with function Runtime_Current return ACPI_Local_Runtime_Status.Phase;
   with procedure Begin_Event (Server:State;Deadline_Microseconds:Interfaces.Unsigned_64;Result:out ACPI_Local_Runtime_Status.Outcome);
   with procedure End_Event (Server:State;Completed:Boolean;Result:out ACPI_Local_Runtime_Status.Outcome);
   with procedure Read_Clock (Now:out Inventory.Samples.Tick;Available:out Boolean);
   with procedure Report_Publication (Server:State;Kind:ACPI_Local_Sample_Status.Notice;
      Value:Inventory.Samples.View;Flags:Interfaces.Unsigned_64);
   with function Events_Healthy return Boolean;
   Fuel_Budget:Positive:=ACPI_Local_Sample_Status.Default_Fuel;
   Window_Microseconds:ACPI_Local_Sample_Status.Window_Allowance:=ACPI_Local_Sample_Status.Default_Window_Microseconds;
   -- Validated discovered descriptor only; callback copies metadata and may
   -- not perform IPC/AML, mutate Core or reenter this collector.
   with procedure Remember_Device (Device : ACPI_Service.Node_Descriptor) is null;
-- Same private request Core and fixed battery collector. Complete inventory
-- publication only; per-entry failure/absence remains explicit. No implicit
-- retry, source dequeue/ACK, hardware grant or shared-owner retirement.
package ACPI_Requests.Battery_Observations.With_Local_Events is
   package Status renames ACPI_Local_Sample_Status;
   package Samples renames Inventory.Samples;
   type Collection_Outcome is (Not_Attempted,Complete,Search_Failed,Budget_Exhausted,
      Inventory_Limit,Descriptor_Failed,Observation_Refused,Recovery_Required,
      Binding_Changed,Counter_Exhausted,Provider_Contract_Error,Source_Unhealthy);
   type Collection_Diagnostic is record
      Outcome:Collection_Outcome:=Not_Attempted;
      Count:Inventory.Battery_Count:=0;
      Charged:Natural:=0;
      Search:Controller.Discovery_Report;
      Probe:Controller.Observation_Report;
   end record;
   -- Copied last attempt; partial count/reports are not a publishable sample.
   function Collection_Report return Collection_Diagnostic;
   procedure Start(Server:State;Accepted:out Boolean) with Pre=>Valid(Server);
   function Started return Boolean;
   function Last_Failure return Status.Failure;
   procedure Route(Server:State;Routed:out Boolean) with Pre=>Valid(Server);
   procedure Advance(Server:in out State;Result:out Status.Outcome) with Pre=>Valid(Server);
   function Read(Server:State;Now:Samples.Clock_Reading;Maximum_Age:Samples.Tick) return Samples.View with Pre=>Valid(Server);
   function Owns_Context(Server:State) return Boolean with Pre=>Valid(Server);
   procedure Request_Refresh(Server:State;At_Time:Samples.Clock_Reading;Accepted:out Boolean)
     with Pre=>Valid(Server);
   function Metadata_View(Server:State) return Samples.View with Pre=>Valid(Server);
   function Metadata_Revision(Server:State) return Samples.Revision_Number with Pre=>Valid(Server);
   procedure Stop;
private
   type Storage is limited record
      Values:Samples.State;
      Pending_Value:Inventory.Observation:=Inventory.Empty_Observation;
      Expected:Samples.Revision_Number:=0;
      Armed:Boolean:=False;
   end record;
   Snapshot:Collection_Diagnostic;
   function Collection_Isolatable return Boolean;
   procedure Invalidate(Data:in out Storage);
   procedure Observe_Claimed(Data:in out Storage;Server:State;Observed:out Boolean);
   function Current(Value:Inventory.Observation;Server:State) return Boolean;
   procedure Collect(Data:in out Storage;Server:in out State;Budget:Natural;Healthy,Collected:out Boolean);
   procedure Publish(Data:in out Storage;Server:State;At_Time:Samples.Clock_Reading;Published:out Boolean;Flags:out Interfaces.Unsigned_64);
   function Cached_Read(Data:Storage;Server:State;Now:Samples.Clock_Reading;Maximum_Age:Samples.Tick) return Samples.View;
   package Owner is new ACPI_Local_Sample_Events
     (Inventory.Observation,Inventory.Empty_Observation,Samples,Storage,Observe_Claimed,Invalidate,
      Collect,Publish,Cached_Read,Runtime_Current,Begin_Event,End_Event,Read_Clock,Report_Publication,Events_Healthy,Fuel_Budget,Window_Microseconds,
      Collection_Isolatable);
end ACPI_Requests.Battery_Observations.With_Local_Events;
