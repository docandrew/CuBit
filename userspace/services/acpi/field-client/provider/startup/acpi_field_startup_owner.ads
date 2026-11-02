pragma Ada_2022;
with ACPI_Capacity_Diagnostics;
with Interfaces;
with ACPI_Field_Clock;
with ACPI_Service_Core;
with ACPI_Global_Lock_Client;
with ACPI_Locked_NVS_Client;
with ACPI_Field_Lease_Scope;
with ACPI_Field_Scoped_Provider;
with ACPI_Global_Lock_Protocol;
with ACPI_Inventory_Protocol;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_NVS_Provider;
with ACPI_Field_Combined_Provider;
with ACPI_Field_Provider_View;
with ACPI_Field_Evaluation;
with AML_Field_Access;
generic
   with package Core is new ACPI_Service_Core (<>);
   type Transport_State is limited private;
   type Endpoint is private;
   No_Endpoint : Endpoint;
   with procedure Read_Clock
     (Now : out ACPI_Inventory_Protocol.Wire.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Transport : in out Transport_State; Destination : Endpoint;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Item : ACPI_Inventory_Protocol.Wire.Packet;
      Reply : out ACPI_Inventory_Protocol.Wire.Packet;
      Delivered : out ACPI_Inventory_Protocol.Wire.Delivery);
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
   Maximum_Confirmations : Positive := AML_Field_Access.Default_Maximum_Confirmations;
   -- Independent microsecond clock: never derive it from the IPC epoch.
   -- Default unavailable preserves NoLock-only behavior for other callers.
   with procedure Read_Lease_Clock
     (Now : out Interfaces.Unsigned_64; Available : out Boolean) is ACPI_Field_Clock.Unavailable;
package ACPI_Field_Startup_Owner is
   package Wire renames ACPI_Inventory_Protocol.Wire;
   type Phase is (Fresh, Provisioning, Ready, Failed, Closed);
   type Report is record
      Stage : Phase := Fresh;
      Module_Attempted, Module_Completed, IO_Pending : Boolean := False;
      Registers : Natural := 0;
      Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
   end record;
   function Observe return Report;
   -- One package instance is one serialized process lifetime. Destination
   -- comes from the trusted typed startup binding, never a client request.
   -- Deadline is absolute MILLISECONDS in the IPC clock epoch; never cast a
   -- snapshot/microsecond deadline. No refresh, retarget, reset or rearm.
   procedure Begin_Startup
     (Destination : Endpoint; Deadline : Wire.Time_Instant; Accepted : out Boolean);
   -- Closing denies further calls but retains provider/uncertainty state.
   procedure Close_Startup;
   procedure Execute_Module_Startup
     (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   -- Callback-compatible trusted entry points. No arena/buffer alias is saved.
   -- Module success precedes these; Initialize_Members precedes first Prepare
   -- or evaluation. The cache never installs a handler or establishes _REG.
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean);
   procedure Evaluate_Named
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_With_Arguments
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   function Uncertain return AML_Field_Access.Observation;
private
   Last_Capacity : ACPI_Capacity_Diagnostics.Report := ACPI_Capacity_Diagnostics.No_Report;
   procedure Record_Capacity_Failure (Item : ACPI_Capacity_Diagnostics.Report);
   procedure Read_Window (Now, Deadline : out Wire.Time_Instant);
   package Scalar is new ACPI_Field_Inventory_Owner
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Read_Window,
      Maximum_Registers, Maximum_Confirmations);
   procedure Read_NVS_Deadline (Deadline : out Wire.Time_Instant);
   package Memory is new ACPI_Field_NVS_Provider
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Read_NVS_Deadline, Maximum_Bindings, Record_Capacity_Failure);
   package Plain is new ACPI_Field_Combined_Provider
     (Endpoint, Scalar, Memory, Read_Clock, Read_Window, Maximum_Registers + Maximum_Bindings, Record_Capacity_Failure);
   function Plain_Ready (IO : Plain.Context) return Boolean;
   package Plain_View is new ACPI_Field_Provider_View
     (Plain.Context, Plain_Ready, Plain.Identity, Plain.Pending, Plain.Register_Count,
      Plain.Export_Target, Plain.Select_Target, Plain.Describe, Plain.Authorize,
      Plain.Dispatch, Plain.Reconciled, Plain.Resolve_Module_Target);
   package Lease is new ACPI_Global_Lock_Client
     (Transport_State, Endpoint, No_Endpoint, Read_Lease_Clock, Read_Clock, Exchange);
   package Locked_NVS is new ACPI_Locked_NVS_Client
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Lease);
   package Lease_Scope is new ACPI_Field_Lease_Scope
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Lease, Locked_NVS,
      Positive (Core.Namespace.Node_ID'Last));
   procedure Read_Lease_Window
     (Lease_Deadline : out Interfaces.Unsigned_64;
      IPC_Deadline : out Wire.Time_Instant; Available : out Boolean);
   procedure Read_Cleanup_Deadline (Deadline : out Wire.Time_Instant);
   package Provider is new ACPI_Field_Scoped_Provider
     (Transport_State, Endpoint, No_Endpoint, Plain.Context, Plain.Initialize,
      Plain_View, Plain.Describe_Scoped_Read, Lease_Scope, Read_Lease_Window,
      Read_NVS_Deadline, Read_Cleanup_Deadline, ACPI_Global_Lock_Protocol.Attempt_Budget'Last);
   function Provider_Ready (IO : Provider.Context) return Boolean;
   package View is new ACPI_Field_Provider_View
     (Provider.Context, Provider_Ready, Provider.Identity, Provider.Pending, Provider.Register_Count,
      Provider.Export_Target, Provider.Select_Target, Provider.Describe, Provider.Authorize,
      Provider.Dispatch, Provider.Reconciled, Provider.Resolve_Module_Target,
      Provider.Begin_Field, Provider.End_Field, Locked_NVS_Reads => True);
   package Evaluation is new ACPI_Field_Evaluation (Core, View, Maximum_Bindings);
   Stage : Phase := Fresh;
   Until_Time, Last_Sample : Wire.Time_Instant := 0;
   Module_Attempted, Module_Completed : Boolean := False;
   IO : Provider.Context;
   Cache : Evaluation.Context;
end ACPI_Field_Startup_Owner;
