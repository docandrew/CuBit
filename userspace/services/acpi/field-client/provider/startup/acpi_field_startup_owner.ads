pragma Ada_2022;
with ACPI_Field_Diagnostics;
with AML_Execute;
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
with ACPI_Field_PCI_Provider;
with ACPI_PCI_Protocol;
with ACPI_Field_Combined_Provider;
with ACPI_Field_Provider_View;
with ACPI_Field_Evaluation;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Field_Routes;
with ACPI_Field_Transactions;
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
   Enable_PCI : Boolean := False;
   with procedure Resolve_PCI_Alias
     (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Protocol.P.Bus_Key; Accepted : in out Boolean) is null;
   -- One package instance owns one startup lifetime. Firmware actuals retain
   -- their own singleton session; they never own another physical guard.
   Enable_Firmware_Data : Boolean := False;
   with procedure Initialize_Firmware
     (Destination : Endpoint; Symbol : AML_Field_Access.Endpoint_ID;
      Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant; Accepted : in out Boolean) is null;
   with procedure Inspect_Firmware
     (Is_Ready, Is_Pending : in out Boolean; Observation : in out AML_Field_Access.Observation;
      Capacity : in out ACPI_Capacity_Diagnostics.Report) is null;
   with procedure Describe_Firmware
     (Target : AML_Field_Access.Target; Geometry : in out AML_Field_Layout.Register_Geometry;
      Accepted : in out Boolean) is null;
   with procedure Authorize_Firmware
     (Target : AML_Field_Access.Target; For_Write : Boolean; Accepted : in out Boolean) is null;
   with procedure Dispatch_Firmware
     (Item : AML_Field_Access.Request; Reply : in out AML_Field_Access.Response) is null;
   with procedure Resolve_Firmware
     (Declaration : Core.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is null;
   with procedure Begin_Firmware
     (Requires_Lock, For_Write : Boolean; Deadline : ACPI_Inventory_Protocol.Wire.Time_Instant;
      Ticket : ACPI_Global_Lock_Protocol.Ticket; Accepted : in out Boolean) is null;
   with procedure End_Firmware (Accepted : in out Boolean) is null;
   with procedure Cleanup_Firmware (Accepted : in out Boolean) is null;
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
   function First_Failure return ACPI_Field_Diagnostics.Report;
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
   -- Integer arguments use the same selected provider and current bindings.
   procedure Invoke_Retained
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
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
   First : ACPI_Field_Diagnostics.Report;
   procedure Record_Capacity_Failure (Item : ACPI_Capacity_Diagnostics.Report);
   procedure Read_Window (Now, Deadline : out Wire.Time_Instant);
   package Scalar is new ACPI_Field_Inventory_Owner
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Read_Window,
      Maximum_Registers, Maximum_Confirmations);
   procedure Read_NVS_Deadline (Deadline : out Wire.Time_Instant);
   package Memory is new ACPI_Field_NVS_Provider
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Exchange, Read_NVS_Deadline, Maximum_Bindings, Record_Capacity_Failure);
   procedure Read_PCI_Clock
     (Now : out ACPI_PCI_Protocol.Microsecond_Deadline; Available : out Boolean);
   procedure Read_PCI_Deadline
     (IPC_Deadline : out Wire.Time_Instant;
      Data_Deadline : out ACPI_PCI_Protocol.Microsecond_Deadline);
   package PCI is new ACPI_Field_PCI_Provider
     (Transport_State, Endpoint, No_Endpoint, Read_Clock, Read_PCI_Clock,
      Exchange, Read_PCI_Deadline, Maximum_Bindings, Record_Capacity_Failure);
   package Plain is new ACPI_Field_Combined_Provider
     (Endpoint, Scalar, Memory, PCI, Read_Clock, Read_Window, Maximum_Registers + Maximum_Bindings,
      Record_Capacity_Failure, Enable_PCI, Resolve_PCI_Alias);
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
      Read_NVS_Deadline, Read_Cleanup_Deadline, ACPI_Global_Lock_Protocol.Attempt_Budget'Last,
      Prepare_Base => Plain.Prepare_Target);
   type Lane is (Existing_Lane, Firmware_Lane);
   procedure Lane_Describe (IO : Provider.Context; Path : Lane; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Lane_Authorize (IO : Provider.Context; Path : Lane; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Lane_Dispatch (IO : in out Provider.Context; Path : Lane;
      Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   package Routes is new ACPI_Field_Routes (Provider.Context,Lane,Lane_Describe,Lane_Authorize,Lane_Dispatch);
   procedure Data_Begin (IO : in out Provider.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure Data_End (IO : in out Provider.Context; Accepted : in out Boolean);
   procedure Data_Cleanup (IO : in out Provider.Context; Accepted : out Boolean);
   package Hooks is new ACPI_Field_Transactions
      (Provider.Context,Provider.Begin_Field,Data_Begin,Data_End,Provider.End_Field,Data_Cleanup,Provider.Cleanup);
   type Root_Context is limited record
      Physical : Provider.Context;
      Routing : Routes.Context;
      Transactions : Hooks.Context;
   end record;
   function Root_Ready (IO : Root_Context) return Boolean;
   function Root_Pending (IO : Root_Context) return Boolean;
   function Root_Identity (IO : Root_Context) return AML_Identity.Identity;
   function Root_Count (IO : Root_Context) return Natural;
   procedure Root_Export (IO : Root_Context; Ordinal : Positive; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   procedure Root_Select (IO : in out Root_Context; Geometry : AML_Field_Layout.Register_Geometry;
      For_Write : Boolean; Target : out AML_Field_Access.Target; Accepted : out Boolean);
   procedure Root_Describe (IO : Root_Context; Target : AML_Field_Access.Target;
      Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
   procedure Root_Authorize (IO : Root_Context; Target : AML_Field_Access.Target;
      For_Write : Boolean; Accepted : out Boolean);
   procedure Root_Dispatch (IO : in out Root_Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   procedure Root_Confirmed (IO : Root_Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
   procedure Root_Resolve_Module (IO : in out Root_Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
   procedure Root_Resolve_Firmware (IO : in out Root_Context; Declaration : Core.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry; Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
   procedure Root_Begin (IO : in out Root_Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure Root_End (IO : in out Root_Context; Accepted : in out Boolean);
   procedure Initialize_Root (Destination : Endpoint; Deadline : Wire.Time_Instant; Accepted : out Boolean);
   function Current_Capacity return ACPI_Capacity_Diagnostics.Report;
   package View is new ACPI_Field_Provider_View
     (Root_Context,Root_Ready,Root_Identity,Root_Pending,Root_Count,Root_Export,Root_Select,
      Root_Describe,Root_Authorize,Root_Dispatch,Root_Confirmed,Root_Resolve_Module,Root_Begin,Root_End,
      Locked_NVS_Reads => True);
   package Evaluation is new ACPI_Field_Evaluation
      (Core,View,Maximum_Bindings,Enable_Firmware_Data,Root_Resolve_Firmware);
   Stage : Phase := Fresh;
   Until_Time, Last_Sample : Wire.Time_Instant := 0;
   Module_Attempted, Module_Completed : Boolean := False;
   IO : Root_Context;
   Cache : Evaluation.Context;
end ACPI_Field_Startup_Owner;
