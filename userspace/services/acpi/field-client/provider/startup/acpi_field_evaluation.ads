pragma Ada_2022;
with AML_Names;
with Interfaces;with System;with ACPI_Fallible_Heap;
with AML_Execute;
with ACPI_Service_Core;
with ACPI_Field_Provider_View;
with ACPI_Field_Bindings;
with AML_Field_Access;
with AML_Field_Layout;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Provider is new ACPI_Field_Provider_View (<>);
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
   -- Explicit startup-only forwarding; default remains disabled. The Core
   -- validates the live original definition before invoking this callback.
   Enable_Startup_Firmware_Resolution : Boolean := False;
   with procedure Resolve_Startup_Firmware_Target
     (IO : in out Provider.Context; Declaration : Core.Namespace.Operation_Region_Attributes;
      Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is null;
   Enable_Startup_Platform_Resolution : Boolean := False;
   with procedure Resolve_Startup_Platform_Target
     (IO : in out Provider.Context; Declaration : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment; Node : Core.Namespace.Node_ID;
      Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is null;
   with function Reserve_Cache(Bytes,Alignment:Interfaces.Unsigned_64) return System.Address
      is ACPI_Fallible_Heap.Reserve;
   with procedure Observe_Region_Handler
     (IO : Provider.Context; Region : Core.Namespace.Node_ID;
      Data : Core.Namespace.Operation_Region_Attributes;
      Ready : in out Boolean; Generation : in out Natural) is null;
package ACPI_Field_Evaluation is
   package Bindings is new ACPI_Field_Bindings (Core, Provider, Maximum_Bindings, Enable_Startup_Firmware_Resolution,
      Resolve_Startup_Firmware_Target, Enable_Startup_Platform_Resolution,
      Resolve_Startup_Platform_Target, Observe_Region_Handler => Observe_Region_Handler);
   Default_Capacity : constant Positive := Maximum_Bindings;
   type Context (Capacity : Positive) is limited private;
   type Context_Access is access all Context;
   type Allocation_Outcome is (Allocated,Size_Unavailable,Capacity_Exceeded,Allocation_Refused);
   type Allocation_Report is record
      Status:Allocation_Outcome:=Size_Unavailable;
      Capacity:Natural:=0;
      Bytes,Allocated_Bytes:Interfaces.Unsigned_64:=0;
   end record;
   -- Trusted process-lifetime owner allocation; no AML/Field IO or retry.
   -- Caller keeps the pointer through all borrowers and uncertain cleanup.
   procedure Allocate(Service:Core.State;IO:Provider.Context;
      Capacity_Ceiling:Positive;Byte_Ceiling:Interfaces.Unsigned_64;
      Run:out Context_Access;Report:out Allocation_Report);
   function Observe (Run : Context) return Bindings.Report;
   -- Adapter for the trusted Device_Initialization evaluator callback. Build
   -- the metadata cache once, after Initialize_Members and before the first
   -- root/_SB._INI. Callers with no such method may Prepare at their region
   -- barrier. This never establishes region readiness or runs _REG.
   -- IO and Service remain caller-owned; no alias or buffer is retained.
   procedure Prepare
     (Run : in out Context; Service : Core.State; IO : in out Provider.Context;
      Accepted : out Boolean);
   procedure Evaluate_Named
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Integer arguments use the same selected provider and current bindings.
   procedure Invoke_Retained
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- Arguments borrow this exact arena's retained values. The underlying
   -- evaluator owns result retention and cleanup; this adapter copies none.
   procedure Invoke_With_Arguments
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result;
      Status : out Core.Values.Access_Status)
     with Pre => Core.Valid (Service) and then not Result'Constrained;
   -- A service-loop turn borrows the already published binding set. The
   -- trusted caller supplies a fresh local handler-install acknowledgment.
   procedure Drain_Region_Connections
     (Run : in out Context; Service : aliased in out Core.State;
      IO : in out Provider.Context; Turn_ID, Handler_Generation : Positive;
      Install_Acknowledged : Boolean;
      Report : out Bindings.Fields.Connection_Turn_Report)
     with Pre => Core.Valid (Service);
private
   type Context (Capacity : Positive) is limited record
      Cache : Bindings.Context (Capacity);
   end record;
end ACPI_Field_Evaluation;
