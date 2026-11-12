pragma Ada_2022;
with ACPI_Service_Core;
with ACPI_Local_Register_Provider;
with ACPI_Field_Provider_View;
with ACPI_Field_Evaluation;
with AML_Execute;
with AML_Field_Access;
with ACPI_Local_Field_Scope;
generic
   with package Core is new ACPI_Service_Core (<>);
   with procedure Begin_Window (Deadline_Microseconds : out Word;
      Budget : out Lock_Protocol.Attempt_Budget; Available : out Boolean);
   Maximum_Depth : Positive;
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
package ACPI_Local_Hardware.Fields is
   type Field_Phase is (Owner_Fresh, Owner_Ready, Owner_Failed, Owner_Closed);
   type Field_Report is record
      Stage : Field_Phase := Owner_Fresh;
      Module_Attempted, Module_Completed, Startup_Finished, IO_Pending : Boolean := False;
      Registers : Natural := 0;
   end record;
   function Observe return Field_Report;
   procedure Begin_Startup (Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean);
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean);
   procedure Evaluate_Named (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_Retained (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Invoke_With_Arguments (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status)
      with Pre => Core.Valid (Service) and then not Result'Constrained;
   procedure Finish_Startup (Accepted : out Boolean);
   -- Closes startup/module replay while keeping same-owner runtime evaluation
   -- available for later device/power events. This is not region readiness.
   procedure Close;
   procedure Cleanup (Completed : out Boolean);
   function Uncertain return AML_Field_Access.Observation;
   -- Close only denies entry; retains all cache, hardware and uncertain state.
   -- Whole-Field lease scope uses the parent's existing physical lock. Each
   -- datum rechecks its lease. Dynamic pool and named NVS providers remain
   -- separate pending composition; this scalar bridge grants neither.
private
   package Scalar is new ACPI_Local_Register_Provider (Catalog,Hardware,Local_Scope,Maximum_Registers);
   package Scope is new ACPI_Local_Field_Scope
      (Lease,Admitting,Needs_Cleanup,Begin_Window,Acquire,Check_Access,Release,Maximum_Depth);
   Transaction : Scope.Context;
   function Scalar_Ready (IO : Scalar.Context) return Boolean;
   function Scalar_Pending (IO : Scalar.Context) return Boolean;
   procedure Dispatch (IO : in out Scalar.Context; Item : AML_Field_Access.Request;
      Reply : out AML_Field_Access.Response);
   procedure Begin_Field (IO : in out Scalar.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure End_Field (IO : in out Scalar.Context; Accepted : in out Boolean);
   package View is new ACPI_Field_Provider_View
     (Scalar.Context,Scalar_Ready,Scalar.Identity,Scalar_Pending,Scalar.Register_Count,
      Scalar.Export_Target,Scalar.Select_Target,Scalar.Describe,Scalar.Authorize,
      Dispatch,Scalar.Reconciled,Begin_Transaction => Begin_Field,End_Transaction => End_Field,
      Allow_Locked_Fields => True);
   package Evaluation is new ACPI_Field_Evaluation (Core,View,Maximum_Bindings);
   IO : Scalar.Context;
   Cache : Evaluation.Context;
   Field_State : Field_Phase := Owner_Fresh;
   Attempted, Completed, Finished, Has_Anchor : Boolean := False;
   Anchor : Core.Node_Descriptor;
   function Owns (Service : Core.State) return Boolean;
end ACPI_Local_Hardware.Fields;
