pragma Ada_2022;
with AML_Global_Locks;
with AML_Frame_Handles;
with AML_Mutexes;
with ACPI_AML_Global_Lock;
with AML_Field_Notes;
with ACPI_Service_Core;
with ACPI_Local_Register_Provider;
with ACPI_Field_Provider_View;
with ACPI_Field_Evaluation;
with AML_Execute;
with AML_Field_Access;
with ACPI_Local_Field_Scope;
with ACPI_Platform_Region_Cache;
with ACPI_Platform_Field_Provider;
with ACPI_Local_Field_Mux;
with ACPI_Local_Field_Mux.With_EC;
with ACPI_Local_Field_Mux.With_EC.With_CMOS;
with ACPI_Local_CMOS_Provider;
with ACPI_Native_CMOS_Ports;
with ACPI_CMOS_Transactions;
with ACPI_Local_EC_Commands;
with ACPI_Local_EC_Field_Provider;
with ACPI_EC_Types;
with ACPI_Local_NVS_Provider;
with ACPI_Native_PCI_Provider;
with ACPI_PCI_Bus_Geometry;
with ACPI_NVS_Writes;
with AML_Names;
with AML_Field_Layout;
generic
   with package Core is new ACPI_Service_Core (<>);
   with package Maps is new ACPI_Platform_Region_Cache (<>);
   Memory : in out Maps.Context;
   with function Compatible_Envelope (First_Page, Pages : Word) return Boolean;
   with procedure Begin_Window (Deadline_Microseconds : out Word;
      Budget : out Lock_Protocol.Attempt_Budget; Available : out Boolean);
   Maximum_Depth : Positive;
   Maximum_Registers : Positive := AML_Field_Access.Default_Maximum_Registers;
   Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
   with procedure PCI_Setup_Deadline (Deadline_Milliseconds : in out Word; Available : in out Boolean) is null;
   with procedure Resolve_PCI_Alias (Geometry : AML_Field_Layout.Register_Geometry;
      Key : in out ACPI_PCI_Bus_Geometry.P.Bus_Key; Accepted : in out Boolean) is null;
   with procedure Inspect_IO_Window (Deadline_Microseconds : out Word; Available : out Boolean);
   Metadata_Capacity_Ceiling : Positive := 65_536;
   Metadata_Byte_Ceiling : Word := Word(64 * 1024 * 1024);
package ACPI_Local_Hardware.Pooled_Fields is
   package EC_Types renames ACPI_EC_Types;
   type EC_Boot_Status is (EC_Not_Inspected,EC_Absent,EC_Multiple,EC_Inspection_Limit,
      EC_Description_Refused,EC_Registers_Refused,EC_Path_Refused,EC_Port_Outside_Grant,EC_Ready);
   type EC_Boot_Report is record
      Status : EC_Boot_Status := EC_Not_Inspected;
      Control, Data : Word := 0;
      UID : Interfaces.Unsigned_32 := 0;
      GPE : EC_Types.Byte := 0;
   end record;
   function EC_Boot_Observation return EC_Boot_Report;
   -- Actual composed handler/cache readiness, not ECDT metadata alone.
   -- Does not authorize any SystemMemory alias used by firmware REG bodies.
   function EC_Handler_Available return Boolean;
   function CMOS_Handler_Available return Boolean;
   type Field_Sequence is new Interfaces.Unsigned_64;
   type Field_Trace_Report is record
      Sequence : Field_Sequence := 0;
      Saturated : Boolean := False;
      Item : AML_Field_Notes.Event;
   end record;
   function Field_Observation return Field_Trace_Report;
   -- Trusted local implementation actuals, not wire calls. Exact command/data
   -- ports must first match copied ECDT metadata AND existing grant windows.
   -- Until namespace sharing policy is confirmed, EVERY command takes the
   -- existing physical Global Lock; no exclusive-ownership claim is guessed.
   procedure EC_Read (Register : EC_Types.Read_Register; Value : out EC_Types.Byte;
      Result : out EC_Types.Access_Result);
   procedure EC_Write (Register : EC_Types.Write_Register; Value : EC_Types.Byte;
      Result : out EC_Types.Access_Result);
   procedure EC_Clock (Now : out EC_Types.Microseconds; Available : out Boolean);
   -- Same clock epoch and active owner window as EC Field command pacing.
   -- Local source owner only; does not renew a deadline or admit hardware.
   procedure Wait_EC (Target : EC_Types.Microseconds; Available : out Boolean);
   procedure EC_Enter (For_Query,For_Write : Boolean; Deadline : EC_Types.Microseconds; Accepted : out Boolean);
   procedure EC_Leave (Released : out Boolean);
   function EC_Quiet return Boolean;
   function EC_Owned return Boolean;
   package EC_Commands is new ACPI_Local_EC_Commands
     (EC_Read,EC_Write,EC_Clock,EC_Enter,EC_Leave,EC_Quiet,EC_Owned);
   type Field_Phase is (Owner_Fresh, Owner_Ready, Owner_Failed, Owner_Closed);
   type Field_Report is record
      Stage : Field_Phase := Owner_Fresh;
      Module_Attempted, Module_Completed, Startup_Finished, IO_Pending : Boolean := False;
      Registers : Natural := 0;
   end record;
   function Observe return Field_Report;
   -- Copied diagnostics only; none of these values authorizes hardware.
   function AML_Lock_Last_Result return AML_Global_Locks.Outcome;
   function AML_Lock_First_Failure return AML_Global_Locks.Outcome;
   function AML_Lock_Had_Failure return Boolean;
   function AML_Lock_Attempts return Natural;
   type Preparation_Outcome is (Not_Prepared, Prepared, Runtime_Already_Claimed,
      Startup_Owner_Mismatch, Provider_Not_Ready, Binding_Metadata_Refused,
      Provider_IO_Pending, Binding_Namespace_Unavailable,
      Binding_Provider_Unavailable, Binding_Pending_IO,
      Binding_Export_Refused, Binding_Cache_Not_Published,
      Binding_Size_Unavailable, Binding_Capacity_Exceeded, Binding_Allocation_Refused);
   type Preparation_Report is record
      Status : Preparation_Outcome := Not_Prepared;
      Module_Completed, Anchor_Present, Core_Valid, Context_Current : Boolean := False;
      Owner_Ready, Scope_Ready, Mux_Ready, Hardware_Admitting : Boolean := False;
      Binding_Limit : Natural := 0;
      Metadata_Bytes, Allocated_Metadata_Bytes : Word := 0;
      Failed_Node : Core.Namespace.Node_ID := Core.Namespace.Root;
      Access_Failure : Core.Values.Access_Status := Core.Values.Available;
      Region_Entries, Read_Fields, Write_Fields, Missing_Fields : Natural := 0;
   end record;
   -- Copied observations only, including the exact guard that refused Prepare.
   -- Reading this report neither retries preparation nor changes authority.
   function Preparation_Observation return Preparation_Report;
   procedure Begin_Startup (Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean);
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status);
   procedure Initialize_Member_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Namespace.Field_Initialization_Report; Status : out Core.Values.Access_Status);
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
   function Check_Field_Access (For_Write : Boolean) return Boolean;
   function Control_Idle return Boolean;
   -- No active frame, held lease, cleanup obligation or pending provider IO.
   -- Useful for explicit startup-carriage disposal after admission closes;
   -- this is not a whole-service retirement or hardware-authority predicate.
   -- Root binds this exact predicate to its native pool cache; never inspect
   -- this provider/cache's Working flags inside the predicate.
   -- Close only denies entry; retains all cache, hardware and uncertain state.
   -- Named registers, PCI, NVS and pool targets share one issuer/physical scope.
   -- Root initializes the sole PCI window owner/arena and supplies deadlines
   -- and pure alias metadata; defaults enable no PCI acquisition. This child
   -- does not manufacture _REG readiness. All backing stays retained.
private
   package Scalar is new ACPI_Local_Register_Provider (Catalog,Hardware,Local_Scope,Maximum_Registers);
   package Scope is new ACPI_Local_Field_Scope
      (Lease,Admitting,Needs_Cleanup,Begin_Window,Acquire,Check_Access,Release,Maximum_Depth,Inspect_IO_Window);
   Transaction : Scope.Context;
   procedure AML_Lock_Clock (Now : out Word; Available : out Boolean);
   procedure AML_Lock_Wait (Target : Word; Completed : out Boolean);
   -- Per-invocation work ceiling, independent of the AML timeout value.
   -- Exhaustion is refusal, never a fabricated successful/elapsed timeout.
   AML_Lock_Maximum_Wait_Slices : constant Positive := 64;
   AML_Lock_Slice_Microseconds : constant Word := 1_000;
   package AML_Lock is new ACPI_AML_Global_Lock
     (Scope, Transaction, AML_Lock_Clock, Inspect_IO_Window, AML_Lock_Wait,
      AML_Lock_Maximum_Wait_Slices, AML_Lock_Slice_Microseconds);
   AML_Lock_State : AML_Lock.Context;
   AML_Lock_Good_Logged, AML_Lock_Failure_Seen : Boolean := False;
   First_AML_Lock_Failure : AML_Global_Locks.Outcome := AML_Global_Locks.Unavailable;
   type Declaration is record
      Region : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment;
      Node : Core.Namespace.Node_ID;
   end record;
   procedure Validate_Declaration (Item : Declaration; Geometry : AML_Field_Layout.Register_Geometry;
      Origin : out Maps.Source; Accepted : out Boolean);
   package Platform is new ACPI_Platform_Field_Provider
      (Maps,Memory,Declaration,Validate_Declaration,Compatible_Envelope);
   procedure Write_NVS (Physical,Virtual : Word; Width : NVS.T.Access_Width;
      Value : Word; Perform : Boolean; Result : in out ACPI_NVS_Writes.Completion);
   package Named_Data is new ACPI_Local_NVS_Provider
      (NVS,NVS_State,Local_Scope,Check_Field_Access,Write_Access => Write_NVS);
   function PCI_Setup_Allowed return Boolean;
   procedure PCI_Deadlines (Setup_Milliseconds,Data_Microseconds : out Word; Available : out Boolean);
   package Configuration is new ACPI_Native_PCI_Provider
      (Local_Scope,PCI_Setup_Allowed,Check_Field_Access,PCI_Deadlines,Resolve_PCI_Alias);
   function EC_Cell_Allowed (Cell : EC_Types.Cell_Offset; For_Write : Boolean) return Boolean;
   procedure EC_Deadline (Until_Time : out EC_Types.Microseconds; Available : out Boolean);
   procedure EC_Pause (Target : EC_Types.Microseconds; Available : out Boolean);
   EC_Command_Work : constant EC_Types.Work_Budget := 10_000;
   package EC_Provider is new ACPI_Local_EC_Field_Provider
     (EC_Commands,EC_Cell_Allowed,EC_Deadline,EC_Pause,EC_Command_Work);
   package Ordinary_Mux is new ACPI_Local_Field_Mux
      (Declaration,Scalar,Platform,Named_Data,Configuration.Provider,Check_Field_Access);
   procedure CMOS_Transfer (Plan : ACPI_CMOS_Transactions.Transfer;
      Value : out ACPI_CMOS_Transactions.I.Byte; Outcome : out ACPI_CMOS_Transactions.Completion);
   package CMOS_Provider is new ACPI_Local_CMOS_Provider
     (ACPI_Native_CMOS_Ports.Read_Configuration, Check_Field_Access, CMOS_Transfer);
   package EC_Mux is new Ordinary_Mux.With_EC (EC_Provider);
   package Mux is new EC_Mux.With_CMOS (CMOS_Provider);
   EC_Info : EC_Boot_Report;
   EC_Control_Ports,EC_Data_Ports : CuBit.Handles.IO_Ports := CuBit.Handles.No_IO_Ports;
   EC_Frame_Open,EC_Guard_Retained : Boolean := False;
   EC_Outer_Depth : Scope.Depth_Count := 0;
   procedure Inspect_EC_Boot (Service : Core.State);
   function Provider_Ready (IO : Mux.Context) return Boolean;
   function Provider_Pending (IO : Mux.Context) return Boolean;
   procedure Resolve_Module (IO : in out Mux.Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
   procedure Resolve_Platform (IO : in out Mux.Context; Region : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment; Node : Core.Namespace.Node_ID; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean);
   procedure Operate_AML_Lock
     (IO : in out Mux.Context; Domain : AML_Frame_Handles.Invocation_Domain;
      Action : AML_Global_Locks.Operation; Timeout : AML_Mutexes.Timeout;
      Outcome : in out AML_Global_Locks.Outcome);
   procedure Begin_Field (IO : in out Mux.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean);
   procedure End_Field (IO : in out Mux.Context; Accepted : in out Boolean);
   Last_Field_Trace : Field_Trace_Report;
   procedure Note_Field (Item : AML_Field_Notes.Event);
   procedure Dispatch_Profile (IO : in out Mux.Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response);
   package View is new ACPI_Field_Provider_View
     (Mux.Context,Provider_Ready,Mux.Identity,Provider_Pending,Mux.Register_Count,
      Mux.Export_Target,Mux.Select_Target,Mux.Describe,Mux.Authorize,Dispatch_Profile,Mux.Reconciled,
      Resolve_Target => Resolve_Module,Begin_Transaction => Begin_Field,End_Transaction => End_Field,
      Allow_Locked_Fields => True, Observe_Field => Note_Field, Field_Notes_Enabled => True,
      Global_Lock_Operation => Operate_AML_Lock);
   package Evaluation is new ACPI_Field_Evaluation (Core,View,Maximum_Bindings,
      Enable_Startup_Platform_Resolution => True,Resolve_Startup_Platform_Target => Resolve_Platform);
   IO : Mux.Context;
   subtype Cache_Access is Evaluation.Context_Access;
   Cache : Cache_Access := null;
   Cache_Attempted : Boolean := False;
   Cache_Capacity : Natural := 0;
   Cache_Bytes, Allocated_Cache_Bytes : Word := 0;
   Cache_Status : Preparation_Outcome := Not_Prepared;
   function Ensure_Cache(Service:Core.State) return Boolean;
   Last_Preparation : Preparation_Report;
   Field_State : Field_Phase := Owner_Fresh;
   Attempted, Completed, Finished, Has_Anchor : Boolean := False;
   Members_Attempted, Members_Completed : Boolean := False;
   Runtime_Claimed : Boolean := False;
   Anchor : Core.Context_Key := Core.No_Context;
   function Owns (Service : Core.State) return Boolean;
   function Runtime_Ready (Service : Core.State) return Boolean;
   function Runtime_Idle return Boolean;
   procedure Claim_Runtime (Service : Core.State; Accepted : out Boolean);
   procedure Runtime_Evaluate
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Runtime_Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
   procedure Runtime_Invoke_Values
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status);
end ACPI_Local_Hardware.Pooled_Fields;
