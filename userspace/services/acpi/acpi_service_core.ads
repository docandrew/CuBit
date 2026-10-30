pragma Ada_2022;
with AML_References;
with AML_Delays;
with AML_Interface_Policy;
with Firmware_Tables.DMAR;
with Firmware_Tables.MCFG;
with Firmware_Tables.MADT;
with Firmware_Tables.SRAT;
with Firmware_Tables.SLIT;
with AML_Clock;
with AML_Identity;
with AML_Decode;
with AML_Table_Backing;
with AML_Field_Data;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Field_Plans;
with AML_Field_Targets;
with AML_Names;
with ACPI_FADT;
with AML_Execute;
with AML_Execution_Limits;
with AML_Events;
with AML_Objects;
with AML_Namespace;
with Firmware_Tables;
with Firmware_Tables.Snapshots;
with Firmware_Tables.Identifiers;
--  Service core only: immutable copied-table input, no OS/hardware imports.
generic
   with procedure Perform_Delay
     (Item : AML_Delays.Request; Result : out AML_Delays.Outcome);
   with procedure Read_Microseconds
     (Value : out AML_Decode.Integer_Value; Available : out Boolean) is AML_Clock.No_Sample;
   -- Explicit per-instance budgets; defaults preserve the standard service.
   Namespace_Node_Capacity : Positive := 512;
   Aggregate_Method_Capacity : Positive := AML_Execute.Max_Method_Bytes;
   Retained_Result_Capacity : Positive := 512;
   -- Pending internal notifications, independent of retained AML values.
   Notification_Capacity : Positive := 32;
   Supported_Interfaces : AML_Interface_Policy.Policy := AML_Interface_Policy.Empty;
   with procedure Wait_Event_Timeout
     (Milliseconds : AML_Events.Timeout; Outcome : out AML_Events.Timeout_Outcome)
     is AML_Events.Unavailable_Timeout;
   Block_Capacity : AML_Execution_Limits.Continuation_Capacity :=
     AML_Execution_Limits.Default_Continuation_Capacity;
package ACPI_Service_Core with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   Max_Namespace_Nodes : constant Positive := Namespace_Node_Capacity;
   package Namespace is new AML_Namespace
     (Perform_Delay => Perform_Delay, Capacity => Max_Namespace_Nodes, Read_Microseconds => Read_Microseconds,
      Aggregate_Method_Capacity => Aggregate_Method_Capacity,
      Max_Retained_Roots => Retained_Result_Capacity,
      Max_Pending_Notifications => Notification_Capacity,
      Supported_Interfaces => Supported_Interfaces,
      Wait_Event_Timeout => Wait_Event_Timeout,
      Block_Capacity => Block_Capacity);
   package Values is new Namespace.Owned.Collecting;
   use type AML_Execute.Execution_Status;
   use type Values.Audit_Value;
   use type Values.Access_Status;
   use type Values.Preparation_Outcome;
   use type Values.Module_Report;
   use type Values.Phase;
   use type Values.Module_Outcome;
   use type Values.Value_Handle;
   use type Values.Node_Descriptor;
   use type AML_Identity.Identity;
   use type Namespace.Bind_Status;
   use type Namespace.Object_Kind;
   use type Firmware_Tables.Byte;
   Max_Table_Bytes : constant := Firmware_Tables.Snapshots.Max_Table_Bytes;
   Max_Total_Bytes : constant := Firmware_Tables.Snapshots.Max_Total_Bytes;
   Max_Tables : constant := Firmware_Tables.Snapshots.Max_Tables;
   Max_Method_Bytes : constant := AML_Execute.Max_Method_Bytes;
   function Method_Storage_Capacity return Positive;
   type Table_Kind is (DSDT, SSDT, Description);
   -- Description admits other immutable standard SDTs without decoding their
   -- bodies. FACS has different mutable semantics and is not admitted here.
   type Table_Metadata is record
      ID : Positive := 1;
      Signature : Firmware_Tables.Signature := "____";
      Extent : Positive range Firmware_Tables.Table_Header_Size .. Positive'Last := Firmware_Tables.Table_Header_Size;
      Revision : Firmware_Tables.Byte := 0;
   end record;
   type Install_Status is
     (Installed, Wrong_Order, Duplicate_ID, Table_Limit, Byte_Limit,
      Invalid_Table, Invalid_AML);
   -- Catalog admission is separate from AML execution. Frozen is not Ready.
   type Import_Phase is (Undecided, Legacy_Import, Catalog_Staging, Catalog_Frozen,
     Catalog_Executed, Catalog_Failed);
   type Stage_Status is
     (Staged, Stage_Wrong_Order, Stage_Duplicate_ID, Stage_Table_Limit,
      Stage_Byte_Limit, Stage_Invalid_Table);
   type Freeze_Status is (Catalog_Frozen_OK, Freeze_Wrong_Phase, Freeze_Empty);
   type Metrics is record
      Tables : Natural := 0;
      Bytes : Natural := 0;
      Objects : Natural range 0 .. Max_Namespace_Nodes := 0;
      Value_Objects : Natural range 0 .. AML_Objects.Max_Objects := 0;
      Value_Bytes : Natural range 0 .. AML_Objects.Max_Bytes := 0;
      Package_Elements : Natural range 0 .. AML_Objects.Max_Elements := 0;
      Method_Bytes : Namespace.Aggregate_Method_Count := 0;
      -- Zero means no AML load attempted; otherwise Load_Status'Pos + 1.
      Last_Load_Code : Natural range 0 .. Namespace.Load_Status'Pos (Namespace.Load_Status'Last) + 1 := 0;
      Rejections : Natural := 0;
      Counter_Saturated : Boolean := False;
   end record;
   -- Capacities are fixed at construction, before any untrusted table import.
   -- No default discriminants: objects must be explicitly constrained or
   -- default initialized, rather than reserving worst-case mutable storage.
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is limited private
     with Default_Initial_Condition => Valid (State) and then Observe (State) = Metrics'(others => <>)
       and then Observe (State).Objects = 0;
   function Valid (Service : State) return Boolean;
   function Current_Import_Phase (Service : State) return Import_Phase;
   function Namespace_Load_Phase (Service : State) return Values.Phase with Ghost;
   function Namespace_Generation (Service : State) return AML_Identity.Identity with Ghost;
   function Table_Count (Service : State) return Natural with
     Post => Table_Count'Result <= Service.Table_Capacity;
   function Observe (Service : State) return Metrics with
     Post => Observe'Result.Tables = Table_Count (Service)
       and then Observe'Result.Tables <= Service.Table_Capacity
       and then Observe'Result.Bytes <= Service.Byte_Capacity;
   function Audit (Service : State) return Values.Audit_Value;
   -- Physical lexical binding; Root means absent.
   -- Physical lexical binding; Root means absent.
   subtype Node_Descriptor is Values.Node_Descriptor;
   function Namespace_Node_Count (Service : State) return Namespace.Node_ID;
   procedure Describe_Node (Service : State; Node : Namespace.Node_ID;
      Item : out Node_Descriptor; Status : out Values.Access_Status)
     with Pre => Valid (Service);
   -- Binding identity includes owner, incarnation, parent and name; data kind may change.
   function Current_Binding (Service : State; Item : Node_Descriptor) return Boolean
     with Pre => Valid (Service);
   -- Stronger snapshot check additionally requires the observed object kind.
   function Current_Node (Service : State; Item : Node_Descriptor) return Boolean
     with Pre => Valid (Service);
   procedure Find_Child (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Node : out Namespace.Node_ID; Status : out Values.Access_Status);
   -- Semantic object selection follows aliases, retaining Found Root.
   procedure Find_Object (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Result : out Namespace.Lookup_Result; Status : out Values.Access_Status);
   -- Detached metadata/key observation; neither grants hardware access.
   procedure Read_Region_Descriptor
     (Service : State; Item : Node_Descriptor;
      Description : out Namespace.Operation_Region_Attributes;
      Status : out Values.Access_Status) with Pre => Valid (Service);
   procedure Read_Field_Descriptor
     (Service : State; Node : Namespace.Node_ID;
      Description : out Values.Field_Description; Key : out Values.Field_Key;
      Status : out Values.Access_Status) with Pre => Valid (Service);
   function Field_Key_Matches (Service : State; Key : Values.Field_Key) return Boolean
     with Pre => Valid (Service);
   procedure Observe_Named_Value (Service : in out State; Node : Namespace.Node_ID;
      Result : out Values.Result; Status : out Values.Access_Status)
      with Pre => not Result'Constrained;
   function Table_Info (Service : State; Index : Positive) return Table_Metadata
     with Pre => Index <= Table_Count (Service),
       Post => Table_Info'Result.Extent <= Service.Table_Byte_Limit;
   -- Read-only descriptions from retained snapshots; no hardware authority.
   function MADT_Info (Service : State; Index : Positive)
     return Firmware_Tables.MADT.Table_Metadata with Pre => Index <= Observe (Service).Tables;
   function MADT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.MADT.Record_Result with Pre => Index <= Observe (Service).Tables;
   function DMAR_Info (Service : State; Index : Positive) return Firmware_Tables.DMAR.Table_Metadata
     with Pre => Index <= Observe (Service).Tables;
   function DMAR_Record (Service : State; Index : Positive ; Record_Index : Natural) return Firmware_Tables.DMAR.Record_Result
     with Pre => Index <= Observe (Service).Tables;
   function DMAR_Scope (Service : State; Index : Positive ; Record_Index, Scope_Index : Natural) return Firmware_Tables.DMAR.Scope_Result
     with Pre => Index <= Observe (Service).Tables;
   function DMAR_Path (Service : State; Index : Positive ; Record_Index, Scope_Index, Path_Index : Natural) return Firmware_Tables.DMAR.Path_Result
     with Pre => Index <= Observe (Service).Tables;
   function SRAT_Info (Service : State; Index : Positive)
     return Firmware_Tables.SRAT.Table_Metadata with Pre => Index <= Observe (Service).Tables;
   function SRAT_Record (Service : State; Index : Positive; Record_Index : Natural)
     return Firmware_Tables.SRAT.Record_Result with Pre => Index <= Observe (Service).Tables;
   function MCFG_Info (Service : State; Index : Positive) return Firmware_Tables.MCFG.Table_Metadata
     with Pre => Index <= Observe (Service).Tables;
   function MCFG_Allocation (Service : State; Index : Positive; Allocation_Index : Natural) return Firmware_Tables.MCFG.Allocation_Result
     with Pre => Index <= Observe (Service).Tables;
   function SLIT_Info (Service : State; Index : Positive) return Firmware_Tables.SLIT.Table_Metadata
     with Pre => Index <= Observe (Service).Tables;
   function SLIT_Distance (Service : State; Index : Positive; From_Locality, To_Locality : Natural) return Firmware_Tables.SLIT.Distance_Result
     with Pre => Index <= Observe (Service).Tables;
   function Table_Byte (Service : State; Index : Positive; Offset : Natural)
      return Firmware_Tables.Byte
     with Pre => Index <= Table_Count (Service)
       and then Offset < Table_Info (Service, Index).Extent;
   function Fixed_Description (Service : State; Index : Positive) return ACPI_FADT.Result
     with Pre => Index <= Observe (Service).Tables;
   -- Reads an exact bit span in an owned immutable table. Index is local to
   -- this service lifetime; it conveys no authority over physical memory.
   function Table_Field
     (Service : State; Index : Positive; Bit_Offset, Bit_Count : Natural)
      return AML_Field_Data.Read_Result
     with Pre => Index <= Observe (Service).Tables;
   function Table_Field_Window
     (Service : State; Index : Positive; Bit_Offset, Bit_Count : Natural;
      First_Byte, Count : Natural) return AML_Table_Backing.Window_Result
     with Pre => Index <= Observe (Service).Tables;
   function Table_Identity (Service : State; Index : Positive)
      return Firmware_Tables.Identifiers.Identity
     with Pre => Index <= Observe (Service).Tables;
   -- First retained match, or zero. The result is a lifetime-local table index,
   -- never a physical address or capability. OEM selectors have explicit wildcards.
   function Find_Table
     (Service : State; Requested : Firmware_Tables.Identifiers.Selection)
      return Natural with
     Post => Find_Table'Result <= Observe (Service).Tables
       and then (if Find_Table'Result > 0 then
         Firmware_Tables.Identifiers.Matches
           (Table_Identity (Service, Find_Table'Result), Requested)
         and then (for all I in 1 .. Find_Table'Result - 1 =>
           not Firmware_Tables.Identifiers.Matches (Table_Identity (Service, I), Requested))
       else (for all I in 1 .. Observe (Service).Tables =>
         not Firmware_Tables.Identifiers.Matches (Table_Identity (Service, I), Requested)));
   -- Plain observer data for contracts; never contains reference authority.
   type Catalog_State (Table_Capacity, Byte_Capacity : Positive) is private;
   function Catalog_Snapshot (Service : State) return Catalog_State with Ghost,
     Post => Catalog_Snapshot'Result.Table_Capacity = Service.Table_Capacity
       and then Catalog_Snapshot'Result.Byte_Capacity = Service.Byte_Capacity;
   function Same_Catalog (Service, Prior : State) return Boolean with Ghost;
   subtype Notification_Count is Namespace.Notification_Count;
   subtype Notification_Observation is Namespace.Notification_Observation;
   -- Trusted service-owner API, not an AML or wire-supplied capability.
   -- Disabled by default. Disabling affects future admissions only.
   procedure Configure_Notifications
     (Service : in out State; Enabled : Boolean; Status : out Values.Access_Status)
     with Pre => Valid (Service), Post => Valid (Service)
       and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
       and then Audit (Service) = Audit (Service)'Old;
   function Pending_Notifications (Service : State) return Notification_Count;
   -- Non-reentrant dequeue: the owning arena rejects calls during invocation.
   -- Observations contain copied descriptive data, never callable authority.
   -- A stale target is reported explicitly rather than redirected after reuse.
   procedure Take_Notification
     (Service : in out State; Item : out Notification_Observation;
      Present : out Boolean; Status : out Values.Access_Status)
     with Pre => Valid (Service), Post => Valid (Service)
       and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
       and then Audit (Service) = Audit (Service)'Old;
   -- Audit-only full state, including identity metadata; never usable as authority.
   type State_Model (Table_Capacity, Byte_Capacity : Positive) is private with Ghost;
   function Model (Service : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Service.Table_Capacity
       and then Model'Result.Byte_Capacity = Service.Byte_Capacity;
   -- Result handles belong to this exact service lifetime. Keep a compound
   -- result retained until all observations/serialization finish, then release.
   -- Invoke_Retained carries integer arguments; Invoke_With_Arguments borrows only
   -- service-owned retained result handles, never raw AML object identifiers.
   function Retained_Results (Service : State) return Namespace.Owned.Retained_Root_Count;
   procedure Evaluate_Named (Service : aliased in out State; Item : Node_Descriptor;
      Budget : Natural; Result : out Values.Result; Status : out Values.Access_Status)
     with Pre => Valid (Service) and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged <= Budget
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
           then Status = Values.Available and then Result.Handle /= Values.No_Value
             and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
           else Retained_Results (Service) = Retained_Results (Service)'Old);
   procedure Invoke_Retained
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status)
     with Pre => Valid (Service) and then Argument_Count <= 7 and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged <= Budget
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
           then Status = Values.Available and then Result.Handle /= Values.No_Value
             and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
           else Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Arguments borrow live retained results from this exact service.
   procedure Invoke_With_Arguments
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : Values.Arguments;
      Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status)
     with Pre => Valid (Service) and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged <= Budget
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
           then Status = Values.Available and then Result.Handle /= Values.No_Value
             and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
           else Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Copied host data for method arguments, with ordinary retained-result
   -- ownership. Charged counts AML instructions; construction executes none.
   procedure Create_Buffer_Result
     (Service : in out State; Data : AML_Decode.Bytes;
      Result : out Values.Result; Status : out Values.Access_Status)
     with Pre => Valid (Service) and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged = 0
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status = AML_Execute.Object_Returned then
           Status = Values.Available and then Result.Handle /= Values.No_Value
           and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
         else Retained_Results (Service) = Retained_Results (Service)'Old);
   procedure Create_Empty_Package_Result
     (Service : in out State; Result : out Values.Result; Status : out Values.Access_Status)
     with Pre => Valid (Service) and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged = 0
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status = AML_Execute.Object_Returned then
           Status = Values.Available and then Result.Handle /= Values.No_Value
           and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
         else Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Opt-in provider path. Caller exclusively owns Service and IO throughout
   -- the synchronous call; trusted provider actuals must not capture Service.
   -- A binding associates one live field with independently granted authority.
   generic
      type IO_Context is limited private;
      with function Pending (IO : IO_Context) return Boolean;
      with procedure Describe_Target
        (IO : IO_Context; Target : AML_Field_Access.Target;
         Geometry : out AML_Field_Layout.Register_Geometry; Accepted : out Boolean);
      with procedure Dispatch
        (IO : in out IO_Context; Item : AML_Field_Access.Request;
         Reply : out AML_Field_Access.Response);
      with procedure Completion_Confirmed
        (IO : IO_Context; Expected : AML_Field_Access.Observation; Accepted : out Boolean);
      Maximum_Bindings : Positive := AML_Field_Access.Default_Maximum_Bindings;
      with procedure Authorize_Target
        (IO : IO_Context; Target : AML_Field_Access.Target;
         For_Write : Boolean; Accepted : out Boolean);
      Maximum_Requests : AML_Execution_Limits.Field_Request_Limit :=
        AML_Execution_Limits.Default_Field_Request_Limit;
      Maximum_Work : AML_Execution_Limits.Field_Work_Limit :=
        AML_Execution_Limits.Default_Field_Work_Limit;
   package Field_IO is
      pragma Unevaluated_Use_Of_Old (Allow);
      type Binding is private;
      No_Binding : constant Binding;
      type Binding_Set is private;
      procedure Clear (Items : out Binding_Set);
      procedure Add_Region_Register
        (Service : State; IO : IO_Context; Region : Node_Descriptor;
         Target : AML_Field_Access.Target; Allow_Read, Allow_Write : Boolean;
         Items : in out Binding_Set; Status : out Values.Access_Status)
        with Pre => Valid (Service);
      procedure Add_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Target : AML_Field_Access.Target; Items : in out Binding_Set;
         Status : out Values.Access_Status) with Pre => Valid (Service);
      procedure Add_Read_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
         Items : in out Binding_Set; Status : out Values.Access_Status)
        with Pre => Valid (Service);
      procedure Add_Write_Binding
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Targets : AML_Field_Targets.Vector; Count : AML_Field_Plans.Datum_Count;
         Items : in out Binding_Set; Status : out Values.Access_Status)
        with Pre => Valid (Service);

      procedure Bind
        (Service : State; IO : IO_Context; Key : Values.Field_Key;
         Target : AML_Field_Access.Target; Item : out Binding;
         Status : out Values.Access_Status) with Pre => Valid (Service);
      function Uncertain (Service : State) return AML_Field_Access.Observation
        with Pre => Valid (Service);
      -- Historical identities only; these do not authorize a post-reset access.
      function Original_Field (Service : State) return AML_References.Reference
        with Pre => Valid (Service);
      function Original_Region (Service : State) return AML_References.Reference
        with Pre => Valid (Service);
      procedure Reconcile
        (Service : in out State; IO : IO_Context; Expected : AML_Field_Access.Observation;
         Status : out Values.Access_Status) with Pre => Valid (Service),
        Post => Valid (Service)
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old;
      procedure Invoke_Retained
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status)
        with Pre => Valid (Service) and then Argument_Count <= Values.Argument_Count'Last
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
          and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
            then Status = Values.Available and then Result.Handle /= Values.No_Value
              and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
            else Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Arguments borrow live retained results from this exact service.
   procedure Invoke_With_Arguments
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : Values.Arguments;
         Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status)
        with Pre => Valid (Service)
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
          and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
            then Status = Values.Available and then Result.Handle /= Values.No_Value
              and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
            else Retained_Results (Service) = Retained_Results (Service)'Old);

      procedure Invoke_Retained
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status)
        with Pre => Valid (Service) and then Argument_Count <= Values.Argument_Count'Last
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
          and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
            then Status = Values.Available and then Result.Handle /= Values.No_Value
              and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
            else Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Arguments borrow live retained results from this exact service.
   procedure Invoke_With_Arguments
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : Values.Arguments;
         Argument_Count : Values.Argument_Count; Budget : Natural; Result : out Values.Result;
         Status : out Values.Access_Status)
        with Pre => Valid (Service)
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
          and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
            then Status = Values.Available and then Result.Handle /= Values.No_Value
              and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
            else Retained_Results (Service) = Retained_Results (Service)'Old);
      procedure Invoke_Scalar
        (Service : aliased in out State; IO : in out IO_Context; Item : Binding;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
        with Pre => Valid (Service) and then Argument_Count <= Values.Argument_Count'Last
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Result.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
          and then Retained_Results (Service) = Retained_Results (Service)'Old
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old;

      procedure Invoke_Scalar
        (Service : aliased in out State; IO : in out IO_Context; Items : Binding_Set;
         Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
         Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
        with Pre => Valid (Service) and then Argument_Count <= Values.Argument_Count'Last
          and then not Result'Constrained,
        Post => Valid (Service) and then Result.Charged <= Budget
          and then Result.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
          and then Retained_Results (Service) = Retained_Results (Service)'Old
          and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old;
      procedure Evaluate_Named
        (Service : aliased in out State; IO : in out IO_Context;
         Items : Binding_Set; Item : Node_Descriptor; Budget : Natural;
         Result : out Values.Result; Status : out Values.Access_Status)
        with Pre => Valid (Service) and then not Result'Constrained,
          Post => Valid (Service) and then Result.Charged <= Budget
            and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
            and then (if Result.Status in AML_Execute.Object_Returned |
                AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
              then Status = Values.Available and then Result.Handle /= Values.No_Value
                and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
              else Retained_Results (Service) = Retained_Results (Service)'Old);
   private
      package Adapter is new Values.Field_IO
        (IO_Context, Pending, Describe_Target, Dispatch, Completion_Confirmed, Maximum_Bindings,
         Authorize_Target, Maximum_Requests, Maximum_Work);
      type Binding is record
         Inner : Adapter.Binding := Adapter.No_Binding;
      end record;
      type Binding_Set is record
         Inner : Adapter.Binding_Set;
      end record;
      No_Binding : constant Binding := (others => <>);
   end Field_IO;
   procedure Describe_Result (Service : State; Handle : Values.Value_Handle;
      Description : out Values.Value_Description; Status : out Values.Access_Status)
      with Pre => not Description'Constrained;
   procedure Read_Result_Device (Service : State; Handle : Values.Value_Handle;
      Item : out Node_Descriptor; Status : out Values.Access_Status)
     with Pre => Valid (Service),
     Post => (if Status = Values.Available then Current_Node (Service, Item)
       and then Values.Node_Kind (Item) = Namespace.Device_Object else Item = Values.No_Node);
   procedure Read_Result_Bytes (Service : State; Handle : Values.Value_Handle; Offset : Natural;
      Data : out AML_Decode.Bytes; Copied : out Natural; Status : out Values.Access_Status);
   procedure Read_Result_Element (Service : in out State; Handle : Values.Value_Handle;
      Index : Natural; Element : out Values.Value_Handle; Status : out Values.Access_Status);
   procedure Dereference_Result (Service : in out State; Handle : Values.Value_Handle;
      Value : out Values.Value_Handle; Status : out Values.Access_Status);
   procedure Release_Result (Service : in out State; Handle : in out Values.Value_Handle;
      Status : out Values.Access_Status)
     with Pre => Valid (Service), Post => Valid (Service)
       and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
       and then Audit (Service) = Audit (Service)'Old
       and then (if Status = Values.Available then Handle = Values.No_Value
         and then Retained_Results (Service) = Retained_Results (Service)'Old - 1
         else Handle = Handle'Old and then Retained_Results (Service) = Retained_Results (Service)'Old);
   -- Compound outcomes are released and rejected; only scalars escape.
   procedure Invoke_Scalar
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
     with Pre => Valid (Service) and then Argument_Count <= 7 and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged <= Budget
         and then Result.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned | AML_Execute.Metadata_Returned
         and then Retained_Results (Service) = Retained_Results (Service)'Old
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old;
   -- Internal declaration/read path for immutable table-backed namespace
   -- objects. AML declaration parsing and evaluation are separate callers.
   procedure Declare_Table_Region
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Requested : Firmware_Tables.Identifiers.Selection;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
     with Pre => Valid (Service),
       Post => Valid (Service) and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old and then
       (if Result /= Namespace.Bound then Audit (Service) = Audit (Service)'Old);
   procedure Declare_Table_Field
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Region_Node : Namespace.Node_ID; Bit_Offset, Bit_Count : Natural;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
     with Pre => Valid (Service),
       Post => Valid (Service) and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old and then
       (if Result /= Namespace.Bound then Audit (Service) = Audit (Service)'Old);
   function Read_Namespace_Field (Service : State; Node : Namespace.Node_ID)
      return AML_Field_Data.Read_Result;
   function Pending_Members (Service : State) return Natural;
   function Initialization_Frame (Current, Prior : State_Model) return Boolean with Ghost;
   -- Explicit phase boundary after every initial definition block is installed.
   -- Missing/unsupported members are reported and remain uninitialized.
   procedure Initialize_Members
     (Service : in out State; Report : out Namespace.Initialization_Report;
      Status : out Values.Access_Status)
     with Pre => Valid (Service), Post => Valid (Service)
       and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
       and then (if Status = Values.Available then Pending_Members (Service) = 0
         and then Initialization_Frame (Model (Service), Model (Service)'Old));
   --  ID is supplied by the bootstrap snapshot provider and must uniquely
   --  identify a table in this service lifetime. It is not a capability.
   procedure Install
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Install_Status)
     with Pre => Valid (Service),
       Post => Valid (Service) and then
       (if Result /= Installed then Audit (Service) = Audit (Service)'Old
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old)
       and then Observe (Service).Tables = Observe (Service)'Old.Tables +
         (if Result = Installed then 1 else 0)
       and then (if Result = Installed then Observe (Service).Tables > 0
         and then Table_Info (Service, Observe (Service).Tables).ID = ID
         and then Table_Info (Service, Observe (Service).Tables).Extent = Data'Length
         and then (for all I in 1 .. Data'Length =>
           Table_Byte (Service, Observe (Service).Tables, I - 1) = Data (Data'First + (I - 1))));
   -- Physical admission only: AML bytes are retained but never executed.
   -- Failure preserves all catalog bytes and the selected import mode.
   procedure Stage_Table
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Stage_Status)
     with Pre => Valid (Service),
       Post => Valid (Service) and then Audit (Service) = Audit (Service)'Old
         and then Pending_Members (Service) = Pending_Members (Service)'Old
         and then Namespace_Load_Phase (Service) = Namespace_Load_Phase (Service)'Old
         and then Namespace_Generation (Service) = Namespace_Generation (Service)'Old
         and then (if Result /= Staged then
           Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
           and then Current_Import_Phase (Service) = Current_Import_Phase (Service)'Old
         else Current_Import_Phase (Service) = Catalog_Staging
           and then Table_Count (Service) = Table_Count (Service)'Old + 1
           and then Table_Info (Service, Table_Count (Service)).ID = ID
           and then Table_Info (Service, Table_Count (Service)).Extent = Data'Length
           and then (for all I in 1 .. Data'Length =>
             Table_Byte (Service, Table_Count (Service), I - 1) = Data (Data'First + (I - 1))));
   type Execution_Phase_Result is
     (Execution_Not_Started, Preparation_Failed, Module_Execution_Reported);
   type Execution_Report is record
      Phase : Execution_Phase_Result := Execution_Not_Started;
      Preparation : Values.Preparation_Outcome := Values.Preparation_Not_Attempted;
      Modules : Values.Module_Report;
      -- Lifetime-local catalog ID, never an address or hardware capability.
      Failed_Table_ID : Natural := 0;
   end record;
   -- Consumes the frozen catalog once. Failure is terminal; no retry API.
   procedure Execute_Frozen
     (Service : in out State; Budget : Natural; Report : out Execution_Report;
      Status : out Values.Access_Status)
     with Pre => Valid (Service),
       Post => Valid (Service) and then Report.Modules.Charged <= Budget
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Current_Import_Phase (Service)'Old /= Catalog_Frozen then
           Model (Service) = Model (Service)'Old and then Status = Values.Wrong_Phase
           and then Report.Phase = Execution_Not_Started
           and then Report.Preparation = Values.Preparation_Not_Attempted
         else Current_Import_Phase (Service) =
           (if Report.Phase = Module_Execution_Reported
              and then Report.Preparation = Values.Firmware_Prepared
              and then Status = Values.Available and then Report.Modules.Outcome = Values.Modules_Completed
            then Catalog_Executed else Catalog_Failed))
         and then (if Report.Phase /= Module_Execution_Reported then
           Report.Modules = Values.Module_Report'(others => <>)
           and then Report.Failed_Table_ID = 0)
         and then Retained_Results (Service) = Retained_Results (Service)'Old
         and then Pending_Notifications (Service) = Pending_Notifications (Service)'Old
         and then (if Report.Modules.Failed_Table > 0 then
           Report.Modules.Failed_Table <= Table_Count (Service)
           and then Report.Failed_Table_ID = Table_Info (Service, Report.Modules.Failed_Table).ID
         else Report.Failed_Table_ID = 0);
   -- Only closes admission. It neither initializes nor executes AML.
   procedure Freeze_Catalog (Service : in out State; Result : out Freeze_Status)
     with Pre => Valid (Service),
       Post => Valid (Service) and then Audit (Service) = Audit (Service)'Old
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then Observe (Service) = Observe (Service)'Old
         and then Namespace_Load_Phase (Service) = Namespace_Load_Phase (Service)'Old
         and then Namespace_Generation (Service) = Namespace_Generation (Service)'Old
         and then (if Result = Catalog_Frozen_OK then
           Current_Import_Phase (Service) = Catalog_Frozen
         else Current_Import_Phase (Service) = Current_Import_Phase (Service)'Old);
private
   type Table_Entry is record
      Metadata : Table_Metadata;
      Offset : Natural := 0;
   end record;
   type Table_Array is array (Positive range <>) of Table_Entry;
   type Catalog_State (Table_Capacity, Byte_Capacity : Positive) is record
      Catalog : Table_Array (1 .. Table_Capacity);
      Backing : AML_Table_Backing.State (Table_Capacity, Byte_Capacity);
      Tables, Bytes : Natural;
   end record;
   type State_Model (Table_Capacity, Byte_Capacity : Positive) is record
      Catalog : Catalog_State (Table_Capacity, Byte_Capacity);
      Tree : Values.Audit_Value;
      Notifications : Namespace.Notification_State;
      Stats : Metrics;
      Width : AML_Decode.Integer_Width;
      Table_Limit : Positive;
      Import_State : Import_Phase;
      Generation : AML_Identity.Identity;
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is limited record
      Import_State : Import_Phase := Undecided;
      Tree : Values.Arena;
      Stats : Metrics;
      Catalog : Table_Array (1 .. Table_Capacity);
      Backing : aliased AML_Table_Backing.State (Table_Capacity, Byte_Capacity);
      Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   end record with Type_Invariant =>
     (if State.Stats.Tables = 0 then Values.Node_Count (State.Tree) = Namespace.Root)
     and then (if State.Import_State = Legacy_Import and then State.Stats.Tables > 0
       then Values.Current (State.Tree) /= Values.Uninitialized)
     and then (if State.Import_State in Catalog_Staging | Catalog_Frozen then
       State.Stats.Tables > 0 and then Values.Current (State.Tree) in Values.Uninitialized | Values.Loading
       and then Values.Node_Count (State.Tree) = Namespace.Root
       and then Values.Observe_Usage (State.Tree).Pending = 0
       and then Values.Observe_Usage (State.Tree).Methods = 0
       and then Values.Observe_Usage (State.Tree).Values.Objects = 0
       and then Values.Observe_Usage (State.Tree).Values.Bytes = 0
       and then Values.Observe_Usage (State.Tree).Values.Elements = 0
       and then Values.Retained_Count (State.Tree) = 0)
     and then (if State.Import_State = Catalog_Executed then
       State.Stats.Tables > 0 and then Values.Current (State.Tree) in Values.Loading | Values.Ready)
     and then (if State.Import_State = Catalog_Failed then State.Stats.Tables > 0)
     and then (if State.Import_State = Undecided then State.Stats.Tables = 0)
     and then State.Backing.Count = State.Stats.Tables
     and then State.Stats.Tables <= State.Table_Capacity
     and then State.Stats.Bytes <= State.Byte_Capacity
     and then (for all I in 1 .. State.Stats.Tables =>
        State.Backing.Tables (I).Offset = State.Catalog (I).Offset and then
        State.Backing.Tables (I).Extent = State.Catalog (I).Metadata.Extent and then
        State.Catalog (I).Metadata.Extent <= State.Table_Byte_Limit and then
        State.Catalog (I).Offset <= State.Stats.Bytes and then
        State.Catalog (I).Metadata.Extent <= State.Stats.Bytes - State.Catalog (I).Offset);
   function Table_Count (Service : State) return Natural is (Service.Stats.Tables);
end ACPI_Service_Core;
