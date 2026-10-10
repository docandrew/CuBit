pragma Ada_2022;
with AML_Delays;
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
with AML_Names;
with ACPI_FADT;
with AML_Execute;
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
package ACPI_Service_Core with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   Max_Namespace_Nodes : constant Positive := Namespace_Node_Capacity;
   package Namespace is new AML_Namespace
     (Perform_Delay => Perform_Delay, Capacity => Max_Namespace_Nodes, Read_Microseconds => Read_Microseconds,
      Aggregate_Method_Capacity => Aggregate_Method_Capacity,
      Max_Retained_Roots => Retained_Result_Capacity);
   package Values is new Namespace.Owned.Collecting;
   use type Values.Audit_Value;
   use type Values.Access_Status;
   use type Values.Phase;
   use type Values.Value_Handle;
   use type Namespace.Bind_Status;
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
   function Table_Count (Service : State) return Natural with
     Post => Table_Count'Result <= Service.Table_Capacity;
   function Observe (Service : State) return Metrics with
     Post => Observe'Result.Tables = Table_Count (Service)
       and then Observe'Result.Tables <= Service.Table_Capacity
       and then Observe'Result.Bytes <= Service.Byte_Capacity;
   function Audit (Service : State) return Values.Audit_Value;
   procedure Find_Child (Service : State; Parent : Namespace.Node_ID; Part : AML_Names.Segment;
      Node : out Namespace.Node_ID; Status : out Values.Access_Status);
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
   -- Audit-only full state, including identity metadata; never usable as authority.
   type State_Model (Table_Capacity, Byte_Capacity : Positive) is private with Ghost;
   function Model (Service : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Service.Table_Capacity
       and then Model'Result.Byte_Capacity = Service.Byte_Capacity;
   -- Result handles belong to this exact service lifetime. Keep a compound
   -- result retained until all observations/serialization finish, then release.
   -- Integer-only arguments carry no borrowed object handles across this API.
   function Retained_Results (Service : State) return Namespace.Owned.Retained_Root_Count;
   procedure Invoke_Retained
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out Values.Result;
      Status : out Values.Access_Status)
     with Pre => Valid (Service) and then Argument_Count <= 7 and then not Result'Constrained,
       Post => Valid (Service) and then Result.Charged <= Budget
         and then Catalog_Snapshot (Service) = Catalog_Snapshot (Service)'Old
         and then (if Result.Status in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
           then Status = Values.Available and then Result.Handle /= Values.No_Value
             and then Retained_Results (Service) = Retained_Results (Service)'Old + 1
           else Retained_Results (Service) = Retained_Results (Service)'Old);
   procedure Describe_Result (Service : State; Handle : Values.Value_Handle;
      Description : out Values.Value_Description; Status : out Values.Access_Status)
      with Pre => not Description'Constrained;
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
         and then Result.Status not in AML_Execute.Object_Returned | AML_Execute.Reference_Returned
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
      Stats : Metrics;
      Width : AML_Decode.Integer_Width;
      Table_Limit : Positive;
      Generation : AML_Identity.Identity;
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is limited record
      Tree : Values.Arena;
      Stats : Metrics;
      Catalog : Table_Array (1 .. Table_Capacity);
      Backing : aliased AML_Table_Backing.State (Table_Capacity, Byte_Capacity);
      Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   end record with Type_Invariant =>
     (if State.Stats.Tables = 0 then Values.Node_Count (State.Tree) = Namespace.Root)
     and then (if State.Stats.Tables > 0 then Values.Current (State.Tree) /= Values.Uninitialized)
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
