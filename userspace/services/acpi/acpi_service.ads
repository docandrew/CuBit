pragma Ada_2022;
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
package ACPI_Service with SPARK_Mode is
   Max_Namespace_Nodes : constant := 512;
   package Namespace is new AML_Namespace (Capacity => Max_Namespace_Nodes);
   use type Namespace.State;
   use type Namespace.Bind_Status;
   use type Firmware_Tables.Byte;
   Max_Table_Bytes : constant := Firmware_Tables.Snapshots.Max_Table_Bytes;
   Max_Total_Bytes : constant := Firmware_Tables.Snapshots.Max_Total_Bytes;
   Max_Tables : constant := Firmware_Tables.Snapshots.Max_Tables;
   Max_Method_Bytes : constant := AML_Execute.Max_Method_Bytes;
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
      Method_Bytes : AML_Execute.Method_Length := 0;
      -- Zero means no AML load attempted; otherwise Load_Status'Pos + 1.
      Last_Load_Code : Natural range 0 .. Namespace.Load_Status'Pos (Namespace.Load_Status'Last) + 1 := 0;
      Rejections : Natural := 0;
      Counter_Saturated : Boolean := False;
   end record;
   -- Capacities are fixed at construction, before any untrusted table import.
   -- No default discriminants: objects must be explicitly constrained or
   -- initialized by Fresh, rather than reserving worst-case mutable storage.
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is private;
   function Observe (Service : State) return Metrics with
     Post => Observe'Result.Tables <= Service.Table_Capacity
       and then Observe'Result.Bytes <= Service.Byte_Capacity;
   function Snapshot (Service : State) return Namespace.State;
   function Table_Info (Service : State; Index : Positive) return Table_Metadata
     with Pre => Index <= Observe (Service).Tables,
       Post => Table_Info'Result.Extent <= Service.Table_Byte_Limit;
   function Table_Byte (Service : State; Index : Positive; Offset : Natural)
      return Firmware_Tables.Byte
     with Pre => Index <= Observe (Service).Tables
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
   function Same_Catalog (Service, Prior : State) return Boolean with Ghost;
   procedure Invoke
     (Service : aliased in out State; Node : Namespace.Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
     with Pre => Argument_Count <= 7 and then not Result'Constrained,
       Post => Result.Charged <= Budget and then Same_Catalog (Service, Service'Old);
   -- Internal declaration/read path for immutable table-backed namespace
   -- objects. AML declaration parsing and evaluation are separate callers.
   procedure Declare_Table_Region
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Requested : Firmware_Tables.Identifiers.Selection;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
     with Post => Same_Catalog (Service, Service'Old) and then
       (if Result /= Namespace.Bound then Snapshot (Service) = Snapshot (Service'Old));
   procedure Declare_Table_Field
     (Service : in out State; Scope : Namespace.Node_ID; Part : AML_Names.Segment;
      Region_Node : Namespace.Node_ID; Bit_Offset, Bit_Count : Natural;
      Node : out Namespace.Node_ID; Result : out Namespace.Bind_Status;
      Owner : Namespace.Node_ID := Namespace.Root)
     with Post => Same_Catalog (Service, Service'Old) and then
       (if Result /= Namespace.Bound then Snapshot (Service) = Snapshot (Service'Old));
   function Read_Namespace_Field (Service : State; Node : Namespace.Node_ID)
      return AML_Field_Data.Read_Result;
   function Fresh
     (Table_Capacity : Positive := Max_Tables;
      Byte_Capacity : Positive := Max_Total_Bytes;
      Table_Byte_Limit : Positive := Max_Table_Bytes) return State with Post =>
     Fresh'Result.Table_Capacity = Table_Capacity
     and then Fresh'Result.Byte_Capacity = Byte_Capacity
     and then Fresh'Result.Table_Byte_Limit = Table_Byte_Limit
     and then Observe (Fresh'Result) = Metrics'(others => <>)
     and then Namespace.Count (Snapshot (Fresh'Result)) = 0;
   --  ID is supplied by the bootstrap snapshot provider and must uniquely
   --  identify a table in this service lifetime. It is not a capability.
   procedure Install
     (Service : in out State; ID : Positive; Kind : Table_Kind;
      Data : Firmware_Tables.Bytes; Result : out Install_Status)
     with Post =>
       (if Result /= Installed then Snapshot (Service) = Snapshot (Service'Old)
         and then Same_Catalog (Service, Service'Old))
       and then Observe (Service).Tables = Observe (Service'Old).Tables +
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
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is record
      Tree : Namespace.State := Namespace.Empty;
      Stats : Metrics;
      Catalog : Table_Array (1 .. Table_Capacity);
      Backing : aliased AML_Table_Backing.State (Table_Capacity, Byte_Capacity);
      Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   end record with Type_Invariant =>
     State.Backing.Count = State.Stats.Tables
     and then State.Stats.Tables <= State.Table_Capacity
     and then State.Stats.Bytes <= State.Byte_Capacity
     and then (for all I in 1 .. State.Stats.Tables =>
        State.Backing.Tables (I).Offset = State.Catalog (I).Offset and then
        State.Backing.Tables (I).Extent = State.Catalog (I).Metadata.Extent and then
        State.Catalog (I).Metadata.Extent <= State.Table_Byte_Limit and then
        State.Catalog (I).Offset <= State.Stats.Bytes and then
        State.Catalog (I).Metadata.Extent <= State.Stats.Bytes - State.Catalog (I).Offset);
end ACPI_Service;
