pragma Ada_2022;
with Interfaces;
with ACPI_Service;
with ACPI_Bootstrap;
with Firmware_Tables;
-- Transport-independent request core. No endpoint/authority tag is allocated
-- here. Origin must come from a trusted native capability adapter, never wire
-- words, a PID claim, or the unauthenticated message reserved field.
package ACPI_Requests with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   use Interfaces;
   use type ACPI_Bootstrap.Phase;
   type Authority is (No_Authority, Observer, Snapshot_Provider);
   -- CCL integers are signed 64-bit. All observation words, including the
   -- revision token, must remain representable without reinterpretation.
   Max_Revision : constant Unsigned_64 := Unsigned_64 (Integer_64'Last);
   subtype Revision_Number is Unsigned_64 range 0 .. Max_Revision;
   type Phase is (Idle, Receiving, Complete, Failed);
   for Phase use (Idle => 0, Receiving => 1, Complete => 2, Failed => 3);
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   type Packet is record
      Label : Unsigned_32 := 0;
      Length : Unsigned_8 := 4;
      Flags : Unsigned_8 := 0;
      Reserved : Unsigned_16 := 0;
      Data : Words := [others => 0];
   end record;
   -- Labels are scoped to this draft service protocol, not kernel service IDs.
   Read_Metrics : constant Unsigned_32 := 0;
   Start_Snapshot : constant Unsigned_32 := 1;
   Begin_Table : constant Unsigned_32 := 2;
   Write_Chunk : constant Unsigned_32 := 3;
   Commit_Table : constant Unsigned_32 := 4;
   Finish_Snapshot : constant Unsigned_32 := 5;
   Read_Table_Info : constant Unsigned_32 := 6;
   Read_Table_Chunk : constant Unsigned_32 := 7;
   -- Queries use [revision, table index, record/row, page/column].
   -- MCFG info page (word2) 0 returns [revision,count,0]; page 1
   -- returns [reserved low32,reserved high32,0]. Word3 must be zero.
   -- Allocation page 0 returns [base low32, base high32, segment];
   -- page 1 returns [first bus, last bus, reserved32].
   -- SLIT info returns [revision, locality count, 0]; distance [value,0,0].
   -- Label 8 is reserved by the native grant-import adapter.
   Read_MCFG_Info : constant Unsigned_32 := 9;
   Read_MCFG_Allocation : constant Unsigned_32 := 10;
   Read_SLIT_Info : constant Unsigned_32 := 11;
   Read_SLIT_Distance : constant Unsigned_32 := 12;
   Read_MADT_Info : constant Unsigned_32 := 13;
   Read_MADT_Record : constant Unsigned_32 := 14;
   Read_MADT_Fields : constant Unsigned_32 := 15;
   Read_SRAT_Info : constant Unsigned_32 := 16;
   Read_SRAT_Record : constant Unsigned_32 := 17;
   Read_SRAT_Fields : constant Unsigned_32 := 18;
   Read_DMAR_Info : constant Unsigned_32 := 19;
   Read_DMAR_Record : constant Unsigned_32 := 20;
   Read_DMAR_Fields : constant Unsigned_32 := 21;
   Read_DMAR_Scope : constant Unsigned_32 := 22;
   Read_DMAR_Path : constant Unsigned_32 := 23;
   subtype DMAR_Header_Page is Unsigned_64 range 0 .. 1;
   subtype DMAR_Record_Page is Unsigned_64 range 0 .. 1;
   subtype DMAR_Field_Page is Unsigned_64 range 0 .. 2;
   subtype DMAR_Scope_Page is Unsigned_64 range 0 .. 2;
   DMAR_Scope_Page_Radix : constant Unsigned_64 := 4;
   DMAR_Path_Index_Radix : constant Unsigned_64 := 128;
   -- Scope/path selectors encode bounded indices, never addresses. Unknown
   -- record scope counts count decoded scopes only; raw bytes remain readable.
   SRAT_Header_Page : constant Unsigned_64 := 0;
   SRAT_Reserved_Page : constant Unsigned_64 := 1;
   SRAT_Common_Page : constant Unsigned_64 := 0;
   SRAT_Detail_Page : constant Unsigned_64 := 1;
   SRAT_Base_Page : constant Unsigned_64 := 2;
   SRAT_Length_Page : constant Unsigned_64 := 3;
   SRAT_Handle_First_Page : constant Unsigned_64 := 4;
   SRAT_Handle_Last_Page : constant Unsigned_64 := 9;
   SRAT_Handle_Bytes_Per_Page : constant := 3;
   MADT_Header_Page : constant Unsigned_64 := 0;
   MADT_Address_Page : constant Unsigned_64 := 1;
   MADT_Fields_Page : constant Unsigned_64 := 0;
   MADT_Override_Flags_Page : constant Unsigned_64 := 1;
   MCFG_Header_Page : constant Unsigned_64 := 0;
   MCFG_Reserved_Page : constant Unsigned_64 := 1;
   MCFG_Address_Page : constant Unsigned_64 := 0;
   MCFG_Bus_Page : constant Unsigned_64 := 1;
   type Outcome is
     (OK, Denied, Malformed, Stale, Wrong_Order, Resource_Limit,
      Table_Rejected, Incomplete, Not_Found, Wrong_Table_Kind, Index_Out_Of_Range, Unsupported_Record_Kind);
   -- Observation results must be representable as CCL integers. Upload packets
   -- retain unrestricted words because their payload contains arbitrary bytes.
   type Response_Words is array (Natural range 0 .. 3) of Revision_Number;
   type Response is record
      Status : Outcome := Malformed;
      Data : Response_Words := [others => 0];
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive; Initial_Revision : Revision_Number) is limited private
     with Default_Initial_Condition => Valid (State)
       and then Revision (State) = State.Initial_Revision
       and then Current (State) = Idle and then not Table_Open (State)
       and then Received (State) = 0;
   function Valid (Server : State) return Boolean;
   type State_Model (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is private with Ghost;
   function Model (Server : State) return State_Model with Ghost,
     Post => Model'Result.Table_Capacity = Server.Table_Capacity
       and then Model'Result.Byte_Capacity = Server.Byte_Capacity
       and then Model'Result.Table_Byte_Limit = Server.Table_Byte_Limit;
   function Revision (Server : State) return Revision_Number;
   function Current (Server : State) return Phase;
   function Table_Open (Server : State) return Boolean;
   function Received (Server : State) return Natural;
   function Observe (Server : State) return ACPI_Service.Metrics;
   -- All requests contain exactly four words and zero flags/reserved.
   -- Mutations use word0 as an optimistic revision token. Every state change
   -- consumes one token; it never wraps. Authorized replies report revision in
   -- word0. Unclassified callers receive Denied with zero data.
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) with
     Pre => Valid (Server), Post => Valid (Server) and then (if Origin = No_Authority then Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
              else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Origin = Snapshot_Provider
         and then Request.Label in Start_Snapshot .. Finish_Snapshot
         and then Reply.Status in OK | Table_Rejected | Incomplete
         then Revision (Server)'Old < Max_Revision
           and then Revision (Server) = Revision (Server)'Old + 1)
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Origin /= Snapshot_Provider or else Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk | Read_MCFG_Info .. Read_DMAR_Path
         or else Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Model (Server) = Model (Server)'Old);
   -- Native adapter entry point for one complete table from a bulk grant.
   -- The adapter authenticates Origin, validates the mapped extent and keeps
   -- Data stable and mapped for this entire call. Receiver read-only permission
   -- alone does not freeze the provider's alias. No address or handle supplied
   -- in a packet is dereferenced here; Data contains exactly the table bytes,
   -- excluding page padding. Accepted imports retain their own byte copy.
   procedure Import_Block
     (Server : in out State; Origin : Authority; Token : Unsigned_64;
      ID : Positive; Kind : ACPI_Service.Table_Kind;
      Data : Firmware_Tables.Bytes; Reply : out Response) with
     Pre => Valid (Server), Post => Valid (Server) and then
       (if Origin = No_Authority then
          Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
        else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server)'Old
       and then (if Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Model (Server) = Model (Server)'Old)
       and then (if Origin /= Snapshot_Provider then
         Reply.Status = Denied and then Model (Server) = Model (Server)'Old)
       and then (if Reply.Status in OK | Table_Rejected then
         Revision (Server)'Old < Max_Revision
         and then Revision (Server) = Revision (Server)'Old + 1
         and then not Table_Open (Server) and then Received (Server) = 0);
private
   type State_Model (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is record
      Boot : ACPI_Bootstrap.State_Model (Table_Capacity, Byte_Capacity);
      Initial_Revision, Version : Revision_Number;
      Open : Boolean;
      ID : Positive;
      Kind : ACPI_Service.Table_Kind;
      Extent, Used : Natural;
      Buffer_Data : Firmware_Tables.Bytes (1 .. Table_Byte_Limit);
   end record;
   function Consistent (Server : State) return Boolean;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive; Initial_Revision : Revision_Number) is limited record
      Boot : ACPI_Bootstrap.State
        (Table_Capacity, Byte_Capacity, Table_Byte_Limit);
      Version : Revision_Number := Initial_Revision;
      Open : Boolean := False;
      ID : Positive := 1;
      Kind : ACPI_Service.Table_Kind := ACPI_Service.DSDT;
      Extent : Natural := 0;
      Used : Natural := 0;
      Buffer_Data : Firmware_Tables.Bytes (1 .. Table_Byte_Limit) := [others => 0];
   end record with Type_Invariant => Consistent (State);
   function Consistent (Server : State) return Boolean is
     (Server.Used <= Server.Extent and then Server.Extent <= Server.Table_Byte_Limit and then
     (if Server.Open then ACPI_Bootstrap.Started (Server.Boot) and then
        ACPI_Bootstrap.Current (Server.Boot) = ACPI_Bootstrap.Receiving
        and then Server.Extent >= Firmware_Tables.Table_Header_Size
      else Server.Used = 0 and then Server.Extent = 0));
   function Current (Server : State) return Phase is
     (if not ACPI_Bootstrap.Started (Server.Boot) then Idle else
       (case ACPI_Bootstrap.Current (Server.Boot) is
          when ACPI_Bootstrap.Receiving => Receiving,
          when ACPI_Bootstrap.Complete => Complete,
          when ACPI_Bootstrap.Failed => Failed));
end ACPI_Requests;
