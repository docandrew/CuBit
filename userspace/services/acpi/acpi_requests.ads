pragma Ada_2022;
with Interfaces;
with ACPI_Service;
with ACPI_Bootstrap;
with Firmware_Tables;
-- Transport-independent request core. No endpoint/authority tag is allocated
-- here. Origin must come from a trusted native capability adapter, never wire
-- words, a PID claim, or the unauthenticated message reserved field.
package ACPI_Requests with SPARK_Mode is
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
   type Outcome is
     (OK, Denied, Malformed, Stale, Wrong_Order, Resource_Limit,
      Table_Rejected, Incomplete, Not_Found);
   -- Observation results must be representable as CCL integers. Upload packets
   -- retain unrestricted words because their payload contains arbitrary bytes.
   type Response_Words is array (Natural range 0 .. 3) of Revision_Number;
   type Response is record
      Status : Outcome := Malformed;
      Data : Response_Words := [others => 0];
   end record;
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is private;
   function Revision (Server : State) return Revision_Number;
   function Current (Server : State) return Phase;
   function Table_Open (Server : State) return Boolean;
   function Received (Server : State) return Natural;
   function Observe (Server : State) return ACPI_Service.Metrics;
   function Fresh
     (Initial_Revision : Revision_Number := 0;
      Table_Capacity : Positive := ACPI_Service.Max_Tables;
      Byte_Capacity : Positive := ACPI_Service.Max_Total_Bytes;
      Table_Byte_Limit : Positive := ACPI_Service.Max_Table_Bytes) return State with
     Post => Fresh'Result.Table_Capacity = Table_Capacity
       and then Fresh'Result.Byte_Capacity = Byte_Capacity
       and then Fresh'Result.Table_Byte_Limit = Table_Byte_Limit
       and then Revision (Fresh'Result) = Initial_Revision
       and then Current (Fresh'Result) = Idle and then not Table_Open (Fresh'Result)
       and then Received (Fresh'Result) = 0;
   -- All requests contain exactly four words and zero flags/reserved.
   -- Mutations use word0 as an optimistic revision token. Every state change
   -- consumes one token; it never wraps. Authorized replies report revision in
   -- word0. Unclassified callers receive Denied with zero data.
   procedure Handle (Server : in out State; Origin : Authority;
                     Request : Packet; Reply : out Response) with
     Post => (if Origin = No_Authority then Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
              else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server'Old)
       and then (for all Word of Reply.Data => Word <= Max_Revision)
       and then (if Origin /= Snapshot_Provider or else Request.Label in Read_Metrics | Read_Table_Info | Read_Table_Chunk
         or else Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Server = Server'Old);
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
     Post =>
       (if Origin = No_Authority then
          Reply.Status = Denied and then Reply.Data = [0, 0, 0, 0]
        else Reply.Data (0) = Revision (Server))
       and then Revision (Server) >= Revision (Server'Old)
       and then (if Reply.Status in Denied | Malformed | Stale | Wrong_Order | Resource_Limit
         then Server = Server'Old)
       and then (if Origin /= Snapshot_Provider then
         Reply.Status = Denied and then Server = Server'Old)
       and then (if Reply.Status in OK | Table_Rejected then
         Revision (Server'Old) < Max_Revision
         and then Revision (Server) = Revision (Server'Old) + 1
         and then not Table_Open (Server) and then Received (Server) = 0);
private
   type State (Table_Capacity, Byte_Capacity, Table_Byte_Limit : Positive) is record
      Boot : ACPI_Bootstrap.State
        (Table_Capacity, Byte_Capacity, Table_Byte_Limit);
      Started : Boolean := False;
      Version : Revision_Number := 0;
      Open : Boolean := False;
      ID : Positive := 1;
      Kind : ACPI_Service.Table_Kind := ACPI_Service.DSDT;
      Extent : Natural := 0;
      Used : Natural := 0;
      Buffer_Data : Firmware_Tables.Bytes (1 .. Table_Byte_Limit) := [others => 0];
   end record with Type_Invariant =>
     State.Used <= State.Extent and then State.Extent <= State.Table_Byte_Limit and then
     (if State.Open then State.Started and then
        ACPI_Bootstrap.Current (State.Boot) = ACPI_Bootstrap.Receiving
        and then State.Extent >= Firmware_Tables.Table_Header_Size
      else State.Used = 0 and then State.Extent = 0);
end ACPI_Requests;
