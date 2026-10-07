with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
package Intel_GPU_Buffer_Backing with SPARK_Mode is
   -- Supervisor/driver-only bootstrap arena, not public Mesa handles.
   Request_Label : constant := 16#0236#;
   Extent_Request_Label : constant := 16#0237#;
   Budget_Request_Label : constant := 16#0238#;
   Retire_Request_Label : constant := 16#0239#;
   subtype Slot is Positive;
   Bootstrap_Slots : constant Positive := 16;
   -- Driver-internal identities must not change meaning when metadata grows.
   -- Low32 is a one-based slot; high32 is generation minus one.
   Ticket_Stride : constant Unsigned_64 := 2 ** 32;
   Ticket_Limit : constant Unsigned_64 := Unsigned_64'Last - Ticket_Stride;
   subtype Page_Count is Positive range 1 .. 4096;
   type Allocation_Reason is
     (Not_Attempted, Request_Check, Generation_Check, Quota_Check, Gap_Check,
      Backing_Check, Owner_Check, Geometry_Check, Directory_Check,
      Metadata_Check, Physical_Call, Physical_Result_Check, View_Check, Ready);
   -- F001 length4 diagnostic denial: [version, allocation key, bytes, reason].
   -- No addresses or authority. Plain F001 length0 remains an admission denial.
   Denial_Version : constant Unsigned_64 := 1;
   function Allocation_Reason_Name (Reason : Allocation_Reason) return String is
     (case Reason is
        when Not_Attempted => "not-entered",
        when Request_Check => "request",
        when Generation_Check => "generation-or-size",
        when Quota_Check => "quota",
        when Gap_Check => "virtual-gap",
        when Backing_Check => "quarantined-backing",
        when Owner_Check => "owner",
        when Geometry_Check => "geometry",
        when Directory_Check => "directory",
        when Metadata_Check => "extent-metadata",
        when Physical_Call => "physical-call",
        when Physical_Result_Check => "physical-result",
        when View_Check => "view",
        when Ready => "ready");
   -- Shared by early readiness and normal supervisor dispatch. This checks
   -- wire shape only; both callers must independently authenticate the sender.
   function Valid_Allocation_Request
     (Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Index, Pages, Generation, Padding : Unsigned_64) return Boolean is
     (Label = Request_Label and then Length = 3 and then Flags = 0 and then
      Reserved = 0 and then Index in 1 .. Unsigned_64 (Slot'Last) and then
      Pages in 1 .. Unsigned_64 (Page_Count'Last) and then
      Generation in 1 .. Unsigned_64 (Unsigned_32'Last) and then Padding = 0);
   -- Allocation request: [slot, pages, generation], generation nonzero U32.
   -- F004 reply: [CPU, bytes, arena, allocation key]. Keep the full generation
   -- in the key; an old slot-only reply must not admit a newer allocation.
   -- Authority still comes from the pinned supervisor endpoint, not this key.
   function Allocation_Key (Index : Slot; Generation : Unsigned_32)
     return Unsigned_64 is
     (if Generation = 0 then 0 else
        Unsigned_64 (Generation) * 2 ** 32 + Unsigned_64 (Index))
     with Post =>
       (if Generation = 0 then Allocation_Key'Result = 0 else
          Allocation_Key'Result / 2 ** 32 = Unsigned_64 (Generation) and then
          Allocation_Key'Result mod 2 ** 32 = Unsigned_64 (Index));
   -- Contiguous CPU arena backed by independently allocated physical extents.
   Capacity : constant Unsigned_64 := Intel_GPU_Physical_Extents.Capacity;
   -- Same per-process DMA aperture convention used by other native drivers.
   -- Intel's firmware/context windows are separate; these are CPU, not GPU VA.
   CPU_Base : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   type Heap_Policy is record
      Byte_Quota, DMA_Limit, Metadata_Bytes : Unsigned_64;
   end record;
   -- Shared trusted bootstrap policy, not a wire grant or hardware inventory.
   -- Backing quota, DMA address ceiling and CPU metadata budget are independent.
   Default_Heap : constant Heap_Policy :=
     (Byte_Quota => Capacity, DMA_Limit => 2 ** 32, Metadata_Bytes => 65536);
   -- Existing kernel SYSINFO MEM_TOTAL: immutable buddy-managed RAM bytes,
   -- not free memory and not device-local VRAM. Both native endpoints select
   -- the same policy before their first allocation. Unknown inventory fails
   -- closed rather than silently retaining the experimental 32MiB default.
   Managed_RAM_Query : constant Unsigned_64 := 1601;
   -- System-backed Intel bring-up policy: at most one quarter of managed RAM
   -- and half the current DMA aperture. Neither quantity reserves physical
   -- pages; allocator failure remains possible. The 32-bit DMA ceiling is
   -- deliberately unchanged until hardware/IOMMU addressing is validated.
   function Native_System_Heap (Managed_RAM : Unsigned_64) return Heap_Policy is
     (Byte_Quota =>
        (if Managed_RAM = Unsigned_64'Last then 0 else
           Unsigned_64'Min (Managed_RAM / 4, Default_Heap.DMA_Limit / 2) /
             Intel_GPU_Physical_Extents.Block_Bytes * Intel_GPU_Physical_Extents.Block_Bytes),
      DMA_Limit => Default_Heap.DMA_Limit,
      Metadata_Bytes => Default_Heap.Metadata_Bytes)
     with Post =>
       Native_System_Heap'Result.DMA_Limit = Default_Heap.DMA_Limit and then
       Native_System_Heap'Result.Metadata_Bytes = Default_Heap.Metadata_Bytes and then
       Native_System_Heap'Result.Byte_Quota mod Intel_GPU_Physical_Extents.Block_Bytes = 0 and then
       Native_System_Heap'Result.Byte_Quota <= Default_Heap.DMA_Limit / 2 and then
       (if Managed_RAM = Unsigned_64'Last then
          Native_System_Heap'Result.Byte_Quota = 0
        else Native_System_Heap'Result.Byte_Quota <= Managed_RAM / 4);
   function Heap_Geometry_Valid
     (Byte_Quota, DMA_Limit, CPU : Unsigned_64) return Boolean is
     (CPU /= 0 and then CPU < 2 ** 47 and then
      CPU mod Intel_GPU_Physical_Extents.Block_Bytes = 0 and then
      Byte_Quota /= 0 and then Byte_Quota mod Intel_GPU_Physical_Extents.Block_Bytes = 0 and then
      Byte_Quota / Intel_GPU_Physical_Extents.Block_Bytes <= Unsigned_64 (Natural'Last) and then
      Byte_Quota <= 2 ** 47 - CPU and then
      DMA_Limit >= 2 * Intel_GPU_Physical_Extents.Block_Bytes and then
      DMA_Limit mod Intel_GPU_Physical_Extents.Block_Bytes = 0);
   type Budget_Words is array (Natural range 0 .. 3) of Unsigned_64;
   -- Shared query gate for live devmgr and native regression router. The
   -- committed prefix is supervisor-owned state, NOT a value from the sender.
   -- Reading geometry does not allocate pages or confer mapping authority.
   function Extent_Request_Authorized
     (Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Budget_Words; Sender, Authority, Owner, Committed_Bytes : Unsigned_64;
      Arena_Granted : Boolean) return Boolean is
     (Label = Extent_Request_Label and then Length = 2 and then Flags = 0 and then
      Reserved = 0 and then Owner /= 0 and then Sender = Owner and then
      Authority = 16#4947# and then Arena_Granted and then
      Data (1) = Owner and then Data (2) = 0 and then Data (3) = 0 and then
      Committed_Bytes >= Intel_GPU_Physical_Extents.Block_Bytes and then
      Committed_Bytes <= 2 ** 47 - CPU_Base and then
      Committed_Bytes mod Intel_GPU_Physical_Extents.Block_Bytes = 0 and then
      Data (0) < Committed_Bytes / Intel_GPU_Physical_Extents.Block_Bytes);
   Budget_Version : constant Unsigned_64 := 2;
   Maximum_Record_Count : constant Unsigned_64 := 2 ** 31 - 1;
   -- Private supervisor operation [slot,generation,arena,all-references-retired].
   -- Sender/authority must come from the kernel envelope, Owner from the
   -- supervisor's pinned Intel incarnation. The final word is a trusted DRIVER
   -- assertion, never accepted from an app. It includes GPU/TLB/CPU retirement.
   -- Successful same-label ack is [0,allocation-key,arena,0]; a lost ack must
   -- quarantine, not trigger replay. No physical DMA blocks are released.
   function Retirement_Authorized
     (Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Budget_Words; Sender, Authority, Owner : Unsigned_64;
      Arena_Granted : Boolean) return Boolean is
     (Label = Retire_Request_Label and then Length = 4 and then Flags = 0 and then
      Reserved = 0 and then Owner /= 0 and then Sender = Owner and then
      Authority = 16#4947# and then Arena_Granted and then
      Data (0) in 1 .. Unsigned_64 (Slot'Last) and then
      Data (1) in 1 .. Unsigned_64 (Unsigned_32'Last) - 1 and then
      Data (2) = Owner and then Data (3) = 1);
   function Budget_Valid
     (Observed_Capacity, Retained, Unused_Slots : Unsigned_64) return Boolean is
     (Observed_Capacity > 0 and then Observed_Capacity mod 4096 = 0 and then
      Retained <= Observed_Capacity and then Retained mod 4096 = 0 and then
      Unused_Slots <= Maximum_Record_Count);
   -- Read-only supervisor request [2,0,0,0], same-label four-word response
   -- [version,capacity,retained,unused slots]. Slots may include identities
   -- allowed by expandable metadata policy, not yet committed records. They
   -- are an admission upper bound, not a promise of available metadata RAM.
   -- Available bytes = capacity minus
   -- retained, but new allocations also need a record. These independent
   -- quantities never infer allocated bytes from record count. Version2 keeps
   -- the current16MiB per-allocation ceiling; it does not grant a larger arena.
   -- This is a point
   -- observation, never a reservation, physical address or reclaim promise.
   -- Capacity/free bytes describe the arena admission quota; backing is now
   -- acquired on demand and physical allocation can fail within that quota.
   -- All-zero encoding means unavailable and is never sent as a success.
   function Budget_Reply
     (Known : Boolean; Observed_Capacity, Retained : Unsigned_64;
      Unused_Slots : Natural) return Budget_Words is
     (if Known and then Budget_Valid
         (Observed_Capacity, Retained, Unsigned_64 (Unused_Slots))
      then [Budget_Version, Observed_Capacity, Retained, Unsigned_64 (Unused_Slots)]
      else [others => 0]);
   type Budget_Snapshot is record
      Known : Boolean := False;
      Total_Bytes, Retained_Bytes, Free_Bytes, Maximum_Allocation : Unsigned_64 := 0;
      Unused_Slots : Natural := 0;
   end record;
   -- The transport must separately authenticate the supervisor endpoint and
   -- returned call tag. No cached budget is valid after ownership is lost.
   function Decode_Budget
     (Label : Unsigned_32; Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Data : Budget_Words) return Budget_Snapshot is
     (if Label = Budget_Request_Label and then Length = 4 and then Flags = 0
         and then Reserved = 0 and then Data (0) = Budget_Version and then
         Budget_Valid (Data (1), Data (2), Data (3))
      then (Known => True, Total_Bytes => Data (1), Retained_Bytes => Data (2),
            Free_Bytes => Data (1) - Data (2), Unused_Slots => Natural (Data (3)),
            Maximum_Allocation => (if Data (3) = 0 then 0 else
              Unsigned_64'Min (Data (1) - Data (2), Unsigned_64 (Page_Count'Last) * 4096)))
      else (others => <>))
     with Post =>
       (if Decode_Budget'Result.Known then
          Decode_Budget'Result.Total_Bytes > 0 and then
          Decode_Budget'Result.Retained_Bytes <= Decode_Budget'Result.Total_Bytes and then
          Decode_Budget'Result.Free_Bytes = Decode_Budget'Result.Total_Bytes - Decode_Budget'Result.Retained_Bytes and then
          Decode_Budget'Result.Maximum_Allocation <= Decode_Budget'Result.Free_Bytes and then
          Decode_Budget'Result.Maximum_Allocation <= Unsigned_64 (Page_Count'Last) * 4096 and then
          Unsigned_64 (Decode_Budget'Result.Unused_Slots) <= Maximum_Record_Count and then
          (if Decode_Budget'Result.Unused_Slots = 0 then Decode_Budget'Result.Maximum_Allocation = 0)
        else Decode_Budget'Result.Total_Bytes = 0 and
          Decode_Budget'Result.Retained_Bytes = 0 and Decode_Budget'Result.Free_Bytes = 0 and
          Decode_Budget'Result.Maximum_Allocation = 0 and Decode_Budget'Result.Unused_Slots = 0);
   function Valid_Physical (Physical : Unsigned_64) return Boolean is
     (Physical /= 0 and then Physical mod 4096 = 0 and then
      Physical <= 2 ** 32 - Capacity);
   pragma Compile_Time_Error (Capacity /= Intel_GPU_Physical_Extents.Capacity,
                              "buffer arena/extent capacity disagree");
end Intel_GPU_Buffer_Backing;
