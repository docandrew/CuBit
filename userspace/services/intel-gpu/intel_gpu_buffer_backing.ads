with Interfaces; use Interfaces;
with Intel_GPU_Physical_Extents;
package Intel_GPU_Buffer_Backing with SPARK_Mode is
   -- Supervisor/driver-only bootstrap arena, not public Mesa handles.
   Request_Label : constant := 16#0236#;
   Extent_Request_Label : constant := 16#0237#;
   Budget_Request_Label : constant := 16#0238#;
   Retire_Request_Label : constant := 16#0239#;
   subtype Slot is Positive range 1 .. 16;
   -- Driver-internal identities must not change meaning when metadata grows.
   -- Low32 is a one-based slot; high32 is generation minus one.
   Ticket_Stride : constant Unsigned_64 := 2 ** 32;
   Ticket_Limit : constant Unsigned_64 := Unsigned_64'Last - Ticket_Stride;
   subtype Page_Count is Positive range 1 .. 4096;
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
   Capacity : constant Unsigned_64 := 32 * 1024 * 1024;
   -- Same per-process DMA aperture convention used by other native drivers.
   -- Intel's firmware/context windows are separate; these are CPU, not GPU VA.
   CPU_Base : constant Unsigned_64 := 16#0000_7000_0000_0000#;
   type Budget_Words is array (Natural range 0 .. 3) of Unsigned_64;
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
     (Observed_Capacity = Capacity and then Retained <= Capacity and then
      Retained mod 4096 = 0 and then Unused_Slots <= Unsigned_64 (Slot'Last)
      and then Retained >= (Unsigned_64 (Slot'Last) - Unused_Slots) * 4096
      and then Retained <= (Unsigned_64 (Slot'Last) - Unused_Slots) *
        Unsigned_64 (Page_Count'Last) * 4096);
   -- Read-only supervisor request [1,0,0,0], same-label four-word response
   -- [version,capacity,retained,unused slots]. Available bytes = capacity minus
   -- retained, but new allocations also need an unused slot. This is a point
   -- observation, never a reservation, physical address or reclaim promise.
   -- All-zero encoding means unavailable and is never sent as a success.
   function Budget_Reply
     (Known : Boolean; Observed_Capacity, Retained : Unsigned_64;
      Unused_Slots : Natural) return Budget_Words is
     (if Known and then Budget_Valid
         (Observed_Capacity, Retained, Unsigned_64 (Unused_Slots))
      then [1, Observed_Capacity, Retained, Unsigned_64 (Unused_Slots)]
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
         and then Reserved = 0 and then Data (0) = 1 and then
         Budget_Valid (Data (1), Data (2), Data (3))
      then (Known => True, Total_Bytes => Data (1), Retained_Bytes => Data (2),
            Free_Bytes => Data (1) - Data (2), Unused_Slots => Natural (Data (3)),
            Maximum_Allocation => (if Data (3) = 0 then 0 else
              Unsigned_64'Min (Data (1) - Data (2), Unsigned_64 (Page_Count'Last) * 4096)))
      else (others => <>))
     with Post =>
       (if Decode_Budget'Result.Known then
          Decode_Budget'Result.Total_Bytes = Capacity and then
          Decode_Budget'Result.Retained_Bytes <= Capacity and then
          Decode_Budget'Result.Free_Bytes = Capacity - Decode_Budget'Result.Retained_Bytes and then
          Decode_Budget'Result.Maximum_Allocation <= Decode_Budget'Result.Free_Bytes and then
          Decode_Budget'Result.Maximum_Allocation <= Unsigned_64 (Page_Count'Last) * 4096 and then
          Decode_Budget'Result.Unused_Slots <= Slot'Last and then
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
