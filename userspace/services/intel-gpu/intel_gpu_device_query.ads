with Interfaces; use Interfaces;
package Intel_GPU_Device_Query with SPARK_Mode, Pure is
   -- Intel backend read-only query on an already authorized endpoint.
   -- No buffer addresses, MMIO, capability delegation or submission here.
   Label : constant Unsigned_32 := 16#0A20#;
   Version : constant Unsigned_64 := 1;
   type Words is array (Natural range 0 .. 3) of Unsigned_64;
   Identity : constant Unsigned_64 := 0;
   Topology : constant Unsigned_64 := 1;
   Timestamp : constant Unsigned_64 := 2;
   Memory : constant Unsigned_64 := 3;
   Virtual_Memory : constant Unsigned_64 := 4;
   type VM_Contract is (VM_Unavailable, Private_PPGTT_48);
   type Memory_Contract is
     (Not_Admitted, Owned_WB_Explicit_Maintenance, Owned_WB_Coherent);
   -- Only owned system-RAM allocations and their matching WB CPU grants.
   -- Excludes GGTT aperture, imported buffers and firmware memory. Coherent
   -- does not waive GPU barriers, initialization or ownership/retirement.
   OK : constant Unsigned_64 := 0;
   Bad_Request : constant Unsigned_64 := 1;
   Unavailable : constant Unsigned_64 := 2;
   Unsupported : constant Unsigned_64 := 3;
   type Snapshot is record
      Device : Unsigned_16 := 0;
      Revision : Unsigned_8 := 0;
      Topology_Observed : Boolean := False;
      DSS_Mask : Unsigned_8 := 0;
      EU_Mask : Unsigned_16 := 0;
   end record;
   -- Request [version, selector, 0, 0], exact four-word/zero-flag envelope.
   -- Response [status, version, value0, value1].
   -- Identity: value0 = vendor16/device16/revision8; value1 = render features.
   -- Render features are ZERO until public allocation/submission exists.
   -- Topology: value0 = DSS mask; value1 = common EU mask per enabled DSS.
   -- Timestamp: value0 = retained CS frequency in Hz; value1 = 0.
   -- Memory: value0 = 1 explicit-maintenance WB, 2 coherent WB; value1 = 0.
   -- Virtual_Memory: value0 = raw GPU address bits (48); value1 = 1,
   -- private per-session PPGTT. Caller supplies Private_PPGTT_48 only for a
   -- currently authenticated healthy render session. This is not allocated
   -- physical capacity, a reservation, or a guarantee of future availability.
   -- Not_Admitted returns Unavailable with zero payload. This query differs
   -- from inventory: caller must supply a currently admitted memory policy.
   -- Caller supplies an observation only while its clock ownership remains
   -- valid. Zero (default) means not observed; never use a platform guess.
   -- Identity/topology/clock are retained observations, NOT current ownership,
   -- power state or readiness. Failures never return partial/stale payloads.
   function Respond
     (Data : Snapshot; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Timestamp_Hz : Unsigned_32 := 0;
      Memory_Policy : Memory_Contract := Not_Admitted;
      VM_Policy : VM_Contract := VM_Unavailable) return Words
   with Post =>
     (Respond'Result (1) = Version and then
      (if Respond'Result (0) /= OK then
         Respond'Result (2) = 0 and Respond'Result (3) = 0));
end Intel_GPU_Device_Query;
