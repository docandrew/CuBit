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
   -- Caller supplies an observation only while its clock ownership remains
   -- valid. Zero (default) means not observed; never use a platform guess.
   -- Retained observations for this endpoint lifetime, NOT current ownership,
   -- power state or readiness. Failures never return partial/stale payloads.
   function Respond
     (Data : Snapshot; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Timestamp_Hz : Unsigned_32 := 0) return Words
   with Post =>
     (Respond'Result (1) = Version and then
      (if Respond'Result (0) /= OK then
         Respond'Result (2) = 0 and Respond'Result (3) = 0));
end Intel_GPU_Device_Query;
