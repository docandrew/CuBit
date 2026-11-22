with Interfaces;
--  ADLN single-domain handshake, not a reference-counted power manager.
--  Caller owns serialized access to this domain, holds PCI/runtime power,
--  and has validated writable MMIO mappings. No nested acquisitions.
--  Callbacks must provide ordered device accesses; Pause must return.
generic
   with function Read_32 (Offset : Interfaces.Unsigned_32)
     return Interfaces.Unsigned_32;
   with procedure Write_32 (Offset, Value : Interfaces.Unsigned_32);
   with procedure Pause;
   with function Now_Milliseconds return Interfaces.Unsigned_64;
   Request_Register, Ack_Register : Interfaces.Unsigned_32;
   with procedure Recover_Ack
     (Expected : Interfaces.Unsigned_32; Recovered : in out Boolean) is null;
   -- Validated platform register pair, fixed for this instance's lifetime.
package Intel_GPU_Forcewake is
   type Result is
     (Ready, Timed_Out, Poll_Exhausted, Invalid_MMIO, Invalid_Clock, Invalid_State);
   type Ownership_State is (Idle, Held, Faulted);
   --  One noncopyable lease per hardware domain, owned by one serialized
   --  caller. It is not a lock and must never be shared unsynchronized.
   type Lease is limited private;
   function State (Object : Lease) return Ownership_State;
   --  One normal-wait elapsed budget covers both acquisition phases. An
   --  optional recovery callback owns a separate bounded recovery budget per
   --  exhausted wait; it must confirm the desired ACK and fallback cleanup.
   --  It is never called for invalid MMIO/clock/state. Default: no recovery.
   --  Poll_Limit
   --  additionally bounds samples per phase if the clock stalls.
   --  Timed_Out means the elapsed deadline expired; Poll_Exhausted means
   --  the sample budget ended while the last clock reading was in budget.
   --  Clock regression/wrap fails closed. Callback execution must itself be bounded;
   --  this cannot preempt a blocked callback or guarantee scheduling latency.
   --  Ready is returned only after observing bit 0 asserted. An unsuccessful
   --  request is deasserted, but cleanup acknowledgement is not guaranteed;
   --  caller must quarantine/reset the domain rather than assume it is idle.
   procedure Acquire
     (Object : in out Lease; Poll_Limit : Positive; Status : out Result;
      Timeout_Milliseconds : Interfaces.Unsigned_64 := 50);
   procedure Release
     (Object : in out Lease; Poll_Limit : Positive; Status : out Result;
      Timeout_Milliseconds : Interfaces.Unsigned_64 := 50);
private
   type Lease is limited record
      Current : Ownership_State := Idle;
   end record;
end Intel_GPU_Forcewake;
