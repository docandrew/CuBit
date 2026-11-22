with Interfaces; use Interfaces;
with Intel_GPU_Timeline;
generic
   with function Owner_Ready return Boolean;
   -- Reads the context's 64-bit PPHWSP timeline slot (high, low, high) with
   -- CPU visibility of DMA writes. Nonraising, bounded callback; no CPU
   -- writes to the slot after Arm. OK is False on any failed or unstable read.
   with procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean);
   with function Now_Us return Unsigned_64;
package Intel_GPU_Initial_Completion is
   type Phase is (Fresh, Armed, Observed, Quarantined);
   type Result is (Rejected, Ready, Pending, Complete, Ownership_Lost, Read_Failed,
                   Unexpected_Marker, Invalid_Clock, Timed_Out, Event_Failed);
   type Attempt is limited private;
   function State (Object : Attempt) return Phase;
   function Last_Marker (Object : Attempt) return Unsigned_64;
   function Marker_Reads (Object : Attempt) return Natural;
   -- Serialized submissions only: caller supplies the last completed value
   -- and its successor, with no outstanding work or CPU marker writes.
   -- First use is 0 -> 1. No wrap: retire the context before exhaustion.
   -- Checks the previous value BEFORE publication. The deadline is
   -- Budget_Us from now: explicit, no default. An Observed attempt may be
   -- armed again for the next job; any other state is rejected.
   procedure Arm (Object : in out Attempt; Budget_Us : Unsigned_64;
                  Status : out Result;
                  Previous_Value : Unsigned_32 := 0;
                  Expected_Value : Unsigned_32 := 1);
   -- One NON-BLOCKING observation, after publication has been accepted:
   -- one ownership check, one timeline read, one clock read. Pending means
   -- observe again later (the deadline has not passed). Gate_Open carries
   -- any further completion condition, e.g. the GuC acknowledged the enable
   -- that submitted the work. Complete means this submission's breadcrumb
   -- was observed, NOT permission to free backing: scheduling disable/reset
   -- and DMA lifetime handling are separate. Any other result quarantines.
   procedure Observe (Object : in out Attempt; Gate_Open : Boolean;
                      Status : out Result);
   procedure Fail (Object : in out Attempt);
   -- Bounded synchronous wait for startup and context registration only;
   -- never for a request handler (the submit path observes per loop turn).
   -- Services events and pauses between observations; Poll_Limit also
   -- bounds a frozen clock.
   generic
      with function Service_Events return Boolean;
      with procedure Pause;
   procedure Wait (Object : in out Attempt; Poll_Limit : Positive;
                   Status : out Result);
private
   type Attempt is limited record
      Value : Phase := Fresh;
      Timeline : Intel_GPU_Timeline.Waiter;
      Marker : Unsigned_64 := Unsigned_64'Last;
      Reads : Natural := 0;
   end record;
end Intel_GPU_Initial_Completion;
