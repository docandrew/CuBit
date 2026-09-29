with Interfaces; use Interfaces;
generic
   with function Owner_Ready return Boolean;
   -- Reads the aligned64-bit context HWSP D0 marker with CPU visibility of
   -- DMA writes. Nonraising, bounded callbacks; no CPU writes after Arm.
   with procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean);
   -- Dispatch a bounded number of GuC events; False includes transport or
   -- context failure. Valid unrelated events must be retained, not discarded.
   with function Service_Events return Boolean;
   with function Now_Us return Unsigned_64;
   with procedure Pause;
package Intel_GPU_Initial_Completion is
   type Phase is (Fresh, Armed, Observed, Quarantined);
   type Result is (Rejected, Ready, Complete, Ownership_Lost, Read_Failed,
                   Unexpected_Marker, Invalid_Clock, Timed_Out, Event_Failed);
   type Attempt is limited private;
   function State (Object : Attempt) return Phase;
   function Last_Marker (Object : Attempt) return Unsigned_64;
   function Marker_Reads (Object : Attempt) return Natural;
   -- Caller supplies a never-scheduled, exclusively owned context. Checks
   -- initial zero before publication/enable; deadline starts here, not later.
   procedure Arm (Object : in out Attempt; Status : out Result);
   -- Call only after publication and scheduling enable have been accepted.
   -- One second, including callbacks; Poll_Limit also bounds a frozen clock.
   -- Complete means the one-shot marker was observed, NOT permission to free
   -- backing: scheduling disable/reset and DMA lifetime handling are separate.
   procedure Wait (Object : in out Attempt; Poll_Limit : Positive;
                   Status : out Result);
   procedure Fail (Object : in out Attempt);
private
   type Attempt is limited record
      Value : Phase := Fresh;
      Started, Previous : Unsigned_64 := 0;
      Marker : Unsigned_64 := Unsigned_64'Last;
      Reads : Natural := 0;
   end record;
end Intel_GPU_Initial_Completion;
