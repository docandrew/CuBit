with Interfaces;
-- ADL-N full GT reset only. NOT PCI/display reset or individual media reset.
-- Caller must already hold all required forcewake domains, stop/drain every
-- admitted engine, prepare every engine, exclude all submission and preserve
-- scanout mappings. Success does not initialize engines or upload firmware.
generic
   with function Read_Reset return Interfaces.Unsigned_32;
   with procedure Write_Reset (Value : Interfaces.Unsigned_32);
   with function Now return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_GT_Reset is
   type Result is (Complete, Timed_Out, Invalid_MMIO, Invalid_Clock, Invalid_State);
   type State is (Fresh, Quarantined, Reset_Complete);
   type Attempt is limited private;
   function Current (Object : Attempt) return State;
   -- Ordered, non-raising, bounded callbacks; Now is monotonic microseconds,
   -- Last means unavailable. For physical intervals <50us, timestamp difference
   -- must overstate elapsed time by at most 2us (all error sources included).
   -- No retry after failure. Caller cancels preparation separately and retains
   -- backing/ownership on ambiguity. Full reset affects all engines, not display.
   procedure Execute (Object : in out Attempt; Poll_Limit : Positive;
                      Status : out Result);
private
   type Attempt is limited record
      Value : State := Fresh;
   end record;
end Intel_GPU_GT_Reset;
