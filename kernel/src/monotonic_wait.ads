with Interfaces;
-- Backend-independent bounded minimum delay, not a scheduler sleep facility.
-- Read must be ordered, bounded, non-raising and migration-safe. Its timestamps
-- use microseconds in one stable epoch. Success=False means unavailable.
-- Pause must be bounded and non-raising; it need not sleep or advance the clock.
generic
   with procedure Read (Stamp : out Interfaces.Unsigned_64;
                        Success : out Boolean);
   with procedure Pause;
package Monotonic_Wait is
   type Outcome is (Completed, Unavailable, Regressed, Polls_Exhausted,
                    Unrepresentable);
   procedure At_Least
     (Duration_US, Overstatement_US : Interfaces.Unsigned_64;
      Poll_Limit : Positive; Result : out Outcome);
   -- The caller/backend must establish that the difference of any two reads
   -- over this operation overstates actual elapsed microseconds by AT MOST
   -- Overstatement_US (including conversion, counter phase and frequency
   -- error). Resolution alone is not such a bound. This is an assumption about
   -- hardware, not a property proved by this package.
   -- Completed implies observed difference >= Duration_US + Overstatement_US.
   -- A stopped or unavailable clock cannot cause an unbounded loop. A zero
   -- delay completes without touching the backend. Wrap is rejected, not
   -- interpreted as an enormous elapsed interval. No absolute deadline add.
end Monotonic_Wait;
