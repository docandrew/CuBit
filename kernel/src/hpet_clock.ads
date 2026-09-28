with Interfaces;
-- One instance per exclusively owned HPET block. Initialize once during boot,
-- before publishing to concurrent readers. Device accesses must be ordered,
-- bounded and non-raising. Read64 must atomically read the 64-bit main counter.
generic
   with function Read32 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with function Read64 (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_64;
   with procedure Write32 (Offset, Value : Interfaces.Unsigned_32);
   with procedure Pause;
package HPET_Clock is
   type Startup_Status is (Not_Attempted, Identity_Rejected,
     Configuration_Unreadable, Disable_Not_Confirmed, Timer_Unreadable,
     Timer_Mask_Not_Confirmed, Counter_Unreadable, Enable_Not_Confirmed,
     Counter_Invalid, Counter_Stalled, Running);
   function Status return Startup_Status;
   function Timer_Offset return Interfaces.Unsigned_32;
   function Timer_Before return Interfaces.Unsigned_32;
   function Timer_After return Interfaces.Unsigned_32;
   -- Last initialization stage, retained without additional hardware reads.
   procedure Initialize (Poll_Limit : Positive; Success : out Boolean);
   function Available return Boolean;
   -- Unsigned_64'Last denotes unavailable/invalid/wrapped epoch.
   -- Hardware period accuracy and quantization are separate from conversion.
   function Microseconds return Interfaces.Unsigned_64;
end HPET_Clock;
