package Scheduler_Alarm is
   subtype Delay_Microseconds is Positive range 1 .. 1_000;
   -- CPU-local hardware adapter. Call with interrupts disabled. An earlier
   -- opportunity never postpones an already armed clock/quantum opportunity.
   procedure Request_Earlier (Delay_Us : Delay_Microseconds);
   -- Consume due software state before yielding. Early/stale hardware vectors
   -- rearm the remainder and cannot rotate an unrelated running context.
   function Interrupt_Due return Boolean;
end Scheduler_Alarm;
