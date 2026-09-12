-- Isolated BOOT diagnostic, never a scheduling/admission mechanism. Called
-- before starting this CPU's scheduler, with interrupts disabled on entry/exit.
-- It temporarily owns the local timer and does not advance Time.msTicks.
package Timer_Expiry_Probe is
   procedure Run;
   function Handle_Interrupt return Boolean;
end Timer_Expiry_Probe;
