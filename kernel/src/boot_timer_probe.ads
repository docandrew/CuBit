with Interfaces;

-- BSP-only observation during the initial PIT calibration, before AP startup.
-- Start/Stop and report reads require interrupts disabled. IRQ hooks run with
-- interrupts disabled too. No locks, allocation, serial output, or routing
-- changes in the hooks. Disabled for the normal scheduler lifetime.
package Boot_Timer_Probe with SPARK_Mode => Off is
   Enabled : Boolean := False with Volatile;
   Entries, Completed, PIC_IRQ0 : Interfaces.Unsigned_64 := 0 with Volatile;
   ISR_Bits : Interfaces.Unsigned_8 := 0 with Volatile;
   Minimum_Gap, Maximum_Gap, Maximum_Handler : Interfaces.Unsigned_64 := 0
     with Volatile;
   procedure Start;
   procedure Stop;
   procedure Enter_IRQ (Stamp : out Interfaces.Unsigned_64);
   procedure Leave_IRQ (Stamp : Interfaces.Unsigned_64);
end Boot_Timer_Probe;
