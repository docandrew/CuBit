-- BSP-only hardware handoff, before enabling interrupts or starting APs.
package Boot_Timer_Setup with SPARK_Mode => Off is
   procedure Take_Over;
end Boot_Timer_Setup;
