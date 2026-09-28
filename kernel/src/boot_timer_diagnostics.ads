with Interfaces;

--  BSP-only boot calibration. Hardware observations, not a SPARK proof of
--  interrupt delivery. Does not repair or change interrupt routing.
package Boot_Timer_Diagnostics with SPARK_Mode => Off is
   procedure Wait_For_Ticks (Count : Interfaces.Unsigned_64);
end Boot_Timer_Diagnostics;
