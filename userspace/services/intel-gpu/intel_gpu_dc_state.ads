with Interfaces;
package Intel_GPU_DC_State with SPARK_Mode is
   use Interfaces;
   type Field is (Clock_Control, PLL, Reference, Buffer_0, Buffer_1, Buffer_2, Buffer_3);
   type Snapshot is array (Field) of Unsigned_32;
   function Register_Offset (Item : Field) return Unsigned_32 is
     (case Item is when Clock_Control => 16#46000#, when PLL => 16#46070#,
       when Reference => 16#51004#, when Buffer_0 => 16#45008#,
       when Buffer_1 => 16#44FE8#, when Buffer_2 => 16#44300#,
       when Buffer_3 => 16#44304#);
   type Outcome is (Invalid_Read, Invalid_Clock, Clock_Changed, Clock_Unsettled,
                    Buffer_Changed, Buffer_Unsettled, Preserved);
   -- ADL-N (no clock squashing). Before is trusted retained configuration
   -- captured under display ownership, not an untrusted proposed baseline.
   -- Retain programmed requests, but allow lock/state bits to settle on exit.
   -- Caller separately obtains stable samples, DC-off and PHY restoration.
   -- Preserved is NOT a complete display-power reference.
   function Check (Before, After : Snapshot) return Outcome
   with Global => null,
     Post => (if Check'Result = Preserved then
       Before (Clock_Control) = After (Clock_Control) and then
       (Before (PLL) and 16#800000FF#) = (After (PLL) and 16#800000FF#) and then
       (for all F in Buffer_0 .. Buffer_3 =>
         (Before (F) and 16#BFFFFFFF#) = (After (F) and 16#BFFFFFFF#) and
         ((After (F) and 16#80000000#) /= 0) = ((After (F) and 16#40000000#) /= 0)));
end Intel_GPU_DC_State;
