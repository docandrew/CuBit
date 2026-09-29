with Interfaces; use Interfaces;
-- Page-granularity authority, not per-register isolation. These pages contain
-- POWER_WELL_CTL2/DC_STATE_EN, the PW1 chicken workaround, and pipe IRQs.
-- They exclude plane, GGTT and engine-control pages. A grant is not a power ref.
package Intel_GPU_Display_Pages with SPARK_Mode, Pure is
   Request_Label : constant := 16#0231#;
   subtype Page_Index is Natural range 0 .. 2;
   function Offset (Index : Page_Index) return Unsigned_64 is
     (case Index is when 0 => 16#45000#, when 1 => 16#46000#, when 2 => 16#44000#)
     with Post => Offset'Result mod 4096 = 0 and Offset'Result < 16#200000#;
   function Slot (Index : Page_Index) return Unsigned_64 is
     -- Slot23 belongs to the shared log publisher protocol. Do not replace
     -- its endpoint when granting the second display-power page.
     -- Slot26 is GGTT, slot27 is the log observer.
     (if Index = 2 then 28 else 24 + Unsigned_64 (Index));
end Intel_GPU_Display_Pages;
