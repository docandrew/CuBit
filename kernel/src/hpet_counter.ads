with Interfaces; use Interfaces;
-- Pure HPET counter policy; no MMIO or hardware ownership is implied.
package HPET_Counter with SPARK_Mode, Pure is
   subtype Tick_Period is Unsigned_64 range 1 .. 100_000_000;
   function Admitted (Capabilities, Period_FS : Unsigned_32) return Boolean is
     (Capabilities /= Unsigned_32'Last and (Capabilities and 255) /= 0 and
      (Capabilities and 16#2000#) /= 0 and Period_FS in 1 .. 100_000_000);
   function Timer_Count (Capabilities : Unsigned_32) return Positive is
     (Natural (Shift_Right (Capabilities, 8) and 31) + 1)
     with Post => Timer_Count'Result in 1 .. 32;
   function Quiet_Timer (Configuration : Unsigned_32) return Unsigned_32 is
     (Configuration and not Unsigned_32'(16#4004#))
     with Post => (Quiet_Timer'Result and 16#4004#) = 0;
   -- Apply only while general enable is clear, to every advertised comparator.
   -- Clears interrupt-enable and FSB-enable; preserves unrelated fields.
   function Counter_Only (Configuration : Unsigned_32) return Unsigned_32 is
     ((Configuration and not Unsigned_32'(3)) or 1)
     with Post => (Counter_Only'Result and 3) = 1;
   function Microseconds (Ticks : Unsigned_64; Period_FS : Tick_Period)
     return Unsigned_64;
   -- Floor conversion only. Counter phase and hardware accuracy remain external
   -- error terms; this is not a promise of <=1us physical-clock uncertainty.
end HPET_Counter;
