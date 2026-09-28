with Interfaces; use Interfaces;
-- Page-granularity authority, not per-register isolation. These contain the
-- ADL-N reset and engine-control registers; they exclude display/GGTT pages.
package Intel_GPU_Reset_Pages with SPARK_Mode, Pure is
   subtype Page_Index is Natural range 0 .. 5;
   function Offset (Index : Page_Index) return Unsigned_64 is
     (case Index is
         when 0 => 16#9000#, when 1 => 16#2000#, when 2 => 16#22000#,
         when 3 => 16#1C0000#, when 4 => 16#1D0000#, when 5 => 16#1C8000#)
     with Post => Offset'Result mod 4096 = 0 and Offset'Result < 16#200000#;
   function Slot (Index : Page_Index) return Unsigned_64 is (16 + Unsigned_64 (Index));
end Intel_GPU_Reset_Pages;
