--  PERF-011 diagnostic: the same small drawing and memory experiments run
--  natively (main.adb) and hosted (tests/metricsview), so the two
--  platforms' per-pixel costs can be compared on identical work.
with Interfaces; use Interfaces;
with CuBit.UI;
generic
   with function Now_Us return Unsigned_64;
   with procedure Redraw;
   with procedure Print (Line : String);
   with procedure Sleep_Ms (Ms : Natural);
package Perf_Probe is
   procedure Run (Frame : CuBit.UI.Canvas);
end Perf_Probe;
