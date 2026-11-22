with Interfaces; use Interfaces;
with CuBit.Timing_Histograms;

--  x86 benchmark instrumentation, not the OS clock API. TSC conversion is
--  calibrated against guest milliseconds; cross-CPU TSC agreement is assumed.
package CuBit.Benchmark_Clock is
   function Read_Counter return Unsigned_64;
   procedure Calibrate (Ticks_Per_Millisecond : out Unsigned_64);
   procedure Print_Value (Value : Unsigned_64);
   procedure Report
     (Name : String; H : CuBit.Timing_Histograms.Histogram);
end CuBit.Benchmark_Clock;
