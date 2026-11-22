with CuBit.Benchmark_Clock;

package body Probe_Clock is
   function Calibrate return Interfaces.Unsigned_64 is
      Rate : Interfaces.Unsigned_64;
   begin
      CuBit.Benchmark_Clock.Calibrate (Rate);
      return Rate;
   end Calibrate;

   function Counter return Interfaces.Unsigned_64 is
     (CuBit.Benchmark_Clock.Read_Counter);
end Probe_Clock;
