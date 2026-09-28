with Interfaces;

-- Benchmark-only FFI to the existing CuBit calibrated TSC instrumentation.
package Probe_Clock is
   function Calibrate return Interfaces.Unsigned_64
     with Export, Convention => C, External_Name => "cubit_probe_clock_calibrate";
   function Counter return Interfaces.Unsigned_64
     with Export, Convention => C, External_Name => "cubit_probe_clock_counter";
end Probe_Clock;
