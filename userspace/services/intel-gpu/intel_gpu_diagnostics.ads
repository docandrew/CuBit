with Interfaces;
with System;
with CuBit.Log_Records;
package Intel_GPU_Diagnostics is
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
   -- Bounded nonblocking work. Never polls or steals driver completions.
   procedure Tick;
   -- Sole raw completion consumer: routes log tokens internally and returns
   -- the first unrelated completion unchanged to the caller.
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
end Intel_GPU_Diagnostics;
