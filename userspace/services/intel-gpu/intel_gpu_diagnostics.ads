with Interfaces;
with System;
with CuBit.Log_Records;
package Intel_GPU_Diagnostics is
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
   -- Bounded nonblocking work. Never polls or steals driver completions.
   procedure Tick;
   -- Sole raw completion consumer: handles the log grant's reply and returns
   -- the first other completion unchanged to the caller. (Publications
   -- complete in the publisher's ring, not on the completion queue.)
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
end Intel_GPU_Diagnostics;
