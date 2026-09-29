with Interfaces;
with System;
package Intel_GPU_Diagnostics is
   procedure Capture (Text : String);
   -- Bounded nonblocking work. Never polls or steals driver completions.
   procedure Tick;
   -- Sole raw completion consumer: routes log tokens internally and returns
   -- the first unrelated completion unchanged to the caller.
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
end Intel_GPU_Diagnostics;
