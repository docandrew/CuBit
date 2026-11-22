with System;
with Interfaces;
package Intel_GPU_Diagnostics is
   -- Transport oracle does not connect to logstore; diagnostics cannot steal IPC.
   procedure Capture (Text : String) is null;
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
end Intel_GPU_Diagnostics;
