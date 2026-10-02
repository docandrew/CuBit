with System;
with Interfaces;
package Intel_GPU_Diagnostics is
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
end Intel_GPU_Diagnostics;
