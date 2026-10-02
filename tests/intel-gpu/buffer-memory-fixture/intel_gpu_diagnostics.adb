with CuBit.Messages;
package body Intel_GPU_Diagnostics is
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64 is
     (CuBit.Messages.Poll (Result));
end Intel_GPU_Diagnostics;
