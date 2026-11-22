with Interfaces.C;
with System;
package Vulkan_Backdrop_Test_Bridge is
   function Draw
     (Borrowed : System.Address; W, H, SW, SH, Mode, L, T, R, B : Interfaces.C.int)
      return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_backdrop_record";
end Vulkan_Backdrop_Test_Bridge;
