with Interfaces.C;
with System;
package Vulkan_Copy_Test_Bridge is
   type Input is record
      TW, TH, SW, SH, X, Y, W, H, Clipped, CX, CY, CW, CH : Interfaces.C.int;
   end record with Convention => C;
   function Draw (Borrowed : System.Address; Value : access constant Input)
     return Interfaces.C.int
     with Export, Convention => C, External_Name => "test_plan_and_record";
end Vulkan_Copy_Test_Bridge;
