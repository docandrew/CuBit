with Interfaces.C;
with Interfaces;
with System;
package Vulkan_Affine_Test_Bridge is
   type Input is record
      W, H, N, D, Rotation, X, Y, L, T, R, B, DL, DT, DR, DB, Over, Mask : Interfaces.C.int;
      Tint : Interfaces.Unsigned_32;
   end record with Convention => C;
   function Draw (Borrowed : System.Address; V : access constant Input)
     return Interfaces.C.int with Export, Convention => C, External_Name => "test_affine_and_record";
end Vulkan_Affine_Test_Bridge;
