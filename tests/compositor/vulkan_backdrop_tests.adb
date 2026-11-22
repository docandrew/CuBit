with Ada.Text_IO;
with Interfaces.C;
with Vulkan_Affine_Test_Bridge;
with Vulkan_Backdrop_Test_Bridge;
procedure Vulkan_Backdrop_Tests is
   function Run return Interfaces.C.int with Import, Convention => C,
     External_Name => "run_vulkan_affine_tests";
   use type Interfaces.C.int;
begin
   if Run /= 0 then raise Program_Error with "Vulkan backdrop oracle failed"; end if;
   Ada.Text_IO.Put_Line ("VULKAN-BACKDROP: PASS hosted Mesa exact wallpaper pixels; NOT CuBit GPU");
end Vulkan_Backdrop_Tests;
