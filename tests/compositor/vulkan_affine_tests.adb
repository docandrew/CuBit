with Ada.Text_IO;
with Interfaces.C;
with Vulkan_Affine_Test_Bridge;
procedure Vulkan_Affine_Tests is
   function Run return Interfaces.C.int with Import, Convention => C,
     External_Name => "run_vulkan_affine_tests";
   use type Interfaces.C.int;
begin
   if Run /= 0 then raise Program_Error with "Vulkan affine oracle failed"; end if;
   Ada.Text_IO.Put_Line ("VULKAN-AFFINE: PASS hosted Mesa shader rendering; NOT CuBit GPU");
end Vulkan_Affine_Tests;
