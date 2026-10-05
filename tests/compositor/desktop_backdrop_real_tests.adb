with Interfaces.C; with Desktop_Backdrop_Real_Bridge; with Vulkan_Affine_Test_Bridge;
with Vulkan_Target_Bundle_Bridge;
procedure Desktop_Backdrop_Real_Tests is
   use type Interfaces.C.int;
   function Run return Interfaces.C.int with Import, Convention => C, External_Name => "run_vulkan_affine_tests";
begin
   if Run /= 0 then raise Program_Error with "Desktop real Vulkan oracle failed"; end if;
end Desktop_Backdrop_Real_Tests;
