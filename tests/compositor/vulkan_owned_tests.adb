with Interfaces.C; with Vulkan_Owned_Bridge; with Vulkan_Affine_Test_Bridge;
procedure Vulkan_Owned_Tests is
   use type Interfaces.C.int;
   function Run return Interfaces.C.int
     with Import, Convention => C, External_Name => "run_vulkan_affine_tests";
begin
   if Run /= 0 then raise Program_Error with "owned Vulkan pixel oracle failed"; end if;
end Vulkan_Owned_Tests;
