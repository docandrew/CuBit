with Interfaces.C; with Vulkan_Affine_Test_Bridge; with Native_Scene_Bridge;
procedure Native_Scene_Pixels is
   use type Interfaces.C.int;
   function Run return Interfaces.C.int
     with Import, Convention => C, External_Name => "run_vulkan_affine_tests";
begin
   if Run /= 0 then raise Program_Error with "native scene bridge pixel oracle failed"; end if;
end Native_Scene_Pixels;
