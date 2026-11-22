with Ada.Text_IO;
with Interfaces.C;
with Vulkan_Copy_Test_Bridge;
procedure Vulkan_Copy_Tests is
   function Run return Interfaces.C.int with Import, Convention => C,
     External_Name => "run_vulkan_copy_tests";
   use type Interfaces.C.int;
begin
   if Run /= 0 then raise Program_Error with "Vulkan copy oracle failed"; end if;
   Ada.Text_IO.Put_Line ("VULKAN-COPY: PASS hosted Mesa pixels + actual SPARK planner/FFI; NOT CuBit GPU");
end Vulkan_Copy_Tests;
