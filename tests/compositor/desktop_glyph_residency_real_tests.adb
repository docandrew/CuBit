with Interfaces.C; with Desktop_Glyph_Residency_Real_Bridge; with Vulkan_Affine_Test_Bridge;
with Vulkan_Target_Bundle_Bridge; with Vulkan_Submission;
procedure Desktop_Glyph_Residency_Real_Tests is
   use type Interfaces.C.int, Interfaces.C.unsigned;
   function Run return Interfaces.C.int with Import, Convention => C, External_Name => "run_vulkan_affine_tests";
   function Native_Sources return Interfaces.C.unsigned
     with Import, Convention => C, External_Name => "desktop_glyph_residency_real_source_capacity";
   function Native_Owned return Interfaces.C.unsigned
     with Import, Convention => C, External_Name => "desktop_glyph_residency_real_owned_capacity";
begin
   --  The C descriptor pool and owned-image tables mirror the Ada slot space.
   if Native_Sources /= Vulkan_Submission.Source_Capacity or else Native_Owned /= Vulkan_Submission.Source_Capacity then
      raise Program_Error with "native source capacity differs from Vulkan_Submission.Source_Capacity";
   end if;
   if Run /= 0 then raise Program_Error with "Desktop real Vulkan oracle failed"; end if;
end Desktop_Glyph_Residency_Real_Tests;
