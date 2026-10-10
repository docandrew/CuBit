with CuBit.Appearance;
with Vulkan_Submission;
-- Capture the desktop's immutable wallpaper or solid background using the
-- existing physical-output backdrop sampler. Capture pins the source until
-- its CPU snapshot and confirmed GPU readers retire.
package Desktop_GPU_Scene.Backdrop with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   -- Settings preview: logical bounds follow output DPI/rotation; unlike the
   -- full wallpaper, it must not use the physical-output backdrop sampler.
   procedure Capture_Preview
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Bounds : V.A.G.Logical_Rectangle;
      Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid,
       Post => Valid (S) and D.Valid and
         (if Accepted then Current (S) = Capturing and
          Layer_Count (S) = Layer_Count (S)'Old +
            (if Style.Backdrop in CuBit.Appearance.Wallpaper | CuBit.Appearance.Cubie then 2 else 1));
   -- Damage is the pass's physical repair box. Its clip is set first: scene
   -- clips are absolute and persist, so without it the full-output fill and
   -- wallpaper would inherit the previous draw's clip (for example the
   -- cursor footprint of an earlier repair box) and paint wallpaper over the
   -- layers already drawn there.
   procedure Capture
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid and
       (if Accepted then Current (S) = Capturing and
          Layer_Count (S) = Layer_Count (S)'Old + 1 +
            (if Style.Backdrop in CuBit.Appearance.Wallpaper | CuBit.Appearance.Cubie then 2 else 1));
end Desktop_GPU_Scene.Backdrop;
