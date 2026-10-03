with CuBit.Appearance;
with Vulkan_Submission;
-- Capture the desktop's immutable wallpaper or solid background using the
-- existing physical-output backdrop sampler. Capture pins the source until
-- its CPU snapshot and confirmed GPU readers retire.
package Desktop_GPU_Scene.Backdrop with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   procedure Capture
     (S : in out State; Style : CuBit.Appearance.Preferences;
      Source : Vulkan_Submission.Source_Ticket; Accepted : out Boolean)
     with Global => (In_Out => D.Engine), Pre => Valid (S) and D.Valid, Post => Valid (S) and D.Valid and
       (if Accepted then Current (S) = Capturing and
          Layer_Count (S) = Layer_Count (S)'Old +
            (if Style.Backdrop in CuBit.Appearance.Wallpaper | CuBit.Appearance.Cubie then 2 else 1));
end Desktop_GPU_Scene.Backdrop;
