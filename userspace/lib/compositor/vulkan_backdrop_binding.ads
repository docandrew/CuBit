with System;
with Compositor_Backdrop;
package Vulkan_Backdrop_Binding with SPARK_Mode is
   package B renames Compositor_Backdrop;
   type Outcome is (Empty, Recorded, Rejected);
   -- Follows an opaque background fill. Fit/Center exterior fragments discard
   -- and preserve that background. Recorded does not mean GPU completion.
   procedure Draw_Output
     (Borrowed : System.Address; W, H, Source_W, Source_H : B.G.Physical_Extent;
      Mode : B.S.Placement; Damage : B.G.Physical_Rectangle; Result : out Outcome)
     with Global => null;
end Vulkan_Backdrop_Binding;
