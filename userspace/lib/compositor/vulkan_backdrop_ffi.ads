with System;
with Compositor_Backdrop;
package Vulkan_Backdrop_FFI with SPARK_Mode is
   -- Exclusively recording compatible render pass; source/target resources,
   -- layouts and matching sampled-image extent are caller-owned assumptions.
   -- No allocation, barriers, submission, completion or retirement occurs here.
   procedure Record_Draw
     (Borrowed : System.Address; Description : Compositor_Backdrop.Draw;
      W, H : Compositor_Backdrop.G.Physical_Extent; Accepted : out Boolean)
     with Pre => Compositor_Backdrop.Valid (Description, W, H), Global => null;
end Vulkan_Backdrop_FFI;
