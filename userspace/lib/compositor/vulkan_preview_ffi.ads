with System;
with Compositor_Preview_Geometry;
-- Trusted command-recording boundary, not submission/completion authority.
package Vulkan_Preview_FFI with SPARK_Mode is
   package P renames Compositor_Preview_Geometry;
   procedure Record_Draw
     (Borrowed : System.Address; Plan : P.Result;
      Width, Height : P.G.Physical_Extent; Accepted : out Boolean)
     with Global => null, Pre => Plan.Visible and then
       P.A.Valid (Plan.Transform, Width, Height);
end Vulkan_Preview_FFI;
