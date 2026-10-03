with System;
with Compositor_Affine;
with Compositor_Transform;
with Compositor_Source_Region;
-- Exclusive borrowed command state and matching descriptor image dimensions
-- remain caller obligations. Global null abstracts native recording effects.
package Vulkan_Region_FFI with SPARK_Mode is
   use type Compositor_Transform.Coefficients;
   procedure Record_Draw (Borrowed : System.Address; D : Compositor_Affine.Draw;
      C : Compositor_Transform.Coefficients;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Region : Compositor_Source_Region.Rectangle; Accepted : out Boolean)
     with Global => null, Pre => Compositor_Source_Region.Valid (Region) and then
       Compositor_Affine.Valid (D, Width, Height) and then
       C = Compositor_Transform.Build (D, Width, Height);
end Vulkan_Region_FFI;
