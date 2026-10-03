with System;
with Compositor_Affine;
with Compositor_Transform;
-- Trusted ABI boundary. Global null abstracts exclusively borrowed native
-- command state, not physical absence of writes to a Vulkan command buffer.
package Vulkan_Affine_FFI with SPARK_Mode is
   use type Compositor_Transform.Coefficients;
   procedure Record_Draw
     (Borrowed : System.Address; D : Compositor_Affine.Draw;
      C : Compositor_Transform.Coefficients;
      Width, Height : Compositor_Affine.G.Physical_Extent; Mask : Boolean;
      Tint : Compositor_Affine.Word; Accepted : out Boolean)
     with Global => null,
       Pre => Compositor_Affine.Valid (D, Width, Height) and then
         C = Compositor_Transform.Build (D, Width, Height);
end Vulkan_Affine_FFI;
