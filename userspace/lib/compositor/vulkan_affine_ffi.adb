package body Vulkan_Affine_FFI with SPARK_Mode => Off is
   use type Compositor_Affine.Word;
   function Submit
     (Borrowed : System.Address; D : access constant Compositor_Affine.Draw;
      C : access constant Compositor_Transform.Coefficients;
      Width, Height, Mask, Tint : Compositor_Affine.Word)
      return Compositor_Affine.Word
     with Import, Convention => C, External_Name => "cubit_vulkan_record_affine";
   procedure Record_Draw
     (Borrowed : System.Address; D : Compositor_Affine.Draw;
      C : Compositor_Transform.Coefficients;
      Width, Height : Compositor_Affine.G.Physical_Extent; Mask : Boolean;
      Tint : Compositor_Affine.Word; Accepted : out Boolean) is
      Draw : aliased constant Compositor_Affine.Draw := D;
      Coefficients : aliased constant Compositor_Transform.Coefficients := C;
   begin
      Accepted := Submit (Borrowed, Draw'Access, Coefficients'Access,
                          Compositor_Affine.Word (Width), Compositor_Affine.Word (Height),
                          (if Mask then 1 else 0), Tint) = 0;
   end Record_Draw;
end Vulkan_Affine_FFI;
