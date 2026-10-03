package body Vulkan_Region_FFI with SPARK_Mode => Off is
   use type Compositor_Affine.Word;
   function Submit (Borrowed : System.Address; D : access constant Compositor_Affine.Draw;
      C : access constant Compositor_Transform.Coefficients;
      Width, Height, Mask, Tint : Compositor_Affine.Word;
      Region : access constant Compositor_Source_Region.Rectangle) return Compositor_Affine.Word
     with Import, Convention => C, External_Name => "cubit_vulkan_record_affine_region";
   procedure Record_Draw (Borrowed : System.Address; D : Compositor_Affine.Draw;
      C : Compositor_Transform.Coefficients;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Region : Compositor_Source_Region.Rectangle; Accepted : out Boolean) is
      Draw : aliased constant Compositor_Affine.Draw := D;
      Coefficients : aliased constant Compositor_Transform.Coefficients := C;
      Window : aliased constant Compositor_Source_Region.Rectangle := Region;
   begin
      Accepted := Submit (Borrowed, Draw'Access, Coefficients'Access,
        Compositor_Affine.Word (Width), Compositor_Affine.Word (Height), 0, 0, Window'Access) = 0;
   end Record_Draw;
end Vulkan_Region_FFI;
