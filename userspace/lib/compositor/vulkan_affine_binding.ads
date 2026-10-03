with System;
with Compositor_Affine;
package Vulkan_Affine_Binding with SPARK_Mode is
   package A renames Compositor_Affine;
   package G renames A.G;
   type Outcome is (Empty, Recorded, Rejected);
   -- Exclusively borrowed command state inside a compatible active render pass.
   -- Recorded is not queue completion or authority to reuse referenced storage.
   -- Raster_Glyph uses an output-density raster at unit scale, snapped from
   -- Surface.Left/Top and clipped to the cell. Its retained R8 source must have
   -- the dimensions/density from Compositor_Glyph_Layout.Plan (Screen.Scale).
   -- This flag neither rasterizes nor imports a font image. It requires Over/Mask.
   procedure Draw_Output
     (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : A.Word; Result : out Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False)
     with Global => null;
end Vulkan_Affine_Binding;
