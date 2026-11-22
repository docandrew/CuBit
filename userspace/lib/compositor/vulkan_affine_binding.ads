with System;
with Compositor_Affine;
with Compositor_Glyph_Layout;
package Vulkan_Affine_Binding with SPARK_Mode is
   package A renames Compositor_Affine;
   package G renames A.G;
   type Outcome is (Empty, Recorded, Rejected);
   -- Exclusively borrowed command state inside a compatible active render pass.
   -- Recorded is not queue completion or authority to reuse referenced storage.
   -- Raster_Glyph uses an output-density raster at unit scale, placed from
   -- Surface.Left plus Glyph_Offset (quarter-pixel phase) and Surface.Top and
   -- clipped to the cell. Its retained R8 source must be the stacked phase
   -- mask of Compositor_Glyph_Layout.Plan (Screen.Scale) (Strip_Height rows);
   -- the draw samples the placement's phase rows only. This flag neither
   -- rasterizes nor imports a font image. It requires Over/Mask.
   procedure Draw_Output
     (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : A.Word; Result : out Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False;
      Glyph_Offset : Compositor_Glyph_Layout.Subpixel := 0)
     with Global => null;
end Vulkan_Affine_Binding;
