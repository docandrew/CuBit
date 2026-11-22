with Compositor_Glyph_Layout;
with Compositor_Affine;
with Compositor_Mask_Batch;
with Mesa_Cache;
package Mesa_Binding.Masks with SPARK_Mode is
   procedure Render_Batch
     (Library : in out Context; Target : System.Address; Sources : Mesa_Cache.Handle_Batch;
      Packet : Compositor_Mask_Batch.Packet; Result : out Compositor_Policy.Completion)
     with Global => null;
   procedure Import_View
     (Library : in out Context; Description : Compositor_Formats.Image;
      Layout : Compositor_Glyph_Layout.Layout; View : out System.Address)
     with Global => null;
   procedure Render
     (Library : in out Context; Target, Source : System.Address;
      Description : Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Compositor_Formats.Word; Result : out Compositor_Policy.Completion)
     with Global => null;
end Mesa_Binding.Masks;
