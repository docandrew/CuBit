with Mesa_Mask_FFI;
package body Mesa_Binding.Masks with SPARK_Mode => Off is
   procedure Render_Batch
     (Library : in out Context; Target : System.Address; Sources : Mesa_Cache.Handle_Batch;
      Packet : Compositor_Mask_Batch.Packet; Result : out Compositor_Policy.Completion) is
      Handles : Mesa_Mask_FFI.Source_Array;
      Code : Compositor_Formats.Word;
   begin
      for I in Handles'Range loop Handles (I) := Sources (I); end loop;
      Code := Mesa_Mask_FFI.Render_Batch (Library.Pointer, Target, Handles, Packet);
      Result := (case Code is
        when 0 => Compositor_Policy.Rendered,
        when 1 => Compositor_Policy.Rejected,
        when 2 => Compositor_Policy.Failed_Quiescent,
        when others => Compositor_Policy.Access_Unknown);
   end Render_Batch;
   procedure Import_View
     (Library : in out Context; Description : Compositor_Formats.Image;
      Layout : Compositor_Glyph_Layout.Layout; View : out System.Address) is
      use type Compositor_Formats.Byte_Count;
   begin
      View := Mesa_Mask_FFI.Import_Mask
        (Library.Pointer, Description.Pixels, Layout,
         Compositor_Formats.Byte_Count (Description.Pitch) * Compositor_Formats.Byte_Count (Description.Height));
   end Import_View;
   procedure Render
     (Library : in out Context; Target, Source : System.Address;
      Description : Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Compositor_Formats.Word; Result : out Compositor_Policy.Completion) is
      D : aliased constant Compositor_Affine.Draw := Description;
      Code : Compositor_Formats.Word;
   begin
      Code := Mesa_Mask_FFI.Render (Library.Pointer, Target, Source, D'Access, Width, Height, Tint);
      Result := (case Code is
        when 0 => Compositor_Policy.Rendered,
        when 1 => Compositor_Policy.Rejected,
        when 2 => Compositor_Policy.Failed_Quiescent,
        when others => Compositor_Policy.Access_Unknown);
   end Render;
end Mesa_Binding.Masks;
