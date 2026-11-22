with Mesa_Binding.Masks;
with Compositor_Policy;
package body Mesa_Masks with SPARK_Mode is
   use Compositor_Formats;
   use type Word, Byte_Count;
   use type System.Address;
   use type Mesa_Cache.Slot;
   procedure Render_Batch
     (S : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot;
      Packet : Compositor_Mask_Batch.Packet; Success : out Boolean) is
      Sources : Mesa_Cache.Mask_Indices := (others => Mesa_Cache.Mask_Slot'First);
      function Fits_Source (I : Mesa_Cache.Batch_Index; Source, Target : Image) return Boolean is
        (I <= Packet.Length and then Source.Writable = 0 and then Target.Writable = 1 and then
         Source.Pixels /= Target.Pixels and then Target.Width = Word (Packet.Width) and then
         Target.Height = Word (Packet.Height) and then Packet.Items (I).Description.Over = 1 and then
         Compositor_Affine.Valid (Packet.Items (I).Description, Packet.Width, Packet.Height));
      procedure Draw_Batch
        (Library : in out Mesa_Binding.Context; Target : System.Address; Sources : Mesa_Cache.Handle_Batch;
         Length : Mesa_Cache.Batch_Count; Result : out Compositor_Policy.Completion) is
      begin
         if Length /= Packet.Length then Result := Compositor_Policy.Rejected; return; end if;
         Mesa_Binding.Masks.Render_Batch (Library, Target, Sources, Packet, Result);
      end Draw_Batch;
      procedure Execute is new Mesa_Cache.Render_Masks (Fits_Source, Draw_Batch);
   begin
      Success := False;
      if not Compositor_Mask_Batch.Valid (Packet) then return; end if;
      for I in 1 .. Packet.Length loop
         Sources (I) := Mesa_Cache.Mask_Slot'First + Mesa_Cache.Slot (Packet.Items (I).Mask);
      end loop;
      Execute (S, Target, Sources, Packet.Length, Success);
   end Render_Batch;
   procedure Ensure
     (S : in out Mesa_Cache.State; Index : Mesa_Cache.Mask_Slot;
      Pixels : System.Address; Layout : Compositor_Glyph_Layout.Layout;
      Capacity : Byte_Count; Success : out Boolean) is
      D : constant Image := (Pixels, Word (Layout.Width), Word (Layout.Height), Word (Layout.Pitch), 0);
      function Valid_Mask (Description : Image; Capacity : Byte_Count) return Boolean is
        (Compositor_Glyph_Layout.Valid (Layout) and then Description.Pixels /= System.Null_Address and then
         Description.Width = Word (Layout.Width) and then Description.Height = Word (Layout.Height) and then
         Description.Pitch = Word (Layout.Pitch) and then Description.Writable = 0 and then Capacity >= Byte_Count (Layout.Bytes));
      procedure Import_Mask (Library : in out Mesa_Binding.Context; Description : Image; View : out System.Address) is
      begin
         Mesa_Binding.Masks.Import_View (Library, Description, Layout, View);
      end Import_Mask;
      procedure Execute is new Mesa_Cache.Ensure_Mask (Valid_Mask, Import_Mask);
   begin
      Execute (S, Index, D, Capacity, Success);
   end Ensure;
   procedure Render
     (S : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot; Source : Mesa_Cache.Mask_Slot;
      Description : Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Word; Success : out Boolean) is
      function Fits_Views (Source, Target : Image) return Boolean is
        (Source.Writable = 0 and Target.Writable = 1 and Source.Pixels /= Target.Pixels and
         Target.Width = Word (Width) and Target.Height = Word (Height) and
         Description.Over = 1 and Compositor_Affine.Valid (Description, Width, Height));
      procedure Draw_Views (Library : in out Mesa_Binding.Context; Target, Source : System.Address;
                            Result : out Compositor_Policy.Completion) is
      begin
         Mesa_Binding.Masks.Render (Library, Target, Source, Description, Width, Height, Tint, Result);
      end Draw_Views;
      procedure Execute is new Mesa_Cache.Render_Checked (Fits_Views, Draw_Views);
   begin
      Execute (S, Target, Source, Success);
   end Render;
end Mesa_Masks;
