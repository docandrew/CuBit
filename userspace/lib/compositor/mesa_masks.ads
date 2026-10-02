with System;
with Mesa_Cache;
with Compositor_Formats;
with Compositor_Glyph_Layout;
with Compositor_Affine;
with Compositor_Mask_Batch;
-- Checked mask operations use the existing context-owned view table. Forget,
-- target retirement and Shutdown retain their existing failure policy.
package Mesa_Masks with SPARK_Mode is
   procedure Render_Batch
     (S : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot;
      Packet : Compositor_Mask_Batch.Packet; Success : out Boolean)
     with Pre => Mesa_Cache.Can_Retire (S),
       Post => (if Success then Mesa_Cache.Can_Retire (S));
   procedure Ensure
     (S : in out Mesa_Cache.State; Index : Mesa_Cache.Mask_Slot;
      Pixels : System.Address; Layout : Compositor_Glyph_Layout.Layout;
      Capacity : Compositor_Formats.Byte_Count; Success : out Boolean)
     with Pre => Mesa_Cache.Can_Retire (S),
       Post => (if Success then Mesa_Cache.Can_Retire (S) and not Mesa_Cache.Empty (S, Index));
   procedure Render
     (S : in out Mesa_Cache.State; Target : Mesa_Cache.Target_Slot; Source : Mesa_Cache.Mask_Slot;
      Description : Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Compositor_Formats.Word; Success : out Boolean)
     with Pre => Mesa_Cache.Can_Retire (S),
       Post => (if Success then Mesa_Cache.Can_Retire (S));
end Mesa_Masks;
