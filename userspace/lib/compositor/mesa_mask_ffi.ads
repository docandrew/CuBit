with System;
with Interfaces;
with Compositor_Glyph_Layout;
with Compositor_Affine;
with Compositor_Mask_Batch;
--  Trusted retained-storage interface. Caller owns the mapped mask, keeps it
--  alive until Mesa_FFI.Release confirms retirement, and serializes all access.
--  Numeric validation does not prove mapping authority or physical aliasing.
package Mesa_Mask_FFI with SPARK_Mode => Off is
   type Source_Array is array (Compositor_Mask_Batch.Index) of System.Address;
   function Render_Batch
     (Context, Target : System.Address; Sources : Source_Array;
      Packet : Compositor_Mask_Batch.Packet) return Interfaces.Unsigned_32;
   function Import_Mask
     (Context, Pixels : System.Address; Layout : Compositor_Glyph_Layout.Layout;
      Capacity : Interfaces.Unsigned_64) return System.Address;
   --  Tint is straight-alpha AARRGGBB. Only source-over composition is accepted.
   --  Completion codes and release use the existing Mesa_FFI contract.
   function Render
     (Context, Target, Mask : System.Address;
      Value : access constant Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
end Mesa_Mask_FFI;
