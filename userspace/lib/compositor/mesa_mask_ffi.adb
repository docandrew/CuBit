with Mesa_FFI;
with Compositor_Transform;
package body Mesa_Mask_FFI with SPARK_Mode => Off is
   use type System.Address, Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   type Native_Command is record
      Source : System.Address;
      Description : Compositor_Affine.Draw;
      Geometry : Compositor_Transform.Quad;
      Tint : Interfaces.Unsigned_32;
   end record with Convention => C, Size => 160 * 8;
   for Native_Command use record
      Source at 0 range 0 .. 63;
      Description at 8 range 0 .. 447;
      Geometry at 64 range 0 .. 703;
      Tint at 152 range 0 .. 31;
   end record;
   type Native_Array is array (Compositor_Mask_Batch.Index) of Native_Command with Convention => C;
   function Submit_Batch
     (Context, Target : System.Address; Values : access constant Native_Array;
      Count : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_mesa_draw_mask_batch";
   function Render_Batch
     (Context, Target : System.Address; Sources : Source_Array;
      Packet : Compositor_Mask_Batch.Packet) return Interfaces.Unsigned_32 is
      Values : aliased Native_Array := (others =>
        (Source => System.Null_Address, Description => (others => <>), Tint => 0,
         Geometry => (Corners => (others => (0, 0)), UD => 1, VD => 1, Width => 1, Height => 1)));
   begin
      if not Compositor_Mask_Batch.Valid (Packet) then return 1; end if;
      if Packet.Length = 0 then return 0; end if;
      pragma Assert (Native_Array'Component_Size = 160 * 8);
      for I in 1 .. Packet.Length loop
         Values (I) := (Sources (I), Packet.Items (I).Description,
           Compositor_Transform.Vertices (Packet.Items (I).Description, Packet.Width, Packet.Height), Packet.Items (I).Tint);
      end loop;
      return Submit_Batch (Context, Target, Values'Access, Interfaces.Unsigned_32 (Packet.Length));
   end Render_Batch;
   function Native_Import
     (Context : System.Address; Image : access constant Mesa_FFI.Image;
      Capacity : Interfaces.Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "cubit_mesa_import_mask";
   function Submit
     (Context, Target, Mask : System.Address;
      Value : access constant Compositor_Affine.Draw;
      Corners : access constant Compositor_Transform.Quad;
      Tint : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
     with Import, Convention => C, External_Name => "cubit_mesa_draw_mask";
   function Import_Mask
     (Context, Pixels : System.Address; Layout : Compositor_Glyph_Layout.Layout;
      Capacity : Interfaces.Unsigned_64) return System.Address is
      Image : aliased constant Mesa_FFI.Image :=
        (Pixels, Interfaces.Unsigned_32 (Layout.Width), Interfaces.Unsigned_32 (Layout.Height),
         Interfaces.Unsigned_32 (Layout.Pitch), 0);
   begin
      if not Compositor_Glyph_Layout.Valid (Layout) or else
        Pixels = System.Null_Address or else Capacity < Interfaces.Unsigned_64 (Layout.Bytes)
      then return System.Null_Address; end if;
      return Native_Import (Context, Image'Access, Capacity);
   end Import_Mask;
   function Render
     (Context, Target, Mask : System.Address;
      Value : access constant Compositor_Affine.Draw;
      Width, Height : Compositor_Affine.G.Physical_Extent;
      Tint : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
   begin
      if Value = null or else Value.Over /= 1 or else
        not Compositor_Affine.Valid (Value.all, Width, Height)
      then return 1; end if;
      declare
         Corners : aliased constant Compositor_Transform.Quad :=
           Compositor_Transform.Vertices (Value.all, Width, Height);
      begin
         return Submit (Context, Target, Mask, Value, Corners'Access, Tint);
      end;
   end Render;
end Mesa_Mask_FFI;
