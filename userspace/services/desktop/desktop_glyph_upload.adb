with Compositor_Glyph_FFI;
with Compositor_Glyph_Layout;
with Compositor_Upload;
with Interfaces;
with System;
package body Desktop_Glyph_Upload with SPARK_Mode is
   package L renames Compositor_Glyph_Layout;
   package G renames Compositor_Upload;
   use type D.Source_Result, G.Pixel_Format;
   procedure Start (Index : D.Backing_Slot; Lease : Vulkan_Owned_Targets.A.Ticket;
      Key : Vulkan_Glyph_Sources.Key; Advance : out Natural;
      Result : out D.Source_Result) is
      Layout : constant L.Layout := L.Plan (Key.Scale);
      Plan : G.Plan;
      Write : D.Write_Ticket;
      Mapping : System.Address;
      Complete, Cancelled : Boolean;
   begin
      Advance := 0;
      D.Begin_Write (Index, Lease, Write, Plan, Mapping, Result,
         Row_Pixels => G.Edge (Layout.Pitch));
      if Result /= D.Source_Accepted then return; end if;
      if not G.Valid (Plan) or else G.Format (Plan) /= G.R8 or else
         G.Image_Width (Plan) /= Layout.Width or else G.Image_Height (Plan) /= Layout.Height or else
         G.Area (Plan).X /= 0 or else G.Area (Plan).Y /= 0 or else
         G.Area (Plan).Height /= Layout.Height or else G.Buffer_Offset (Plan) /= 0 or else
         G.Row_Length (Plan) /= Layout.Pitch or else
         G.Capacity (Plan) < Layout.Bytes
      then
         D.Cancel_Write (Write, True, Cancelled);
         Result := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         return;
      end if;
      Compositor_Glyph_FFI.Rasterize (Interfaces.Unsigned_32 (Key.Face),
         Interfaces.Unsigned_32 (Key.Code), Layout, Mapping,
         Interfaces.Unsigned_64 (G.Capacity (Plan)), Advance, Complete);
      if not Complete then
         D.Cancel_Write (Write, True, Cancelled);
         Result := (if Cancelled then D.Source_Rejected else D.Source_Unsafe);
         return;
      end if;
      D.Submit_Write (Write, True, Result);
   end Start;
end Desktop_Glyph_Upload;
