with Compositor_Glyph_Placement;
with Compositor_Source_Region;
with Compositor_Text;
with Compositor_Transform;
with Vulkan_Affine_FFI;
with Vulkan_Region_FFI;
package body Vulkan_Affine_Binding with SPARK_Mode is
   package L renames Compositor_Glyph_Layout;
   use type A.Word;
   --  Phase P's raster of a stacked glyph mask, as a source window: all of
   --  its columns, rows P * Height .. P * Height + Height - 1. In Word
   --  arithmetic, phase by phase (linear).
   function Phase_Rows (Raster : L.Layout; Phase : L.Phase) return Compositor_Source_Region.Rectangle
     with Post => Compositor_Source_Region.Valid (Phase_Rows'Result);
   function Phase_Rows (Raster : L.Layout; Phase : L.Phase) return Compositor_Source_Region.Rectangle is
      W : constant A.Word := A.Word (Raster.Width);
      H : constant A.Word := A.Word (Raster.Height);
      Y : constant A.Word := (case Phase is when 0 => 0, when 1 => H, when 2 => 2 * H, when 3 => 3 * H);
   begin
      pragma Assert (W in 1 .. L.Last_Width and H in 1 .. L.Last_Height);
      return (X => 0, Y => Y, Width => W, Height => H, Image_Width => W, Image_Height => L.Phases * H);
   end Phase_Rows;
   procedure Draw_Output
     (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : A.Word; Result : out Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False;
      Glyph_Offset : Compositor_Glyph_Layout.Subpixel := 0) is
      Full : constant A.Result :=
        (if Raster_Glyph then Compositor_Glyph_Placement.Plan (Screen, (Surface.Left, Surface.Top), Glyph_Offset)
         else A.Plan (Screen, Surface, Over or Mask, Straight_Alpha));
      Area : constant G.Physical_Rectangle :=
        (if Raster_Glyph then Compositor_Text.Clip (Screen, Surface, Damage) else Damage);
      P : constant A.Result :=
        (if Full.Visible then A.Clip (Full.Value, Screen.Width, Screen.Height, Area)
         else (Visible => False));
      Accepted : Boolean;
   begin
      if (Straight_Alpha and then (not Over or Mask or Raster_Glyph)) or else
        (Raster_Glyph and then (not Over or not Mask)) then
         Result := Rejected;
      elsif not P.Visible then
         Result := Empty;
      else
         declare
            C : constant Compositor_Transform.Coefficients :=
              Compositor_Transform.Build (P.Value, Screen.Width, Screen.Height);
         begin
            if Raster_Glyph then
               declare
                  Phase : constant L.Phase := Compositor_Glyph_Placement.Snap_Phase
                    (Surface.Left, Glyph_Offset, Screen.X, Screen.Scale).Phase;
                  Rows : constant Compositor_Source_Region.Rectangle := Phase_Rows (L.Plan (Screen.Scale), Phase);
               begin
                  Vulkan_Region_FFI.Record_Draw
                    (Borrowed, P.Value, C, Screen.Width, Screen.Height, Rows, True, Tint, Accepted);
               end;
            else
               Vulkan_Affine_FFI.Record_Draw
                 (Borrowed, P.Value, C, Screen.Width, Screen.Height, Mask, Tint, Accepted);
            end if;
            Result := (if Accepted then Recorded else Rejected);
         end;
      end if;
   end Draw_Output;
end Vulkan_Affine_Binding;
