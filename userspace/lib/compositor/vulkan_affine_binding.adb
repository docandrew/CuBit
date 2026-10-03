with Compositor_Glyph_Placement;
with Compositor_Text;
with Compositor_Transform;
with Vulkan_Affine_FFI;
package body Vulkan_Affine_Binding with SPARK_Mode is
   procedure Draw_Output
     (Borrowed : System.Address; Screen : G.Output;
      Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Over, Mask : Boolean; Tint : A.Word; Result : out Outcome;
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False) is
      Full : constant A.Result :=
        (if Raster_Glyph then Compositor_Glyph_Placement.Plan (Screen, (Surface.Left, Surface.Top))
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
            Vulkan_Affine_FFI.Record_Draw
              (Borrowed, P.Value, C, Screen.Width, Screen.Height, Mask, Tint, Accepted);
            Result := (if Accepted then Recorded else Rejected);
         end;
      end if;
   end Draw_Output;
end Vulkan_Affine_Binding;
