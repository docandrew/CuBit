with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Store;
with Compositor_Backdrop_Strips;
with Compositor_Image_Sampling;
with Compositor_Sampling;
with Compositor_Text;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Desktop_Wallpaper is
   use CuBit.Appearance;
   package Store renames Desktop_Wallpaper_Store;
   type Pixels is array (Natural range <>) of Unsigned_32
     with Convention => C;
   --  A decoded raster is only drawn once it is Ready; until then (or if its
   --  file is unavailable) the backdrop is its flat theme colour.
   function Image_Ready (Style : Preferences) return Boolean is
     (Desktop_Backdrop_Style.Has_Image (Style.Backdrop) and then
      Store.Ready (Style.Backdrop));
   --  The address of Style's raster, or a null address when it has none.
   function Raster (Style : Preferences) return System.Address is
     (if Image_Ready (Style) then Store.Pixels (Style.Backdrop)
      else System.Null_Address);

   function Blend (A, B : Unsigned_32; Fraction : Natural)
     return Unsigned_32
   is
      Result : Unsigned_32 := 16#FF00_0000#;
      Channel : Unsigned_32;
   begin
      --  Integer bilinear filtering; only exposed damage is resampled.
      for Component in 0 .. 2 loop
         Channel :=
           ((Shift_Right (A, Component * 8) and 255) *
              Unsigned_32 (256 - Fraction) +
            (Shift_Right (B, Component * 8) and 255) *
              Unsigned_32 (Fraction) + 128) / 256;
         Result := Result or Shift_Left (Channel, Component * 8);
      end loop;
      return Result;
   end Blend;

   procedure Render
     (Target : System.Address;
      Width, Height, Pitch : Positive)
   is
   begin
      Paint (Target, Width, Height, Pitch, 0, 0, Width, Height);
   end Render;

   procedure Paint
     (Target : System.Address;
      Width, Height, Pitch : Positive;
      X, Y, W, H : Natural;
      Style : Preferences := Default)
   is
      package S renames Compositor_Image_Sampling;
      package Strips renames Compositor_Backdrop_Strips;
      Image_Width : constant S.Extent :=
        Desktop_Backdrop_Style.Width (Style.Backdrop);
      Image_Height : constant S.Extent :=
        Desktop_Backdrop_Style.Height (Style.Backdrop);
      Has_Image : constant Boolean := Image_Ready (Style);
      Background : constant Unsigned_32 :=
        Desktop_Backdrop_Style.Color (Style);
      Source : constant Pixels (0 .. Image_Width * Image_Height - 1)
        with Import, Address => Raster (Style);
      Plan : S.Layout;
      First : Natural := X;
      function Sample (X, Y : S.Index) return Unsigned_32 is
        (Source (Y * Image_Width + X)) with Inline;
   begin
      -- Output extents and clipping belong to the validated display geometry.
      -- Reject invalid callers before computing offsets or touching memory.
      if Width > S.Extent'Last or else Height > S.Extent'Last or else
        Pitch / 4 < Width or else Pitch > Natural'Last / Height or else
        X >= Width or else Y >= Height or else W = 0 or else H = 0 or else
        W > Width - X or else H > Height - Y
      then return; end if;
      Plan := S.Prepare (S.Extent (Width), S.Extent (Height), Image_Width, Image_Height,
        Desktop_Backdrop_Style.Placement (Style.Position));
      while First < X + W loop
         declare
            Length : constant Strips.Count := Natural'Min (Strips.Width, X + W - First);
            Columns : constant Strips.Samples :=
              (if Has_Image then Strips.Prepare (Plan, S.Index (First), Length)
               else (others => (Valid => False)));
            AY : S.Axis_Sample;
         begin
            for Row in Y .. Y + H - 1 loop
               AY := (if Has_Image then S.Vertical (Plan, S.Position (Row * 256))
                      else (Valid => False));
               for I in 0 .. Length - 1 loop
                  declare
                     AX : S.Axis_Sample renames Columns (I);
                     Color : Unsigned_32 := Background;
                     Pixel : Unsigned_32 with Import, Address => Target +
                       Storage_Offset (Row * Pitch + (First + I) * 4);
                  begin
                     if AX.Valid and then AY.Valid then
                        Color := Blend
                          (Blend (Sample (AX.First, AY.First), Sample (AX.Last, AY.First), AX.Weight),
                           Blend (Sample (AX.First, AY.Last), Sample (AX.Last, AY.Last), AX.Weight), AY.Weight);
                     end if;
                     Pixel := Color;
                  end;
               end loop;
            end loop;
            First := First + Length;
         end;
      end loop;
   end Paint;
   procedure Paint_Output
     (Target : System.Address; Pitch : Positive;
      Screen : CuBit.Display_Geometry.Output;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : Preferences := Default)
   is
      package G renames CuBit.Display_Geometry;
      package S renames Compositor_Image_Sampling;
      use type G.Pixel_Edge, S.Wide;
      W : constant S.Wide := S.Wide (Bounds.Right) - S.Wide (Bounds.Left);
      H : constant S.Wide := S.Wide (Bounds.Bottom) - S.Wide (Bounds.Top);
      Area : constant G.Physical_Rectangle := Compositor_Text.Clip (Screen, Bounds, Damage);
      IW : constant S.Extent := Desktop_Backdrop_Style.Width (Style.Backdrop);
      IH : constant S.Extent := Desktop_Backdrop_Style.Height (Style.Backdrop);
      Background : constant Unsigned_32 :=
        Desktop_Backdrop_Style.Color (Style);
      Has_Image : constant Boolean := Image_Ready (Style);
      Source : constant Pixels (0 .. IW * IH - 1)
        with Import, Address => Raster (Style);
      Plan : S.Layout;
      Point : Compositor_Sampling.Fine_Sample;
      Q : S.Sample;
      Color : Unsigned_32;
      function Read_Source (X, Y : S.Index) return Unsigned_32 is
        (Source (Y * IW + X));
   begin
      if W not in 1 .. S.Wide (S.Extent'Last) or else
        H not in 1 .. S.Wide (S.Extent'Last) or else
        Area.Left >= Area.Right or else Area.Top >= Area.Bottom
      then return; end if;
      Plan := S.Prepare (S.Extent (W), S.Extent (H), IW, IH,
        Desktop_Backdrop_Style.Placement (Style.Position));
      for Y in Area.Top .. Area.Bottom - 1 loop
         for X in Area.Left .. Area.Right - 1 loop
            Point := Compositor_Sampling.Fine_Map (Screen, (X, Y), Bounds,
              Compositor_Sampling.Fine_Extent (W * 256), Compositor_Sampling.Fine_Extent (H * 256));
            if Point.Valid then
               Color := Background;
               if Has_Image then
                  Q := S.At_Point (Plan, S.From_Centre (Point.X, S.Extent (W)),
                                        S.From_Centre (Point.Y, S.Extent (H)));
                  if Q.Valid then
                     Color := Blend
                       (Blend (Read_Source (Q.X0, Q.Y0), Read_Source (Q.X1, Q.Y0), Q.FX),
                        Blend (Read_Source (Q.X0, Q.Y1), Read_Source (Q.X1, Q.Y1), Q.FX), Q.FY);
                  end if;
               end if;
               declare
                  Pixel : Unsigned_32 with Import, Address => Target +
                    Storage_Offset (Natural (Y) * Pitch + Natural (X) * 4);
               begin Pixel := Color; end;
            end if;
         end loop;
      end loop;
   end Paint_Output;
end Desktop_Wallpaper;
