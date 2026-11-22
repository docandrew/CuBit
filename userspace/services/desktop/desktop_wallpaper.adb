with Desktop_Backdrop_Style;
with Desktop_Wallpaper_Store;
with Compositor_Backdrop_Strips;
with Compositor_Image_Sampling;
with Compositor_Sampling;
with Compositor_Text;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Desktop_Wallpaper is
   package Style_Of renames Desktop_Backdrop_Style;
   subtype Preferences is Style_Of.Backdrop;
   package Store renames Desktop_Wallpaper_Store;
   type Pixels is array (Natural range <>) of Unsigned_32
     with Convention => C;
   --  A decoded raster is only drawn once it is Ready and is the size the
   --  style was resolved with; until then (or if its file is unavailable)
   --  the backdrop is its flat colour.
   function Image_Ready (Style : Preferences) return Boolean is
     (Style_Of.Has_Image (Style) and then Store.Ready (Style.Image) and then
      Store.Width (Style.Image) = Style.Width and then Store.Height (Style.Image) = Style.Height);
   --  The address of Style's raster, or a null address when it has none.
   function Raster (Style : Preferences) return System.Address is
     (if Image_Ready (Style) then Store.Pixels (Style.Image)
      else System.Null_Address);
   --  The sampled image's size (1 x 1 when there is none: never read).
   function Image_Width (Style : Preferences) return Positive is
     (if Image_Ready (Style) then Style.Width else 1);
   function Image_Height (Style : Preferences) return Positive is
     (if Image_Ready (Style) then Style.Height else 1);

   --  Integer bilinear filtering of the three colour channels:
   --  (A * (256 - Fraction) + B * Fraction + 128) / 256 each, alpha opaque.
   --  Red and blue share one multiply in 16-bit lanes (each lane's sum is at
   --  most 255 * 256 + 128 < 2**16, so no carry crosses lanes), green takes
   --  another: the same pixels as a per-channel loop, two multiplies fewer.
   function Blend (A, B : Unsigned_32; Fraction : Natural)
     return Unsigned_32
   is
      F : constant Unsigned_32 := Unsigned_32 (Fraction);
      G : constant Unsigned_32 := 256 - F;
      Red_Blue : constant Unsigned_32 :=
        (A and 16#00FF_00FF#) * G + (B and 16#00FF_00FF#) * F + 16#0080_0080#;
      Green : constant Unsigned_32 :=
        (A and 16#0000_FF00#) * G + (B and 16#0000_FF00#) * F + 16#0000_8000#;
   begin
      return 16#FF00_0000# or (Shift_Right (Red_Blue, 8) and 16#00FF_00FF#) or
        (Shift_Right (Green, 8) and 16#0000_FF00#);
   end Blend;
   pragma Inline (Blend);

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
      Style : Desktop_Backdrop_Style.Backdrop := Desktop_Backdrop_Style.Flat)
   is
      package S renames Compositor_Image_Sampling;
      package Strips renames Compositor_Backdrop_Strips;
      Source_Width : constant S.Extent := Image_Width (Style);
      Source_Height : constant S.Extent := Image_Height (Style);
      Has_Image : constant Boolean := Image_Ready (Style);
      Background : constant Unsigned_32 :=
        Desktop_Backdrop_Style.Color (Style);
      Source : constant Pixels (0 .. Source_Width * Source_Height - 1)
        with Import, Address => Raster (Style);
      Plan : S.Layout;
      First : Natural := X;
      function Sample (X, Y : S.Index) return Unsigned_32 is
        (Source (Y * Source_Width + X)) with Inline;
   begin
      -- Output extents and clipping belong to the validated display geometry.
      -- Reject invalid callers before computing offsets or touching memory.
      if Width > S.Extent'Last or else Height > S.Extent'Last or else
        Pitch / 4 < Width or else Pitch > Natural'Last / Height or else
        X >= Width or else Y >= Height or else W = 0 or else H = 0 or else
        W > Width - X or else H > Height - Y
      then return; end if;
      Plan := S.Prepare (S.Extent (Width), S.Extent (Height), Source_Width, Source_Height,
        Desktop_Backdrop_Style.Placement (Style.Position));
      --  Strip by strip (Strips.Width columns), row by row. A source row's
      --  horizontal pass is kept (two rows, Row_Pass) and reused while the
      --  output rows below still sample it, so each pixel costs one
      --  vertical blend and, amortized, at most one horizontal one: the
      --  pixels are the same as blending all four neighbours per pixel.
      while First < X + W loop
         declare
            Length : constant Strips.Count := Natural'Min (Strips.Width, X + W - First);
            Columns : constant Strips.Samples :=
              (if Has_Image then Strips.Prepare (Plan, S.Index (First), Length)
               else (others => (Valid => False)));
            type Row_Pass is array (Strips.Slot) of Unsigned_32;
            Passes : array (0 .. 1) of Row_Pass := (others => (others => 0));
            --  The source row each pass holds (none yet: -1).
            Held : array (0 .. 1) of Integer := (-1, -1);
            AY : S.Axis_Sample;
            --  The pass holding source row R, computed into the slot that
            --  Keep does not hold when it is missing.
            function Pass_For (R : S.Index; Keep : Integer) return Natural is
               Slot : Natural;
            begin
               if Held (0) = R then return 0; elsif Held (1) = R then return 1; end if;
               Slot := (if Held (0) = Keep then 1 else 0);
               for I in 0 .. Length - 1 loop
                  declare
                     AX : S.Axis_Sample renames Columns (I);
                  begin
                     if AX.Valid then
                        Passes (Slot) (I) := Blend (Sample (AX.First, R), Sample (AX.Last, R), AX.Weight);
                     end if;
                  end;
               end loop;
               Held (Slot) := R;
               return Slot;
            end Pass_For;
         begin
            for Row in Y .. Y + H - 1 loop
               AY := (if Has_Image then S.Vertical (Plan, S.Position (Row * 256))
                      else (Valid => False));
               if AY.Valid then
                  declare
                     Top : constant Natural := Pass_For (AY.First, AY.Last);
                     Bottom : constant Natural := Pass_For (AY.Last, AY.First);
                     --  SEC-025: own: the caller's private target (a wallpaper layer or the frame
                     --  Desktop draws); no peer writes it.
                     Target_Row : Pixels (0 .. Length - 1) with Import, Address => Target +
                       Storage_Offset (Row * Pitch + First * 4);
                  begin
                     for I in 0 .. Length - 1 loop
                        Target_Row (I) :=
                          (if Columns (I).Valid
                           then Blend (Passes (Top) (I), Passes (Bottom) (I), AY.Weight)
                           else Background);
                     end loop;
                  end;
               else
                  declare
                     --  SEC-025: own: the caller's private target (a wallpaper layer or the frame
                     --  Desktop draws); no peer writes it.
                     Target_Row : Pixels (0 .. Length - 1) with Import, Address => Target +
                       Storage_Offset (Row * Pitch + First * 4);
                  begin
                     Target_Row := (others => Background);
                  end;
               end if;
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
      Style : Desktop_Backdrop_Style.Backdrop := Desktop_Backdrop_Style.Flat)
   is
      package G renames CuBit.Display_Geometry;
      package S renames Compositor_Image_Sampling;
      use type G.Pixel_Edge, S.Wide;
      W : constant S.Wide := S.Wide (Bounds.Right) - S.Wide (Bounds.Left);
      H : constant S.Wide := S.Wide (Bounds.Bottom) - S.Wide (Bounds.Top);
      Area : constant G.Physical_Rectangle := Compositor_Text.Clip (Screen, Bounds, Damage);
      IW : constant S.Extent := Image_Width (Style);
      IH : constant S.Extent := Image_Height (Style);
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
