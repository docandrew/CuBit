with Client_Raster;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Font8x16;
with CuBit.Fonts;
with Icon_Edge_Choice;
with Glyph_Pen;
with Client_Glyphs;
with Client_Glyph_Blend;
with Compositor_Glyph_Placement;
with Text_Fit;

package body CuBit.UI is
   Selected_Theme : Theme := CuBit_Alloy;
   Palettes : array (CuBit.Appearance.Color_Scheme) of Theme :=
     [CuBit_Alloy, CuBit_Alloy_Dark];
   function Palette (Scheme : CuBit.Appearance.Color_Scheme) return Theme is
     (Palettes (Scheme));
   procedure Install_Palette (Scheme : CuBit.Appearance.Color_Scheme; Value : Theme) is
   begin
      Palettes (Scheme) := Value;
   end Install_Palette;
   procedure Set_Color_Scheme (Scheme : CuBit.Appearance.Color_Scheme) is
   begin
      Selected_Theme := Palette (Scheme);
   end Set_Color_Scheme;
   procedure Set_Theme (Value : Theme) is
   begin
      Selected_Theme := Value;
   end Set_Theme;
   function Current_Theme return Theme is (Selected_Theme);
   use type System.Address;

   function Is_Empty (r : Rect) return Boolean is
   begin
      return r.w = 0 or else r.h = 0;
   end Is_Empty;

   function Point_In_Rect (x, y : Natural; r : Rect) return Boolean is
   begin
      return not Is_Empty (r) and then
         x >= r.x and then x < r.x + r.w and then
         y >= r.y and then y < r.y + r.h;
   end Point_In_Rect;

   function Union_Rect (a, b : Rect) return Rect is
      x1 : Natural;
      y1 : Natural;
      x2 : Natural;
      y2 : Natural;
   begin
      if Is_Empty (a) then
         return b;
      elsif Is_Empty (b) then
         return a;
      end if;

      x1 := Natural'Min (a.x, b.x);
      y1 := Natural'Min (a.y, b.y);
      x2 := Natural'Max (a.x + a.w, b.x + b.w);
      y2 := Natural'Max (a.y + a.h, b.y + b.h);
      return (x => x1, y => y1, w => x2 - x1, h => y2 - y1);
   end Union_Rect;

   function Inflate_Rect (r : Rect; amount : Natural) return Rect is
      nx : Natural := 0;
      ny : Natural := 0;
      nw : constant Natural := r.w + amount * 2;
      nh : constant Natural := r.h + amount * 2;
   begin
      if Is_Empty (r) then
         return r;
      end if;

      if r.x > amount then
         nx := r.x - amount;
      end if;
      if r.y > amount then
         ny := r.y - amount;
      end if;

      return (x => nx, y => ny, w => nw, h => nh);
   end Inflate_Rect;

   function Clamp_Rect (c : Canvas; r : Rect) return Rect is
      minX : Natural := r.x;
      minY : Natural := r.y;
      maxX : Natural := Client_Canvas_Geometry.Clamped_End (r.x, r.w, c.width);
      maxY : Natural := Client_Canvas_Geometry.Clamped_End (r.y, r.h, c.height);
   begin
      if Is_Empty (r) or else r.x >= c.width or else r.y >= c.height then
         return (others => 0);
      end if;

      if c.clipEnabled then
         if minX < c.clip.x then
            minX := c.clip.x;
         end if;
         if minY < c.clip.y then
            minY := c.clip.y;
         end if;
         if maxX > Client_Canvas_Geometry.Clamped_End (c.clip.x, c.clip.w, c.width) then
            maxX := Client_Canvas_Geometry.Clamped_End (c.clip.x, c.clip.w, c.width);
         end if;
         if maxY > Client_Canvas_Geometry.Clamped_End (c.clip.y, c.clip.h, c.height) then
            maxY := Client_Canvas_Geometry.Clamped_End (c.clip.y, c.clip.h, c.height);
         end if;
      end if;

      if maxX > c.width then
         maxX := c.width;
      end if;
      if maxY > c.height then
         maxY := c.height;
      end if;
      if minX >= maxX or else minY >= maxY then
         return (others => 0);
      end if;

      return (x => minX, y => minY, w => maxX - minX, h => maxY - minY);
   end Clamp_Rect;

   function Input_Rect (c : Canvas; r : Rect) return Rect is
      layout : Canvas := c;
   begin
      if c.inputClipEnabled then
         layout.clipEnabled := True;
         layout.clip := c.inputClip;
      end if;
      return Clamp_Rect (layout, r);
   end Input_Rect;

   function With_Repair_Clip (c : Canvas; repair : Rect) return Canvas is
      ret : Canvas := c;
   begin
      ret.inputClip := Input_Rect (c, (0, 0, c.width, c.height));
      ret.inputClipEnabled := True;
      ret.clip := Clamp_Rect (c, repair);
      ret.clipEnabled := True;
      return ret;
   end With_Repair_Clip;

   function With_Clip (c : Canvas; clip : Rect) return Canvas is
      ret : Canvas := c;
   begin
      if c.inputClipEnabled then
         ret.inputClip := Input_Rect (c, clip);
      end if;
      ret.clip := Clamp_Rect (c, clip);
      ret.clipEnabled := True;
      return ret;
   end With_Clip;

   procedure Set_Pixel (c : Canvas; x, y : Natural; fill : Color) is
   begin
      if x < c.width and then y < c.height then
         Fill_Rect (c, (x, y, 1, 1), fill);
      end if;
   end Set_Pixel;

   function Raster_Rect (c : Canvas; r : Rect) return Rect is
      package G renames Client_Canvas_Geometry;
      clipped : Rect;
      left, top, right, bottom : Natural;
   begin
      if c.width > G.Logical_Edge'Last - c.originX or else
        c.height > G.Logical_Edge'Last - c.originY
      then return (others => 0); end if;
      clipped := Clamp_Rect (c, r);
      if Is_Empty (clipped) or else c.densityNumerator = c.densityDenominator then
         return clipped;
      end if;
      left := G.Relative (c.originX, clipped.x, c.densityNumerator, c.densityDenominator);
      right := G.Relative (c.originX, clipped.x + clipped.w, c.densityNumerator, c.densityDenominator);
      top := G.Relative (c.originY, clipped.y, c.densityNumerator, c.densityDenominator);
      bottom := G.Relative (c.originY, clipped.y + clipped.h, c.densityNumerator, c.densityDenominator);
      return (left, top, right - left, bottom - top);
   end Raster_Rect;

   function Logical_X (c : Canvas; pixel : Natural) return Natural is
     (if c.densityNumerator = c.densityDenominator then pixel else
       Client_Canvas_Geometry.Sample
         (c.originX, c.width, pixel, c.densityNumerator, c.densityDenominator));
   function Logical_Y (c : Canvas; pixel : Natural) return Natural is
     (if c.densityNumerator = c.densityDenominator then pixel else
       Client_Canvas_Geometry.Sample
         (c.originY, c.height, pixel, c.densityNumerator, c.densityDenominator));

   procedure Fill_Rect (c : Canvas; r : Rect; fill : Color) is
      clipped : constant Rect := Raster_Rect (c, r);
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) or else c.pitch mod 4 /= 0 or else
        clipped.x + clipped.w > c.pitch / 4
      then
         return;
      end if;
      declare
         Pitch : constant Positive := c.pitch / 4;
         --  Only the rows touched, from the first of them (Client_Raster).
         Target : Client_Raster.Pixels (0 .. (clipped.h - 1) * Pitch + clipped.x + clipped.w - 1)
           with Import, Address => c.addr + Storage_Offset (clipped.y * c.pitch);
      begin
         Client_Raster.Fill (Target, Pitch, (clipped.x, 0, clipped.w, clipped.h), fill);
      end;
   end Fill_Rect;

   procedure Stroke_Rect
      (c : Canvas; r : Rect; light : Color; dark : Color)
   is
   begin
      if r.w < 2 or else r.h < 2 then
         return;
      end if;

      Fill_Rect (c, (x => r.x, y => r.y, w => r.w, h => 1), light);
      Fill_Rect (c, (x => r.x, y => r.y, w => 1, h => r.h), light);
      Fill_Rect (c, (x => r.x, y => r.y + r.h - 1, w => r.w, h => 1), dark);
      Fill_Rect (c, (x => r.x + r.w - 1, y => r.y, w => 1, h => r.h), dark);
   end Stroke_Rect;



   function Center_Text_Y (r : Rect) return Natural is
      y : Natural := r.y;
   begin
      if r.h > UI_Text_Height then
         y := r.y + (r.h - UI_Text_Height) / 2;
      end if;
      if y > r.y then
         y := y - 1;
      end if;
      return y;
   end Center_Text_Y;

   procedure Draw_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      glyph : Font8x16.GlyphData renames Font8x16.font (Character'Pos (ch));
      clipped : constant Rect :=
        Raster_Rect (c, (x => x, y => y,
                        w => Font8x16.GLYPH_WIDTH,
                        h => Font8x16.GLYPH_HEIGHT));
      offset : Storage_Offset;
      row : Natural;
      bit : Natural;
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         row := Logical_Y (c, yy) - y;
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for xx in clipped.x .. clipped.x + clipped.w - 1 loop
               bit := Logical_X (c, xx) - x;
               offset := Storage_Offset (yy * c.pitch + xx * 4);
               declare
                  pixel : Color with Import, Address => c.addr + offset;
               begin
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                     pixel := fg;
               else
                     pixel := bg;
               end if;
               end;
            end loop;
         end;
      end loop;
   end Draw_Glyph;

   procedure Draw_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color)
   is
      cx : Natural := x;
      glyphW : constant Natural := Font8x16.GLYPH_WIDTH;
      glyphH : constant Natural := Font8x16.GLYPH_HEIGHT;
      textW  : constant Natural := text'Length * glyphW;
   begin
      if c.clipEnabled and then
         (text'Length = 0 or else
          x >= c.clip.x + c.clip.w or else
          x + textW <= c.clip.x or else
          y >= c.clip.y + c.clip.h or else
          y + glyphH <= c.clip.y)
      then
         return;
      end if;

      for i in text'Range loop
         exit when cx + glyphW > c.width;
         if not c.clipEnabled or else
            (cx < c.clip.x + c.clip.w and then
             cx + glyphW > c.clip.x)
         then
            Draw_Glyph (c, cx, y, text (i), fg, bg);
         end if;
         cx := cx + glyphW;
      end loop;
   end Draw_Text;

   function Blend (fg, bg : Color; alpha : Unsigned_8) return Color is
      a : constant Unsigned_32 := Unsigned_32 (alpha);
      inv : constant Unsigned_32 := 255 - a;
      fr : constant Unsigned_32 := Shift_Right (fg, 16) and 16#FF#;
      fgG : constant Unsigned_32 := Shift_Right (fg, 8) and 16#FF#;
      fb : constant Unsigned_32 := fg and 16#FF#;
      br : constant Unsigned_32 := Shift_Right (bg, 16) and 16#FF#;
      bgG : constant Unsigned_32 := Shift_Right (bg, 8) and 16#FF#;
      bb : constant Unsigned_32 := bg and 16#FF#;
      r : constant Unsigned_32 := (fr * a + br * inv + 127) / 255;
      g : constant Unsigned_32 := (fgG * a + bgG * inv + 127) / 255;
      b : constant Unsigned_32 := (fb * a + bb * inv + 127) / 255;
   begin
      return Shift_Left (r, 16) or Shift_Left (g, 8) or b;
   end Blend;

   --  Disabled artwork: desaturated (Rec. 601 luma), pulled halfway to the
   --  theme's muted text colour so it stays visible on dark faces as well
   --  as light ones, and faded.
   LUMA_RED : constant := 77;
   LUMA_GREEN : constant := 150;
   LUMA_BLUE : constant := 29;
   LUMA_SCALE : constant := 256;
   MUTED_PULL : constant := 128;
   DISABLED_ALPHA : constant := 112;
   OPAQUE : constant := 255;
   procedure Shade_Disabled (source : in out Color; alpha : in out Unsigned_8) is
      gray : constant Unsigned_32 :=
        (LUMA_RED * (Shift_Right (source, 16) and 16#FF#) +
         LUMA_GREEN * (Shift_Right (source, 8) and 16#FF#) +
         LUMA_BLUE * (source and 16#FF#) + LUMA_SCALE / 2) / LUMA_SCALE;
   begin
      source := Blend (Shift_Left (gray, 16) or Shift_Left (gray, 8) or gray, Selected_Theme.muted, MUTED_PULL);
      alpha := Unsigned_8 (Unsigned_32 (alpha) * DISABLED_ALPHA / OPAQUE);
   end Shade_Disabled;

   procedure Draw_Bitmap
     (c : Canvas; x, y : Natural; pixels : ARGB_Bitmap;
      enabled : Boolean := True)
   is
      clipped : constant Rect := Raster_Rect
        (c, (x => x, y => y, w => pixels'Length (2), h => pixels'Length (1)));
      source : Color;
      alpha : Unsigned_8;
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      for row in clipped.y .. clipped.y + clipped.h - 1 loop
         for col in clipped.x .. clipped.x + clipped.w - 1 loop
            source := pixels
              (pixels'First (1) + (Logical_Y (c, row) - y), pixels'First (2) + (Logical_X (c, col) - x));
            alpha := Unsigned_8 (Shift_Right (source, 24));
            if alpha /= 0 then
               if not enabled then
                  Shade_Disabled (source, alpha);
               end if;
               declare
                  offset : constant Storage_Offset :=
                    Storage_Offset (row * c.pitch + col * 4);
                  destination : Color with Import, Address => c.addr + offset;
               begin
                  destination :=
                    (if alpha = 255 then source and 16#00FF_FFFF#
                     else Blend (source, destination, alpha));
               end;
            end if;
         end loop;
      end loop;
   end Draw_Bitmap;

   procedure Draw_Bitmap_Native
     (c : Canvas; r : Rect; pixels : ARGB_Bitmap; enabled : Boolean := True)
   is
      package G renames Client_Canvas_Geometry;
      --  The canvas's clip in physical pixels.
      clip : constant Rect := Raster_Rect (c, (0, 0, c.width, c.height));
      boxLeft, boxTop, boxRight, boxBottom : Natural;
      left, top : Integer;
      source : Color;
      alpha : Unsigned_8;
   begin
      if c.addr = System.Null_Address or else Is_Empty (clip) or else Is_Empty (r)
        or else pixels'Length (1) = 0 or else pixels'Length (2) = 0
        or else pixels'Length (1) > Icon_Edge_Choice.Largest_Edge
        or else pixels'Length (2) > Icon_Edge_Choice.Largest_Edge
        or else c.width > G.Logical_Edge'Last - c.originX or else c.height > G.Logical_Edge'Last - c.originY
        or else r.x > G.Logical_Edge'Last - r.w or else r.y > G.Logical_Edge'Last - r.h
      then
         return;
      end if;
      if c.densityNumerator = c.densityDenominator then
         boxLeft := r.x; boxTop := r.y; boxRight := r.x + r.w; boxBottom := r.y + r.h;
      else
         boxLeft := G.Relative (c.originX, r.x, c.densityNumerator, c.densityDenominator);
         boxRight := G.Relative (c.originX, r.x + r.w, c.densityNumerator, c.densityDenominator);
         boxTop := G.Relative (c.originY, r.y, c.densityNumerator, c.densityDenominator);
         boxBottom := G.Relative (c.originY, r.y + r.h, c.densityNumerator, c.densityDenominator);
      end if;
      if boxRight < boxLeft or else boxBottom < boxTop or else
        boxRight > Icon_Edge_Choice.Pixel_Bound or else boxBottom > Icon_Edge_Choice.Pixel_Bound
      then
         return;
      end if;
      left := Icon_Edge_Choice.Centered (boxLeft, boxRight, pixels'Length (2));
      top := Icon_Edge_Choice.Centered (boxTop, boxBottom, pixels'Length (1));
      for row in clip.y .. clip.y + clip.h - 1 loop
         for col in clip.x .. clip.x + clip.w - 1 loop
            if row >= top and then row < top + pixels'Length (1) and then
              col >= left and then col < left + pixels'Length (2)
            then
               source := pixels (pixels'First (1) + (row - top), pixels'First (2) + (col - left));
               alpha := Unsigned_8 (Shift_Right (source, 24));
               if alpha /= 0 then
                  if not enabled then
                     Shade_Disabled (source, alpha);
                  end if;
                  declare
                     offset : constant Storage_Offset := Storage_Offset (row * c.pitch + col * 4);
                     destination : Color with Import, Address => c.addr + offset;
                  begin
                     destination :=
                       (if alpha = 255 then source and 16#00FF_FFFF# else Blend (source, destination, alpha));
                  end;
               end if;
            end if;
         end loop;
      end loop;
   end Draw_Bitmap_Native;

   procedure Fill_Vertical_Gradient
      (c : Canvas; r : Rect; topColor, bottomColor : Color)
   is
      alpha : Unsigned_8;
   begin
      if Is_Empty (r) then
         return;
      elsif r.h = 1 then
         Fill_Rect (c, r, topColor);
         return;
      end if;

      for row in 0 .. r.h - 1 loop
         alpha := Unsigned_8 (row * 255 / (r.h - 1));
         Fill_Rect
           (c, (x => r.x, y => r.y + row, w => r.w, h => 1),
            Blend (bottomColor, topColor, alpha));
      end loop;
   end Fill_Vertical_Gradient;

   --  The one measure (CuBit.Fonts.Width): unrounded advances and kerning,
   --  the same pen positions the drawing below places glyphs at.
   function UI_Text_Width (text : String) return Natural is
     (CuBit.Fonts.Width (CuBit.Fonts.Sans, text));
   function Emphasis_Text_Width (text : String) return Natural is
     (CuBit.Fonts.Width (CuBit.Fonts.Sans_Semibold, text));

   function UI_Text_Height return Natural is
   begin
      return CuBit.Fonts.Line_Height;
   end UI_Text_Height;

   --  A text run's logical box, for its background.
   function Run_Box (x, y : Natural; width : Natural) return Rect is
     ((x, y, width, CuBit.Fonts.Line_Height));

   --  Pen positions are logical pixels and quarters; Pixel_Of may round up to
   --  the next pixel, which still fits Natural for any on-canvas text.
   function Pen_Pixel (Pen : Glyph_Pen.Position) return Natural is (Glyph_Pen.Pixel_Of (Pen));

   Density_Fonts : Client_Glyphs.State;
   -- Pointer access is the narrow bridge: Read pins the immutable A8 mask,
   -- this synchronous blend finishes all reads before returning its lease.
   -- The mask's pen origin sits CuBit.Fonts.Origin logical pixels from its
   -- left edge, so its left edge is that far left of the pen (possibly left
   -- of the canvas); nothing is clipped at the advance. The pen's fraction
   -- picks the mask's phase: the compositor's quarter-pixel placement
   -- (Compositor_Glyph_Placement.Snap_Phase), at the canvas density.
   pragma Compile_Time_Error
     (Glyph_Pen.Unit /= Client_Glyphs.L.Pen_Unit or else Glyph_Pen.Phases /= Client_Glyphs.L.Phases,
      "toolkit pens and density masks must share pen units and phases");
   --  The normal cell size is density 1, its stems fitted to the pixel grid
   --  by the font engine at every phase; the double size (density 2) is not.
   pragma Compile_Time_Error
     (not (Client_Glyphs.L.Grid_Fit_Denominator < Client_Glyphs.L.Grid_Fit_Numerator) or else
      2 * Client_Glyphs.L.Grid_Fit_Denominator < Client_Glyphs.L.Grid_Fit_Numerator,
      "normal cells (density 1) are grid fitted and double cells (density 2) are not");
   procedure Draw_Density_Glyph
     (c : Canvas; Pen : Glyph_Pen.Position; y : Natural; ch : Character; face : CuBit.Fonts.Face; fg : Color)
   is
      package G renames Client_Canvas_Geometry;
      package F renames Client_Glyphs;
      package P renames Compositor_Glyph_Placement;
      Key : F.C.Key;
      View : F.View;
      Layout : F.L.Layout;
      Pen_X : constant Natural := Pen / Glyph_Pen.Unit;
      Box_X : constant Natural := (if Pen_X > CuBit.Fonts.Origin then Pen_X - CuBit.Fonts.Origin else 0);
      Place : P.Subpixel_Origin;
      Clipped : constant Rect := Raster_Rect
        (c, (Box_X, y, CuBit.Fonts.Max_Width, CuBit.Fonts.Line_Height));
      Left, Top, Right, Bottom, X0, Y0 : Integer;
   begin
      if c.addr = System.Null_Address or else Is_Empty (Clipped) or else
        Pen_X > G.Logical_Edge'Last - c.originX or else y > G.Logical_Edge'Last - c.originY
      then return; end if;
      Key := (CuBit.Fonts.Face'Pos (face),
        (if Character'Pos (ch) in 32 .. 126 then Character'Pos (ch) else 63),
        (F.L.G.Scale_Component (c.densityNumerator), F.L.G.Scale_Component (c.densityDenominator)));
      F.Read (Density_Fonts, Key, View);
      if not F.Ready (View) then return; end if;
      Layout := F.Raster (View);
      --  The raster's left column in the surface, less the canvas's own
      --  first column (Edge of its origin): canvas-relative.
      Place := P.Snap_Phase
        (P.G.Logical_Coordinate (c.originX + Pen_X - CuBit.Fonts.Origin), Pen mod Glyph_Pen.Unit, 0,
         (P.G.Scale_Component (c.densityNumerator), P.G.Scale_Component (c.densityDenominator)));
      Left := Integer (Place.Pixel) - Integer (G.Edge (c.originX, c.densityNumerator, c.densityDenominator));
      Top := Integer (G.Relative (c.originY, y, c.densityNumerator, c.densityDenominator));
      Right := Integer'Min (Left + Layout.Width, Clipped.x + Clipped.w);
      Bottom := Integer'Min (Top + Layout.Height, Clipped.y + Clipped.h);
      --  The visible part: the clip and the raster's own box.
      X0 := Integer'Max (Clipped.x, Left);
      Y0 := Integer'Max (Clipped.y, Top);
      declare
         package B renames Client_Glyph_Blend;
         -- Imported arrays cover only the accessed prefix, including a partial
         -- final row. Mapping validity and exclusive writable ownership remain
         -- caller obligations; the pure core proves indexing and damage bounds.
         procedure Paint_View is
            Pitch, Length : Natural;
            Source, Destination : B.Rectangle;
            Mask_Address : constant Integer_Address := To_Integer (F.Pixels (View));
            Target_Address : constant Integer_Address := To_Integer (c.addr);
            Target_Bytes, Row_Address : Integer_Address;
         begin
            if X0 >= Right or else Y0 >= Bottom or else
              X0 < Left or else Y0 < Top or else
              c.pitch = 0 or else c.pitch mod 4 /= 0 or else
              Target_Address mod 4 /= 0
            then return; end if;
            Pitch := c.pitch / 4;
            if Right > Pitch or else Right > Natural'Last / 4 or else
              Bottom - 1 > (Natural'Last / 4 - Right) / Pitch
            then return; end if;
            --  Only the rows the glyph touches: the imported target starts at
            --  its first row, so its contracts never span the whole surface.
            Length := (Bottom - 1 - Y0) * Pitch + Right;
            Source := (X0 - Left, Y0 - Top + Place.Phase * Layout.Height,
                       Right - X0, Bottom - Y0);
            Destination := (X0, 0, Source.Width, Source.Height);
            if not B.Fits (Layout.Bytes, Layout.Pitch, Source) or else
              not B.Fits (Length, Pitch, Destination)
            then return; end if;
            Target_Bytes := Integer_Address (Length) * 4;
            Row_Address := Target_Address + Integer_Address (Y0) * Integer_Address (c.pitch);
            if Mask_Address = 0 or else
              Mask_Address > Integer_Address'Last - Integer_Address (Layout.Bytes) or else
              Row_Address > Integer_Address'Last - Target_Bytes
            then return; end if;
            if Mask_Address < Row_Address + Target_Bytes and then
              Row_Address < Mask_Address + Integer_Address (Layout.Bytes)
            then return; end if;
            declare
               Mask : B.Bytes (0 .. Layout.Bytes - 1)
                 with Import, Address => F.Pixels (View);
               Target : B.Pixels (0 .. Length - 1)
                 with Import, Address => To_Address (Row_Address);
            begin
               B.Paint (Mask, Target, Layout.Pitch, Pitch, Source, Destination, fg);
            end;
         end Paint_View;
      begin
         Paint_View;
      end;
      F.Finish (Density_Fonts, View);
   end Draw_Density_Glyph;

   --  Paint mask columns of Glyph into clipped, the mask's pen origin column
   --  (CuBit.Fonts.Origin) at Pen_X and its top row at y. The pixel loop is
   --  Client_Raster's, proved; this checks everything it relies on.
   type Ramp_Access is access constant Client_Raster.Ramp;
   procedure Blit_Glyph
     (c : Canvas; clipped : Rect; Glyph : CuBit.Fonts.Glyph_Access; Pen_X, y : Natural;
      fg : Color; Opaque : Boolean; bg : Color; Over : Ramp_Access := null)
   is
      package R renames Client_Raster;
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) or else c.pitch mod 4 /= 0 or else
        clipped.x + clipped.w > c.pitch / 4 or else clipped.x + CuBit.Fonts.Origin < Pen_X or else clipped.y < y or else
        clipped.y - y > R.Mask_Row'Last or else clipped.x + CuBit.Fonts.Origin - Pen_X > R.Mask_Column'Last or else
        clipped.h > R.MASK_ROWS - (clipped.y - y) or else
        clipped.w > R.MASK_COLUMNS - (clipped.x + CuBit.Fonts.Origin - Pen_X)
      then
         return;
      end if;
      declare
         Row : constant R.Mask_Row := clipped.y - y;
         Column : constant R.Mask_Column := clipped.x + CuBit.Fonts.Origin - Pen_X;
         Pitch : constant Positive := c.pitch / 4;
         Target : R.Pixels (0 .. (clipped.h - 1) * Pitch + clipped.x + clipped.w - 1)
           with Import, Address => c.addr + Storage_Offset (clipped.y * c.pitch);
         Source : R.Mask with Import, Address => Glyph.Alpha'Address;
      begin
         if Over /= null then
            R.Blit_Mask_Over (Target, Pitch, (clipped.x, 0, clipped.w, clipped.h), Source, Row, Column, fg, bg, Over.all);
         else
            R.Blit_Mask (Target, Pitch, (clipped.x, 0, clipped.w, clipped.h), Source, Row, Column, fg, Opaque, bg);
         end if;
      end;
   end Blit_Glyph;

   --  Blend only Glyph's ink box over what is drawn (density 1): its pen
   --  origin at (Pen_X, y). Ink left of the canvas is clipped.
   procedure Blit_Ink
     (c : Canvas; Glyph : CuBit.Fonts.Glyph_Access; Pen_X, y : Natural; fg : Color;
      Under : Color := 0; Over : Ramp_Access := null)
   is
      Ink_Left : constant Natural := Natural (Glyph.Ink_Left);
      Width : constant Natural := Natural (Glyph.Ink_Width);
      Left : Natural;
      Lost : Natural := 0;
   begin
      if Width = 0 or else Glyph.Ink_Height = 0 or else Ink_Left > CuBit.Fonts.Max_Width or else
        Width > CuBit.Fonts.Max_Width or else Glyph.Ink_Top > Unsigned_32 (CuBit.Fonts.Max_Width) or else
        Glyph.Ink_Height > Unsigned_32 (CuBit.Fonts.Max_Width) or else Pen_X > Natural'Last / 2 or else
        y > Natural'Last / 2
      then return; end if;
      if Pen_X + Ink_Left >= CuBit.Fonts.Origin then
         Left := Pen_X + Ink_Left - CuBit.Fonts.Origin;
      else
         Left := 0;
         Lost := CuBit.Fonts.Origin - Pen_X - Ink_Left;
      end if;
      if Lost >= Width then return; end if;
      Blit_Glyph (c, Clamp_Rect (c, (Left, y + Natural (Glyph.Ink_Top), Width - Lost, Natural (Glyph.Ink_Height))),
                  Glyph, Pen_X, y, fg, False, Under, Over);
   end Blit_Ink;

   --  Glyphs already blended over a solid background, for text whose
   --  background is known: drawing one is then row copies of its cell
   --  instead of blending every pixel. Whole-number densities up to 2 only
   --  (a cell's physical width is then the same everywhere); a glyph whose
   --  ink leaves its cell, or that does not fit a block, is never cached.
   --  Monospace cells only: proportional text overlaps (kerning, phases).
   COMPOSITE_ENTRIES : constant := 256;
   COMPOSITE_WIDTH : constant := 64;
   COMPOSITE_HEIGHT : constant := 34;
   subtype Composite_Index is Natural range 0 .. COMPOSITE_ENTRIES - 1;
   type Composite_Key is record
      Face : CuBit.Fonts.Face := CuBit.Fonts.Sans;
      Ch : Character := ' ';
      Scale : Positive := 1;
      Ink, Under : Color := 0;
   end record;
   type Composite_Entry is record
      Key : Composite_Key;
      Filled : Boolean := False;
      --  False: the glyph is drawn by blending (overhang, or too large).
      Usable : Boolean := False;
      Cell_Width : Natural := 0;
      Pixels : Client_Raster.Pixels (0 .. COMPOSITE_WIDTH * COMPOSITE_HEIGHT - 1) := [others => 0];
   end record;
   type Composite_Table is array (Composite_Index) of Composite_Entry;
   Composites : Composite_Table;

   function Composite_Slot (Key : Composite_Key) return Composite_Index is
      H : Unsigned_32 := Unsigned_32 (Character'Pos (Key.Ch)) * 16#9E37_79B1#;
   begin
      H := H xor (Unsigned_32 (CuBit.Fonts.Face'Pos (Key.Face)) * 16#85EB_CA6B#);
      H := H xor (Key.Ink * 16#C2B2_AE35#) xor (Key.Under * 16#27D4_EB2F#) xor Unsigned_32 (Key.Scale);
      H := H xor Shift_Right (H, 15);
      return Composite_Index (H mod COMPOSITE_ENTRIES);
   end Composite_Slot;

   --  The glyph's whole cell (Advance logical pixels wide, a line high),
   --  ink over Under: what an opaque density glyph draws.
   procedure Draw_Density_Cell
     (c : Canvas; x, y : Natural; ch : Character; face : CuBit.Fonts.Face; fg, bg : Color;
      Advance : Natural)
   is
      package F renames Client_Glyphs;
      package B renames Client_Glyph_Blend;
      package G renames Client_Canvas_Geometry;
      Cell : constant Rect := Raster_Rect (c, (x, y, Advance, CuBit.Fonts.Line_Height));
      Scale : constant Natural :=
        (if c.densityDenominator /= 0 and then c.densityNumerator mod c.densityDenominator = 0
         then Natural (c.densityNumerator / c.densityDenominator) else 0);
      Key : constant Composite_Key := (face, ch, Positive'Max (1, Scale), fg, bg);
      Slot : constant Composite_Index := Composite_Slot (Key);
      E : Composite_Entry renames Composites (Slot);
      --  Physical columns of the mask left of its pen origin.
      Margin : constant Natural := CuBit.Fonts.Origin * Positive'Max (1, Scale);

      procedure Fill_Entry is
         Glyph_Key : F.C.Key;
         View : F.View;
         Layout : F.L.Layout;
      begin
         E.Key := Key;
         E.Filled := True;
         E.Usable := False;
         E.Cell_Width := Advance * Scale;
         if Scale not in 1 .. 2 or else E.Cell_Width > COMPOSITE_WIDTH then
            return;
         end if;
         Glyph_Key := (CuBit.Fonts.Face'Pos (face),
           (if Character'Pos (ch) in 32 .. 126 then Character'Pos (ch) else 63),
           (F.L.G.Scale_Component (c.densityNumerator), F.L.G.Scale_Component (c.densityDenominator)));
         F.Read (Density_Fonts, Glyph_Key, View);
         if not F.Ready (View) then
            return;
         end if;
         Layout := F.Raster (View);
         if Layout.Width <= COMPOSITE_WIDTH and then Layout.Width > Margin and then
           Layout.Height <= COMPOSITE_HEIGHT and then Layout.Height >= CuBit.Fonts.Line_Height * Scale
         then
            declare
               Mask : B.Bytes (0 .. Layout.Bytes - 1) with Import, Address => F.Pixels (View);
               Ink_Outside : Boolean := False;
            begin
               for Row in 0 .. Layout.Height - 1 loop
                  for Column in 0 .. Layout.Width - 1 loop
                     if Column < Margin or else Column >= Margin + E.Cell_Width then
                        Ink_Outside := Ink_Outside or else Mask (Row * Layout.Pitch + Column) /= 0;
                     end if;
                  end loop;
               end loop;
               if not Ink_Outside then
                  E.Pixels := [others => bg];
                  B.Paint (Mask, B.Pixels (E.Pixels), Layout.Pitch, COMPOSITE_WIDTH,
                           (Margin, 0, Layout.Width - Margin, Layout.Height),
                           (0, 0, Layout.Width - Margin, Layout.Height), fg);
                  E.Usable := True;
               end if;
            end;
         end if;
         F.Finish (Density_Fonts, View);
      end Fill_Entry;
   begin
      if c.addr = System.Null_Address or else Is_Empty (Cell) then
         return;
      end if;
      if not E.Filled or else E.Key /= Key then
         Fill_Entry;
      end if;
      if not E.Usable then
         Fill_Rect (c, (x, y, Advance, CuBit.Fonts.Line_Height), bg);
         Draw_Density_Glyph (c, Glyph_Pen.Start (x), y, ch, face, fg);
         return;
      end if;
      declare
         Left : constant Natural := G.Relative (c.originX, x, c.densityNumerator, c.densityDenominator);
         Top : constant Natural := G.Relative (c.originY, y, c.densityNumerator, c.densityDenominator);
      begin
         if c.pitch mod 4 /= 0 or else Cell.x + Cell.w > c.pitch / 4 or else Cell.x < Left or else Cell.y < Top
           or else Cell.x - Left + Cell.w > E.Cell_Width or else Cell.y - Top + Cell.h > COMPOSITE_HEIGHT
         then
            return;
         end if;
         declare
            Pitch : constant Positive := c.pitch / 4;
            Target : Client_Raster.Pixels (0 .. (Cell.h - 1) * Pitch + Cell.x + Cell.w - 1)
              with Import, Address => c.addr + Storage_Offset (Cell.y * c.pitch);
         begin
            Client_Raster.Copy_Block
              (Target, Pitch, (Cell.x, 0, Cell.w, Cell.h), E.Pixels, COMPOSITE_WIDTH, Cell.x - Left, Cell.y - Top);
         end;
      end;
   end Draw_Density_Cell;

   --  Opaque monospace cells at density 1 (code over a known background)
   --  have their own cache of small cells: an editor's palette (token inks
   --  over line, selection, match and occurrence backgrounds) needs far more
   --  entries than the density cache's large blocks allow. Only the keys are
   --  static (about 48 KiB); a slot's pixels come from the process heap the
   --  first time it is filled, so an app pays for the cells it draws (at
   --  most DIRECT_ENTRIES of 1 KiB).
   DIRECT_ENTRIES : constant := 2_048;
   DIRECT_WIDTH : constant := 16;
   DIRECT_HEIGHT : constant := CuBit.Fonts.Line_Height;
   subtype Direct_Index is Natural range 0 .. DIRECT_ENTRIES - 1;
   subtype Direct_Cell is Client_Raster.Pixels (0 .. DIRECT_WIDTH * DIRECT_HEIGHT - 1);
   type Direct_Cell_Access is access Direct_Cell;
   type Direct_Entry is record
      Ch : Character := ' ';
      Face : CuBit.Fonts.Face := CuBit.Fonts.Sans;
      Ink, Under : Color := 0;
      Width : Natural := 0;   --  0: empty
      Cell : Direct_Cell_Access := null;
   end record;
   type Direct_Table is array (Direct_Index) of Direct_Entry;
   Directs : Direct_Table;

   function Direct_Slot (ch : Character; face : CuBit.Fonts.Face; fg, bg : Color) return Direct_Index is
      H : Unsigned_32 := Unsigned_32 (Character'Pos (ch)) * 16#9E37_79B1#;
   begin
      H := H xor (Unsigned_32 (CuBit.Fonts.Face'Pos (face)) * 16#85EB_CA6B#);
      H := H xor (fg * 16#C2B2_AE35#) xor (bg * 16#27D4_EB2F#);
      H := H xor Shift_Right (H, 16);
      H := H * 16#7FEB_352D#;
      H := H xor Shift_Right (H, 15);
      return Direct_Index (H mod DIRECT_ENTRIES);
   end Direct_Slot;

   --  An opaque glyph cell (Width logical pixels by a line) at density 1:
   --  blended over bg once into the direct cache, then row copies. The
   --  cached cell is exactly what Blit_Glyph paints, so the pixels are the
   --  same either way; a cell the cache cannot hold is blended directly.
   procedure Draw_Opaque_Glyph
     (c : Canvas; x, y : Natural; ch : Character; face : CuBit.Fonts.Face; glyph : CuBit.Fonts.Glyph_Access;
      Width : Natural; fg, bg : Color)
   is
      package R renames Client_Raster;
      clipped : constant Rect := Clamp_Rect (c, (x => x, y => y, w => Width, h => CuBit.Fonts.Line_Height));
   begin
      if Width = 0 or else Width > DIRECT_WIDTH or else Width > R.MASK_COLUMNS - CuBit.Fonts.Origin
        or else CuBit.Fonts.Line_Height > R.MASK_ROWS
      then
         Blit_Glyph (c, clipped, glyph, x, y, fg, True, bg);
         return;
      end if;
      if c.addr = System.Null_Address or else Is_Empty (clipped) or else c.pitch mod 4 /= 0 or else
        clipped.x + clipped.w > c.pitch / 4 or else clipped.x < x or else clipped.y < y or else
        clipped.x - x + clipped.w > Width or else clipped.y - y + clipped.h > CuBit.Fonts.Line_Height
      then
         return;
      end if;
      declare
         E : Direct_Entry renames Directs (Direct_Slot (ch, face, fg, bg));
      begin
         if E.Width /= Width or else E.Ch /= ch or else CuBit.Fonts."/=" (E.Face, face) or else E.Ink /= fg or else E.Under /= bg
         then
            declare
               Source : R.Mask with Import, Address => glyph.Alpha'Address;
            begin
               E.Ch := ch;
               E.Face := face;
               E.Ink := fg;
               E.Under := bg;
               E.Width := Width;
               if E.Cell = null then
                  E.Cell := new Direct_Cell;
               end if;
               E.Cell.all := [others => bg];
               R.Blit_Mask (E.Cell.all, DIRECT_WIDTH, (0, 0, Width, CuBit.Fonts.Line_Height), Source, 0,
                            CuBit.Fonts.Origin, fg, True, bg);
            end;
         end if;
         declare
            Pitch : constant Positive := c.pitch / 4;
            Target : R.Pixels (0 .. (clipped.h - 1) * Pitch + clipped.x + clipped.w - 1)
              with Import, Address => c.addr + Storage_Offset (clipped.y * c.pitch);
         begin
            R.Copy_Block (Target, Pitch, (clipped.x, 0, clipped.w, clipped.h), E.Cell.all, DIRECT_WIDTH,
                          clipped.x - x, clipped.y - y);
         end;
      end;
   end Draw_Opaque_Glyph;

   --  Ink-over-background colour ramps for opaque text (Draw_UI_Text), one
   --  per recent (ink, background) pair. Only the keys are static (3 KiB); a
   --  slot's ramp (1 KiB) comes from the process heap the first time it is
   --  filled, so an app pays for the colour pairs it draws. A miss costs one
   --  ramp (256 blends); an app's palette (Files: rows, selection, header,
   --  muted text) must fit without thrashing.
   RAMP_ENTRIES : constant := 256;
   subtype Ramp_Index is Natural range 0 .. RAMP_ENTRIES - 1;
   type Ramp_Cell is access all Client_Raster.Ramp;
   type Ramp_Entry is record
      Ink, Under : Color := 0;
      Shades : Ramp_Cell := null;
   end record;
   type Ramp_Table is array (Ramp_Index) of Ramp_Entry;
   Ramps : Ramp_Table;

   function Ramp_For (fg, bg : Color) return Ramp_Access is
      H : Unsigned_32 := (fg * 16#9E37_79B1#) xor (bg * 16#85EB_CA6B#);
   begin
      H := H xor Shift_Right (H, 16);
      H := H * 16#7FEB_352D#;
      H := H xor Shift_Right (H, 15);
      declare
         E : Ramp_Entry renames Ramps (Ramp_Index (H mod RAMP_ENTRIES));
      begin
         if E.Shades = null then
            E.Shades := new Client_Raster.Ramp;
         elsif E.Ink = fg and then E.Under = bg then
            return Ramp_Access (E.Shades);
         end if;
         E.Ink := fg;
         E.Under := bg;
         E.Shades.all := Client_Raster.Shades (fg, bg);
         return Ramp_Access (E.Shades);
      end;
   end Ramp_For;

   --  Proportional text: one pass of CuBit.Fonts.Lay_Out, each glyph's ink
   --  blended at its pen position (quarter-pixel phase at density 1, the
   --  nearest pixel through the density cache otherwise). Opaque: the run
   --  is already filled with bg.
   procedure Draw_Sans_Glyphs
     (c : Canvas; x, y : Natural; text : String; fg : Color; Opaque : Boolean := False; bg : Color := 0;
      Face : CuBit.Fonts.Face := CuBit.Fonts.Sans)
   is
      Over : constant Ramp_Access :=
        (if Opaque and then c.densityNumerator = c.densityDenominator then Ramp_For (fg, bg) else null);
      procedure Place (Index : Positive; Ch : Character; Pen : Glyph_Pen.Position) is
         pragma Unreferenced (Index);
      begin
         if c.densityNumerator /= c.densityDenominator then
            Draw_Density_Glyph (c, Pen, y, Ch, Face, fg);
         else
            Blit_Ink (c, CuBit.Fonts.Get (Face, Ch, CuBit.Fonts.Normal, Glyph_Pen.Phase_Of (Pen)),
                      Pen_Pixel (Pen), y, fg, bg, Over);
         end if;
      end Place;
      procedure Each is new CuBit.Fonts.Lay_Out (Place);
   begin
      if x > Glyph_Pen.Last_Pixel then return; end if;
      Each (Face, text, x);
   end Draw_Sans_Glyphs;

   --  The run is visible at all (before measuring or rasterizing anything).
   function Run_Visible (c : Canvas; x, y : Natural; text : String) return Boolean is
     (text'Length > 0 and then x < c.width and then y < c.height and then
      (not c.clipEnabled or else
         (x < c.clip.x + c.clip.w and then y < c.clip.y + c.clip.h and then
          y + CuBit.Fonts.Line_Height > c.clip.y)));

   procedure Draw_UI_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color)
   is
   begin
      if not Run_Visible (c, x, y, text) then
         return;
      end if;
      --  One background for the run, then every glyph over it: kerned and
      --  phased glyphs overlap their neighbours' cells.
      Fill_Rect (c, Run_Box (x, y, UI_Text_Width (text)), bg);
      Draw_Sans_Glyphs (c, x, y, text, fg, Opaque => True, bg => bg);
   end Draw_UI_Text;

   procedure Draw_UI_Text_Transparent
      (c : Canvas; x, y : Natural; text : String; fg : Color)
   is
   begin
      if not Run_Visible (c, x, y, text) then
         return;
      end if;
      Draw_Sans_Glyphs (c, x, y, text, fg);
   end Draw_UI_Text_Transparent;

   procedure Draw_Emphasis_Text_Transparent
      (c : Canvas; x, y : Natural; text : String; fg : Color)
   is
   begin
      if not Run_Visible (c, x, y, text) then
         return;
      end if;
      Draw_Sans_Glyphs (c, x, y, text, fg, Face => CuBit.Fonts.Sans_Semibold);
   end Draw_Emphasis_Text_Transparent;

   procedure Draw_Code_Text_Transparent
      (c : Canvas; x, y : Natural; text : String; fg : Color)
   is
      cx : Natural := x;
      width : constant Natural := text'Length * CuBit.Fonts.Mono_Width;
      glyph : CuBit.Fonts.Glyph_Access;
      clipped : Rect;
   begin
      if c.densityNumerator /= c.densityDenominator then
         for i in text'Range loop
            exit when cx >= c.width or else (c.clipEnabled and then cx >= c.clip.x + c.clip.w);
            Draw_Density_Glyph (c, Glyph_Pen.Start (cx), y, text (i), CuBit.Fonts.Monospace, fg);
            cx := cx + CuBit.Fonts.Mono_Width;
         end loop;
         return;
      end if;
      if c.clipEnabled and then
        (text'Length = 0 or else
         x >= c.clip.x + c.clip.w or else
         x + width <= c.clip.x or else
         y >= c.clip.y + c.clip.h or else
         y + CuBit.Fonts.Line_Height <= c.clip.y)
      then
         return;
      end if;
      for i in text'Range loop
         exit when cx >= c.width or else (c.clipEnabled and then cx >= c.clip.x + c.clip.w);
         glyph := CuBit.Fonts.Get (CuBit.Fonts.Monospace, text (i));
         clipped := Clamp_Rect (c, (x => cx, y => y, w => CuBit.Fonts.Mono_Width, h => CuBit.Fonts.Line_Height));
         Blit_Glyph (c, clipped, glyph, cx, y, fg, False, 0);
         cx := cx + CuBit.Fonts.Mono_Width;
      end loop;
   end Draw_Code_Text_Transparent;

   function Code_Text_Width (text : String) return Natural is
     (CuBit.Fonts.Width (CuBit.Fonts.Monospace, text));

   function Code_Text_Height return Natural is
     (CuBit.Fonts.Line_Height);

   function UI_Text_Boundary_At (text : String; x : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Sans, text, x));
   function Code_Text_Boundary_At (text : String; x : Natural) return Natural is
     (CuBit.Fonts.Boundary_At (CuBit.Fonts.Monospace, text, x));
   package UI_Fit is new Text_Fit (UI_Text_Width, UI_Text_Boundary_At);
   package Code_Fit is new Text_Fit (Code_Text_Width, Code_Text_Boundary_At);

   --  Text past Last_Index (no room to append the ellipsis) is left whole
   --  for the clip; no label comes near it.
   function Fit_UI_Text (text : String; width : Natural) return String is
     (if text'Last > UI_Fit.Last_Index then text else UI_Fit.Fitted (text, width));
   function Fit_Code_Text (text : String; width : Natural) return String is
     (if text'Last > Code_Fit.Last_Index then text else Code_Fit.Fitted (text, width));
   function Kept_Code_Text (text : String; width : Natural) return Natural is
     (if text'Last > Code_Fit.Last_Index then text'Length
      elsif text'Length = 0 or else Code_Text_Width (text) <= width then text'Length
      elsif Code_Fit.Prefix_Fits (text, Code_Fit.Kept (text, width), width) then Code_Fit.Kept (text, width)
      else 0);
   function Fit_UI_Text_Tail (text : String; width : Natural) return String is
     (if text'Last > UI_Fit.Last_Index then text else UI_Fit.Fitted_Tail (text, width));

   function Room_Right_Of (box : Rect; x : Natural) return Natural is
     (if box.x + box.w > x then box.x + box.w - x else 0);

   procedure Draw_UI_Text_Fitted
      (c : Canvas; box : Rect; x, y : Natural; text : String; fg, bg : Color) is
   begin
      Draw_UI_Text (With_Clip (c, box), x, y, Fit_UI_Text (text, Room_Right_Of (box, x)), fg, bg);
   end Draw_UI_Text_Fitted;

   procedure Draw_Code_Text_Fitted
      (c : Canvas; box : Rect; x, y : Natural; text : String; fg, bg : Color) is
   begin
      Draw_Code_Text (With_Clip (c, box), x, y, Fit_Code_Text (text, Room_Right_Of (box, x)), fg, bg);
   end Draw_Code_Text_Fitted;

   procedure Draw_Code_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Monospace, ch);
   begin
      Draw_Opaque_Glyph (c, x, y, ch, CuBit.Fonts.Monospace, glyph, CuBit.Fonts.Mono_Width, fg, bg);
   end Draw_Code_Glyph;

   procedure Draw_Code_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color)
   is
      cx : Natural := x;
      width : constant Natural := Code_Text_Width (text);
   begin
      if c.densityNumerator /= c.densityDenominator then
         for i in text'Range loop
            exit when cx >= c.width or else (c.clipEnabled and then cx >= c.clip.x + c.clip.w);
            Draw_Density_Cell (c, cx, y, text (i), CuBit.Fonts.Monospace, fg, bg, CuBit.Fonts.Mono_Width);
            cx := cx + CuBit.Fonts.Mono_Width;
         end loop;
         return;
      end if;
      if c.clipEnabled and then
        (text'Length = 0 or else
         x >= c.clip.x + c.clip.w or else
         x + width <= c.clip.x or else
         y >= c.clip.y + c.clip.h or else
         y + CuBit.Fonts.Line_Height <= c.clip.y)
      then
         return;
      end if;
      for i in text'Range loop
         exit when cx >= c.width;
         if not c.clipEnabled or else
           (cx < c.clip.x + c.clip.w and then
            cx + CuBit.Fonts.Mono_Width > c.clip.x)
         then
            Draw_Code_Glyph (c, cx, y, text (i), fg, bg);
         end if;
         cx := cx + CuBit.Fonts.Mono_Width;
      end loop;
   end Draw_Code_Text;

   function Content_Rect (R : Rect; X_Pad, Y_Pad : Natural) return Rect is
     ((R.x + Natural'Min (X_Pad, R.w), R.y + Natural'Min (Y_Pad, R.h),
       R.w - Natural'Min (2 * X_Pad, R.w),
       R.h - Natural'Min (2 * Y_Pad, R.h)));

   function Control_Edge (Colors : Theme) return Color is
     (Blend (Colors.text, Colors.panel, 70));

   --  Label insets: a button's text keeps BUTTON_TEXT_INSET from both
   --  sides; a tab's starts TAB_TEXT_LEFT in and ends TAB_TEXT_INSET short.
   BUTTON_TEXT_INSET : constant := 6;
   TAB_TEXT_LEFT : constant := 10;
   TAB_TEXT_INSET : constant := 8;
   PANE_TITLE_INSET : constant := 12;

   package body Control_Renderer is
      procedure Stroke_Sunken (c : Canvas; r : Rect; colors : Theme) is
      begin
         Stroke_Rect (c, r, colors.shadow, colors.highlight);
         if r.w > 2 and then r.h > 2 then
            Stroke_Rect (c, Content_Rect (r, 1, 1),
              Blend (colors.darkShadow, colors.field, 150), colors.field);
         end if;
      end Stroke_Sunken;

      procedure Stroke_Raised (c : Canvas; r : Rect; colors : Theme) is
      begin
         Stroke_Rect (c, r, Blend (colors.shadow, colors.face, 180),
           colors.darkShadow);
         if r.w > 2 and then r.h > 2 then
            Stroke_Rect (c, Content_Rect (r, 1, 1), colors.highlight,
              Blend (colors.shadow, colors.face, 190));
         end if;
      end Stroke_Raised;

      function Button_Face (colors : Theme; style : Button_Style) return Color is
        (case style is
           when Button_Hot => Blend (colors.accent, colors.face, 18),
           when Button_Pressed => Blend (colors.accent, colors.face, 45),
           when Button_Disabled => colors.face,
           --  A chosen option: the selection colours, as a selected menu
           --  item or list row (4.5:1 in both schemes; the accent is text-
           --  coloured in the dark one).
           when Button_Active => colors.selection,
           when Button_Normal => Blend (colors.highlight, colors.face, 35));

      procedure Draw_Button_Frame
         (c : Canvas; r : Rect; colors : Theme; style : Button_Style)
      is
         border : constant Color :=
           (case style is
              when Button_Hot => Blend (colors.accent, colors.face, 130),
              when Button_Pressed => colors.accent,
              when Button_Active => colors.selection,
              when Button_Disabled => Blend (colors.text, colors.face, 35),
              when Button_Normal => Control_Edge (colors));
      begin
         if Is_Empty (r) then return; end if;
         -- The small edge bands give depth without repainting the whole face.
         Fill_Rect (c, r, Button_Face (colors, style));
         if r.w > 8 and then r.h > 8 and then
           style not in Button_Disabled | Button_Pressed
         then
            Fill_Rect (c, (r.x + 2, r.y + 2, r.w - 4, 2),
              Blend (colors.highlight, Button_Face (colors, style), 65));
            Fill_Rect (c, (r.x + 2, r.y + r.h - 4, r.w - 4, 2),
              Blend (colors.shadow, Button_Face (colors, style), 35));
         end if;
         case style is
            when Button_Normal => Stroke_Raised (c, r, colors);
            when Button_Pressed =>
               Stroke_Rect (c, r, colors.darkShadow, colors.highlight);
               if r.w > 2 and then r.h > 2 then
                  Stroke_Rect (c, Content_Rect (r, 1, 1), colors.shadow,
                    Button_Face (colors, style));
               end if;
            when Button_Hot | Button_Active =>
               Stroke_Rect (c, r, border, Blend (colors.darkShadow, border, 150));
               if r.w > 2 and then r.h > 2 then
                  Stroke_Rect (c, Content_Rect (r, 1, 1),
                    Blend (colors.highlight, border, 175), border);
               end if;
            when Button_Disabled => Stroke_Rect (c, r, border, border);
         end case;
      end Draw_Button_Frame;

      procedure Draw_Button
         (c : Canvas; r : Rect; colors : Theme; style : Button_Style;
          label : String)
      is
         --  The label elided to the frame's padded inside.
         shown : constant String :=
           Fit_UI_Text (label, (if r.w > 2 * BUTTON_TEXT_INSET then r.w - 2 * BUTTON_TEXT_INSET else 0));
         textW : constant Natural := UI_Text_Width (shown);
         tx : Natural := r.x + BUTTON_TEXT_INSET;
         ty : Natural := r.y;
         fg : Color := colors.text;
      begin
         Draw_Button_Frame (c, r, colors, style);

         if r.w > textW then
            tx := r.x + (r.w - textW) / 2;
         end if;
         ty := Center_Text_Y (r);
         if style = Button_Disabled then
            fg := colors.muted;
         elsif style = Button_Active then
            fg := colors.selectionText;
         end if;

         Draw_UI_Text (With_Clip (c, Content_Rect (r, BUTTON_TEXT_INSET, 2)), tx, ty, shown, fg,
           Button_Face (colors, style));
      end Draw_Button;

      procedure Draw_Tab
         (c : Canvas; r : Rect; colors : Theme;
          selected : Boolean; hot : Boolean; active : Boolean;
          label : String;
          orientation : Tab_Orientation := Horizontal)
      is
         bg : Color := colors.panel;
         fg : constant Color := colors.text;
         ty : Natural := r.y;
         clipped : constant Canvas := With_Clip (c, Content_Rect (r, TAB_TEXT_INSET, 2));
         --  The label elided to end at the right inset.
         shown : constant String :=
           Fit_UI_Text (label, (if r.w > TAB_TEXT_LEFT + TAB_TEXT_INSET then r.w - TAB_TEXT_LEFT - TAB_TEXT_INSET else 0));
      begin
         if Is_Empty (r) then return; end if;
         if selected then
            bg := colors.face;
         elsif hot then
            bg := colors.edge;
         end if;
         if active then
            bg := colors.edge;
         end if;

         Fill_Rect (c, r, bg);
         if active then
            Stroke_Sunken (c, r, colors);
         else
            Stroke_Raised (c, r, colors);
         end if;
         if r.w > 8 and then r.h > 8 and then not active then
            Fill_Rect (c, (r.x + 2, r.y + 2, r.w - 4, 2),
              Blend (colors.highlight, bg, 65));
         end if;
         -- Tabs have side/top relief, never a lower bevel.
         if r.h > 1 and then r.w > 2 then
            Fill_Rect (c, (r.x + 1, r.y + r.h - 2, r.w - 2, 1), bg);
         end if;
         Fill_Rect (c, (r.x, r.y + r.h - 1, r.w, 1), Control_Edge (colors));
         ty := Center_Text_Y (r);
         Draw_UI_Text (clipped, r.x + TAB_TEXT_LEFT, ty, shown, fg, bg);
         if selected then
            case orientation is
               when Horizontal =>
                  Fill_Rect (c, (r.x, r.y, r.w, Natural'Min (2, r.h)), colors.accent);
                  if r.w > 2 then
                     Fill_Rect (c, (r.x, r.y, 1, Natural'Min (2, r.h)),
                       Blend (colors.highlight, colors.accent, 90));
                     Fill_Rect (c, (r.x + r.w - 1, r.y, 1, Natural'Min (2, r.h)),
                       Blend (colors.shadow, colors.accent, 85));
                  end if;
                  if r.w > 2 then
                     Fill_Rect
                       (c, (r.x + 1, r.y + r.h - 1, r.w - 2, 1), bg);
                  end if;
               when Vertical =>
                  if r.h > 2 then
                     --  The selected tab opens into its page on the right.
                     Fill_Rect
                       (c, (r.x + r.w - 1, r.y + 1, 1, r.h - 2), bg);
                     Fill_Rect
                       (c, (r.x, r.y + 1, Natural'Min (2, r.w), r.h - 2),
                        colors.accent);
                  end if;
            end case;
         end if;
      end Draw_Tab;
   end Control_Renderer;

   package Canvas_Controls is new Control_Renderer
     (Fill_Rect, Stroke_Rect, Draw_UI_Text);

   procedure Stroke_Sunken (C : Canvas; R : Rect; Colors : Theme) is
   begin
      Canvas_Controls.Stroke_Sunken (C, R, Colors);
   end Stroke_Sunken;

   procedure Stroke_Raised (C : Canvas; R : Rect; Colors : Theme) is
   begin
      Canvas_Controls.Stroke_Raised (C, R, Colors);
   end Stroke_Raised;

   procedure Draw_Button_Frame
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style)
   is
   begin
      Canvas_Controls.Draw_Button_Frame (c, r, colors, style);
   end Draw_Button_Frame;

   procedure Draw_Button
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style;
       label : String)
   is
   begin
      Canvas_Controls.Draw_Button (c, r, colors, style, label);
   end Draw_Button;

   --  The menu bar continues the compositor's title bar (one flat band, the
   --  title/menu rule between them drawn by the compositor) and ends in a
   --  one-pixel rule (CuBit.UI.Window_Chrome).
   procedure Draw_Menu_Surface (c : Canvas; r : Rect; colors : Theme) is
   begin
      Fill_Rect (c, (r.x, r.y, r.w, r.h - 1), colors.titleBar);
      Fill_Rect (c, (r.x, r.y + r.h - 1, r.w, 1), colors.menuRule);
   end Draw_Menu_Surface;

   procedure Draw_Menu_Bar (c : Canvas; r : Rect; colors : Theme) is
   begin
      if Is_Empty (r) then return; end if;
      Draw_Menu_Surface (c, r, colors);
   end Draw_Menu_Bar;

   procedure Draw_Menu_Title
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean; label : String)
   is
      bg : constant Color := (if active then colors.selection else colors.face);
      fg : constant Color := (if active then colors.selectionText else colors.text);
      inner : constant Rect := Content_Rect (r, 2, 2);
   begin
      if Is_Empty (r) then return; end if;
      -- Use the same row colors as the strip, including during partial repair.
      Draw_Menu_Surface (c, r, colors);
      if active or hot then
         Fill_Rect (c, inner, bg);
         Stroke_Rect (c, inner, colors.shadow, colors.shadow);
      end if;
      Draw_UI_Text_Transparent (With_Clip (c, inner),
        r.x + MENU_TITLE_PADDING, Center_Text_Y (r), label, fg);
   end Draw_Menu_Title;

   function Menu_Title_Width (label : String) return Natural is
     (UI_Text_Width (label) + 2 * MENU_TITLE_PADDING);

   procedure Draw_Status_Bar
      (c : Canvas; r : Rect; colors : Theme; left, right : String)
   is
      textBounds : constant Rect := Content_Rect (r, 8, 3);
      rightWidth : constant Natural :=
        (if right'Length = 0 or else textBounds.w < 120 then 0
         else Natural'Min (UI_Text_Width (right), textBounds.w / 3));
      gap : constant Natural := (if rightWidth > 0 then 16 else 0);
      leftBounds : constant Rect :=
        (textBounds.x, textBounds.y, textBounds.w - rightWidth - gap, textBounds.h);
      rightBounds : constant Rect :=
        (textBounds.x + textBounds.w - rightWidth, textBounds.y, rightWidth, textBounds.h);
   begin
      if Is_Empty (r) then return; end if;
      Fill_Rect (c, r, colors.panel);
      -- A recessed status well with a narrow panel margin around its rim.
      Stroke_Rect (c, Content_Rect (r, 1, 1), colors.shadow, colors.highlight);
      Stroke_Rect (c, Content_Rect (r, 2, 2),
        Blend (colors.darkShadow, colors.panel, 150), colors.panel);
      Draw_UI_Text_Fitted (c, leftBounds, leftBounds.x, Center_Text_Y (r),
        left, colors.text, colors.panel);
      if rightWidth > 0 then
         Draw_UI_Text_Fitted (c, rightBounds, rightBounds.x, Center_Text_Y (r),
           right, colors.muted, colors.panel);
      end if;
   end Draw_Status_Bar;

   procedure Draw_Pane
      (c : Canvas; r : Rect; colors : Theme; title : String)
   is
      --  The title starts PANE_TITLE_INSET in and ends as far from the right.
      shown : constant String :=
        Fit_UI_Text (title, (if r.w > 2 * PANE_TITLE_INSET then r.w - 2 * PANE_TITLE_INSET else 0));
      titleW : constant Natural := UI_Text_Width (shown);
      frameY : constant Natural := r.y + UI_Text_Height / 2;
      frame : constant Rect :=
        (x => r.x + 2, y => frameY,
         w => (if r.w > 4 then r.w - 4 else 0),
         h => (if r.h > UI_Text_Height / 2 + 2 then
                  r.h - UI_Text_Height / 2 - 2
               else 0));
      titleRect : constant Rect :=
        (x => r.x + 8, y => r.y, w => titleW + 8, h => UI_Text_Height);
   begin
      if Is_Empty (r) then return; end if;
      Fill_Rect (c, r, colors.panel);
      Stroke_Rect (With_Clip (c, r), frame, Control_Edge (colors), Control_Edge (colors));
      if shown'Length > 0 then
         Fill_Rect (With_Clip (c, Content_Rect (r, 8, 0)), titleRect, colors.panel);
         Draw_UI_Text (With_Clip (c, Content_Rect (r, PANE_TITLE_INSET, 0)), titleRect.x + 4, titleRect.y,
                       shown, colors.muted, colors.panel);
      end if;
   end Draw_Pane;

   procedure Draw_Table_Viewport
      (c : Canvas; r : Rect; colors : Theme)
   is
   begin
      Fill_Rect (c, r, colors.field);
      Stroke_Sunken (c, r, colors);
   end Draw_Table_Viewport;

   procedure Draw_Table_Viewport_Frame
      (c : Canvas; r : Rect; colors : Theme)
   is
   begin
      Stroke_Sunken (c, r, colors);
   end Draw_Table_Viewport_Frame;

   function Table_Interior (r : Rect) return Rect is
      FRAME_WIDTH : constant Natural := 2;
      FRAME_PAIR  : constant Natural := FRAME_WIDTH * 2;
   begin
      if r.w > FRAME_PAIR and then r.h > FRAME_PAIR then
         return
           (x => r.x + FRAME_WIDTH,
            y => r.y + FRAME_WIDTH,
            w => r.w - FRAME_PAIR,
            h => r.h - FRAME_PAIR);
      else
         return (x => r.x, y => r.y, w => 0, h => 0);
      end if;
   end Table_Interior;

   function Layout_Table (viewport : Rect) return Table_Regions is
      interior : constant Rect := Table_Interior (viewport);
      headerHeight : constant Natural :=
        Natural'Min (Table_Header_Height, interior.h);
   begin
      return
        (Header =>
           (x => interior.x, y => interior.y,
            w => interior.w, h => headerHeight),
         Rows =>
           (x => interior.x, y => interior.y + headerHeight,
            w => interior.w, h => interior.h - headerHeight));
   end Layout_Table;

   procedure Draw_Table_Header
      (c : Canvas; r : Rect; colors : Theme; c1, c2, c3 : String;
       layout : Table_Column_Layout := Default_Table_Columns)
   is
      HEADER_FRAME_WIDTH : constant Natural := 2;
      firstWidth : constant Natural := Natural'Min (layout.First_Width, r.w);
      remaining : constant Natural := r.w - firstWidth;
      secondWidth : constant Natural :=
        Natural'Min (layout.Second_Width, remaining);
      thirdWidth : constant Natural := remaining - secondWidth;
      first : constant Rect :=
        (x => r.x, y => r.y, w => firstWidth, h => r.h);
      second : constant Rect :=
        (x => r.x + firstWidth, y => r.y, w => secondWidth, h => r.h);
      third : constant Rect :=
        (x => second.x + secondWidth, y => r.y, w => thirdWidth, h => r.h);
      labelBounds : constant Rect :=
        (if r.h > HEADER_FRAME_WIDTH * 2 then
           (x => r.x,
            y => r.y + HEADER_FRAME_WIDTH,
            w => r.w,
            h => r.h - HEADER_FRAME_WIDTH * 2)
         else r);
      labelY : constant Natural := Center_Text_Y (labelBounds);
   begin
      if Is_Empty (r) then return; end if;
      Fill_Rect (c, r, colors.panel);
      Fill_Rect (c, (r.x, r.y + r.h - 1, r.w, 1), Control_Edge (colors));
      if firstWidth > 0 and firstWidth < r.w then
         Fill_Rect (c, (r.x + firstWidth - 1, r.y, 1, r.h), Control_Edge (colors));
      end if;
      if secondWidth > 0 and firstWidth + secondWidth < r.w then
         Fill_Rect (c, (r.x + firstWidth + secondWidth - 1, r.y, 1, r.h), Control_Edge (colors));
      end if;
      Draw_UI_Text_Fitted
        (c, Content_Rect (first, layout.Cell_Padding, 2), first.x + layout.Cell_Padding,
         labelY, c1, colors.text, colors.panel);
      Draw_UI_Text_Fitted
        (c, Content_Rect (second, layout.Cell_Padding, 2), second.x + layout.Cell_Padding,
         labelY, c2, colors.text, colors.panel);
      Draw_UI_Text_Fitted
        (c, Content_Rect (third, layout.Cell_Padding, 2), third.x + layout.Cell_Padding,
         labelY, c3, colors.text, colors.panel);
   end Draw_Table_Header;

   procedure Draw_Table_Row
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean;
       c1, c2, c3 : String;
       layout : Table_Column_Layout := Default_Table_Columns;
       textStyle : Table_Text_Style := Table_Interface_Text;
       detail3 : String := "")
   is
      bg : Color := colors.field;
      fg : Color := colors.text;
      firstWidth : constant Natural := Natural'Min (layout.First_Width, r.w);
      remaining : constant Natural := r.w - firstWidth;
      secondWidth : constant Natural :=
        Natural'Min (layout.Second_Width, remaining);
      thirdWidth : constant Natural := remaining - secondWidth;
      first : constant Rect :=
        (x => r.x, y => r.y, w => firstWidth, h => r.h);
      second : constant Rect :=
        (x => r.x + firstWidth, y => r.y, w => secondWidth, h => r.h);
      third : constant Rect :=
        (x => second.x + secondWidth, y => r.y, w => thirdWidth, h => r.h);

      procedure Draw_Cell
        (cell : Rect; value : String; detail : String := "")
      is
         box : constant Rect := Content_Rect (cell, layout.Cell_Padding, 1);
         primaryY : constant Natural :=
           (if detail'Length > 0 then cell.y + 1
            elsif textStyle = Table_Code_Text and then
              cell.h > Code_Text_Height
            then cell.y + (cell.h - Code_Text_Height) / 2
            else Center_Text_Y (cell));
         detailForeground : constant Color :=
           (if selected then colors.selectionText else colors.muted);
      begin
         if textStyle = Table_Code_Text then
            Draw_Code_Text_Fitted
              (c, box, cell.x + layout.Cell_Padding, primaryY, value, fg, bg);
            if detail'Length > 0 then
               Draw_Code_Text_Fitted
                 (c, box, cell.x + layout.Cell_Padding,
                  primaryY + Code_Text_Height, detail,
                  detailForeground, bg);
            end if;
         else
            Draw_UI_Text_Fitted
              (c, box, cell.x + layout.Cell_Padding, primaryY,
               value, fg, bg);
            if detail'Length > 0 then
               Draw_UI_Text_Fitted
                 (c, box, cell.x + layout.Cell_Padding,
                  primaryY + UI_Text_Height, detail,
                  detailForeground, bg);
            end if;
         end if;
      end Draw_Cell;
   begin
      if Is_Empty (r) then return; end if;
      if selected then
         bg := colors.selection;
         fg := colors.selectionText;
      elsif hot then
         bg := colors.panel;
      end if;

      Fill_Rect (c, r, bg);
      Fill_Rect (c, (x => r.x, y => r.y + r.h - 1, w => r.w, h => 1),
                 colors.edge);
      if firstWidth > 0 and then firstWidth < r.w then
         Fill_Rect
           (c, (x => r.x + firstWidth - 1, y => r.y, w => 1, h => r.h),
            colors.edge);
      end if;
      if secondWidth > 0 and then firstWidth + secondWidth < r.w then
         Fill_Rect
           (c, (x => r.x + firstWidth + secondWidth - 1,
                y => r.y, w => 1, h => r.h), colors.edge);
      end if;
      Draw_Cell (first, c1);
      Draw_Cell (second, c2);
      Draw_Cell (third, c3, detail3);
   end Draw_Table_Row;

   procedure Draw_Vertical_Splitter
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean)
   is
      fill : constant Color :=
        (if active then colors.shadow
         elsif hot then colors.edge
         else colors.desktop);
      centerX : Natural;
      gripY : Natural;
   begin
      if Is_Empty (r) then return; end if;
      Fill_Rect (c, r, fill);
      centerX := r.x + r.w / 2;
      if r.h >= 30 then
         gripY := r.y + r.h / 2 - 12;
         for Index in 0 .. 4 loop
            if centerX > 0 then
               Set_Pixel (c, centerX - 1, gripY + Index * 5,
                          colors.darkShadow);
            end if;
            Set_Pixel (c, centerX, gripY + Index * 5 + 1,
                       colors.highlight);
         end loop;
      end if;
   end Draw_Vertical_Splitter;

   procedure Draw_Tab_Strip
      (c : Canvas; r : Rect; colors : Theme)
   is
   begin
      Fill_Rect (c, r, colors.panel);
      if r.h > 0 then
         Fill_Rect
           (c, (x => r.x, y => r.y + r.h - 1, w => r.w, h => 1),
            Control_Edge (colors));
      end if;
   end Draw_Tab_Strip;

   procedure Draw_Tab
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean;
       label : String;
       orientation : Tab_Orientation := Horizontal)
   is
   begin
      Canvas_Controls.Draw_Tab (c, r, colors, selected, hot, active, label, orientation);
   end Draw_Tab;

   procedure Draw_Natural_Value
      (c : Canvas; r : Rect; colors : Theme; value : Natural)
   is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      first : Natural;
      v : Natural := value;
   begin
      Fill_Rect (c, r, colors.panel);
      if v = 0 then
         Draw_UI_Text (c, r.x, r.y, "0", colors.text, colors.panel);
         return;
      end if;

      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') + (v mod 10));
         v := v / 10;
         exit when pos = buf'First;
         pos := pos - 1;
      end loop;

      if v = 0 then
         if pos = buf'First then
            first := pos;
         else
            first := pos + 1;
         end if;
      else
         first := pos;
      end if;

      Draw_UI_Text (c, r.x, r.y, buf (first .. buf'Last),
                    colors.text, colors.panel);
   end Draw_Natural_Value;

   procedure Draw_Progress_Bar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural)
   is
      span : Natural := 1;
      pos : Natural := 0;
      fillW : Natural := 0;
   begin
      if maxValue > minValue then
         span := maxValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if r.w > 0 then
         fillW := (pos * r.w) / span;
      end if;

      Fill_Rect (c, r, colors.shadow);
      if fillW > 0 then
         Fill_Rect (c, (x => r.x, y => r.y, w => fillW, h => r.h),
                    colors.good);
      end if;
   end Draw_Progress_Bar;

   procedure Draw_Swatch
      (c : Canvas; r : Rect; colors : Theme;
       fill : Color; label : String)
   is
      swatch : constant Rect := (x => r.x, y => r.y, w => Natural'Min (28, r.w), h => r.h);
   begin
      Fill_Rect (c, swatch, fill);
      Stroke_Rect (c, swatch, colors.edge, colors.shadow);
      Draw_UI_Text_Fitted (c, r, r.x + 36, Center_Text_Y (r), label, colors.text, colors.panel);
   end Draw_Swatch;

   procedure Draw_Text_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       focused : Boolean; hot : Boolean)
   is
      face : constant Color := colors.field;
      textX : constant Natural := r.x + 8;
      textY : Natural := r.y;
      cursorX : Natural := textX + UI_Text_Width (text);
      cursor : Rect;
      textCanvas : constant Canvas := With_Clip
        (c, Content_Rect (r, 8, 2));
   begin
      if Is_Empty (r) then return; end if;

      Fill_Rect (c, r, face);
      if focused or hot then
         Stroke_Rect (c, r, colors.accent, colors.highlight);
         if r.w > 2 and then r.h > 2 then
            Stroke_Rect (c, Content_Rect (r, 1, 1),
              Blend (colors.accent, colors.field, 100), colors.field);
         end if;
      else
         Stroke_Sunken (c, r, colors);
      end if;

      textY := Center_Text_Y (r) +
        (if r.h >= UI_Text_Height + 8 then 2 else 0);

      Draw_UI_Text (textCanvas, textX, textY, text, colors.text, face);
      if focused then
         if cursorX + 1 >= r.x + r.w - Natural'Min (8, r.w) then
            cursorX := r.x + r.w - Natural'Min (10, r.w);
         end if;
         cursor := (x => cursorX + 1, y => textY + 2,
                    w => 1, h => UI_Text_Height - 4);
         Fill_Rect (textCanvas, cursor, colors.accent);
      end if;
   end Draw_Text_Field;

   procedure Draw_Text_Edit_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       cursor, selectionStart, selectionEnd : Natural;
       focused : Boolean; hot : Boolean; suggestion : String := "";
       masked : Boolean := False)
   is
      face : constant Color := colors.field;
      textX : Natural := r.x + 8;
      textY : Natural := r.y;
      cursorX : Natural := textX;
      caret : Rect;
      textCanvas : constant Canvas := With_Clip
        (c, Content_Rect (r, 8, 2));
   begin
      if Is_Empty (r) then return; end if;
      if masked then
         declare
            Shown : constant Natural :=
              (if text'Length = 0 then 0 else Secret_Mask'Length);
            Selected : constant Boolean :=
              Shown > 0 and then selectionStart < selectionEnd;
         begin
            Draw_Text_Edit_Field
              (c, r, colors, Secret_Mask (1 .. Shown), Shown,
               (if Selected then 0 else Shown), Shown, focused, hot);
            return;
         end;
      end if;

      Fill_Rect (c, r, face);
      if focused or hot then
         Stroke_Rect (c, r, colors.accent, colors.highlight);
         if r.w > 2 and then r.h > 2 then
            Stroke_Rect (c, Content_Rect (r, 1, 1),
              Blend (colors.accent, colors.field, 100), colors.field);
         end if;
      else
         Stroke_Sunken (c, r, colors);
      end if;

      textY := Center_Text_Y (r) +
        (if r.h >= UI_Text_Height + 8 then 2 else 0);

      --  The text as one run (kerning, subpixel pens); the selection is a
      --  rectangle between two caret boundaries of that same layout, its
      --  glyphs inked in the selection colour through a clip.
      declare
         Last : constant Natural := text'Length;
         From : constant Natural := textX + CuBit.Fonts.Boundary
           (CuBit.Fonts.Sans, text, Natural'Min (selectionStart, Last));
         To : constant Natural := textX + CuBit.Fonts.Boundary
           (CuBit.Fonts.Sans, text, Natural'Min (selectionEnd, Last));
         Selected : constant Boolean := focused and then selectionStart < selectionEnd and then From < To;
         Line : constant Natural := UI_Text_Height;
      begin
         if Selected then
            Fill_Rect (textCanvas, (From, textY, To - From, Line), colors.selection);
            Draw_UI_Text_Transparent (With_Clip (textCanvas, (0, textY, From, Line)), textX, textY, text, colors.text);
            Draw_UI_Text_Transparent (With_Clip (textCanvas, (From, textY, To - From, Line)), textX, textY, text,
                                      colors.selectionText);
            Draw_UI_Text_Transparent
              (With_Clip (textCanvas, (To, textY, Natural'Max (r.x + r.w, To) - To, Line)), textX, textY, text,
               colors.text);
         else
            Draw_UI_Text_Transparent (textCanvas, textX, textY, text, colors.text);
         end if;
         cursorX := textX + CuBit.Fonts.Boundary (CuBit.Fonts.Sans, text, Natural'Min (cursor, Last));
         textX := textX + UI_Text_Width (text);
      end;

      if focused and then cursor = text'Length and then
        selectionStart = selectionEnd
      then
         Draw_UI_Text_Transparent (textCanvas, textX, textY, suggestion, colors.muted);
      end if;

      if focused then
         if cursorX + 1 >= r.x + r.w - Natural'Min (8, r.w) then
            cursorX := r.x + r.w - Natural'Min (10, r.w);
         end if;
         caret := (x => cursorX + 1, y => textY + 2,
                   w => 1, h => UI_Text_Height - 4);
         Fill_Rect (textCanvas, caret, colors.accent);
      end if;
   end Draw_Text_Edit_Field;

   procedure Draw_Multiline_Text_Edit
      (c : Canvas; r : Rect; colors : Theme; text : String;
       firstLine, visibleLines, cursor, selectionStart, selectionEnd : Positive;
       focused : Boolean; hot : Boolean; firstColumn : Positive := 1)
   is
   begin
      Draw_Multiline_Text_Edit_Multiple
         (c, r, colors, text, firstLine, visibleLines,
         [(cursor, selectionStart, selectionEnd)], focused, hot, firstColumn);
   end Draw_Multiline_Text_Edit;

   procedure Draw_Multiline_Text_Edit_Multiple
      (c : Canvas; r : Rect; colors : Theme; text : String;
       firstLine, visibleLines : Positive; cursors : Text_Cursor_States;
       focused : Boolean; hot : Boolean; firstColumn : Positive := 1)
   is
      Plain_Text : constant Text_Style_Spans :=
        [(firstPosition => 1, lastPosition => 1,
          foreground => colors.text,
          decoration => No_Text_Decoration, decorationColor => 0)];
   begin
      Draw_Multiline_Text_Edit_Multiple_Styled
        (c, r, colors, text, firstLine, visibleLines, cursors, Plain_Text,
         focused, hot, firstColumn);
   end Draw_Multiline_Text_Edit_Multiple;

   procedure Draw_Multiline_Text_Edit_Multiple_Styled
      (c : Canvas; r : Rect; colors : Theme; text : String;
       firstLine, visibleLines : Positive; cursors : Text_Cursor_States;
       styles : Text_Style_Spans;
       focused : Boolean; hot : Boolean; firstColumn : Positive := 1)
   is
      face : Color := colors.field;
      lineHeight : constant Natural := Code_Text_Height + 2;
      line : Positive := 1;
      column : Positive := 1;
      textX : Natural := r.x + 6;
      textY : Natural := r.y + 5;
      charW : Natural;
      fg : Color;
      bg : Color;
      absolutePosition : Positive := 1;
      lastVisible : constant Natural := firstLine + visibleLines - 1;
      styleIndex : Positive := styles'First;
      hasStyle : Boolean := styles'Length > 0;
      previousStyleEnd : Natural := 0;
      textCanvas : constant Canvas := With_Clip
        (c, (x => r.x + 3, y => r.y + 3,
             w => (if r.w > 6 then r.w - 6 else 0),
             h => (if r.h > 6 then r.h - 6 else 0)));

      function Selected (Position : Positive) return Boolean is
      begin
         for State of cursors loop
            if Position >= State.selectionStart and then
              Position < State.selectionEnd
            then
               return True;
            end if;
         end loop;
         return False;
      end Selected;

      function Foreground_At (Position : Positive) return Color is
      begin
         while hasStyle and then
           Position > styles (styleIndex).lastPosition
         loop
            if styleIndex = styles'Last then
               hasStyle := False;
            else
               styleIndex := styleIndex + 1;
            end if;
         end loop;
         if hasStyle and then
           Position >= styles (styleIndex).firstPosition and then
           Position <= styles (styleIndex).lastPosition
         then
            return styles (styleIndex).foreground;
         end if;
         return colors.text;
      end Foreground_At;

      function Underlined_At (Position : Positive) return Boolean is
        (hasStyle and then
         Position >= styles (styleIndex).firstPosition and then
         Position <= styles (styleIndex).lastPosition and then
         styles (styleIndex).decoration = Text_Underline);

      function Decoration_Color_At (Position : Positive) return Color is
        (if Underlined_At (Position) then
            styles (styleIndex).decorationColor
         else 0);

      procedure Draw_Carets (Position : Positive) is
      begin
         if not focused then return; end if;
         for State of cursors loop
            if Position = State.cursor and then
              textX + 1 < r.x + r.w and then textY < r.y + r.h
            then
               Fill_Rect
                 (textCanvas, (x => textX, y => textY + 1,
                    w => 1, h => Code_Text_Height), colors.accent);
            end if;
         end loop;
      end Draw_Carets;
   begin
      --  Reject the complete decoration set if it is not a valid ordered,
      --  non-overlapping view of this document.  Decorations are optional;
      --  malformed ones must never affect editor correctness or safety.
      if hasStyle then
         for Style of styles loop
            if Style.firstPosition > Style.lastPosition or else
              Style.lastPosition > text'Length or else
              Style.firstPosition <= previousStyleEnd
            then
               hasStyle := False;
               exit;
            end if;
            previousStyleEnd := Style.lastPosition;
         end loop;
      end if;
      if hot then face := colors.face; end if;
      Fill_Rect (c, r, face);
      if focused then
         Stroke_Rect (c, r, colors.accent, colors.highlight);
         if r.w > 2 and then r.h > 2 then
            Stroke_Rect (c, Content_Rect (r, 1, 1),
              Blend (colors.accent, colors.field, 100), colors.field);
         end if;
      else
         Stroke_Sunken (c, r, colors);
      end if;

      for index in text'Range loop
         if line >= firstLine and then line <= lastVisible and then
           column >= firstColumn
         then
            textY := r.y + 5 + (line - firstLine) * lineHeight;
            if text (index) /= ASCII.LF then
               if Selected (absolutePosition) then
                  fg := colors.selectionText;
                  bg := colors.selection;
               else
                  fg := Foreground_At (absolutePosition);
                  bg := face;
               end if;
               Draw_Code_Text
                 (textCanvas, textX, textY, text (index .. index), fg, bg);
               charW := Code_Text_Width (text (index .. index));
               if not Selected (absolutePosition) and then
                 Underlined_At (absolutePosition)
               then
                  Fill_Rect
                    (textCanvas,
                     (x => textX, y => textY + Code_Text_Height - 1,
                      w => charW, h => 1),
                     Decoration_Color_At (absolutePosition));
               end if;
            end if;
            Draw_Carets (absolutePosition);
            if text (index) /= ASCII.LF then
               textX := textX + charW;
            end if;
         end if;
         if text (index) = ASCII.LF then
            line := line + 1;
            column := 1;
            textX := r.x + 6;
         else
            column := column + 1;
         end if;
         absolutePosition := absolutePosition + 1;
      end loop;

      if line >= firstLine and then line <= lastVisible and then
        column >= firstColumn
      then
         textY := r.y + 5 + (line - firstLine) * lineHeight;
         Draw_Carets (absolutePosition);
      end if;
   end Draw_Multiline_Text_Edit_Multiple_Styled;

   procedure Draw_Checkbox
      (c : Canvas; r : Rect; colors : Theme;
       checked : Boolean; hot : Boolean; active : Boolean)
   is
      pc : constant Canvas := With_Clip (c, r);
      edge : constant Color := (if hot or active then colors.accent else Control_Edge (colors));
      cx : constant Natural := r.x + r.w / 2;
      cy : constant Natural := r.y + r.h / 2;
   begin
      if Is_Empty (r) then return; end if;
      Fill_Rect (pc, r, (if checked then colors.selection else colors.field));
      Stroke_Rect (pc, r, edge, colors.highlight);
      if r.w > 2 and then r.h > 2 then
         Stroke_Rect (pc, Content_Rect (r, 1, 1),
           Blend (colors.darkShadow, edge, 100),
           (if checked then colors.selection else colors.field));
      end if;
      if checked and then r.w >= 12 and then r.h >= 12 then
         for I in 0 .. 2 loop
            Fill_Rect (pc, (cx - 4 + I, cy + I, 2, 2), colors.selectionText);
         end loop;
         for I in 0 .. 4 loop
            Fill_Rect (pc, (cx - 1 + I, cy + 2 - I, 2, 2), colors.selectionText);
         end loop;
      end if;
   end Draw_Checkbox;

   -- Small circular coverage masks are prepared once, never during painting.
   -- Four-by-four coverage keeps the 14px radio round without floating point.
   type Radio_Coverage_Table is
     array (Natural range 0 .. 14, Natural range 0 .. 13,
            Natural range 0 .. 13) of Unsigned_8;
   function Build_Radio_Coverage return Radio_Coverage_Table is
      Result : Radio_Coverage_Table := [others => [others => [others => 0]]];
      Count : Natural;
      DX, DY : Integer;
   begin
      for Size in 1 .. 14 loop
         for Y in 0 .. Size - 1 loop
            for X in 0 .. Size - 1 loop
               Count := 0;
               for SY in 0 .. 3 loop
                  DY := 8 * Y + 2 * SY + 1 - 4 * Size;
                  for SX in 0 .. 3 loop
                     DX := 8 * X + 2 * SX + 1 - 4 * Size;
                     if DX * DX + DY * DY <= 16 * Size * Size then
                        Count := Count + 1;
                     end if;
                  end loop;
               end loop;
               Result (Size, Y, X) := Unsigned_8 ((Count * 255 + 8) / 16);
            end loop;
         end loop;
      end loop;
      return Result;
   end Build_Radio_Coverage;
   Radio_Coverage : constant Radio_Coverage_Table := Build_Radio_Coverage;

   procedure Draw_Radio_Button
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean; label : String)
   is
      pc : constant Canvas := With_Clip (c, r);
      size : constant Natural := Natural'Min (14, Natural'Min (r.w, r.h));
      box : constant Rect := (r.x, r.y + (r.h - size) / 2, size, size);
      edge : constant Color := (if hot or active then colors.accent else Control_Edge (colors));
      procedure Disc (B : Rect; Fill, Background : Color) is
         X, Last : Natural;
         Coverage : Unsigned_8;
      begin
         if Is_Empty (B) then return; end if;
         for Y in 0 .. B.h - 1 loop
            X := 0;
            while X < B.w loop
               Coverage := Radio_Coverage (B.w, Y, X);
               Last := X + 1;
               while Last < B.w and then
                 Radio_Coverage (B.w, Y, Last) = Coverage
               loop
                  Last := Last + 1;
               end loop;
               if Coverage > 0 then
                  Fill_Rect (pc, (B.x + X, B.y + Y, Last - X, 1),
                    Blend (Fill, Background, Coverage));
               end if;
               X := Last;
            end loop;
         end loop;
      end Disc;
   begin
      if Is_Empty (r) then return; end if;
      Disc (box, edge, colors.panel);
      Disc (Content_Rect (box, 1, 1), colors.field, edge);
      if selected then
         Disc (Content_Rect (box, 4, 4), colors.accent, colors.field);
      end if;
      Draw_UI_Text (With_Clip (pc, (r.x + Natural'Min (22, r.w), r.y,
        r.w - Natural'Min (22, r.w), r.h)), r.x + 22, Center_Text_Y (r),
        label, colors.text, colors.panel);
   end Draw_Radio_Button;

   procedure Draw_List_Item
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; label : String)
   is
      bg : Color := colors.field;
      fg : Color := colors.text;
   begin
      if selected then
         bg := colors.selection;
         fg := colors.selectionText;
      elsif hot then
         bg := colors.edge;
      end if;

      Fill_Rect (c, r, bg);
      Draw_UI_Text_Fitted (c, Content_Rect (r, 8, 2), r.x + 8, Center_Text_Y (r), label, fg, bg);
   end Draw_List_Item;

   procedure Draw_Menu_Item
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean; enabled : Boolean;
       label : String)
   is
      bg : Color := colors.panel;
      fg : Color := colors.text;
      icon : constant Rect := (x => r.x + 5, y => r.y + 4, w => 14, h => 14);
   begin
      if not enabled then
         fg := colors.muted;
      elsif active then
         bg := colors.accent;
         fg := colors.edge;
      elsif hot then
         bg := colors.face;
      end if;

      if Is_Empty (r) then return; end if;
      Fill_Rect (c, r, bg);
      if enabled then
         Fill_Rect (With_Clip (c, r), icon, colors.face);
         Stroke_Rect (With_Clip (c, r), icon, Control_Edge (colors), Control_Edge (colors));
      else
         Stroke_Rect (With_Clip (c, r), icon, Control_Edge (colors), Control_Edge (colors));
      end if;
      Draw_UI_Text_Fitted (c, Content_Rect (r, 6, 2), r.x + 30, Center_Text_Y (r), label, fg, bg);
   end Draw_Menu_Item;

   function Layout_Horizontal_Slider
      (r : Rect;
       minValue, maxValue, value : Natural) return Horizontal_Slider_Layout
   is
      result : Horizontal_Slider_Layout;
      thumbWidth : constant Natural := Natural'Min (10, r.w);
      thumbHeight : constant Natural :=
        (if r.h > 6 then r.h - 6 else r.h);
      inset : constant Natural :=
        (if r.w > thumbWidth + 2 then 1 else 0);
      span : Natural := 1;
      pos  : Natural := 0;
      travel : Natural := 0;
      trackY : Natural := r.y;
   begin
      result.minimumThumbX := r.x + inset;
      result.maximumThumbX := result.minimumThumbX;
      if r.w >= thumbWidth + inset then
         result.maximumThumbX := r.x + r.w - thumbWidth - inset;
      end if;
      travel := result.maximumThumbX - result.minimumThumbX;
      if maxValue > minValue then
         span := maxValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      result.thumb :=
        (x => result.minimumThumbX + (pos * travel) / span,
         y => r.y + (if r.h > thumbHeight then (r.h - thumbHeight) / 2 else 0),
         w => thumbWidth,
         h => thumbHeight);
      if r.h >= 4 then
         trackY := r.y + (r.h - 4) / 2;
      end if;
      if thumbWidth > 0 then
         result.track :=
           (x => result.minimumThumbX + thumbWidth / 2,
            y => trackY,
            w => travel + 1,
            h => Natural'Min (4, r.h));
      end if;
      return result;
   end Layout_Horizontal_Slider;

   procedure Draw_Horizontal_Slider
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean)
   is
      layout : constant Horizontal_Slider_Layout :=
        Layout_Horizontal_Slider (r, minValue, maxValue, value);
      fillColor : Color := colors.accent;
   begin
      if active then
         fillColor := colors.good;
      elsif hot then
         fillColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (layout.track) then
         Fill_Rect (c, layout.track, colors.shadow);
         Fill_Rect (c, (x => layout.track.x, y => layout.track.y,
                        w => layout.thumb.x + layout.thumb.w / 2 -
                          layout.track.x + 1,
                        h => layout.track.h),
                    fillColor);
      end if;
      Fill_Rect (c, layout.thumb, colors.face);
      if active then
         Stroke_Sunken (c, layout.thumb, colors);
      else
         Stroke_Raised (c, layout.thumb, colors);
      end if;
   end Draw_Horizontal_Slider;

   function Layout_Vertical_Scrollbar
      (r : Rect;
       minValue, maxValue, value : Natural;
       pageSize : Positive := 1) return Vertical_Scrollbar_Layout
   is
      buttonExtent : constant Natural := Natural'Min (r.w, r.h / 2);
      result : Vertical_Scrollbar_Layout;
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      span : Natural := 1;
      pos : Natural := 0;
      thumbHeight : Natural := 0;
      travel : Natural := 0;
   begin
      result.decrementButton :=
        (x => r.x, y => r.y, w => r.w, h => buttonExtent);
      result.incrementButton :=
        (x => r.x, y => r.y + r.h - buttonExtent,
         w => r.w, h => buttonExtent);
      result.trackFrame :=
        (x => r.x, y => r.y + buttonExtent, w => r.w,
         h => (if r.h > buttonExtent * 2
               then r.h - buttonExtent * 2 else 0));
      result.track := result.trackFrame;
      result.maximumValue :=
        (if shown >= total then minValue else maxValue - shown + 1);

      if result.maximumValue > minValue then
         span := result.maximumValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if not Is_Empty (result.track) and then shown < total then
         thumbHeight := Natural'Max (12, result.track.h * shown / total);
         thumbHeight := Natural'Min (thumbHeight, result.track.h);
         travel := result.track.h - thumbHeight;
         result.thumb :=
           (x => result.track.x,
            y => result.track.y + (pos * travel) / span,
            w => result.track.w, h => thumbHeight);
      end if;
      return result;
   end Layout_Vertical_Scrollbar;

   function Layout_Horizontal_Scrollbar
      (r : Rect;
       minValue, maxValue, value : Natural;
       pageSize : Positive := 1) return Horizontal_Scrollbar_Layout
   is
      buttonExtent : constant Natural := Natural'Min (r.h, r.w / 2);
      result : Horizontal_Scrollbar_Layout;
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      span : Natural := 1;
      pos : Natural := 0;
      thumbWidth : Natural := 0;
      travel : Natural := 0;
   begin
      result.decrementButton :=
        (x => r.x, y => r.y, w => buttonExtent, h => r.h);
      result.incrementButton :=
        (x => r.x + r.w - buttonExtent, y => r.y,
         w => buttonExtent, h => r.h);
      result.trackFrame :=
        (x => r.x + buttonExtent, y => r.y,
         w => (if r.w > buttonExtent * 2
               then r.w - buttonExtent * 2 else 0),
         h => r.h);
      result.track := result.trackFrame;
      result.maximumValue :=
        (if shown >= total then minValue else maxValue - shown + 1);

      if result.maximumValue > minValue then
         span := result.maximumValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if not Is_Empty (result.track) and then shown < total then
         thumbWidth := Natural'Max (12, result.track.w * shown / total);
         thumbWidth := Natural'Min (thumbWidth, result.track.w);
         travel := result.track.w - thumbWidth;
         result.thumb :=
           (x => result.track.x + (pos * travel) / span,
            y => result.track.y,
            w => thumbWidth, h => result.track.h);
      end if;
      return result;
   end Layout_Horizontal_Scrollbar;

   procedure Apply_Wheel_Scroll
      (value : in out Natural;
       minValue, maxValue : Natural;
       wheelDelta : Integer;
       step : Positive := 3)
   is
   begin
      if maxValue <= minValue then
         value := minValue;
      elsif value < minValue then
         value := minValue;
      elsif value > maxValue then
         value := maxValue;
      elsif wheelDelta > 0 then
         value :=
           (if value - minValue > step then value - step else minValue);
      elsif wheelDelta < 0 then
         value :=
           (if maxValue - value > step then value + step else maxValue);
      end if;
   end Apply_Wheel_Scroll;

   procedure Draw_Arrow_Button
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style;
       direction : Arrow_Direction)
   is
      pc : constant Canvas := With_Clip (c, r);
      offset : constant Natural := (if style = Button_Pressed then 1 else 0);
      ink : constant Color := (if style = Button_Disabled then colors.muted else colors.text);
      cx, cy : Natural;
   begin
      Draw_Button_Frame (pc, r, colors, style);
      if r.w < 8 or else r.h < 8 then return; end if;
      --  Flat disabled frames need no bevel compensation.
      --  Pressed glyphs move with the recessed face.
      cx := r.x + r.w / 2 - (if style = Button_Disabled then 0 else 1) + offset;
      cy := r.y + r.h / 2 + offset;
      for step in 0 .. 3 loop
         case direction is
            when Arrow_Up | Arrow_Down =>
               declare
                  half : constant Natural := (if direction = Arrow_Up then step else 3 - step);
               begin
                  Fill_Rect (pc, (cx - half, cy - 2 + step, 1 + 2 * half, 1), ink);
               end;
            when Arrow_Left | Arrow_Right =>
               Fill_Rect (pc,
                 ((if direction = Arrow_Left then cx - 2 + step else cx + 2 - step),
                  cy - step, 1, 1 + 2 * step), ink);
         end case;
      end loop;
   end Draw_Arrow_Button;

   procedure Draw_Vertical_Scrollbar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean; pageSize : Positive := 1;
       pressedPart : Scrollbar_Part := Scrollbar_Thumb)
   is
      layout : constant Vertical_Scrollbar_Layout :=
        Layout_Vertical_Scrollbar
          (r, minValue, maxValue, value, pageSize);
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      knobColor : Color := colors.face;
      canDecrement : constant Boolean :=
        shown < total and then value > minValue;
      canIncrement : constant Boolean :=
        shown < total and then value < layout.maximumValue;


   begin
      if active then
         knobColor := Blend (colors.edge, colors.face, 64);
      elsif hot then
         knobColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (layout.trackFrame) then
         Fill_Rect (c, layout.trackFrame, Blend (colors.shadow, colors.panel, 48));
         Stroke_Rect (c, layout.trackFrame, colors.shadow, colors.shadow);
      end if;
      Draw_Arrow_Button
        (c, layout.decrementButton, colors,
         (if not canDecrement then Button_Disabled
          elsif active and then pressedPart = Scrollbar_Decrement then Button_Pressed
          else Button_Normal), Arrow_Up);
      Draw_Arrow_Button
        (c, layout.incrementButton, colors,
         (if not canIncrement then Button_Disabled
          elsif active and then pressedPart = Scrollbar_Increment then Button_Pressed
          else Button_Normal), Arrow_Down);

      if not Is_Empty (layout.thumb) then
         Fill_Rect (c, layout.thumb, knobColor);
         Stroke_Raised (c, layout.thumb, colors);
      end if;
   end Draw_Vertical_Scrollbar;

   procedure Draw_Horizontal_Scrollbar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean; pageSize : Positive := 1;
       pressedPart : Scrollbar_Part := Scrollbar_Thumb)
   is
      layout : constant Horizontal_Scrollbar_Layout :=
        Layout_Horizontal_Scrollbar
          (r, minValue, maxValue, value, pageSize);
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      knobColor : Color := colors.face;
      canDecrement : constant Boolean :=
        shown < total and then value > minValue;
      canIncrement : constant Boolean :=
        shown < total and then value < layout.maximumValue;


   begin
      if active then
         knobColor := Blend (colors.edge, colors.face, 64);
      elsif hot then
         knobColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (layout.trackFrame) then
         Fill_Rect (c, layout.trackFrame, Blend (colors.shadow, colors.panel, 48));
         Stroke_Rect (c, layout.trackFrame, colors.shadow, colors.shadow);
      end if;
      Draw_Arrow_Button
        (c, layout.decrementButton, colors,
         (if not canDecrement then Button_Disabled
          elsif active and then pressedPart = Scrollbar_Decrement then Button_Pressed
          else Button_Normal), Arrow_Left);
      Draw_Arrow_Button
        (c, layout.incrementButton, colors,
         (if not canIncrement then Button_Disabled
          elsif active and then pressedPart = Scrollbar_Increment then Button_Pressed
          else Button_Normal), Arrow_Right);

      if not Is_Empty (layout.thumb) then
         Fill_Rect (c, layout.thumb, knobColor);
         Stroke_Raised (c, layout.thumb, colors);
      end if;
   end Draw_Horizontal_Scrollbar;

   function Button
      (bounds : Rect; pointer : Pointer_State) return Widget_Result
   is
      hot : constant Boolean :=
         pointer.enabled and then Point_In_Rect (pointer.x, pointer.y, bounds);
   begin
      return
        (hot       => hot,
         active    => hot and then pointer.down,
         activated => hot and then pointer.released);
   end Button;
end CuBit.UI;
