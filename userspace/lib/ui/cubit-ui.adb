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
with Client_Glyphs;
with Client_Glyph_Blend;

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

   function With_Clip (c : Canvas; clip : Rect) return Canvas is
      ret : Canvas := c;
   begin
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
      pairFill : constant Unsigned_64 :=
         Shift_Left (Unsigned_64 (fill), 32) or Unsigned_64 (fill);
      startX : Natural;
      endX : Natural;
      offset : Storage_Offset;
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         startX := clipped.x;
         endX := clipped.x + clipped.w;

         if startX < endX and then startX mod 2 /= 0 then
            declare
               offset : constant Storage_Offset :=
                  Storage_Offset (yy * c.pitch + startX * 4);
               pixel : Color with Import, Address => c.addr + offset;
            begin
               pixel := fill;
            end;
            startX := startX + 1;
         end if;

         while startX + 1 < endX loop
            offset := Storage_Offset (yy * c.pitch + startX * 4);
            declare
               pixels : Unsigned_64 with Import, Address => c.addr + offset;
            begin
               pixels := pairFill;
            end;
            startX := startX + 2;
         end loop;

         if startX < endX then
            offset := Storage_Offset (yy * c.pitch + startX * 4);
            declare
               pixel : Color with Import, Address => c.addr + offset;
            begin
               pixel := fill;
            end;
         end if;
      end loop;
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

   procedure Draw_Bitmap
     (c : Canvas; x, y : Natural; pixels : ARGB_Bitmap;
      enabled : Boolean := True)
   is
      clipped : constant Rect := Raster_Rect
        (c, (x => x, y => y, w => pixels'Length (2), h => pixels'Length (1)));
      source : Color;
      alpha : Unsigned_8;
      gray : Unsigned_32;
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
                  gray :=
                    (77 * (Shift_Right (source, 16) and 16#FF#) +
                     150 * (Shift_Right (source, 8) and 16#FF#) +
                     29 * (source and 16#FF#) + 128) / 256;
                  source := Shift_Left (gray, 16) or Shift_Left (gray, 8) or gray;
                  alpha := Unsigned_8 (Unsigned_32 (alpha) * 112 / 255);
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

   function UI_Text_Width (text : String) return Natural is
      width : Natural := 0;
   begin
      for i in text'Range loop
         width := width + CuBit.Fonts.Width (CuBit.Fonts.Sans, text (i));
      end loop;
      return width;
   end UI_Text_Width;

   function UI_Text_Height return Natural is
   begin
      return CuBit.Fonts.Line_Height;
   end UI_Text_Height;

   Density_Fonts : Client_Glyphs.State;
   -- Pointer access is the narrow bridge: Read pins the immutable A8 mask,
   -- this synchronous blend finishes all reads before returning its lease.
   procedure Draw_Density_Glyph
     (c : Canvas; x, y : Natural; ch : Character; face : CuBit.Fonts.Face; fg : Color)
   is
      package G renames Client_Canvas_Geometry;
      package F renames Client_Glyphs;
      Key : F.C.Key;
      View : F.View;
      Layout : F.L.Layout;
      Clipped : constant Rect := Raster_Rect
        (c, (x, y, CuBit.Fonts.Max_Width, CuBit.Fonts.Line_Height));
      Left, Top, Right, Bottom : Natural;
   begin
      if c.addr = System.Null_Address or else Is_Empty (Clipped) then return; end if;
      Key := (CuBit.Fonts.Face'Pos (face),
        (if Character'Pos (ch) in 32 .. 126 then Character'Pos (ch) else 63),
        (F.L.G.Scale_Component (c.densityNumerator), F.L.G.Scale_Component (c.densityDenominator)));
      F.Read (Density_Fonts, Key, View);
      if not F.Ready (View) then return; end if;
      Layout := F.Raster (View);
      Left := G.Relative (c.originX, x, c.densityNumerator, c.densityDenominator);
      Top := G.Relative (c.originY, y, c.densityNumerator, c.densityDenominator);
      Right := Natural'Min (Left + Layout.Width, Clipped.x + Clipped.w);
      Bottom := Natural'Min (Top + Layout.Height, Clipped.y + Clipped.h);
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
            Target_Bytes : Integer_Address;
         begin
            if Clipped.x >= Right or else Clipped.y >= Bottom or else
              Clipped.x < Left or else Clipped.y < Top or else
              c.pitch = 0 or else c.pitch mod 4 /= 0 or else
              Target_Address mod 4 /= 0
            then return; end if;
            Pitch := c.pitch / 4;
            if Right > Pitch or else Right > Natural'Last / 4 or else
              Bottom - 1 > (Natural'Last / 4 - Right) / Pitch
            then return; end if;
            Length := (Bottom - 1) * Pitch + Right;
            Source := (Clipped.x - Left, Clipped.y - Top,
                       Right - Clipped.x, Bottom - Clipped.y);
            Destination := (Clipped.x, Clipped.y, Source.Width, Source.Height);
            if not B.Fits (Layout.Bytes, Layout.Pitch, Source) or else
              not B.Fits (Length, Pitch, Destination)
            then return; end if;
            Target_Bytes := Integer_Address (Length) * 4;
            if Mask_Address = 0 or else
              Mask_Address > Integer_Address'Last - Integer_Address (Layout.Bytes) or else
              Target_Address > Integer_Address'Last - Target_Bytes
            then return; end if;
            if Mask_Address < Target_Address + Target_Bytes and then
              Target_Address < Mask_Address + Integer_Address (Layout.Bytes)
            then return; end if;
            declare
               Mask : B.Bytes (0 .. Layout.Bytes - 1)
                 with Import, Address => F.Pixels (View);
               Target : B.Pixels (0 .. Length - 1)
                 with Import, Address => c.addr;
            begin
               B.Paint (Mask, Target, Layout.Pitch, Pitch, Source, Destination, fg);
            end;
         end Paint_View;
      begin
         Paint_View;
      end;
      F.Finish (Density_Fonts, View);
   end Draw_Density_Glyph;

   procedure Draw_UI_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Sans, ch);
      width : Natural;
      alpha : Unsigned_8;
      clipped : Rect;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      width := Natural (glyph.Advance);
      clipped := Clamp_Rect
        (c, (x => x, y => y, w => width, h => CuBit.Fonts.Line_Height));

      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      Fill_Rect (c, (x => x, y => y, w => width, h => CuBit.Fonts.Line_Height), bg);
      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         srcY := yy - y;
         for xx in clipped.x .. clipped.x + clipped.w - 1 loop
            srcX := xx - x;
            alpha := glyph.Alpha (srcY, srcX);
            if alpha = 255 then
               offset := Storage_Offset (yy * c.pitch + xx * 4);
               declare
                  pixel : Color with Import, Address => c.addr + offset;
               begin
                  pixel := fg;
               end;
            elsif alpha > 0 then
               offset := Storage_Offset (yy * c.pitch + xx * 4);
               declare
                  pixel : Color with Import, Address => c.addr + offset;
               begin
                  pixel := Blend (fg, bg, alpha);
               end;
            end if;
         end loop;
      end loop;
   end Draw_UI_Glyph;

   procedure Draw_UI_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color)
   is
      cx : Natural := x;
      width : constant Natural := UI_Text_Width (text);
   begin
      if c.densityNumerator /= c.densityDenominator then
         Fill_Rect (c, (x, y, width, CuBit.Fonts.Line_Height), bg);
         for i in text'Range loop
            exit when cx >= c.width;
            Draw_Density_Glyph (c, cx, y, text (i), CuBit.Fonts.Sans, fg);
            cx := cx + CuBit.Fonts.Width (CuBit.Fonts.Sans, text (i));
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
             cx + CuBit.Fonts.Max_Width > c.clip.x)
         then
            Draw_UI_Glyph (c, cx, y, text (i), fg, bg);
         end if;

         cx := cx + CuBit.Fonts.Width (CuBit.Fonts.Sans, text (i));
      end loop;
   end Draw_UI_Text;

   procedure Draw_UI_Text_Transparent
      (c : Canvas; x, y : Natural; text : String; fg : Color)
   is
      cx : Natural := x;
      width : constant Natural := UI_Text_Width (text);
      glyph : CuBit.Fonts.Glyph_Access;
      glyphWidth : Natural;
      clipped : Rect;
      alpha : Unsigned_8;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      if c.densityNumerator /= c.densityDenominator then
         for i in text'Range loop
            exit when cx >= c.width;
            Draw_Density_Glyph (c, cx, y, text (i), CuBit.Fonts.Sans, fg);
            cx := cx + CuBit.Fonts.Width (CuBit.Fonts.Sans, text (i));
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
         glyph := CuBit.Fonts.Get (CuBit.Fonts.Sans, text (i));
         glyphWidth := Natural (glyph.Advance);
         clipped := Clamp_Rect
           (c, (x => cx, y => y, w => glyphWidth,
                h => CuBit.Fonts.Line_Height));
         if c.addr /= System.Null_Address and then not Is_Empty (clipped) then
            for yy in clipped.y .. clipped.y + clipped.h - 1 loop
               srcY := yy - y;
               for xx in clipped.x .. clipped.x + clipped.w - 1 loop
                  srcX := xx - cx;
                  alpha := glyph.Alpha (srcY, srcX);
                  if alpha > 0 then
                     offset := Storage_Offset (yy * c.pitch + xx * 4);
                     declare
                        pixel : Color with Import, Address => c.addr + offset;
                     begin
                        pixel :=
                          (if alpha = 255 then fg
                           else Blend (fg, pixel, alpha));
                     end;
                  end if;
               end loop;
            end loop;
         end if;
         cx := cx + glyphWidth;
      end loop;
   end Draw_UI_Text_Transparent;

   function Code_Text_Width (text : String) return Natural is
     (text'Length * CuBit.Fonts.Mono_Width);

   function Code_Text_Height return Natural is
     (CuBit.Fonts.Line_Height);

   procedure Draw_Code_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      glyph : constant CuBit.Fonts.Glyph_Access := CuBit.Fonts.Get (CuBit.Fonts.Monospace, ch);
      alpha : Unsigned_8;
      clipped : Rect;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      clipped := Clamp_Rect
        (c, (x => x, y => y, w => CuBit.Fonts.Mono_Width,
             h => CuBit.Fonts.Line_Height));
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;
      Fill_Rect
        (c, (x => x, y => y, w => CuBit.Fonts.Mono_Width,
             h => CuBit.Fonts.Line_Height), bg);
      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         srcY := yy - y;
         for xx in clipped.x .. clipped.x + clipped.w - 1 loop
            srcX := xx - x;
            alpha := glyph.Alpha (srcY, srcX);
            if alpha > 0 then
               offset := Storage_Offset (yy * c.pitch + xx * 4);
               declare
                  pixel : Color with Import, Address => c.addr + offset;
               begin
                  pixel :=
                    (if alpha = 255 then fg else Blend (fg, bg, alpha));
               end;
            end if;
         end loop;
      end loop;
   end Draw_Code_Glyph;

   procedure Draw_Code_Text
      (c : Canvas; x, y : Natural; text : String; fg, bg : Color)
   is
      cx : Natural := x;
      width : constant Natural := Code_Text_Width (text);
   begin
      if c.densityNumerator /= c.densityDenominator then
         Fill_Rect (c, (x, y, width, CuBit.Fonts.Line_Height), bg);
         for i in text'Range loop
            exit when cx >= c.width;
            Draw_Density_Glyph (c, cx, y, text (i), CuBit.Fonts.Monospace, fg);
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
           when Button_Active => colors.accent,
           when Button_Normal => Blend (colors.highlight, colors.face, 35));

      procedure Draw_Button_Frame
         (c : Canvas; r : Rect; colors : Theme; style : Button_Style)
      is
         border : constant Color :=
           (case style is
              when Button_Hot => Blend (colors.accent, colors.face, 130),
              when Button_Pressed | Button_Active => colors.accent,
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
         textW : constant Natural := UI_Text_Width (label);
         tx : Natural := r.x + 6;
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
            fg := colors.face;
         end if;

         Draw_UI_Text (With_Clip (c, Content_Rect (r, 6, 2)), tx, ty, label, fg,
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
         clipped : constant Canvas := With_Clip (c, Content_Rect (r, 8, 2));
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
         Draw_UI_Text (clipped, r.x + 10, ty, label, fg, bg);
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

   procedure Draw_Menu_Surface (c : Canvas; r : Rect; colors : Theme) is
   begin
      Fill_Vertical_Gradient (c, r,
        Blend (colors.highlight, colors.panel, 70),
        Blend (colors.shadow, colors.panel, 28));
   end Draw_Menu_Surface;

   procedure Draw_Menu_Bar (c : Canvas; r : Rect; colors : Theme) is
   begin
      if Is_Empty (r) then return; end if;
      Draw_Menu_Surface (c, r, colors);
      Stroke_Rect (c, r, colors.highlight, colors.shadow);
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
      Fill_Rect (c, (r.x, r.y, r.w, 1), colors.highlight);
      Fill_Rect (c, (r.x, r.y + r.h - 1, r.w, 1), colors.shadow);
      if active or hot then
         Fill_Rect (c, inner, bg);
         Stroke_Rect (c, inner, colors.shadow, colors.shadow);
      end if;
      Draw_UI_Text_Transparent (With_Clip (c, Content_Rect (r, 8, 2)),
        r.x + 10, Center_Text_Y (r), label, fg);
   end Draw_Menu_Title;

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
      Draw_UI_Text (With_Clip (c, leftBounds), leftBounds.x, Center_Text_Y (r),
        left, colors.text, colors.panel);
      if rightWidth > 0 then
         Draw_UI_Text (With_Clip (c, rightBounds), rightBounds.x, Center_Text_Y (r),
           right, colors.muted, colors.panel);
      end if;
   end Draw_Status_Bar;

   procedure Draw_Pane
      (c : Canvas; r : Rect; colors : Theme; title : String)
   is
      titleW : constant Natural := UI_Text_Width (title);
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
      if title'Length > 0 then
         Fill_Rect (With_Clip (c, Content_Rect (r, 8, 0)), titleRect, colors.panel);
         Draw_UI_Text (With_Clip (c, Content_Rect (r, 12, 0)), titleRect.x + 4, titleRect.y,
                       title, colors.muted, colors.panel);
      end if;
   end Draw_Pane;

   procedure Draw_Table_Viewport
      (c : Canvas; r : Rect; colors : Theme)
   is
   begin
      Fill_Rect (c, r, colors.field);
      Stroke_Sunken (c, r, colors);
   end Draw_Table_Viewport;

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
      Draw_UI_Text
        (With_Clip (c, Content_Rect (first, layout.Cell_Padding, 2)), first.x + layout.Cell_Padding,
         labelY, c1, colors.text, colors.panel);
      Draw_UI_Text
        (With_Clip (c, Content_Rect (second, layout.Cell_Padding, 2)), second.x + layout.Cell_Padding,
         labelY, c2, colors.text, colors.panel);
      Draw_UI_Text
        (With_Clip (c, Content_Rect (third, layout.Cell_Padding, 2)), third.x + layout.Cell_Padding,
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
         tc : constant Canvas := With_Clip (c, Content_Rect (cell, layout.Cell_Padding, 1));
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
            Draw_Code_Text
              (tc, cell.x + layout.Cell_Padding, primaryY, value, fg, bg);
            if detail'Length > 0 then
               Draw_Code_Text
                 (tc, cell.x + layout.Cell_Padding,
                  primaryY + Code_Text_Height, detail,
                  detailForeground, bg);
            end if;
         else
            Draw_UI_Text
              (tc, cell.x + layout.Cell_Padding, primaryY,
               value, fg, bg);
            if detail'Length > 0 then
               Draw_UI_Text
                 (tc, cell.x + layout.Cell_Padding,
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
      Draw_UI_Text (With_Clip (c, r), r.x + 36, Center_Text_Y (r), label, colors.text, colors.panel);
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
       focused : Boolean; hot : Boolean; suggestion : String := "")
   is
      face : constant Color := colors.field;
      textX : Natural := r.x + 8;
      textY : Natural := r.y;
      cursorX : Natural := textX;
      charW : Natural;
      fg : Color;
      bg : Color;
      caret : Rect;
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

      for i in text'Range loop
         if focused and then
            Natural (i - text'First) >= selectionStart and then
            Natural (i - text'First) < selectionEnd
         then
            fg := colors.selectionText;
            bg := colors.selection;
         else
            fg := colors.text;
            bg := face;
         end if;

         if cursor = Natural (i - text'First) then
            cursorX := textX;
         end if;

         Draw_UI_Text (textCanvas, textX, textY, text (i .. i), fg, bg);
         charW := UI_Text_Width (text (i .. i));
         textX := textX + charW;
      end loop;

      if cursor >= text'Length then
         cursorX := textX;
      end if;

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
      Draw_UI_Text (With_Clip (c, Content_Rect (r, 8, 2)), r.x + 8, Center_Text_Y (r), label, fg, bg);
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
      Draw_UI_Text (With_Clip (c, Content_Rect (r, 6, 2)), r.x + 30, Center_Text_Y (r), label, fg, bg);
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
