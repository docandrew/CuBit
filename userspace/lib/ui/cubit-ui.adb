------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Font8x16;
with CuBit.UI.Fonts_IBM_Plex_Mono_11;
with CuBit.UI.Fonts_IBM_Plex_Sans_11;

package body CuBit.UI is
   use type System.Address;
   package UI_Font renames CuBit.UI.Fonts_IBM_Plex_Sans_11;
   package Code_Font renames CuBit.UI.Fonts_IBM_Plex_Mono_11;

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
      maxX : Natural := r.x + r.w;
      maxY : Natural := r.y + r.h;
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
         if maxX > c.clip.x + c.clip.w then
            maxX := c.clip.x + c.clip.w;
         end if;
         if maxY > c.clip.y + c.clip.h then
            maxY := c.clip.y + c.clip.h;
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
      offset : constant Storage_Offset := Storage_Offset (y * c.pitch + x * 4);
      pixel : Color with Import, Address => c.addr + offset;
   begin
      if c.addr /= System.Null_Address and then
         x < c.width and then y < c.height and then
         (not c.clipEnabled or else Point_In_Rect (x, y, c.clip))
      then
         pixel := fill;
      end if;
   end Set_Pixel;

   procedure Fill_Rect (c : Canvas; r : Rect; fill : Color) is
      clipped : constant Rect := Clamp_Rect (c, r);
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

   procedure Stroke_Sunken (c : Canvas; r : Rect; colors : Theme) is
   begin
      Stroke_Rect (c, r, colors.darkShadow, colors.highlight);
      if r.w > 3 and then r.h > 3 then
         Stroke_Rect
           (c, (x => r.x + 1, y => r.y + 1, w => r.w - 2, h => r.h - 2),
            colors.shadow, colors.edge);
      end if;
   end Stroke_Sunken;

   procedure Stroke_Raised (c : Canvas; r : Rect; colors : Theme) is
   begin
      Stroke_Rect (c, r, colors.highlight, colors.darkShadow);
      if r.w > 3 and then r.h > 3 then
         Stroke_Rect
           (c, (x => r.x + 1, y => r.y + 1, w => r.w - 2, h => r.h - 2),
            colors.edge, colors.shadow);
      end if;
   end Stroke_Raised;

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
        Clamp_Rect (c, (x => x, y => y,
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
         row := yy - y;
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for xx in clipped.x .. clipped.x + clipped.w - 1 loop
               bit := xx - x;
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
      code  : Natural;
   begin
      for i in text'Range loop
         code := Character'Pos (text (i));
         if code >= UI_Font.FIRST_GLYPH and then code <= UI_Font.LAST_GLYPH then
            width := width + UI_Font.Widths (code);
         else
            width := width + UI_Font.Widths (Character'Pos ('?'));
         end if;
      end loop;
      return width;
   end UI_Text_Width;

   function UI_Text_Height return Natural is
   begin
      return UI_Font.LINE_HEIGHT;
   end UI_Text_Height;

   procedure Draw_UI_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      code : Natural := Character'Pos (ch);
      width : Natural;
      alpha : Unsigned_8;
      clipped : Rect;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      if code < UI_Font.FIRST_GLYPH or else code > UI_Font.LAST_GLYPH then
         code := Character'Pos ('?');
      end if;

      width := UI_Font.Widths (code);
      clipped := Clamp_Rect
        (c, (x => x, y => y, w => width, h => UI_Font.LINE_HEIGHT));

      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      Fill_Rect (c, (x => x, y => y, w => width, h => UI_Font.LINE_HEIGHT), bg);
      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         srcY := yy - y;
         for xx in clipped.x .. clipped.x + clipped.w - 1 loop
            srcX := xx - x;
            alpha := UI_Font.Alpha (code) (srcY) (srcX);
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
      if c.clipEnabled and then
         (text'Length = 0 or else
          x >= c.clip.x + c.clip.w or else
          x + width <= c.clip.x or else
          y >= c.clip.y + c.clip.h or else
          y + UI_Font.LINE_HEIGHT <= c.clip.y)
      then
         return;
      end if;

      for i in text'Range loop
         exit when cx >= c.width;
         if not c.clipEnabled or else
            (cx < c.clip.x + c.clip.w and then
             cx + UI_Font.MAX_GLYPH_WIDTH > c.clip.x)
         then
            Draw_UI_Glyph (c, cx, y, text (i), fg, bg);
         end if;

         declare
            code : Natural := Character'Pos (text (i));
         begin
            if code < UI_Font.FIRST_GLYPH or else code > UI_Font.LAST_GLYPH then
               code := Character'Pos ('?');
            end if;
            cx := cx + UI_Font.Widths (code);
         end;
      end loop;
   end Draw_UI_Text;

   procedure Draw_UI_Text_Transparent
      (c : Canvas; x, y : Natural; text : String; fg : Color)
   is
      cx : Natural := x;
      width : constant Natural := UI_Text_Width (text);
      code : Natural;
      glyphWidth : Natural;
      clipped : Rect;
      alpha : Unsigned_8;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      if c.clipEnabled and then
        (text'Length = 0 or else
         x >= c.clip.x + c.clip.w or else
         x + width <= c.clip.x or else
         y >= c.clip.y + c.clip.h or else
         y + UI_Font.LINE_HEIGHT <= c.clip.y)
      then
         return;
      end if;

      for i in text'Range loop
         exit when cx >= c.width;
         code := Character'Pos (text (i));
         if code < UI_Font.FIRST_GLYPH or else code > UI_Font.LAST_GLYPH then
            code := Character'Pos ('?');
         end if;
         glyphWidth := UI_Font.Widths (code);
         clipped := Clamp_Rect
           (c, (x => cx, y => y, w => glyphWidth,
                h => UI_Font.LINE_HEIGHT));
         if c.addr /= System.Null_Address and then not Is_Empty (clipped) then
            for yy in clipped.y .. clipped.y + clipped.h - 1 loop
               srcY := yy - y;
               for xx in clipped.x .. clipped.x + clipped.w - 1 loop
                  srcX := xx - cx;
                  alpha := UI_Font.Alpha (code) (srcY) (srcX);
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
     (text'Length * Code_Font.GLYPH_WIDTH);

   function Code_Text_Height return Natural is
     (Code_Font.LINE_HEIGHT);

   procedure Draw_Code_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      code : Natural := Character'Pos (ch);
      alpha : Unsigned_8;
      clipped : Rect;
      offset : Storage_Offset;
      srcX : Natural;
      srcY : Natural;
   begin
      if code < Code_Font.FIRST_GLYPH or else
        code > Code_Font.LAST_GLYPH
      then
         code := Character'Pos ('?');
      end if;
      clipped := Clamp_Rect
        (c, (x => x, y => y, w => Code_Font.GLYPH_WIDTH,
             h => Code_Font.LINE_HEIGHT));
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;
      Fill_Rect
        (c, (x => x, y => y, w => Code_Font.GLYPH_WIDTH,
             h => Code_Font.LINE_HEIGHT), bg);
      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         srcY := yy - y;
         for xx in clipped.x .. clipped.x + clipped.w - 1 loop
            srcX := xx - x;
            alpha := Code_Font.Alpha (code) (srcY) (srcX);
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
      if c.clipEnabled and then
        (text'Length = 0 or else
         x >= c.clip.x + c.clip.w or else
         x + width <= c.clip.x or else
         y >= c.clip.y + c.clip.h or else
         y + Code_Font.LINE_HEIGHT <= c.clip.y)
      then
         return;
      end if;
      for i in text'Range loop
         exit when cx >= c.width;
         if not c.clipEnabled or else
           (cx < c.clip.x + c.clip.w and then
            cx + Code_Font.GLYPH_WIDTH > c.clip.x)
         then
            Draw_Code_Glyph (c, cx, y, text (i), fg, bg);
         end if;
         cx := cx + Code_Font.GLYPH_WIDTH;
      end loop;
   end Draw_Code_Text;

   procedure Draw_Button_Frame
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style)
   is
      face : Color := colors.face;
      border : Color := colors.edge;
   begin
      case style is
         when Button_Hot =>
            face := colors.panel;
            border := colors.accent;
         when Button_Pressed =>
            face := colors.edge;
            border := colors.accent;
         when Button_Disabled =>
            face := colors.panel;
            border := colors.edge;
         when Button_Active =>
            face := colors.accent;
            border := colors.accent;
         when Button_Normal =>
            null;
      end case;

      Fill_Rect (c, r, face);
      if style = Button_Pressed or else style = Button_Active then
         Stroke_Sunken (c, r, colors);
      elsif style = Button_Hot then
         Stroke_Rect (c, r, border, colors.shadow);
      else
         Stroke_Raised (c, r, colors);
      end if;
   end Draw_Button_Frame;

   function Button_Face (colors : Theme; style : Button_Style) return Color is
   begin
      case style is
         when Button_Hot =>
            return colors.panel;
         when Button_Disabled =>
            return colors.panel;
         when Button_Active =>
            return colors.accent;
         when Button_Pressed =>
            return colors.edge;
         when Button_Normal =>
            return colors.face;
      end case;
   end Button_Face;

   procedure Draw_Button
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style;
       label : String)
   is
      textW : constant Natural := UI_Text_Width (label);
      tx : Natural := r.x + 4;
      ty : Natural := r.y;
      fg : Color := colors.text;
   begin
      Draw_Button_Frame (c, r, colors, style);

      if r.w > textW then
         tx := r.x + (r.w - textW) / 2;
      end if;
      ty := Center_Text_Y (r);
      if style = Button_Pressed and then r.w > 2 and then r.h > 2 then
         tx := tx + 1;
         ty := ty + 1;
      end if;
      if style = Button_Disabled then
         fg := colors.muted;
      elsif style = Button_Active then
         fg := colors.face;
      end if;

      Draw_UI_Text (c, tx, ty, label, fg, Button_Face (colors, style));
   end Draw_Button;

   procedure Draw_Menu_Bar
      (c : Canvas; r : Rect; colors : Theme)
   is
      bottom : constant Rect :=
        (x => r.x, y => r.y + r.h - 1, w => r.w, h => 1);
   begin
      Fill_Rect (c, r, colors.panel);
      if r.h > 0 then
         Fill_Rect (c, bottom, colors.shadow);
      end if;
   end Draw_Menu_Bar;

   procedure Draw_Menu_Title
      (c : Canvas; r : Rect; colors : Theme;
       hot : Boolean; active : Boolean; label : String)
   is
      bg : Color := colors.panel;
      fg : Color := colors.text;
      tx : constant Natural := r.x + 8;
      ty : Natural := r.y;
   begin
      if active then
         bg := colors.accent;
         fg := colors.face;
      elsif hot then
         bg := colors.face;
      end if;

      Fill_Rect (c, r, bg);
      if active or else hot then
         Stroke_Rect (c, r, colors.accent, colors.shadow);
      end if;
      ty := Center_Text_Y (r);
      Draw_UI_Text (c, tx, ty, label, fg, bg);
   end Draw_Menu_Title;

   procedure Draw_Status_Bar
      (c : Canvas; r : Rect; colors : Theme; left, right : String)
   is
      leftPane : Rect :=
        (x => r.x + 3, y => r.y + 3,
         w => (if r.w > 156 then r.w - 150 else r.w),
         h => (if r.h > 6 then r.h - 6 else r.h));
      rightPane : Rect :=
        (x => r.x, y => r.y + 3,
         w => 137, h => (if r.h > 6 then r.h - 6 else r.h));
      rightX : Natural := rightPane.x + 5;
      rightW : constant Natural := UI_Text_Width (right);
      leftText : Rect;
      rightText : Rect;
      leftCanvas : Canvas;
      rightCanvas : Canvas;
   begin
      Fill_Rect (c, r, colors.panel);
      Fill_Rect (c, (x => r.x, y => r.y, w => r.w, h => 1), colors.edge);

      if r.w <= 156 then
         leftPane.w := (if r.w > 6 then r.w - 6 else r.w);
         rightPane := (others => 0);
      else
         rightPane.x := r.x + r.w - 140;
      end if;

      Fill_Rect (c, leftPane, colors.face);
      Stroke_Sunken (c, leftPane, colors);
      leftText :=
        (x => leftPane.x + 7,
         y => leftPane.y + 2,
         w => (if leftPane.w > 14 then leftPane.w - 14 else 0),
         h => (if leftPane.h > 4 then leftPane.h - 4 else 0));
      leftCanvas := With_Clip (c, leftText);
      Draw_UI_Text (leftCanvas, leftText.x, Center_Text_Y (leftPane),
                    left, colors.text, colors.face);

      if not Is_Empty (rightPane) then
         Fill_Rect (c, rightPane, colors.face);
         Stroke_Sunken (c, rightPane, colors);
         rightText :=
           (x => rightPane.x + 7,
            y => rightPane.y + 2,
            w => (if rightPane.w > 14 then rightPane.w - 14 else 0),
            h => (if rightPane.h > 4 then rightPane.h - 4 else 0));
         rightCanvas := With_Clip (c, rightText);
         if rightText.w > rightW then
            rightX := rightText.x + rightText.w - rightW;
         else
            rightX := rightText.x;
         end if;
         Draw_UI_Text (rightCanvas, rightX, Center_Text_Y (rightPane),
                       right, colors.muted, colors.face);
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
      Fill_Rect (c, r, colors.panel);
      Stroke_Rect (c, frame, colors.shadow, colors.highlight);
      if title'Length > 0 then
         Fill_Rect (c, titleRect, colors.panel);
         Draw_UI_Text (c, titleRect.x + 4, titleRect.y,
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
      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (first) then Stroke_Raised (c, first, colors); end if;
      if not Is_Empty (second) then Stroke_Raised (c, second, colors); end if;
      if not Is_Empty (third) then Stroke_Raised (c, third, colors); end if;
      Draw_UI_Text
        (With_Clip (c, first), first.x + layout.Cell_Padding,
         labelY, c1, colors.text, colors.panel);
      Draw_UI_Text
        (With_Clip (c, second), second.x + layout.Cell_Padding,
         labelY, c2, colors.text, colors.panel);
      Draw_UI_Text
        (With_Clip (c, third), third.x + layout.Cell_Padding,
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
         tc : constant Canvas := With_Clip (c, cell);
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
      if selected then
         bg := colors.accent;
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
            colors.shadow);
      end if;
   end Draw_Tab_Strip;

   procedure Draw_Tab
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean;
       label : String)
   is
      bg : Color := colors.panel;
      fg : Color := colors.text;
      ty : Natural := r.y;
   begin
      if selected then
         bg := colors.face;
      elsif hot then
         bg := colors.edge;
      end if;
      if active then
         bg := colors.shadow;
         fg := colors.face;
      end if;

      Fill_Rect (c, r, bg);
      if selected then
         Stroke_Rect (c, r, colors.shadow, colors.shadow);
      else
         Stroke_Rect (c, r, colors.panel, colors.shadow);
      end if;
      ty := Center_Text_Y (r);
      Draw_UI_Text (c, r.x + 10, ty, label, fg, bg);
      if selected and then r.h > 0 then
         Fill_Rect
           (c, (x => r.x + 1, y => r.y + r.h - 1,
                w => (if r.w > 2 then r.w - 2 else r.w), h => 1),
            colors.face);
      end if;
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
      swatch : constant Rect := (x => r.x, y => r.y, w => 28, h => r.h);
   begin
      Fill_Rect (c, swatch, fill);
      Stroke_Rect (c, swatch, colors.edge, colors.shadow);
      Draw_UI_Text (c, r.x + 36, r.y + 1, label, colors.text, colors.panel);
   end Draw_Swatch;

   procedure Draw_Text_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       focused : Boolean; hot : Boolean)
   is
      face : Color := colors.field;
      textX : constant Natural := r.x + 8;
      textY : Natural := r.y;
      cursorX : Natural := textX + UI_Text_Width (text);
      cursor : Rect;
      textCanvas : constant Canvas := With_Clip
        (c, (x => r.x + 3, y => r.y + 2,
             w => (if r.w > 6 then r.w - 6 else 0),
             h => (if r.h > 4 then r.h - 4 else 0)));
   begin
      if hot then
         face := colors.face;
      end if;

      Fill_Rect (c, r, face);
      if focused then
         Stroke_Rect (c, r, colors.accent, colors.shadow);
      else
         Stroke_Sunken (c, r, colors);
      end if;

      textY := Center_Text_Y (r);

      Draw_UI_Text (textCanvas, textX, textY, text, colors.text, face);
      if focused then
         if cursorX + 1 >= r.x + r.w then
            cursorX := r.x + r.w - 2;
         end if;
         cursor := (x => cursorX + 1, y => textY + 2,
                    w => 1, h => UI_Text_Height - 4);
         Fill_Rect (textCanvas, cursor, colors.accent);
      end if;
   end Draw_Text_Field;

   procedure Draw_Text_Edit_Field
      (c : Canvas; r : Rect; colors : Theme; text : String;
       cursor, selectionStart, selectionEnd : Natural;
       focused : Boolean; hot : Boolean)
   is
      face : Color := colors.field;
      textX : Natural := r.x + 8;
      textY : Natural := r.y;
      cursorX : Natural := textX;
      charW : Natural;
      fg : Color;
      bg : Color;
      caret : Rect;
      textCanvas : constant Canvas := With_Clip
        (c, (x => r.x + 3, y => r.y + 2,
             w => (if r.w > 6 then r.w - 6 else 0),
             h => (if r.h > 4 then r.h - 4 else 0)));
   begin
      if hot then
         face := colors.face;
      end if;

      Fill_Rect (c, r, face);
      if focused then
         Stroke_Rect (c, r, colors.accent, colors.shadow);
      else
         Stroke_Sunken (c, r, colors);
      end if;

      textY := Center_Text_Y (r);

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

      if focused then
         if cursorX + 1 >= r.x + r.w then
            cursorX := r.x + r.w - 2;
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
         Stroke_Rect (c, r, colors.accent, colors.shadow);
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
      face : Color := colors.face;
      mark : constant Rect :=
        (x => r.x + 4, y => r.y + 4,
         w => (if r.w > 8 then r.w - 8 else 0),
         h => (if r.h > 8 then r.h - 8 else 0));
   begin
      if active then
         face := colors.shadow;
      elsif hot then
         face := colors.panel;
      end if;

      Fill_Rect (c, r, face);
      Stroke_Sunken (c, r, colors);
      if checked and then not Is_Empty (mark) then
         Fill_Rect (c, mark, colors.accent);
         Stroke_Rect (c, mark, colors.good, colors.shadow);
      end if;
   end Draw_Checkbox;

   procedure Draw_Radio_Button
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; active : Boolean;
       label : String)
   is
      box : constant Rect := (x => r.x, y => r.y, w => 18, h => 18);
      mark : constant Rect := (x => r.x + 5, y => r.y + 5, w => 8, h => 8);
      face : Color := colors.shadow;
   begin
      if hot then
         face := colors.face;
      end if;
      if active then
         face := colors.edge;
      end if;

      Fill_Rect (c, box, face);
      Stroke_Sunken (c, box, colors);
      if selected then
         Fill_Rect (c, mark, colors.accent);
      end if;
      Draw_UI_Text (c, r.x + 28, r.y + 1, label, colors.text, colors.panel);
   end Draw_Radio_Button;

   procedure Draw_List_Item
      (c : Canvas; r : Rect; colors : Theme;
       selected : Boolean; hot : Boolean; label : String)
   is
      bg : Color := colors.panel;
      fg : Color := colors.text;
   begin
      if selected then
         bg := colors.accent;
         fg := colors.shadow;
      elsif hot then
         bg := colors.face;
      end if;

      Fill_Rect (c, r, bg);
      Draw_UI_Text (c, r.x + 8, Center_Text_Y (r), label, fg, bg);
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

      Fill_Rect (c, r, bg);
      if enabled then
         Fill_Rect (c, icon, colors.face);
         Stroke_Rect (c, icon, colors.edge, colors.shadow);
      else
         Stroke_Rect (c, icon, colors.edge, colors.shadow);
      end if;
      Draw_UI_Text (c, r.x + 30, Center_Text_Y (r), label, fg, bg);
   end Draw_Menu_Item;

   procedure Draw_Horizontal_Slider
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean)
   is
      trackY : constant Natural := r.y + r.h / 2 - 2;
      track : constant Rect := (x => r.x + 6, y => trackY,
                                w => (if r.w > 12 then r.w - 12 else 0),
                                h => 4);
      span : Natural := 1;
      pos  : Natural := 0;
      knobX : Natural := r.x;
      knob : Rect;
      fillColor : Color := colors.accent;
   begin
      if maxValue > minValue then
         span := maxValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if not Is_Empty (track) then
         knobX := track.x + (pos * track.w) / span;
      end if;
      if r.w > 10 and then knobX > r.x + r.w - 10 then
         knobX := r.x + r.w - 10;
      end if;

      if active then
         fillColor := colors.good;
      elsif hot then
         fillColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (track) then
         Fill_Rect (c, track, colors.shadow);
         Fill_Rect (c, (x => track.x, y => track.y,
                        w => knobX - track.x + 5, h => track.h),
                    fillColor);
      end if;

      knob := (x => knobX, y => r.y + 3, w => 10,
               h => (if r.h > 6 then r.h - 6 else r.h));
      Fill_Rect (c, knob, colors.face);
      Stroke_Rect (c, knob, colors.edge, colors.shadow);
   end Draw_Horizontal_Slider;

   procedure Draw_Vertical_Scrollbar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean; pageSize : Positive := 1;
       pressedPart : Scrollbar_Part := Scrollbar_Thumb)
   is
      buttonExtent : constant Natural := Natural'Min (r.w, r.h / 2);
      upButton : constant Rect :=
        (x => r.x, y => r.y, w => r.w, h => buttonExtent);
      downButton : constant Rect :=
        (x => r.x, y => r.y + r.h - buttonExtent,
         w => r.w, h => buttonExtent);
      trackFrame : constant Rect :=
        (x => r.x, y => r.y + buttonExtent, w => r.w,
         h => (if r.h > buttonExtent * 2 then r.h - buttonExtent * 2 else 0));
      track : constant Rect :=
        (x => trackFrame.x + 2, y => trackFrame.y + 2,
         w => (if trackFrame.w > 4 then trackFrame.w - 4 else 0),
         h => (if trackFrame.h > 4 then trackFrame.h - 4 else 0));
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      maximumValue : constant Natural :=
        (if shown >= total then minValue else maxValue - shown + 1);
      span : Natural := 1;
      pos  : Natural := 0;
      thumbHeight : Natural := 0;
      travel : Natural := 0;
      knobY : Natural := track.y;
      knob : Rect;
      knobColor : Color := colors.face;
      canDecrement : constant Boolean :=
        shown < total and then value > minValue;
      canIncrement : constant Boolean :=
        shown < total and then value < maximumValue;

      procedure Draw_Arrow
        (Button : Rect; Points_Up, Enabled, Pressed : Boolean)
      is
         Offset : constant Natural := (if Pressed then 1 else 0);
         centerX : constant Natural := Button.x + Button.w / 2 + Offset;
         centerY : constant Natural := Button.y + Button.h / 2 + Offset;
         arrowColor : constant Color :=
           (if Enabled then colors.text else colors.shadow);
      begin
         if Button.w < 8 or else Button.h < 8 then return; end if;
         for Row in 0 .. 3 loop
            Fill_Rect
              (c,
               (x => centerX - (if Points_Up then Row else 3 - Row),
                y => centerY - 2 + Row,
                w => 1 + 2 * (if Points_Up then Row else 3 - Row), h => 1),
               arrowColor);
         end loop;
      end Draw_Arrow;
   begin
      if maximumValue > minValue then
         span := maximumValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if not Is_Empty (track) then
         thumbHeight := Natural'Max (12, track.h * shown / total);
         thumbHeight := Natural'Min (thumbHeight, track.h);
         travel := track.h - thumbHeight;
         knobY := track.y + (pos * travel) / span;
      end if;

      if active then
         knobColor := Blend (colors.edge, colors.face, 64);
      elsif hot then
         knobColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (trackFrame) then
         Fill_Rect (c, trackFrame, colors.edge);
         Stroke_Sunken (c, trackFrame, colors);
      end if;
      Fill_Rect (c, upButton, colors.panel);
      if active and then canDecrement and then
        pressedPart = Scrollbar_Decrement
      then
         Stroke_Sunken (c, upButton, colors);
      else
         Stroke_Raised (c, upButton, colors);
      end if;
      Draw_Arrow
        (upButton, True, canDecrement,
         active and then canDecrement and then
           pressedPart = Scrollbar_Decrement);
      Fill_Rect (c, downButton, colors.panel);
      if active and then canIncrement and then
        pressedPart = Scrollbar_Increment
      then
         Stroke_Sunken (c, downButton, colors);
      else
         Stroke_Raised (c, downButton, colors);
      end if;
      Draw_Arrow
        (downButton, False, canIncrement,
         active and then canIncrement and then
           pressedPart = Scrollbar_Increment);

      if shown < total then
         knob := (x => track.x, y => knobY, w => track.w,
                  h => thumbHeight);
         Fill_Rect (c, knob, knobColor);
         Stroke_Raised (c, knob, colors);
      end if;
   end Draw_Vertical_Scrollbar;

   procedure Draw_Horizontal_Scrollbar
      (c : Canvas; r : Rect; colors : Theme;
       minValue, maxValue, value : Natural;
       hot : Boolean; active : Boolean; pageSize : Positive := 1;
       pressedPart : Scrollbar_Part := Scrollbar_Thumb)
   is
      buttonExtent : constant Natural := Natural'Min (r.h, r.w / 2);
      leftButton : constant Rect :=
        (x => r.x, y => r.y, w => buttonExtent, h => r.h);
      rightButton : constant Rect :=
        (x => r.x + r.w - buttonExtent, y => r.y,
         w => buttonExtent, h => r.h);
      trackFrame : constant Rect :=
        (x => r.x + buttonExtent, y => r.y,
         w => (if r.w > buttonExtent * 2 then r.w - buttonExtent * 2 else 0),
         h => r.h);
      track : constant Rect :=
        (x => trackFrame.x + 2, y => trackFrame.y + 2,
         w => (if trackFrame.w > 4 then trackFrame.w - 4 else 0),
         h => (if trackFrame.h > 4 then trackFrame.h - 4 else 0));
      total : constant Natural :=
        (if maxValue >= minValue then maxValue - minValue + 1 else 1);
      shown : constant Natural := Natural'Min (pageSize, total);
      maximumValue : constant Natural :=
        (if shown >= total then minValue else maxValue - shown + 1);
      span : Natural := 1;
      pos : Natural := 0;
      thumbWidth : Natural := 0;
      travel : Natural := 0;
      knobX : Natural := track.x;
      knob : Rect;
      knobColor : Color := colors.face;
      canDecrement : constant Boolean :=
        shown < total and then value > minValue;
      canIncrement : constant Boolean :=
        shown < total and then value < maximumValue;

      procedure Draw_Arrow
        (Button : Rect; Points_Left, Enabled, Pressed : Boolean)
      is
         Offset : constant Natural := (if Pressed then 1 else 0);
         centerX : constant Natural := Button.x + Button.w / 2 + Offset;
         centerY : constant Natural := Button.y + Button.h / 2 + Offset;
         arrowColor : constant Color :=
           (if Enabled then colors.text else colors.shadow);
      begin
         if Button.w < 8 or else Button.h < 8 then return; end if;
         for Column in 0 .. 3 loop
            Fill_Rect
              (c,
               (x => (if Points_Left then centerX - 2 + Column
                      else centerX + 2 - Column),
                y => centerY - Column,
                w => 1, h => 1 + 2 * Column),
               arrowColor);
         end loop;
      end Draw_Arrow;
   begin
      if maximumValue > minValue then
         span := maximumValue - minValue;
      end if;
      if value > minValue then
         pos := Natural'Min (value - minValue, span);
      end if;
      if not Is_Empty (track) then
         thumbWidth := Natural'Max (12, track.w * shown / total);
         thumbWidth := Natural'Min (thumbWidth, track.w);
         travel := track.w - thumbWidth;
         knobX := track.x + (pos * travel) / span;
      end if;

      if active then
         knobColor := Blend (colors.edge, colors.face, 64);
      elsif hot then
         knobColor := colors.accent;
      end if;

      Fill_Rect (c, r, colors.panel);
      if not Is_Empty (trackFrame) then
         Fill_Rect (c, trackFrame, colors.edge);
         Stroke_Sunken (c, trackFrame, colors);
      end if;
      Fill_Rect (c, leftButton, colors.panel);
      if active and then canDecrement and then
        pressedPart = Scrollbar_Decrement
      then
         Stroke_Sunken (c, leftButton, colors);
      else
         Stroke_Raised (c, leftButton, colors);
      end if;
      Draw_Arrow
        (leftButton, True, canDecrement,
         active and then canDecrement and then
           pressedPart = Scrollbar_Decrement);
      Fill_Rect (c, rightButton, colors.panel);
      if active and then canIncrement and then
        pressedPart = Scrollbar_Increment
      then
         Stroke_Sunken (c, rightButton, colors);
      else
         Stroke_Raised (c, rightButton, colors);
      end if;
      Draw_Arrow
        (rightButton, False, canIncrement,
         active and then canIncrement and then
           pressedPart = Scrollbar_Increment);

      if shown < total then
         knob := (x => knobX, y => track.y, w => thumbWidth, h => track.h);
         Fill_Rect (c, knob, knobColor);
         Stroke_Raised (c, knob, colors);
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
