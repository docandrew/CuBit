------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Font8x16;
with CuBit.UI.Fonts_Luxi_Sans_12;

package body CuBit.UI is
   use type System.Address;
   package UI_Font renames CuBit.UI.Fonts_Luxi_Sans_12;

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
   begin
      if c.addr = System.Null_Address or else Is_Empty (clipped) then
         return;
      end if;

      for yy in clipped.y .. clipped.y + clipped.h - 1 loop
         for xx in clipped.x .. clipped.x + clipped.w - 1 loop
            declare
               offset : constant Storage_Offset :=
                  Storage_Offset (yy * c.pitch + xx * 4);
               pixel : Color with Import, Address => c.addr + offset;
            begin
               pixel := fill;
            end;
         end loop;
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

   procedure Draw_Glyph
      (c : Canvas; x, y : Natural; ch : Character; fg, bg : Color)
   is
      glyph : Font8x16.GlyphData renames Font8x16.font (Character'Pos (ch));
   begin
      for row in 0 .. Font8x16.GLYPH_HEIGHT - 1 loop
         declare
            bits : constant Unsigned_8 := glyph (row);
         begin
            for bit in 0 .. Font8x16.GLYPH_WIDTH - 1 loop
               if (bits and Shift_Right (16#80#, bit)) /= 0 then
                  Set_Pixel (c, x + bit, y + row, fg);
               else
                  Set_Pixel (c, x + bit, y + row, bg);
               end if;
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
   begin
      if code < UI_Font.FIRST_GLYPH or else code > UI_Font.LAST_GLYPH then
         code := Character'Pos ('?');
      end if;

      width := UI_Font.Widths (code);
      Fill_Rect (c, (x => x, y => y, w => width, h => UI_Font.LINE_HEIGHT), bg);
      for yy in 0 .. UI_Font.LINE_HEIGHT - 1 loop
         for xx in 0 .. width - 1 loop
            alpha := UI_Font.Alpha (code) (yy) (xx);
            if alpha = 255 then
               Set_Pixel (c, x + xx, y + yy, fg);
            elsif alpha > 0 then
               Set_Pixel (c, x + xx, y + yy, Blend (fg, bg, alpha));
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

   procedure Draw_Button_Frame
      (c : Canvas; r : Rect; colors : Theme; style : Button_Style)
   is
      face : Color := colors.face;
      light : Color := colors.edge;
      dark : Color := colors.shadow;
   begin
      case style is
         when Button_Hot =>
            face := colors.panel;
         when Button_Pressed =>
            light := colors.shadow;
            dark := colors.edge;
         when Button_Disabled =>
            face := colors.shadow;
            light := colors.edge;
            dark := colors.shadow;
         when Button_Active =>
            face := colors.accent;
            light := colors.edge;
            dark := colors.shadow;
         when Button_Normal =>
            null;
      end case;

      Fill_Rect (c, r, face);
      Stroke_Rect (c, r, light, dark);
   end Draw_Button_Frame;

   function Button_Face (colors : Theme; style : Button_Style) return Color is
   begin
      case style is
         when Button_Hot =>
            return colors.panel;
         when Button_Disabled =>
            return colors.shadow;
         when Button_Active =>
            return colors.accent;
         when Button_Normal | Button_Pressed =>
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
      if r.h > UI_Text_Height then
         ty := r.y + (r.h - UI_Text_Height) / 2;
      end if;
      if style = Button_Disabled then
         fg := colors.muted;
      end if;

      Draw_UI_Text (c, tx, ty, label, fg, Button_Face (colors, style));
   end Draw_Button;

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
      face : Color := colors.shadow;
      textX : constant Natural := r.x + 6;
      textY : Natural := r.y;
      cursorX : Natural := textX + UI_Text_Width (text);
      cursor : Rect;
   begin
      if hot then
         face := colors.face;
      end if;

      Fill_Rect (c, r, face);
      Stroke_Rect (c, r,
                   (if focused then colors.accent else colors.edge),
                   colors.shadow);

      if r.h > UI_Text_Height then
         textY := r.y + (r.h - UI_Text_Height) / 2;
      end if;

      Draw_UI_Text (c, textX, textY, text, colors.text, face);
      if focused then
         if cursorX + 1 >= r.x + r.w then
            cursorX := r.x + r.w - 2;
         end if;
         cursor := (x => cursorX + 1, y => textY + 2,
                    w => 1, h => UI_Text_Height - 4);
         Fill_Rect (c, cursor, colors.accent);
      end if;
   end Draw_Text_Field;

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
      Stroke_Rect (c, r, colors.edge, colors.shadow);
      if checked and then not Is_Empty (mark) then
         Fill_Rect (c, mark, colors.accent);
         Stroke_Rect (c, mark, colors.good, colors.shadow);
      end if;
   end Draw_Checkbox;

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
