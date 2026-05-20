------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Small immediate-mode UI drawing primitives for user surfaces
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;
with Font8x16;

package body CuBit.UI is
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
   begin
      for i in text'Range loop
         exit when cx + Font8x16.GLYPH_WIDTH > c.width;
         Draw_Glyph (c, cx, y, text (i), fg, bg);
         cx := cx + Font8x16.GLYPH_WIDTH;
      end loop;
   end Draw_Text;

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

   function Button
      (bounds : Rect; pointer : Pointer_State) return Widget_Result
   is
      hot : constant Boolean :=
         pointer.enabled and then Point_In_Rect (pointer.x, pointer.y, bounds);
   begin
      return
        (hot       => hot,
         active    => hot and then pointer.down,
         activated => hot and then pointer.pressed);
   end Button;
end CuBit.UI;
