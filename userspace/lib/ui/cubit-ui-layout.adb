------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tiny deterministic layout helpers for immediate-mode UI code
------------------------------------------------------------------------------
package body CuBit.UI.Layout is
   function Inset
      (r : CuBit.UI.Rect;
       left, top, right, bottom : Natural) return CuBit.UI.Rect
   is
      trimW : constant Natural := left + right;
      trimH : constant Natural := top + bottom;
   begin
      if CuBit.UI.Is_Empty (r) or else trimW >= r.w or else trimH >= r.h then
         return (others => 0);
      end if;

      return
        (x => r.x + left,
         y => r.y + top,
         w => r.w - trimW,
         h => r.h - trimH);
   end Inset;

   function Start
      (bounds : CuBit.UI.Rect;
       gapX   : Natural := 8;
       gapY   : Natural := 8) return Cursor
   is
   begin
      return
        (bounds   => bounds,
         x        => bounds.x,
         y        => bounds.y,
         rowH     => 0,
         gapX     => gapX,
         gapY     => gapY,
         overflow => CuBit.UI.Is_Empty (bounds));
   end Start;

   procedure New_Row
      (l : in out Cursor;
       gapBefore : Natural := 0)
   is
   begin
      if l.rowH = 0 then
         l.y := l.y + gapBefore;
      else
         l.y := l.y + l.rowH + l.gapY + gapBefore;
      end if;

      l.x := l.bounds.x;
      l.rowH := 0;
      if l.y >= l.bounds.y + l.bounds.h then
         l.overflow := True;
      end if;
   end New_Row;

   function Take
      (l : in out Cursor;
       w, h : Natural) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := (x => l.x, y => l.y, w => w, h => h);
      maxX : constant Natural := l.bounds.x + l.bounds.w;
      maxY : constant Natural := l.bounds.y + l.bounds.h;
   begin
      if l.overflow or else w = 0 or else h = 0 or else
         l.x >= maxX or else l.y >= maxY
      then
         l.overflow := True;
         return (others => 0);
      end if;

      if ret.x + ret.w > maxX then
         ret.w := maxX - ret.x;
         l.overflow := True;
      end if;
      if ret.y + ret.h > maxY then
         ret.h := maxY - ret.y;
         l.overflow := True;
      end if;

      if h > l.rowH then
         l.rowH := h;
      end if;

      if ret.x + ret.w + l.gapX >= maxX then
         l.x := maxX;
      else
         l.x := ret.x + ret.w + l.gapX;
      end if;

      return ret;
   end Take;

   function Take_Remaining
      (l : in out Cursor;
       h : Natural) return CuBit.UI.Rect
   is
      maxX : constant Natural := l.bounds.x + l.bounds.w;
   begin
      if l.x >= maxX then
         l.overflow := True;
         return (others => 0);
      end if;

      return Take (l, maxX - l.x, h);
   end Take_Remaining;
end CuBit.UI.Layout;
