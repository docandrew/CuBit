------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tiny deterministic layout helpers for immediate-mode UI code
------------------------------------------------------------------------------
package body CuBit.UI.Layout is
   function Intersect
      (a, b : CuBit.UI.Rect) return CuBit.UI.Rect
   is
      x1 : Natural;
      y1 : Natural;
      x2 : Natural;
      y2 : Natural;
   begin
      if CuBit.UI.Is_Empty (a) or else CuBit.UI.Is_Empty (b) then
         return (others => 0);
      end if;

      x1 := Natural'Max (a.x, b.x);
      y1 := Natural'Max (a.y, b.y);
      x2 := Natural'Min (a.x + a.w, b.x + b.w);
      y2 := Natural'Min (a.y + a.h, b.y + b.h);

      if x1 >= x2 or else y1 >= y2 then
         return (others => 0);
      end if;
      return (x => x1, y => y1, w => x2 - x1, h => y2 - y1);
   end Intersect;

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

   function Root (bounds : CuBit.UI.Rect) return Container is
   begin
      return (bounds => bounds, clip => bounds);
   end Root;

   function Content
      (parent : Container;
       padding : Natural) return Container
   is
      b : constant CuBit.UI.Rect :=
         Inset (parent.bounds, padding, padding, padding, padding);
   begin
      return (bounds => b, clip => Intersect (parent.clip, b));
   end Content;

   function Resolve
      (parent : Container;
       relativeBounds : CuBit.UI.Rect) return CuBit.UI.Rect
   is
   begin
      if CuBit.UI.Is_Empty (parent.bounds) or else
         CuBit.UI.Is_Empty (relativeBounds) or else
         relativeBounds.x >= parent.bounds.w or else
         relativeBounds.y >= parent.bounds.h
      then
         return (others => 0);
      end if;

      return
        (x => parent.bounds.x + relativeBounds.x,
         y => parent.bounds.y + relativeBounds.y,
         w => Natural'Min (relativeBounds.w,
                           parent.bounds.w - relativeBounds.x),
         h => Natural'Min (relativeBounds.h,
                           parent.bounds.h - relativeBounds.y));
   end Resolve;

   function Child
      (parent : Container;
       relativeBounds : CuBit.UI.Rect) return Container
   is
      b : constant CuBit.UI.Rect := Resolve (parent, relativeBounds);
   begin
      return (bounds => b, clip => Intersect (parent.clip, b));
   end Child;

   function Canvas_For
      (c : CuBit.UI.Canvas;
       parent : Container) return CuBit.UI.Canvas
   is
   begin
      return CuBit.UI.With_Clip (c, parent.clip);
   end Canvas_For;

   function Begin_Dock (bounds : CuBit.UI.Rect) return Dock_Frame is
   begin
      return (remaining => bounds);
   end Begin_Dock;

   function Dock_Top
      (f : in out Dock_Frame; h : Natural) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := f.remaining;
   begin
      if CuBit.UI.Is_Empty (f.remaining) or else h = 0 then
         return (others => 0);
      end if;

      ret.h := Natural'Min (h, f.remaining.h);
      f.remaining.y := f.remaining.y + ret.h;
      f.remaining.h := f.remaining.h - ret.h;
      return ret;
   end Dock_Top;

   function Dock_Bottom
      (f : in out Dock_Frame; h : Natural) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := f.remaining;
      actualH : Natural;
   begin
      if CuBit.UI.Is_Empty (f.remaining) or else h = 0 then
         return (others => 0);
      end if;

      actualH := Natural'Min (h, f.remaining.h);
      ret.y := f.remaining.y + f.remaining.h - actualH;
      ret.h := actualH;
      f.remaining.h := f.remaining.h - actualH;
      return ret;
   end Dock_Bottom;

   function Dock_Left
      (f : in out Dock_Frame; w : Natural) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := f.remaining;
   begin
      if CuBit.UI.Is_Empty (f.remaining) or else w = 0 then
         return (others => 0);
      end if;

      ret.w := Natural'Min (w, f.remaining.w);
      f.remaining.x := f.remaining.x + ret.w;
      f.remaining.w := f.remaining.w - ret.w;
      return ret;
   end Dock_Left;

   function Dock_Right
      (f : in out Dock_Frame; w : Natural) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := f.remaining;
      actualW : Natural;
   begin
      if CuBit.UI.Is_Empty (f.remaining) or else w = 0 then
         return (others => 0);
      end if;

      actualW := Natural'Min (w, f.remaining.w);
      ret.x := f.remaining.x + f.remaining.w - actualW;
      ret.w := actualW;
      f.remaining.w := f.remaining.w - actualW;
      return ret;
   end Dock_Right;

   function Fill (f : Dock_Frame) return CuBit.UI.Rect is
   begin
      return f.remaining;
   end Fill;

   function Anchor
      (designBounds : CuBit.UI.Rect;
       designParent : CuBit.UI.Rect;
       actualParent : CuBit.UI.Rect;
       anchors : Anchor_Set) return CuBit.UI.Rect
   is
      ret : CuBit.UI.Rect := designBounds;
      parentDeltaW : constant Integer :=
         Integer (actualParent.w) - Integer (designParent.w);
      parentDeltaH : constant Integer :=
         Integer (actualParent.h) - Integer (designParent.h);

      function Add_Delta (value : Natural; amount : Integer) return Natural is
      begin
         if amount < 0 and then Natural (-amount) >= value then
            return 0;
         elsif amount < 0 then
            return value - Natural (-amount);
         else
            return value + Natural (amount);
         end if;
      end Add_Delta;
   begin
      ret.x := actualParent.x + (designBounds.x - designParent.x);
      ret.y := actualParent.y + (designBounds.y - designParent.y);

      if anchors.left and then anchors.right then
         ret.w := Add_Delta (designBounds.w, parentDeltaW);
      elsif anchors.right and then not anchors.left then
         ret.x := Add_Delta (ret.x, parentDeltaW);
      end if;

      if anchors.top and then anchors.bottom then
         ret.h := Add_Delta (designBounds.h, parentDeltaH);
      elsif anchors.bottom and then not anchors.top then
         ret.y := Add_Delta (ret.y, parentDeltaH);
      end if;

      return ret;
   end Anchor;

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
