------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tiny deterministic layout helpers for immediate-mode UI code
--
--  The layout package is intentionally boring: it hands out stable rectangles.
--  App-facing layout should be parent-relative: "x=12" means 12 pixels inside
--  the current container. Drawing, hit testing, and damage tracking still use
--  absolute surface coordinates. Container/Resolve is the bridge between those
--  two worlds.
------------------------------------------------------------------------------
package CuBit.UI.Layout is
   type Container is record
      --  Absolute content bounds for children.
      bounds : CuBit.UI.Rect := (others => 0);

      --  Absolute clip region for drawing children. This may be smaller than
      --  bounds for scroll views or clipped pages.
      clip : CuBit.UI.Rect := (others => 0);
   end record;

   type Dock_Frame is record
      remaining : CuBit.UI.Rect := (others => 0);
   end record;

   type Anchor_Set is record
      left   : Boolean := True;
      top    : Boolean := True;
      right  : Boolean := False;
      bottom : Boolean := False;
   end record;

   type Cursor is record
      bounds : CuBit.UI.Rect := (others => 0);
      x      : Natural := 0;
      y      : Natural := 0;
      rowH   : Natural := 0;
      gapX   : Natural := 0;
      gapY   : Natural := 0;
      overflow : Boolean := False;
   end record;

   function Inset
      (r : CuBit.UI.Rect;
       left, top, right, bottom : Natural) return CuBit.UI.Rect;

   function Root (bounds : CuBit.UI.Rect) return Container;

   function Content
      (parent : Container;
       padding : Natural) return Container;

   function Child
      (parent : Container;
       relativeBounds : CuBit.UI.Rect) return Container;

   function Resolve
      (parent : Container;
       relativeBounds : CuBit.UI.Rect) return CuBit.UI.Rect;

   function Canvas_For
      (c : CuBit.UI.Canvas;
       parent : Container) return CuBit.UI.Canvas;

   function Begin_Dock (bounds : CuBit.UI.Rect) return Dock_Frame;

   function Dock_Top
      (f : in out Dock_Frame; h : Natural) return CuBit.UI.Rect;

   function Dock_Bottom
      (f : in out Dock_Frame; h : Natural) return CuBit.UI.Rect;

   function Dock_Left
      (f : in out Dock_Frame; w : Natural) return CuBit.UI.Rect;

   function Dock_Right
      (f : in out Dock_Frame; w : Natural) return CuBit.UI.Rect;

   function Fill (f : Dock_Frame) return CuBit.UI.Rect;

   function Anchor
      (designBounds : CuBit.UI.Rect;
       designParent : CuBit.UI.Rect;
       actualParent : CuBit.UI.Rect;
       anchors : Anchor_Set) return CuBit.UI.Rect;

   function Start
      (bounds : CuBit.UI.Rect;
       gapX   : Natural := 8;
       gapY   : Natural := 8) return Cursor;

   procedure New_Row
      (l : in out Cursor;
       gapBefore : Natural := 0);

   function Take
      (l : in out Cursor;
       w, h : Natural) return CuBit.UI.Rect;

   function Take_Remaining
      (l : in out Cursor;
       h : Natural) return CuBit.UI.Rect;
end CuBit.UI.Layout;
