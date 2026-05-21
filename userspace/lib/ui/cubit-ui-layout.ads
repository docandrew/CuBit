------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Tiny deterministic layout helpers for immediate-mode UI code
--
--  The layout package is intentionally boring: it hands out stable rectangles
--  from a caller-provided region. Apps still own their visual hierarchy, but
--  they no longer need to scatter magic x/y pairs through rendering, hit
--  testing, and damage tracking.
------------------------------------------------------------------------------
package CuBit.UI.Layout is
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
