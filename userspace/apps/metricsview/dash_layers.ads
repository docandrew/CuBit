------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The metrics viewer's retained frame: the whole window, kept in the
--  app's own memory, always complete. The view draws only what changed
--  (Invalidate: several exact rectangles, not one bounding box) into it,
--  and a frame is a copy out of it (Copy_Out), which costs about a fill.
--
--  @description
--  This separates what the toolkit must repaint (one rectangle per frame,
--  plus the other buffer's repair) from what the view must redraw: slivers
--  in several panels cost their own area, the toolkit's union is a copy. A
--  pan shifts a plot's pixels here (Shift) and redraws only the strip that
--  scrolled in. The buffer grows on CuAlloc with the window (up to the
--  toolkit's surface size) and is never shrunk.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.UI;

package Dash_Layers is
   subtype Rect is CuBit.UI.Rect;

   --  The window's logical size and density.
   procedure Resize (Width, Height : Natural; Numerator, Denominator : Positive);
   function Ready return Boolean;

   --  The retained frame as a canvas (logical geometry, the window's density).
   function Canvas return CuBit.UI.Canvas;
   function Canvas (Clip : Rect) return CuBit.UI.Canvas;

   --  What must be redrawn into the retained frame. At most Maximum_Dirty
   --  rectangles are kept; another one merges with the one it grows least.
   Maximum_Dirty : constant := 16;
   procedure Invalidate (R : Rect);
   function Dirty_Count return Natural;
   function Dirty (I : Positive) return Rect;
   procedure Clear_Dirty;
   --  The union of what is dirty (empty when nothing is).
   function Dirty_Bounds return Rect;

   --  Copy R of the retained frame into Target (the same geometry).
   procedure Copy_Out (Target : CuBit.UI.Canvas; R : Rect);

   --  Move R's pixels Dx logical pixels sideways inside the retained frame
   --  (negative: to the left); what scrolls in is left for the caller to
   --  invalidate and redraw. Only at whole physical pixels: at a density
   --  where Dx is not, R is invalidated whole instead.
   procedure Shift (R : Rect; Dx : Integer);
end Dash_Layers;
