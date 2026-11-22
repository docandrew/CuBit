------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Surface: a rectangle of a window painted by an embedded renderer (for
--  example a web engine, an emulator or a CCL canvas) while the application
--  draws all surrounding UI with CuBit.UI. Immediate-mode, like the rest of
--  the toolkit: the application keeps a Surface record, asks for a View to
--  paint through, and routes window input through Route.
------------------------------------------------------------------------------
with CuBit.UI.Input;

package CuBit.UI.Surfaces is

   type Surface is record
      --  In window coordinates; set by the application's layout.
      area : Rect := (others => 0);
      --  Keyboard and text input go to the surface only while focused.
      focused : Boolean := False;
      --  A pointer press inside the area captures the pointer until
      --  release, so drags keep reporting (clamped) surface coordinates.
      captured : Boolean := False;
      pointerInside : Boolean := False;
   end record;

   --  Where a view lands inside its parent canvas: the area clamped to the
   --  parent, and the parent's clip (if any) translated into view
   --  coordinates. Pure geometry, independent of pixel memory.
   type Placement is record
      x, y : Natural := 0;           --  top-left in parent coordinates
      width, height : Natural := 0;  --  zero when the area is off-canvas
      clipEnabled : Boolean := False;
      clip : Rect := (others => 0);  --  in view coordinates
   end record;

   function Place
     (parentWidth, parentHeight : Natural;
      parentClipEnabled : Boolean; parentClip : Rect;
      area : Rect) return Placement
   with
     Post =>
       Place'Result.width <= parentWidth and then
       Place'Result.height <= parentHeight and then
       (Place'Result.width = 0 or else
          (Place'Result.x < parentWidth and then
           Place'Result.width <= parentWidth - Place'Result.x)) and then
       (Place'Result.height = 0 or else
          (Place'Result.y < parentHeight and then
           Place'Result.height <= parentHeight - Place'Result.y)) and then
       (if Place'Result.clipEnabled then
          Place'Result.clip.w <= Place'Result.width and then
          Place'Result.clip.h <= Place'Result.height);

   --  A canvas addressing exactly the placed area of Parent: origin at the
   --  area's top-left, the parent's pitch, and a clip that keeps every draw
   --  inside the area (and inside the parent's own clip). Painting through
   --  it cannot touch pixels outside the area. Same pixels, no copy.
   function View (parent : Canvas; area : Rect) return Canvas;

   type Event_Kind is
     (No_Event, Pointer_Move, Pointer_Down, Pointer_Up, Pointer_Leave,
      Wheel, Key_Down, Key_Up, Text);

   type Surface_Event is record
      kind : Event_Kind := No_Event;
      --  Surface-relative pointer position, clamped to the area.
      x, y : Natural := 0;
      primaryDown : Boolean := False;
      wheelDelta : Integer := 0;
      --  Key: scancode and modifiers as delivered by the desktop. Text: the
      --  character code. Passed through unchanged.
      code : Unsigned_64 := 0;
      modifiers : Unsigned_64 := 0;
   end record;

   --  Translate one window input event for this surface. Pointer events
   --  inside the area (or while captured) become surface-relative; leaving
   --  the area yields one Pointer_Leave. A press inside focuses the surface
   --  and a press outside unfocuses it. Keyboard and text events are routed
   --  only while focused. Everything else yields No_Event, and the
   --  application handles it as ordinary UI input.
   procedure Route
     (item : in out Surface; event : CuBit.UI.Input.Input_Event;
      result : out Surface_Event);
end CuBit.UI.Surfaces;
