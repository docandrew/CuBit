------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Minimal desktop-surface harness for native CuBit UI applications
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;

package CuBit.UI.App is
   INPUT_NONE         : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN     : constant Unsigned_64 := 1;
   INPUT_KEY_UP       : constant Unsigned_64 := 2;
   INPUT_POINTER_MOVE : constant Unsigned_64 := 3;
   INPUT_POINTER_DOWN : constant Unsigned_64 := 4;
   INPUT_POINTER_UP   : constant Unsigned_64 := 5;
   INPUT_TEXT         : constant Unsigned_64 := 6;
   INPUT_POINTER_WHEEL : constant Unsigned_64 := 7;
   INPUT_CONFIGURE    : constant Unsigned_64 := 8;
   INPUT_RESYNC       : constant Unsigned_64 := 9;

   KEY_ESC : constant Unsigned_64 := 16#01#;
   KEY_Q   : constant Unsigned_64 := 16#10#;
   KEY_F   : constant Unsigned_64 := 16#21#;
   KEY_R   : constant Unsigned_64 := 16#13#;
   KEY_M   : constant Unsigned_64 := 16#32#;

   KEYMOD_SHIFT : constant Unsigned_64 := 1;
   KEYMOD_CTRL  : constant Unsigned_64 := 2;
   KEYMOD_ALT   : constant Unsigned_64 := 4;
   KEYMOD_CAPS  : constant Unsigned_64 := 8;

   WINDOW_FLAG_DECORATED   : constant Unsigned_64 := 1;
   WINDOW_FLAG_RESIZABLE   : constant Unsigned_64 := 2;
   WINDOW_FLAG_MINIMIZABLE : constant Unsigned_64 := 4;
   WINDOW_FLAG_MAXIMIZABLE : constant Unsigned_64 := 8;
   WINDOW_FLAG_CLOSEABLE   : constant Unsigned_64 := 16;
   WINDOW_FLAG_FIXED_SIZE  : constant Unsigned_64 := 128;

   type Input_Event is record
      kind    : Unsigned_64 := INPUT_NONE;
      serial  : Unsigned_64 := 0;
      payload0 : Unsigned_64 := 0;
      payload1 : Unsigned_64 := 0;
   end record;

   --  Normal controls repaint only when their visual state can change.
   --  Canvases and similar position-sensitive surfaces can explicitly opt in
   --  to receiving a repaint for every pointer-motion event.
   type Pointer_Repaint_Policy is
     (Repaint_Changed_Controls, Repaint_Every_Motion);

   type Pointer_Interaction is private;

   type Window is private;

   procedure Open
      (win : in out Window;
       width, height : Natural;
       flags : Unsigned_64;
       ok : out Boolean;
       maximum_width : Natural := 0;
       maximum_height : Natural := 0;
       title : String := "Application");

   procedure Set_Title (win : Window; title : String);

   function Is_Open (win : Window) return Boolean;
   function Surface_ID (win : Window) return Unsigned_64;
   function Width (win : Window) return Natural;
   function Height (win : Window) return Natural;
   function Full_Rect (win : Window) return CuBit.UI.Rect;
   function Canvas (win : Window) return CuBit.UI.Canvas;
   function Canvas
      (win : Window; clip : CuBit.UI.Rect) return CuBit.UI.Canvas;

   procedure Poll_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean);

   --  Block without polling until the compositor can return the next event.
   --  The compositor retains a one-use reply capability while this call is
   --  parked, so a wake cannot be forged or redirected to another client.
   procedure Wait_Input
      (win : in out Window;
       event : out Input_Event;
       found : out Boolean);

   --  True only when the compositor explicitly reported another queued event
   --  with the last input reply. It is a drain hint, never authority or an
   --  assertion that a later event cannot arrive.
   function Input_May_Remain (win : Window) return Boolean;

   --  Wheel deltas use a signed 32-bit wire field in payload1. Decode it
   --  without a range-checked modular-to-signed conversion in each app.
   function Pointer_Wheel_Delta (event : Input_Event) return Integer;

   --  Request compositor-owned pointer feedback for this surface.  Widget
   --  applications normally get this automatically from Apply_Pointer_Event.
   procedure Set_Pointer_Cursor
      (win : Window; cursor : CuBit.UI.Pointer_Cursor_Style);

   procedure Present
      (win : Window; damage : CuBit.UI.Rect);

   procedure Apply_Pointer_Event
      (interaction : in out Pointer_Interaction;
       ui : in out CuBit.UI.State.UI_State;
       controls : CuBit.UI.Controls.Control_Map;
       win : Window;
       event : Input_Event;
       dirty : in out CuBit.UI.Rect;
       repaint : Pointer_Repaint_Policy := Repaint_Changed_Controls);

   generic
      --  Run owns pointer bookkeeping and minimal damage. Render rebuilds the
      --  control map each frame through ordinary widget calls; Handle_Event
      --  only needs application semantics such as navigation or data reloads.
      ui : in out CuBit.UI.State.UI_State;
      controls : in out CuBit.UI.Controls.Control_Map;
      pointerRepaint : Pointer_Repaint_Policy := Repaint_Changed_Controls;
      with procedure Render
         (win : in out Window; damage : CuBit.UI.Rect);
      with procedure Handle_Event
         (win : in out Window;
          event : Input_Event;
          dirty : in out CuBit.UI.Rect;
          running : in out Boolean);
   procedure Run (win : in out Window);

   procedure Close (win : in out Window);

private
   type Pointer_Interaction is record
      hovered  : CuBit.UI.Controls.Control_ID :=
        CuBit.UI.Controls.NO_CONTROL;
      captured : CuBit.UI.Controls.Control_ID :=
        CuBit.UI.Controls.NO_CONTROL;
      cursor   : CuBit.UI.Pointer_Cursor_Style := CuBit.UI.Pointer_Default;
      controlsValid : Boolean := True;
   end record;

   type Window is record
      surfaceId : Unsigned_64 := 0;
      flags : Unsigned_64 := 0;
      bufferAddr : System.Address := System.Null_Address;
      bufferGrant : Unsigned_64 := 0;
      bufferPages : Unsigned_64 := 0;
      width : Natural := 0;
      height : Natural := 0;
      pitch : Natural := 0;
      lastEvent : Unsigned_64 := 0;
      inputMayRemain : Boolean := False;
      sentBye : Boolean := False;
   end record;
end CuBit.UI.App;
