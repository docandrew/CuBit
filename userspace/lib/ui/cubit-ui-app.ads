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

   type Window is private;

   procedure Open
      (win : in out Window;
       width, height : Natural;
       flags : Unsigned_64;
       ok : out Boolean);

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

   procedure Present
      (win : Window; damage : CuBit.UI.Rect);

   generic
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
      sentBye : Boolean := False;
   end record;
end CuBit.UI.App;
