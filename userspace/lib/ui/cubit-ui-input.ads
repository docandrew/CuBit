------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Desktop input event encoding shared by CuBit.UI.App and widgets that
--  route input (CuBit.UI.Surfaces). No IPC, so it builds for hosted tests.
------------------------------------------------------------------------------

package CuBit.UI.Input is
   INPUT_NONE          : constant Unsigned_64 := 0;
   INPUT_KEY_DOWN      : constant Unsigned_64 := 1;
   INPUT_KEY_UP        : constant Unsigned_64 := 2;
   INPUT_POINTER_MOVE  : constant Unsigned_64 := 3;
   INPUT_POINTER_DOWN  : constant Unsigned_64 := 4;
   INPUT_POINTER_UP    : constant Unsigned_64 := 5;
   INPUT_TEXT          : constant Unsigned_64 := 6;
   INPUT_POINTER_WHEEL : constant Unsigned_64 := 7;
   INPUT_CONFIGURE     : constant Unsigned_64 := 8;
   INPUT_RESYNC        : constant Unsigned_64 := 9;

   type Input_Event is record
      kind     : Unsigned_64 := INPUT_NONE;
      serial   : Unsigned_64 := 0;
      payload0 : Unsigned_64 := 0;
      payload1 : Unsigned_64 := 0;
   end record;

   --  Pointer events: payload0 = x (bits 0..31) and y (bits 32..63).
   function Pointer_X (event : Input_Event) return Natural is
     (Natural (event.payload0 and 16#7FFF_FFFF#));
   function Pointer_Y (event : Input_Event) return Natural is
     (Natural (Shift_Right (event.payload0, 32) and 16#7FFF_FFFF#));

   --  Wheel deltas use a signed 32-bit wire field in payload1.
   function Pointer_Wheel_Delta (event : Input_Event) return Integer;
end CuBit.UI.Input;
