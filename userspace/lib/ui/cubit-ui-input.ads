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
   INPUT_CLOSE_REQUEST : constant Unsigned_64 := 10;

   --  Why an INPUT_CLOSE_REQUEST came (payload0, Close_Reason'Enum_Rep):
   --  User_Close, the window's close (Desktop sends 0); System_Shutdown,
   --  procmgr's Stop during the root stop (PWR-003: CuBit.UI.App.Run turns
   --  the kernel's Stop control event into this event); Replace, the
   --  program is being replaced (LIVE-001; reserved: no switch of a UI app
   --  sends it yet).
   type Close_Reason is (User_Close, System_Shutdown, Replace);
   for Close_Reason use (User_Close => 0, System_Shutdown => 1, Replace => 2);

   type Input_Event is record
      kind     : Unsigned_64 := INPUT_NONE;
      serial   : Unsigned_64 := 0;
      payload0 : Unsigned_64 := 0;
      payload1 : Unsigned_64 := 0;
   end record;

   --  Key events: payload0 = set-1 scan code; payload1's low word holds the
   --  KEYMOD_* modifiers and its high word the key flags. Text events:
   --  payload1 holds the key flags alone. The values are the desktop
   --  protocol's (CuBit.Desktop_Protocol.Key_Repeat_Flag).
   function Close_Reason_Of (event : Input_Event) return Close_Reason is
     (if event.payload0 = Close_Reason'Enum_Rep (System_Shutdown) then System_Shutdown
      elsif event.payload0 = Close_Reason'Enum_Rep (Replace) then Replace
      else User_Close);

   function Close_Event (Reason : Close_Reason) return Input_Event is
     ((kind => INPUT_CLOSE_REQUEST, serial => 0, payload0 => Close_Reason'Enum_Rep (Reason), payload1 => 0));

   KEY_MODIFIERS_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   KEY_REPEAT_FLAG    : constant Unsigned_64 := 16#1_0000_0000#;

   --  A key press or text event the desktop synthesized because the key is
   --  held (auto-repeat). Commands and toggles usually act once per press
   --  and ignore these; editing and movement keys take them.
   function Is_Repeat (event : Input_Event) return Boolean is
     (event.kind in INPUT_KEY_DOWN | INPUT_TEXT and then
      (event.payload1 and KEY_REPEAT_FLAG) /= 0);

   --  Key events: the KEYMOD_* bits alone.
   function Key_Modifiers (event : Input_Event) return Unsigned_64 is
     (event.payload1 and KEY_MODIFIERS_MASK);

   --  Pointer move, down and up events: payload1's low word holds the
   --  held buttons and its high word the KEYMOD_* modifiers held when the
   --  pointer event happened (the layout INPUT_RESYNC uses). Apps take a
   --  click's modifiers from here, never from the last key event: a key
   --  release can go to another window.
   POINTER_BUTTONS_MASK : constant Unsigned_64 := 16#FFFF_FFFF#;
   POINTER_MODIFIERS_SHIFT : constant := 32;
   function Pointer_Buttons (event : Input_Event) return Unsigned_64 is
     (event.payload1 and POINTER_BUTTONS_MASK);
   function Pointer_Modifiers (event : Input_Event) return Unsigned_64 is
     (Shift_Right (event.payload1, POINTER_MODIFIERS_SHIFT) and KEY_MODIFIERS_MASK);

   --  Pointer events: payload0 = x (bits 0..31) and y (bits 32..63).
   function Pointer_X (event : Input_Event) return Natural is
     (Natural (event.payload0 and 16#7FFF_FFFF#));
   function Pointer_Y (event : Input_Event) return Natural is
     (Natural (Shift_Right (event.payload0, 32) and 16#7FFF_FFFF#));

   --  Wheel deltas use a signed 32-bit wire field in payload1.
   function Pointer_Wheel_Delta (event : Input_Event) return Integer;
end CuBit.UI.Input;
