------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  PS/2 Mouse client API
--
--  @description
--  Provides mouse event polling for userspace applications. Registers as
--  DRIVER_MOUSE and receives packed PS/2 mouse events from the kernel
--  mouse service via Poll_Event.
--
--  Mouse event packed format in words(0):
--    Bits  0-7:   buttons (L=0, R=1, M=2)
--    Bits  8-19:  dx (signed 12-bit)
--    Bits 20-31:  dy (signed 12-bit)
--    Bits 32-39:  dz (signed 8-bit scroll wheel)
--    Bits 40-47:  flags (bit 0 = hasWheel)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Mouse is

   type MouseEvent is record
      dx       : Integer_16;    --  Horizontal movement
      dy       : Integer_16;    --  Vertical movement
      dz       : Integer_8;     --  Scroll wheel
      buttons  : Unsigned_8;    --  Bit 0=left, 1=right, 2=middle
      hasWheel : Boolean;       --  True if wheel data is valid
   end record;

   BUTTON_LEFT   : constant Unsigned_8 := 1;
   BUTTON_RIGHT  : constant Unsigned_8 := 2;
   BUTTON_MIDDLE : constant Unsigned_8 := 4;

   ---------------------------------------------------------------------------
   --  init
   --  Register this process as the mouse driver (DRIVER_MOUSE).
   --  Call once at startup before polling.
   ---------------------------------------------------------------------------
   procedure init;

   ---------------------------------------------------------------------------
   --  poll
   --  Drain all pending mouse events from the kernel service into an
   --  internal ring buffer. Also drains keyboard events (label=1) back
   --  to the keyboard buffer if cubit_keyboard was initialized.
   ---------------------------------------------------------------------------
   procedure poll;

   ---------------------------------------------------------------------------
   --  get
   --  Get next mouse event from the internal buffer.
   --  Returns True if an event was available, False otherwise.
   ---------------------------------------------------------------------------
   function get (ev : out MouseEvent) return Boolean;

end CuBit.Mouse;
