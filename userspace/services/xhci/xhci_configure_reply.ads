------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  xhci.drv's answer to devmgr's configure message (OP_XHCI_CONFIGURE)
--
--  @description
--  REPLY_OK with three words: the root port count, the connected port
--  count, and what the controller enumerated, as Device_Bits. devmgr starts
--  one xhci.drv per controller and logs each one's devices from it, and
--  attaches the optical volume to a controller that found USB storage.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package XHCI_Configure_Reply with Pure, SPARK_Mode is
   Reply_Words  : constant := 3;
   Ports_Word   : constant := 0;
   Connected_Word : constant := 1;
   Devices_Word : constant := 2;

   Keyboard_Bit : constant Unsigned_64 := 2 ** 0;
   Mouse_Bit    : constant Unsigned_64 := 2 ** 1;
   Storage_Bit  : constant Unsigned_64 := 2 ** 2;

   function Device_Bits (Keyboard, Mouse, Storage : Boolean) return Unsigned_64 is
     ((if Keyboard then Keyboard_Bit else 0) or (if Mouse then Mouse_Bit else 0) or
      (if Storage then Storage_Bit else 0));

   function Has (Bits, Bit : Unsigned_64) return Boolean is ((Bits and Bit) /= 0);
end XHCI_Configure_Reply;
