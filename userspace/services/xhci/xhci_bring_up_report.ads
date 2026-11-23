------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  How devmgr learns an xhci.drv controller's bring-up outcome.
--
--  @description
--  devmgr's configure message (OP_XHCI_CONFIGURE) is answered as soon as
--  the driver has checked it: REPLY_OK accepts it (no words), REPLY_ERR
--  refuses it. The bring-up itself (firmware handoff, reset, the boot
--  settle window, enumeration, the storage probe) runs after the answer,
--  so devmgr never waits on a controller's hardware while that driver, or
--  any other, may be calling devmgr (IPC DAG: synchronous calls only go one
--  way). Two controllers come up side by side.
--
--  Its outcome is this report, sent once on the driver's device-manager
--  endpoint (a synchronous send devmgr answers before acting on it):
--  * Ready_Label with Report_Words words: the root port count, the
--    connected port count and what the controller enumerated, as
--    Device_Bits. devmgr then unmasks the controller's interrupts and
--    attaches the optical volume to the first controller with USB storage.
--  * Failed_Label with Failure_Word: the XHCI.Init_Result position. devmgr
--    releases its vectors; the driver exits.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with CuBit.Driver_Readiness;

package XHCI_Bring_Up_Report with Pure, SPARK_Mode is
   Ready_Label  : constant Unsigned_32 := CuBit.Driver_Readiness.Ready_Label;
   Failed_Label : constant Unsigned_32 := CuBit.Driver_Readiness.Not_Present_Label;

   Report_Words   : constant := 3;
   Ports_Word     : constant := 0;
   Connected_Word : constant := 1;
   Devices_Word   : constant := 2;
   Failure_Word   : constant := 0;

   Keyboard_Bit : constant Unsigned_64 := 2 ** 0;
   Mouse_Bit    : constant Unsigned_64 := 2 ** 1;
   Storage_Bit  : constant Unsigned_64 := 2 ** 2;

   function Device_Bits (Keyboard, Mouse, Storage : Boolean) return Unsigned_64 is
     ((if Keyboard then Keyboard_Bit else 0) or (if Mouse then Mouse_Bit else 0) or
      (if Storage then Storage_Bit else 0));

   function Has (Bits, Bit : Unsigned_64) return Boolean is ((Bits and Bit) /= 0);
end XHCI_Bring_Up_Report;
