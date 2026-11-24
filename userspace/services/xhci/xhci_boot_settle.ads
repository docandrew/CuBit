------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  When xhci.drv's boot discovery window ends (XHCI.Initialize).
--
--  @description
--  After HCRST the devices attached before boot come back: a USB 2.0 port
--  shows its device at once, a SuperSpeed link trains (Polling, with no
--  connection yet), and a device whose SuperSpeed attempt fails connects
--  again on its USB 2.0 companion port. Hotplug takes HID devices and USB
--  disks but never the boot medium (Probe_Optical looks once, after this
--  window), so the window must not end while a port is still moving. It
--  ends once every root port has held still for Quiet_Ms (what Attachment
--  shows unchanged, none Moving), and at the latest after Limit_Ms.
--
--  The window used to be Limit_Ms every boot, on every controller. On the
--  IdeaPad's two controllers that was 6 s of devmgr's ~10 s wait (images
--  31-33, tests/xhci-bringup).
--
--  Reference: xHCI 1.2 section 5.4.8 (PORTSC, PLS); USB 2.0 section 7.1.7.3
--  (TATTDB, the 100 ms connect debounce: Quiet_Ms is three of them).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with XHCI_Registers; use XHCI_Registers;

package XHCI_Boot_Settle with SPARK_Mode => On, Pure is

   Quiet_Ms : constant := 300;
   Limit_Ms : constant := 3_000;

   --  What one look at a port compares with the last: connection, enable,
   --  reset, link state and speed; never the change bits (the window leaves
   --  them for hotplug to acknowledge).
   Attachment_Fields : constant Unsigned_32 :=
     PORTSC_CCS or PORTSC_PED or PORTSC_PR or
     PORTSC_PLS_MASK * 2 ** PORTSC_PLS_SHIFT or
     PORTSC_SPEED_MASK * 2 ** PORTSC_SPEED_SHIFT;

   function Attachment (Status : Unsigned_32) return Unsigned_32 is
     (Status and Attachment_Fields);

   --  Mid-change: in a reset, recovering, resuming, or a SuperSpeed link
   --  training with no connection yet. A USB 2.0 port in Polling with its
   --  device connected is not moving: it waits for software's reset.
   function Moving (Status : Unsigned_32) return Boolean is
     (In_Reset (Status)
      or else Link (Status) in LINK_RECOVERY | LINK_HOT_RESET | LINK_RESUME
      or else (not Connected (Status) and then Link (Status) = LINK_POLLING));

   type Verdict is (Keep_Waiting, Ports_Quiet, Window_Ceiling);

   --  Elapsed: since the controller started running; Still_For: since the
   --  last change any port showed (or an event arrived).
   function Decide (Elapsed, Still_For : Unsigned_64; Any_Moving : Boolean) return Verdict is
     (if Elapsed >= Limit_Ms then Window_Ceiling
      elsif not Any_Moving and then Still_For >= Quiet_Ms then Ports_Quiet
      else Keep_Waiting)
   with Post =>
     (case Decide'Result is
        when Window_Ceiling => Elapsed >= Limit_Ms,
        when Ports_Quiet    => Elapsed < Limit_Ms and then not Any_Moving
                               and then Still_For >= Quiet_Ms,
        when Keep_Waiting   => Elapsed < Limit_Ms
                               and then (Any_Moving or else Still_For < Quiet_Ms));

   function Verdict_Name (V : Verdict) return String is
     (case V is
        when Keep_Waiting   => "waiting",
        when Ports_Quiet    => "quiet",
        when Window_Ceiling => "ceiling");

end XHCI_Boot_Settle;
