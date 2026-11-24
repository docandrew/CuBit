------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The controller's USB disks as Block.Device.V1, on the block driver
--  contract (CuBit.Block_Driver, DEV-002): READ/WRITE(10) or (16) and
--  SYNCHRONIZE CACHE through XHCI's bulk-only transport, one command at a
--  time across the controller (BOT has no queueing; the bulk buffers are
--  shared).
--
--  @description
--  Each disk is reached only through its unit badge
--  (CuBit.Block_Unit_Badges): devmgr hands the filesystem a copy of
--  xhci.drv's endpoint badged with the unit, which is that disk's write
--  grant (never the boot medium's: it has no unit). A medium that is
--  write protected (MODE SENSE WP, or DATA PROTECT on a write) is read
--  only. When a disk goes away, Describe answers that there is no device,
--  every request through its badge fails at once, and a command in flight
--  is answered as failed: nothing waits on a missing disk.
------------------------------------------------------------------------------
with CuBit.Messages;
with CuBit.Block_Unit_Badges;

package USB_Disk_Service is
   --  Answers finished commands, cancels late ones.
   procedure Complete (Progressed : out Boolean);
   --  A command is in flight.
   function Busy return Boolean;
   --  Takes one request (already received) for Unit's disk.
   procedure Serve
     (Sender : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message;
      Unit : CuBit.Block_Unit_Badges.Unit)
     with Pre => not Busy;
end USB_Disk_Service;
