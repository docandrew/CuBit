------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The block requests on xhci.drv's endpoint, routed to the device they
--  are for: the boot medium (Optical_Service) or the USB disk
--  (USB_Disk_Service).
--
--  @description
--  devmgr attaches the boot medium with a plain copy of the endpoint and
--  each disk with a copy badged with its unit
--  (CuBit.Block_Unit_Badges, FS-034). A request carrying a unit badge is
--  that unit's disk's (a unit that is gone answers that there is no
--  device); an unbadged one is the boot medium's, read only; anything
--  else is refused. So the boot medium, which has no unit, is never
--  reachable for writing. Only the filesystem may use either. One request at a time, and none while a
--  disk is being probed or recovered (they wait in the kernel's queue).
------------------------------------------------------------------------------
package Storage_Requests is
   procedure Poll (Progressed : out Boolean);
end Storage_Requests;
