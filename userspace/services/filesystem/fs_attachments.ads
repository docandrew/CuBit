------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Block devices devmgr attaches to the running filesystem (CAP-002).
--
--  @description
--  A block driver that starts after the filesystem (ATA, NVMe, the USB
--  optical drive) reaches it by an offer: devmgr calls the filesystem with
--  CALL_OFFERING_CAPABILITY, Attach_Label, words (0) = the device's
--  Device_Code, offering a Read+Write copy of the driver's endpoint. The
--  filesystem accepts it only from devmgr (the kernel-stamped sender is the
--  process registered as DRIVER_DEVMGR) and only once per device, and
--  answers REPLY_OK, or REPLY_ERR without accepting. Nothing is placed into
--  the running filesystem unasked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Fs_Attachments with Pure is
   Attach_Label : constant Unsigned_32 := 16#0090#;
   Attach_Words : constant := 1;

   type Device is (ATA_Device, NVMe_Device, Optical_Device);
   for Device use (ATA_Device => 1, NVMe_Device => 2, Optical_Device => 3);

   function Device_Code (Item : Device) return Unsigned_64 is
     (Unsigned_64 (Device'Enum_Rep (Item)));
end Fs_Attachments;
