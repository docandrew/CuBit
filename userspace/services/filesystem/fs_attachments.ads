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
--  Device_Code and words (1) its Device_Ordinal, offering a Read+Write copy
--  of the driver's endpoint. devmgr starts one driver per controller; the
--  ordinal is the controller's place among those of its kind in PCI
--  location order, and names its volume (@nvme:0, @nvme:1). The
--  filesystem accepts it only from devmgr (the kernel-stamped sender is the
--  process registered as DRIVER_DEVMGR) and only once per device, and
--  answers REPLY_OK, or REPLY_ERR without accepting. Nothing is placed into
--  the running filesystem unasked.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Fs_Attachments with Pure is
   Attach_Label : constant Unsigned_32 := 16#0090#;
   Attach_Words : constant := 2;

   --  UFS_Device: a disk behind a generic storage driver (UFS); its
   --  volume (@ufs:N) is the GPT disk's Linux filesystem partition.
   type Device is (ATA_Device, NVMe_Device, Optical_Device, UFS_Device);
   for Device use (ATA_Device => 1, NVMe_Device => 2, Optical_Device => 3, UFS_Device => 4);

   function Device_Code (Item : Device) return Unsigned_64 is
     (Unsigned_64 (Device'Enum_Rep (Item)));

   --  A controller's place among those of its kind, from 0: below devmgr's
   --  PCI inventory size (checked in devmgr). The filesystem's volume list
   --  bounds how many it can name; past that it refuses the attachment.
   Maximum_Ordinal : constant := 255;
   subtype Device_Ordinal is Natural range 0 .. Maximum_Ordinal;
end Fs_Attachments;
