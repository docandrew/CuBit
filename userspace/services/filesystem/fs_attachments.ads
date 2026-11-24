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
--  Device_Code, words (1) its Device_Ordinal and words (2) whether it is
--  the system disk (Data_Disk, System_Disk), offering a Read+Write copy
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
   Attach_Words : constant := 3;
   --  words (2): whether the disk is the installed system's root
   --  (storage.system-volume), which the filesystem then names @system.
   Data_Disk   : constant := 0;
   System_Disk : constant := 1;

   --  UFS_Device: a disk behind a generic storage driver (UFS); its
   --  volume (@ufs:N) is the GPT disk's Linux filesystem partition.
   --  USB_Disk_Device: a USB mass-storage disk (a stick, a card reader);
   --  its volume is @usb:N, N the lowest name free when it came (devmgr's
   --  Devmgr_USB_Disks, FS-034). Never the boot medium (that is the
   --  optical volume).
   type Device is (ATA_Device, NVMe_Device, Optical_Device, UFS_Device, USB_Disk_Device);
   for Device use (ATA_Device => 1, NVMe_Device => 2, Optical_Device => 3, UFS_Device => 4,
                   USB_Disk_Device => 5);

   function Device_Code (Item : Device) return Unsigned_64 is
     (Unsigned_64 (Device'Enum_Rep (Item)));

   --  FS-034: devmgr detaches a block device that went away (a USB stick
   --  pulled out): Detach_Label, words (0) = its Device_Code, words (1) its
   --  Device_Ordinal. Only from devmgr. The filesystem forgets the volume
   --  (its handles answer that it is gone, its endpoint is closed, the
   --  name is free for the next attach) and answers REPLY_OK, also when
   --  nothing is attached under that name (a repeated or late detach);
   --  REPLY_ERR only for a malformed or unauthorized request. A repeated
   --  attach of a name already attached is answered REPLY_OK (devmgr
   --  repeats an attach whose answer it did not get in time).
   Detach_Label : constant Unsigned_32 := 16#0091#;
   Detach_Words : constant := 2;

   --  A controller's place among those of its kind, from 0: below devmgr's
   --  PCI inventory size (checked in devmgr). The filesystem's volume list
   --  bounds how many it can name; past that it refuses the attachment.
   Maximum_Ordinal : constant := 255;
   subtype Device_Ordinal is Natural range 0 .. Maximum_Ordinal;
end Fs_Attachments;
