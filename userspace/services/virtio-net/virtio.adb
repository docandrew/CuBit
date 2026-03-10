------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Virtio legacy PCI transport layer - body.
--  All device register access via port I/O syscalls.
------------------------------------------------------------------------------
with CuBit.Messages; use CuBit.Messages;

package body Virtio is

   ---------------------------------------------------------------------------
   --  outb / inp8 / outp16 / inp16 / outp32 / inp32 wrappers
   ---------------------------------------------------------------------------
   procedure outb (port : Unsigned_16; val : Unsigned_8) is
      ignore : Unsigned_64;
   begin
      ignore := portOutp8 (port, val);
   end outb;

   function inb (port : Unsigned_16) return Unsigned_8 is
   begin
      return Unsigned_8 (portInp8 (port) and 16#FF#);
   end inb;

   procedure outw (port : Unsigned_16; val : Unsigned_16) is
      ignore : Unsigned_64;
   begin
      ignore := portOutp16 (port, val);
   end outw;

   function inw (port : Unsigned_16) return Unsigned_16 is
   begin
      return Unsigned_16 (portInp16 (port) and 16#FFFF#);
   end inw;

   procedure outl (port : Unsigned_16; val : Unsigned_32) is
      ignore : Unsigned_64;
   begin
      ignore := portOutp32 (port, val);
   end outl;

   function inl (port : Unsigned_16) return Unsigned_32 is
   begin
      return Unsigned_32 (portInp32 (port) and 16#FFFF_FFFF#);
   end inl;

   ---------------------------------------------------------------------------
   --  resetDevice - write 0 to device status register
   ---------------------------------------------------------------------------
   procedure resetDevice (ioBase : Unsigned_16) is
   begin
      outb (ioBase + REG_DEVICE_STATUS, STATUS_RESET);
   end resetDevice;

   ---------------------------------------------------------------------------
   --  initDevice - perform virtio device initialization sequence
   --  ACKNOWLEDGE -> DRIVER -> negotiate features -> DRIVER_OK
   ---------------------------------------------------------------------------
   procedure initDevice (ioBase : Unsigned_16) is
      devFeatures : Unsigned_32;
      drvFeatures : Unsigned_32;
      status      : Unsigned_8;
   begin
      --  1. Reset
      resetDevice (ioBase);

      --  2. Set ACKNOWLEDGE
      status := STATUS_ACKNOWLEDGE;
      outb (ioBase + REG_DEVICE_STATUS, status);

      --  3. Set DRIVER
      status := status or STATUS_DRIVER;
      outb (ioBase + REG_DEVICE_STATUS, status);

      --  4. Read device features, negotiate
      devFeatures := inl (ioBase + REG_DEVICE_FEATURES);

      --  We want MAC address if available
      drvFeatures := devFeatures and VIRTIO_NET_F_MAC;
      outl (ioBase + REG_DRIVER_FEATURES, drvFeatures);

      --  5. Set DRIVER_OK
      status := status or STATUS_DRIVER_OK;
      outb (ioBase + REG_DEVICE_STATUS, status);
   end initDevice;

   ---------------------------------------------------------------------------
   --  readISR - read and acknowledge interrupt status
   ---------------------------------------------------------------------------
   function readISR (ioBase : Unsigned_16) return Unsigned_8 is
   begin
      return inb (ioBase + REG_ISR_STATUS);
   end readISR;

   ---------------------------------------------------------------------------
   --  selectQueue - select a virtqueue by index
   ---------------------------------------------------------------------------
   procedure selectQueue (ioBase : Unsigned_16; idx : Unsigned_16) is
   begin
      outw (ioBase + REG_QUEUE_SELECT, idx);
   end selectQueue;

   ---------------------------------------------------------------------------
   --  getQueueSize - read the device's queue size for selected queue
   ---------------------------------------------------------------------------
   function getQueueSize (ioBase : Unsigned_16) return Unsigned_16 is
   begin
      return inw (ioBase + REG_QUEUE_SIZE);
   end getQueueSize;

   ---------------------------------------------------------------------------
   --  setQueueAddr - write the physical page frame number of the vring
   ---------------------------------------------------------------------------
   procedure setQueueAddr (ioBase : Unsigned_16; pfn : Unsigned_32) is
   begin
      outl (ioBase + REG_QUEUE_ADDRESS, pfn);
   end setQueueAddr;

   ---------------------------------------------------------------------------
   --  notifyQueue - kick the device to process a queue
   ---------------------------------------------------------------------------
   procedure notifyQueue (ioBase : Unsigned_16; idx : Unsigned_16) is
   begin
      outw (ioBase + REG_QUEUE_NOTIFY, idx);
   end notifyQueue;

end Virtio;
