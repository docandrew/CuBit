------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Virtio legacy (transitional) PCI transport layer.
--
--  Register layout and vring structures for virtio legacy devices
--  accessed via I/O ports (BAR0).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Virtio is

   ---------------------------------------------------------------------------
   --  Legacy device register offsets from BAR0 I/O port base
   ---------------------------------------------------------------------------
   REG_DEVICE_FEATURES   : constant Unsigned_16 := 16#00#;  -- 32-bit RO
   REG_DRIVER_FEATURES   : constant Unsigned_16 := 16#04#;  -- 32-bit RW
   REG_QUEUE_ADDRESS     : constant Unsigned_16 := 16#08#;  -- 32-bit RW (PFN)
   REG_QUEUE_SIZE        : constant Unsigned_16 := 16#0C#;  -- 16-bit RO
   REG_QUEUE_SELECT      : constant Unsigned_16 := 16#0E#;  -- 16-bit RW
   REG_QUEUE_NOTIFY      : constant Unsigned_16 := 16#10#;  -- 16-bit RW
   REG_DEVICE_STATUS     : constant Unsigned_16 := 16#12#;  -- 8-bit  RW
   REG_ISR_STATUS        : constant Unsigned_16 := 16#13#;  -- 8-bit  RO

   --  Virtio-net MAC address starts at offset 0x14 (6 bytes)
   REG_NET_MAC           : constant Unsigned_16 := 16#14#;

   ---------------------------------------------------------------------------
   --  Device status bits
   ---------------------------------------------------------------------------
   STATUS_RESET          : constant Unsigned_8 := 0;
   STATUS_ACKNOWLEDGE    : constant Unsigned_8 := 1;
   STATUS_DRIVER         : constant Unsigned_8 := 2;
   STATUS_DRIVER_OK      : constant Unsigned_8 := 4;
   STATUS_FEATURES_OK    : constant Unsigned_8 := 8;
   STATUS_FAILED         : constant Unsigned_8 := 128;

   ---------------------------------------------------------------------------
   --  Virtio-net feature bits
   ---------------------------------------------------------------------------
   VIRTIO_NET_F_MAC      : constant Unsigned_32 := Shift_Left (1, 5);
   VIRTIO_NET_F_STATUS   : constant Unsigned_32 := Shift_Left (1, 16);

   ---------------------------------------------------------------------------
   --  Vring descriptor flags
   ---------------------------------------------------------------------------
   VRING_DESC_F_NEXT     : constant Unsigned_16 := 1;
   VRING_DESC_F_WRITE    : constant Unsigned_16 := 2;

   ---------------------------------------------------------------------------
   --  Vring descriptor: 16 bytes
   ---------------------------------------------------------------------------
   type VringDesc is record
      addr  : Unsigned_64;
      len   : Unsigned_32;
      flags : Unsigned_16;
      next  : Unsigned_16;
   end record with Size => 128;

   for VringDesc use record
      addr  at 0  range 0 .. 63;
      len   at 8  range 0 .. 31;
      flags at 12 range 0 .. 15;
      next  at 14 range 0 .. 15;
   end record;

   ---------------------------------------------------------------------------
   --  Vring used element: 8 bytes
   ---------------------------------------------------------------------------
   type VringUsedElem is record
      id  : Unsigned_32;
      len : Unsigned_32;
   end record with Size => 64;

   for VringUsedElem use record
      id  at 0 range 0 .. 31;
      len at 4 range 0 .. 31;
   end record;

   ---------------------------------------------------------------------------
   --  Queue size we'll use (must be <= device's reported queue size)
   ---------------------------------------------------------------------------
   QUEUE_SIZE : constant := 256;

   type DescArray  is array (0 .. QUEUE_SIZE - 1) of VringDesc;
   type RingArray  is array (0 .. QUEUE_SIZE - 1) of Unsigned_16;
   type UsedArray  is array (0 .. QUEUE_SIZE - 1) of VringUsedElem;

   ---------------------------------------------------------------------------
   --  Vring available ring — must match virtio spec layout exactly
   --  Volatile: device reads these fields asynchronously via DMA.
   ---------------------------------------------------------------------------
   type VringAvail is record
      flags : Unsigned_16;
      idx   : Unsigned_16;
      ring  : RingArray;
   end record with Volatile;

   for VringAvail use record
      flags at 0 range 0 .. 15;
      idx   at 2 range 0 .. 15;
      ring  at 4 range 0 .. QUEUE_SIZE * 16 - 1;
   end record;

   ---------------------------------------------------------------------------
   --  Vring used ring — must match virtio spec layout exactly
   --  Volatile: device writes these fields asynchronously via DMA.
   ---------------------------------------------------------------------------
   type VringUsed is record
      flags : Unsigned_16;
      idx   : Unsigned_16;
      ring  : UsedArray;
   end record with Volatile;

   for VringUsed use record
      flags at 0 range 0 .. 15;
      idx   at 2 range 0 .. 15;
      ring  at 4 range 0 .. QUEUE_SIZE * 64 - 1;
   end record;

   ---------------------------------------------------------------------------
   --  Device operations (all via port I/O syscalls)
   ---------------------------------------------------------------------------
   procedure resetDevice (ioBase : Unsigned_16);
   procedure initDevice  (ioBase : Unsigned_16);
   function  readISR     (ioBase : Unsigned_16) return Unsigned_8;
   procedure selectQueue (ioBase : Unsigned_16; idx : Unsigned_16);
   function  getQueueSize (ioBase : Unsigned_16) return Unsigned_16;
   procedure setQueueAddr (ioBase : Unsigned_16; pfn : Unsigned_32);
   procedure notifyQueue (ioBase : Unsigned_16; idx : Unsigned_16);

end Virtio;
