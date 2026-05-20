------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  VirtIO-GPU driver bring-up.
--
--  This is a real modern virtio-pci control-queue path: devmgr discovers the
--  PCI transport capabilities, maps the common/notify/device BAR, allocates
--  DMA, then this driver creates a 2D resource, attaches backing memory,
--  assigns scanout 0, transfers pixels to the host, and flushes the resource.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;

procedure main is
   use ASCII;

   BAR_VIRT_BASE : constant Unsigned_64 := 16#0000_6000_4000_0000#;
   DMA_BASE      : constant System.Address :=
      To_Address (16#0000_7000_0000_0000#);

   QUEUE_SIZE : constant Natural := 16;
   CTRL_QUEUE : constant Unsigned_16 := 0;

   DESC_OFF  : constant Storage_Offset := 16#0000#;
   AVAIL_OFF : constant Storage_Offset := 16#1000#;
   USED_OFF  : constant Storage_Offset := 16#2000#;
   CMD_OFF   : constant Storage_Offset := 16#3000#;
   RESP_OFF  : constant Storage_Offset := 16#4000#;
   FB_OFF    : constant Storage_Offset := 16#100000#;

   FB_W : constant Unsigned_32 := 1024;
   FB_H : constant Unsigned_32 := 768;
   FB_BYTES : constant Unsigned_32 := FB_W * FB_H * 4;

   VIRTIO_STATUS_ACKNOWLEDGE : constant Unsigned_8 := 1;
   VIRTIO_STATUS_DRIVER      : constant Unsigned_8 := 2;
   VIRTIO_STATUS_DRIVER_OK   : constant Unsigned_8 := 4;
   VIRTIO_STATUS_FEATURES_OK : constant Unsigned_8 := 8;
   VIRTIO_STATUS_FAILED      : constant Unsigned_8 := 128;

   REG_DEVICE_FEATURE_SELECT : constant Unsigned_64 := 0;
   REG_DEVICE_FEATURE        : constant Unsigned_64 := 4;
   REG_DRIVER_FEATURE_SELECT : constant Unsigned_64 := 8;
   REG_DRIVER_FEATURE        : constant Unsigned_64 := 12;
   REG_NUM_QUEUES            : constant Unsigned_64 := 18;
   REG_DEVICE_STATUS         : constant Unsigned_64 := 20;
   REG_QUEUE_SELECT          : constant Unsigned_64 := 22;
   REG_QUEUE_SIZE            : constant Unsigned_64 := 24;
   REG_QUEUE_ENABLE          : constant Unsigned_64 := 28;
   REG_QUEUE_NOTIFY_OFF      : constant Unsigned_64 := 30;
   REG_QUEUE_DESC            : constant Unsigned_64 := 32;
   REG_QUEUE_DRIVER          : constant Unsigned_64 := 40;
   REG_QUEUE_DEVICE          : constant Unsigned_64 := 48;

   VRING_DESC_F_NEXT  : constant Unsigned_16 := 1;
   VRING_DESC_F_WRITE : constant Unsigned_16 := 2;

   CMD_GET_DISPLAY_INFO    : constant Unsigned_32 := 16#0100#;
   CMD_RESOURCE_CREATE_2D  : constant Unsigned_32 := 16#0101#;
   CMD_SET_SCANOUT         : constant Unsigned_32 := 16#0103#;
   CMD_RESOURCE_FLUSH      : constant Unsigned_32 := 16#0104#;
   CMD_TRANSFER_TO_HOST_2D : constant Unsigned_32 := 16#0105#;
   CMD_RESOURCE_ATTACH     : constant Unsigned_32 := 16#0106#;

   RESP_OK_NODATA       : constant Unsigned_32 := 16#1100#;
   RESP_OK_DISPLAY_INFO : constant Unsigned_32 := 16#1101#;

   OP_GPU_GET_INFO      : constant Unsigned_32 := 16#0A00#;
   OP_GPU_ATTACH_BUFFER : constant Unsigned_32 := 16#0A01#;
   OP_GPU_PRESENT_RECT  : constant Unsigned_32 := 16#0A02#;
   OP_GPU_CLEAR         : constant Unsigned_32 := 16#0A03#;
   OP_GPU_GET_STATUS    : constant Unsigned_32 := 16#0A04#;
   OP_GPU_MAP_FRAMEBUFFER : constant Unsigned_32 := 16#0A05#;
   OP_GPU_FLUSH_RECT    : constant Unsigned_32 := 16#0A06#;

   GPU_OK              : constant Unsigned_64 := 0;
   GPU_ERR_BAD_STATE   : constant Unsigned_64 := 3;
   GPU_ERR_UNSUPPORTED : constant Unsigned_64 := 5;

   FORMAT_B8G8R8X8_UNORM : constant Unsigned_32 := 2;
   SUBMIT_POLL_LIMIT : constant Natural := 500_000;

   --  Per-command tracing is useful while bringing up the virtqueue, but it is
   --  catastrophic once the display server starts presenting frames. Each frame
   --  issues transfer/flush commands, and debugPrint goes through the kernel
   --  text path. Keep startup/error logging live and leave this off by default.
   TRACE_COMMANDS : constant Boolean := False;

   GRANT_REGION_BASE : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Unsigned_64 := 4096 * 4096;

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

   type DescArray is array (0 .. QUEUE_SIZE - 1) of VringDesc;
   type RingArray is array (0 .. QUEUE_SIZE - 1) of Unsigned_16;

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

   type VringUsedElem is record
      id  : Unsigned_32;
      len : Unsigned_32;
   end record with Size => 64;
   for VringUsedElem use record
      id  at 0 range 0 .. 31;
      len at 4 range 0 .. 31;
   end record;

   type UsedArray is array (0 .. QUEUE_SIZE - 1) of VringUsedElem;
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

   descs : DescArray with Import, Address => DMA_BASE + DESC_OFF, Volatile;
   avail : VringAvail with Import, Address => DMA_BASE + AVAIL_OFF, Volatile;
   used  : VringUsed with Import, Address => DMA_BASE + USED_OFF, Volatile;

   dmaPhys : Unsigned_64 := 0;
   barPhys : Unsigned_64 := 0;
   commonOff : Unsigned_64 := 0;
   notifyOff : Unsigned_64 := 0;
   isrOff : Unsigned_64 := 0;
   notifyMult : Unsigned_64 := 0;
   gpuPrimary : Boolean := False;
   lastUsedIdx : Unsigned_16 := 0;
   nextDesc : Natural := 0;
   srcAddr : System.Address := System.Null_Address;
   srcWidth : Natural := 0;
   srcHeight : Natural := 0;
   srcPitch : Natural := 0;

   function memcpy
      (dest : System.Address;
       src  : System.Address;
       len  : Storage_Count)
      return System.Address with
      Import => True,
      Convention => C,
      External_Name => "memcpy";

   procedure printDec (val : Unsigned_64) is
      buf : String (1 .. 20);
      pos : Natural := buf'Last;
      v   : Unsigned_64 := val;
   begin
      if v = 0 then
         debugPrint ("0");
         return;
      end if;
      while v > 0 loop
         buf (pos) := Character'Val (Character'Pos ('0') +
                                      Natural (v mod 10));
         v := v / 10;
         pos := pos - 1;
      end loop;
      debugPrint (buf (pos + 1 .. buf'Last));
   end printDec;

   procedure signalReady (label : Unsigned_32) is
      ignore : MessageTag;
   begin
      ignore := capSend (15,
         (tag      => (label => label, length => 0, flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
   end signalReady;

   procedure fail (why : String) is
      ignore : Unsigned_64;
   begin
      debugPrint ("virtio-gpu: " & why & LF);
      signalReady (16#FF01#);
      ignore := syscall (SYSCALL_EXIT, 1);
   end fail;

   procedure trace (what : String) is
   begin
      if TRACE_COMMANDS then
         debugPrint ("virtio-gpu: " & what & LF);
      end if;
   end trace;

   procedure printHex32 (val : Unsigned_32) is
      hex : constant String := "0123456789ABCDEF";
      outStr : String (1 .. 8);
      v : Unsigned_32 := val;
   begin
      for i in reverse outStr'Range loop
         outStr (i) := hex (Natural (v and 16#F#) + 1);
         v := Shift_Right (v, 4);
      end loop;
      debugPrint (outStr);
   end printHex32;

   function mmioAddr (offset : Unsigned_64) return System.Address is
   begin
      return To_Address (Integer_Address (BAR_VIRT_BASE + commonOff + offset));
   end mmioAddr;

   procedure write8 (offset : Unsigned_64; value : Unsigned_8) is
      reg : Unsigned_8 with Import, Address => mmioAddr (offset), Volatile;
   begin
      reg := value;
   end write8;

   function read8 (offset : Unsigned_64) return Unsigned_8 is
      reg : Unsigned_8 with Import, Address => mmioAddr (offset), Volatile;
   begin
      return reg;
   end read8;

   procedure write16 (offset : Unsigned_64; value : Unsigned_16) is
      reg : Unsigned_16 with Import, Address => mmioAddr (offset), Volatile;
   begin
      reg := value;
   end write16;

   function read16 (offset : Unsigned_64) return Unsigned_16 is
      reg : Unsigned_16 with Import, Address => mmioAddr (offset), Volatile;
   begin
      return reg;
   end read16;

   procedure write32 (offset : Unsigned_64; value : Unsigned_32) is
      reg : Unsigned_32 with Import, Address => mmioAddr (offset), Volatile;
   begin
      reg := value;
   end write32;

   function read32 (offset : Unsigned_64) return Unsigned_32 is
      reg : Unsigned_32 with Import, Address => mmioAddr (offset), Volatile;
   begin
      return reg;
   end read32;

   procedure write64 (offset : Unsigned_64; value : Unsigned_64) is
   begin
      write32 (offset, Unsigned_32 (value and 16#FFFF_FFFF#));
      write32 (offset + 4, Unsigned_32 (Shift_Right (value, 32)));
   end write64;

   procedure notifyQueue is
      notifyAddr : constant System.Address :=
         To_Address (Integer_Address
           (BAR_VIRT_BASE + notifyOff +
            Unsigned_64 (read16 (REG_QUEUE_NOTIFY_OFF)) * notifyMult));
      reg : Unsigned_16 with Import, Address => notifyAddr, Volatile;
   begin
      reg := CTRL_QUEUE;
   end notifyQueue;

   procedure zeroDma is
      bytes : array (0 .. 16#10_FFFF#) of Unsigned_8
        with Import, Address => DMA_BASE;
   begin
      for i in bytes'Range loop
         bytes (i) := 0;
      end loop;
   end zeroDma;

   procedure put32 (base : Storage_Offset; off : Storage_Offset;
                    value : Unsigned_32) is
      v : Unsigned_32 with Import, Address => DMA_BASE + base + off;
   begin
      v := value;
   end put32;

   procedure put64 (base : Storage_Offset; off : Storage_Offset;
                    value : Unsigned_64) is
      v : Unsigned_64 with Import, Address => DMA_BASE + base + off;
   begin
      v := value;
   end put64;

   function get32 (base : Storage_Offset; off : Storage_Offset)
                   return Unsigned_32 is
      v : Unsigned_32 with Import, Address => DMA_BASE + base + off;
   begin
      return v;
   end get32;

   function toAddr (x : Unsigned_64) return System.Address is
   begin
      return To_Address (Integer_Address (x));
   end toAddr;

   procedure beginCmd (cmd : Unsigned_32) is
   begin
      for i in Storage_Offset range 0 .. 511 loop
         declare
            b : Unsigned_8 with Import, Address => DMA_BASE + CMD_OFF + i;
         begin
            b := 0;
         end;
      end loop;
      for i in Storage_Offset range 0 .. 255 loop
         declare
            b : Unsigned_8 with Import, Address => DMA_BASE + RESP_OFF + i;
         begin
            b := 0;
         end;
      end loop;
      put32 (CMD_OFF, 0, cmd);
   end beginCmd;

   function submitCmd (cmdLen : Unsigned_32; respLen : Unsigned_32;
                       expected : Unsigned_32) return Boolean is
      id : constant Natural := nextDesc;
      polls : Natural := 0;
      ignoreSleep : Unsigned_64;
      typ : Unsigned_32;
   begin
      if TRACE_COMMANDS then
         debugPrint ("virtio-gpu: submit cmd=0x");
         printHex32 (get32 (CMD_OFF, 0));
         debugPrint (" expected=0x");
         printHex32 (expected);
         debugPrint ("" & LF);
      end if;

      nextDesc := (nextDesc + 2) mod QUEUE_SIZE;
      descs (id) :=
        (addr  => dmaPhys + Unsigned_64 (CMD_OFF),
         len   => cmdLen,
         flags => VRING_DESC_F_NEXT,
         next  => Unsigned_16 ((id + 1) mod QUEUE_SIZE));
      descs ((id + 1) mod QUEUE_SIZE) :=
        (addr  => dmaPhys + Unsigned_64 (RESP_OFF),
         len   => respLen,
         flags => VRING_DESC_F_WRITE,
         next  => 0);

      avail.ring (Natural (avail.idx mod Unsigned_16 (QUEUE_SIZE))) :=
         Unsigned_16 (id);
      avail.idx := avail.idx + 1;
      trace ("notify queue");
      notifyQueue;

      while used.idx = lastUsedIdx and then polls < SUBMIT_POLL_LIMIT loop
         if (polls mod 50_000) = 0 then
            ignoreSleep := syscall (SYSCALL_SLEEP, 1);
         end if;
         polls := polls + 1;
      end loop;
      if used.idx = lastUsedIdx then
         debugPrint ("virtio-gpu: command timeout cmd=0x");
         printHex32 (get32 (CMD_OFF, 0));
         debugPrint (" last_used=");
         printDec (Unsigned_64 (lastUsedIdx));
         debugPrint (" used=");
         printDec (Unsigned_64 (used.idx));
         debugPrint ("" & LF);
         return False;
      end if;

      lastUsedIdx := lastUsedIdx + 1;
      typ := get32 (RESP_OFF, 0);
      if typ /= expected then
         debugPrint ("virtio-gpu: unexpected response=");
         printDec (Unsigned_64 (typ));
         debugPrint (" expected=");
         printDec (Unsigned_64 (expected));
         debugPrint ("" & LF);
      end if;
      return typ = expected;
   end submitCmd;

   procedure initTransport is
      status : Unsigned_8;
      qsz : Unsigned_16;
      ret : Unsigned_64;
   begin
      trace ("map BAR");
      ret := syscall (SYSCALL_MAP_DEVICE, barPhys, BAR_VIRT_BASE, 16);
      if ret = Unsigned_64'Last then
         fail ("BAR map failed");
         return;
      end if;
      trace ("BAR mapped");

      trace ("clear DMA");
      zeroDma;
      trace ("DMA clear complete");

      --  Every MMIO access below is logged before it happens. Primary
      --  virtio-vga bring-up can currently wedge the emulator, so the last
      --  serial line is the breadcrumb that tells us which register access or
      --  transport transition was unsafe.
      if gpuPrimary then
         --  When VirtIO VGA is the primary visible adapter, QEMU's GTK
         --  frontend can wedge if the guest hard-resets the display device
         --  after firmware/GRUB have already used it for scanout. The
         --  headless path tolerates the reset, but the interactive path does
         --  not. Separate virtio-gpu-pci devices still take the normal reset.
         trace ("skip primary device reset");
      else
         trace ("reset device");
         write8 (REG_DEVICE_STATUS, 0);
      end if;

      status := VIRTIO_STATUS_ACKNOWLEDGE;
      trace ("set ACKNOWLEDGE");
      write8 (REG_DEVICE_STATUS, status);
      status := status or VIRTIO_STATUS_DRIVER;
      trace ("set DRIVER");
      write8 (REG_DEVICE_STATUS, status);

      trace ("select feature page 0");
      write32 (REG_DEVICE_FEATURE_SELECT, 0);
      declare
         --  If primary virtio-vga freezes before features0 prints, the
         --  device-feature MMIO read is the next suspect.
         pragma Warnings (Off, "variable * is read but never assigned");
         devFeatures : constant Unsigned_32 := read32 (REG_DEVICE_FEATURE);
         pragma Warnings (On, "variable * is read but never assigned");
      begin
         debugPrint ("virtio-gpu: features0=");
         printDec (Unsigned_64 (devFeatures));
         debugPrint ("" & LF);
      end;
      trace ("publish empty feature set");
      write32 (REG_DRIVER_FEATURE_SELECT, 0);
      write32 (REG_DRIVER_FEATURE, 0);
      status := status or VIRTIO_STATUS_FEATURES_OK;
      trace ("set FEATURES_OK");
      write8 (REG_DEVICE_STATUS, status);
      if (read8 (REG_DEVICE_STATUS) and VIRTIO_STATUS_FEATURES_OK) = 0 then
         write8 (REG_DEVICE_STATUS, status or VIRTIO_STATUS_FAILED);
         fail ("feature negotiation failed");
         return;
      end if;

      trace ("select control queue");
      write16 (REG_QUEUE_SELECT, CTRL_QUEUE);
      qsz := read16 (REG_QUEUE_SIZE);
      debugPrint ("virtio-gpu: control qsz=");
      printDec (Unsigned_64 (qsz));
      debugPrint ("" & LF);
      if qsz < Unsigned_16 (QUEUE_SIZE) then
         fail ("control queue too small");
         return;
      end if;
      trace ("configure control queue");
      write16 (REG_QUEUE_SIZE, Unsigned_16 (QUEUE_SIZE));
      write64 (REG_QUEUE_DESC, dmaPhys + Unsigned_64 (DESC_OFF));
      write64 (REG_QUEUE_DRIVER, dmaPhys + Unsigned_64 (AVAIL_OFF));
      write64 (REG_QUEUE_DEVICE, dmaPhys + Unsigned_64 (USED_OFF));
      write16 (REG_QUEUE_ENABLE, 1);

      status := status or VIRTIO_STATUS_DRIVER_OK;
      trace ("set DRIVER_OK");
      write8 (REG_DEVICE_STATUS, status);
      debugPrint ("virtio-gpu: transport ready queues=");
      printDec (Unsigned_64 (read16 (REG_NUM_QUEUES)));
      debugPrint ("" & LF);
   end initTransport;

   procedure paintFramebuffer is
      pixels : array (0 .. Natural (FB_W * FB_H) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + FB_OFF;
      color : Unsigned_32;
      x : Natural;
      y : Natural;
   begin
      for i in pixels'Range loop
         x := i mod Natural (FB_W);
         y := i / Natural (FB_W);
         if x < 8 or else y < 8 or else
            x >= Natural (FB_W) - 8 or else y >= Natural (FB_H) - 8
         then
            color := 16#00FF_FFFF#;
         elsif x < Natural (FB_W) / 3 then
            color := 16#0030_6FE0#;
         elsif x < (Natural (FB_W) * 2) / 3 then
            color := 16#00E0_D040#;
         else
            color := 16#00D040_40#;
         end if;
         pixels (i) := color;
      end loop;
   end paintFramebuffer;

   procedure initGpu is
      ok : Boolean;
      width : Unsigned_32;
      height : Unsigned_32;
      enabled : Unsigned_32;
      RESOURCE_ID : constant Unsigned_32 := 1;
   begin
      trace ("cmd GET_DISPLAY_INFO");
      beginCmd (CMD_GET_DISPLAY_INFO);
      ok := submitCmd (24, 408, RESP_OK_DISPLAY_INFO);
      if not ok then
         fail ("GET_DISPLAY_INFO failed");
         return;
      end if;

      width := get32 (RESP_OFF, 24 + 8);
      height := get32 (RESP_OFF, 24 + 12);
      enabled := get32 (RESP_OFF, 24 + 16);
      debugPrint ("virtio-gpu: scanout0 ");
      printDec (Unsigned_64 (width));
      debugPrint ("x");
      printDec (Unsigned_64 (height));
      debugPrint (" enabled=");
      printDec (Unsigned_64 (enabled));
      debugPrint ("" & LF);

      trace ("paint test framebuffer");
      paintFramebuffer;
      trace ("test framebuffer painted");

      trace ("cmd RESOURCE_CREATE_2D");
      beginCmd (CMD_RESOURCE_CREATE_2D);
      put32 (CMD_OFF, 24, RESOURCE_ID);
      put32 (CMD_OFF, 28, FORMAT_B8G8R8X8_UNORM);
      put32 (CMD_OFF, 32, FB_W);
      put32 (CMD_OFF, 36, FB_H);
      ok := submitCmd (40, 24, RESP_OK_NODATA);
      if not ok then
         fail ("RESOURCE_CREATE_2D failed");
         return;
      end if;

      trace ("cmd RESOURCE_ATTACH_BACKING");
      beginCmd (CMD_RESOURCE_ATTACH);
      put32 (CMD_OFF, 24, RESOURCE_ID);
      put32 (CMD_OFF, 28, 1);
      put64 (CMD_OFF, 32, dmaPhys + Unsigned_64 (FB_OFF));
      put32 (CMD_OFF, 40, FB_BYTES);
      put32 (CMD_OFF, 44, 0);
      ok := submitCmd (48, 24, RESP_OK_NODATA);
      if not ok then
         fail ("RESOURCE_ATTACH_BACKING failed");
         return;
      end if;

      trace ("cmd TRANSFER_TO_HOST_2D");
      beginCmd (CMD_TRANSFER_TO_HOST_2D);
      put32 (CMD_OFF, 24, 0);
      put32 (CMD_OFF, 28, 0);
      put32 (CMD_OFF, 32, FB_W);
      put32 (CMD_OFF, 36, FB_H);
      put64 (CMD_OFF, 40, 0);
      put32 (CMD_OFF, 48, RESOURCE_ID);
      put32 (CMD_OFF, 52, 0);
      ok := submitCmd (56, 24, RESP_OK_NODATA);
      if not ok then
         fail ("TRANSFER_TO_HOST_2D failed");
         return;
      end if;

      trace ("cmd SET_SCANOUT");
      beginCmd (CMD_SET_SCANOUT);
      put32 (CMD_OFF, 24, 0);
      put32 (CMD_OFF, 28, 0);
      put32 (CMD_OFF, 32, FB_W);
      put32 (CMD_OFF, 36, FB_H);
      put32 (CMD_OFF, 40, 0);
      put32 (CMD_OFF, 44, RESOURCE_ID);
      ok := submitCmd (48, 24, RESP_OK_NODATA);
      if not ok then
         fail ("SET_SCANOUT failed");
         return;
      end if;

      trace ("cmd RESOURCE_FLUSH");
      beginCmd (CMD_RESOURCE_FLUSH);
      put32 (CMD_OFF, 24, 0);
      put32 (CMD_OFF, 28, 0);
      put32 (CMD_OFF, 32, FB_W);
      put32 (CMD_OFF, 36, FB_H);
      put32 (CMD_OFF, 40, RESOURCE_ID);
      put32 (CMD_OFF, 44, 0);
      ok := submitCmd (48, 24, RESP_OK_NODATA);
      if not ok then
         fail ("RESOURCE_FLUSH failed");
         return;
      end if;

      debugPrint ("virtio-gpu: scanout test frame presented" & LF);
   end initGpu;

   procedure copySourceRect (x, y, w, h : Natural) is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      ignore : System.Address;
   begin
      if srcAddr = System.Null_Address then
         return;
      end if;
      if w = 0 or else h = 0 or else x >= Natural (FB_W) or else
         y >= Natural (FB_H)
      then
         return;
      end if;

      if maxX > Natural (FB_W) then
         maxX := Natural (FB_W);
      end if;
      if maxY > Natural (FB_H) then
         maxY := Natural (FB_H);
      end if;
      if maxX > srcWidth then
         maxX := srcWidth;
      end if;
      if maxY > srcHeight then
         maxY := srcHeight;
      end if;
      if x >= maxX or else y >= maxY then
         return;
      end if;

      if x = 0 and then maxX = Natural (FB_W) and then
         srcPitch = Natural (FB_W) * 4
      then
         ignore := memcpy
           (DMA_BASE + FB_OFF + Storage_Offset (y * Natural (FB_W) * 4),
            srcAddr + Storage_Offset (y * srcPitch),
            Storage_Count ((maxY - y) * srcPitch));
         return;
      end if;

      for row in y .. maxY - 1 loop
         ignore := memcpy
           (DMA_BASE + FB_OFF +
              Storage_Offset ((row * Natural (FB_W) + x) * 4),
            srcAddr + Storage_Offset (row * srcPitch + x * 4),
            Storage_Count ((maxX - x) * 4));
      end loop;
   end copySourceRect;

   function transferAndFlush (x, y, w, h : Natural) return Boolean is
      maxX : Natural := x + w;
      maxY : Natural := y + h;
      backingOffset : Unsigned_64;
      ok : Boolean;
   begin
      if w = 0 or else h = 0 or else x >= Natural (FB_W) or else
         y >= Natural (FB_H)
      then
         return True;
      end if;

      if maxX > Natural (FB_W) then
         maxX := Natural (FB_W);
      end if;
      if maxY > Natural (FB_H) then
         maxY := Natural (FB_H);
      end if;

      backingOffset :=
        Unsigned_64 ((y * Natural (FB_W) + x) * 4);

      beginCmd (CMD_TRANSFER_TO_HOST_2D);
      put32 (CMD_OFF, 24, Unsigned_32 (x));
      put32 (CMD_OFF, 28, Unsigned_32 (y));
      put32 (CMD_OFF, 32, Unsigned_32 (maxX - x));
      put32 (CMD_OFF, 36, Unsigned_32 (maxY - y));
      --  The transfer rectangle is in resource coordinates, and the backing
      --  offset must point at the same top-left pixel inside the linear host
      --  backing. Using zero here for partial damage copies the beginning of
      --  the framebuffer into arbitrary screen rectangles, which looks like
      --  cursor/window movement erasing or smearing unrelated pixels.
      put64 (CMD_OFF, 40, backingOffset);
      put32 (CMD_OFF, 48, 1);
      put32 (CMD_OFF, 52, 0);
      ok := submitCmd (56, 24, RESP_OK_NODATA);
      if not ok then
         return False;
      end if;

      beginCmd (CMD_RESOURCE_FLUSH);
      put32 (CMD_OFF, 24, Unsigned_32 (x));
      put32 (CMD_OFF, 28, Unsigned_32 (y));
      put32 (CMD_OFF, 32, Unsigned_32 (maxX - x));
      put32 (CMD_OFF, 36, Unsigned_32 (maxY - y));
      put32 (CMD_OFF, 40, 1);
      put32 (CMD_OFF, 44, 0);
      return submitCmd (48, 24, RESP_OK_NODATA);
   end transferAndFlush;

   procedure clearFb (color : Unsigned_32) is
      pixels : array (0 .. Natural (FB_W * FB_H) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + FB_OFF;
   begin
      for i in pixels'Range loop
         pixels (i) := color;
      end loop;
   end clearFb;

   procedure handleRequest (from : ProcessID; request : Message) is
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
      ok : Boolean;
   begin
      case request.tag.label is
         when OP_GPU_GET_INFO =>
            replyMsg.tag := (label => OP_GPU_GET_INFO,
                             length => 4, flags => 0, badge => 0);
            replyMsg.words (0) := Unsigned_64 (FB_W);
            replyMsg.words (1) := Unsigned_64 (FB_H);
            replyMsg.words (2) := Unsigned_64 (FB_W) * 4;
            replyMsg.words (3) := 32;

         when OP_GPU_GET_STATUS =>
            replyMsg.tag := (label => OP_GPU_GET_STATUS,
                             length => 4, flags => 0, badge => 0);
            replyMsg.words (0) := GPU_OK;
            replyMsg.words (1) := 1; -- scanout resource initialized
            replyMsg.words (2) := Unsigned_64 (FB_W);
            replyMsg.words (3) := Unsigned_64 (FB_H);

         when OP_GPU_MAP_FRAMEBUFFER =>
            declare
               pages : constant Natural :=
                  Natural ((Unsigned_64 (FB_BYTES) + 4095) / 4096);
               gid   : Unsigned_64;
               grantOk : Boolean;
            begin
               replyMsg.tag := (label => OP_GPU_MAP_FRAMEBUFFER,
                                length => 4, flags => 0, badge => 0);
               createGrant
                 (grantee   => from,
                  localAddr => DMA_BASE + FB_OFF,
                  numPages  => pages,
                  readWrite => True,
                  grantId   => gid,
                  success   => grantOk);
               if grantOk then
                  replyMsg.words (0) := GPU_OK;
                  replyMsg.words (1) := gid;
                  replyMsg.words (2) := Unsigned_64 (FB_W) or
                     Shift_Left (Unsigned_64 (FB_H), 32);
                  replyMsg.words (3) := Unsigned_64 (FB_W) * 4;
               else
                  replyMsg.words (0) := GPU_ERR_BAD_STATE;
               end if;
            end;

         when OP_GPU_ATTACH_BUFFER =>
            replyMsg.tag := (label => OP_GPU_ATTACH_BUFFER,
                             length => 1, flags => 0, badge => 0);
            if request.words (1) = 0 or else request.words (2) = 0 or else
               request.words (1) > Unsigned_64 (FB_W) or else
               request.words (2) > Unsigned_64 (FB_H) or else
               request.words (3) < request.words (1) * 4
            then
               replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
            else
               srcAddr := toAddr
                 (GRANT_REGION_BASE + request.words (0) * GRANT_SLOT_SIZE);
               srcWidth := Natural (request.words (1));
               srcHeight := Natural (request.words (2));
               srcPitch := Natural (request.words (3));
               replyMsg.words (0) := GPU_OK;
               debugPrint ("virtio-gpu: display buffer attached" & LF);
            end if;

         when OP_GPU_PRESENT_RECT =>
            replyMsg.tag := (label => OP_GPU_PRESENT_RECT,
                             length => 1, flags => 0, badge => 0);
            if srcAddr = System.Null_Address then
               replyMsg.words (0) := GPU_ERR_BAD_STATE;
            else
               copySourceRect
                 (Natural (request.words (0)),
                  Natural (request.words (1)),
                  Natural (request.words (2)),
                  Natural (request.words (3)));
               ok := transferAndFlush
                 (Natural (request.words (0)),
                  Natural (request.words (1)),
                  Natural (request.words (2)),
                  Natural (request.words (3)));
               if ok then
                  replyMsg.words (0) := GPU_OK;
               else
                  replyMsg.words (0) := GPU_ERR_BAD_STATE;
               end if;
            end if;

         when OP_GPU_FLUSH_RECT =>
            replyMsg.tag := (label => OP_GPU_FLUSH_RECT,
                             length => 1, flags => 0, badge => 0);
            ok := transferAndFlush
              (Natural (request.words (0)),
               Natural (request.words (1)),
               Natural (request.words (2)),
               Natural (request.words (3)));
            if ok then
               replyMsg.words (0) := GPU_OK;
            else
               replyMsg.words (0) := GPU_ERR_BAD_STATE;
            end if;

         when OP_GPU_CLEAR =>
            replyMsg.tag := (label => OP_GPU_CLEAR,
                             length => 1, flags => 0, badge => 0);
            clearFb (Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
            ok := transferAndFlush (0, 0, Natural (FB_W), Natural (FB_H));
            if ok then
               replyMsg.words (0) := GPU_OK;
            else
               replyMsg.words (0) := GPU_ERR_BAD_STATE;
            end if;

         when others =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, flags => 0, badge => 0);
            replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
      end case;

      ignore := reply (from, replyMsg);
   end handleRequest;

   eventMsg : Message;
   eventFound : Boolean;
   from : ProcessID;
   msg : Message;
   found : Boolean;
begin
   debugPrint ("virtio-gpu: starting" & LF);

   trace ("read devmgr sysinfo");
   barPhys := getInfo (SYSINFO_GPU_BAR0);
   dmaPhys := getInfo (SYSINFO_GPU_DMA_PHYS);
   commonOff := getInfo (SYSINFO_GPU_COMMON_OFF);
   notifyOff := getInfo (SYSINFO_GPU_NOTIFY_OFF);
   isrOff := getInfo (SYSINFO_GPU_ISR_OFF);
   notifyMult := getInfo (SYSINFO_GPU_NOTIFY_MULT);
   gpuPrimary := getInfo (SYSINFO_GPU_IS_PRIMARY) /= 0;
   debugPrint ("virtio-gpu: bar=");
   printDec (barPhys);
   debugPrint (" dma=");
   printDec (dmaPhys);
   debugPrint (" common_off=");
   printDec (commonOff);
   debugPrint (" notify_off=");
   printDec (notifyOff);
   debugPrint (" notify_mult=");
   printDec (notifyMult);
   debugPrint (" primary=");
   if gpuPrimary then
      printDec (1);
   else
      printDec (0);
   end if;
   debugPrint ("" & LF);

   if barPhys = 0 or else barPhys = Unsigned_64'Last or else
      dmaPhys = 0 or else dmaPhys = Unsigned_64'Last or else
      notifyMult = 0
   then
      fail ("missing devmgr transport info");
      return;
   end if;

   initTransport;
   initGpu;

   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_GPU);
   end;

   debugPrint ("virtio-gpu: ready" & LF);
   signalReady (16#FF00#);

   loop
      loop
         Poll_Service_Request (from, msg, found);
         exit when not found;
         handleRequest (from, msg);
      end loop;

      eventFound := Poll_Event (eventMsg);
      if eventFound then
         declare
            isr : Unsigned_8 with
               Import,
               Address => To_Address (Integer_Address (BAR_VIRT_BASE + isrOff)),
               Volatile;
         begin
            if isr = 16#FF# then
               null;
            end if;
         end;
      elsif not found then
         if syscall (SYSCALL_SLEEP, 10) = Unsigned_64'Last then
            null;
         end if;
      end if;
   end loop;
end main;
