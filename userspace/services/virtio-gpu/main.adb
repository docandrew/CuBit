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
--  assigns bounded per-output scanout resources, transfers pixels to the host,
--  and flushes the resources. The Desktop, not this driver, chooses a primary.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Output_Discovery;
with CuBit.Desktop_Messages;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with GPU_Test_Policy;

procedure main is
   use ASCII;
   package OD renames CuBit.Output_Discovery;
   scanouts : OD.Catalog;
   package GM renames CuBit.Graphics_Metrics;
   uploads, legacyCopies : GM.Counter;
   uploadReporter, legacyReporter : CuBit.Graphics_Metrics_IO.Reporter;
   metricsAt : Unsigned_64 := 0;

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
   FB0_OFF   : constant Storage_Offset := 16#100000#;
   DMA_BANK_BYTES : constant Storage_Offset := 16#800000#;

   FB_W : constant Unsigned_32 := 1024;
   FB_H : constant Unsigned_32 := 768;
   FB_BYTES : constant Unsigned_32 := FB_W * FB_H * 4;
   pragma Compile_Time_Error
     (FB0_OFF + 2 * Storage_Offset (FB_BYTES) > DMA_BANK_BYTES,
      "GPU double buffers exceed their owned DMA bank");

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
   --  GET_DISPLAY_INFO has sixteen fixed-size entries, independent of how
   --  many outputs are currently connected. Discovery is not modesetting.
   type Scanout_Number is range 0 .. 15;
   DISPLAY_ENTRY_BYTES : constant Storage_Offset := 24;
   DISPLAY_HEADER_BYTES : constant Storage_Offset := 24;

   OP_GPU_GET_INFO      : constant Unsigned_32 := 16#0A00#;
   OP_GPU_CLEAR         : constant Unsigned_32 := 16#0A03#;
   OP_GPU_GET_STATUS    : constant Unsigned_32 := 16#0A04#;
   OP_GPU_MAP_FRAMEBUFFER : constant Unsigned_32 := 16#0A05#;
   OP_GPU_PRESENT_BUFFER : constant Unsigned_32 := 16#0A07#;

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
   secondDmaPhys : Unsigned_64 := 0;
   barPhys : Unsigned_64 := 0;
   commonOff : Unsigned_64 := 0;
   notifyOff : Unsigned_64 := 0;
   isrOff : Unsigned_64 := 0;
   notifyMult : Unsigned_64 := 0;
   gpuPrimary : Boolean := False;
   lastUsedIdx : Unsigned_16 := 0;
   nextDesc : Natural := 0;
   subtype Head_Index is Natural range 0 .. 1;
   selectedHead : Head_Index := 0;
   readyHeads : array (Head_Index) of Boolean := [others => False];
   flipsAnnounced : Boolean := False;

   type Command_Phase is
     (Idle, Transfer, Awaiting_Scanout, Set_Scanout, Flush, Quarantined);
   subtype Buffer_Index is Natural range 0 .. 1;
   type Pending_Presentation is record
      Phase : Command_Phase := Idle;
      Buffer : Buffer_Index := 0;
      X, Y, W, H : Natural := 0;
      Clearing : Boolean := False;
      Label : Unsigned_32 := OP_GPU_PRESENT_BUFFER;
      Fence : Unsigned_64 := 0;
      Deadline : Unsigned_64 := 0;
   end record;
   Pending : array (Head_Index) of Pending_Presentation;
   Fence_Sequence : Unsigned_64 := 0;
   Command_Timeout_Ms : constant Unsigned_64 := 500;
   Command_Stride : constant Storage_Offset := 512;
   function Command_Offset (Head : Head_Index) return Storage_Offset is
     (CMD_OFF + Storage_Offset (Head) * Command_Stride);
   function Response_Offset (Head : Head_Index) return Storage_Offset is
     (RESP_OFF + Storage_Offset (Head) * Command_Stride);
   function Reply_Slot (Head : Head_Index) return CapabilitySlot is
     (CapabilitySlot (32 + Head));

   function framebufferOffset
     (index : Natural) return Storage_Offset is
     (Storage_Offset (selectedHead) * DMA_BANK_BYTES + FB0_OFF +
        Storage_Offset (index) * Storage_Offset (FB_BYTES));

   function framebufferPhysical (index : Natural) return Unsigned_64 is
     ((if selectedHead = 0 then dmaPhys else secondDmaPhys) +
        Unsigned_64 (FB0_OFF) + Unsigned_64 (index) * Unsigned_64 (FB_BYTES));

   function resourceId (index : Natural) return Unsigned_32 is
     (Unsigned_32 (1 + selectedHead * 2 + index));

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
         (tag      => (label => label, length => 0, flags => 0, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]));
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
      v : Unsigned_32 with Import, Volatile, Address => DMA_BASE + base + off;
   begin
      return v;
   end get32;

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
         if polls /= 0 and then (polls mod 50_000) = 0 then
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

   procedure paintFramebuffer (index : Natural) is
      pixels : array (0 .. Natural (FB_W * FB_H) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + framebufferOffset (index);
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
      function Decimal (Value : Unsigned_32) return String is
         Text : constant String := Value'Image;
      begin
         return Text (Text'First + 1 .. Text'Last);
      end Decimal;
   begin
      trace ("cmd GET_DISPLAY_INFO");
      beginCmd (CMD_GET_DISPLAY_INFO);
      ok := submitCmd (24, 408, RESP_OK_DISPLAY_INFO);
      if not ok then
         fail ("GET_DISPLAY_INFO failed");
         return;
      end if;

      for Index in Scanout_Number loop
         declare
            Offset : constant Storage_Offset :=
              DISPLAY_HEADER_BYTES + Storage_Offset (Index) * DISPLAY_ENTRY_BYTES;
            Width : constant Unsigned_32 := get32 (RESP_OFF, Offset + 8);
            Height : constant Unsigned_32 := get32 (RESP_OFF, Offset + 12);
            Enabled : constant Unsigned_32 := get32 (RESP_OFF, Offset + 16);
         begin
            if Index = 0 or else Enabled /= 0 then
               debugPrint ("virtio-gpu: scanout" & Decimal (Unsigned_32 (Index)) &
                 " " & Decimal (Width) & "x" & Decimal (Height) &
                 " enabled=" & Decimal (Enabled) & LF);
            end if;
            if Enabled /= 0 then
               if Width not in 1 .. Unsigned_32 (OD.Extent'Last) or else
                  Height not in 1 .. Unsigned_32 (OD.Extent'Last)
               then
                  fail ("invalid scanout dimensions");
                  return;
               end if;
               scanouts.Count := scanouts.Count + 1;
               scanouts.Items (scanouts.Count) :=
                 (OD.Detected_Only, OD.Virtio_GPU,
                  OD.Native_Output_Number (Index),
                  OD.Extent (Width), OD.Extent (Height));
            end if;
         end;
      end loop;

      for Head in Head_Index loop
         selectedHead := Head;
         -- GET_DISPLAY_INFO advertises the host's preferred viewport, not a
         -- required resource size. In particular GTK can replace the command
         -- line hint with its initial 640x480 widget size. Activate connected
         -- supported heads with our bounded 1024x768 resources independently
         -- of that hint; retain both advertised and active sizes in discovery.
         -- Never size DMA allocations or copy spans from host preferences.
         if Head = 0 or else (secondDmaPhys /= 0 and then
           (for some Index in 1 .. scanouts.Count =>
              scanouts.Items (Index).Native_Number = Head))
         then
            trace ("paint test framebuffer");
            paintFramebuffer (0);
            paintFramebuffer (1);
            trace ("test framebuffer painted");

            --  Keep two complete 2D resources. display.svc updates the inactive
            --  backing and asks us to switch scanout only after the transfer has
            --  completed, so the host never scans a resource while it is changing.
            for index in 0 .. 1 loop
               trace ("cmd RESOURCE_CREATE_2D");
               beginCmd (CMD_RESOURCE_CREATE_2D);
               put32 (CMD_OFF, 24, resourceId (index));
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
               put32 (CMD_OFF, 24, resourceId (index));
               put32 (CMD_OFF, 28, 1);
               put64
                 (CMD_OFF, 32,
                  framebufferPhysical (index));
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
               put32 (CMD_OFF, 48, resourceId (index));
               put32 (CMD_OFF, 52, 0);
               GM.Add (uploads, Unsigned_64 (FB_BYTES));
               ok := submitCmd (56, 24, RESP_OK_NODATA);
               if not ok then
                  fail ("TRANSFER_TO_HOST_2D failed");
                  return;
               end if;
            end loop;

            trace ("cmd SET_SCANOUT");
            beginCmd (CMD_SET_SCANOUT);
            put32 (CMD_OFF, 24, 0);
            put32 (CMD_OFF, 28, 0);
            put32 (CMD_OFF, 32, FB_W);
            put32 (CMD_OFF, 36, FB_H);
            put32 (CMD_OFF, 40, Unsigned_32 (selectedHead));
            put32 (CMD_OFF, 44, resourceId (0));
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
            put32 (CMD_OFF, 40, resourceId (0));
            put32 (CMD_OFF, 44, 0);
            ok := submitCmd (48, 24, RESP_OK_NODATA);
            if not ok then
               fail ("RESOURCE_FLUSH failed");
               return;
            end if;

            debugPrint ("virtio-gpu: scanout test frame presented" & LF);
            for Index in 1 .. scanouts.Count loop
               if scanouts.Items (Index).Native_Number =
                 OD.Native_Output_Number (selectedHead) then
                  declare
                     Detected : constant OD.Description := scanouts.Items (Index);
                  begin
                     scanouts.Items (Index) :=
                       (OD.Backend_Ready, OD.Virtio_GPU,
                        OD.Native_Output_Number (selectedHead),
                        Detected.Advertised_Width, Detected.Advertised_Height,
                        OD.Extent (FB_W), OD.Extent (FB_H));
                  end;
               end if;
            end loop;
            readyHeads (Head) := True;
         end if;
      end loop;
      selectedHead := 0;
   end initGpu;


   procedure clearFb (bufferIndex : Natural; color : Unsigned_32) is
      pixels : array (0 .. Natural (FB_W * FB_H) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + framebufferOffset (bufferIndex);
   begin
      for i in pixels'Range loop
         pixels (i) := color;
      end loop;
   end clearFb;

   procedure finishPresentation (Head : Head_Index; Success : Boolean) is
      Item : Pending_Presentation renames Pending (Head);
      Response : constant Message :=
        (tag => (Item.Label, 1, 0, 0), authorityTag => 0,
         words => [(if Success then GPU_OK else GPU_ERR_BAD_STATE), 0, 0, 0]);
      Ignored : Unsigned_64;
   begin
      if Success then
         if not flipsAnnounced and then not Item.Clearing then
            debugPrint ("virtio-gpu: page flipping active" & LF);
            flipsAnnounced := True;
         end if;
      end if;
      if Item.Phase in Transfer | Awaiting_Scanout | Set_Scanout | Flush then
         Ignored := replyCap (Reply_Slot (Head), Response);
      end if;
      Item.Phase := (if Success then Idle else Quarantined);
      --  Failure is permanent until a separately designed adapter reset.
      --  In particular a timeout does NOT return DMA ownership or authorize
      --  rewriting the descriptor, response buffer or scanout backing.
   end finishPresentation;

   procedure issueCommand (Head : Head_Index) is
      Item : Pending_Presentation renames Pending (Head);
      Cmd : constant Storage_Offset := Command_Offset (Head);
      Resp : constant Storage_Offset := Response_Offset (Head);
      ID : constant Natural := Head * 2;
      Resource : constant Unsigned_32 := Unsigned_32 (1 + Head * 2 + Item.Buffer);
      Length : Unsigned_32 := 48;
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      if Fence_Sequence = Unsigned_64'Last or else
        Now > Unsigned_64'Last - Command_Timeout_Ms
      then
         finishPresentation (Head, False);
         return;
      end if;
      for Offset in Storage_Offset range 0 .. Command_Stride - 1 loop
         declare
            C : Unsigned_8 with Import, Address => DMA_BASE + Cmd + Offset;
            R : Unsigned_8 with Import, Address => DMA_BASE + Resp + Offset;
         begin
            C := 0;
            R := 0;
         end;
      end loop;
      Fence_Sequence := Fence_Sequence + 1;
      Item.Fence := Fence_Sequence;
      Item.Deadline := Now + Command_Timeout_Ms;
      --  Fence completion is the backend boundary, not a photon timestamp.
      put32 (Cmd, 4, 1); -- VIRTIO_GPU_FLAG_FENCE
      put64 (Cmd, 8, Item.Fence);
      case Item.Phase is
         when Transfer | Flush =>
            put32 (Cmd, 0, (if Item.Phase = Transfer then
                            CMD_TRANSFER_TO_HOST_2D else CMD_RESOURCE_FLUSH));
            put32 (Cmd, 24, Unsigned_32 (Item.X));
            put32 (Cmd, 28, Unsigned_32 (Item.Y));
            put32 (Cmd, 32, Unsigned_32 (Item.W));
            put32 (Cmd, 36, Unsigned_32 (Item.H));
            if Item.Phase = Transfer then
               put64 (Cmd, 40, Unsigned_64 ((Item.Y * Natural (FB_W) + Item.X) * 4));
               put32 (Cmd, 48, Resource);
               Length := 56;
               GM.Add (uploads, Unsigned_64 (Item.W) * Unsigned_64 (Item.H) * 4);
            else
               put32 (Cmd, 40, Resource);
            end if;
         when Set_Scanout =>
            put32 (Cmd, 0, CMD_SET_SCANOUT);
            put32 (Cmd, 32, FB_W);
            put32 (Cmd, 36, FB_H);
            put32 (Cmd, 40, Unsigned_32 (Head));
            put32 (Cmd, 44, Resource);
         when Idle | Awaiting_Scanout | Quarantined =>
            return;
      end case;
      descs (ID) :=
        (dmaPhys + Unsigned_64 (Cmd), Length, VRING_DESC_F_NEXT, Unsigned_16 (ID + 1));
      descs (ID + 1) :=
        (dmaPhys + Unsigned_64 (Resp), 24, VRING_DESC_F_WRITE, 0);
      avail.ring (Natural (avail.idx mod Unsigned_16 (QUEUE_SIZE))) := Unsigned_16 (ID);
      --  x86 coherent DMA: publish all command/pixel/descriptor writes before
      --  the producer index, and the index before notifying the device.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      avail.idx := avail.idx + 1;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      notifyQueue;
   end issueCommand;

   procedure collectCommands is
      Entry_Value : VringUsedElem;
      Head : Head_Index;
      Resp : Storage_Offset;
      Fence : Unsigned_64;
      Now : Unsigned_64;
   begin
      if used.idx - lastUsedIdx > Unsigned_16 (QUEUE_SIZE) then
         for H in Head_Index loop finishPresentation (H, False); end loop;
         lastUsedIdx := used.idx;
         debugPrint ("virtio-gpu: invalid used ring distance" & LF);
         return;
      end if;
      --  Bounded even if the peer produces completions continuously.
      for Count in 1 .. QUEUE_SIZE loop
         exit when used.idx = lastUsedIdx;
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         Entry_Value := used.ring (Natural (lastUsedIdx mod Unsigned_16 (QUEUE_SIZE)));
         lastUsedIdx := lastUsedIdx + 1;
         if Entry_Value.id /= 0 and then Entry_Value.id /= 2 then
            for H in Head_Index loop finishPresentation (H, False); end loop;
            debugPrint ("virtio-gpu: invalid used descriptor" & LF);
            return;
         end if;
         Head := Natural (Entry_Value.id / 2);
         if Pending (Head).Phase /= Quarantined then
            Resp := Response_Offset (Head);
            Fence := Unsigned_64 (get32 (Resp, 8)) or
              Shift_Left (Unsigned_64 (get32 (Resp, 12)), 32);
            if Pending (Head).Phase in Idle | Awaiting_Scanout or else Entry_Value.len /= 24 or else
              get32 (Resp, 0) /= RESP_OK_NODATA or else
              get32 (Resp, 4) /= 1 or else Fence /= Pending (Head).Fence
            then
               finishPresentation (Head, False);
               debugPrint ("virtio-gpu: invalid fenced completion" & LF);
            else
               case Pending (Head).Phase is
                  when Transfer =>
                     if Head = 0 and then not Pending (Head).Clearing and then
                       GPU_Test_Policy.Delay_First_Output_Ms /= 0
                     then
                        Pending (Head).Phase := Awaiting_Scanout;
                        Pending (Head).Deadline := syscall (SYSCALL_GETTIME) +
                          GPU_Test_Policy.Delay_First_Output_Ms;
                     else
                        Pending (Head).Phase :=
                          (if Pending (Head).Clearing and Pending (Head).Buffer = 1
                           then Flush else Set_Scanout);
                        issueCommand (Head);
                     end if;
                  when Set_Scanout =>
                     Pending (Head).Phase := Flush;
                     issueCommand (Head);
                  when Flush =>
                     if Pending (Head).Clearing and Pending (Head).Buffer = 1 then
                        Pending (Head).Buffer := 0;
                        Pending (Head).Phase := Transfer;
                        issueCommand (Head);
                     else
                        finishPresentation (Head, True);
                     end if;
                  when Idle | Awaiting_Scanout | Quarantined => null;
               end case;
            end if;
         end if;
      end loop;
      Now := syscall (SYSCALL_GETTIME);
      for H in Head_Index loop
         if Pending (H).Phase = Awaiting_Scanout and then Now >= Pending (H).Deadline then
            Pending (H).Phase := Set_Scanout;
            issueCommand (H);
         elsif Pending (H).Phase in Transfer | Set_Scanout | Flush and then
           Now >= Pending (H).Deadline
         then
            finishPresentation (H, False);
            debugPrint ("virtio-gpu: asynchronous command timeout" & LF);
         end if;
      end loop;
   end collectCommands;

   procedure handleRequest (from : ProcessID; incoming : Message) is
      request : Message := incoming;
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if request.tag.label = OD.Code (OD.GPU_Backend, OD.Get_Catalog) or else
         request.tag.label = OD.Code (OD.GPU_Backend, OD.Get_Description)
      then
         replyMsg := CuBit.Desktop_Messages.From_Wire (OD.Respond
           (OD.GPU_Backend, scanouts, 1,
            CuBit.Desktop_Messages.To_Wire (request)));
         ignore := reply (from, replyMsg);
         return;
      end if;
      if request.tag.length /= 4 or else request.tag.flags /= 0 or else
        request.tag.reserved > Unsigned_16 (Head_Index'Last) or else
        not readyHeads (Natural (request.tag.reserved))
      then
         replyMsg.tag := (request.tag.label, 1, 0, 0);
         replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
         ignore := reply (from, replyMsg);
         return;
      end if;
      selectedHead := Natural (request.tag.reserved);
      request.tag.reserved := 0;
      if Pending (selectedHead).Phase /= Idle and then
        request.tag.label not in OP_GPU_GET_INFO | OP_GPU_GET_STATUS
      then
         replyMsg.tag := (request.tag.label, 1, 0, 0);
         replyMsg.words (0) := GPU_ERR_BAD_STATE;
         ignore := reply (from, replyMsg);
         return;
      end if;
      case request.tag.label is
         when OP_GPU_GET_INFO =>
            replyMsg.tag := (label => OP_GPU_GET_INFO,
                             length => 4, flags => 0, reserved => 0);
            replyMsg.words (0) := Unsigned_64 (FB_W);
            replyMsg.words (1) := Unsigned_64 (FB_H);
            replyMsg.words (2) := Unsigned_64 (FB_W) * 4;
            replyMsg.words (3) := 32;

         when OP_GPU_GET_STATUS =>
            replyMsg.tag := (label => OP_GPU_GET_STATUS,
                             length => 4, flags => 0, reserved => 0);
            replyMsg.words (0) :=
              (if Pending (selectedHead).Phase = Quarantined
               then GPU_ERR_BAD_STATE else GPU_OK);
            replyMsg.words (1) :=
              (if Pending (selectedHead).Phase = Quarantined then 0 else 1);
            replyMsg.words (2) := Unsigned_64 (FB_W);
            replyMsg.words (3) := Unsigned_64 (FB_H);

         when OP_GPU_MAP_FRAMEBUFFER =>
            declare
               bufferIndexRaw : constant Unsigned_64 := request.words (0);
               bufferIndex : Natural range 0 .. 1 := 0;
               pages : constant Natural :=
                  Natural ((Unsigned_64 (FB_BYTES) + 4095) / 4096);
               reference : CuBit.Memory_Grants.Grant_Reference;
               grantOk : Boolean;
            begin
               replyMsg.tag := (label => OP_GPU_MAP_FRAMEBUFFER,
                                length => 4, flags => 0, reserved => 0);
               if bufferIndexRaw <= 1 then
                  bufferIndex := Natural (bufferIndexRaw);
                  CuBit.Memory_Grants.Create_For_Process
                    (grantee   => from,
                     localAddr =>
                       DMA_BASE + framebufferOffset (bufferIndex),
                     numPages  => pages,
                     readWrite => True,
                     reference => reference,
                     success   => grantOk);
               else
                  grantOk := False;
               end if;
               if grantOk then
                  -- Same checked layout/reference payload as display attachment.
                  -- A successful map has four words; errors have one, never a
                  -- numerically ambiguous slot-or-status word.
                  replyMsg.words (0) := reference.slot;
                  replyMsg.words (1) := reference.generation;
                  replyMsg.words (2) := Unsigned_64 (FB_W) or
                     Shift_Left (Unsigned_64 (FB_H), 32);
                  replyMsg.words (3) := Unsigned_64 (FB_W) * 4;
               else
                  replyMsg.tag.length := 1;
                  replyMsg.words (0) := GPU_ERR_BAD_STATE;
               end if;
            end;

         when OP_GPU_PRESENT_BUFFER =>
            declare
               bufferIndexRaw : constant Unsigned_64 := request.words (0);
               bufferIndex : Natural range 0 .. 1 := 0;
               packedXY : constant Unsigned_64 := request.words (1);
               packedWH : constant Unsigned_64 := request.words (2);
            begin
               replyMsg.tag := (label => OP_GPU_PRESENT_BUFFER,
                                length => 1, flags => 0, reserved => 0);
               if bufferIndexRaw > 1 or else request.words (3) /= 0 or else
                 (packedXY and 16#FFFF_FFFF#) >= Unsigned_64 (FB_W) or else
                 Shift_Right (packedXY, 32) >= Unsigned_64 (FB_H) or else
                 (packedWH and 16#FFFF_FFFF#) not in 1 .. Unsigned_64 (FB_W) or else
                 Shift_Right (packedWH, 32) not in 1 .. Unsigned_64 (FB_H)
               then
                  replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
               else
                  bufferIndex := Natural (bufferIndexRaw);
                  if saveReplyCap (Unsigned_64 (Reply_Slot (selectedHead))) = 1 then
                     Pending (selectedHead) :=
                       (Phase => Transfer, Buffer => bufferIndex,
                        X => Natural (packedXY and 16#FFFF_FFFF#),
                        Y => Natural (Shift_Right (packedXY, 32)),
                        W => Natural (packedWH and 16#FFFF_FFFF#),
                        H => Natural (Shift_Right (packedWH, 32)),
                        Clearing => False, Label => OP_GPU_PRESENT_BUFFER,
                        others => <>);
                     Pending (selectedHead).W := Natural'Min
                       (Pending (selectedHead).W, Natural (FB_W) - Pending (selectedHead).X);
                     Pending (selectedHead).H := Natural'Min
                       (Pending (selectedHead).H, Natural (FB_H) - Pending (selectedHead).Y);
                     issueCommand (selectedHead);
                     return; -- saved reply consumed only by completion/failure
                  end if;
                  replyMsg.words (0) := GPU_ERR_BAD_STATE;
               end if;
            end;

         when OP_GPU_CLEAR =>
            replyMsg.tag := (label => OP_GPU_CLEAR,
                             length => 1, flags => 0, reserved => 0);
            if saveReplyCap (Unsigned_64 (Reply_Slot (selectedHead))) = 1 then
               clearFb (0, Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
               clearFb (1, Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
               Pending (selectedHead) :=
                 (Phase => Transfer, Buffer => 1, X => 0, Y => 0,
                  W => Natural (FB_W), H => Natural (FB_H),
                  Clearing => True, Label => OP_GPU_CLEAR, others => <>);
               issueCommand (selectedHead);
               return;
            end if;
            replyMsg.words (0) := GPU_ERR_BAD_STATE;

         when others =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, flags => 0, reserved => 0);
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
   secondDmaPhys := getInfo (SYSINFO_GPU_SECOND_DMA_PHYS);
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
      collectCommands;
      for Count in 1 .. 8 loop
         Poll_Service_Request (from, msg, found);
         exit when not found;
         handleRequest (from, msg);
      end loop;

      eventFound := Poll_Event (eventMsg);
      declare
         Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      begin
         if Now /= Unsigned_64'Last and then Now >= metricsAt and then
            Now - metricsAt >= 1000
         then
            metricsAt := Now;
            CuBit.Graphics_Metrics_IO.Publish
              (GM.GPU_Upload_Request, uploads, uploadReporter);
            CuBit.Graphics_Metrics_IO.Publish
              (GM.GPU_Legacy_Copy, legacyCopies, legacyReporter);
         end if;
      end;
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
      else
         --  Requests and latched IRQs wake this wait atomically: work arriving
         --  after the polls above cannot be stranded behind a polling sleep.
         --  The deadline is for periodic diagnostics, not presentation pacing.
         declare
            Deadline : Unsigned_64 :=
              (if metricsAt > Unsigned_64'Last - 1000 then Unsigned_64'Last
               else metricsAt + 1000);
         begin
            for Item of Pending loop
               if Item.Phase in Transfer | Awaiting_Scanout | Set_Scanout | Flush then
                  Deadline := Unsigned_64'Min (Deadline, Item.Deadline);
               end if;
            end loop;
            if Wait_For_Activity_Until (Deadline) = Unavailable then
               fail ("activity wait unavailable");
               return;
            end if;
         end;
      end if;
   end loop;
end main;
