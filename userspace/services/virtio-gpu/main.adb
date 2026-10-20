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
with CuBit.Launch_Configurations;
with Launch_Configuration;
with CuBit.Memory_Grants;
with CuBit.Output_Discovery;
with CuBit.Output_Modes;
with CuBit.Monitor_EDID;
with CuBit.Monitor_EDID.Catalog;
with CuBit.Desktop_Messages;
with CuBit.Graphics_Metrics;
with CuBit.Graphics_Metrics_IO;
with CuBit.Display_Protocol;
with CuBit.Display_Planes;
with CuBit.GPU_Plane_Protocol;
with GPU_Test_Policy;
with CuBit.Device_Interrupts;
with Startup;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with Ada.Unchecked_Deallocation;

procedure main is
   use ASCII;
   package OD renames CuBit.Output_Discovery;
   package OM renames CuBit.Output_Modes;
   use type OM.Mode, OM.Admission, OM.Rate_Choice, OM.DP.Wire_Message;
   package DPL renames CuBit.Display_Planes;
   package GP renames CuBit.GPU_Plane_Protocol;
   use type DPL.Plane_Number;
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
   --  virtio-gpu cursor queue: UPDATE_CURSOR/MOVE_CURSOR, no responses.
   CURSOR_QUEUE : constant Unsigned_16 := 1;

   DESC_OFF  : constant Storage_Offset := 16#0000#;
   AVAIL_OFF : constant Storage_Offset := 16#1000#;
   USED_OFF  : constant Storage_Offset := 16#2000#;
   CMD_OFF   : constant Storage_Offset := 16#3000#;
   RESP_OFF  : constant Storage_Offset := 16#4000#;
   --  Rings and command responses occupy the first 64 KiB. Keep framebuffer
   --  pages separate without wasting almost a MiB of each bounded DMA bank.
   --  Cursor queue rings, its command slots, and one 64x64 ARGB cursor
   --  backing per head, all inside the first 64 KiB of the first bank.
   CURSOR_DESC_OFF  : constant Storage_Offset := 16#5000#;
   CURSOR_AVAIL_OFF : constant Storage_Offset := 16#5400#;
   CURSOR_USED_OFF  : constant Storage_Offset := 16#5800#;
   CURSOR_CMD_OFF   : constant Storage_Offset := 16#5C00#;
   CURSOR_CMD_STRIDE : constant Storage_Offset := 64;
   CURSOR_IMAGE_OFF : constant Storage_Offset := 16#6000#;
   CURSOR_EXTENT : constant := 64;
   CURSOR_PITCH : constant := CURSOR_EXTENT * 4;
   CURSOR_IMAGE_BYTES : constant := CURSOR_PITCH * CURSOR_EXTENT;
   FB0_OFF   : constant Storage_Offset := 16#10000#;
   pragma Compile_Time_Error
     (CURSOR_CMD_OFF + 16 * CURSOR_CMD_STRIDE > CURSOR_IMAGE_OFF or else
      CURSOR_IMAGE_OFF + 2 * CURSOR_IMAGE_BYTES > FB0_OFF,
      "cursor queue and backing overlap other DMA regions");
   --  One DMA bank per head: 16 MiB, or 8 MiB on a machine without that
   --  much contiguous memory (devmgr decides; read from the DMA capability
   --  at startup). After the first bank's 64 KiB of rings, each head holds
   --  two scanout buffers of bufferCapacity bytes at fixed offsets, so a
   --  mode change never moves a buffer: up to 1920x1080 (16 MiB banks) or
   --  1280x800 (8 MiB) at 32 bits per pixel.
   LARGE_BANK_BYTES : constant Storage_Offset := 16#1000000#;
   SMALL_BANK_BYTES : constant Storage_Offset := 16#800000#;
   bankBytes : Storage_Offset := SMALL_BANK_BYTES;
   PAGE_BYTES : constant := 4096;

   package EDID renames CuBit.Monitor_EDID;
   use type EDID.Decode_Status;
   FALLBACK_WIDTH : constant EDID.Extent := 1024;
   FALLBACK_HEIGHT : constant EDID.Extent := 768;
   function bufferCapacity return Storage_Offset is
     ((bankBytes - FB0_OFF) / 2 / PAGE_BYTES * PAGE_BYTES);
   --  The smallest mode the desktop is laid out for; hosts asking for less
   --  (a tiny window) get this, scaled by the host.
   MINIMUM_MODE : constant OM.Mode := (800, 600);
   pragma Compile_Time_Error
     (Storage_Offset ((FALLBACK_WIDTH * FALLBACK_HEIGHT * 4 + 4095) / 4096 * 4096) >
        (SMALL_BANK_BYTES - FB0_OFF) / 2,
      "Fallback GPU double buffers exceed their owned DMA bank");
   edidSupported : Boolean := False;

   VIRTIO_STATUS_ACKNOWLEDGE : constant Unsigned_8 := 1;
   VIRTIO_STATUS_DRIVER      : constant Unsigned_8 := 2;
   VIRTIO_STATUS_DRIVER_OK   : constant Unsigned_8 := 4;
   VIRTIO_STATUS_FEATURES_OK : constant Unsigned_8 := 8;
   VIRTIO_STATUS_FAILED      : constant Unsigned_8 := 128;

   REG_DEVICE_FEATURE_SELECT : constant Unsigned_64 := 0;
   REG_DEVICE_FEATURE        : constant Unsigned_64 := 4;
   REG_DRIVER_FEATURE_SELECT : constant Unsigned_64 := 8;
   REG_DRIVER_FEATURE        : constant Unsigned_64 := 12;
   REG_CONFIG_MSIX_VECTOR    : constant Unsigned_64 := 16;
   REG_NUM_QUEUES            : constant Unsigned_64 := 18;
   REG_DEVICE_STATUS         : constant Unsigned_64 := 20;
   REG_QUEUE_SELECT          : constant Unsigned_64 := 22;
   REG_QUEUE_SIZE            : constant Unsigned_64 := 24;
   REG_QUEUE_MSIX_VECTOR     : constant Unsigned_64 := 26;
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
   CMD_GET_EDID            : constant Unsigned_32 := 16#010A#;
   CMD_UPDATE_CURSOR       : constant Unsigned_32 := 16#0300#;
   CMD_MOVE_CURSOR         : constant Unsigned_32 := 16#0301#;

   RESP_OK_NODATA       : constant Unsigned_32 := 16#1100#;
   RESP_OK_DISPLAY_INFO : constant Unsigned_32 := 16#1101#;
   RESP_OK_EDID         : constant Unsigned_32 := 16#1104#;
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

   FORMAT_B8G8R8A8_UNORM : constant Unsigned_32 := 1;
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
   cursorDescs : DescArray
     with Import, Address => DMA_BASE + CURSOR_DESC_OFF, Volatile;
   cursorAvail : VringAvail
     with Import, Address => DMA_BASE + CURSOR_AVAIL_OFF, Volatile;
   cursorUsed  : VringUsed
     with Import, Address => DMA_BASE + CURSOR_USED_OFF, Volatile;
   cursorNotifyOffset : Unsigned_64 := 0;
   cursorUsedSeen : Unsigned_16 := 0;
   cursorQueueReady : Boolean := False;

   dmaPhys : Unsigned_64 := 0;
   secondDmaPhys : Unsigned_64 := 0;
   dmaBytes : Unsigned_64 := 0;  --  all DMA banks together
   barPhys : Unsigned_64 := 0;
   commonOff : Unsigned_64 := 0;
   notifyOff : Unsigned_64 := 0;
   isrOff : Unsigned_64 := 0;
   --  devmgr programmed MSI-X entry MSIX_ENTRY for the vector in the
   --  manifest's device slot (docs/msi-interrupts.md); without one the
   --  device signals its legacy line and the ISR register acknowledges it.
   messageInterrupts : Boolean := False;
   interruptBlock : CuBit.Device_Interrupts.Interrupt_Block;
   --  Queues use entry 0; configuration changes entry 1 when devmgr gave
   --  two vectors (the manifest asks for two), else entry 0 too.
   MSIX_ENTRY : constant Unsigned_16 := 0;
   CONFIG_ENTRY : constant Unsigned_16 := 1;
   configEntry : Unsigned_16 := MSIX_ENTRY;
   notifyMult : Unsigned_64 := 0;
   barBytes, notifyBytes : Unsigned_64 := 0;
   queueNotifyOffset : Unsigned_64 := 0;
   gpuPrimary : Boolean := False;
   lastUsedIdx : Unsigned_16 := 0;
   nextDesc : Natural := 0;
   subtype Head_Index is Natural range 0 .. 1;
   selectedHead : Head_Index := 0;
   Test_Startup_Clear_Seen : array (Head_Index) of Boolean := [others => False];
   type Head_Geometry is record
      Width : EDID.Extent := FALLBACK_WIDTH;
      Height : EDID.Extent := FALLBACK_HEIGHT;
   end record;
   geometry : array (Head_Index) of Head_Geometry;
   --  Each head's monitor size from its EDID (zero: unknown), for the
   --  output catalog (display.svc and Desktop compute DPI from it).
   headSize : array (Head_Index) of CuBit.Monitor_EDID.Catalog.Physical_Size;
   --  Each head's monitor as its EDID states it (DISP-016): who it is and
   --  every mode it lists (CuBit.Monitor_EDID.Catalog), each judged against
   --  the scanout buffers (Verdict_Of), for List_Mode and Identify. QEMU
   --  regenerates the EDID when its window changes size (readDisplayEvent).
   type Listed_Modes is access CuBit.Monitor_EDID.Catalog.Mode_List;
   type Head_Monitor is record
      Known : Boolean := False;
      Identity : CuBit.Monitor_EDID.Catalog.Identity;
      Modes : Listed_Modes;
      --  The listed mode last applied by an Exact_Rate Set_Mode (zero:
      --  none); Current_Mode prefers it among modes of the head's size.
      Chosen : Natural := 0;
   end record;
   monitors : array (Head_Index) of Head_Monitor;
   procedure Free is new Ada.Unchecked_Deallocation (CuBit.Monitor_EDID.Catalog.Mode_List, Listed_Modes);
   function Frame_Width (Head : Head_Index) return Unsigned_32 is
     (Unsigned_32 (geometry (Head).Width));
   function Frame_Height (Head : Head_Index) return Unsigned_32 is
     (Unsigned_32 (geometry (Head).Height));
   function Frame_Bytes (Head : Head_Index) return Unsigned_32 is
     (Frame_Width (Head) * Frame_Height (Head) * 4);
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
   --  A saved reply (CAP-002): the kernel moves a request's reply into this
   --  process's handle table (a Reply handle, no slot to choose); it is
   --  answered once, later. Keep refuses when one is already held.
   type Owed_Reply is record
      Handle : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   end record;
   function Keep (Item : in out Owed_Reply) return Boolean is
   begin
      if CuBit.Handles.Valid (Item.Handle) then return False; end if;
      declare
         Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
      begin
         if not Saved.Saved then return False; end if;
         Item.Handle := Saved.Handle;
         return True;
      end;
   end Keep;
   function Answer (Item : in out Owed_Reply; Msg : Message) return Unsigned_64 is
      Delivered : Unsigned_64 := 0;
   begin
      if CuBit.Handles.Valid (Item.Handle) then
         Delivered := replyCap (Item.Handle, Msg);
         Item.Handle := CuBit.Handles.No_Reply;
      end if;
      return Delivered;
   end Answer;
   Head_Replies : array (Head_Index) of Owed_Reply;
   Cursor_Reply, Watch_Reply : Owed_Reply;

   function framebufferOffset
     (index : Natural) return Storage_Offset is
     (Storage_Offset (selectedHead) * bankBytes + FB0_OFF +
        Storage_Offset (index) * bufferCapacity);

   function framebufferPhysical (index : Natural) return Unsigned_64 is
     ((if selectedHead = 0 then dmaPhys else secondDmaPhys) +
        Unsigned_64 (FB0_OFF) + Unsigned_64 (index) * Unsigned_64 (bufferCapacity));

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
      ignore := capSend (Startup.Ready,
         (tag      => (label => label, length => 0, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]), CuBit.Messages.Wait_Forever);
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
           (BAR_VIRT_BASE + queueNotifyOffset));
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

      --  Boot commands have one outstanding chain. Do not parse a short reply.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      declare
         Completion : constant VringUsedElem :=
           used.ring (Natural (lastUsedIdx mod Unsigned_16 (QUEUE_SIZE)));
      begin
         if used.idx /= lastUsedIdx + 1 or else
           Completion.id /= Unsigned_32 (id) or else Completion.len /= respLen
         then
            fail ("invalid boot command completion");
            return False;
         end if;
      end;
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
      Registers : constant CuBit.Handles.IPC.Description :=
        CuBit.Handles.IPC.Describe (Startup.Registers);
      Notify : constant CuBit.Handles.IPC.Description :=
        CuBit.Handles.IPC.Describe (Startup.Notify);
      Notify_Displacement : Unsigned_64;
      use type CuBit.Handles.ABI.Kind;
      function Fits (Offset, Bytes, Limit : Unsigned_64) return Boolean is
        (Bytes /= 0 and then Offset <= Limit and then Bytes <= Limit - Offset);
   begin
      --  The startup "registers" region devmgr granted: the whole register
      --  BAR, read and write (CAP-002: described by handle, no slot).
      if not Registers.Current or else Registers.Kind /= CuBit.Handles.ABI.Device_Memory
        or else not Registers.Held.Read or else not Registers.Held.Write
        or else Registers.Object /= barPhys or else barPhys mod 4096 /= 0
        or else Registers.Parameter < 4096 or else Registers.Parameter > 1024 * 1024
        or else Registers.Parameter mod 4096 /= 0
        or else barPhys > Unsigned_64'Last - Registers.Parameter
      then fail ("invalid BAR capability"); return; end if;
      barBytes := Registers.Parameter;
      if commonOff mod 4 /= 0 or else notifyOff mod 2 /= 0 or else
        not Fits (commonOff, 56, barBytes) or else
        not Fits (notifyOff, 2, barBytes) or else
        not Fits (isrOff, 1, barBytes) or else
        notifyMult > Unsigned_64 (Unsigned_32'Last)
      then fail ("invalid BAR register spans"); return; end if;
      if not Notify.Current or else Notify.Kind /= CuBit.Handles.ABI.Device_Memory
        or else not Notify.Held.Write
        or else Notify.Object /= barPhys + notifyOff
        or else not Fits (notifyOff, Notify.Parameter, barBytes) or else Notify.Parameter < 2
      then fail ("invalid notify capability"); return; end if;
      notifyBytes := Notify.Parameter;
      trace ("map BAR");
      if not CuBit.Handles.Devices.Map_Device
           (Startup.Registers, barPhys, BAR_VIRT_BASE, barBytes / 4096, Read_Only => False)
      then
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
         edidSupported := (devFeatures and 2) /= 0;
         printDec (Unsigned_64 (devFeatures));
         debugPrint ("" & LF);
      end;
      trace ("publish supported feature set");
      write32 (REG_DRIVER_FEATURE_SELECT, 0);
      write32 (REG_DRIVER_FEATURE, (if edidSupported then 2 else 0));
      status := status or VIRTIO_STATUS_FEATURES_OK;
      trace ("set FEATURES_OK");
      write8 (REG_DEVICE_STATUS, status);
      if (read8 (REG_DEVICE_STATUS) and VIRTIO_STATUS_FEATURES_OK) = 0 then
         write8 (REG_DEVICE_STATUS, status or VIRTIO_STATUS_FAILED);
         fail ("feature negotiation failed");
         return;
      end if;

      interruptBlock :=
        CuBit.Device_Interrupts.Block_Of (Startup.Device);
      messageInterrupts := interruptBlock.Valid;
      if messageInterrupts then
         configEntry := (if interruptBlock.Count >= 2 then CONFIG_ENTRY else MSIX_ENTRY);
         trace ("route configuration changes to MSI-X");
         write16 (REG_CONFIG_MSIX_VECTOR, configEntry);
         if read16 (REG_CONFIG_MSIX_VECTOR) /= configEntry then
            write8 (REG_DEVICE_STATUS, status or VIRTIO_STATUS_FAILED);
            fail ("configuration MSI-X vector refused");
            return;
         end if;
      end if;

      trace ("select control queue");
      write16 (REG_QUEUE_SELECT, CTRL_QUEUE);
      -- 16-bit queue offset times a validated32-bit multiplier cannot overflow.
      -- Cache only the checked address; later notifications do not trust a
      -- potentially changed device register to choose an MMIO destination.
      Notify_Displacement := Unsigned_64 (read16 (REG_QUEUE_NOTIFY_OFF)) * notifyMult;
      if Notify_Displacement mod 2 /= 0 or else
        not Fits (Notify_Displacement, 2, notifyBytes) then
         fail ("queue notification outside admitted span"); return;
      end if;
      queueNotifyOffset := notifyOff + Notify_Displacement;
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
      if messageInterrupts then
         write16 (REG_QUEUE_MSIX_VECTOR, MSIX_ENTRY);
         if read16 (REG_QUEUE_MSIX_VECTOR) /= MSIX_ENTRY then
            write8 (REG_DEVICE_STATUS, status or VIRTIO_STATUS_FAILED);
            fail ("control queue MSI-X vector refused");
            return;
         end if;
      end if;
      write64 (REG_QUEUE_DESC, dmaPhys + Unsigned_64 (DESC_OFF));
      write64 (REG_QUEUE_DRIVER, dmaPhys + Unsigned_64 (AVAIL_OFF));
      write64 (REG_QUEUE_DEVICE, dmaPhys + Unsigned_64 (USED_OFF));
      write16 (REG_QUEUE_ENABLE, 1);

      --  The cursor queue is optional: without it every cursor stays a
      --  software cursor, which display.svc already handles.
      trace ("select cursor queue");
      write16 (REG_QUEUE_SELECT, CURSOR_QUEUE);
      Notify_Displacement := Unsigned_64 (read16 (REG_QUEUE_NOTIFY_OFF)) * notifyMult;
      qsz := read16 (REG_QUEUE_SIZE);
      if Notify_Displacement mod 2 = 0 and then
        Fits (Notify_Displacement, 2, notifyBytes) and then
        qsz >= Unsigned_16 (QUEUE_SIZE)
      then
         write16 (REG_QUEUE_SIZE, Unsigned_16 (QUEUE_SIZE));
         if messageInterrupts then
            write16 (REG_QUEUE_MSIX_VECTOR, MSIX_ENTRY);
         end if;
         write64 (REG_QUEUE_DESC, dmaPhys + Unsigned_64 (CURSOR_DESC_OFF));
         write64 (REG_QUEUE_DRIVER, dmaPhys + Unsigned_64 (CURSOR_AVAIL_OFF));
         write64 (REG_QUEUE_DEVICE, dmaPhys + Unsigned_64 (CURSOR_USED_OFF));
         write16 (REG_QUEUE_ENABLE, 1);
         cursorNotifyOffset := notifyOff + Notify_Displacement;
         cursorQueueReady := True;
      else
         debugPrint ("virtio-gpu: no usable cursor queue; software cursors only" & LF);
      end if;

      status := status or VIRTIO_STATUS_DRIVER_OK;
      trace ("set DRIVER_OK");
      write8 (REG_DEVICE_STATUS, status);
      debugPrint ("virtio-gpu: transport ready queues=");
      printDec (Unsigned_64 (read16 (REG_NUM_QUEUES)));
      debugPrint ("" & LF);
   end initTransport;

   procedure paintFramebuffer (index : Natural) is
      pixels : array (0 .. Natural (Frame_Width (selectedHead) * Frame_Height (selectedHead)) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + framebufferOffset (index);
      color : Unsigned_32;
      x : Natural;
      y : Natural;
   begin
      for i in pixels'Range loop
         x := i mod Natural (Frame_Width (selectedHead));
         y := i / Natural (Frame_Width (selectedHead));
         if x < 8 or else y < 8 or else
            x >= Natural (Frame_Width (selectedHead)) - 8 or else y >= Natural (Frame_Height (selectedHead)) - 8
         then
            color := 16#00FF_FFFF#;
         elsif x < Natural (Frame_Width (selectedHead)) / 3 then
            color := 16#0030_6FE0#;
         elsif x < (Natural (Frame_Width (selectedHead)) * 2) / 3 then
            color := 16#00E0_D040#;
         else
            color := 16#00D040_40#;
         end if;
         pixels (i) := color;
      end loop;
   end paintFramebuffer;

   --  GET_EDID returns the base block and its extensions, up to 1024 bytes.
   EDID_Response_Bytes : constant := 1024;
   EDID_Blocks : constant := EDID_Response_Bytes / CuBit.Monitor_EDID.Catalog.Block_Bytes;
   subtype EDID_Extension_Count is Natural range 0 .. EDID_Blocks - 1;

   --  Read Head's EDID into monitors (Head): identity, physical size and its
   --  listed modes. False (and the monitor unknown) when the host gives none.
   function readEDID (Head : Head_Index; Base : out EDID.Base_Block) return Boolean is
      package C renames CuBit.Monitor_EDID.Catalog;
      type Raw_Blocks is array (0 .. EDID_Blocks - 1) of EDID.Base_Block;
      Raw : Raw_Blocks with Import, Volatile, Address => DMA_BASE + RESP_OFF + 32;
      Size : Unsigned_32;
      Extensions : EDID_Extension_Count;
      M : Head_Monitor renames monitors (Head);
   begin
      Base := [others => 0];
      if not edidSupported then return False; end if;
      beginCmd (CMD_GET_EDID);
      put32 (CMD_OFF, 24, Unsigned_32 (Head));
      if not submitCmd (32, 1056, RESP_OK_EDID) then
         --  A transport timeout is uncertain ownership, not permission to
         --  reuse its command/response storage for another request.
         fail ("GET_EDID transport failure");
         return False;
      end if;
      Size := get32 (RESP_OFF, 24);
      if Size < C.Block_Bytes or else Size > EDID_Response_Bytes or else Size mod C.Block_Bytes /= 0 then
         debugPrint ("virtio-gpu: invalid EDID length; using fallback" & LF);
         return False;
      end if;
      for I in Base'Range loop Base (I) := Raw (0) (I); end loop;
      Extensions := Natural'Min (C.Extensions_Declared (Base), Natural (Size) / C.Block_Bytes - 1);
      declare
         Blocks : C.Block_Array (1 .. Extensions);
      begin
         for B in Blocks'Range loop
            for I in EDID.Base_Block'Range loop Blocks (B) (I) := Raw (B) (I); end loop;
         end loop;
         --  A fresh list each read: a host may add extensions (QEMU adds
         --  a DisplayID block for large windows).
         Free (M.Modes);
         M.Modes := new C.Mode_List (C.Capacity (Extensions));
         C.Collect (Base, Blocks, M.Modes.all);
      end;
      M.Identity := C.Identify (Base);
      M.Known := True;
      M.Chosen := 0;
      headSize (Head) := C.Size_Of (Base);
      debugPrint ("virtio-gpu: EDID head" & Head'Image & " modes" & M.Modes.Count'Image &
        " extensions" & Extensions'Image & LF);
      return True;
   end readEDID;

   procedure selectPreferredMode (Head : Head_Index) is
      Snapshot : EDID.Base_Block;
      Parsed : EDID.Result;
   begin
      if not readEDID (Head, Snapshot) then return; end if;
      Parsed := EDID.Decode (Snapshot);
      if Parsed.Status /= EDID.Accepted then
         debugPrint ("virtio-gpu: EDID " & Parsed.Status'Image & "; using fallback" & LF);
         return;
      end if;
      declare
         Mode : constant EDID.Timing := Parsed.Preferred;
         Bytes : constant Positive := EDID.Buffer_Bytes (Mode.Width, Mode.Height);
      begin
         --  This is a virtual resource-size choice, not physical link training
         --  or a claim that EDID alone authorizes a hardware mode. Keep the
         --  current desktop minimum and the actual owned DMA budget explicit.
         if Mode.Width < MINIMUM_MODE.Width or else Mode.Height < MINIMUM_MODE.Height then
            debugPrint ("virtio-gpu: EDID preference below startup UI minimum; using fallback" & LF);
            return;
         elsif Storage_Offset (Bytes) > bufferCapacity then
            debugPrint ("virtio-gpu: EDID mode exceeds scanout budget; using fallback" & LF);
            return;
         end if;
         geometry (Head) := (Mode.Width, Mode.Height);
         debugPrint ("virtio-gpu: EDID head" & Head'Image &
           " active" & Mode.Width'Image & "x" & Mode.Height'Image &
           " nominal_millihz" & EDID.Refresh_Millihertz (Mode)'Image & LF);
      end;
   end selectPreferredMode;

   --  A virtual output drives any size its buffers hold, from the
   --  desktop's minimum: the monitor's timing details do not matter.
   function Verdict_Of (Mode : EDID.Timing) return OM.Admission is
     (if Mode.Width < MINIMUM_MODE.Width or else Mode.Height < MINIMUM_MODE.Height
      then OM.Below_Minimum_Size
      elsif Storage_Offset (EDID.Buffer_Bytes (Mode.Width, Mode.Height)) > bufferCapacity
      then OM.Above_Scanout_Buffers
      else OM.Admitted);
   function Listed_Refresh (Mode : EDID.Timing) return Unsigned_32 is
     (Unsigned_32 (Unsigned_64'Min (EDID.Refresh_Millihertz (Mode), Unsigned_64 (Unsigned_32'Last))));
   function Sized (Item : CuBit.Monitor_EDID.Catalog.Mode; Width, Height : Unsigned_32) return Boolean is
     (Unsigned_32 (Item.Timing.Width) = Width and then Unsigned_32 (Item.Timing.Height) = Height);

   --  The one listed mode Head runs: the mode last chosen while the head
   --  keeps its size, else the first listed mode of the head's size
   --  (the monitor's preferred one first). Zero when none has its size.
   function Current_Mode (Head : Head_Index) return Natural is
      M : Head_Monitor renames monitors (Head);
      First : Natural := 0;
   begin
      if not M.Known or else M.Modes = null then return 0; end if;
      if M.Chosen in 1 .. M.Modes.Count and then
        Sized (M.Modes.Items (M.Chosen), Frame_Width (Head), Frame_Height (Head))
      then
         return M.Chosen;
      end if;
      for I in 1 .. M.Modes.Count loop
         if Sized (M.Modes.Items (I), Frame_Width (Head), Frame_Height (Head)) then
            if M.Modes.Items (I).Preferred then return I; end if;
            if First = 0 then First := I; end if;
         end if;
      end loop;
      return First;
   end Current_Mode;

   ---------------------------------------------------------------------------
   --  Hardware cursor plane (CuBit.GPU_Plane_Protocol): one plane per head.
   --  display.svc writes the head's mapped backing, Show uploads it with a
   --  fenced TRANSFER_TO_HOST_2D on the control queue and then defines the
   --  cursor on the cursor queue. Moves and hides go straight to the cursor
   --  queue. The host composes the cursor; the guest scanout never holds it.
   ---------------------------------------------------------------------------
   CURSOR_PLANE : constant DPL.Plane_Number := 1;
   CURSOR_PLANES : constant DPL.Plane_Number := 1;
   CURSOR_Z : constant DPL.Plane_Z := 1;
   CURSOR_TRANSFER_ID : constant Natural := 4;
   CURSOR_TRANSFER_CMD : constant Storage_Offset := CMD_OFF + 2 * 512;
   CURSOR_TRANSFER_RESP : constant Storage_Offset := RESP_OFF + 2 * 512;
   UPDATE_CURSOR_BYTES : constant Unsigned_32 := 56;

   function cursorResource (Head : Head_Index) return Unsigned_32 is
     (Unsigned_32 (5 + Head));
   function cursorImageOffset (Head : Head_Index) return Storage_Offset is
     (CURSOR_IMAGE_OFF + Storage_Offset (Head) * CURSOR_IMAGE_BYTES);

   type Cursor_State is record
      Ready : Boolean := False;
      --  Desired host state and whether it still has to be queued.
      Defined : Boolean := False;
      Position : GP.Position;
      Hot_X, Hot_Y : DPL.Hotspot_Coordinate := 0;
      Kind : Unsigned_32 := CMD_UPDATE_CURSOR;
      Dirty : Boolean := False;
   end record;
   cursors : array (Head_Index) of Cursor_State;
   cursorAvailIdx : Unsigned_16 := 0;
   cursorFaulted : Boolean := False;

   type Transfer_Phase is (Idle, Transfer, Quarantined);
   type Cursor_Transfer is record
      Phase : Transfer_Phase := Idle;
      Head : Head_Index := 0;
      Fence : Unsigned_64 := 0;
      Deadline : Unsigned_64 := 0;
      Show : GP.Show;
   end record;
   cursorTransfer : Cursor_Transfer;

   procedure notifyCursorQueue is
      reg : Unsigned_16 with Import, Volatile,
        Address => To_Address (Integer_Address (BAR_VIRT_BASE + cursorNotifyOffset));
   begin
      reg := CURSOR_QUEUE;
   end notifyCursorQueue;

   --  Retire used cursor commands. QEMU completes the cursor queue in order;
   --  anything else is treated as a fault rather than guessed at.
   procedure reclaimCursorQueue is
      Seen : constant Unsigned_16 := cursorUsed.idx;
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      if Seen - cursorUsedSeen > cursorAvailIdx - cursorUsedSeen then
         cursorFaulted := True;
         debugPrint ("virtio-gpu: invalid cursor used index" & LF);
         return;
      end if;
      cursorUsedSeen := Seen;
   end reclaimCursorQueue;

   --  Queue the newest state for each head that has a change pending.
   procedure flushCursors is
      Slot : Natural;
      Cmd : Storage_Offset;
   begin
      if not cursorQueueReady or else cursorFaulted then return; end if;
      reclaimCursorQueue;
      for Head in Head_Index loop
         if cursors (Head).Dirty and then
           cursorAvailIdx - cursorUsedSeen < Unsigned_16 (QUEUE_SIZE)
         then
            Slot := Natural (cursorAvailIdx mod Unsigned_16 (QUEUE_SIZE));
            Cmd := CURSOR_CMD_OFF + Storage_Offset (Slot) * CURSOR_CMD_STRIDE;
            for Offset in Storage_Offset range 0 .. CURSOR_CMD_STRIDE - 1 loop
               declare
                  B : Unsigned_8 with Import, Address => DMA_BASE + Cmd + Offset;
               begin
                  B := 0;
               end;
            end loop;
            put32 (Cmd, 0, cursors (Head).Kind);
            put32 (Cmd, 24, Unsigned_32 (Head));
            put32 (Cmd, 28, Unsigned_32'Mod (Integer (cursors (Head).Position.X)));
            put32 (Cmd, 32, Unsigned_32'Mod (Integer (cursors (Head).Position.Y)));
            --  QEMU shows the cursor for MOVE_CURSOR only with a resource.
            put32 (Cmd, 40, (if cursors (Head).Defined then cursorResource (Head) else 0));
            put32 (Cmd, 44, Unsigned_32 (cursors (Head).Hot_X));
            put32 (Cmd, 48, Unsigned_32 (cursors (Head).Hot_Y));
            cursorDescs (Slot) :=
              (dmaPhys + Unsigned_64 (Cmd), UPDATE_CURSOR_BYTES, 0, 0);
            cursorAvail.ring (Slot) := Unsigned_16 (Slot);
            System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
            cursorAvailIdx := cursorAvailIdx + 1;
            cursorAvail.idx := cursorAvailIdx;
            System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
            notifyCursorQueue;
            cursors (Head).Dirty := False;
            --  A queued definition is followed only by moves.
            cursors (Head).Kind := CMD_MOVE_CURSOR;
         end if;
      end loop;
   end flushCursors;

   function cursorsPending return Boolean is
     (for some C of cursors => C.Dirty);

   --  Boot only: one 64x64 ARGB resource per ready head.
   procedure createCursor (Head : Head_Index) is
   begin
      if not cursorQueueReady then return; end if;
      beginCmd (CMD_RESOURCE_CREATE_2D);
      put32 (CMD_OFF, 24, cursorResource (Head));
      put32 (CMD_OFF, 28, FORMAT_B8G8R8A8_UNORM);
      put32 (CMD_OFF, 32, CURSOR_EXTENT);
      put32 (CMD_OFF, 36, CURSOR_EXTENT);
      if not submitCmd (40, 24, RESP_OK_NODATA) then
         debugPrint ("virtio-gpu: cursor resource rejected; software cursors only" & LF);
         return;
      end if;
      beginCmd (CMD_RESOURCE_ATTACH);
      put32 (CMD_OFF, 24, cursorResource (Head));
      put32 (CMD_OFF, 28, 1);
      put64 (CMD_OFF, 32, dmaPhys + Unsigned_64 (cursorImageOffset (Head)));
      put32 (CMD_OFF, 40, CURSOR_IMAGE_BYTES);
      put32 (CMD_OFF, 44, 0);
      if not submitCmd (48, 24, RESP_OK_NODATA) then
         debugPrint ("virtio-gpu: cursor backing rejected; software cursors only" & LF);
         return;
      end if;
      cursors (Head).Ready := True;
      debugPrint ("virtio-gpu: hardware cursor plane head" & Head'Image & LF);
   end createCursor;

   --  Two complete 2D resources of the selected head's geometry on its
   --  fixed buffers, the first one scanned out. display.svc updates the
   --  inactive backing and asks for the switch only after the transfer has
   --  completed, so the host never scans a resource while it is changing.
   --  Synchronous: only with no other command outstanding (boot, or a mode
   --  change on an idle device). False after a transport failure (fail).
   function bringUpScanout return Boolean is
      ok : Boolean;
   begin
      for index in 0 .. 1 loop
         trace ("cmd RESOURCE_CREATE_2D");
         beginCmd (CMD_RESOURCE_CREATE_2D);
         put32 (CMD_OFF, 24, resourceId (index));
         put32 (CMD_OFF, 28, FORMAT_B8G8R8X8_UNORM);
         put32 (CMD_OFF, 32, Frame_Width (selectedHead));
         put32 (CMD_OFF, 36, Frame_Height (selectedHead));
         ok := submitCmd (40, 24, RESP_OK_NODATA);
         if not ok then
            fail ("RESOURCE_CREATE_2D failed");
            return False;
         end if;

         trace ("cmd RESOURCE_ATTACH_BACKING");
         beginCmd (CMD_RESOURCE_ATTACH);
         put32 (CMD_OFF, 24, resourceId (index));
         put32 (CMD_OFF, 28, 1);
         put64
           (CMD_OFF, 32,
            framebufferPhysical (index));
         put32 (CMD_OFF, 40, Frame_Bytes (selectedHead));
         put32 (CMD_OFF, 44, 0);
         ok := submitCmd (48, 24, RESP_OK_NODATA);
         if not ok then
            fail ("RESOURCE_ATTACH_BACKING failed");
            return False;
         end if;

         trace ("cmd TRANSFER_TO_HOST_2D");
         beginCmd (CMD_TRANSFER_TO_HOST_2D);
         put32 (CMD_OFF, 24, 0);
         put32 (CMD_OFF, 28, 0);
         put32 (CMD_OFF, 32, Frame_Width (selectedHead));
         put32 (CMD_OFF, 36, Frame_Height (selectedHead));
         put64 (CMD_OFF, 40, 0);
         put32 (CMD_OFF, 48, resourceId (index));
         put32 (CMD_OFF, 52, 0);
         GM.Add (uploads, Unsigned_64 (Frame_Bytes (selectedHead)));
         ok := submitCmd (56, 24, RESP_OK_NODATA);
         if not ok then
            fail ("TRANSFER_TO_HOST_2D failed");
            return False;
         end if;
      end loop;

      trace ("cmd SET_SCANOUT");
      beginCmd (CMD_SET_SCANOUT);
      put32 (CMD_OFF, 24, 0);
      put32 (CMD_OFF, 28, 0);
      put32 (CMD_OFF, 32, Frame_Width (selectedHead));
      put32 (CMD_OFF, 36, Frame_Height (selectedHead));
      put32 (CMD_OFF, 40, Unsigned_32 (selectedHead));
      put32 (CMD_OFF, 44, resourceId (0));
      ok := submitCmd (48, 24, RESP_OK_NODATA);
      if not ok then
         fail ("SET_SCANOUT failed");
         return False;
      end if;

      trace ("cmd RESOURCE_FLUSH");
      beginCmd (CMD_RESOURCE_FLUSH);
      put32 (CMD_OFF, 24, 0);
      put32 (CMD_OFF, 28, 0);
      put32 (CMD_OFF, 32, Frame_Width (selectedHead));
      put32 (CMD_OFF, 36, Frame_Height (selectedHead));
      put32 (CMD_OFF, 40, resourceId (0));
      put32 (CMD_OFF, 44, 0);
      ok := submitCmd (48, 24, RESP_OK_NODATA);
      if not ok then
         fail ("RESOURCE_FLUSH failed");
         return False;
      end if;
      return True;
   end bringUpScanout;

   --  "WxH" with no padding (log markers the tests match).
   function Dimensions (Width, Height : Natural) return String is
      W : constant String := Width'Image;
      H : constant String := Height'Image;
   begin
      return W (W'First + 1 .. W'Last) & "x" & H (H'First + 1 .. H'Last);
   end Dimensions;

   --  The catalog entry of Head reports the head's geometry as current.
   procedure noteCurrentMode (Head : Head_Index) is
   begin
      for Index in 1 .. scanouts.Count loop
         if scanouts.Items (Index).Native_Number = OD.Native_Output_Number (Head) then
            declare
               Detected : constant OD.Description := scanouts.Items (Index);
            begin
               scanouts.Items (Index) :=
                 (OD.Backend_Ready, OD.Virtio_GPU,
                  OD.Native_Output_Number (Head),
                  Detected.Advertised_Width, Detected.Advertised_Height,
                  OD.Millimetres (headSize (Head).Width_MM), OD.Millimetres (headSize (Head).Height_MM),
                  OD.Extent (Frame_Width (Head)), OD.Extent (Frame_Height (Head)));
            end;
         end if;
      end loop;
   end noteCurrentMode;

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
                  OD.Extent (Width), OD.Extent (Height), 0, 0);
            end if;
         end;
      end loop;

      for Head in Head_Index loop
         selectedHead := Head;
         -- GET_DISPLAY_INFO advertises the host's preferred viewport, not a
         -- required resource size. In particular GTK can replace the command
         -- line hint with its initial 640x480 widget size. Prefer validated
         -- EDID geometry within owned storage; otherwise retain 1024x768.
         -- Never size DMA allocations or copy spans from unchecked hints.
         if Head = 0 or else (secondDmaPhys /= 0 and then
           (for some Index in 1 .. scanouts.Count =>
              scanouts.Items (Index).Native_Number = Head))
         then
            selectPreferredMode (Head);
            trace ("paint test framebuffer");
            paintFramebuffer (0);
            paintFramebuffer (1);
            trace ("test framebuffer painted");
            if not bringUpScanout then
               return;
            end if;

            debugPrint ("virtio-gpu: scanout test frame presented" & LF);
            noteCurrentMode (Head);
            createCursor (Head);
            readyHeads (Head) := True;
         end if;
      end loop;
      selectedHead := 0;
   end initGpu;


   procedure clearFb (bufferIndex : Natural; color : Unsigned_32) is
      pixels : array (0 .. Natural (Frame_Width (selectedHead) * Frame_Height (selectedHead)) - 1) of Unsigned_32
        with Import, Address => DMA_BASE + framebufferOffset (bufferIndex);
   begin
      for i in pixels'Range loop
         pixels (i) := color;
      end loop;
   end clearFb;

   ---------------------------------------------------------------------------
   --  Mode changes (CuBit.Output_Modes). A host changes its preferred mode
   --  (QEMU: the window was resized, VIRTIO_GPU_EVENT_DISPLAY). The driver
   --  re-reads GET_DISPLAY_INFO, publishes the preference in its catalog
   --  (Advertised) under a new revision and answers the watcher. display.svc
   --  decides when to switch: Set_Mode on an idle device, fitted to the
   --  fixed scanout buffers (OM.Fit).
   ---------------------------------------------------------------------------
   VIRTIO_GPU_EVENT_DISPLAY : constant Unsigned_32 := 1;
   ISR_CONFIG_CHANGE : constant Unsigned_8 := 2;
   --  struct virtio_gpu_config: events_read, events_clear, num_scanouts.
   GPU_CONFIG_EVENTS_READ  : constant Unsigned_64 := 0;
   GPU_CONFIG_EVENTS_CLEAR : constant Unsigned_64 := 4;
   GPU_CONFIG_BYTES : constant := 16;
   CMD_RESOURCE_UNREF : constant Unsigned_32 := 16#0102#;
   deviceOff : Unsigned_64 := 0;
   deviceConfigReady : Boolean := False;
   modeRevision : OM.Revision := 1;
   watchArmed : Boolean := False;
   displayEventPending : Boolean := False;

   --  The device configuration structure, at its own offset in the BAR
   --  (read32/write32 address the common configuration).
   function configWord (Offset : Unsigned_64) return Unsigned_32 is
      Reg : Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (BAR_VIRT_BASE + deviceOff + Offset));
   begin
      return Reg;
   end configWord;
   procedure setConfigWord (Offset : Unsigned_64; Value : Unsigned_32) is
      Reg : Unsigned_32 with Import, Volatile,
        Address => To_Address (Integer_Address (BAR_VIRT_BASE + deviceOff + Offset));
   begin
      Reg := Value;
   end setConfigWord;

   --  Synchronous commands need the control queue to themselves.
   function commandsIdle return Boolean is
     ((for all P of Pending => P.Phase = Idle) and then
      cursorTransfer.Phase = Idle);

   procedure answerWatch is
      Ignored : Unsigned_64;
   begin
      if watchArmed then
         watchArmed := False;
         Ignored := Answer (Watch_Reply, CuBit.Desktop_Messages.From_Wire
           (OM.Encode_Watch (OD.GPU_Backend, modeRevision)));
      end if;
   end answerWatch;

   procedure bumpRevision is
   begin
      if modeRevision < OM.Revision'Last then
         modeRevision := modeRevision + 1;
      end if;
      answerWatch;
   end bumpRevision;

   --  VIRTIO_GPU_EVENT_DISPLAY: acknowledge it, re-read the host's preferred
   --  modes and publish those that changed.
   procedure readDisplayEvent is
      Events : Unsigned_32;
      Changed : Boolean := False;
      Reread : array (Head_Index) of Boolean := [others => False];
   begin
      displayEventPending := False;
      if not deviceConfigReady then return; end if;
      Events := configWord (GPU_CONFIG_EVENTS_READ);
      if (Events and VIRTIO_GPU_EVENT_DISPLAY) = 0 then
         debugPrint ("virtio-gpu: configuration change without display event" & LF);
         return;
      end if;
      setConfigWord (GPU_CONFIG_EVENTS_CLEAR, VIRTIO_GPU_EVENT_DISPLAY);
      beginCmd (CMD_GET_DISPLAY_INFO);
      if not submitCmd (24, 408, RESP_OK_DISPLAY_INFO) then
         fail ("GET_DISPLAY_INFO failed");
         return;
      end if;
      for Index in 1 .. scanouts.Count loop
         declare
            Item : OD.Description renames scanouts.Items (Index);
            Offset : constant Storage_Offset := DISPLAY_HEADER_BYTES +
              Storage_Offset (Item.Native_Number) * DISPLAY_ENTRY_BYTES;
            Width : constant Unsigned_32 := get32 (RESP_OFF, Offset + 8);
            Height : constant Unsigned_32 := get32 (RESP_OFF, Offset + 12);
            Enabled : constant Unsigned_32 := get32 (RESP_OFF, Offset + 16);
         begin
            --  A disabled or degenerate preference (a closed or minimized
            --  window) changes nothing: the output keeps its mode.
            if Enabled /= 0 and then
              Width in 1 .. Unsigned_32 (OD.Extent'Last) and then
              Height in 1 .. Unsigned_32 (OD.Extent'Last) and then
              (OD.Extent (Width) /= Item.Advertised_Width or else
               OD.Extent (Height) /= Item.Advertised_Height)
            then
               Item.Advertised_Width := OD.Extent (Width);
               Item.Advertised_Height := OD.Extent (Height);
               Changed := True;
               if Item.Native_Number <= Head_Index'Last then
                  Reread (Head_Index (Item.Native_Number)) := True;
               end if;
               debugPrint ("virtio-gpu: display change head" & Item.Native_Number'Image &
                 " preferred " & Dimensions (Natural (Width), Natural (Height)) & LF);
            end if;
         end;
      end loop;
      --  QEMU's EDID follows the window: re-read the mode lists (after the
      --  loop: GET_EDID reuses the response buffer the loop reads).
      for Head in Head_Index loop
         if Reread (Head) then
            declare
               Ignored_Base : EDID.Base_Block;
            begin
               if not readEDID (Head, Ignored_Base) then
                  monitors (Head).Known := False;
               end if;
            end;
         end if;
      end loop;
      if Changed then
         bumpRevision;
      end if;
   end readDisplayEvent;

   --  Switch Head to Wanted, fitted to its scanout buffers. Old resources go
   --  (UNREF also ends their scanout); new ones are built on the same fixed
   --  buffers, cleared to black. Only on an idle device (commandsIdle).
   --  The listed, admitted mode of Head an Exact_Rate request names; zero
   --  when none.
   function Named_Mode (Head : Head_Index; Wanted : OM.Mode_Request) return Natural is
      M : Head_Monitor renames monitors (Head);
   begin
      if Wanted.Rate /= OM.Exact_Rate or else not M.Known or else M.Modes = null then
         return 0;
      end if;
      for I in 1 .. M.Modes.Count loop
         if Sized (M.Modes.Items (I), Unsigned_32 (Wanted.Size.Width), Unsigned_32 (Wanted.Size.Height))
           and then Listed_Refresh (M.Modes.Items (I).Timing) = Wanted.Refresh
           and then Verdict_Of (M.Modes.Items (I).Timing) = OM.Admitted
         then
            return I;
         end if;
      end loop;
      return 0;
   end Named_Mode;

   --  Switch Head to Wanted: a Best_Rate size fitted to the scanout buffers
   --  (OM.Fit; a host window), an Exact_Rate one only when it names an
   --  admitted listed mode, at its size.
   procedure applyMode (Head : Head_Index; Wanted : OM.Mode_Request; Result : out OM.Mode_Result) is
      Named : constant Natural := Named_Mode (Head, Wanted);
      Fitted : constant OM.Mode :=
        (if Wanted.Rate = OM.Exact_Rate then Wanted.Size
         else OM.Fit (Wanted.Size, MINIMUM_MODE, OM.Byte_Count (bufferCapacity)));
      Black : constant Unsigned_32 := 0;
   begin
      Result := (OM.DP.Bad_State,
                 (OD.Extent (Frame_Width (Head)), OD.Extent (Frame_Height (Head))));
      if Wanted.Rate = OM.Exact_Rate and then Named = 0 then
         Result.Status := OM.DP.Invalid_Request;
         return;
      end if;
      if not readyHeads (Head) or else not commandsIdle then
         return;
      end if;
      monitors (Head).Chosen := Named;
      if Fitted = Result.Current then
         Result.Status := OM.DP.Success;
         return;
      end if;
      selectedHead := Head;
      for index in 0 .. 1 loop
         beginCmd (CMD_RESOURCE_UNREF);
         put32 (CMD_OFF, 24, resourceId (index));
         if not submitCmd (32, 24, RESP_OK_NODATA) then
            fail ("RESOURCE_UNREF failed");
            readyHeads (Head) := False;
            return;
         end if;
      end loop;
      geometry (Head) := (EDID.Extent (Fitted.Width), EDID.Extent (Fitted.Height));
      clearFb (0, Black);
      clearFb (1, Black);
      if not bringUpScanout then
         readyHeads (Head) := False;
         return;
      end if;
      noteCurrentMode (Head);
      debugPrint ("virtio-gpu: head" & Head'Image & " mode" &
        " " & Dimensions (Fitted.Width, Fitted.Height) & LF);
      Result := (OM.DP.Success, Fitted);
      bumpRevision;
   end applyMode;

   procedure finishPresentation (Head : Head_Index; Success : Boolean) is
      Item : Pending_Presentation renames Pending (Head);
      Response : constant Message :=
        (tag => (label => Item.Label, length => 1,
                 protocol => 0, ipc => <>, reserved => 0), authorityTag => 0,
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
         Ignored := Answer (Head_Replies (Head), Response);
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
               put64 (Cmd, 40, Unsigned_64 ((Item.Y * Natural (Frame_Width (Head)) + Item.X) * 4));
               put32 (Cmd, 48, Resource);
               Length := 56;
               GM.Add (uploads, Unsigned_64 (Item.W) * Unsigned_64 (Item.H) * 4);
            else
               put32 (Cmd, 40, Resource);
            end if;
         when Set_Scanout =>
            put32 (Cmd, 0, CMD_SET_SCANOUT);
            put32 (Cmd, 32, Frame_Width (Head));
            put32 (Cmd, 36, Frame_Height (Head));
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

   procedure finishCursorTransfer (Success : Boolean) is
      Ignored : Unsigned_64;
      Head : constant Head_Index := cursorTransfer.Head;
   begin
      if cursorTransfer.Phase /= Transfer then return; end if;
      if Success then
         cursors (Head).Defined := True;
         cursors (Head).Kind := CMD_UPDATE_CURSOR;
         cursors (Head).Position := cursorTransfer.Show.At_Position;
         cursors (Head).Hot_X := cursorTransfer.Show.Hot_X;
         cursors (Head).Hot_Y := cursorTransfer.Show.Hot_Y;
         cursors (Head).Dirty := True;
         flushCursors;
         cursorTransfer.Phase := Idle;
      else
         --  Like presentation failures: the backing stays owned by the
         --  device until a designed reset; no further cursor uploads.
         cursorTransfer.Phase := Quarantined;
         debugPrint ("virtio-gpu: cursor upload failed" & LF);
      end if;
      Ignored := Answer (Cursor_Reply, CuBit.Desktop_Messages.From_Wire
        (GP.Encode_Status (GP.Show_Plane,
           (if Success then GP.Accepted else GP.Bad_State))));
   end finishCursorTransfer;

   procedure issueCursorTransfer is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      Head : constant Head_Index := cursorTransfer.Head;
   begin
      if Fence_Sequence = Unsigned_64'Last or else
        Now > Unsigned_64'Last - Command_Timeout_Ms
      then
         finishCursorTransfer (False);
         return;
      end if;
      for Offset in Storage_Offset range 0 .. Command_Stride - 1 loop
         declare
            C : Unsigned_8 with Import, Address => DMA_BASE + CURSOR_TRANSFER_CMD + Offset;
            R : Unsigned_8 with Import, Address => DMA_BASE + CURSOR_TRANSFER_RESP + Offset;
         begin
            C := 0;
            R := 0;
         end;
      end loop;
      Fence_Sequence := Fence_Sequence + 1;
      cursorTransfer.Fence := Fence_Sequence;
      cursorTransfer.Deadline := Now + Command_Timeout_Ms;
      put32 (CURSOR_TRANSFER_CMD, 0, CMD_TRANSFER_TO_HOST_2D);
      put32 (CURSOR_TRANSFER_CMD, 4, 1); -- VIRTIO_GPU_FLAG_FENCE
      put64 (CURSOR_TRANSFER_CMD, 8, cursorTransfer.Fence);
      put32 (CURSOR_TRANSFER_CMD, 32, CURSOR_EXTENT);
      put32 (CURSOR_TRANSFER_CMD, 36, CURSOR_EXTENT);
      put32 (CURSOR_TRANSFER_CMD, 48, cursorResource (Head));
      descs (CURSOR_TRANSFER_ID) :=
        (dmaPhys + Unsigned_64 (CURSOR_TRANSFER_CMD), 56, VRING_DESC_F_NEXT,
         Unsigned_16 (CURSOR_TRANSFER_ID + 1));
      descs (CURSOR_TRANSFER_ID + 1) :=
        (dmaPhys + Unsigned_64 (CURSOR_TRANSFER_RESP), 24, VRING_DESC_F_WRITE, 0);
      avail.ring (Natural (avail.idx mod Unsigned_16 (QUEUE_SIZE))) :=
        Unsigned_16 (CURSOR_TRANSFER_ID);
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      avail.idx := avail.idx + 1;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      notifyQueue;
   end issueCursorTransfer;

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
         if Entry_Value.id = Unsigned_32 (CURSOR_TRANSFER_ID) then
            declare
               Fence_Seen : constant Unsigned_64 :=
                 Unsigned_64 (get32 (CURSOR_TRANSFER_RESP, 8)) or
                 Shift_Left (Unsigned_64 (get32 (CURSOR_TRANSFER_RESP, 12)), 32);
            begin
               finishCursorTransfer
                 (Entry_Value.len = 24 and then
                  get32 (CURSOR_TRANSFER_RESP, 0) = RESP_OK_NODATA and then
                  get32 (CURSOR_TRANSFER_RESP, 4) = 1 and then
                  Fence_Seen = cursorTransfer.Fence);
            end;
            goto Next_Completion;
         elsif Entry_Value.id /= 0 and then Entry_Value.id /= 2 then
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
         <<Next_Completion>>
      end loop;
      Now := syscall (SYSCALL_GETTIME);
      if cursorTransfer.Phase = Transfer and then Now >= cursorTransfer.Deadline then
         finishCursorTransfer (False);
         debugPrint ("virtio-gpu: cursor upload timeout" & LF);
      end if;
      if cursorsPending then
         flushCursors;
      end if;
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

   procedure handleRequest (from : Process_ID; incoming : Message) is
      request : Message := incoming;
      replyMsg : Message := NULL_MESSAGE;
      ignore : Unsigned_64;
   begin
      if request.tag.label = OD.Code (OD.GPU_Backend, OD.Get_Catalog) or else
         request.tag.label = OD.Code (OD.GPU_Backend, OD.Get_Description)
      then
         replyMsg := CuBit.Desktop_Messages.From_Wire (OD.Respond
           (OD.GPU_Backend, scanouts, modeRevision,
            CuBit.Desktop_Messages.To_Wire (request)));
         ignore := reply (from, replyMsg);
         return;
      end if;
      if request.tag.label = OM.Code (OD.GPU_Backend, OM.Watch) then
         declare
            D : constant OM.Revision_Decoding :=
              OM.Decode_Watch (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (request));
         begin
            if not D.Valid then
               replyMsg.tag := (label => request.tag.label, length => 1,
                                protocol => 0, ipc => <>, reserved => 0);
               replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
            elsif D.Value /= modeRevision then
               replyMsg := CuBit.Desktop_Messages.From_Wire
                 (OM.Encode_Watch (OD.GPU_Backend, modeRevision));
            else
               --  One watcher: a newer watch supersedes (and answers) an
               --  older one, so no saved reply is ever stranded.
               answerWatch;
               if Keep (Watch_Reply) then
                  watchArmed := True;
                  return; -- answered on the next revision
               end if;
               replyMsg.tag := (label => request.tag.label, length => 1,
                                protocol => 0, ipc => <>, reserved => 0);
               replyMsg.words (0) := GPU_ERR_BAD_STATE;
            end if;
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      if request.tag.length /= 4 or else request.tag.protocol /= 0 or else
        request.tag.reserved > Unsigned_16 (Head_Index'Last) or else
        not readyHeads (Natural (request.tag.reserved))
      then
         replyMsg.tag := (label => request.tag.label, length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
         ignore := reply (from, replyMsg);
         return;
      end if;
      selectedHead := Natural (request.tag.reserved);
      request.tag.reserved := 0;
      if request.tag.label = OM.Code (OD.GPU_Backend, OM.Set_Mode) then
         declare
            D : constant OM.Request_Decoding :=
              OM.Decode_Set_Mode (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (request));
            Result : OM.Mode_Result :=
              (OM.DP.Invalid_Request,
               (OD.Extent (Frame_Width (selectedHead)), OD.Extent (Frame_Height (selectedHead))));
            Head : constant Head_Index := selectedHead;
         begin
            if D.Valid then
               applyMode (Head, D.Value, Result);
            end if;
            selectedHead := Head;
            ignore := reply (from, CuBit.Desktop_Messages.From_Wire
              (OM.Encode_Result (OD.GPU_Backend, Result)));
            return;
         end;
      end if;
      --  DISP-016: the head's listed modes and its monitor's identity.
      if request.tag.label = OM.Code (OD.GPU_Backend, OM.List_Mode) then
         declare
            Wanted : constant OM.Index_Decoding :=
              OM.Decode_List_Request (OD.GPU_Backend, CuBit.Desktop_Messages.To_Wire (request));
            M : Head_Monitor renames monitors (selectedHead);
         begin
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
            if Wanted.Valid and then M.Known and then M.Modes /= null and then
              M.Modes.Count <= Natural (OM.Mode_Count'Last) and then
              Natural (Wanted.Value) <= M.Modes.Count
            then
               declare
                  Item : CuBit.Monitor_EDID.Catalog.Mode renames M.Modes.Items (Natural (Wanted.Value));
               begin
                  replyMsg := CuBit.Desktop_Messages.From_Wire (OM.Encode_Listing
                    (OD.GPU_Backend,
                     (Index => Wanted.Value, Count => OM.Mode_Count (M.Modes.Count),
                      Item => ((OD.Extent (Item.Timing.Width), OD.Extent (Item.Timing.Height)),
                               Listed_Refresh (Item.Timing), Unsigned_32 (Item.Timing.Clock) * 10,
                               Verdict_Of (Item.Timing), Item.Preferred,
                               Natural (Wanted.Value) = Current_Mode (selectedHead)))));
               end;
            end if;
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      if request.tag.label = OM.Code (OD.GPU_Backend, OM.Identify) then
         declare
            M : Head_Monitor renames monitors (selectedHead);
            Item : OM.Monitor_Identity;
         begin
            replyMsg.tag := (label => request.tag.label, length => 1,
                             protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
            if M.Known and then CuBit.Desktop_Messages.To_Wire (request) = OM.Identify_Request (OD.GPU_Backend) then
               for I in 1 .. 3 loop
                  if M.Identity.Vendor (I) in 'A' .. 'Z' then
                     Item.Vendor (I) := M.Identity.Vendor (I);
                  end if;
               end loop;
               Item.Product := M.Identity.Product;
               Item.Serial := M.Identity.Serial;
               Item.Name_Last := M.Identity.Name.Length;
               for I in 1 .. M.Identity.Name.Length loop
                  Item.Name (I) := (if M.Identity.Name.Text (I) in OM.Name_Character
                                    then M.Identity.Name.Text (I) else '?');
               end loop;
               replyMsg := CuBit.Desktop_Messages.From_Wire (OM.Encode_Identity (OD.GPU_Backend, Item));
            end if;
            ignore := reply (from, replyMsg);
            return;
         end;
      end if;
      --  Cursor planes use their own queue and transfer slot: they never
      --  wait for, or disturb, a presentation in flight on the head.
      if request.tag.label in GP.Code (GP.Query) .. GP.Code (GP.Hide_Plane) then
         declare
            Wire : constant CuBit.Display_Protocol.Wire_Message :=
              CuBit.Desktop_Messages.To_Wire (request);
            C : Cursor_State renames cursors (selectedHead);
            Result : CuBit.Display_Protocol.Wire_Message;
            function Status (Kind : GP.Operation; Item : GP.Status)
              return CuBit.Display_Protocol.Wire_Message
              renames GP.Encode_Status;
         begin
            if request.tag.label = GP.Code (GP.Query) then
               declare
                  D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Query, Wire);
               begin
                  if D.Valid and then D.Value = CURSOR_PLANE and then C.Ready then
                     Result := GP.Encode (GP.Description'
                       (Count => CURSOR_PLANES,
                        Descriptor =>
                          (Kinds => [DPL.Cursor => True, others => False],
                           Formats => [DPL.ARGB8888 => True, others => False],
                           Max_Width => CURSOR_EXTENT,
                           Max_Height => CURSOR_EXTENT,
                           Scaler => False, Z => CURSOR_Z,
                           --  QEMU draws it as the host pointer image.
                           Host_Pointer => True)));
                  else
                     Result := Status (GP.Query, GP.Unsupported);
                  end if;
               end;
            elsif request.tag.label = GP.Code (GP.Map_Plane) then
               declare
                  D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Map_Plane, Wire);
                  Reference : CuBit.Memory_Grants.Grant_Reference;
                  Granted : Boolean := False;
               begin
                  if D.Valid and then D.Value = CURSOR_PLANE and then C.Ready then
                     CuBit.Memory_Grants.Create_For_Process
                       (grantee => from,
                        localAddr => DMA_BASE + cursorImageOffset (selectedHead),
                        numPages => CURSOR_IMAGE_BYTES / 4096,
                        readWrite => True, reference => Reference,
                        success => Granted);
                  end if;
                  Result := (if Granted then GP.Encode (GP.Buffer'
                               (Reference, CURSOR_EXTENT, CURSOR_EXTENT))
                             else Status (GP.Map_Plane, GP.Bad_State));
               end;
            elsif request.tag.label = GP.Code (GP.Show_Plane) then
               declare
                  D : constant GP.Show_Decoding := GP.Decode_Show (Wire);
               begin
                  if D.Valid and then D.Value.Plane = CURSOR_PLANE and then
                    C.Ready and then cursorTransfer.Phase = Idle and then
                    Keep (Cursor_Reply)
                  then
                     cursorTransfer := (Phase => Transfer, Head => selectedHead,
                                        Show => D.Value, others => <>);
                     issueCursorTransfer;
                     return; -- answered when the upload completes
                  end if;
                  Result := Status (GP.Show_Plane, GP.Bad_State);
               end;
            elsif request.tag.label = GP.Code (GP.Move_Plane) then
               declare
                  D : constant GP.Move_Decoding := GP.Decode_Move (Wire);
               begin
                  if D.Valid and then D.Value.Plane = CURSOR_PLANE and then
                    C.Defined and then not cursorFaulted
                  then
                     C.Position := D.Value.At_Position;
                     C.Dirty := True;
                     flushCursors;
                     Result := Status (GP.Move_Plane, GP.Accepted);
                  else
                     Result := Status (GP.Move_Plane, GP.Bad_State);
                  end if;
               end;
            else
               declare
                  D : constant GP.Plane_Decoding := GP.Decode_Plane (GP.Hide_Plane, Wire);
               begin
                  if D.Valid and then D.Value = CURSOR_PLANE and then
                    C.Ready and then not cursorFaulted
                  then
                     C.Defined := False;
                     C.Kind := CMD_UPDATE_CURSOR;
                     C.Dirty := True;
                     flushCursors;
                     Result := Status (GP.Hide_Plane, GP.Accepted);
                  else
                     Result := Status (GP.Hide_Plane, GP.Bad_State);
                  end if;
               end;
            end if;
            ignore := reply (from, CuBit.Desktop_Messages.From_Wire (Result));
            return;
         end;
      end if;
      if Pending (selectedHead).Phase /= Idle and then
        request.tag.label not in OP_GPU_GET_INFO | OP_GPU_GET_STATUS
      then
         replyMsg.tag := (label => request.tag.label, length => 1,
                          protocol => 0, ipc => <>, reserved => 0);
         replyMsg.words (0) := GPU_ERR_BAD_STATE;
         ignore := reply (from, replyMsg);
         return;
      end if;
      case request.tag.label is
         when OP_GPU_GET_INFO =>
            replyMsg.tag := (label => OP_GPU_GET_INFO,
                             length => 4, protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := Unsigned_64 (Frame_Width (selectedHead));
            replyMsg.words (1) := Unsigned_64 (Frame_Height (selectedHead));
            replyMsg.words (2) := Unsigned_64 (Frame_Width (selectedHead)) * 4;
            replyMsg.words (3) := 32;

         when OP_GPU_GET_STATUS =>
            replyMsg.tag := (label => OP_GPU_GET_STATUS,
                             length => 4, protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) :=
              (if Pending (selectedHead).Phase = Quarantined
               then GPU_ERR_BAD_STATE else GPU_OK);
            replyMsg.words (1) :=
              (if Pending (selectedHead).Phase = Quarantined then 0 else 1);
            replyMsg.words (2) := Unsigned_64 (Frame_Width (selectedHead));
            replyMsg.words (3) := Unsigned_64 (Frame_Height (selectedHead));

         when OP_GPU_MAP_FRAMEBUFFER =>
            declare
               bufferIndexRaw : constant Unsigned_64 := request.words (0);
               bufferIndex : Natural range 0 .. 1 := 0;
               pages : constant Natural :=
                  Natural ((Unsigned_64 (Frame_Bytes (selectedHead)) + 4095) / 4096);
               reference : CuBit.Memory_Grants.Grant_Reference;
               grantOk : Boolean;
            begin
               replyMsg.tag := (label => OP_GPU_MAP_FRAMEBUFFER,
                                length => 4, protocol => 0, ipc => <>, reserved => 0);
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
                  replyMsg.words (2) := Unsigned_64 (Frame_Width (selectedHead)) or
                     Shift_Left (Unsigned_64 (Frame_Height (selectedHead)), 32);
                  replyMsg.words (3) := Unsigned_64 (Frame_Width (selectedHead)) * 4;
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
                                length => 1, protocol => 0, ipc => <>, reserved => 0);
               if bufferIndexRaw > 1 or else request.words (3) /= 0 or else
                 (packedXY and 16#FFFF_FFFF#) >= Unsigned_64 (Frame_Width (selectedHead)) or else
                 Shift_Right (packedXY, 32) >= Unsigned_64 (Frame_Height (selectedHead)) or else
                 (packedWH and 16#FFFF_FFFF#) not in 1 .. Unsigned_64 (Frame_Width (selectedHead)) or else
                 Shift_Right (packedWH, 32) not in 1 .. Unsigned_64 (Frame_Height (selectedHead))
               then
                  replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
               else
                  bufferIndex := Natural (bufferIndexRaw);
                  if Keep (Head_Replies (selectedHead)) then
                     Pending (selectedHead) :=
                       (Phase => Transfer, Buffer => bufferIndex,
                        X => Natural (packedXY and 16#FFFF_FFFF#),
                        Y => Natural (Shift_Right (packedXY, 32)),
                        W => Natural (packedWH and 16#FFFF_FFFF#),
                        H => Natural (Shift_Right (packedWH, 32)),
                        Clearing => False, Label => OP_GPU_PRESENT_BUFFER,
                        others => <>);
                     Pending (selectedHead).W := Natural'Min
                       (Pending (selectedHead).W, Natural (Frame_Width (selectedHead)) - Pending (selectedHead).X);
                     Pending (selectedHead).H := Natural'Min
                       (Pending (selectedHead).H, Natural (Frame_Height (selectedHead)) - Pending (selectedHead).Y);
                     issueCommand (selectedHead);
                     return; -- saved reply consumed only by completion/failure
                  end if;
                  replyMsg.words (0) := GPU_ERR_BAD_STATE;
               end if;
            end;

         when OP_GPU_CLEAR =>
            replyMsg.tag := (label => OP_GPU_CLEAR,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            if GPU_Test_Policy.Reject_Client_Clear and then
              Test_Startup_Clear_Seen (selectedHead)
            then
               debugPrint ("virtio-gpu: injected clear rejection head" & selectedHead'Image & LF);
            elsif Keep (Head_Replies (selectedHead)) then
               if GPU_Test_Policy.Reject_Client_Clear then
                  Test_Startup_Clear_Seen (selectedHead) := True;
               end if;
               clearFb (0, Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
               clearFb (1, Unsigned_32 (request.words (0) and 16#FFFF_FFFF#));
               Pending (selectedHead) :=
                 (Phase => Transfer, Buffer => 1, X => 0, Y => 0,
                  W => Natural (Frame_Width (selectedHead)), H => Natural (Frame_Height (selectedHead)),
                  Clearing => True, Label => OP_GPU_CLEAR, others => <>);
               issueCommand (selectedHead);
               return;
            end if;
            replyMsg.words (0) := GPU_ERR_BAD_STATE;

         when others =>
            replyMsg.tag := (label => request.tag.label,
                             length => 1, protocol => 0, ipc => <>, reserved => 0);
            replyMsg.words (0) := GPU_ERR_UNSUPPORTED;
      end case;

      ignore := reply (from, replyMsg);
   end handleRequest;

   eventMsg : Message;
   eventFound : Boolean;
   from : Process_ID;
   msg : Message;
   found : Boolean;
begin
   debugPrint ("virtio-gpu: starting" & LF);

   trace ("read launch configuration");
   --  devmgr's typed launch configuration for this driver alone.
   declare
      Configuration : Launch_Configuration.Virtio_GPU_Configuration;
      Configured : Boolean;
   begin
      CuBit.Launch_Configurations.Get (Configuration, Configured);
      if not Configured then
         fail ("missing launch configuration");
         return;
      end if;
      barPhys := Configuration.Registers;
      dmaPhys := Configuration.DMA;
      secondDmaPhys := Configuration.Second_DMA;
      dmaBytes := Configuration.DMA_Bytes;
      commonOff := Configuration.Common_Offset;
      notifyOff := Configuration.Notify_Offset;
      isrOff := Configuration.ISR_Offset;
      deviceOff := Configuration.Device_Offset;
      notifyMult := Configuration.Notify_Multiplier;
      gpuPrimary := Configuration.Boot_Display;
   end;
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

   if barPhys = 0 or else dmaPhys = 0 then
      fail ("missing devmgr transport info");
      return;
   end if;

   --  Bank size from the launch configuration's DMA bytes: the bytes of one
   --  bank, or of two equal banks when the second exists.
   declare
      Total : Unsigned_64 := dmaBytes;
   begin
      if secondDmaPhys /= 0 then
         Total := Total / 2;
      end if;
      if Total = Unsigned_64 (LARGE_BANK_BYTES) then
         bankBytes := LARGE_BANK_BYTES;
      elsif Total = Unsigned_64 (SMALL_BANK_BYTES) then
         bankBytes := SMALL_BANK_BYTES;
      else
         fail ("unexpected DMA bank size");
         return;
      end if;
   end;
   initTransport;
   --  The device configuration (display events) is optional: without it the
   --  outputs keep their startup modes.
   deviceConfigReady := deviceOff /= 0 and then deviceOff mod 4 = 0 and then
     deviceOff <= barBytes and then GPU_CONFIG_BYTES <= barBytes - deviceOff;
   if deviceConfigReady then
      debugPrint ("virtio-gpu: display events enabled" & LF);
   else
      debugPrint ("virtio-gpu: no device configuration; modes fixed" & LF);
   end if;
   initGpu;

   declare
      ignore : Unsigned_64;
   begin
      if Startup.Registration.Present then
         ignore := registerDriver (DRIVER_GPU, Startup.Registration.Value);
      end if;
   end;

   debugPrint ("virtio-gpu: ready" & LF);
   signalReady (16#FF00#);

   loop
      collectCommands;
      if displayEventPending and then commandsIdle then
         readDisplayEvent;
      end if;
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
      if eventFound and then messageInterrupts then
         --  MSI-X needs no acknowledgement in the device. A configuration
         --  change (display event) has its own entry when the device has
         --  two; otherwise any doorbell may be one, and readDisplayEvent
         --  checks events_read once the control queue is idle.
         if interruptBlock.Count < 2 or else
           CuBit.Device_Interrupts.Fired (eventMsg, interruptBlock, Natural (CONFIG_ENTRY))
         then
            displayEventPending := True;
         end if;
      elsif eventFound then
         declare
            isr : Unsigned_8 with
               Import,
               Address => To_Address (Integer_Address (BAR_VIRT_BASE + isrOff)),
               Volatile;
            --  Reading the ISR acknowledges (clears) it: read it exactly once.
            Status : constant Unsigned_8 := isr;
         begin
            --  A configuration change (display event) is handled once the
            --  control queue is idle.
            if Status /= 16#FF# and then (Status and ISR_CONFIG_CHANGE) /= 0 then
               debugPrint ("virtio-gpu: configuration change interrupt" & LF);
               displayEventPending := True;
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
            if cursorTransfer.Phase = Transfer then
               Deadline := Unsigned_64'Min (Deadline, cursorTransfer.Deadline);
            end if;
            --  A display event waits for in-flight commands to complete;
            --  their completions wake this wait.
            --  A cursor queue that was full is retried by polling soon.
            if cursorsPending then
               Deadline := Unsigned_64'Min (Deadline, syscall (SYSCALL_GETTIME) + 1);
            end if;
            if Wait_For_Activity_Until (Deadline) = Unavailable then
               fail ("activity wait unavailable");
               return;
            end if;
         end;
      end if;
   end loop;
end main;
