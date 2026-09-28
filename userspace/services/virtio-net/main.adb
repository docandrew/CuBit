------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace virtio-net driver — thin packet mover.
--
--  Initializes virtio-net legacy PCI device, reads MAC address,
--  sets up RX and TX virtqueues. All protocol processing (ARP, IPv4,
--  ICMP) is handled by the netstack service. This driver communicates
--  with netstack via IPC and a shared memory grant.
--
--  DMA region at 0x7000_0000_0000 (mapped by kernel):
--    0x0000 .. 0x2FFF : RX vring (3 pages)
--    0x3000 .. 0x5FFF : TX vring (3 pages)
--    0x6000+          : Packet buffers (112 x 2K: 0-31 RX, 32-111 TX; the
--                       256 KiB region devmgr allocates holds 116)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Machine_Code;

with CuBit.Messages; use CuBit.Messages;
with Virtio;
with Descriptor_Pool;
with Frame_Ring;
with Virtqueue_Index;
with CuBit.Virtio_Net_Control;

procedure main is
   use ASCII;

   DMA_BASE      : constant System.Address :=
      To_Address (16#0000_7000_0000_0000#);
   DMA_PHYS_BASE : Unsigned_64;

   --  RX queue = 0, TX queue = 1
   RX_QUEUE : constant Unsigned_16 := 0;
   TX_QUEUE : constant Unsigned_16 := 1;

   --  Buffer layout (devmgr maps 1 MiB): a receive buffer for every queue
   --  entry, so the device can hold a full window of frames while we are
   --  busy, then 80 TX buffers; each 2KB. TCP sends in bursts; the device
   --  may not have consumed earlier frames yet.
   NUM_RX_BUFS  : constant := Virtio.QUEUE_SIZE;
   NUM_TX_BUFS  : constant := 80;
   TX_BUF_FIRST : constant := NUM_RX_BUFS;
   package TX_Descriptors is new Descriptor_Pool (Count => NUM_TX_BUFS);
   --  Receive buffers with the device (posted): the device's returned ids
   --  are accepted only for buffers it holds, so none is posted twice.
   package RX_Buffers is new Descriptor_Pool (Count => NUM_RX_BUFS);
   RX_BUF_SIZE  : constant := 2048;

   --  Virtio-net header is 10 bytes (legacy), prepended to every packet
   VIRTIO_NET_HDR_SIZE : constant := 10;

   --  vring area offsets within DMA region
   RX_VRING_OFFSET : constant Storage_Offset := 0;

   --  I/O base from sysinfo
   ioBase : Unsigned_16;

   --  MAC address (raw bytes)
   mac : array (0 .. 5) of Unsigned_8;

   --  Grant region constants (must match kernel process.ads)
   GRANT_REGION_BASE : constant Integer_Address := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Integer_Address := 4096 * 4096;

   --  Cap slot for netstack endpoint (granted by kernel modules.adb)
   CAP_SLOT_NETSTACK : constant CapabilitySlot := 7;

   --  IPC label constants (must match kernel ipc_labels.ads)
   OP_NET_ATTACH : constant Unsigned_32 := 16#0400#;
   OP_NET_RX     : constant Unsigned_32 := 16#0401#;
   OP_NET_TX     : constant Unsigned_32 := 16#0402#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;

   --  Netstack connection state
   grantId        : Unsigned_64 := 0;
   grantBufSize   : Unsigned_64 := 0;
   grantBase      : System.Address := System.Null_Address;

   ---------------------------------------------------------------------------
   --  RX vring components (overlay on DMA memory)
   ---------------------------------------------------------------------------
   rxDescs : Virtio.DescArray with
      Import, Address => DMA_BASE + RX_VRING_OFFSET;

   rxAvail : Virtio.VringAvail with
      Import, Address => DMA_BASE + RX_VRING_OFFSET + 16#1000#;

   rxUsed : Virtio.VringUsed with
      Import, Address => DMA_BASE + RX_VRING_OFFSET + 16#2000#;

   --  TX vring (starts at 0x3000)
   TX_VRING_OFFSET : constant Storage_Offset := 16#3000#;

   txDescs : Virtio.DescArray with
      Import, Address => DMA_BASE + TX_VRING_OFFSET;

   txAvail : Virtio.VringAvail with
      Import, Address => DMA_BASE + TX_VRING_OFFSET + 16#1000#;

   txUsed : Virtio.VringUsed with
      Import, Address => DMA_BASE + TX_VRING_OFFSET + 16#2000#;

   --  Packet buffers start at offset 0x6000
   PACKET_BUF_OFFSET : constant Storage_Offset := 16#6000#;

   --  Track our position in the used rings
   lastRXUsedIdx : Unsigned_16 := 0;
   lastTXUsedIdx : Unsigned_16 := 0;

   ---------------------------------------------------------------------------
   --  TX free descriptor stack
   ---------------------------------------------------------------------------
   --  Transmit descriptor ownership (proved, userspace/net/src
   --  descriptor_pool): the device's returned ids are accepted only for
   --  descriptors in flight, so no buffer is handed out twice.
   txPool : TX_Descriptors.Pool;
   rxPool : RX_Buffers.Pool;

   function allocTXDesc return Integer is
      d  : TX_Descriptors.Id;
      ok : Boolean;
   begin
      TX_Descriptors.Take (txPool, d, ok);
      return (if ok then d else -1);
   end allocTXDesc;

   procedure freeTXDesc (id : Unsigned_32) is
      ignore : Boolean;
   begin
      TX_Descriptors.Give_Back (txPool, id, ignore);
   end freeTXDesc;

   ---------------------------------------------------------------------------
   --  Print helpers (minimal set for driver diagnostics)
   ---------------------------------------------------------------------------
   function hexDigit (n : Unsigned_8) return Character is
      hex : constant String := "0123456789ABCDEF";
   begin
      return hex (Natural (n) + 1);
   end hexDigit;

   procedure printHex8 (val : Unsigned_8) is
      s : String (1 .. 2);
   begin
      s (1) := hexDigit (Shift_Right (val, 4) and 16#0F#);
      s (2) := hexDigit (val and 16#0F#);
      debugPrint (s);
   end printHex8;

   procedure printMAC is
   begin
      for i in mac'Range loop
         if i > 0 then
            debugPrint (":");
         end if;
         printHex8 (mac (i));
      end loop;
   end printMAC;

   procedure printDec (val : Unsigned_32) is
      buf : String (1 .. 10);
      pos : Natural := buf'Last;
      v   : Unsigned_32 := val;
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

   ---------------------------------------------------------------------------
   --  bufAddr - compute virtual address of a packet buffer by index
   ---------------------------------------------------------------------------
   function bufAddr (idx : Natural) return System.Address is
   begin
      return DMA_BASE + PACKET_BUF_OFFSET +
         Storage_Offset (idx) * Storage_Offset (RX_BUF_SIZE);
   end bufAddr;

   ---------------------------------------------------------------------------
   --  bufPhys - compute physical address of a packet buffer by index
   ---------------------------------------------------------------------------
   function bufPhys (idx : Natural) return Unsigned_64 is
   begin
      return DMA_PHYS_BASE +
         Unsigned_64 (PACKET_BUF_OFFSET) +
         Unsigned_64 (idx) * Unsigned_64 (RX_BUF_SIZE);
   end bufPhys;

   ---------------------------------------------------------------------------
   --  setupRXQueue
   ---------------------------------------------------------------------------
   procedure setupRXQueue is
      d  : RX_Buffers.Id;
      ok : Boolean;
   begin
      RX_Buffers.Initialize (rxPool);
      for i in 0 .. NUM_RX_BUFS - 1 loop
         rxDescs (i) := (addr  => bufPhys (i),
                         len   => RX_BUF_SIZE,
                         flags => Virtio.VRING_DESC_F_WRITE,
                         next  => 0);
      end loop;
      --  Post every buffer.
      for i in 0 .. NUM_RX_BUFS - 1 loop
         RX_Buffers.Take (rxPool, d, ok);
         rxAvail.ring (i) := Unsigned_16 (d);
      end loop;

      rxAvail.flags := 0;
      rxAvail.idx   := Unsigned_16 (NUM_RX_BUFS);
   end setupRXQueue;

   ---------------------------------------------------------------------------
   --  submitTX - send a frame via the TX virtqueue
   ---------------------------------------------------------------------------
   txPending : Boolean := False;
   VRING_USED_F_NO_NOTIFY : constant Unsigned_16 := 1;
   --  In our avail rings: the device need not interrupt on completions.
   VRING_AVAIL_F_NO_INTERRUPT : constant Unsigned_16 := 1;
   --  At most this many frames in flight counts as sparse traffic.
   SPARSE_TX_FRAMES : constant := 2;

   procedure submitTX (frameAddr : System.Address; frameLen : Natural) is
      descIdx : Integer;
      txBuf   : System.Address;
   begin
      descIdx := allocTXDesc;
      if descIdx < 0 then
         debugPrint ("TX: no free descriptors" & LF);
         return;
      end if;

      --  TX descriptor D uses buffer TX_BUF_FIRST + D (the descriptor
      --  table has QUEUE_SIZE entries; buffer indices go past it).
      txBuf := bufAddr (TX_BUF_FIRST + descIdx);

      --  Zero the 10-byte virtio-net header
      declare
         vhdr : array (0 .. VIRTIO_NET_HDR_SIZE - 1) of Unsigned_8 with
            Import, Address => txBuf;
      begin
         for i in vhdr'Range loop
            vhdr (i) := 0;
         end loop;
      end;

      --  Copy frame data after the virtio-net header
      declare
         type Frame is array (0 .. frameLen - 1) of Unsigned_8;
         src : Frame with Import, Address => frameAddr;
         dst : Frame with
            Import, Address => txBuf + Storage_Offset (VIRTIO_NET_HDR_SIZE);
      begin
         dst := src;
      end;

      --  Set up the descriptor
      txDescs (descIdx) :=
         (addr  => bufPhys (TX_BUF_FIRST + descIdx),
          len   => Unsigned_32 (VIRTIO_NET_HDR_SIZE + frameLen),
          flags => 0,
          next  => 0);

      --  Add to available ring
      txAvail.ring (Natural (txAvail.idx mod Virtio.QUEUE_SIZE)) :=
         Unsigned_16 (descIdx);
      txAvail.idx := txAvail.idx + 1;

      --  The device is kicked once for a run of frames (kickTX).
      txPending := True;
   end submitTX;

   ---------------------------------------------------------------------------
   --  kickTX - tell the device about the frames added since the last kick,
   --  unless it asked not to be notified (it is already processing the
   --  ring: VRING_USED_F_NO_NOTIFY).
   ---------------------------------------------------------------------------
   procedure kickTX is
      usedFlags : Unsigned_16 with Volatile, Import, Address => txUsed'Address;
   begin
      if txPending then
         txPending := False;
         --  The avail index must be visible before the flags are read.
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         if (usedFlags and VRING_USED_F_NO_NOTIFY) = 0 then
            Virtio.notifyQueue (ioBase, TX_QUEUE);
         end if;
      end if;
   end kickTX;

   ---------------------------------------------------------------------------
   --  kickRX - after replenishing receive buffers, notify the device only
   --  if it asked to be (it had run out: VRING_USED_F_NO_NOTIFY clear).
   ---------------------------------------------------------------------------
   procedure kickRX is
      usedFlags : Unsigned_16 with Volatile, Import, Address => rxUsed'Address;
   begin
      --  The replenished avail index must be visible before the flags are read.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      if (usedFlags and VRING_USED_F_NO_NOTIFY) = 0 then
         Virtio.notifyQueue (ioBase, RX_QUEUE);
      end if;
   end kickRX;



   ---------------------------------------------------------------------------
   --  processTXUsed - reclaim completed TX descriptors
   ---------------------------------------------------------------------------
   procedure processTXUsed is
      use Virtqueue_Index;
      taken : constant Natural :=
        New_Entries (Index (lastTXUsedIdx), Index (txUsed.idx),
                     NUM_TX_BUFS - txPool.Top);
   begin
      for K in 1 .. taken loop
         freeTXDesc (txUsed.ring (Slot (Index (lastTXUsedIdx))).id);
         lastTXUsedIdx := lastTXUsedIdx + 1;
      end loop;
   end processTXUsed;

   ---------------------------------------------------------------------------
   --  The TX ring in the grant's second half (netstack's doSendFrame is the
   --  producer). Its first 2 KiB slot holds the indices: netstack's
   --  producer count at byte 0, ours (the consumer) at byte 64, both
   --  free-running. Frame N is in slot (N mod TX_RING_SLOTS) + 1: its
   --  length (4 bytes) at the slot's start, the frame TX_FRAME_HEADER bytes
   --  in. A slot is netstack's again once the consumer count passes it.
   ---------------------------------------------------------------------------
   TX_RING_SLOT_BYTES : constant := 2048;
   TX_FRAME_HEADER    : constant := 16;
   TX_CONSUMER_OFFSET : constant := 64;
   txRingSlots : Unsigned_32 := 0;   --  set from the grant's size at attach
   txConsumed  : Unsigned_32 := 0;
   --  Our doorbell epoch in the TX area (netstack reads it): a new nonzero
   --  value each time we are about to wait.
   TX_DOORBELL_OFFSET : constant := 68;
   txIdleEpoch : Unsigned_32 := 0;

   --  Copy queued frames to the device; True if any were taken.
   function drainTXRing return Boolean is
      area : constant System.Address :=
        grantBase + Storage_Offset (grantBufSize / 2);
      produced : Unsigned_32 with Volatile, Import, Address => area;
      consumed : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (TX_CONSUMER_OFFSET);
      available : Unsigned_32;
      took : Boolean := False;
   begin
      if grantBase = System.Null_Address or else txRingSlots = 0 then
         return False;
      end if;
      available := produced;
      --  The frames were written before the count was: read them after.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      --  A producer count too far ahead is not ours to trust.
      if Frame_Ring.Fill (available, txConsumed) > txRingSlots then
         return False;
      end if;
      while txConsumed /= available loop
         processTXUsed;
         if txPool.Top = 0 then
            --  Out of descriptors with frames waiting: let the device's
            --  completion wake us to reclaim them.
            txAvail.flags := 0;
            System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
            processTXUsed;
            exit when txPool.Top = 0;
         end if;
         if txAvail.flags /= VRING_AVAIL_F_NO_INTERRUPT then
            txAvail.flags := VRING_AVAIL_F_NO_INTERRUPT;
         end if;
         declare
            slot : constant System.Address :=
              area + Storage_Offset
                (Frame_Ring.Slot_Of (txConsumed, Natural (txRingSlots)) * TX_RING_SLOT_BYTES);
            len : Unsigned_32 with Volatile, Import, Address => slot;
            frameLen : constant Unsigned_32 := len;
         begin
            if frameLen >= 14 and then
              Frame_Ring.Fits (frameLen, TX_RING_SLOT_BYTES, TX_FRAME_HEADER)
            then
               submitTX (slot + Storage_Offset (TX_FRAME_HEADER), Natural (frameLen));
            end if;
         end;
         txConsumed := txConsumed + 1;
         took := True;
      end loop;
      if took then
         --  Copied out before the slots are handed back.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         consumed := txConsumed;
         kickTX;
      end if;
      return took;
   end drainTXRing;

   ---------------------------------------------------------------------------
   --  processRX - check used ring, forward packets to netstack via grant
   ---------------------------------------------------------------------------
   --  Received frames go to netstack through a ring in the grant's RX
   --  half (netstack's drainRXRing reads the same layout). Slot 0 holds
   --  the counts and flags; frame N is in slot (N mod rxRingSlots) + 1, its
   --  length (4 bytes, little endian) in the slot's first RX_SLOT_HEADER
   --  bytes and the Ethernet frame after them.
   --  - produced (byte 0, ours) and consumed (byte 64, netstack's) are
   --    free-running frame counts;
   --  - space wanted (byte 4, ours): the ring was full; netstack rings our
   --    doorbell (OP_NET_TX) once it frees slots;
   --  - doorbell wanted (byte 68, netstack's): a nonzero epoch while
   --    netstack is idle; one OP_NET_RX (one-way) per epoch wakes it.
   --  No call and no reply per batch: we never wait for netstack.
   RX_SLOT_BYTES  : constant := 2048;
   RX_SLOT_HEADER : constant := 16;
   RX_SPACE_WANTED_OFFSET : constant := 4;
   RX_CONSUMER_OFFSET     : constant := 64;
   RX_DOORBELL_OFFSET     : constant := 68;
   type Frame_Bytes is array (Natural range <>) of Unsigned_8;
   rxRingSlots : Unsigned_32 := 0;   --  set from the grant's size at attach
   rxProduced  : Unsigned_32 := 0;
   rxRungEpoch : Unsigned_32 := 0;   --  the netstack epoch we last woke
   --  processRX stopped at a full ring: the used entries left are not new
   --  work until netstack frees a slot.
   rxBlocked   : Boolean := False;

   function rxSpace return Boolean is
      consumed : Unsigned_32 with Volatile, Import,
        Address => grantBase + Storage_Offset (RX_CONSUMER_OFFSET);
   begin
      return Frame_Ring.Has_Room (rxProduced, consumed, Natural (rxRingSlots));
   end rxSpace;

   --  Publish the frames written so far and wake netstack if it is idle.
   procedure publishRX is
      produced : Unsigned_32 with Volatile, Import, Address => grantBase;
      doorbell : Unsigned_32 with Volatile, Import,
        Address => grantBase + Storage_Offset (RX_DOORBELL_OFFSET);
      epoch : Unsigned_32;
      rxMsg : constant Message :=
        (tag      => (label  => OP_NET_RX,
                      length => 0,
                      flags  => 1,      --  a doorbell: no reply
                      reserved  => 0),
         authorityTag => 0,
         words    => (others => 0));
      ignore : Boolean;
   begin
      --  The frames were written before the count that hands them over.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      produced := rxProduced;
      --  The count must be visible before the flag is read.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      epoch := doorbell;
      if epoch /= 0 and then epoch /= rxRungEpoch then
         rxRungEpoch := epoch;
         ignore := capSubmit (CAP_SLOT_NETSTACK, rxMsg, NO_COMPLETION_TOKEN);
      end if;
   end publishRX;

   --  Room for one more frame; if the ring is full, ask netstack to say
   --  when it has taken some (and look once more after asking).
   function rxRoom return Boolean is
      consumed : Unsigned_32 with Volatile, Import,
        Address => grantBase + Storage_Offset (RX_CONSUMER_OFFSET);
      wanted : Unsigned_32 with Volatile, Import,
        Address => grantBase + Storage_Offset (RX_SPACE_WANTED_OFFSET);
   begin
      if Frame_Ring.Has_Room (rxProduced, consumed, Natural (rxRingSlots)) then
         if wanted /= 0 then
            wanted := 0;
         end if;
         return True;
      end if;
      wanted := 1;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      if Frame_Ring.Has_Room (rxProduced, consumed, Natural (rxRingSlots)) then
         wanted := 0;
         return True;
      end if;
      return False;
   end rxRoom;

   --  The device returned receive buffer Raw: take it back if it is one it
   --  holds (True), and post a free buffer in its place.
   function takeBackRX (Raw : Unsigned_32) return Boolean is
      d  : RX_Buffers.Id;
      ok : Boolean;
   begin
      RX_Buffers.Give_Back (rxPool, Raw, ok);
      if ok then
         RX_Buffers.Take (rxPool, d, ok);
         rxAvail.ring (Virtqueue_Index.Slot (Virtqueue_Index.Index (rxAvail.idx))) :=
           Unsigned_16 (d);
         rxAvail.idx := rxAvail.idx + 1;
         return True;
      end if;
      return False;
   end takeBackRX;

   procedure processRX is
      usedIdx : Unsigned_16;
      descIdx : Unsigned_32;
      pktLen  : Unsigned_32;
      pktBuf  : System.Address;
      ethLen  : Natural;
      put     : Boolean := False;
   begin
      usedIdx := rxUsed.idx;
      declare
         use Virtqueue_Index;
         returned : constant Natural :=
           New_Entries (Index (lastRXUsedIdx), Index (usedIdx), NUM_RX_BUFS - rxPool.Top);
         attached : constant Boolean :=
           grantBase /= System.Null_Address and then rxRingSlots /= 0;
      begin
         --  A full ring leaves the rest in the device's used ring until
         --  netstack frees slots (it rings our doorbell). Before netstack
         --  attaches, frames are dropped.
         for K in 1 .. returned loop
            exit when attached and then not rxRoom;
            descIdx := rxUsed.ring (Slot (Index (lastRXUsedIdx))).id;
            pktLen  := rxUsed.ring (Slot (Index (lastRXUsedIdx))).len;
            lastRXUsedIdx := lastRXUsedIdx + 1;
            --  Only a buffer the device holds is read, and posted again once
            --  copied; an id that is not one (a device fault) is skipped.
            if descIdx < NUM_RX_BUFS and then attached and then
              rxPool.In_Flight (Natural (descIdx)) and then
              pktLen > Unsigned_32 (VIRTIO_NET_HDR_SIZE + 14) and then
              Natural (pktLen) - VIRTIO_NET_HDR_SIZE <= RX_SLOT_BYTES - RX_SLOT_HEADER
            then
               pktBuf := bufAddr (Natural (descIdx));
               ethLen := Natural (pktLen) - VIRTIO_NET_HDR_SIZE;
               declare
                  slot : constant System.Address :=
                    grantBase + Storage_Offset
                      (Frame_Ring.Slot_Of (rxProduced, Natural (rxRingSlots)) * RX_SLOT_BYTES);
                  src : Frame_Bytes (0 .. ethLen - 1) with
                     Import, Address =>
                        pktBuf + Storage_Offset (VIRTIO_NET_HDR_SIZE);
                  dst : Frame_Bytes (0 .. ethLen - 1) with
                     Import, Address => slot + Storage_Offset (RX_SLOT_HEADER);
                  lenField : Unsigned_32 with Import, Address => slot;
               begin
                  dst := src;
                  lenField := Unsigned_32 (ethLen);
               end;
               rxProduced := rxProduced + 1;
               put := True;
            end if;
            if takeBackRX (descIdx) then
               null;   --  replenished
            end if;
         end loop;
      end;
      rxBlocked := lastRXUsedIdx /= usedIdx;
      if put then
         publishRX;
      end if;
   end processRX;

   ---------------------------------------------------------------------------
   --  attachToNetstack - send OP_NET_ATTACH to netstack, get grant back
   ---------------------------------------------------------------------------
   procedure attachToNetstack is
      macPacked : Unsigned_64 := 0;
   begin
      --  Pack MAC address into a u64 (low 48 bits)
      for i in mac'Range loop
         macPacked := macPacked or
            Shift_Left (Unsigned_64 (mac (i)), i * 8);
      end loop;

      declare
         attachMsg : Message :=
           (tag      => (label  => OP_NET_ATTACH,
                         length => 1,
                         flags  => 0,
                         reserved  => 0),
            authorityTag => 0,
            words    => (0 => macPacked,
                         others => 0));
         replyTag : MessageTag;
      begin
         replyTag := capCall (CAP_SLOT_NETSTACK, attachMsg);

         if replyTag.label = REPLY_OK then
            grantId      := attachMsg.words (0);
            grantBufSize := attachMsg.words (1);
            rxRingSlots := Unsigned_32 (grantBufSize / 2 / RX_SLOT_BYTES) - 1;
            txRingSlots := Unsigned_32 (grantBufSize / 2 / TX_RING_SLOT_BYTES) - 1;

            --  Grant region is mapped at GRANT_REGION_BASE + grantId * slot
            grantBase := To_Address (
               GRANT_REGION_BASE +
               Integer_Address (grantId) * GRANT_SLOT_SIZE);

            debugPrint ("virtio-net: attached to netstack, grant=");
            printDec (Unsigned_32 (grantId));
            debugPrint (" size=");
            printDec (Unsigned_32 (grantBufSize));
            debugPrint ("" & LF);
         else
            debugPrint ("virtio-net: netstack attach failed" & LF);
         end if;
      end;
   end attachToNetstack;

   --  Main variables
   ipcSender  : ProcessID;
   ipcMsg     : Message;
   ipcFound   : Boolean;
   evtMsg     : Message;
   evtFound   : Boolean;
   rxActive   : Boolean;
   devQSz     : Unsigned_16;
   rxPFN      : Unsigned_32;
   txPFN      : Unsigned_32;
   configSender : ProcessID;
   configMessage : Message;
   ignore : Unsigned_64;

   procedure Reject_Configuration is
   begin
      Virtio.resetDevice (ioBase);
      ignore := reply (configSender,
        (tag => (label => 16#F001#, length => 0, flags => 0, reserved => 0),
         authorityTag => 0, words => (others => 0)));
   end Reject_Configuration;
   --  devQSz is used in queue setup debug output
begin
   debugPrint ("virtio-net: starting..." & LF);

   --  Bind startup to the registered device manager's kernel-supplied sender
   --  identity; netstack's TX endpoint cannot impersonate the configurator.
   receive (configSender, configMessage);
   if configSender = NO_PROCESS or else configSender /=
      getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR) or else
      configMessage.tag.label /= CuBit.Virtio_Net_Control.Operation'Enum_Rep
        (CuBit.Virtio_Net_Control.Configure_MSIX) or else
      configMessage.tag.length /= 3 or else
      configMessage.words (0) not in 1 .. 16#FFE0# or else
      configMessage.words (1) not in CuBit.Virtio_Net_Control.Table_Offset or else
      configMessage.words (1) mod 8 /= 0 or else
      configMessage.words (2) /= CuBit.Virtio_Net_Control.Device_Vector
   then
      ignore := reply (configSender,
        (tag => (label => 16#F001#, length => 0, flags => 0, reserved => 0),
         authorityTag => 0, words => (others => 0)));
      return;
   end if;
   ioBase := Unsigned_16 (configMessage.words (0));
   declare
      tableEntry : array (0 .. 3) of Unsigned_32 with Volatile,
        Import, Address => To_Address (Integer_Address
          (CuBit.Virtio_Net_Control.Table_Virtual_Address +
           configMessage.words (1)));
      flushed : Unsigned_32;
   begin
      --  Function remains masked by devmgr. Mask this entry while editing;
      --  the UC readback orders posted MMIO before the configuration reply.
      tableEntry (3) := 1;
      tableEntry (0) := 16#FEE0_0000#; -- physical destination APIC 0
      tableEntry (1) := 0;
      tableEntry (2) := Unsigned_32 (CuBit.Virtio_Net_Control.Device_Vector);
      tableEntry (3) := 0;
      flushed := tableEntry (3);
      if flushed /= 0 then Reject_Configuration; return; end if;
   end;

   debugPrint ("virtio-net: ioBase=0x");
   printHex8 (Unsigned_8 (Shift_Right (ioBase, 8)));
   printHex8 (Unsigned_8 (ioBase and 16#FF#));
   debugPrint ("" & LF);

   --  2. Get DMA physical base address
   DMA_PHYS_BASE := virtToPhys (DMA_BASE);

   if DMA_PHYS_BASE = Unsigned_64'Last then
      Reject_Configuration;
      debugPrint ("virtio-net: DMA virt-to-phys failed." & LF);
      return;
   end if;

   debugPrint ("virtio-net: DMA phys=0x");
   printHex8 (Unsigned_8 (Shift_Right (DMA_PHYS_BASE, 24) and 16#FF#));
   printHex8 (Unsigned_8 (Shift_Right (DMA_PHYS_BASE, 16) and 16#FF#));
   printHex8 (Unsigned_8 (Shift_Right (DMA_PHYS_BASE, 8) and 16#FF#));
   printHex8 (Unsigned_8 (DMA_PHYS_BASE and 16#FF#));
   debugPrint ("" & LF);

   --  3. Initialize device
   Virtio.initDevice (ioBase);
   debugPrint ("virtio-net: device initialized." & LF);

   --  4. MSI-X-enabled legacy MAC address (BAR0+0x18).
   for i in mac'Range loop
      mac (i) := Unsigned_8 (portInp8 (ioBase + Virtio.REG_NET_MAC +
                                        Unsigned_16 (i)) and 16#FF#);
   end loop;

   debugPrint ("virtio-net: MAC=");
   printMAC;
   debugPrint ("" & LF);

   --  5. Set up RX queue (queue 0)
   Virtio.selectQueue (ioBase, RX_QUEUE);
   devQSz := Virtio.getQueueSize (ioBase);

   debugPrint ("virtio-net: RX queue size=");
   printDec (Unsigned_32 (devQSz));
   debugPrint ("" & LF);

   --  Zero-initialize vring area (6 pages: 3 RX + 3 TX)
   declare
      zeroArea : array (0 .. 16#5FFF#) of Unsigned_8 with
         Import, Address => DMA_BASE;
   begin
      for i in zeroArea'Range loop
         zeroArea (i) := 0;
      end loop;
   end;

   setupRXQueue;

   --  Tell device where the RX vring is (physical PFN)
   rxPFN := Unsigned_32 (DMA_PHYS_BASE / 4096);
   Virtio.selectQueue (ioBase, RX_QUEUE);
   Virtio.setQueueAddr (ioBase, rxPFN);
   if not Virtio.setQueueVector (ioBase) then
      Reject_Configuration;
      return;
   end if;

   --  6. Set up TX queue (queue 1)
   Virtio.selectQueue (ioBase, TX_QUEUE);

   declare
      txQSz : Unsigned_16;
   begin
      txQSz := Virtio.getQueueSize (ioBase);
      debugPrint ("virtio-net: TX queue size=");
      printDec (Unsigned_32 (txQSz));
      debugPrint ("" & LF);
   end;

   --  No TX completion interrupts: descriptors are reclaimed on each pass
   --  of the loop (drainTXRing re-enables them if it runs out).
   txAvail.flags := VRING_AVAIL_F_NO_INTERRUPT;
   txAvail.idx   := 0;

   txPFN := Unsigned_32 ((DMA_PHYS_BASE + Unsigned_64 (TX_VRING_OFFSET)) / 4096);
   Virtio.setQueueAddr (ioBase, txPFN);
   if not Virtio.setQueueVector (ioBase) then
      Reject_Configuration;
      return;
   end if;

   debugPrint ("virtio-net: TX PFN=0x");
   printHex8 (Unsigned_8 (Shift_Right (txPFN, 8) and 16#FF#));
   printHex8 (Unsigned_8 (txPFN and 16#FF#));
   debugPrint ("" & LF);

   --  Every transmit descriptor starts free.
   TX_Descriptors.Initialize (txPool);

   ignore := portOutp16 (ioBase + Virtio.REG_CONFIG_MSIX_VECTOR, 0);
   if portInp16 (ioBase + Virtio.REG_CONFIG_MSIX_VECTOR) /= 0 then
      Reject_Configuration;
      return;
   end if;
   Virtio.startDevice (ioBase);
   --  Devmgr may now unmask the function. Any IRQ before our wait is retained
   --  by the kernel notification latch.
   ignore := reply (configSender,
     (tag => (label => REPLY_OK, length => 0, flags => 0, reserved => 0),
      authorityTag => 0, words => (others => 0)));
   debugPrint ("virtio-net: MSI-X RX/TX/config vectors ready" & LF);

   --  7. Notify device that RX buffers are available
   Virtio.notifyQueue (ioBase, RX_QUEUE);

   debugPrint ("virtio-net: queues configured, waiting for netstack." & LF);

   --  8. Attach to netstack service
   attachToNetstack;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      rdyIgnore : MessageTag;
   begin
      rdyIgnore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   --  9. Drain work, then atomically wait on requests OR latched device IRQs.
   --  Interrupts stay enabled in both rings (no suppression/rearm race).
   loop
      ipcFound := False;
      evtFound := False;
      rxActive := False;
      --  Reclaim TX even when no RX arrives, before accepting another send.
      processTXUsed;

      --  Check for service requests from netstack. IRQ-style traffic is
      --  handled through Poll_Event below, so this must stay request-only.
      --  OP_NET_TX is only a doorbell: the frames are in the TX ring.
      Poll_Service_Request (ipcSender, ipcMsg, ipcFound);
      if ipcFound and then ipcMsg.tag.label = OP_NET_TX and then ipcMsg.tag.flags = 0 then
         declare
            replyMsg : constant Message :=
              (tag      => (label  => REPLY_OK,
                            length => 0,
                            flags  => 0,
                            reserved  => 0),
               authorityTag => 0,
               words    => (others => 0));
            ignore : Unsigned_64;
         begin
            ignore := reply (ipcSender, replyMsg);
         end;
      end if;

      --  Every frame netstack has queued, while descriptors last; one kick.
      if drainTXRing then
         ipcFound := True;
      end if;

      --  Drain IRQ events. With MSI-X (configured above; INTx disabled)
      --  nothing needs acknowledging in the device: the ISR register exists
      --  for legacy INTx, and reading it is a costly exit to the hypervisor.
      evtFound := Poll_Event (evtMsg);

      --  Inspect durable ring state, not the number of coalesced IRQs.
      if lastRXUsedIdx /= rxUsed.idx and then
        (not rxBlocked or else rxSpace)
      then
         rxActive := True;
         processRX;
         processTXUsed;
         kickRX;
      end if;

      --  The kernel checks all queues and the IRQ latch under the same lock
      --  used to publish work before enrolling the waiter. Work arriving
      --  between the checks above and this syscall cannot be lost.
      if not ipcFound and not evtFound and not rxActive then
         --  About to wait: publish a new doorbell epoch so netstack wakes
         --  us once, then look at its TX ring once more (it may have
         --  published frames before it could see the epoch).
         if grantBase /= System.Null_Address and then txRingSlots /= 0 then
            declare
               area : constant System.Address :=
                 grantBase + Storage_Offset (grantBufSize / 2);
               epochWord : Unsigned_32 with Volatile, Import,
                 Address => area + Storage_Offset (TX_DOORBELL_OFFSET);
               produced : Unsigned_32 with Volatile, Import, Address => area;
            begin
               txIdleEpoch :=
                 (if txIdleEpoch = Unsigned_32'Last then 1 else txIdleEpoch + 1);
               epochWord := txIdleEpoch;
               --  A lone frame in flight (a request awaiting its reply):
               --  take its completion interrupt, which keeps this CPU
               --  quick to wake for the reply. Bulk sending reclaims
               --  descriptors on the loop's passes instead.
               txAvail.flags :=
                 (if NUM_TX_BUFS - txPool.Top in 1 .. SPARSE_TX_FRAMES then 0
                  else VRING_AVAIL_F_NO_INTERRUPT);
               System.Machine_Code.Asm
                 ("mfence", Clobber => "memory", Volatile => True);
               if produced /= txConsumed then
                  epochWord := 0;
                  rxActive := True;   --  frames arrived: drain, do not wait
               end if;
            end;
         end if;
      end if;
      if not ipcFound and not evtFound and not rxActive then
         declare
            activity : Activity_Result;
         begin
            activity := Wait_For_Activity_Until (Unsigned_64'Last);
            if activity = Unavailable then
               debugPrint ("virtio-net: activity wait unavailable" & LF);
               return;
            end if;
         end;
      end if;
   end loop;

end main;
