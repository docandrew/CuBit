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
with Virtio.Modern;
with Descriptor_Pool;
with CuBit.Frame_Rings;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with Virtqueue_Index;
with CuBit.Virtio_Net_Control;
with CuBit.Busy_Poll;

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

   --  The virtio-net header prepended to every packet: 10 bytes on a
   --  legacy device, 12 on a modern one (VERSION_1 adds num_buffers).
   LEGACY_NET_HDR_SIZE : constant := 10;
   MODERN_NET_HDR_SIZE : constant := 12;
   VIRTIO_NET_HDR_SIZE : Natural := LEGACY_NET_HDR_SIZE;
   --  The device is modern (virtio 1.0): registers in a memory BAR.
   modern : Boolean := False;

   --  vring area offsets within DMA region
   RX_VRING_OFFSET : constant Storage_Offset := 0;

   --  I/O base from sysinfo
   ioBase : Unsigned_16 := 0;

   --  MAC address (raw bytes)
   mac : array (0 .. 5) of Unsigned_8;

   --  Grant region constants (must match kernel process.ads)

   --  Cap slot for netstack endpoint (granted by kernel modules.adb)
   CAP_SLOT_NETSTACK : constant CapabilitySlot := 7;

   --  IPC label constants (must match kernel ipc_labels.ads)
   OP_NET_ATTACH : constant Unsigned_32 := 16#0400#;
   OP_NET_RX     : constant Unsigned_32 := 16#0401#;
   OP_NET_TX     : constant Unsigned_32 := 16#0402#;
   REPLY_OK      : constant Unsigned_32 := 16#F000#;

   --  Netstack connection state
   packetGrant    : CuBit.Grant_References.Reference;
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

   --  VIRTIO_F_EVENT_IDX (virtio 1.x 2.7.10): each side names the index at
   --  which it wants to hear from the other. Ours (used_event) follows our
   --  avail ring; the device's (avail_event) follows its used ring.
   eventIdx : Boolean := False;
   USED_EVENT_AT  : constant Storage_Offset := 4 + 2 * Virtio.QUEUE_SIZE;
   AVAIL_EVENT_AT : constant Storage_Offset := 4 + 8 * Virtio.QUEUE_SIZE;
   rxUsedEvent : Unsigned_16 with Volatile, Import,
     Address => DMA_BASE + RX_VRING_OFFSET + 16#1000# + USED_EVENT_AT;
   rxAvailEvent : Unsigned_16 with Volatile, Import,
     Address => DMA_BASE + RX_VRING_OFFSET + 16#2000# + AVAIL_EVENT_AT;
   txUsedEvent : Unsigned_16 with Volatile, Import,
     Address => DMA_BASE + TX_VRING_OFFSET + 16#1000# + USED_EVENT_AT;
   txAvailEvent : Unsigned_16 with Volatile, Import,
     Address => DMA_BASE + TX_VRING_OFFSET + 16#2000# + AVAIL_EVENT_AT;
   --  Our avail indices when we last decided whether to notify.
   rxKickedIdx, txKickedIdx : Unsigned_16 := 0;
   --  With EVENT_IDX, "no interrupt" is an event index half the index
   --  space ahead: the device will not reach it.
   FAR_AHEAD : constant Unsigned_16 := 16#8000#;

   --  Whether the device should interrupt on transmit completions.
   procedure setTXInterrupts (wanted : Boolean) is
      VRING_AVAIL_F_NO_INTERRUPT : constant Unsigned_16 := 1;
   begin
      if eventIdx then
         txUsedEvent := (if wanted then lastTXUsedIdx else lastTXUsedIdx + FAR_AHEAD);
      else
         txAvail.flags := (if wanted then 0 else VRING_AVAIL_F_NO_INTERRUPT);
      end if;
   end setTXInterrupts;

   --  A kick is due for a ring whose avail index moved from old to now.
   function kickDue (availEvent, now, old : Unsigned_16; usedFlags : Unsigned_16)
     return Boolean
   is
      VRING_USED_F_NO_NOTIFY : constant Unsigned_16 := 1;
      use Virtqueue_Index;
   begin
      if eventIdx then
         return Needs_Event (Index (availEvent), Index (now), Index (old));
      end if;
      return (usedFlags and VRING_USED_F_NO_NOTIFY) = 0;
   end kickDue;

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
   procedure notifyQueue (queue : Unsigned_16) is
   begin
      if modern then
         Virtio.Modern.Notify (queue);
      else
         Virtio.notifyQueue (ioBase, queue);
      end if;
   end notifyQueue;

   txPending : Boolean := False;
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
      now : constant Unsigned_16 := txAvail.idx;
   begin
      if txPending then
         txPending := False;
         --  The avail index must be visible before the device's event
         --  index (or flags) is read.
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         if kickDue (txAvailEvent, now, txKickedIdx, usedFlags) then
            notifyQueue (TX_QUEUE);
         end if;
         txKickedIdx := now;
      end if;
   end kickTX;

   ---------------------------------------------------------------------------
   --  kickRX - after replenishing receive buffers, notify the device only
   --  if it asked to be (it had run out: VRING_USED_F_NO_NOTIFY clear).
   ---------------------------------------------------------------------------
   procedure kickRX is
      usedFlags : Unsigned_16 with Volatile, Import, Address => rxUsed'Address;
      now : constant Unsigned_16 := rxAvail.idx;
   begin
      --  The replenished avail index must be visible before the device's
      --  event index (or flags) is read.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      if now /= rxKickedIdx and then kickDue (rxAvailEvent, now, rxKickedIdx, usedFlags) then
         notifyQueue (RX_QUEUE);
      end if;
      rxKickedIdx := now;
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
   --  The packet grant netstack lent us (layout in CuBit.Frame_Rings): we
   --  consume its transmit ring and produce into its receive ring.
   ---------------------------------------------------------------------------
   package Frames renames CuBit.Frame_Rings;
   package Frame_Ring renames CuBit.Frame_Rings.Rings;
   use type Frame_Ring.Index;
   attached : Boolean := False;   --  the grant is mapped and its size checked
   txRing   : Frame_Ring.Consumer;
   rxRing   : Frame_Ring.Producer;
   --  Our wake epoch in the transmit header (netstack reads it): a new
   --  nonzero value each time we are about to wait.
   txIdleEpoch : Unsigned_32 := 0;

   function txWord (Offset : Natural) return System.Address is
     (grantBase + Storage_Offset (Frames.Transmit_Header_At + Offset));
   function rxWord (Offset : Natural) return System.Address is
     (grantBase + Storage_Offset (Frames.Receive_Header_At + Offset));

   --  netstack has queued frames in the transmit ring.
   function txRingPending return Boolean is
   begin
      if not attached then
         return False;
      end if;
      declare
         produced : Unsigned_32 with Volatile, Import,
           Address => txWord (Frames.Produced_At);
      begin
         return Frame_Ring.Index (produced) /= txRing.Consumed;
      end;
   end txRingPending;

   --  The last time the driver had work (TSC).
   lastActivity : Unsigned_64 := 0;

   --  Copy queued frames to the device; True if any were taken.
   function drainTXRing return Boolean is
      took : Boolean := False;
      ok   : Boolean;
   begin
      if not attached then
         return False;
      end if;
      declare
         produced : Unsigned_32 with Volatile, Import,
           Address => txWord (Frames.Produced_At);
         consumed : Unsigned_32 with Volatile, Import,
           Address => txWord (Frames.Consumed_At);
      begin
         --  netstack's count, read once; one that goes back or is more
         --  than a ring ahead is not ours to trust.
         Frame_Ring.Accept_Produced (txRing, Frame_Ring.Index (produced), ok);
         if not ok then
            return False;
         end if;
         --  The frames were written before the count was: read them after.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         while txRing.Available > 0 loop
            processTXUsed;
            if txPool.Top = 0 then
               --  Out of descriptors with frames waiting: let the device's
               --  completion wake us to reclaim them.
               setTXInterrupts (True);
               System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
               processTXUsed;
               exit when txPool.Top = 0;
            end if;
            setTXInterrupts (False);
            declare
               slot : constant System.Address :=
                 grantBase + Storage_Offset
                   (Frames.Transmit_Slot_At (Frame_Ring.Head_Slot (txRing)));
               len : Unsigned_32 with Volatile, Import,
                 Address => slot + Storage_Offset (Frames.Length_At);
               frameLen : constant Unsigned_32 := len;   --  read once
            begin
               if Frames.Fits (frameLen) then
                  submitTX (slot + Storage_Offset (Frames.Frame_At), Natural (frameLen));
               end if;
            end;
            Frame_Ring.Release (txRing);
            took := True;
         end loop;
         if took then
            --  Copied out before the slots are handed back.
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            consumed := Unsigned_32 (txRing.Consumed);
            kickTX;
         end if;
      end;
      return took;
   end drainTXRing;

   ---------------------------------------------------------------------------
   --  processRX - check used ring, forward packets to netstack via grant
   ---------------------------------------------------------------------------
   --  Received frames go to netstack through the grant's receive ring
   --  (netstack's drainRXRing reads it; layout in CuBit.Frame_Rings).
   --  - produced (ours) and consumed (netstack's) are free-running counts;
   --  - space wanted (ours): the ring was full; netstack rings our doorbell
   --    (OP_NET_TX) once it frees slots;
   --  - wake (netstack's): a nonzero epoch while netstack is idle; one
   --    OP_NET_RX (one-way) per epoch wakes it.
   --  No call and no reply per batch: we never wait for netstack.
   type Frame_Bytes is array (Natural range <>) of Unsigned_8;
   rxRungEpoch : Unsigned_32 := 0;   --  the netstack epoch we last woke
   --  processRX stopped at a full ring: the used entries left are not new
   --  work until netstack frees a slot.
   rxBlocked   : Boolean := False;

   --  Take netstack's consumed count if it is sane (a bad one frees
   --  nothing); True if a frame fits.
   function rxSpace return Boolean is
      consumed : Unsigned_32 with Volatile, Import,
        Address => rxWord (Frames.Consumed_At);
      ignore : Boolean;
   begin
      Frame_Ring.Accept_Consumed (rxRing, Frame_Ring.Index (consumed), ignore);
      return Frame_Ring.Space (rxRing) > 0;
   end rxSpace;

   --  Publish the frames written so far and wake netstack if it is idle.
   procedure publishRX is
      produced : Unsigned_32 with Volatile, Import,
        Address => rxWord (Frames.Produced_At);
      doorbell : Unsigned_32 with Volatile, Import,
        Address => rxWord (Frames.Wake_At);
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
      produced := Unsigned_32 (rxRing.Produced);
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
      wanted : Unsigned_32 with Volatile, Import,
        Address => rxWord (Frames.Space_Wanted_At);
   begin
      if rxSpace then
         if wanted /= 0 then
            wanted := 0;
         end if;
         return True;
      end if;
      wanted := 1;
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      if rxSpace then
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
              Natural (pktLen) - VIRTIO_NET_HDR_SIZE <= Frames.Maximum_Frame
            then
               pktBuf := bufAddr (Natural (descIdx));
               ethLen := Natural (pktLen) - VIRTIO_NET_HDR_SIZE;
               declare
                  slot : constant System.Address :=
                    grantBase + Storage_Offset
                      (Frames.Receive_Slot_At (Frame_Ring.Next_Slot (rxRing)));
                  src : Frame_Bytes (0 .. ethLen - 1) with
                     Import, Address =>
                        pktBuf + Storage_Offset (VIRTIO_NET_HDR_SIZE);
                  dst : Frame_Bytes (0 .. ethLen - 1) with
                     Import, Address => slot + Storage_Offset (Frames.Frame_At);
                  lenField : Unsigned_32 with Import,
                    Address => slot + Storage_Offset (Frames.Length_At);
               begin
                  dst := src;
                  lenField := Unsigned_32 (ethLen);
               end;
               Frame_Ring.Commit (rxRing);   --  rxRoom found space
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
         replyTag := capCall (CAP_SLOT_NETSTACK, attachMsg, CuBit.Messages.Wait_Forever);

         if replyTag.label = REPLY_OK then
            grantBufSize := attachMsg.words (1);
            --  Only a grant laid out as CuBit.Frame_Rings says is used, and
            --  only once the kernel confirms netstack owns it; the kernel
            --  says where it is mapped.
            attached := grantBufSize = Frames.Grant_Bytes and then
                        CuBit.Grant_References.Valid_Wire (attachMsg.words (0));
            if attached then
               packetGrant := CuBit.Grant_References.Decode (attachMsg.words (0));
               CuBit.Memory_Grants.Acquire_Via_Capability
                 (CAP_SLOT_NETSTACK, packetGrant, 0, grantBufSize,
                  CuBit.Memory_Grants.Write_Access, grantBase, attached);
            end if;
            if not attached then
               debugPrint ("virtio-net: packet grant refused" & LF);
            end if;

            debugPrint ("virtio-net: attached to netstack, grant=");
            printDec (Unsigned_32 (packetGrant.slot));
            debugPrint (" size=");
            printDec (Unsigned_32 (grantBufSize));
            debugPrint ("" & LF);
         else
            debugPrint ("virtio-net: netstack attach failed" & LF);
         end if;
      end;
   end attachToNetstack;

   --  Main variables
   ipcSender  : Process_ID;
   ipcMsg     : Message;
   ipcFound   : Boolean;
   evtMsg     : Message;
   evtFound   : Boolean;
   rxActive   : Boolean;
   devQSz     : Unsigned_16;
   rxPFN      : Unsigned_32;
   txPFN      : Unsigned_32;
   configSender : Process_ID;
   configMessage : Message;
   ignore : Unsigned_64;

   procedure Reject_Configuration is
   begin
      if modern then
         Virtio.Modern.Reset;
      elsif ioBase /= 0 then
         Virtio.resetDevice (ioBase);
      end if;
      ignore := reply (configSender,
        (tag => (label => 16#F001#, length => 0, flags => 0, reserved => 0),
         authorityTag => 0, words => (others => 0)));
   end Reject_Configuration;
   --  devQSz is used in queue setup debug output
begin
   debugPrint ("virtio-net: starting..." & LF);
   CuBit.Busy_Poll.Calibrate;

   --  Bind startup to the registered device manager's kernel-supplied sender
   --  identity; netstack's TX endpoint cannot impersonate the configurator.
   receive (configSender, configMessage);
   declare
      use CuBit.Virtio_Net_Control;
      tableOffset : Unsigned_64 := 0;
      valid : Boolean := configSender /= No_Process and then
        configSender = Registered_Driver (DRIVER_DEVMGR);
   begin
      if valid and then configMessage.tag.label = Operation'Enum_Rep (Configure_MSIX) then
         valid := configMessage.tag.length = 3 and then
           configMessage.words (0) in 1 .. 16#FFE0# and then
           configMessage.words (2) = Device_Vector;
         tableOffset := configMessage.words (1);
         if valid then
            ioBase := Unsigned_16 (configMessage.words (0));
         end if;
      elsif valid and then configMessage.tag.label = Operation'Enum_Rep (Configure_Modern) then
         valid := configMessage.tag.length = 4 and then
           configMessage.words (3) = Device_Vector;
         tableOffset := Low (configMessage.words (2));
         if valid then
            Virtio.Modern.Bind
              (Common     => Low (configMessage.words (0)),
               Device     => High (configMessage.words (0)),
               Notify     => Low (configMessage.words (1)),
               Multiplier => High (configMessage.words (1)),
               Mapped     => High (configMessage.words (2)),
               OK         => valid);
            modern := valid;
         end if;
      else
         valid := False;
      end if;
      if not valid or else tableOffset not in Table_Offset or else tableOffset mod 8 /= 0 then
         modern := False;
         ignore := reply (configSender,
           (tag => (label => 16#F001#, length => 0, flags => 0, reserved => 0),
            authorityTag => 0, words => (others => 0)));
         return;
      end if;
      declare
         tableEntry : array (0 .. 3) of Unsigned_32 with Volatile,
           Import, Address => To_Address (Integer_Address
             (Table_Virtual_Address + tableOffset));
         flushed : Unsigned_32;
      begin
         --  Function remains masked by devmgr. Mask this entry while editing;
         --  the UC readback orders posted MMIO before the configuration reply.
         tableEntry (3) := 1;
         tableEntry (0) := 16#FEE0_0000#; -- physical destination APIC 0
         tableEntry (1) := 0;
         tableEntry (2) := Unsigned_32 (Device_Vector);
         tableEntry (3) := 0;
         flushed := tableEntry (3);
         if flushed /= 0 then Reject_Configuration; return; end if;
      end;
   end;
   if modern then
      VIRTIO_NET_HDR_SIZE := MODERN_NET_HDR_SIZE;
      debugPrint ("virtio-net: modern (virtio 1.0) transport" & LF);
   end if;

   if not modern then
      debugPrint ("virtio-net: ioBase=0x");
      printHex8 (Unsigned_8 (Shift_Right (ioBase, 8)));
      printHex8 (Unsigned_8 (ioBase and 16#FF#));
      debugPrint ("" & LF);
   end if;

   --  2. Get DMA physical base address
   DMA_PHYS_BASE := virtToPhys (DMA_BASE);

   if DMA_PHYS_BASE = Unsigned_64'Last then
      Reject_Configuration;
      debugPrint ("virtio-net: DMA virt-to-phys failed." & LF);
      return;
   end if;

   --  3. Initialize the device and read its MAC address.
   if modern then
      declare
         agreed : Unsigned_64;
         ok     : Boolean;
      begin
         Virtio.Modern.Negotiate
           (Virtio.Modern.F_VERSION_1 or Virtio.Modern.F_NET_MAC or
            Virtio.Modern.F_EVENT_IDX, agreed, ok);
         eventIdx := (agreed and Virtio.Modern.F_EVENT_IDX) /= 0;
         if not ok then
            debugPrint ("virtio-net: feature negotiation failed" & LF);
            Reject_Configuration;
            return;
         end if;
      end;
      for i in mac'Range loop
         mac (i) := Virtio.Modern.Device_Byte (i);
      end loop;
   else
      Virtio.initDevice (ioBase);
      --  MSI-X-enabled legacy MAC address (BAR0+0x18).
      for i in mac'Range loop
         mac (i) := Unsigned_8 (portInp8 (ioBase + Virtio.REG_NET_MAC +
                                           Unsigned_16 (i)) and 16#FF#);
      end loop;
   end if;
   debugPrint ("virtio-net: MAC=");
   printMAC;
   debugPrint ("" & LF);

   --  4. The rings: zeroed (6 pages: 3 RX + 3 TX), every receive buffer
   --  posted, no transmit completion interrupts (descriptors are
   --  reclaimed on each pass of the loop; drainTXRing re-enables them if
   --  it runs out).
   declare
      zeroArea : array (0 .. 16#5FFF#) of Unsigned_8 with
         Import, Address => DMA_BASE;
   begin
      for i in zeroArea'Range loop
         zeroArea (i) := 0;
      end loop;
   end;
   setupRXQueue;
   txAvail.idx   := 0;
   if eventIdx then
      txAvail.flags := 0;
      rxUsedEvent := 0;   --  interrupt for the first frame
   end if;
   setTXInterrupts (False);
   rxKickedIdx := rxAvail.idx;
   TX_Descriptors.Initialize (txPool);

   --  5. Tell the device where they are; every vector is MSI-X entry 0.
   if modern then
      declare
         RING_AVAIL : constant := 16#1000#;
         RING_USED  : constant := 16#2000#;
         rxPhys : constant Unsigned_64 := DMA_PHYS_BASE + Unsigned_64 (RX_VRING_OFFSET);
         txPhys : constant Unsigned_64 := DMA_PHYS_BASE + Unsigned_64 (TX_VRING_OFFSET);
         ok, ok2, ok3 : Boolean;
      begin
         Virtio.Modern.Setup_Queue
           (RX_QUEUE, Virtio.QUEUE_SIZE, rxPhys, rxPhys + RING_AVAIL, rxPhys + RING_USED, 0, ok);
         Virtio.Modern.Setup_Queue
           (TX_QUEUE, Virtio.QUEUE_SIZE, txPhys, txPhys + RING_AVAIL, txPhys + RING_USED, 0, ok2);
         Virtio.Modern.Set_Config_Vector (0, ok3);
         if not (ok and then ok2 and then ok3) then
            debugPrint ("virtio-net: queue setup failed" & LF);
            Reject_Configuration;
            return;
         end if;
      end;
      Virtio.Modern.Start;
   else
      Virtio.selectQueue (ioBase, RX_QUEUE);
      devQSz := Virtio.getQueueSize (ioBase);
      if Natural (devQSz) /= Virtio.QUEUE_SIZE then
         debugPrint ("virtio-net: unexpected legacy queue size" & LF);
         Reject_Configuration;
         return;
      end if;
      rxPFN := Unsigned_32 (DMA_PHYS_BASE / 4096);
      Virtio.setQueueAddr (ioBase, rxPFN);
      if not Virtio.setQueueVector (ioBase) then
         Reject_Configuration;
         return;
      end if;
      Virtio.selectQueue (ioBase, TX_QUEUE);
      txPFN := Unsigned_32 ((DMA_PHYS_BASE + Unsigned_64 (TX_VRING_OFFSET)) / 4096);
      Virtio.setQueueAddr (ioBase, txPFN);
      if not Virtio.setQueueVector (ioBase) then
         Reject_Configuration;
         return;
      end if;
      ignore := portOutp16 (ioBase + Virtio.REG_CONFIG_MSIX_VECTOR, 0);
      if portInp16 (ioBase + Virtio.REG_CONFIG_MSIX_VECTOR) /= 0 then
         Reject_Configuration;
         return;
      end if;
      Virtio.startDevice (ioBase);
   end if;
   --  Devmgr may now unmask the function. Any IRQ before our wait is retained
   --  by the kernel notification latch.
   ignore := reply (configSender,
     (tag => (label => REPLY_OK, length => 0, flags => 0, reserved => 0),
      authorityTag => 0, words => (others => 0)));
   debugPrint ("virtio-net: MSI-X RX/TX/config vectors ready" & LF);

   --  7. Notify device that RX buffers are available
   notifyQueue (RX_QUEUE);

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
          words    => (others => 0)), CuBit.Messages.Wait_Forever);
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
      --  While traffic flows, poll the device's receive ring and
      --  netstack's transmit ring for a short window before arming the
      --  doorbell and interrupts to sleep (CuBit.Busy_Poll).
      if ipcFound or else evtFound or else rxActive then
         lastActivity := CuBit.Busy_Poll.Now;
      else
         while CuBit.Busy_Poll.Within
           (lastActivity, CuBit.Busy_Poll.Default_Window_Microseconds)
         loop
            if (lastRXUsedIdx /= rxUsed.idx and then (not rxBlocked or else rxSpace))
              or else txRingPending
            then
               rxActive := True;
               lastActivity := CuBit.Busy_Poll.Now;
               exit;
            end if;
            CuBit.Busy_Poll.Relax;
         end loop;
      end if;

      if not ipcFound and not evtFound and not rxActive then
         --  About to wait: publish a new doorbell epoch so netstack wakes
         --  us once, then look at its TX ring once more (it may have
         --  published frames before it could see the epoch).
         if attached then
            declare
               epochWord : Unsigned_32 with Volatile, Import,
                 Address => txWord (Frames.Wake_At);
               produced : Unsigned_32 with Volatile, Import,
                 Address => txWord (Frames.Produced_At);
            begin
               txIdleEpoch :=
                 (if txIdleEpoch = Unsigned_32'Last then 1 else txIdleEpoch + 1);
               epochWord := txIdleEpoch;
               --  A lone frame in flight (a request awaiting its reply):
               --  take its completion interrupt, which keeps this CPU
               --  quick to wake for the reply. Bulk sending reclaims
               --  descriptors on the loop's passes instead.
               setTXInterrupts (NUM_TX_BUFS - txPool.Top in 1 .. SPARSE_TX_FRAMES);
               --  Receive interrupts, off while we poll, are armed for the
               --  next frame only now that we are about to wait.
               if eventIdx then
                  rxUsedEvent := lastRXUsedIdx;
               end if;
               System.Machine_Code.Asm
                 ("mfence", Clobber => "memory", Volatile => True);
               if Frame_Ring.Index (produced) /= txRing.Consumed or else
                 (eventIdx and then rxUsed.idx /= lastRXUsedIdx)
               then
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
