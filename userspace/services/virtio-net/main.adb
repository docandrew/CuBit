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
--    0x6000+          : Packet buffers (64 x 2K: 0-31 RX, 32-63 TX)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Virtio;

procedure main is
   use ASCII;

   DMA_BASE      : constant System.Address :=
      To_Address (16#0000_7000_0000_0000#);
   DMA_PHYS_BASE : Unsigned_64;

   --  RX queue = 0, TX queue = 1
   RX_QUEUE : constant Unsigned_16 := 0;
   TX_QUEUE : constant Unsigned_16 := 1;

   --  Buffer layout: 32 RX + 32 TX, each 2KB
   NUM_RX_BUFS  : constant := 32;
   NUM_TX_BUFS  : constant := 32;
   TX_BUF_FIRST : constant := 32;
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
   GRANT_SLOT_SIZE   : constant Integer_Address := 256 * 4096;

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
   txFreeStack : array (0 .. NUM_TX_BUFS - 1) of Natural;
   txFreeTop   : Natural := NUM_TX_BUFS;

   function allocTXDesc return Integer is
   begin
      if txFreeTop = 0 then
         return -1;
      end if;
      txFreeTop := txFreeTop - 1;
      return txFreeStack (txFreeTop);
   end allocTXDesc;

   procedure freeTXDesc (idx : Natural) is
   begin
      if txFreeTop < NUM_TX_BUFS then
         txFreeStack (txFreeTop) := idx;
         txFreeTop := txFreeTop + 1;
      end if;
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
   begin
      for i in 0 .. NUM_RX_BUFS - 1 loop
         rxDescs (i) := (addr  => bufPhys (i),
                         len   => RX_BUF_SIZE,
                         flags => Virtio.VRING_DESC_F_WRITE,
                         next  => 0);

         rxAvail.ring (i) := Unsigned_16 (i);
      end loop;

      rxAvail.flags := 0;
      rxAvail.idx   := Unsigned_16 (NUM_RX_BUFS);
   end setupRXQueue;

   ---------------------------------------------------------------------------
   --  submitTX - send a frame via the TX virtqueue
   ---------------------------------------------------------------------------
   procedure submitTX (frameAddr : System.Address; frameLen : Natural) is
      descIdx : Integer;
      txBuf   : System.Address;
   begin
      descIdx := allocTXDesc;
      if descIdx < 0 then
         debugPrint ("TX: no free descriptors" & LF);
         return;
      end if;

      txBuf := bufAddr (descIdx);

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
         src : array (0 .. frameLen - 1) of Unsigned_8 with
            Import, Address => frameAddr;
         dst : array (0 .. frameLen - 1) of Unsigned_8 with
            Import, Address => txBuf + Storage_Offset (VIRTIO_NET_HDR_SIZE);
      begin
         for i in src'Range loop
            dst (i) := src (i);
         end loop;
      end;

      --  Set up the descriptor
      txDescs (descIdx) :=
         (addr  => bufPhys (descIdx),
          len   => Unsigned_32 (VIRTIO_NET_HDR_SIZE + frameLen),
          flags => 0,
          next  => 0);

      --  Add to available ring
      txAvail.ring (Natural (txAvail.idx mod Virtio.QUEUE_SIZE)) :=
         Unsigned_16 (descIdx);
      txAvail.idx := txAvail.idx + 1;

      --  Kick the device
      Virtio.notifyQueue (ioBase, TX_QUEUE);
   end submitTX;

   ---------------------------------------------------------------------------
   --  processTXUsed - reclaim completed TX descriptors
   ---------------------------------------------------------------------------
   procedure processTXUsed is
      usedIdx : Unsigned_16;
      descIdx : Unsigned_32;
   begin
      usedIdx := txUsed.idx;

      while lastTXUsedIdx /= usedIdx loop
         descIdx := txUsed.ring (Natural (lastTXUsedIdx mod
                                          Virtio.QUEUE_SIZE)).id;
         freeTXDesc (Natural (descIdx));
         lastTXUsedIdx := lastTXUsedIdx + 1;
      end loop;
   end processTXUsed;

   ---------------------------------------------------------------------------
   --  processRX - check used ring, forward packets to netstack via grant
   ---------------------------------------------------------------------------
   procedure processRX is
      usedIdx : Unsigned_16;
      descIdx : Unsigned_32;
      pktLen  : Unsigned_32;
      pktBuf  : System.Address;
      ethLen  : Natural;
   begin
      if grantBase = System.Null_Address then
         --  Not attached yet; drain and discard
         usedIdx := rxUsed.idx;
         while lastRXUsedIdx /= usedIdx loop
            descIdx := rxUsed.ring (Natural (lastRXUsedIdx mod
                                             Virtio.QUEUE_SIZE)).id;
            rxAvail.ring (Natural (rxAvail.idx mod Virtio.QUEUE_SIZE)) :=
               Unsigned_16 (descIdx);
            rxAvail.idx := rxAvail.idx + 1;
            lastRXUsedIdx := lastRXUsedIdx + 1;
         end loop;
         return;
      end if;

      usedIdx := rxUsed.idx;

      while lastRXUsedIdx /= usedIdx loop
         descIdx := rxUsed.ring (Natural (lastRXUsedIdx mod
                                          Virtio.QUEUE_SIZE)).id;
         pktLen  := rxUsed.ring (Natural (lastRXUsedIdx mod
                                          Virtio.QUEUE_SIZE)).len;

         pktBuf := bufAddr (Natural (descIdx));

         --  Forward packet to netstack (skip virtio-net header)
         if pktLen > Unsigned_32 (VIRTIO_NET_HDR_SIZE + 14) then
            ethLen := Natural (pktLen) - VIRTIO_NET_HDR_SIZE;

            --  Clamp to grant RX area size
            if ethLen > Natural (grantBufSize) / 2 then
               ethLen := Natural (grantBufSize) / 2;
            end if;

            --  Copy packet into grant RX area (offset 0)
            declare
               src : array (0 .. ethLen - 1) of Unsigned_8 with
                  Import, Address =>
                     pktBuf + Storage_Offset (VIRTIO_NET_HDR_SIZE);
               dst : array (0 .. ethLen - 1) of Unsigned_8 with
                  Import, Address => grantBase;
            begin
               for i in src'Range loop
                  dst (i) := src (i);
               end loop;
            end;

            --  Send OP_NET_RX to netstack via capCall (blocking)
            declare
               rxMsg : Message :=
                 (tag      => (label  => OP_NET_RX,
                               length => 2,
                               flags  => 0,
                               badge  => 0),
                  capBadge => 0,
                  words    => (0 => 0,  -- offset in grant (RX area)
                               1 => Unsigned_64 (ethLen),
                               others => 0));
               ignore : MessageTag;
            begin
               ignore := capCall (CAP_SLOT_NETSTACK, rxMsg);
            end;
         end if;

         --  Replenish: put this descriptor back in the available ring
         rxAvail.ring (Natural (rxAvail.idx mod Virtio.QUEUE_SIZE)) :=
            Unsigned_16 (descIdx);
         rxAvail.idx := rxAvail.idx + 1;

         lastRXUsedIdx := lastRXUsedIdx + 1;
      end loop;
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
                         badge  => 0),
            capBadge => 0,
            words    => (0 => macPacked,
                         others => 0));
         replyTag : MessageTag;
      begin
         replyTag := capCall (CAP_SLOT_NETSTACK, attachMsg);

         if replyTag.label = REPLY_OK then
            grantId      := attachMsg.words (0);
            grantBufSize := attachMsg.words (1);

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
   isr        : Unsigned_8;
   devQSz     : Unsigned_16;
   rxPFN      : Unsigned_32;
   txPFN      : Unsigned_32;
   --  devQSz is used in queue setup debug output
begin
   debugPrint ("virtio-net: starting..." & LF);

   --  1. Query sysinfo for BAR0 I/O base
   ioBase := Unsigned_16 (getInfo (SYSINFO_NET_IOBASE) and 16#FFFF#);

   if ioBase = 0 then
      debugPrint ("virtio-net: no I/O base from sysinfo, exiting." & LF);
      return;
   end if;

   debugPrint ("virtio-net: ioBase=0x");
   printHex8 (Unsigned_8 (Shift_Right (ioBase, 8)));
   printHex8 (Unsigned_8 (ioBase and 16#FF#));
   debugPrint ("" & LF);

   --  2. Get DMA physical base address
   DMA_PHYS_BASE := virtToPhys (DMA_BASE);

   if DMA_PHYS_BASE = Unsigned_64'Last then
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

   --  4. Read MAC address (6 bytes at BAR0+0x14)
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

   txAvail.flags := 0;
   txAvail.idx   := 0;

   txPFN := Unsigned_32 ((DMA_PHYS_BASE + Unsigned_64 (TX_VRING_OFFSET)) / 4096);
   Virtio.setQueueAddr (ioBase, txPFN);

   debugPrint ("virtio-net: TX PFN=0x");
   printHex8 (Unsigned_8 (Shift_Right (txPFN, 8) and 16#FF#));
   printHex8 (Unsigned_8 (txPFN and 16#FF#));
   debugPrint ("" & LF);

   --  Initialize TX free descriptor stack
   for i in 0 .. NUM_TX_BUFS - 1 loop
      txFreeStack (i) := TX_BUF_FIRST + i;
   end loop;

   --  7. Notify device that RX buffers are available
   Virtio.notifyQueue (ioBase, RX_QUEUE);

   debugPrint ("virtio-net: queues configured, waiting for netstack." & LF);

   --  8. Attach to netstack service
   attachToNetstack;

   --  9. Event loop: poll both IPC and event queues.
   --  IPC messages come from netstack (OP_NET_TX); events come from
   --  IRQs. When both queues are empty we sleep briefly to avoid
   --  busy-spinning.
   loop
      ipcFound := False;
      evtFound := False;

      --  Check for IPC messages from netstack (non-blocking)
      receiveNB (ipcSender, ipcMsg, ipcFound);

      if ipcFound then
         case ipcMsg.tag.label is
            when OP_NET_TX =>
               --  Netstack wants to transmit a frame via grant buffer
               if grantBase /= System.Null_Address then
                  declare
                     offset    : constant Unsigned_64 := ipcMsg.words (0);
                     frameLen  : constant Natural :=
                        Natural (ipcMsg.words (1));
                     frameAddr : constant System.Address :=
                        grantBase + Storage_Offset (offset);
                  begin
                     submitTX (frameAddr, frameLen);
                     --  Only reply if sender expects it (flags=0).
                     --  capSubmit'd TX uses flags=1 (fire-and-forget).
                     if ipcMsg.tag.flags = 0 then
                        declare
                           replyMsg : constant Message :=
                             (tag      => (label  => REPLY_OK,
                                           length => 0,
                                           flags  => 0,
                                           badge  => 0),
                              capBadge => 0,
                              words    => (others => 0));
                           ignore : Unsigned_64;
                        begin
                           ignore := reply (ipcSender, replyMsg);
                        end;
                     end if;
                  end;
               end if;

            when others =>
               null;
         end case;
      end if;

      --  Check for IRQ events (non-blocking)
      evtFound := receiveEventNB (evtMsg);

      if evtFound then
         isr := Virtio.readISR (ioBase);
         if (isr and 1) /= 0 then
            processRX;
            processTXUsed;
            Virtio.notifyQueue (ioBase, RX_QUEUE);
         end if;
      end if;

      --  If nothing happened, sleep 1ms to avoid busy-spinning
      if not ipcFound and not evtFound then
         declare
            ignore : Unsigned_64;
         begin
            ignore := syscall (SYSCALL_SLEEP, 1);
         end;
      end if;
   end loop;

end main;
