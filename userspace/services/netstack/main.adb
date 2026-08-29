------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace network stack service (netstack.svc).
--
--  Owns all protocol processing (ARP, IPv4, ICMP). Communicates with the
--  virtio-net driver via IPC and a shared memory grant for packet data.
--
--  Grant layout:
--    The netstack allocates a packet buffer via sbrk and grants it to the
--    driver.  Both sides use offsets within this grant for RX/TX packets.
--    Offset 0 is reserved for RX (driver writes, netstack reads).
--    Offset PACKET_BUF_SIZE/2 is reserved for TX (netstack writes, driver
--    reads).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with Net;
with Net.RFLX_Builtin_Types;
with Net.RFLX_Types;
with Net.UDP.Datagram;
with Net.TCP.Segment;
with TCPSession;

procedure main is
   use ASCII;
   use type Net.IPv4Address;
   use type TCPSession.TCPState;
   use type TCPSession.ActionKind;

   --  Well-known capability slots (granted by kernel modules.adb)
   CAP_SLOT_NET_DRV : constant CapabilitySlot := 10;

   --  Shared packet buffer layout
   PACKET_BUF_PAGES : constant := 16;     -- 64 KB total
   PACKET_BUF_SIZE  : constant := PACKET_BUF_PAGES * 4096;
   TX_AREA_OFFSET   : constant := PACKET_BUF_SIZE / 2;    -- TX half
   TX_SLOT_SIZE     : constant := 2048;                   -- per-slot size
   NUM_TX_SLOTS     : constant := 8;                      -- rotating slots

   --  Grant region mapping (must match kernel process.ads)
   GRANT_REGION_BASE : constant Integer_Address := 16#0000_4000_0000_0000#;
   GRANT_SLOT_SIZE   : constant Integer_Address := 4096 * 4096;

   --  Multi-interface support
   MAX_INTERFACES : constant := 4;
   type InterfaceState is (IF_DOWN, IF_UP, IF_CONFIGURING);

   type InterfaceRecord is record
      state     : InterfaceState := IF_DOWN;
      mac       : Net.MACAddress := (others => 0);
      ipv4      : Net.IPv4Address := (others => 0);
      netmask   : Net.IPv4Address := (others => 0);
      gateway   : Net.IPv4Address := (others => 0);
      driverPID : ProcessID := NO_PROCESS;
      arpCache  : Net.ARPTable := (others =>
         (ip => (others => 0), mac => (others => 0), valid => False));
      gwMAC     : Net.MACAddress := Net.ZERO_MAC;
      grantId   : Unsigned_64 := 0;
      txSlotIdx : Natural := 0;
      pktBuf    : System.Address := System.Null_Address;
      pktGrant  : Unsigned_64 := 0;
   end record;

   interfaces : array (0 .. MAX_INTERFACES - 1) of InterfaceRecord;
   numIfaces  : Natural := 0;

   --  Global DNS (not per-interface)
   primaryDNS   : Net.IPv4Address := (others => 0);
   secondaryDNS : Net.IPv4Address := (others => 0);

   --  Convenience aliases for interface 0 (used during transition)
   --  These are procedures/functions that access interfaces(0) directly.
   --  TODO: thread ifIdx through all packet handlers for full multi-if.

   --  Self-test state (removed: netmgr handles config now)
   resolvedIP   : Net.IPv4Address := (others => 0);

   --  TCP connection table (types in TCPSession package)
   tcpConns : TCPSession.ConnTable;
   RX_BUFFER_SIZE : constant := 65_536;
   type RX_Data is array (0 .. RX_BUFFER_SIZE - 1) of Unsigned_8;
   type RX_Buffer is record
      data : RX_Data;
      len  : Natural range 0 .. RX_BUFFER_SIZE := 0;
   end record;
   rxBuffers : array (tcpConns'Range) of RX_Buffer;
   tcpISN   : Unsigned_32 := 16#CB17_0000#;
   nextEphemeralPort : Unsigned_16 := 49152;  -- incrementing ephemeral port

   --  Legacy aliases to interface 0 (for incremental refactoring)
   --  New code should use interfaces(ifIdx).xxx directly.

   --  IPC label constants (must match kernel ipc_labels.ads)
   OP_NET_ATTACH  : constant Unsigned_32 := 16#0400#;
   OP_NET_RX      : constant Unsigned_32 := 16#0401#;
   OP_NET_TX      : constant Unsigned_32 := 16#0402#;
   OP_NET_RESOLVE : constant Unsigned_32 := 16#0410#;
   OP_NET_CONNECT : constant Unsigned_32 := 16#0411#;
   OP_NET_SEND    : constant Unsigned_32 := 16#0412#;
   OP_NET_RECV    : constant Unsigned_32 := 16#0413#;
   OP_NET_CLOSE   : constant Unsigned_32 := 16#0414#;
   OP_NET_OPEN    : constant Unsigned_32 := 16#0420#;
   OP_NET_WRITE   : constant Unsigned_32 := 16#0421#;
   OP_NET_READ    : constant Unsigned_32 := 16#0422#;
   OP_NET_SHUT    : constant Unsigned_32 := 16#0423#;
   OP_NET_OPEN_RAW : constant Unsigned_32 := 16#0426#;

   --  Network management IPC labels (from netmgr)
   OP_NET_CONFIGURE : constant Unsigned_32 := 16#0430#;
   OP_NET_SET_DNS   : constant Unsigned_32 := 16#0432#;
   OP_NET_ROUTE_ADD : constant Unsigned_32 := 16#0433#;
   OP_NET_ROUTE_DEL : constant Unsigned_32 := 16#0434#;
   OP_NET_LIST_IF    : constant Unsigned_32 := 16#0435#;
   OP_NET_IF_DETAIL  : constant Unsigned_32 := 16#0436#;
   OP_NET_ROUTE_LIST : constant Unsigned_32 := 16#0437#;
   OP_NET_PING       : constant Unsigned_32 := 16#0438#;

   REPLY_OK       : constant Unsigned_32 := 16#F000#;
   REPLY_ERR      : constant Unsigned_32 := 16#F001#;
   REPLY_EOF      : constant Unsigned_32 := 16#F006#;

   --  Routing table
   MAX_ROUTES : constant := 16;
   type RouteEntry is record
      active  : Boolean := False;
      dest    : Net.IPv4Address := (others => 0);
      prefix  : Natural := 0;
      gateway : Net.IPv4Address := (others => 0);
      ifIdx   : Natural := 0;
      metric  : Natural := 0;
   end record;
   routeTable : array (0 .. MAX_ROUTES - 1) of RouteEntry;

   --  Deferred TX queue: during OP_NET_RX processing we can't capCall to
   --  the driver (it's blocked waiting for our reply). Buffer frames here
   --  and flush between message receives.
   MAX_DEFERRED_TX  : constant := 4;
   MAX_DEFERRED_LEN : constant := 1500;  -- max Ethernet frame

   type FrameData is array (0 .. MAX_DEFERRED_LEN - 1) of Unsigned_8;

   type DeferredFrame is record
      data : FrameData;
      len  : Natural := 0;
   end record;

   deferredTX    : array (0 .. MAX_DEFERRED_TX - 1) of DeferredFrame;
   deferredCount : Natural := 0;

   --  Channel table (tracks app connections via new channel API + legacy)
   type ChannelKind is (CHANNEL_NONE, CHANNEL_CLIENT,
                        CHANNEL_SERVER, CHANNEL_RAW, CHANNEL_SNIFF);

   type NetChannel is record
      kind       : ChannelKind := CHANNEL_NONE;
      proto      : Unsigned_8 := 0;
      pid        : ProcessID := NO_PROCESS;
      bufAddr    : System.Address := System.Null_Address;
      grantId    : Unsigned_64 := 0;
      bufSize    : Natural := 0;
      connIdx    : Integer := -1;
      remoteIP   : Net.IPv4Address := (others => 0);
      remotePort : Unsigned_16 := 0;
      localPort  : Unsigned_16 := 0;
   end record;

   MAX_NET_CHANNELS : constant := 8;
   channels : array (0 .. MAX_NET_CHANNELS - 1) of NetChannel;

   --  Legacy app channel table (for old socket-style API compatibility)
   MAX_APP_CHANNELS : constant := 4;
   type AppChannel is record
      pid     : ProcessID := NO_PROCESS;
      bufAddr : System.Address := System.Null_Address;
      grantId : Unsigned_64 := 0;
      bufSize : Natural := 0;
   end record;
   appChannels : array (0 .. MAX_APP_CHANNELS - 1) of AppChannel;

   --  Pending request queue (deferred reply for blocking ops)
   type PendingKind is (PENDING_NONE, PENDING_RESOLVE,
                        PENDING_CONNECT, PENDING_RECV,
                        PENDING_OPEN, PENDING_PING);
   type PendingRequest is record
      kind       : PendingKind := PENDING_NONE;
      sender     : ProcessID := NO_PROCESS;
      connIdx    : Integer := -1;
      channelIdx : Integer := -1;
      bufAddr    : System.Address := System.Null_Address;
      bufOff     : Natural := 0;
      maxLen     : Natural := 0;
      txid       : Unsigned_16 := 0;
      dstPort    : Unsigned_16 := 0;
      replySlot  : Unsigned_64 := 0;
   end record;

   MAX_PENDING : constant := 8;
   pendingReqs : array (0 .. MAX_PENDING - 1) of PendingRequest;
   nextDnsTxid : Unsigned_16 := 16#CB20#;

   ---------------------------------------------------------------------------
   --  hexDigit
   ---------------------------------------------------------------------------
   function hexDigit (n : Unsigned_8) return Character is
      hex : constant String := "0123456789ABCDEF";
   begin
      return hex (Natural (n) + 1);
   end hexDigit;

   ---------------------------------------------------------------------------
   --  printHex8
   ---------------------------------------------------------------------------
   procedure printHex8 (val : Unsigned_8) is
      s : String (1 .. 2);
   begin
      s (1) := hexDigit (Shift_Right (val, 4) and 16#0F#);
      s (2) := hexDigit (val and 16#0F#);
      debugPrint (s);
   end printHex8;

   ---------------------------------------------------------------------------
   --  printMACAddr
   ---------------------------------------------------------------------------
   procedure printMACAddr (m : Net.MACAddress) is
   begin
      for i in m'Range loop
         if i > 0 then
            debugPrint (":");
         end if;
         printHex8 (m (i));
      end loop;
   end printMACAddr;

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
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
   --  printIP
   ---------------------------------------------------------------------------
   procedure printIP (ip : Net.IPv4Address) is
   begin
      for i in ip'Range loop
         if i > 0 then
            debugPrint (".");
         end if;
         printDec (Unsigned_32 (ip (i)));
      end loop;
   end printIP;

   ---------------------------------------------------------------------------
   --  findInterfaceByPID - find interface slot by driver PID
   ---------------------------------------------------------------------------
   function findInterfaceByPID (pid : ProcessID) return Integer is
   begin
      for i in 0 .. numIfaces - 1 loop
         if interfaces (i).driverPID = pid then
            return i;
         end if;
      end loop;
      return -1;
   end findInterfaceByPID;

   ---------------------------------------------------------------------------
   --  findInterfaceForIP - find interface that owns this IP
   ---------------------------------------------------------------------------
   function findInterfaceForIP (ip : Net.IPv4Address) return Integer is
   begin
      for i in 0 .. numIfaces - 1 loop
         if interfaces (i).state = IF_UP and then
            interfaces (i).ipv4 = ip
         then
            return i;
         end if;
      end loop;
      --  Also accept broadcast
      if ip = (255, 255, 255, 255) and numIfaces > 0 then
         return 0;
      end if;
      return -1;
   end findInterfaceForIP;

   ---------------------------------------------------------------------------
   --  gwMACResolved - check if interface 0 gateway MAC is resolved
   --  (element-wise to avoid memcmp in freestanding)
   ---------------------------------------------------------------------------
   function gwMACResolved return Boolean is
   begin
      if numIfaces = 0 then
         return False;
      end if;
      for i in interfaces (0).gwMAC'Range loop
         if interfaces (0).gwMAC (i) /= 0 then
            return True;
         end if;
      end loop;
      return False;
   end gwMACResolved;

   ---------------------------------------------------------------------------
   --  routeLookup - longest-prefix match routing lookup
   --  Returns interface index and next-hop gateway.
   ---------------------------------------------------------------------------
   procedure routeLookup (dstIP   : Net.IPv4Address;
                          ifIdx   : out Integer;
                          nextHop : out Net.IPv4Address)
   is
      bestPrefix : Integer := -1;
      bestMetric : Natural := Natural'Last;
   begin
      ifIdx := -1;
      nextHop := (others => 0);

      for i in routeTable'Range loop
         if routeTable (i).active then
            if Net.matchesPrefix (dstIP, routeTable (i).dest,
                                  routeTable (i).prefix) then
               if routeTable (i).prefix > bestPrefix or
                  (routeTable (i).prefix = bestPrefix and
                   routeTable (i).metric < bestMetric)
               then
                  bestPrefix := routeTable (i).prefix;
                  bestMetric := routeTable (i).metric;
                  ifIdx := routeTable (i).ifIdx;
                  nextHop := routeTable (i).gateway;
               end if;
            end if;
         end if;
      end loop;

      --  If next-hop is 0.0.0.0 (connected route), send directly
      if ifIdx >= 0 and nextHop = Net.IPv4Address'(others => 0) then
         nextHop := dstIP;
      end if;
   end routeLookup;

   ---------------------------------------------------------------------------
   --  installConnectedRoute - add connected route for an interface
   ---------------------------------------------------------------------------
   procedure installConnectedRoute (ifIdx : Natural) is
      network : Net.IPv4Address;
   begin
      --  Compute network address from IP & netmask
      for i in 0 .. 3 loop
         network (i) := interfaces (ifIdx).ipv4 (i) and
                        interfaces (ifIdx).netmask (i);
      end loop;

      --  Compute prefix length from netmask
      declare
         maskPacked : constant Unsigned_32 :=
            Shift_Left (Unsigned_32 (interfaces (ifIdx).netmask (0)), 24) or
            Shift_Left (Unsigned_32 (interfaces (ifIdx).netmask (1)), 16) or
            Shift_Left (Unsigned_32 (interfaces (ifIdx).netmask (2)), 8) or
            Unsigned_32 (interfaces (ifIdx).netmask (3));
         prefix : Natural := 0;
         m : Unsigned_32 := maskPacked;
      begin
         while (m and 16#8000_0000#) /= 0 loop
            prefix := prefix + 1;
            m := Shift_Left (m, 1);
         end loop;

         --  Find free slot
         for i in routeTable'Range loop
            if not routeTable (i).active then
               routeTable (i) := (active  => True,
                                   dest    => network,
                                   prefix  => prefix,
                                   gateway => (others => 0),
                                   ifIdx   => ifIdx,
                                   metric  => 0);
               exit;
            end if;
         end loop;
      end;

      --  Install default route via gateway if set
      if interfaces (ifIdx).gateway /= Net.IPv4Address'(others => 0) then
         for i in routeTable'Range loop
            if not routeTable (i).active then
               routeTable (i) := (active  => True,
                                   dest    => (others => 0),
                                   prefix  => 0,
                                   gateway => interfaces (ifIdx).gateway,
                                   ifIdx   => ifIdx,
                                   metric  => 100);
               exit;
            end if;
         end loop;
      end if;
   end installConnectedRoute;

   ---------------------------------------------------------------------------
   --  doSendFrame - send a frame to the driver via capSubmit (non-blocking)
   --
   --  Copies the frame into a rotating TX slot of the shared grant buffer,
   --  then sends OP_NET_TX to the driver using capSubmit with flags=1 so
   --  the driver does not reply (avoiding stale replies in our mailbox).
   --  Rotating slots ensure consecutive frames don't overwrite each other.
   --  Returns True if the submit succeeded, False if mailbox was full.
   ---------------------------------------------------------------------------
   function doSendFrame (frameAddr : System.Address;
                         frameLen  : Natural) return Boolean is
      curBuf  : constant System.Address := interfaces (0).pktBuf;
      slotOff : constant Natural :=
         TX_AREA_OFFSET + interfaces (0).txSlotIdx * TX_SLOT_SIZE;
      txBuf   : constant System.Address :=
         curBuf + Storage_Offset (slotOff);
      txMsg   : Message :=
        (tag      => (label  => OP_NET_TX,
                      length => 2,
                      flags  => 1,      -- fire-and-forget (no reply)
                      badge  => 0),
         capBadge => 0,
         words    => (0 => Unsigned_64 (slotOff),
                      1 => Unsigned_64 (frameLen),
                      others => 0));
      ok : Boolean;
   begin
      --  Copy frame into this TX slot of the grant buffer
      declare
         src : array (0 .. frameLen - 1) of Unsigned_8 with
            Import, Address => frameAddr;
         dst : array (0 .. frameLen - 1) of Unsigned_8 with
            Import, Address => txBuf;
      begin
         for i in src'Range loop
            dst (i) := src (i);
         end loop;
      end;

      ok := capSubmit (CAP_SLOT_NET_DRV, txMsg, Unsigned_64'Last);
      if ok then
         interfaces (0).txSlotIdx :=
            (interfaces (0).txSlotIdx + 1) mod NUM_TX_SLOTS;
      end if;
      return ok;
   end doSendFrame;

   ---------------------------------------------------------------------------
   --  flushOneDeferredTX - try to send one deferred TX frame via capSubmit
   --
   --  Returns True if the frame was sent, False if the driver's mailbox
   --  was full (caller should yield and retry later).
   ---------------------------------------------------------------------------
   function flushOneDeferredTX return Boolean is
      ok : Boolean;
   begin
      if deferredCount = 0 then
         return True;
      end if;

      ok := doSendFrame (deferredTX (0).data'Address,
                          deferredTX (0).len);
      if ok then
         --  Shift remaining frames down
         for i in 1 .. deferredCount - 1 loop
            deferredTX (i - 1) := deferredTX (i);
         end loop;
         deferredCount := deferredCount - 1;
      end if;
      return ok;
   end flushOneDeferredTX;

   ---------------------------------------------------------------------------
   --  sendFrame - send a frame to the driver via capSubmit
   --
   --  Always attempts capSubmit immediately.  If the driver's mailbox is
   --  full (submit fails), the frame is buffered in the deferred TX queue
   --  for later retry from the main loop.
   ---------------------------------------------------------------------------
   procedure sendFrame (frameAddr : System.Address; frameLen : Natural) is
      ok : Boolean;
   begin
      if interfaces (0).driverPID = NO_PROCESS or
         interfaces (0).pktBuf = System.Null_Address
      then
         debugPrint ("netstack: sendFrame: not attached" & LF);
         return;
      end if;

      if frameLen > MAX_DEFERRED_LEN then
         debugPrint ("netstack: sendFrame: frame too large" & LF);
         return;
      end if;

      ok := doSendFrame (frameAddr, frameLen);
      if not ok then
         --  Driver mailbox full, buffer for later
         if deferredCount < MAX_DEFERRED_TX then
            declare
               src : array (0 .. frameLen - 1) of Unsigned_8 with
                  Import, Address => frameAddr;
            begin
               for i in src'Range loop
                  deferredTX (deferredCount).data (i) := src (i);
               end loop;
            end;
            deferredTX (deferredCount).len := frameLen;
            deferredCount := deferredCount + 1;
            debugPrint ("netstack: deferred TX (");
            printDec (Unsigned_32 (deferredCount));
            debugPrint ("/");
            printDec (Unsigned_32 (MAX_DEFERRED_TX));
            debugPrint (")" & LF);
         else
            debugPrint ("netstack: deferred TX full, dropping" & LF);
         end if;
      end if;
   end sendFrame;

   ---------------------------------------------------------------------------
   --  sendARPReply - respond to an ARP request
   ---------------------------------------------------------------------------
   procedure sendARPReply (dstMAC : Net.MACAddress;
                           dstIP  : Net.IPv4Address) is
      frame : array (0 .. 41) of Unsigned_8;  -- 14 eth + 28 ARP
      fAddr : constant System.Address := frame'Address;
   begin
      --  Ethernet header
      Net.putMAC   (fAddr, 0,  dstMAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_ARP);

      --  ARP payload
      Net.putU16BE (fAddr, 14, 1);                 -- HTYPE: Ethernet
      Net.putU16BE (fAddr, 16, 16#0800#);          -- PTYPE: IPv4
      Net.putU8    (fAddr, 18, 6);                  -- HLEN
      Net.putU8    (fAddr, 19, 4);                  -- PLEN
      Net.putU16BE (fAddr, 20, Net.ARP_REPLY);     -- OPER
      Net.putMAC   (fAddr, 22, interfaces (0).mac);
      Net.putIP    (fAddr, 28, interfaces (0).ipv4);
      Net.putMAC   (fAddr, 32, dstMAC);
      Net.putIP    (fAddr, 38, dstIP);

      sendFrame (fAddr, 42);
      debugPrint ("NET: sent ARP reply to ");
      printIP (dstIP);
      debugPrint ("" & LF);
   end sendARPReply;

   ---------------------------------------------------------------------------
   --  sendGratuitousARP
   ---------------------------------------------------------------------------
   procedure sendGratuitousARP is
      frame : array (0 .. 41) of Unsigned_8;
      fAddr : constant System.Address := frame'Address;
   begin
      Net.putMAC   (fAddr, 0,  Net.BROADCAST_MAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_ARP);

      Net.putU16BE (fAddr, 14, 1);
      Net.putU16BE (fAddr, 16, 16#0800#);
      Net.putU8    (fAddr, 18, 6);
      Net.putU8    (fAddr, 19, 4);
      Net.putU16BE (fAddr, 20, Net.ARP_REPLY);
      Net.putMAC   (fAddr, 22, interfaces (0).mac);
      Net.putIP    (fAddr, 28, interfaces (0).ipv4);
      Net.putMAC   (fAddr, 32, Net.ZERO_MAC);
      Net.putIP    (fAddr, 38, interfaces (0).ipv4);

      sendFrame (fAddr, 42);
      debugPrint ("NET: sent gratuitous ARP for ");
      printIP (interfaces (0).ipv4);
      debugPrint ("" & LF);
   end sendGratuitousARP;

   ---------------------------------------------------------------------------
   --  sendARPRequest
   ---------------------------------------------------------------------------
   procedure sendARPRequest (targetIP : Net.IPv4Address) is
      frame : array (0 .. 41) of Unsigned_8;
      fAddr : constant System.Address := frame'Address;
   begin
      Net.putMAC   (fAddr, 0,  Net.BROADCAST_MAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_ARP);

      Net.putU16BE (fAddr, 14, 1);
      Net.putU16BE (fAddr, 16, 16#0800#);
      Net.putU8    (fAddr, 18, 6);
      Net.putU8    (fAddr, 19, 4);
      Net.putU16BE (fAddr, 20, Net.ARP_REQUEST);
      Net.putMAC   (fAddr, 22, interfaces (0).mac);
      Net.putIP    (fAddr, 28, interfaces (0).ipv4);
      Net.putMAC   (fAddr, 32, Net.ZERO_MAC);
      Net.putIP    (fAddr, 38, targetIP);

      sendFrame (fAddr, 42);
      debugPrint ("NET: sent ARP request for ");
      printIP (targetIP);
      debugPrint ("" & LF);
   end sendARPRequest;

   ---------------------------------------------------------------------------
   --  sendICMPEchoRequest
   ---------------------------------------------------------------------------
   procedure sendICMPEchoRequest (dstIP  : Net.IPv4Address;
                                  dstMAC : Net.MACAddress;
                                  seq    : Unsigned_16;
                                  ifIdx  : Natural := 0) is
      FRAME_LEN   : constant := 74;
      PAYLOAD_LEN : constant := 32;
      frame : array (0 .. FRAME_LEN - 1) of Unsigned_8;
      fAddr : constant System.Address := frame'Address;
      ipLen : constant Unsigned_16 := 20 + 8 + PAYLOAD_LEN;
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      --  Ethernet header
      Net.putMAC   (fAddr, 0,  dstMAC);
      Net.putMAC   (fAddr, 6,  interfaces (ifIdx).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      --  IPv4 header (20 bytes at offset 14)
      Net.putU8    (fAddr, 14, 16#45#);
      Net.putU8    (fAddr, 15, 0);
      Net.putU16BE (fAddr, 16, ipLen);
      Net.putU16BE (fAddr, 18, 16#0001#);
      Net.putU16BE (fAddr, 20, 0);
      Net.putU8    (fAddr, 22, 64);
      Net.putU8    (fAddr, 23, Net.PROTO_ICMP);
      Net.putU16BE (fAddr, 24, 0);
      Net.putIP    (fAddr, 26, interfaces (ifIdx).ipv4);
      Net.putIP    (fAddr, 30, dstIP);

      declare
         ipCksum : Unsigned_16;
      begin
         ipCksum := Net.internetChecksum (fAddr + 14, 20);
         Net.putU16BE (fAddr, 24, ipCksum);
      end;

      --  ICMP echo request (at offset 34)
      Net.putU8    (fAddr, 34, Net.ICMP_ECHO_REQUEST);
      Net.putU8    (fAddr, 35, 0);
      Net.putU16BE (fAddr, 36, 0);
      Net.putU16BE (fAddr, 38, 16#CB17#);
      Net.putU16BE (fAddr, 40, seq);

      for i in 0 .. PAYLOAD_LEN - 1 loop
         Net.putU8 (fAddr, 42 + i, Unsigned_8 (i mod 256));
      end loop;

      declare
         icmpCksum : Unsigned_16;
      begin
         icmpCksum := Net.internetChecksum (fAddr + 34, 8 + PAYLOAD_LEN);
         Net.putU16BE (fAddr, 36, icmpCksum);
      end;

      sendFrame (fAddr, FRAME_LEN);
      debugPrint ("NET: sent ICMP echo to ");
      printIP (dstIP);
      debugPrint (" seq=");
      printDec (Unsigned_32 (seq));
      debugPrint ("" & LF);
   end sendICMPEchoRequest;

   ---------------------------------------------------------------------------
   --  sendIPv4Frame - build Ethernet + IPv4 wrapper, place payload, send
   --
   --  Caller writes transport payload into frame at offset 34.
   --  This fills in Ethernet (14 bytes) + IPv4 (20 bytes) headers around it.
   ---------------------------------------------------------------------------
   procedure sendIPv4Frame (dstMAC   : Net.MACAddress;
                            dstIP    : Net.IPv4Address;
                            proto    : Unsigned_8;
                            fAddr    : System.Address;
                            frameLen : Natural) is
      ipTotal : constant Unsigned_16 := Unsigned_16 (frameLen - 14);
   begin
      --  Ethernet header
      Net.putMAC   (fAddr, 0,  dstMAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      --  IPv4 header (20 bytes at offset 14)
      Net.putU8    (fAddr, 14, 16#45#);         -- ver=4, IHL=5
      Net.putU8    (fAddr, 15, 0);               -- DSCP/ECN
      Net.putU16BE (fAddr, 16, ipTotal);         -- total length
      Net.putU16BE (fAddr, 18, 16#0001#);        -- identification
      Net.putU16BE (fAddr, 20, 16#4000#);        -- flags=DF, frag=0
      Net.putU8    (fAddr, 22, 64);              -- TTL
      Net.putU8    (fAddr, 23, proto);
      Net.putU16BE (fAddr, 24, 0);               -- checksum (filled below)
      Net.putIP    (fAddr, 26, interfaces (0).ipv4);
      Net.putIP    (fAddr, 30, dstIP);

      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.internetChecksum (fAddr + 14, 20);
         Net.putU16BE (fAddr, 24, cksum);
      end;

      sendFrame (fAddr, frameLen);
   end sendIPv4Frame;

   ---------------------------------------------------------------------------
   --  sendUDP - build and send a UDP datagram inside an IPv4 frame
   --
   --  Uses RecordFlux to serialize the UDP header into a local buffer,
   --  then wraps in Ethernet + IPv4 manually.
   ---------------------------------------------------------------------------
   procedure sendUDP (dstIP      : Net.IPv4Address;
                      dstMAC     : Net.MACAddress;
                      srcPort    : Unsigned_16;
                      dstPort    : Unsigned_16;
                      payload    : System.Address;
                      payloadLen : Natural) is
      use Net.RFLX_Builtin_Types;
      totalUDP : constant Natural := 8 + payloadLen;
      frameLen : constant Natural := 14 + 20 + totalUDP;
      frame    : array (0 .. frameLen - 1) of Unsigned_8;
      fAddr    : constant System.Address := frame'Address;

      --  RecordFlux buffer overlaid at UDP offset (byte 34) in frame
      udpBuf : aliased Bytes (1 .. Index (totalUDP))
         with Import, Address => fAddr + 34;
      bufPtr : Bytes_Ptr := udpBuf'Unrestricted_Access;
      ctx    : Net.UDP.Datagram.Context;

      --  Payload as RFLX Bytes for Set_Payload
      payBytes : Bytes (1 .. Index (payloadLen))
         with Import, Address => payload;
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      --  Build UDP datagram via RecordFlux
      Net.UDP.Datagram.Initialize (ctx, bufPtr);
      Net.UDP.Datagram.Set_Source_Port
         (ctx, Net.UDP.Port (srcPort));
      Net.UDP.Datagram.Set_Destination_Port
         (ctx, Net.UDP.Port (dstPort));
      Net.UDP.Datagram.Set_Length
         (ctx, Net.UDP.Length (totalUDP));
      Net.UDP.Datagram.Set_Checksum (ctx, 0);
      if payloadLen > 0 then
         Net.UDP.Datagram.Set_Payload (ctx, payBytes);
      else
         Net.UDP.Datagram.Set_Payload_Empty (ctx);
      end if;
      Net.UDP.Datagram.Take_Buffer (ctx, bufPtr);

      --  Compute UDP checksum over pseudo-header + serialized datagram
      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.transportChecksum
            (interfaces (0).ipv4, dstIP, Net.PROTO_UDP, fAddr + 34, totalUDP);
         Net.putU16BE (fAddr, 40, cksum);   -- UDP checksum at offset 34+6
      end;

      sendIPv4Frame (dstMAC, dstIP, Net.PROTO_UDP, fAddr, frameLen);
   end sendUDP;

   ---------------------------------------------------------------------------
   --  sendDNSQuery - send a DNS A-record query for a given hostname
   --
   --  Encodes hostname as DNS labels (split on '.'), uses given TXID.
   ---------------------------------------------------------------------------
   procedure sendDNSQuery (hostname : String; txid : Unsigned_16) is
      --  Max DNS query payload: 12 header + ~64 question = ~76 bytes
      MAX_DNS_PAYLOAD : constant := 80;
      dnsPayload : array (0 .. MAX_DNS_PAYLOAD - 1) of Unsigned_8;
      pAddr  : constant System.Address := dnsPayload'Address;
      dnsMAC : Net.MACAddress;
      off    : Natural := 12;   -- question section starts after header
      lblStart : Natural;
      lblLen   : Natural;
   begin
      for i in dnsPayload'Range loop
         dnsPayload (i) := 0;
      end loop;

      --  DNS header
      Net.putU16BE (pAddr, 0, txid);
      Net.putU16BE (pAddr, 2, 16#0100#);  -- flags: RD=1
      Net.putU16BE (pAddr, 4, 1);         -- QDCOUNT=1

      --  Encode hostname as DNS labels: split on '.', emit len-prefixed
      lblStart := hostname'First;
      for i in hostname'First .. hostname'Last + 1 loop
         if i > hostname'Last or else hostname (i) = '.' then
            lblLen := i - lblStart;
            if lblLen > 0 and off < MAX_DNS_PAYLOAD - 5 then
               Net.putU8 (pAddr, off, Unsigned_8 (lblLen));
               off := off + 1;
               for j in lblStart .. i - 1 loop
                  Net.putU8 (pAddr, off,
                             Unsigned_8 (Character'Pos (hostname (j))));
                  off := off + 1;
               end loop;
            end if;
            lblStart := i + 1;
         end if;
      end loop;
      --  Guard: need 1 (root label) + 2 (QTYPE) + 2 (QCLASS) = 5 bytes
      if off + 5 > MAX_DNS_PAYLOAD then
         debugPrint ("DNS: hostname too long, query dropped" & LF);
         return;
      end if;

      Net.putU8 (pAddr, off, 0);          -- root label
      off := off + 1;
      Net.putU16BE (pAddr, off, 1);       -- QTYPE: A
      off := off + 2;
      Net.putU16BE (pAddr, off, 1);       -- QCLASS: IN
      off := off + 2;

      --  Resolve DNS server MAC
      if not Net.arpLookup (interfaces (0).arpCache, primaryDNS,
                             dnsMAC) then
         dnsMAC := interfaces (0).gwMAC;
      end if;

      sendUDP (primaryDNS, dnsMAC, 10053, 53, pAddr, off);
      debugPrint ("UDP: sent DNS query for ");
      debugPrint (hostname);
      debugPrint (" to ");
      printIP (primaryDNS);
      debugPrint ("" & LF);
   end sendDNSQuery;

   --  Forward declarations for functions used by handleDNSResponse
   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstMAC  : Net.MACAddress;
                        dstPort : Unsigned_16) return Integer;
   procedure replyError (to : ProcessID);

   ---------------------------------------------------------------------------
   --  handleDNSResponse - parse DNS A-record response, extract IP
   --
   --  Called with raw DNS payload bytes (after UDP header).
   ---------------------------------------------------------------------------
   procedure handleDNSResponse (dnsBuf : System.Address;
                                dnsLen : Natural) is
      txid    : Unsigned_16;
      flags   : Unsigned_16;
      ancount : Unsigned_16;
      off     : Natural;
      rtype   : Unsigned_16;
      rdlen   : Unsigned_16;
      foundA  : Boolean := False;

      function skipDNSName (pos : in out Natural) return Boolean is
         length : Unsigned_8;
      begin
         loop
            if pos >= dnsLen then
               return False;
            end if;
            length := Net.getU8 (dnsBuf, pos);
            pos := pos + 1;
            if length = 0 then
               return True;
            elsif (length and 16#C0#) = 16#C0# then
               --  A compression pointer always occupies two bytes.  Its
               --  target need not be followed to find the end of this name.
               if pos >= dnsLen then
                  return False;
               end if;
               pos := pos + 1;
               return True;
            elsif length > 63 or else Natural (length) > dnsLen - pos then
               return False;
            end if;
            pos := pos + Natural (length);
         end loop;
      end skipDNSName;
   begin
      if dnsLen < 12 then
         return;
      end if;

      txid    := Net.getU16BE (dnsBuf, 0);
      flags   := Net.getU16BE (dnsBuf, 2);
      ancount := Net.getU16BE (dnsBuf, 6);

      --  Verify QR bit (response)
      if (flags and 16#8000#) = 0 then
         return;
      end if;

      --  Check TXID: accept self-test (CB17) or any pending app request
      declare
         knownTxid : Boolean := (txid = 16#CB17#);
      begin
         if not knownTxid then
            for i in pendingReqs'Range loop
               if (pendingReqs (i).kind = PENDING_RESOLVE or
                   pendingReqs (i).kind = PENDING_OPEN) and
                  pendingReqs (i).txid = txid
               then
                  knownTxid := True;
                  exit;
               end if;
            end loop;
         end if;
         if not knownTxid then
            return;
         end if;
      end;

      if ancount = 0 then
         debugPrint ("DNS: no answers" & LF);
         return;
      end if;

      --  Skip question section: QNAME + QTYPE(2) + QCLASS(2).
      off := 12;
      if not skipDNSName (off) or else off > dnsLen - 4 then
         return;
      end if;
      off := off + 4;

      --  CNAME chains are common on real sites. Walk every answer RR and
      --  select the first bounded IPv4 A record instead of assuming answer
      --  zero is directly an A record.
      for answer in 1 .. Natural (ancount) loop
         if not skipDNSName (off) or else off > dnsLen - 10 then
            return;
         end if;
         rtype := Net.getU16BE (dnsBuf, off);
         off := off + 8;   -- TYPE(2) + CLASS(2) + TTL(4)
         rdlen := Net.getU16BE (dnsBuf, off);
         off := off + 2;
         if Natural (rdlen) > dnsLen - off then
            return;
         end if;
         if rtype = 1 and rdlen = 4 then
            Net.getIP (dnsBuf, off, resolvedIP);
            foundA := True;
            exit;
         end if;
         off := off + Natural (rdlen);
      end loop;

      --  A record: type=1, rdlen=4
      if foundA then
         debugPrint ("DNS: response -> ");
         printIP (resolvedIP);
         debugPrint ("" & LF);

         --  Complete any pending RESOLVE request matching this TXID
         for i in pendingReqs'Range loop
            if pendingReqs (i).kind = PENDING_RESOLVE and
               pendingReqs (i).txid = txid
            then
               declare
                  ipPacked : constant Unsigned_64 :=
                     Unsigned_64 (resolvedIP (0)) or
                     Shift_Left (Unsigned_64 (resolvedIP (1)), 8) or
                     Shift_Left (Unsigned_64 (resolvedIP (2)), 16) or
                     Shift_Left (Unsigned_64 (resolvedIP (3)), 24);
                  replyMsg : constant Message :=
                    (tag      => (label  => REPLY_OK,
                                  length => 1,
                                  flags  => 0,
                                  badge  => 0),
                     capBadge => 0,
                     words    => (0 => ipPacked, others => 0));
                  ignore : Unsigned_64;
               begin
                  ignore := reply (pendingReqs (i).sender, replyMsg);
               end;
               pendingReqs (i).kind := PENDING_NONE;
               exit;
            end if;
         end loop;

         --  Complete any pending OPEN request matching this TXID
         --  (DNS phase done → initiate TCP connect, transition to
         --  PENDING_CONNECT so completePendingConnect picks it up)
         for i in pendingReqs'Range loop
            if pendingReqs (i).kind = PENDING_OPEN and
               pendingReqs (i).txid = txid
            then
               declare
                  chIdx   : constant Integer := pendingReqs (i).channelIdx;
                  connIdx : Integer;
               begin
                  if chIdx >= 0 and chIdx <= channels'Last then
                     channels (chIdx).remoteIP := resolvedIP;
                     connIdx := tcpConnect (resolvedIP, interfaces (0).gwMAC,
                                            pendingReqs (i).dstPort);
                     if connIdx < 0 then
                        channels (chIdx).kind := CHANNEL_NONE;
                        replyError (pendingReqs (i).sender);
                        pendingReqs (i).kind := PENDING_NONE;
                     else
                        channels (chIdx).connIdx := connIdx;
                        --  Transition: PENDING_OPEN -> PENDING_CONNECT
                        --  so completePendingConnect will reply with
                        --  the channel handle.
                        pendingReqs (i).kind := PENDING_CONNECT;
                        pendingReqs (i).connIdx := connIdx;
                     end if;
                  else
                     replyError (pendingReqs (i).sender);
                     pendingReqs (i).kind := PENDING_NONE;
                  end if;
               end;
               exit;
            end if;
         end loop;
      end if;
   end handleDNSResponse;

   ---------------------------------------------------------------------------
   --  handleUDP - parse UDP datagram via RecordFlux, dispatch on port
   ---------------------------------------------------------------------------
   procedure handleUDP (pktBuf     : System.Address;
                        ipOff      : Natural;
                        ipHdrLen   : Natural;
                        srcIP      : Net.IPv4Address;
                        totalIPLen : Natural) is
      use Net.RFLX_Builtin_Types;
      udpOff : constant Natural := ipOff + ipHdrLen;
      udpLen : constant Natural := totalIPLen - ipHdrLen;

      udpBuf : aliased Bytes (1 .. Index (udpLen))
         with Import, Address => pktBuf + Storage_Offset (udpOff);
      bufPtr : Bytes_Ptr := udpBuf'Unrestricted_Access;
      ctx    : Net.UDP.Datagram.Context;

      srcPort : Unsigned_16;
      dstPort : Unsigned_16;
   begin
      if udpLen < 8 then
         return;
      end if;

      Net.UDP.Datagram.Initialize
         (ctx, bufPtr,
          Written_Last => Net.RFLX_Types.Bit_Length (udpLen) * 8);
      Net.UDP.Datagram.Verify_Message (ctx);

      if Net.UDP.Datagram.Well_Formed_Message (ctx) then
         srcPort := Unsigned_16 (Net.UDP.Datagram.Get_Source_Port (ctx));
         dstPort := Unsigned_16 (Net.UDP.Datagram.Get_Destination_Port (ctx));

         debugPrint ("UDP: ");
         printIP (srcIP);
         debugPrint (":");
         printDec (Unsigned_32 (srcPort));
         debugPrint (" -> port ");
         printDec (Unsigned_32 (dstPort));
         debugPrint ("" & LF);

         --  DNS response (from port 53)
         if srcPort = 53 then
            --  DNS payload starts at UDP offset + 8
            handleDNSResponse
               (pktBuf + Storage_Offset (udpOff + 8), udpLen - 8);
         end if;
      else
         debugPrint ("UDP: malformed datagram" & LF);
      end if;

      Net.UDP.Datagram.Take_Buffer (ctx, bufPtr);
   end handleUDP;

   ---------------------------------------------------------------------------
   --  sendTCPSegment - build TCP segment via RecordFlux, wrap in IPv4
   --
   --  flags encoding: bit 0=FIN, 1=SYN, 2=RST, 3=PSH, 4=ACK
   ---------------------------------------------------------------------------
   procedure sendTCPSegment (conn    : TCPSession.Connection;
                             flags   : Unsigned_8;
                             seqNum  : Unsigned_32;
                             ackNum  : Unsigned_32;
                             payload : System.Address;
                             payLen  : Natural) is
      use Net.RFLX_Builtin_Types;
      tcpLen   : constant Natural := 20 + payLen;  -- data offset = 5
      frameLen : constant Natural := 14 + 20 + tcpLen;
      frame    : array (0 .. frameLen - 1) of Unsigned_8;
      fAddr    : constant System.Address := frame'Address;

      --  RecordFlux buffer overlaid at TCP offset (byte 34) in frame
      tcpBuf : aliased Bytes (1 .. Index (tcpLen))
         with Import, Address => fAddr + 34;
      bufPtr : Bytes_Ptr := tcpBuf'Unrestricted_Access;
      ctx    : Net.TCP.Segment.Context;

      isSYN : constant Boolean := (flags and 2) /= 0;
      isFIN : constant Boolean := (flags and 1) /= 0;
      isRST : constant Boolean := (flags and 4) /= 0;
      isPSH : constant Boolean := (flags and 8) /= 0;
      isACK : constant Boolean := (flags and 16) /= 0;
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      --  Build TCP segment via RecordFlux
      Net.TCP.Segment.Initialize
         (ctx, bufPtr,
          Segment_Length => Net.TCP.Segment_Length (tcpLen));
      Net.TCP.Segment.Set_Source_Port
         (ctx, Net.TCP.Port (conn.localPort));
      Net.TCP.Segment.Set_Destination_Port
         (ctx, Net.TCP.Port (conn.remotePort));
      Net.TCP.Segment.Set_Sequence_Number
         (ctx, Net.TCP.Sequence_Number (seqNum));
      Net.TCP.Segment.Set_Acknowledgment_Number
         (ctx, Net.TCP.Acknowledgment_Number (ackNum));
      Net.TCP.Segment.Set_Data_Offset (ctx, 5);
      Net.TCP.Segment.Set_Reserved (ctx, False);
      Net.TCP.Segment.Set_Reserved_2 (ctx, False);
      Net.TCP.Segment.Set_Reserved_3 (ctx, False);
      Net.TCP.Segment.Set_NS  (ctx, False);
      Net.TCP.Segment.Set_CWR (ctx, False);
      Net.TCP.Segment.Set_ECN (ctx, False);
      Net.TCP.Segment.Set_URG (ctx, False);
      Net.TCP.Segment.Set_ACK (ctx, isACK);
      Net.TCP.Segment.Set_PSH (ctx, isPSH);
      Net.TCP.Segment.Set_RST (ctx, isRST);
      Net.TCP.Segment.Set_SYN (ctx, isSYN);
      Net.TCP.Segment.Set_FIN (ctx, isFIN);
      Net.TCP.Segment.Set_Window (ctx, 8192);
      Net.TCP.Segment.Set_Checksum (ctx, 0);
      Net.TCP.Segment.Set_Urgent_Pointer (ctx, 0);
      Net.TCP.Segment.Set_Options_Empty (ctx);
      if payLen > 0 then
         declare
            payBytes : Bytes (1 .. Index (payLen))
               with Import, Address => payload;
         begin
            Net.TCP.Segment.Set_Data (ctx, payBytes);
         end;
      else
         Net.TCP.Segment.Set_Data_Empty (ctx);
      end if;
      Net.TCP.Segment.Take_Buffer (ctx, bufPtr);

      --  Compute TCP checksum over pseudo-header + serialized segment
      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.transportChecksum
            (interfaces (0).ipv4, conn.remoteIP, Net.PROTO_TCP, fAddr + 34, tcpLen);
         Net.putU16BE (fAddr, 50, cksum);  -- TCP checksum at 34+16
      end;

      sendIPv4Frame (conn.remoteMAC, conn.remoteIP, Net.PROTO_TCP,
                     fAddr, frameLen);
   end sendTCPSegment;

   ---------------------------------------------------------------------------
   --  tcpConnect - initiate a TCP connection (SYN)
   ---------------------------------------------------------------------------
   procedure executeActions (connIdx : Natural;
                             res     : TCPSession.Result;
                             pktBuf  : System.Address);

   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstMAC  : Net.MACAddress;
                        dstPort : Unsigned_16) return Integer is
      lport : Unsigned_16;
      idx   : Integer;
      res   : TCPSession.Result;
   begin
      tcpISN := tcpISN + 64000;
      lport := nextEphemeralPort;
      nextEphemeralPort := nextEphemeralPort + 1;
      if nextEphemeralPort = 0 then
         nextEphemeralPort := 49152;
      end if;

      idx := TCPSession.allocateConn
         (tcpConns, dstIP, dstMAC, dstPort, lport, tcpISN);
      if idx < 0 then
         return -1;
      end if;
      rxBuffers (idx).len := 0;

      debugPrint ("TCP: SYN to ");
      printIP (dstIP);
      debugPrint (":");
      printDec (Unsigned_32 (dstPort));
      debugPrint ("" & LF);

      TCPSession.onConnect (tcpConns (idx), res);
      executeActions (idx, res, System.Null_Address);
      return idx;
   end tcpConnect;

   ---------------------------------------------------------------------------
   --  tcpSend - send data on an established connection (PSH+ACK)
   ---------------------------------------------------------------------------
   procedure tcpSend (connIdx : Natural;
                      payload : System.Address;
                      payLen  : Natural) is
      res : TCPSession.Result;
   begin
      if connIdx > tcpConns'Last then
         return;
      end if;
      TCPSession.onSend (tcpConns (connIdx), payLen, 0, res);
      --  onSend produces ACT_SEND_SEGMENT; we handle it specially here
      --  since the payload address is known only by the caller.
      if res.numActions > 0 and then
         res.actions (0).kind = TCPSession.ACT_SEND_SEGMENT
      then
         sendTCPSegment (tcpConns (connIdx), res.actions (0).flags,
                         res.actions (0).seqNum,
                         res.actions (0).ackNum,
                         payload, payLen);
         debugPrint ("TCP: sent ");
         printDec (Unsigned_32 (payLen));
         debugPrint (" bytes" & LF);
      end if;
   end tcpSend;

   ---------------------------------------------------------------------------
   --  tcpClose - initiate close (FIN+ACK)
   ---------------------------------------------------------------------------
   procedure tcpClose (connIdx : Natural) is
      res : TCPSession.Result;
   begin
      if connIdx > tcpConns'Last then
         return;
      end if;
      TCPSession.onClose (tcpConns (connIdx), res);
      executeActions (connIdx, res, System.Null_Address);
   end tcpClose;

   ---------------------------------------------------------------------------
   --  sendTCPDNSQuery - send DNS query over TCP (2-byte length prefix)
   ---------------------------------------------------------------------------
   procedure sendTCPDNSQuery (connIdx : Natural) is
      DNS_PAYLOAD_LEN : constant := 29;
      --  2-byte length prefix + DNS query
      tcpPayload : array (0 .. 1 + DNS_PAYLOAD_LEN - 1) of Unsigned_8;
      pAddr      : constant System.Address := tcpPayload'Address;
   begin
      for i in tcpPayload'Range loop
         tcpPayload (i) := 0;
      end loop;

      --  2-byte length prefix (big-endian)
      Net.putU16BE (pAddr, 0, DNS_PAYLOAD_LEN);

      --  DNS header (same as UDP query)
      Net.putU16BE (pAddr, 2, 16#CB17#);
      Net.putU16BE (pAddr, 4, 16#0100#);
      Net.putU16BE (pAddr, 6, 1);

      --  Question: \x07example\x03com\x00 type=A class=IN
      Net.putU8 (pAddr, 14, 7);
      Net.putU8 (pAddr, 15, Character'Pos ('e'));
      Net.putU8 (pAddr, 16, Character'Pos ('x'));
      Net.putU8 (pAddr, 17, Character'Pos ('a'));
      Net.putU8 (pAddr, 18, Character'Pos ('m'));
      Net.putU8 (pAddr, 19, Character'Pos ('p'));
      Net.putU8 (pAddr, 20, Character'Pos ('l'));
      Net.putU8 (pAddr, 21, Character'Pos ('e'));
      Net.putU8 (pAddr, 22, 3);
      Net.putU8 (pAddr, 23, Character'Pos ('c'));
      Net.putU8 (pAddr, 24, Character'Pos ('o'));
      Net.putU8 (pAddr, 25, Character'Pos ('m'));
      Net.putU8 (pAddr, 26, 0);
      Net.putU16BE (pAddr, 27, 1);
      Net.putU16BE (pAddr, 29, 1);

      tcpSend (connIdx, pAddr, 2 + DNS_PAYLOAD_LEN);
      debugPrint ("TCP: sent DNS query (");
      printDec (Unsigned_32 (2 + DNS_PAYLOAD_LEN));
      debugPrint (" bytes)" & LF);
   end sendTCPDNSQuery;

   --  Forward declaration for reply helper (defined later in file)
   procedure replyOKWord (to : ProcessID; w0 : Unsigned_64);

   ---------------------------------------------------------------------------
   --  completePendingConnect - complete a PENDING_CONNECT for connIdx
   ---------------------------------------------------------------------------
   procedure completePendingConnect (connIdx : Natural) is
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_CONNECT and
            pendingReqs (i).connIdx = connIdx
         then
            if pendingReqs (i).channelIdx >= 0 then
               --  Channel API: reply with channel handle
               replyOKWord (pendingReqs (i).sender,
                            Unsigned_64 (pendingReqs (i).channelIdx));
            else
               --  Legacy API: reply with raw connIdx
               replyOKWord (pendingReqs (i).sender,
                            Unsigned_64 (connIdx));
            end if;
            pendingReqs (i).kind := PENDING_NONE;
            exit;
         end if;
      end loop;

      --  Also check for PENDING_OPEN (channel API: DNS resolved, now
      --  connected) — same logic as PENDING_CONNECT with channelIdx.
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_OPEN and
            pendingReqs (i).connIdx = connIdx and
            pendingReqs (i).channelIdx >= 0
         then
            replyOKWord (pendingReqs (i).sender,
                         Unsigned_64 (pendingReqs (i).channelIdx));
            pendingReqs (i).kind := PENDING_NONE;
            exit;
         end if;
      end loop;
   end completePendingConnect;

   function hasPendingRecv (connIdx : Natural) return Boolean is
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_RECV and
            pendingReqs (i).connIdx = connIdx
         then
            return True;
         end if;
      end loop;
      return False;
   end hasPendingRecv;

   procedure bufferReceived (connIdx : Natural;
                             pktBuf  : System.Address;
                             dataOff : Natural;
                             dataLen : Natural) is
      available : constant Natural := RX_BUFFER_SIZE - rxBuffers (connIdx).len;
      copyLen   : constant Natural := Natural'Min (dataLen, available);
   begin
      if copyLen > 0 then
         declare
            src : array (0 .. copyLen - 1) of Unsigned_8
               with Import, Address => pktBuf + Storage_Offset (dataOff);
         begin
            for j in src'Range loop
               rxBuffers (connIdx).data (rxBuffers (connIdx).len + j) := src (j);
            end loop;
         end;
         rxBuffers (connIdx).len := rxBuffers (connIdx).len + copyLen;
      end if;
   end bufferReceived;

   procedure replyBuffered (snd : ProcessID; connIdx : Natural;
                            bufAddr : System.Address; offset, maxLen : Natural) is
      copyLen : constant Natural := Natural'Min (rxBuffers (connIdx).len, maxLen);
   begin
      if copyLen > 0 then
         declare
            dst : array (0 .. copyLen - 1) of Unsigned_8
               with Import, Address => bufAddr + Storage_Offset (offset);
         begin
            for j in dst'Range loop
               dst (j) := rxBuffers (connIdx).data (j);
            end loop;
         end;
         for j in copyLen .. rxBuffers (connIdx).len - 1 loop
            rxBuffers (connIdx).data (j - copyLen) := rxBuffers (connIdx).data (j);
         end loop;
         rxBuffers (connIdx).len := rxBuffers (connIdx).len - copyLen;
      end if;
      replyOKWord (snd, Unsigned_64 (copyLen));
   end replyBuffered;

   procedure completePendingBuffered (connIdx : Natural) is
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_RECV and
            pendingReqs (i).connIdx = connIdx
         then
            replyBuffered (pendingReqs (i).sender, connIdx,
                           pendingReqs (i).bufAddr,
                           pendingReqs (i).bufOff,
                           pendingReqs (i).maxLen);
            pendingReqs (i).kind := PENDING_NONE;
            return;
         end if;
      end loop;
   end completePendingBuffered;

   ---------------------------------------------------------------------------
   --  completePendingRecvEOF - reply EOF to pending RECV for connIdx
   ---------------------------------------------------------------------------
   procedure completePendingRecvEOF (connIdx : Natural) is
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_RECV and
            pendingReqs (i).connIdx = connIdx
         then
            declare
               eofMsg : constant Message :=
                 (tag      => (label  => REPLY_EOF,
                               length => 0,
                               flags  => 0,
                               badge  => 0),
                  capBadge => 0,
                  words    => (others => 0));
               ignore : Unsigned_64;
            begin
               ignore := reply (pendingReqs (i).sender, eofMsg);
            end;
            pendingReqs (i).kind := PENDING_NONE;
         end if;
      end loop;
   end completePendingRecvEOF;

   ---------------------------------------------------------------------------
   --  completePendingError - reply ERR to CONNECT or RECV for connIdx
   ---------------------------------------------------------------------------
   procedure completePendingError (connIdx : Natural) is
   begin
      for i in pendingReqs'Range loop
         if (pendingReqs (i).kind = PENDING_CONNECT or
             pendingReqs (i).kind = PENDING_RECV or
             pendingReqs (i).kind = PENDING_OPEN) and
            pendingReqs (i).connIdx = connIdx
         then
            replyError (pendingReqs (i).sender);
            pendingReqs (i).kind := PENDING_NONE;
         end if;
      end loop;
   end completePendingError;

   ---------------------------------------------------------------------------
   --  executeActions - execute actions returned by TCPSession
   ---------------------------------------------------------------------------
   procedure executeActions (connIdx : Natural;
                             res     : TCPSession.Result;
                             pktBuf  : System.Address) is
   begin
      for i in 0 .. res.numActions - 1 loop
         case res.actions (i).kind is
            when TCPSession.ACT_SEND_SEGMENT =>
               if res.actions (i).dataLen > 0 and
                  pktBuf /= System.Null_Address
               then
                  sendTCPSegment (tcpConns (connIdx),
                                  res.actions (i).flags,
                                  res.actions (i).seqNum,
                                  res.actions (i).ackNum,
                                  pktBuf +
                                     Storage_Offset (
                                        res.actions (i).dataOff),
                                  res.actions (i).dataLen);
               else
                  sendTCPSegment (tcpConns (connIdx),
                                  res.actions (i).flags,
                                  res.actions (i).seqNum,
                                  res.actions (i).ackNum,
                                  System.Null_Address, 0);
               end if;

            when TCPSession.ACT_NOTIFY_ESTABLISHED =>
               debugPrint ("TCP: ESTABLISHED with ");
               printIP (tcpConns (connIdx).remoteIP);
               debugPrint (":");
               printDec (Unsigned_32 (tcpConns (connIdx).remotePort));
               debugPrint ("" & LF);
               completePendingConnect (connIdx);

            when TCPSession.ACT_NOTIFY_DATA =>
               debugPrint ("TCP: received ");
               printDec (Unsigned_32 (res.actions (i).dataLen));
               debugPrint (" bytes from ");
               printIP (tcpConns (connIdx).remoteIP);
               debugPrint (":");
               printDec (Unsigned_32 (tcpConns (connIdx).remotePort));
               debugPrint ("" & LF);

               --  DNS/TCP response: parse it (skip 2-byte length prefix)
               if tcpConns (connIdx).remotePort = 53 and
                  res.actions (i).dataLen > 2 and
                  pktBuf /= System.Null_Address
               then
                  debugPrint ("DNS/TCP: ");
                  handleDNSResponse
                     (pktBuf +
                        Storage_Offset (res.actions (i).dataOff + 2),
                      res.actions (i).dataLen - 2);
               end if;

               bufferReceived (connIdx, pktBuf,
                               res.actions (i).dataOff,
                               res.actions (i).dataLen);
               if hasPendingRecv (connIdx) then
                  completePendingBuffered (connIdx);
               end if;

            when TCPSession.ACT_NOTIFY_CLOSED =>
               debugPrint ("TCP: connection closed" & LF);
               completePendingRecvEOF (connIdx);

            when TCPSession.ACT_NOTIFY_ERROR =>
               debugPrint ("TCP: RST from ");
               printIP (tcpConns (connIdx).remoteIP);
               debugPrint ("" & LF);
               completePendingError (connIdx);

            when TCPSession.ACT_NONE =>
               null;
         end case;
      end loop;
   end executeActions;

   ---------------------------------------------------------------------------
   --  handleTCP - parse TCP segment via RecordFlux, drive state machine
   ---------------------------------------------------------------------------
   procedure handleTCP (pktBuf     : System.Address;
                        ipOff      : Natural;
                        ipHdrLen   : Natural;
                        srcIP      : Net.IPv4Address;
                        srcMAC     : Net.MACAddress;
                        totalIPLen : Natural) is
      pragma Unreferenced (srcMAC);
      use Net.RFLX_Builtin_Types;
      tcpOff : constant Natural := ipOff + ipHdrLen;
      tcpLen : constant Natural := totalIPLen - ipHdrLen;

      tcpBuf : aliased Bytes (1 .. Index (tcpLen))
         with Import, Address => pktBuf + Storage_Offset (tcpOff);
      bufPtr : Bytes_Ptr := tcpBuf'Unrestricted_Access;
      ctx    : Net.TCP.Segment.Context;

      seg     : TCPSession.SegmentInfo;
      connIdx : Integer := -1;
      res     : TCPSession.Result;
      dataLen : Natural;
   begin
      if tcpLen < 20 then
         return;
      end if;

      Net.TCP.Segment.Initialize
         (ctx, bufPtr,
          Segment_Length => Net.TCP.Segment_Length (tcpLen),
          Written_Last  => Net.RFLX_Types.Bit_Length (tcpLen) * 8);
      Net.TCP.Segment.Verify_Message (ctx);

      if not Net.TCP.Segment.Well_Formed_Message (ctx) then
         debugPrint ("TCP: malformed segment" & LF);
         Net.TCP.Segment.Take_Buffer (ctx, bufPtr);
         return;
      end if;

      seg.srcIP   := srcIP;
      seg.srcPort := Unsigned_16 (
                        Net.TCP.Segment.Get_Source_Port (ctx));
      seg.dstPort := Unsigned_16 (
                        Net.TCP.Segment.Get_Destination_Port (ctx));
      seg.seqNum  := Unsigned_32 (
                        Net.TCP.Segment.Get_Sequence_Number (ctx));
      seg.ackNum  := Unsigned_32 (
                        Net.TCP.Segment.Get_Acknowledgment_Number (ctx));
      seg.flagSYN := Net.TCP.Segment.Get_SYN (ctx);
      seg.flagACK := Net.TCP.Segment.Get_ACK (ctx);
      seg.flagFIN := Net.TCP.Segment.Get_FIN (ctx);
      seg.flagRST := Net.TCP.Segment.Get_RST (ctx);
      seg.winSize := Unsigned_16 (Net.TCP.Segment.Get_Window (ctx));

      --  Compute data length and offset from segment
      declare
         doff : constant Natural :=
            Natural (Net.TCP.Segment.Get_Data_Offset (ctx)) * 4;
      begin
         if tcpLen > doff then
            dataLen := tcpLen - doff;
         else
            dataLen := 0;
         end if;
         seg.dataLen := dataLen;
         seg.dataOff := tcpOff + doff;
      end;

      Net.TCP.Segment.Take_Buffer (ctx, bufPtr);

      --  Drive the state machine
      TCPSession.onSegmentIn (tcpConns, seg, connIdx, res);

      if connIdx < 0 then
         return;
      end if;

      --  Execute returned actions
      executeActions (connIdx, res, pktBuf);
   end handleTCP;

   ---------------------------------------------------------------------------
   --  handleARP
   ---------------------------------------------------------------------------
   procedure handleARP (pktBuf : System.Address; pktLen : Natural) is
      oper      : Unsigned_16;
      senderMAC : Net.MACAddress;
      senderIP  : Net.IPv4Address;
      targetIP  : Net.IPv4Address;
   begin
      if pktLen < 42 then
         return;
      end if;

      oper := Net.getU16BE (pktBuf, 20);
      Net.getMAC (pktBuf, 22, senderMAC);
      Net.getIP  (pktBuf, 28, senderIP);
      Net.getIP  (pktBuf, 38, targetIP);

      Net.arpUpdate (interfaces (0).arpCache, senderIP, senderMAC);

      if oper = Net.ARP_REQUEST then
         debugPrint ("ARP: request from ");
         printIP (senderIP);
         debugPrint (" for ");
         printIP (targetIP);
         debugPrint ("" & LF);

         if findInterfaceForIP (targetIP) >= 0 then
            sendARPReply (senderMAC, senderIP);
         end if;

      elsif oper = Net.ARP_REPLY then
         debugPrint ("ARP: reply from ");
         printIP (senderIP);
         debugPrint (" [");
         printMACAddr (senderMAC);
         debugPrint ("]" & LF);

         --  If this is from our gateway, record the gateway MAC
         for i in 0 .. numIfaces - 1 loop
            if interfaces (i).gateway = senderIP then
               interfaces (i).gwMAC := senderMAC;
               debugPrint ("ARP: gateway MAC resolved" & LF);
            end if;
         end loop;
      end if;
   end handleARP;

   ---------------------------------------------------------------------------
   --  sendICMPReply
   ---------------------------------------------------------------------------
   procedure sendICMPReply (srcIP   : Net.IPv4Address;
                            srcMAC  : Net.MACAddress;
                            pktBuf  : System.Address;
                            icmpOff : Natural;
                            icmpLen : Natural) is
      frameLen : constant Natural := 14 + 20 + icmpLen;
      frame    : array (0 .. frameLen - 1) of Unsigned_8;
      fAddr    : constant System.Address := frame'Address;
      ipTotal  : constant Unsigned_16 := Unsigned_16 (20 + icmpLen);
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      Net.putMAC   (fAddr, 0,  srcMAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      Net.putU8    (fAddr, 14, 16#45#);
      Net.putU8    (fAddr, 15, 0);
      Net.putU16BE (fAddr, 16, ipTotal);
      Net.putU16BE (fAddr, 18, 16#0001#);
      Net.putU16BE (fAddr, 20, 0);
      Net.putU8    (fAddr, 22, 64);
      Net.putU8    (fAddr, 23, Net.PROTO_ICMP);
      Net.putU16BE (fAddr, 24, 0);
      Net.putIP    (fAddr, 26, interfaces (0).ipv4);
      Net.putIP    (fAddr, 30, srcIP);

      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.internetChecksum (fAddr + 14, 20);
         Net.putU16BE (fAddr, 24, cksum);
      end;

      --  Copy ICMP data from request, change type to reply
      declare
         srcData : array (0 .. icmpLen - 1) of Unsigned_8 with
            Import, Address => pktBuf + Storage_Offset (icmpOff);
         dstData : array (0 .. icmpLen - 1) of Unsigned_8 with
            Import, Address => fAddr + 34;
      begin
         for i in srcData'Range loop
            dstData (i) := srcData (i);
         end loop;
      end;

      Net.putU8    (fAddr, 34, Net.ICMP_ECHO_REPLY);
      Net.putU16BE (fAddr, 36, 0);

      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.internetChecksum (fAddr + 34, icmpLen);
         Net.putU16BE (fAddr, 36, cksum);
      end;

      sendFrame (fAddr, frameLen);
   end sendICMPReply;

   ---------------------------------------------------------------------------
   --  handleICMP
   ---------------------------------------------------------------------------
   procedure handleICMP (pktBuf     : System.Address;
                         ipOff      : Natural;
                         ipHdrLen   : Natural;
                         srcIP      : Net.IPv4Address;
                         srcMAC     : Net.MACAddress;
                         totalIPLen : Natural) is
      icmpOff  : constant Natural := ipOff + ipHdrLen;
      icmpLen  : constant Natural := totalIPLen - ipHdrLen;
      icmpType : Unsigned_8;
      icmpSeq  : Unsigned_16;
   begin
      if icmpLen < 8 then
         return;
      end if;

      icmpType := Net.getU8 (pktBuf, icmpOff);

      if icmpType = Net.ICMP_ECHO_REQUEST then
         debugPrint ("ICMP: echo request from ");
         printIP (srcIP);
         debugPrint ("" & LF);
         sendICMPReply (srcIP, srcMAC, pktBuf, icmpOff, icmpLen);

      elsif icmpType = Net.ICMP_ECHO_REPLY then
         icmpSeq := Net.getU16BE (pktBuf, icmpOff + 6);
         debugPrint ("ICMP: echo reply from ");
         printIP (srcIP);
         debugPrint (" seq=");
         printDec (Unsigned_32 (icmpSeq));
         debugPrint ("" & LF);

         --  Check identifier (must be our 0xCB17)
         declare
            icmpId : constant Unsigned_16 :=
               Net.getU16BE (pktBuf, icmpOff + 4);
            nowMs  : constant Unsigned_64 :=
               syscall (SYSCALL_GETTIME);
         begin
            if icmpId = 16#CB17# then
               declare
                  matched : Boolean := False;
               begin
                  for i in pendingReqs'Range loop
                     if pendingReqs (i).kind = PENDING_PING and
                        pendingReqs (i).txid = icmpSeq
                     then
                        declare
                           sendTs : constant Unsigned_64 := Unsigned_64 (
                              To_Integer (pendingReqs (i).bufAddr));
                           rtt : Unsigned_64 := 0;
                           srcPacked : constant Unsigned_64 :=
                              Net.packIPv4 (srcIP);
                           replyMsg : constant Message :=
                             (tag      => (label  => REPLY_OK,
                                           length => 3,
                                           flags  => 0,
                                           badge  => 0),
                              capBadge => 0,
                              words    => (0 => Unsigned_64 (icmpSeq),
                                           1 => srcPacked,
                                           2 => (if nowMs > sendTs
                                                 then nowMs - sendTs
                                                 else 0),
                                           3 => 0));
                           ignore : Unsigned_64;
                        begin
                           ignore := reply (pendingReqs (i).sender,
                                            replyMsg);
                        end;
                        pendingReqs (i).kind := PENDING_NONE;
                        matched := True;
                        exit;
                     end if;
                  end loop;
                  if not matched then
                     debugPrint ("ICMP: unmatched reply seq=");
                     printDec (Unsigned_32 (icmpSeq));
                     debugPrint (" pending=[");
                     for i in pendingReqs'Range loop
                        printDec (Unsigned_32 (
                           PendingKind'Pos (pendingReqs (i).kind)));
                        if i < pendingReqs'Last then
                           debugPrint (",");
                        end if;
                     end loop;
                     debugPrint ("]" & LF);
                  end if;
               end;
            else
               debugPrint ("ICMP: reply wrong id=");
               printDec (Unsigned_32 (icmpId));
               debugPrint (" (expected CB17)" & LF);
            end if;
         end;
      end if;
   end handleICMP;

   ---------------------------------------------------------------------------
   --  handleIPv4
   ---------------------------------------------------------------------------
   procedure handleIPv4 (pktBuf : System.Address; pktLen : Natural) is
      ipOff    : constant Natural := 14;
      verIHL   : Unsigned_8;
      ipHdrLen : Natural;
      proto    : Unsigned_8;
      srcIP    : Net.IPv4Address;
      dstIP    : Net.IPv4Address;
      srcMAC   : Net.MACAddress;
      totalLen : Unsigned_16;
   begin
      if pktLen < 34 then
         return;
      end if;

      verIHL   := Net.getU8 (pktBuf, ipOff);
      ipHdrLen := Natural (verIHL and 16#0F#) * 4;

      if ipHdrLen < 20 or else pktLen < 14 + ipHdrLen then
         return;
      end if;

      totalLen := Net.getU16BE (pktBuf, ipOff + 2);

      --  Clamp totalLen to actual received data (prevents OOB read from
      --  a crafted IP header claiming more data than actually present).
      if Natural (totalLen) > pktLen - 14 then
         totalLen := Unsigned_16 (pktLen - 14);
      end if;

      proto := Net.getU8 (pktBuf, ipOff + 9);
      Net.getIP (pktBuf, ipOff + 12, srcIP);
      Net.getIP (pktBuf, ipOff + 16, dstIP);
      Net.getMAC (pktBuf, 6, srcMAC);

      if findInterfaceForIP (dstIP) < 0 then
         return;
      end if;

      if proto = Net.PROTO_ICMP then
         handleICMP (pktBuf, ipOff, ipHdrLen, srcIP, srcMAC,
                     Natural (totalLen));
      elsif proto = Net.PROTO_UDP then
         handleUDP (pktBuf, ipOff, ipHdrLen, srcIP,
                    Natural (totalLen));
      elsif proto = Net.PROTO_TCP then
         handleTCP (pktBuf, ipOff, ipHdrLen, srcIP, srcMAC,
                    Natural (totalLen));
      end if;
   end handleIPv4;

   ---------------------------------------------------------------------------
   --  handlePacket - dispatch on EtherType
   ---------------------------------------------------------------------------
   procedure handlePacket (pktBuf : System.Address; pktLen : Natural) is
      etherType : Unsigned_16;
   begin
      if pktLen < 14 then
         return;
      end if;

      etherType := Net.getU16BE (pktBuf, 12);

      if etherType = Net.ETHERTYPE_ARP then
         handleARP (pktBuf, pktLen);
      elsif etherType = Net.ETHERTYPE_IPV4 then
         handleIPv4 (pktBuf, pktLen);
      end if;
   end handlePacket;

   --  State machine removed: netmgr now handles IP configuration.
   --  Interface goes IF_DOWN -> IF_UP via OP_NET_CONFIGURE.

   ---------------------------------------------------------------------------
   --  findAppChannel - find or allocate app channel for a sender PID
   ---------------------------------------------------------------------------
   function findAppChannel (pid : ProcessID) return Integer is
   begin
      for i in appChannels'Range loop
         if appChannels (i).pid = pid then
            return i;
         end if;
      end loop;
      return -1;
   end findAppChannel;

   ---------------------------------------------------------------------------
   --  allocAppChannel - allocate a new app channel slot
   ---------------------------------------------------------------------------
   function allocAppChannel (pid     : ProcessID;
                             bufAddr : System.Address;
                             grantId : Unsigned_64;
                             bufSize : Natural) return Integer is
   begin
      for i in appChannels'Range loop
         if appChannels (i).pid = NO_PROCESS then
            appChannels (i) :=
               (pid     => pid,
                bufAddr => bufAddr,
                grantId => grantId,
                bufSize => bufSize);
            return i;
         end if;
      end loop;
      return -1;
   end allocAppChannel;

   ---------------------------------------------------------------------------
   --  allocNetChannel - allocate a new NetChannel slot, return index or -1
   ---------------------------------------------------------------------------
   function allocNetChannel return Integer is
   begin
      for i in channels'Range loop
         if channels (i).kind = CHANNEL_NONE then
            return i;
         end if;
      end loop;
      return -1;
   end allocNetChannel;

   ---------------------------------------------------------------------------
   --  Scheme parser types and procedure
   --
   --  Parses "@net:<proto>:<host>:<port>" from raw bytes at a given address.
   ---------------------------------------------------------------------------
   MAX_HOSTNAME_LEN : constant := 64;

   type ParsedScheme is record
      valid      : Boolean := False;
      proto      : Unsigned_8 := 0;
      hostname   : String (1 .. MAX_HOSTNAME_LEN);
      hostLen    : Natural := 0;
      port       : Unsigned_16 := 0;
      isIPLiteral : Boolean := False;
   end record;

   procedure parseNetScheme (addr   : System.Address;
                             len    : Natural;
                             result : out ParsedScheme) is
      raw : array (0 .. len - 1) of Unsigned_8 with
         Import, Address => addr;
      pos : Natural := 0;
      ch  : Unsigned_8;

      --  Skip a known prefix string, return True if matched
      function skipPrefix (prefix : String) return Boolean is
      begin
         if len - pos < prefix'Length then
            return False;
         end if;
         for i in prefix'Range loop
            if raw (pos + i - prefix'First) /=
               Unsigned_8 (Character'Pos (prefix (i)))
            then
               return False;
            end if;
         end loop;
         pos := pos + prefix'Length;
         return True;
      end skipPrefix;

      --  Read until ':' or end, return as string
      procedure readToken (buf : out String; tLen : out Natural) is
      begin
         tLen := 0;
         while pos < len loop
            ch := raw (pos);
            exit when ch = Unsigned_8 (Character'Pos (':'));
            if tLen < buf'Length then
               tLen := tLen + 1;
               buf (buf'First + tLen - 1) :=
                  Character'Val (Natural (ch));
            end if;
            pos := pos + 1;
         end loop;
         --  Skip the ':' delimiter
         if pos < len and then
            raw (pos) = Unsigned_8 (Character'Pos (':'))
         then
            pos := pos + 1;
         end if;
      end readToken;

      protoStr : String (1 .. 4);
      protoLen : Natural := 0;
      portStr  : String (1 .. 6);
      portLen  : Natural := 0;
      portVal  : Natural := 0;
      allDigitsAndDots : Boolean;
   begin
      result := (valid => False, proto => 0,
                 hostname => (others => ' '), hostLen => 0,
                 port => 0, isIPLiteral => False);

      --  Must start with "@net:"
      if not skipPrefix ("@net:") then
         return;
      end if;

      --  Protocol token (tcp, udp) — char-by-char to avoid memcmp
      readToken (protoStr, protoLen);
      if protoLen = 3 and then
         protoStr (1) = 't' and then
         protoStr (2) = 'c' and then
         protoStr (3) = 'p'
      then
         result.proto := Net.PROTO_TCP;
      elsif protoLen = 3 and then
         protoStr (1) = 'u' and then
         protoStr (2) = 'd' and then
         protoStr (3) = 'p'
      then
         result.proto := Net.PROTO_UDP;
      else
         return;
      end if;

      --  Hostname
      readToken (result.hostname, result.hostLen);
      if result.hostLen = 0 then
         return;
      end if;

      --  Port
      readToken (portStr, portLen);
      if portLen = 0 then
         return;
      end if;
      for i in 1 .. portLen loop
         if portStr (i) < '0' or portStr (i) > '9' then
            return;
         end if;
         portVal := portVal * 10 +
            (Character'Pos (portStr (i)) - Character'Pos ('0'));
      end loop;
      if portVal > 65535 then
         return;
      end if;
      result.port := Unsigned_16 (portVal);

      --  Detect IP literal (all digits and dots)
      allDigitsAndDots := True;
      for i in 1 .. result.hostLen loop
         if not ((result.hostname (i) >= '0' and
                  result.hostname (i) <= '9') or
                 result.hostname (i) = '.')
         then
            allDigitsAndDots := False;
            exit;
         end if;
      end loop;
      result.isIPLiteral := allDigitsAndDots;
      result.valid := True;
   end parseNetScheme;

   ---------------------------------------------------------------------------
   --  parseIPLiteral - parse "A.B.C.D" string into IPv4Address
   ---------------------------------------------------------------------------
   procedure parseIPLiteral (s   : String;
                             len : Natural;
                             ip  : out Net.IPv4Address;
                             ok  : out Boolean) is
      octet : Natural := 0;
      idx   : Natural := 0;
   begin
      ip := (others => 0);
      ok := False;
      for i in s'First .. s'First + len - 1 loop
         if s (i) = '.' then
            if idx > 3 or octet > 255 then
               return;
            end if;
            ip (idx) := Unsigned_8 (octet);
            idx := idx + 1;
            octet := 0;
         elsif s (i) >= '0' and s (i) <= '9' then
            octet := octet * 10 +
               (Character'Pos (s (i)) - Character'Pos ('0'));
         else
            return;
         end if;
      end loop;
      if idx = 3 and octet <= 255 then
         ip (3) := Unsigned_8 (octet);
         ok := True;
      end if;
   end parseIPLiteral;

   ---------------------------------------------------------------------------
   --  addPending - store a pending request in the first free slot
   ---------------------------------------------------------------------------
   function addPending (req : PendingRequest) return Boolean is
      ignore : Unsigned_64;
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_NONE then
            pendingReqs (i) := req;
            -- Save reply cap from slot 63 to slot 16+i so the next
            -- receive() won't overwrite it.
            pendingReqs (i).replySlot := Unsigned_64 (16 + i);
            ignore := saveReplyCap (Unsigned_64 (16 + i));
            return True;
         end if;
      end loop;
      debugPrint ("PEND+: FULL, cannot add kind=");
      printDec (Unsigned_32 (PendingKind'Pos (req.kind)));
      debugPrint ("" & LF);
      return False;
   end addPending;

   ---------------------------------------------------------------------------
   --  replyError - send REPLY_ERR to a sender
   ---------------------------------------------------------------------------
   procedure replyError (to : ProcessID) is
      errMsg : constant Message :=
        (tag      => (label  => REPLY_ERR,
                      length => 0,
                      flags  => 0,
                      badge  => 0),
         capBadge => 0,
         words    => (others => 0));
      ignore : Unsigned_64;
   begin
      ignore := reply (to, errMsg);
   end replyError;

   ---------------------------------------------------------------------------
   --  replyOK - send REPLY_OK with word0 to a sender
   ---------------------------------------------------------------------------
   procedure replyOKWord (to : ProcessID; w0 : Unsigned_64) is
      okMsg : constant Message :=
        (tag      => (label  => REPLY_OK,
                      length => 1,
                      flags  => 0,
                      badge  => 0),
         capBadge => 0,
         words    => (0 => w0, others => 0));
      ignore : Unsigned_64;
   begin
      ignore := reply (to, okMsg);
   end replyOKWord;

   ---------------------------------------------------------------------------
   --  handleAppResolve - DNS A-record lookup for an app
   --
   --  Request: words(0..3) = hostname bytes (up to 32 chars),
   --           tag.length = hostname length
   --  Reply: deferred until DNS response arrives
   ---------------------------------------------------------------------------
   procedure handleAppResolve (snd : ProcessID; m : Message) is
      nameLen : constant Natural := Natural (m.tag.length);
      hostname : String (1 .. 32);
      txid : Unsigned_16;
      ok   : Boolean;
   begin
      if nameLen = 0 or nameLen > 32 then
         replyError (snd);
         return;
      end if;

      --  Gateway MAC must be resolved before we can send DNS queries
      if not gwMACResolved then
         debugPrint ("netstack: resolve rejected, gateway MAC unknown" & LF);
         replyError (snd);
         return;
      end if;

      --  Extract hostname from message words (packed as bytes)
      declare
         raw : array (0 .. 31) of Unsigned_8 with
            Import, Address => m.words'Address;
      begin
         for i in 0 .. nameLen - 1 loop
            hostname (i + 1) := Character'Val (Natural (raw (i)));
         end loop;
      end;

      txid := nextDnsTxid;
      nextDnsTxid := nextDnsTxid + 1;

      ok := addPending (
         (kind       => PENDING_RESOLVE,
          sender     => snd,
          connIdx    => -1,
          channelIdx => -1,
          bufAddr    => System.Null_Address,
          bufOff     => 0,
          maxLen     => 0,
          txid       => txid,
          dstPort    => 0,
          replySlot  => 0));

      if not ok then
         replyError (snd);
         return;
      end if;

      sendDNSQuery (hostname (1 .. nameLen), txid);
   end handleAppResolve;

   ---------------------------------------------------------------------------
   --  handleAppConnect - TCP connect for an app
   --
   --  Request: word0=IP(packed u32 BE), word1=port,
   --           word2=buffer size (bytes), word3=grant ID
   --  Reply: deferred until ESTABLISHED
   ---------------------------------------------------------------------------
   procedure handleAppConnect (snd : ProcessID; m : Message) is
      ipPacked : constant Unsigned_64 := m.words (0);
      port     : constant Unsigned_16 := Unsigned_16 (m.words (1));
      bufSize  : constant Natural := Natural (m.words (2));
      grantId  : constant Unsigned_64 := m.words (3);
      --  Compute grant-mapped address in our (netstack's) address space
      grantAddr : constant System.Address :=
         To_Address (GRANT_REGION_BASE +
                     Integer_Address (grantId) * GRANT_SLOT_SIZE);
      dstIP    : Net.IPv4Address;
      chIdx    : Integer;
      connIdx  : Integer;
      ok       : Boolean;
   begin
      if bufSize = 0 then
         replyError (snd);
         return;
      end if;

      --  Gateway MAC must be resolved for outgoing TCP SYN
      if not gwMACResolved then
         debugPrint ("netstack: connect rejected, gateway MAC unknown" & LF);
         replyError (snd);
         return;
      end if;

      dstIP (0) := Unsigned_8 (ipPacked and 16#FF#);
      dstIP (1) := Unsigned_8 (Shift_Right (ipPacked, 8) and 16#FF#);
      dstIP (2) := Unsigned_8 (Shift_Right (ipPacked, 16) and 16#FF#);
      dstIP (3) := Unsigned_8 (Shift_Right (ipPacked, 24) and 16#FF#);

      --  Register or find app channel (use grant-mapped address)
      chIdx := findAppChannel (snd);
      if chIdx < 0 then
         chIdx := allocAppChannel (snd, grantAddr, grantId, bufSize);
      end if;
      if chIdx < 0 then
         replyError (snd);
         return;
      end if;

      --  All traffic goes through gateway MAC
      connIdx := tcpConnect (dstIP, interfaces (0).gwMAC, port);
      if connIdx < 0 then
         replyError (snd);
         return;
      end if;

      ok := addPending (
         (kind       => PENDING_CONNECT,
          sender     => snd,
          connIdx    => connIdx,
          channelIdx => -1,
          bufAddr    => grantAddr,
          bufOff     => 0,
          maxLen     => 0,
          txid       => 0,
          dstPort    => 0,
          replySlot  => 0));

      if not ok then
         replyError (snd);
         return;
      end if;
      --  Reply deferred until TCP_ESTABLISHED
   end handleAppConnect;

   ---------------------------------------------------------------------------
   --  handleAppSend - send data on a TCP connection for an app
   --
   --  Request: word0=conn handle, word1=offset in grant buf, word2=length
   --  Reply: immediate REPLY_OK or REPLY_ERR
   ---------------------------------------------------------------------------
   procedure handleAppSend (snd : ProcessID; m : Message) is
      connHandle : constant Natural := Natural (m.words (0));
      offset     : constant Natural := Natural (m.words (1));
      len        : constant Natural := Natural (m.words (2));
      chIdx      : Integer;
      dataAddr   : System.Address;
   begin
      chIdx := findAppChannel (snd);
      if chIdx < 0 or connHandle > tcpConns'Last then
         replyError (snd);
         return;
      end if;

      if tcpConns (connHandle).state /= TCPSession.TCP_ESTABLISHED then
         replyError (snd);
         return;
      end if;

      --  Bounds check: offset + len must fit within app's grant buffer
      if offset > appChannels (chIdx).bufSize or
         len > appChannels (chIdx).bufSize - offset
      then
         replyError (snd);
         return;
      end if;

      dataAddr := appChannels (chIdx).bufAddr +
         Storage_Offset (offset);
      tcpSend (connHandle, dataAddr, len);
      replyOKWord (snd, Unsigned_64 (len));
   end handleAppSend;

   ---------------------------------------------------------------------------
   --  handleAppRecv - receive data on a TCP connection for an app
   --
   --  Request: word0=conn handle, word1=offset in grant buf, word2=max len
   --  Reply: deferred until data arrives or connection closes
   ---------------------------------------------------------------------------
   procedure handleAppRecv (snd : ProcessID; m : Message) is
      connHandle : constant Natural := Natural (m.words (0));
      offset     : constant Natural := Natural (m.words (1));
      maxLen     : constant Natural := Natural (m.words (2));
      chIdx      : Integer;
      ok         : Boolean;
   begin
      chIdx := findAppChannel (snd);
      if chIdx < 0 or connHandle > tcpConns'Last then
         replyError (snd);
         return;
      end if;

      --  Bounds check: offset + maxLen must fit within app's grant buffer
      if offset > appChannels (chIdx).bufSize or
         maxLen > appChannels (chIdx).bufSize - offset
      then
         replyError (snd);
         return;
      end if;

      if rxBuffers (connHandle).len > 0 then
         replyBuffered (snd, connHandle, appChannels (chIdx).bufAddr,
                        offset, maxLen);
         return;
      end if;

      --  If connection already closed, reply EOF immediately
      if tcpConns (connHandle).state = TCPSession.TCP_CLOSED or
         tcpConns (connHandle).state = TCPSession.TCP_CLOSE_WAIT or
         tcpConns (connHandle).state = TCPSession.TCP_LAST_ACK or
         tcpConns (connHandle).state = TCPSession.TCP_TIME_WAIT
      then
         declare
            eofMsg : constant Message :=
              (tag      => (label  => REPLY_EOF,
                            length => 0,
                            flags  => 0,
                            badge  => 0),
               capBadge => 0,
               words    => (others => 0));
            ignore : Unsigned_64;
         begin
            ignore := reply (snd, eofMsg);
         end;
         return;
      end if;

      ok := addPending (
         (kind       => PENDING_RECV,
          sender     => snd,
          connIdx    => connHandle,
          channelIdx => -1,
          bufAddr    => appChannels (chIdx).bufAddr,
          bufOff     => offset,
          maxLen     => maxLen,
          txid       => 0,
          dstPort    => 0,
          replySlot  => 0));

      if not ok then
         replyError (snd);
      end if;
      --  Reply deferred
   end handleAppRecv;

   ---------------------------------------------------------------------------
   --  handleAppClose - close a TCP connection for an app
   --
   --  Request: word0=conn handle
   --  Reply: immediate REPLY_OK
   ---------------------------------------------------------------------------
   procedure handleAppClose (snd : ProcessID; m : Message) is
      connHandle : constant Natural := Natural (m.words (0));
   begin
      if connHandle > tcpConns'Last then
         replyError (snd);
         return;
      end if;

      --  Complete any pending RECV for this connection with EOF
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_RECV and
            pendingReqs (i).connIdx = connHandle
         then
            declare
               eofMsg : constant Message :=
                 (tag      => (label  => REPLY_EOF,
                               length => 0,
                               flags  => 0,
                               badge  => 0),
                  capBadge => 0,
                  words    => (others => 0));
               ignore : Unsigned_64;
            begin
               ignore := reply (pendingReqs (i).sender, eofMsg);
            end;
            pendingReqs (i).kind := PENDING_NONE;
            exit;
         end if;
      end loop;

      tcpClose (connHandle);
      replyOKWord (snd, 0);
   end handleAppClose;

   ---------------------------------------------------------------------------
   --  handleNetOpen - open a network channel (DNS + connect in one call)
   --
   --  Request: tag.label=OP_NET_OPEN, tag.length=scheme string length,
   --           tag.flags=channel kind (0=client),
   --           words(0)=grant ID, words(1)=buffer size
   --  Scheme string is at offset 0 of the grant buffer.
   --  Reply: deferred until DNS+TCP handshake completes
   ---------------------------------------------------------------------------
   procedure handleNetOpen (snd : ProcessID; m : Message) is
      schemeLen : constant Natural := Natural (m.tag.length);
      grantId   : constant Unsigned_64 := m.words (0);
      bufSize   : constant Natural := Natural (m.words (1));
      grantAddr : constant System.Address :=
         To_Address (GRANT_REGION_BASE +
                     Integer_Address (grantId) * GRANT_SLOT_SIZE);
      scheme    : ParsedScheme;
      chIdx     : Integer;
      ok        : Boolean;
   begin
      if schemeLen = 0 or bufSize = 0 then
         replyError (snd);
         return;
      end if;

      if not gwMACResolved then
         debugPrint ("netstack: open rejected, gateway MAC unknown" & LF);
         replyError (snd);
         return;
      end if;

      --  Parse scheme string from grant buffer
      parseNetScheme (grantAddr, schemeLen, scheme);
      if not scheme.valid then
         debugPrint ("netstack: open: invalid scheme" & LF);
         replyError (snd);
         return;
      end if;

      --  Only TCP client channels for now
      if scheme.proto /= Net.PROTO_TCP then
         debugPrint ("netstack: open: only TCP supported" & LF);
         replyError (snd);
         return;
      end if;

      --  Allocate a channel slot
      chIdx := allocNetChannel;
      if chIdx < 0 then
         debugPrint ("netstack: open: no free channels" & LF);
         replyError (snd);
         return;
      end if;

      channels (chIdx) :=
         (kind       => CHANNEL_CLIENT,
          proto      => scheme.proto,
          pid        => snd,
          bufAddr    => grantAddr,
          grantId    => grantId,
          bufSize    => bufSize,
          connIdx    => -1,
          remoteIP   => (others => 0),
          remotePort => scheme.port,
          localPort  => 0);

      if scheme.isIPLiteral then
         --  Parse IP directly, skip DNS
         declare
            dstIP : Net.IPv4Address;
            ipOK  : Boolean;
            connIdx : Integer;
         begin
            parseIPLiteral (scheme.hostname, scheme.hostLen, dstIP, ipOK);
            if not ipOK then
               channels (chIdx).kind := CHANNEL_NONE;
               replyError (snd);
               return;
            end if;

            channels (chIdx).remoteIP := dstIP;
            connIdx := tcpConnect (dstIP, interfaces (0).gwMAC, scheme.port);
            if connIdx < 0 then
               channels (chIdx).kind := CHANNEL_NONE;
               replyError (snd);
               return;
            end if;
            channels (chIdx).connIdx := connIdx;

            ok := addPending (
               (kind       => PENDING_CONNECT,
                sender     => snd,
                connIdx    => connIdx,
                channelIdx => chIdx,
                bufAddr    => grantAddr,
                bufOff     => 0,
                maxLen     => 0,
                txid       => 0,
                dstPort    => scheme.port,
                replySlot  => 0));
            if not ok then
               channels (chIdx).kind := CHANNEL_NONE;
               replyError (snd);
               return;
            end if;
         end;
      else
         --  Need DNS resolution first
         declare
            txid : Unsigned_16;
         begin
            txid := nextDnsTxid;
            nextDnsTxid := nextDnsTxid + 1;

            ok := addPending (
               (kind       => PENDING_OPEN,
                sender     => snd,
                connIdx    => -1,
                channelIdx => chIdx,
                bufAddr    => grantAddr,
                bufOff     => 0,
                maxLen     => 0,
                txid       => txid,
                dstPort    => scheme.port,
                replySlot  => 0));
            if not ok then
               channels (chIdx).kind := CHANNEL_NONE;
               replyError (snd);
               return;
            end if;

            sendDNSQuery (scheme.hostname (1 .. scheme.hostLen), txid);
         end;
      end if;
      --  Reply deferred
   end handleNetOpen;

   ---------------------------------------------------------------------------
   --  handleNetWrite - send data on a channel
   --
   --  Request: words(0)=channel handle, words(1)=offset, words(2)=length
   --  Reply: immediate REPLY_OK or REPLY_ERR
   ---------------------------------------------------------------------------
   procedure handleNetWrite (snd : ProcessID; m : Message) is
      chHandle : constant Natural := Natural (m.words (0));
      offset   : constant Natural := Natural (m.words (1));
      len      : constant Natural := Natural (m.words (2));
      dataAddr : System.Address;
   begin
      if chHandle > channels'Last or else
         channels (chHandle).kind = CHANNEL_NONE or else
         channels (chHandle).pid /= snd
      then
         replyError (snd);
         return;
      end if;

      if channels (chHandle).connIdx < 0 or else
         channels (chHandle).connIdx > tcpConns'Last or else
         tcpConns (channels (chHandle).connIdx).state /=
            TCPSession.TCP_ESTABLISHED
      then
         replyError (snd);
         return;
      end if;

      if offset > channels (chHandle).bufSize or
         len > channels (chHandle).bufSize - offset
      then
         replyError (snd);
         return;
      end if;

      dataAddr := channels (chHandle).bufAddr +
         Storage_Offset (offset);
      tcpSend (channels (chHandle).connIdx, dataAddr, len);
      replyOKWord (snd, Unsigned_64 (len));
   end handleNetWrite;

   ---------------------------------------------------------------------------
   --  handleNetRead - receive data on a channel (deferred)
   --
   --  Request: words(0)=channel handle, words(1)=offset, words(2)=max len
   --  Reply: deferred until data arrives or connection closes
   ---------------------------------------------------------------------------
   procedure handleNetRead (snd : ProcessID; m : Message) is
      chHandle : constant Natural := Natural (m.words (0));
      offset   : constant Natural := Natural (m.words (1));
      maxLen   : constant Natural := Natural (m.words (2));
      ok       : Boolean;
   begin
      if chHandle > channels'Last or else
         channels (chHandle).kind = CHANNEL_NONE or else
         channels (chHandle).pid /= snd
      then
         replyError (snd);
         return;
      end if;

      if channels (chHandle).connIdx < 0 or else
         channels (chHandle).connIdx > tcpConns'Last
      then
         replyError (snd);
         return;
      end if;

      if offset > channels (chHandle).bufSize or
         maxLen > channels (chHandle).bufSize - offset
      then
         replyError (snd);
         return;
      end if;

      if rxBuffers (channels (chHandle).connIdx).len > 0 then
         replyBuffered (snd, channels (chHandle).connIdx,
                        channels (chHandle).bufAddr, offset, maxLen);
         return;
      end if;

      --  If connection already closed, reply EOF immediately
      declare
         st : constant TCPSession.TCPState :=
            tcpConns (channels (chHandle).connIdx).state;
      begin
         if st = TCPSession.TCP_CLOSED or
            st = TCPSession.TCP_CLOSE_WAIT or
            st = TCPSession.TCP_LAST_ACK or
            st = TCPSession.TCP_TIME_WAIT
         then
            declare
               eofMsg : constant Message :=
                 (tag      => (label  => REPLY_EOF,
                               length => 0,
                               flags  => 0,
                               badge  => 0),
                  capBadge => 0,
                  words    => (others => 0));
               ignore : Unsigned_64;
            begin
               ignore := reply (snd, eofMsg);
            end;
            return;
         end if;
      end;

      ok := addPending (
         (kind       => PENDING_RECV,
          sender     => snd,
          connIdx    => channels (chHandle).connIdx,
          channelIdx => chHandle,
          bufAddr    => channels (chHandle).bufAddr,
          bufOff     => offset,
          maxLen     => maxLen,
          txid       => 0,
          dstPort    => 0,
          replySlot  => 0));
      if not ok then
         replyError (snd);
      end if;
      --  Reply deferred
   end handleNetRead;

   ---------------------------------------------------------------------------
   --  handleNetShut - close a channel
   --
   --  Request: words(0)=channel handle
   --  Reply: immediate REPLY_OK
   ---------------------------------------------------------------------------
   procedure handleNetShut (snd : ProcessID; m : Message) is
      chHandle : constant Natural := Natural (m.words (0));
   begin
      if chHandle > channels'Last or else
         channels (chHandle).kind = CHANNEL_NONE or else
         channels (chHandle).pid /= snd
      then
         replyError (snd);
         return;
      end if;

      --  Complete all pending RECV for this connection with EOF
      if channels (chHandle).connIdx >= 0 then
         for i in pendingReqs'Range loop
            if pendingReqs (i).kind = PENDING_RECV and
               pendingReqs (i).connIdx = channels (chHandle).connIdx
            then
               declare
                  eofMsg : constant Message :=
                    (tag      => (label  => REPLY_EOF,
                                  length => 0,
                                  flags  => 0,
                                  badge  => 0),
                     capBadge => 0,
                     words    => (others => 0));
                  ignore : Unsigned_64;
               begin
                  ignore := reply (pendingReqs (i).sender, eofMsg);
               end;
               pendingReqs (i).kind := PENDING_NONE;
            end if;
         end loop;

         if channels (chHandle).connIdx <= tcpConns'Last then
            tcpClose (channels (chHandle).connIdx);
         end if;
      end if;

      --  Free the channel slot
      channels (chHandle).kind := CHANNEL_NONE;
      channels (chHandle).pid := NO_PROCESS;
      channels (chHandle).connIdx := -1;

      replyOKWord (snd, 0);
   end handleNetShut;

   ---------------------------------------------------------------------------
   --  handleAttach - process OP_NET_ATTACH from the driver
   --
   --  The driver sends us its PID; we allocate a grant buffer and reply
   --  with the grant ID + our MAC address packed into a u64.
   ---------------------------------------------------------------------------
   procedure handleAttach (sender : ProcessID) is
      ok    : Boolean;
      ifIdx : Natural;
   begin
      --  Allocate an interface slot for this driver
      if numIfaces >= MAX_INTERFACES then
         debugPrint ("netstack: too many interfaces" & LF);
         replyError (sender);
         return;
      end if;
      ifIdx := numIfaces;
      numIfaces := numIfaces + 1;
      interfaces (ifIdx).driverPID := sender;

      --  Allocate shared packet buffer if not yet done
      if interfaces (ifIdx).pktBuf = System.Null_Address then
         declare
            ret : Unsigned_64;
         begin
            ret := syscall (SYSCALL_SBRK, Unsigned_64 (PACKET_BUF_SIZE));
            if ret = Unsigned_64'Last then
               debugPrint ("netstack: sbrk failed for packet buffer" & LF);
               replyError (sender);
               numIfaces := numIfaces - 1;
               return;
            end if;
            interfaces (ifIdx).pktBuf := To_Address (Integer_Address (ret));
         end;

         --  Zero the buffer manually (avoid memset)
         declare
            buf : array (0 .. PACKET_BUF_SIZE - 1) of Unsigned_8 with
               Import, Address => interfaces (ifIdx).pktBuf;
         begin
            for i in buf'Range loop
               buf (i) := 0;
            end loop;
         end;
      end if;

      --  Create grant to the driver for our packet buffer
      createGrant (
         grantee   => interfaces (ifIdx).driverPID,
         localAddr => interfaces (ifIdx).pktBuf,
         numPages  => PACKET_BUF_PAGES,
         readWrite => True,
         grantId   => interfaces (ifIdx).pktGrant,
         success   => ok);

      if not ok then
         debugPrint ("netstack: createGrant failed" & LF);
         replyError (sender);
         numIfaces := numIfaces - 1;
         return;
      end if;

      debugPrint ("netstack: grant created, id=");
      printDec (Unsigned_32 (interfaces (ifIdx).pktGrant));
      debugPrint ("" & LF);

      --  Reply with grant ID and buffer size
      declare
         replyMsg : constant Message :=
           (tag      => (label  => REPLY_OK,
                         length => 2,
                         flags  => 0,
                         badge  => 0),
            capBadge => 0,
            words    => (0 => interfaces (ifIdx).pktGrant,
                         1 => Unsigned_64 (PACKET_BUF_SIZE),
                         others => 0));
         ignore : Unsigned_64;
      begin
         ignore := reply (sender, replyMsg);
      end;

      debugPrint ("netstack: attached to driver pid=");
      printDec (Unsigned_32 (interfaces (ifIdx).driverPID));
      debugPrint ("" & LF);
   end handleAttach;

   ---------------------------------------------------------------------------
   --  handleNetRX - process a received packet notification from the driver
   --
   --  The driver copied the packet into RX area of our grant buffer and
   --  sent the offset + length via sendEvent (non-blocking).
   ---------------------------------------------------------------------------
   procedure handleNetRX (msg : Message; sender : ProcessID) is
      offset : constant Unsigned_64 := msg.words (0);
      len    : constant Unsigned_64 := msg.words (1);
      pktBuf : System.Address;
      ifIdx  : Integer;
   begin
      --  Find the interface this RX came from
      ifIdx := findInterfaceByPID (sender);
      if ifIdx < 0 then
         return;
      end if;

      if interfaces (ifIdx).pktBuf = System.Null_Address then
         return;
      end if;

      if len < 14 or len > Unsigned_64 (PACKET_BUF_SIZE / 2) then
         return;
      end if;

      if offset + len > Unsigned_64 (PACKET_BUF_SIZE / 2) then
         return;
      end if;

      pktBuf := interfaces (ifIdx).pktBuf + Storage_Offset (offset);
      handlePacket (pktBuf, Natural (len));
   end handleNetRX;

   --  Main variables
   sender  : ProcessID;
   msg     : Message;
   found   : Boolean;

begin
   debugPrint ("netstack: starting..." & LF);

   --  Register as DRIVER_NETSTACK
   declare
      ret : Unsigned_64;
   begin
      ret := registerDriver (DRIVER_NETSTACK);
      debugPrint ("netstack: registered as driver ");
      printDec (Unsigned_32 (ret));
      debugPrint ("" & LF);
   end;

   --  Query the driver's MAC address via capCall to CAP_SLOT_NET_DRV
   --  We do this after the driver starts and sends us OP_NET_ATTACH.
   --  For now, just enter the message loop and wait.

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      rdyIgnore : MessageTag;
   begin
      rdyIgnore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, badge => 0),
          capBadge => 0,
          words    => (others => 0)));
   end;

   debugPrint ("netstack: waiting for driver attach..." & LF);

   --  Message loop
   --
   --  TX uses fire-and-forget capSubmit (no reply from driver), so no
   --  deadlock risk.  If the driver's single-slot mailbox is full when we
   --  submit, the frame is buffered in deferredTX and retried between
   --  message dispatches.
   loop
      found := False;

      --  1. Try non-blocking service-request receive to keep responsiveness.
      --     Network driver events and completions stay on their own lanes.
      Poll_Service_Request (sender, msg, found);

      --  2. If no message but deferred TX pending, try to flush one frame
      if not found and deferredCount > 0 then
         if not flushOneDeferredTX then
            declare
               ignore : Unsigned_64;
            begin
               ignore := syscall (SYSCALL_SLEEP, 1);
            end;
         end if;

      --  3. If no message, check for expired pings before blocking
      elsif not found then
         declare
            nowMs : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
            PING_TIMEOUT_MS : constant Unsigned_64 := 5000;
         begin
            --  Expire any timed-out pings before blocking
            for i in pendingReqs'Range loop
               if pendingReqs (i).kind = PENDING_PING then
                  declare
                     sendTs : constant Unsigned_64 := Unsigned_64 (
                        To_Integer (pendingReqs (i).bufAddr));
                  begin
                     if nowMs > sendTs and
                        nowMs - sendTs > PING_TIMEOUT_MS
                     then
                        replyError (pendingReqs (i).sender);
                        pendingReqs (i).kind := PENDING_NONE;
                     end if;
                  end;
               end if;
            end loop;

            --  Block until next message. ICMP replies arrive as
            --  OP_NET_RX IPC from the driver, so we wake immediately.
            receive (sender, msg);
            found := True;
         end;
      end if;

      --  4. Dispatch message
      if found then
         case msg.tag.label is
            when OP_NET_ATTACH =>
               handleAttach (sender);

               --  Extract MAC from attach message into latest interface
               if numIfaces > 0 then
                  declare
                     macPacked : constant Unsigned_64 := msg.words (0);
                     idx : constant Natural := numIfaces - 1;
                  begin
                     interfaces (idx).mac (0) :=
                        Unsigned_8 (macPacked and 16#FF#);
                     interfaces (idx).mac (1) := Unsigned_8 (
                        Shift_Right (macPacked, 8) and 16#FF#);
                     interfaces (idx).mac (2) := Unsigned_8 (
                        Shift_Right (macPacked, 16) and 16#FF#);
                     interfaces (idx).mac (3) := Unsigned_8 (
                        Shift_Right (macPacked, 24) and 16#FF#);
                     interfaces (idx).mac (4) := Unsigned_8 (
                        Shift_Right (macPacked, 32) and 16#FF#);
                     interfaces (idx).mac (5) := Unsigned_8 (
                        Shift_Right (macPacked, 40) and 16#FF#);

                     debugPrint ("netstack: attached, MAC=");
                     printMACAddr (interfaces (idx).mac);
                     debugPrint ("" & LF);
                  end;
               end if;

            when OP_NET_RX =>
               handleNetRX (msg, sender);

               --  Reply to the driver so it unblocks
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
                  ignore := reply (sender, replyMsg);
               end;

            --  Network management IPC (from netmgr)
            when OP_NET_CONFIGURE =>
               declare
                  cfgIfIdx : constant Natural :=
                     Natural (msg.words (0));
                  addrPacked : constant Unsigned_64 := msg.words (1);
                  maskPacked : constant Unsigned_64 := msg.words (2);
                  gwPacked   : constant Unsigned_64 := msg.words (3);
               begin
                  if cfgIfIdx < numIfaces then
                     interfaces (cfgIfIdx).ipv4 :=
                        Net.unpackIPv4 (addrPacked);
                     interfaces (cfgIfIdx).netmask :=
                        Net.unpackIPv4 (maskPacked);
                     interfaces (cfgIfIdx).gateway :=
                        Net.unpackIPv4 (gwPacked);
                     interfaces (cfgIfIdx).state := IF_UP;

                     --  Install connected + default routes
                     installConnectedRoute (cfgIfIdx);

                     --  Send gratuitous ARP and ARP for gateway
                     sendGratuitousARP;
                     if interfaces (cfgIfIdx).gateway /=
                        Net.IPv4Address'(others => 0)
                     then
                        sendARPRequest (interfaces (cfgIfIdx).gateway);
                     end if;

                     debugPrint ("netstack: if");
                     printDec (Unsigned_32 (cfgIfIdx));
                     debugPrint (" configured: ");
                     printIP (interfaces (cfgIfIdx).ipv4);
                     debugPrint ("" & LF);

                     replyOKWord (sender, 0);
                  else
                     replyError (sender);
                  end if;
               end;

            when OP_NET_SET_DNS =>
               primaryDNS := Net.unpackIPv4 (msg.words (0));
               if msg.words (1) /= 0 then
                  secondaryDNS := Net.unpackIPv4 (msg.words (1));
               end if;
               debugPrint ("netstack: DNS set to ");
               printIP (primaryDNS);
               debugPrint ("" & LF);
               replyOKWord (sender, 0);

            when OP_NET_LIST_IF =>
               replyOKWord (sender, Unsigned_64 (numIfaces));

            when OP_NET_IF_DETAIL =>
               declare
                  reqIfIdx : constant Natural :=
                     Natural (msg.words (0));
               begin
                  if reqIfIdx < numIfaces then
                     declare
                        ifc : InterfaceRecord renames
                           interfaces (reqIfIdx);
                        stateVal : Unsigned_64 :=
                           (case ifc.state is
                               when IF_DOWN => 0,
                               when IF_UP => 1,
                               when IF_CONFIGURING => 2);
                        ipPacked : constant Unsigned_64 :=
                           Net.packIPv4 (ifc.ipv4);
                        maskPacked : constant Unsigned_64 :=
                           Net.packIPv4 (ifc.netmask);
                        gwPacked : constant Unsigned_64 :=
                           Net.packIPv4 (ifc.gateway);
                        macPacked : constant Unsigned_64 :=
                           Unsigned_64 (ifc.mac (0)) or
                           Shift_Left (Unsigned_64 (ifc.mac (1)), 8) or
                           Shift_Left (Unsigned_64 (ifc.mac (2)), 16) or
                           Shift_Left (Unsigned_64 (ifc.mac (3)), 24) or
                           Shift_Left (Unsigned_64 (ifc.mac (4)), 32) or
                           Shift_Left (Unsigned_64 (ifc.mac (5)), 40);
                        dnsPri : constant Unsigned_64 :=
                           Net.packIPv4 (primaryDNS);
                        dnsSec : constant Unsigned_64 :=
                           Net.packIPv4 (secondaryDNS);
                        detailMsg : constant Message :=
                          (tag      => (label  => REPLY_OK,
                                        length => 4,
                                        flags  => 0,
                                        badge  => 0),
                           capBadge => 0,
                           words    => (
                              0 => ipPacked or
                                   Shift_Left (stateVal, 32),
                              1 => maskPacked or
                                   Shift_Left (gwPacked, 32),
                              2 => macPacked,
                              3 => dnsPri or
                                   Shift_Left (dnsSec, 32)));
                        ignore : Unsigned_64;
                     begin
                        ignore := reply (sender, detailMsg);
                     end;
                  else
                     replyError (sender);
                  end if;
               end;

            when OP_NET_ROUTE_LIST =>
               declare
                  startIdx : constant Natural :=
                     Natural (msg.words (0));
                  total  : Natural := 0;
                  packed : array (0 .. 3) of Unsigned_64 := (others => 0);
                  slot   : Natural := 0;
                  nextStart : Natural := 0;
               begin
                  --  Count total active routes
                  for i in routeTable'Range loop
                     if routeTable (i).active then
                        total := total + 1;
                     end if;
                  end loop;

                  --  Pack up to 2 routes starting from startIdx
                  declare
                     seen : Natural := 0;
                  begin
                     for i in routeTable'Range loop
                        if routeTable (i).active then
                           if seen >= startIdx and slot < 2 then
                              packed (slot * 2) :=
                                 Net.packIPv4 (routeTable (i).dest) or
                                 Shift_Left (Unsigned_64 (
                                    routeTable (i).prefix), 32) or
                                 Shift_Left (Unsigned_64 (
                                    routeTable (i).ifIdx), 40) or
                                 Shift_Left (Unsigned_64 (
                                    routeTable (i).metric), 48);
                              packed (slot * 2 + 1) :=
                                 Net.packIPv4 (routeTable (i).gateway);
                              slot := slot + 1;
                              nextStart := seen + 1;
                           end if;
                           seen := seen + 1;
                        end if;
                     end loop;
                  end;

                  declare
                     routeReply : constant Message :=
                       (tag      => (label  => REPLY_OK,
                                     length => Unsigned_8 (total),
                                     flags  => Unsigned_8 (nextStart),
                                     badge  => 0),
                        capBadge => 0,
                        words    => (0 => packed (0),
                                     1 => packed (1),
                                     2 => packed (2),
                                     3 => packed (3)));
                     ignore : Unsigned_64;
                  begin
                     ignore := reply (sender, routeReply);
                  end;
               end;

            when OP_NET_PING =>
               declare
                  dstIP : constant Net.IPv4Address :=
                     Net.unpackIPv4 (msg.words (0));
                  seq : constant Unsigned_16 :=
                     Unsigned_16 (msg.words (1) and 16#FFFF#);
                  sendTs : constant Unsigned_64 := msg.words (2);
                  isLoopback : Boolean := False;
                  rIfIdx  : Integer;
                  nextHop : Net.IPv4Address;
                  dstMAC  : Net.MACAddress;
                  ok : Boolean;
               begin
                  --  Loopback: 127.0.0.0/8 or own interface IP
                  if dstIP (0) = 127 then
                     isLoopback := True;
                  else
                     for i in 0 .. numIfaces - 1 loop
                        if interfaces (i).ipv4 = dstIP and
                           interfaces (i).state = IF_UP
                        then
                           isLoopback := True;
                           exit;
                        end if;
                     end loop;
                  end if;

                  if isLoopback then
                     --  Immediate reply with RTT=0
                     declare
                        nowMs : constant Unsigned_64 :=
                           syscall (SYSCALL_GETTIME);
                        rtt   : constant Unsigned_64 :=
                           (if nowMs >= sendTs then nowMs - sendTs
                            else 0);
                        loopReply : Message :=
                          (tag      => (label  => REPLY_OK,
                                        length => 3,
                                        flags  => 0,
                                        badge  => 0),
                           capBadge => 0,
                           words    => (0 => Unsigned_64 (seq),
                                        1 => msg.words (0),
                                        2 => rtt,
                                        others => 0));
                        ignore : Unsigned_64;
                     begin
                        ignore := reply (sender, loopReply);
                     end;
                  else
                     routeLookup (dstIP, rIfIdx, nextHop);
                     if rIfIdx < 0 then
                        replyError (sender);
                     else
                        --  Resolve MAC: ARP cache, fall back to gwMAC
                        if not Net.arpLookup (
                           interfaces (rIfIdx).arpCache, nextHop,
                           dstMAC)
                        then
                           dstMAC := interfaces (rIfIdx).gwMAC;
                        end if;

                        ok := addPending (
                           (kind       => PENDING_PING,
                            sender     => sender,
                            connIdx    => -1,
                            channelIdx => -1,
                            bufAddr    => To_Address (
                               Integer_Address (sendTs)),
                            bufOff     => 0,
                            maxLen     => 0,
                            txid       => seq,
                            dstPort    => 0,
                            replySlot  => 0));

                        if ok then
                           sendICMPEchoRequest (dstIP, dstMAC, seq,
                                                rIfIdx);
                        else
                           replyError (sender);
                        end if;
                     end if;
                  end if;
               end;

            when OP_NET_ROUTE_ADD =>
               declare
                  routeDest : constant Net.IPv4Address :=
                     Net.unpackIPv4 (msg.words (0));
                  routePrefix : constant Natural :=
                     Natural (msg.words (1));
                  routeGW : constant Net.IPv4Address :=
                     Net.unpackIPv4 (msg.words (2));
                  routeIF : constant Natural :=
                     Natural (msg.words (3));
                  added : Boolean := False;
               begin
                  for i in routeTable'Range loop
                     if not routeTable (i).active then
                        routeTable (i) :=
                          (active  => True,
                           dest    => routeDest,
                           prefix  => routePrefix,
                           gateway => routeGW,
                           ifIdx   => routeIF,
                           metric  => 0);
                        added := True;
                        exit;
                     end if;
                  end loop;
                  if added then
                     replyOKWord (sender, 0);
                  else
                     replyError (sender);
                  end if;
               end;

            when OP_NET_ROUTE_DEL =>
               declare
                  routeDest : constant Net.IPv4Address :=
                     Net.unpackIPv4 (msg.words (0));
                  routePrefix : constant Natural :=
                     Natural (msg.words (1));
                  deleted : Boolean := False;
               begin
                  for i in routeTable'Range loop
                     if routeTable (i).active and then
                        routeTable (i).dest = routeDest and then
                        routeTable (i).prefix = routePrefix
                     then
                        routeTable (i).active := False;
                        deleted := True;
                        exit;
                     end if;
                  end loop;
                  if deleted then
                     replyOKWord (sender, 0);
                  else
                     replyError (sender);
                  end if;
               end;

            when OP_NET_OPEN_RAW =>
               --  Open raw UDP channel (for DHCP etc.)
               --  words(0)=ifIdx, words(1)=proto, words(2)=port
               --  For now, just acknowledge (raw channel handled by
               --  regular packet dispatch with broadcast acceptance)
               replyOKWord (sender, 0);

            when OP_NET_RESOLVE =>
               handleAppResolve (sender, msg);

            when OP_NET_CONNECT =>
               handleAppConnect (sender, msg);

            when OP_NET_SEND =>
               handleAppSend (sender, msg);

            when OP_NET_RECV =>
               handleAppRecv (sender, msg);

            when OP_NET_CLOSE =>
               handleAppClose (sender, msg);

            when OP_NET_OPEN =>
               handleNetOpen (sender, msg);

            when OP_NET_WRITE =>
               handleNetWrite (sender, msg);

            when OP_NET_READ =>
               handleNetRead (sender, msg);

            when OP_NET_SHUT =>
               handleNetShut (sender, msg);

            when others =>
               declare
                  replyMsg : constant Message :=
                    (tag      => (label  => REPLY_ERR,
                                  length => 0,
                                  flags  => 0,
                                  badge  => 0),
                     capBadge => 0,
                     words    => (others => 0));
                  ignore : Unsigned_64;
               begin
                  ignore := reply (sender, replyMsg);
               end;
         end case;
      end if;
   end loop;

end main;
