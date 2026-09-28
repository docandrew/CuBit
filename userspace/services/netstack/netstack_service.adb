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
with Net.UDP.Datagram;
with SipHash;
with TCP_Congestion;
with TCP_Header;
with DNS_Name;
with DNS_Response;
with IPv4_Header;
with ICMPv4_Error;
with ARP_Packet;
with ARP_Cache;
with UDP_Header;
with TCP_Connection;
with TCP_Engine;
with TCP_Isn;
with TCP_Limits;
with TCP_Options;
with TCP_Sequence;
with TCP_Slots;
with TCP_Time_Wait;
with TCP_Reset;
with TCP_Wire;
with TCP_Listeners;
with Frame_Ring;
with Channel_Geometry;
with Channel_Arenas;
with IPv6_Frame;
with IPv6_Header;
with IPv6_Link;
with Connection_Table;
with TCP_Isn;
with CuBit.Locators;
with CuBit.Net_Locator;
with CuBit.Net_Address;
with System.Machine_Code;
with Network_Grants;
with Network_Channel_Handles;
with UDP_Channels;
with CuBit.Network_Authority;
with CuBit.Memory_Grants;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
with CuBit.Net_Channel_Layout;
with Netstack_Profile;

package body Netstack_Service is
   use ASCII;
   use type Net.IPv4Address;
   use type Net.MACAddress;
   use type TCP_Connection.State;
   use type TCP_Connection.Event;
   use type ARP_Packet.Operation;
   subtype Seq is TCP_Sequence.Seq;
   use type Seq;
   package Network_Authority renames CuBit.Network_Authority;
   package Layout renames CuBit.Net_Channel_Layout;
   package Rings renames CuBit.Channel_Rings;
   package Datagrams renames CuBit.Datagram_Rings;
   package Prof renames Netstack_Profile;
   use type TCP_Listeners.Bind_Status;
   networkGrants : Network_Grants.Table;
   listeners : TCP_Listeners.Table;

   --  Well-known capability slots (granted by kernel modules.adb)
   CAP_SLOT_NET_DRV : constant CapabilitySlot := 10;

   --  Shared packet buffer layout
   PACKET_BUF_PAGES : constant := 128;    -- 512 KB: RX ring, then TX ring
   PACKET_BUF_SIZE  : constant := PACKET_BUF_PAGES * 4096;
   TX_AREA_OFFSET   : constant := PACKET_BUF_SIZE / 2;    -- TX half
   TX_SLOT_SIZE     : constant := 2048;                   -- per-slot size
   --  The TX half is a ring the driver drains (virtio-net's drainTXRing):
   --  its first slot holds our producer count (byte 0) and the driver's
   --  consumer count (byte 64), both free-running; frame N is in slot
   --  (N mod TX_RING_SLOTS) + 1, its length (4 bytes) first and the frame
   --  TX_FRAME_HEADER bytes in. OP_NET_TX is only a doorbell.
   NUM_TX_SLOTS       : constant := (PACKET_BUF_SIZE / 2) / TX_SLOT_SIZE;
   TX_RING_SLOTS      : constant := NUM_TX_SLOTS - 1;
   TX_FRAME_HEADER    : constant := 16;
   TX_CONSUMER_OFFSET : constant := 64;


   --  Multi-interface support
   MAX_INTERFACES : constant := 4;
   type InterfaceState is (IF_DOWN, IF_UP, IF_CONFIGURING);

   type InterfaceRecord is record
      state     : InterfaceState := IF_DOWN;
      mac       : Net.MACAddress := [others => 0];
      ipv4      : Net.IPv4Address := [others => 0];
      netmask   : Net.IPv4Address := [others => 0];
      gateway   : Net.IPv4Address := [others => 0];
      driverPID : ProcessID := NO_PROCESS;
      --  Neighbours (proved: unsolicited replies never change it).
      arpCache  : ARP_Cache.Table;
      gwMAC     : Net.MACAddress := Net.ZERO_MAC;
      grantId   : Unsigned_64 := 0;
      txProduced : Unsigned_32 := 0;   --  frames put in the TX ring
      pktBuf    : System.Address := System.Null_Address;
      pktGrant  : Unsigned_64 := 0;
   end record;

   interfaces : array (0 .. MAX_INTERFACES - 1) of InterfaceRecord;

   function arpIP (A : Net.IPv4Address) return ARP_Packet.IPv4 is ([A (0), A (1), A (2), A (3)]);
   function netMAC (M : ARP_Packet.MAC) return Net.MACAddress is
     ([M (0), M (1), M (2), M (3), M (4), M (5)]);

   --  The neighbour's hardware address, if resolved.
   function arpLookup (ifIdx : Natural; ip : Net.IPv4Address; mac : out Net.MACAddress)
     return Boolean
   is
      hw    : ARP_Packet.MAC;
      found : Boolean;
   begin
      ARP_Cache.Lookup (interfaces (ifIdx).arpCache, arpIP (ip), hw, found);
      mac := (if found then netMAC (hw) else Net.ZERO_MAC);
      return found;
   end arpLookup;
   numIfaces  : Natural := 0;

   --  Global DNS (not per-interface)
   primaryDNS   : Net.IPv4Address := [others => 0];
   --  Per-packet serial traces. Off by default: concurrent serial output
   --  from several processes interleaves inside other lines, including test
   --  readiness markers. Errors and drops are still always reported.
   Trace_Packets : constant Boolean := False;

   --  netstack's own resolver port. Application UDP channels use ephemeral
   --  ports (49152..65535), so they can never receive resolver replies.
   --  Resolver source ports: one at random per query (RFC 5452 9.2).
   DNS_PORT_FIRST : constant Unsigned_16 := 49152;
   DNS_PORT_COUNT : constant := 16_384;
   DNS_SERVER_PORT : constant Unsigned_16 := 53;
   secondaryDNS : Net.IPv4Address := [others => 0];

   --  Convenience aliases for interface 0 (used during transition)
   --  These are procedures/functions that access interfaces(0) directly.
   --  TODO: thread ifIdx through all packet handlers for full multi-if.

   --  Self-test state (removed: netmgr handles config now)
   resolvedIP   : Net.IPv4Address := [others => 0];

   --  TCP connections: each slot's addressing and lifetime (TCP_Slots), and
   --  its proved protocol engine (TCP_Engine: flow, queues, timers).
   tcpConns : TCP_Slots.Table;
   package Flows renames TCP_Engine.Flows;
   package EP renames TCP_Engine.Endpoints;
   package Sends renames TCP_Engine.Sends;
   package Receives renames TCP_Engine.Receives;
   tcpFlows : TCP_Engine.Flow_Array renames TCP_Engine.Flow_Table;
   tcpPool  : TCP_Engine.Pool.Pool renames TCP_Engine.Shared_Pool;
   connectionAuthority : array (tcpConns'Range) of Unsigned_64 := [others => 0];
   parentListener : array (tcpConns'Range) of TCP_Listeners.Handle := [others => 0];
   --  When an ownerless connection is given up, or TIME-WAIT held in a slot
   --  (its channel still open) ends.
   lingerDeadline : array (tcpConns'Range) of Unsigned_64 := [others => Unsigned_64'Last];
   --  Delayed acknowledgements (RFC 1122 4.2.3.2, RFC 5681 4.2): at least
   --  every second in-order segment, else within DELAYED_ACK_MS.
   ackOwed     : array (tcpConns'Range) of Natural := [others => 0];
   ackDeadline : array (tcpConns'Range) of Unsigned_64 := [others => Unsigned_64'Last];
   DELAYED_ACK_MS : constant Unsigned_64 := 10;
   --  Received TCP segments whose header was not well formed.
   tcpMalformed : Unsigned_64 := 0;

   --  The monotonic clock (ms), read once per main-loop pass and per
   --  received batch rather than per segment.
   clockNow : Unsigned_64 := 0;

   --  The connection stopped sending because the transmit queue was full.
   txBlocked : array (tcpConns'Range) of Boolean := [others => False];
   --  While a batch of received frames is handled, replies to readers and
   --  acknowledgements wait for its end: one of each per connection per
   --  batch instead of per segment (fewer IPCs and frames).
   inRxBatch    : Boolean := False;
   batchTouched : array (tcpConns'Range) of Boolean := [others => False];
   --  The connections touched in this batch, in order (no scan of every slot).
   touchedList  : array (tcpConns'Range) of TCP_Slots.Connection_Index := [others => 0];
   touchedCount : Natural range 0 .. tcpConns'Length := 0;
   HANDSHAKE_TIMEOUT_MS : constant Unsigned_64 := 5000;
   PING_TIMEOUT_MS : constant Unsigned_64 := 5000;
   TIME_WAIT_MS : constant Unsigned_64 := TCP_Time_Wait.Wait_Milliseconds;
   --  How long a connection whose owner has let go may take to finish.
   ORPHAN_LINGER_MS : constant Unsigned_64 := 120_000;

   --  The largest application buffer (grant) a channel request may name:
   --  a stream channel's header page and two rings of the largest size.
   CHANNEL_BUFFER_SIZE : constant := Layout.Header_Bytes + 2 * Rings.Maximum_Size;

   --  Our segment size on the link (Ethernet, IPv4, no TCP options), and
   --  what our SYNs offer: window scaling (RFC 7323); SACK and timestamps
   --  are not offered yet.
   LINK_MSS : constant := 1_500 - TCP_Limits.IPv4_Header_Size - TCP_Limits.TCP_Header_Size;
   OUR_OFFER : constant TCP_Options.Offer :=
     (MSS => LINK_MSS, Window_Scale => True, Shift_Count => TCP_Options.Default_Shift,
      SACK_Permitted => False, Timestamps => False);
   --  What each connection's SYN exchange agreed (no scaling until then).
   agreements : array (tcpConns'Range) of TCP_Options.Agreement;

   --  RFC 6528's secret for initial sequence numbers, chosen at start.
   isnKey : SipHash.Key := (K0 => 0, K1 => 0);

   --  Which connection a segment belongs to: the proved, hashed
   --  Connection_Table (userspace/net/src), keyed by the 4-tuple of 16-byte
   --  addresses (IPv4 held mapped). Slot S is tcpConns (S - 1).
   package Conns is new Connection_Table
     (Max_Connections => TCP_Slots.MAX_TCP_CONNS, Bucket_Count => 64,
      Bucket_Size => 8);
   connTable : Conns.Table;
   connHandle : array (TCP_Slots.Connection_Index) of Conns.Handle;

   function mappedOf (a : Net.IPv4Address) return TCP_Isn.Address is
     [1 .. 10 => 0, 11 => 16#FF#, 12 => 16#FF#,
      13 => a (0), 14 => a (1), 15 => a (2), 16 => a (3)];

   function tupleOf (localIP, remoteIP : Net.IPv4Address;
                     localPort, remotePort : Unsigned_16) return TCP_Isn.Endpoints is
     ((Local => mappedOf (localIP), Remote => mappedOf (remoteIP),
       Local_Port => localPort, Remote_Port => remotePort));

   --  A slot for this 4-tuple, or -1 (open already, or no room).
   procedure claimSlot (localIP, remoteIP : Net.IPv4Address;
                        localPort, remotePort : Unsigned_16;
                        remoteMAC : Net.MACAddress;
                        index : out TCP_Slots.Connection_Reference);
   --  The slot's connection is gone from the table.
   procedure dropSlot (index : TCP_Slots.Connection_Index);

   procedure claimSlot (localIP, remoteIP : Net.IPv4Address;
                        localPort, remotePort : Unsigned_16;
                        remoteMAC : Net.MACAddress;
                        index : out TCP_Slots.Connection_Reference) is
      h : Conns.Handle;
      status : Conns.Insert_Status;
      use type Conns.Insert_Status;
   begin
      index := -1;
      Conns.Insert (connTable, tupleOf (localIP, remoteIP, localPort, remotePort),
                    h, status);
      if status /= Conns.Inserted then
         return;
      end if;
      index := h.Index - 1;
      connHandle (index) := h;
      tcpConns (index) := (inUse => True, reserved => True,
                           localPort => localPort, remotePort => remotePort,
                           remoteIP => remoteIP, remoteMAC => remoteMAC);
   end claimSlot;

   procedure dropSlot (index : TCP_Slots.Connection_Index) is
   begin
      if Conns.Current (connTable, connHandle (index)) then
         Conns.Remove (connTable, connHandle (index));
      end if;
      tcpConns (index) := (others => <>);
   end dropSlot;



   --  Connections in TIME-WAIT, out of their slots (proved table).
   timeWaits : TCP_Time_Wait.Table;

   function deadlineAfter (Now, Delay_MS : Unsigned_64) return Unsigned_64 is
     (if Delay_MS > Unsigned_64'Last - Now then Unsigned_64'Last else Now + Delay_MS);
   nextEphemeralPort : Unsigned_16 := 49152;  -- incrementing ephemeral port

   --  Legacy aliases to interface 0 (for incremental refactoring)
   --  New code should use interfaces(ifIdx).xxx directly.

   --  IPC label constants (must match kernel ipc_labels.ads)
   OP_NET_ATTACH  : constant Unsigned_32 := 16#0400#;
   OP_NET_RX      : constant Unsigned_32 := 16#0401#;
   OP_NET_TX      : constant Unsigned_32 := 16#0402#;
   OP_NET_RESOLVE : constant Unsigned_32 := 16#0410#;
   OP_NET_OPEN    : constant Unsigned_32 := 16#0420#;
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

   --  Routing table
   MAX_ROUTES : constant := 16;
   type RouteEntry is record
      active  : Boolean := False;
      dest    : Net.IPv4Address := [others => 0];
      prefix  : Natural := 0;
      gateway : Net.IPv4Address := [others => 0];
      ifIdx   : Natural := 0;
      metric  : Natural := 0;
   end record;
   routeTable : array (0 .. MAX_ROUTES - 1) of RouteEntry;

   --  Deferred TX queue: during OP_NET_RX processing we can't capCall to
   --  the driver (it's blocked waiting for our reply). Buffer frames here
   --  and flush between message receives.
   --  STOPGAP (2026-09-25): deep enough that Servo's parallel downloads do
   --  not drop frames while the driver's mailbox is full. TCP here has no
   --  retransmission yet, so a dropped segment stalls its connection. To be
   --  replaced by the netstack redesign (docs/servo-port.md).
   MAX_DEFERRED_TX  : constant := 256;
   MAX_DEFERRED_LEN : constant := 1514;  -- Ethernet header + 1500-byte MTU

   type FrameData is array (0 .. MAX_DEFERRED_LEN - 1) of Unsigned_8;

   type DeferredFrame is record
      data : FrameData;
      len  : Natural := 0;
   end record;

   --  TCP stops sending data above the high-water mark and resumes below
   --  the low one, keeping room for acknowledgements and control segments.
   TX_HIGH_WATER : constant := MAX_DEFERRED_TX / 2;
   TX_LOW_WATER  : constant := MAX_DEFERRED_TX / 8;
   txDropped     : Unsigned_64 := 0;

   --  A ring: deferredHead is the oldest frame, deferredCount the number.
   deferredTX    : array (0 .. MAX_DEFERRED_TX - 1) of DeferredFrame;
   deferredHead  : Natural range 0 .. MAX_DEFERRED_TX - 1 := 0;
   deferredCount : Natural range 0 .. MAX_DEFERRED_TX := 0;

   --  Channel table (tracks app connections via new channel API + legacy)
   type ChannelKind is (CHANNEL_NONE, CHANNEL_CLIENT, CHANNEL_SERVER, CHANNEL_LISTENER);

   type NetChannel is record
      kind       : ChannelKind := CHANNEL_NONE;
      proto      : Unsigned_8 := 0;
      pid        : ProcessID := NO_PROCESS;
      bufAddr    : System.Address := System.Null_Address;
      bufSize    : Natural := 0;
      connIdx    : Integer := -1;
      remoteIP   : Net.IPv4Address := [others => 0];
      remotePort : Unsigned_16 := 0;
      localPort  : Unsigned_16 := 0;
      authorityTag : Unsigned_64 := 0;
      charged  : Boolean := False;  -- holds one of authorityTag's channels
      --  The arena buffer the channel's rings live in, while claimed.
      arena    : Channel_Arenas.Arena_Index := Channel_Arenas.Arena_Index'First;
      slot     : Channel_Arenas.Buffer_Index := 0;
      claimed  : Boolean := False;
      --  CHANNEL_LISTENER: its listener; its rings carry offers and arrivals.
      listener : TCP_Listeners.Handle := 0;
      --  Stream (TCP) channels: the shared rings (CuBit.Net_Channel_Layout),
      --  our view of their indices, and what we last published.
      stream    : Boolean := False;
      tx        : Rings.Consumer;
      rx        : Rings.Producer;
      waitBit   : Natural range 0 .. Layout.Maximum_Wait_Bit := 0;
      status    : Unsigned_32 := Layout.Status_Opening;
      kickFlags : Unsigned_32 := 0;
      shutDone  : Boolean := False;
   end record;

   channels : array (Network_Channel_Handles.Channel_Index) of NetChannel;

   --  Channel arenas: which buffers channels hold (proved bookkeeping), and
   --  beside it each arena's grant and where netstack mapped it.
   arenas : Channel_Arenas.Table;
   arenaGrant : array (Channel_Arenas.Arena_Index) of CuBit.Memory_Grants.Grant_Reference;
   arenaBase : array (Channel_Arenas.Arena_Index) of System.Address :=
     [others => System.Null_Address];
   channelHandles : Network_Channel_Handles.Table;
   --  Connected UDP state shares the channel index with channels above.
   udpChannels : UDP_Channels.Table;

   procedure releaseConnection (Index : TCP_Slots.Connection_Index);
   procedure discardConnection (Index : TCP_Slots.Connection_Index);

   procedure releaseChannel (Index : Network_Channel_Handles.Channel_Index) is
   begin
      --  A listener takes the connections nobody took with it.
      if channels (Index).kind = CHANNEL_LISTENER then
         declare
            children : TCP_Listeners.Connection_List;
            closed : Boolean;
         begin
            TCP_Listeners.Close
              (listeners, channels (Index).authorityTag, channels (Index).listener,
               children, closed);
            for C in children'Range loop
               if children (C) then discardConnection (C); end if;
            end loop;
         end;
      end if;
      if channels (Index).connIdx in tcpConns'Range then
         releaseConnection (channels (Index).connIdx);
      end if;
      if channels (Index).claimed then
         Channel_Arenas.Release (arenas, channels (Index).arena, channels (Index).slot);
      end if;
      UDP_Channels.Close (udpChannels, Index);
      if channels (Index).charged then
         Network_Grants.Refund (networkGrants, channels (Index).authorityTag);
      end if;
      channels (Index) := (others => <>);
      Network_Channel_Handles.Release (channelHandles, Index);
   end releaseChannel;

   --  Charge a new channel to the scope it was opened under. A holder may
   --  keep open only as many channels as its scope declared.
   function chargeChannel
     (Index : Network_Channel_Handles.Channel_Index) return Boolean is
   begin
      Network_Grants.Charge (networkGrants, channels (Index).pid,
                             channels (Index).authorityTag,
                             channels (Index).charged);
      if not channels (Index).charged then
         debugPrint ("netstack: open: the scope's declared connections are all in use" & LF);
      end if;
      return channels (Index).charged;
   end chargeChannel;

   --  Take buffer Buffer of the caller's arena Arena for channel Index.
   function claimBuffer
     (Index  : Network_Channel_Handles.Channel_Index;
      Owner  : ProcessID;
      Arena  : Unsigned_64;
      Buffer : Unsigned_64) return Boolean
   is
      A : Channel_Arenas.Arena_Index;
      S : Channel_Arenas.Buffer_Index;
      Offset : Channel_Arenas.Span_Bytes;
      OK : Boolean;
   begin
      Channel_Arenas.Claim
        (arenas, Unsigned_64 (Owner), Channel_Arenas.Handle (Arena), Buffer,
         A, S, Offset, OK);
      if OK then
         channels (Index).arena := A;
         channels (Index).slot := S;
         channels (Index).claimed := True;
         channels (Index).bufAddr := arenaBase (A) + Storage_Offset (Offset);
         channels (Index).bufSize := Channel_Arenas.Size_Of (arenas, A);
      end if;
      return OK;
   end claimBuffer;

   --  Pending request queue (deferred reply for blocking ops)
   type PendingKind is (PENDING_NONE, PENDING_RESOLVE,
                        PENDING_CONNECT,
                        PENDING_OPEN, PENDING_PING,
                        PENDING_WAIT);
   DNS_ATTEMPTS : constant := 3;
   subtype DNS_Attempt is Natural range 0 .. DNS_ATTEMPTS;

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
      replySlot  : CapabilitySlot := CapabilitySlot'First;
      --  PENDING_RESOLVE / PENDING_OPEN: the query's random source port and
      --  a keyed hash of the question name it asked.
      queryPort  : Unsigned_16 := 0;
      nameHash   : Unsigned_64 := 0;
      --  When an unanswered query gives up.
      queryDeadline : Unsigned_64 := Unsigned_64'Last;
      --  The question, for retransmission: attempts so far, and when the
      --  next goes out (to the other server when there are two).
      question    : DNS_Name.Wire := [others => 0];
      questionLen : DNS_Name.Wire_Length := 0;
      attempts    : DNS_Attempt := 0;
      nextSend    : Unsigned_64 := Unsigned_64'Last;
      --  PENDING_WAIT: when it completes with no ready channel, and the
      --  wait bits of the channels it is for.
      waitDeadline : Unsigned_64 := Unsigned_64'Last;
      waitMask     : Unsigned_64 := 0;
   end record;

   --  How long a resolver query waits for its answer, in all. Within it,
   --  a query is sent up to DNS_ATTEMPTS times, alternating servers, the
   --  wait doubling from DNS_FIRST_WAIT_MS (1 s, then 2 s: RFC 1536 3).
   DNS_TIMEOUT_MS    : constant Unsigned_64 := 5_000;
   DNS_FIRST_WAIT_MS : constant Unsigned_64 := 1_000;

   --  Deferred replies are saved in capability slots 16 .. 16 + MAX_PENDING - 1.
   MAX_PENDING : constant := 32;
   pendingReqs : array (0 .. MAX_PENDING - 1) of PendingRequest;
   --  Resolver randomness: transaction IDs and source ports are a keyed
   --  PRF of a counter (unpredictable off-path, RFC 5452), keyed at start.
   dnsKey     : SipHash.Key := (K0 => 0, K1 => 0);
   dnsCounter : Unsigned_64 := 0;

   function dnsRandom return Unsigned_64 is
      counter : SipHash.Byte_Array (0 .. 7);
   begin
      dnsCounter := dnsCounter + 1;
      for I in counter'Range loop
         counter (I) := Unsigned_8 (Shift_Right (dnsCounter, 8 * I) and 16#FF#);
      end loop;
      return SipHash.Hash (dnsKey, counter);
   end dnsRandom;

   --  A keyed hash of a question name in wire form (lower case).
   function nameHashOf (name : DNS_Name.Bytes) return Unsigned_64 is
      data : SipHash.Byte_Array (0 .. name'Length - 1);
   begin
      for I in data'Range loop
         data (I) := name (name'First + I);
      end loop;
      return SipHash.Hash (dnsKey, data);
   end nameHashOf;

   function policyAddress (Address : Net.IPv4Address) return Unsigned_32 is
     (Shift_Left (Unsigned_32 (Address (0)), 24) or
      Shift_Left (Unsigned_32 (Address (1)), 16) or
      Shift_Left (Unsigned_32 (Address (2)), 8) or Unsigned_32 (Address (3)));

   --  Category admission precedes pointer/range decoding in every handler.
   --  A general service endpoint grants inspection, not networking or admin.
   function admittedRequest (Owner : ProcessID; Request : Message)
                             return Boolean is
   begin
      case Request.tag.label is
         when Network_Authority.OP_INSTALL_SCOPE |
              Network_Authority.OP_RELEASE_SCOPE |
              Network_Authority.OP_RELEASE_OWNER =>
            return Request.authorityTag = Network_Authority.Policy_Authority_Tag;
         when OP_NET_ATTACH | OP_NET_RX | REPLY_OK =>
            return Request.authorityTag = Network_Authority.Driver_Authority_Tag;
         when OP_NET_CONFIGURE | OP_NET_SET_DNS | OP_NET_ROUTE_ADD |
              OP_NET_ROUTE_DEL | OP_NET_OPEN_RAW | OP_NET_PING =>
            return Request.authorityTag = Network_Authority.Manager_Authority_Tag;
         when OP_NET_LIST_IF =>
            return True; -- an endpoint invocation is still required by the kernel
         when OP_NET_IF_DETAIL =>
            return Request.words (0) < Unsigned_64 (numIfaces);
         when OP_NET_ROUTE_LIST =>
            return Request.words (0) <= Unsigned_64 (routeTable'Length);
         when OP_NET_RESOLVE =>
            return Request.tag.length in 1 .. 32 and then Network_Grants.May_Resolve
              (networkGrants, Owner, Request.authorityTag);
         when OP_NET_OPEN =>
            return Request.tag.flags = 0 and then
              Request.tag.length in 1 .. Layout.Target_Maximum and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when Layout.OP_NET_ARENA =>
            return Request.tag.length = 4 and then
              Request.words (0) <= CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT and then
              Request.words (1) in 1 .. CuBit.Memory_Grants.MAXIMUM_GENERATION and then
              Rings.Valid_Size (Request.words (2) and 16#FFFF_FFFF#) and then
              Rings.Valid_Size (Shift_Right (Request.words (2), 32)) and then
              Request.words (3) in 1 .. Layout.Maximum_Arena_Buffers and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when Layout.OP_NET_SCOPE =>
            return Request.tag.length = 0 and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when Layout.OP_NET_ARENA_RELEASE =>
            return Request.tag.length = 1 and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when Layout.OP_NET_WAIT =>
            return Request.tag.length = 3 and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when Layout.OP_NET_KICK =>
            return Request.tag.length = 2 and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when OP_NET_SHUT =>
            return Request.words (1) <= CHANNEL_BUFFER_SIZE and then
              Request.words (2) <= CHANNEL_BUFFER_SIZE and then
              Network_Grants.Owned (networkGrants, Owner, Request.authorityTag);
         when others =>
            return False; -- includes retired raw connection-index operations
      end case;
   end admittedRequest;

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
      if ip = [255, 255, 255, 255] and numIfaces > 0 then
         return 0;
      end if;
      return -1;
   end findInterfaceForIP;

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
      nextHop := [others => 0];

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
                                   gateway => [others => 0],
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
                                   dest    => [others => 0],
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
   --  Frames were added to the TX ring since the driver was last told.
   txDoorbell : Boolean := False;

   function doSendFrame (frameAddr : System.Address;
                         frameLen  : Natural) return Boolean is
      area : constant System.Address := interfaces (0).pktBuf + Storage_Offset (TX_AREA_OFFSET);
      produced : Unsigned_32 with Volatile, Import, Address => area;
      consumed : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (TX_CONSUMER_OFFSET);
      next : constant Unsigned_32 := interfaces (0).txProduced;
   begin
      if not Frame_Ring.Has_Room (next, consumed, TX_RING_SLOTS) or else
        not Frame_Ring.Fits (Unsigned_32 (frameLen), TX_SLOT_SIZE, TX_FRAME_HEADER)
      then
         return False;   --  the ring is full: the caller defers the frame
      end if;
      declare
         slot : constant System.Address :=
           area + Storage_Offset (Frame_Ring.Slot_Of (next, TX_RING_SLOTS) * TX_SLOT_SIZE);
         type Frame is array (0 .. frameLen - 1) of Unsigned_8;
         src : Frame with Import, Address => frameAddr;
         dst : Frame with Import, Address => slot + Storage_Offset (TX_FRAME_HEADER);
         len : Unsigned_32 with Volatile, Import, Address => slot;
      begin
         dst := src;
         len := Unsigned_32 (frameLen);
      end;
      --  The frame is written before the count that hands it over.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      interfaces (0).txProduced := next + 1;
      produced := next + 1;
      txDoorbell := True;
      return True;
   end doSendFrame;

   --  Zero-copy transmit: the next ring slot's frame area, to build a frame
   --  in place, or Null_Address if the ring is full or older frames still
   --  wait in the deferred queue (they go first). txCommit hands it over.
   function txReserve (frameLen : Natural) return System.Address is
      area : constant System.Address := interfaces (0).pktBuf + Storage_Offset (TX_AREA_OFFSET);
      consumed : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (TX_CONSUMER_OFFSET);
      next : constant Unsigned_32 := interfaces (0).txProduced;
   begin
      if interfaces (0).pktBuf = System.Null_Address or else deferredCount > 0 or else
        not Frame_Ring.Has_Room (next, consumed, TX_RING_SLOTS) or else
        not Frame_Ring.Fits (Unsigned_32 (frameLen), TX_SLOT_SIZE, TX_FRAME_HEADER)
      then
         return System.Null_Address;
      end if;
      return area + Storage_Offset (Frame_Ring.Slot_Of (next, TX_RING_SLOTS) * TX_SLOT_SIZE + TX_FRAME_HEADER);
   end txReserve;

   procedure txCommit (frameLen : Natural) is
      area : constant System.Address := interfaces (0).pktBuf + Storage_Offset (TX_AREA_OFFSET);
      produced : Unsigned_32 with Volatile, Import, Address => area;
      next : constant Unsigned_32 := interfaces (0).txProduced;
      len : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (Frame_Ring.Slot_Of (next, TX_RING_SLOTS) * TX_SLOT_SIZE);
   begin
      len := Unsigned_32 (frameLen);
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      interfaces (0).txProduced := next + 1;
      produced := next + 1;
      txDoorbell := True;
   end txCommit;

   --  The driver publishes a new nonzero epoch here (TX area, byte 68) each
   --  time it is about to wait; one doorbell per epoch wakes it.
   TX_DOORBELL_OFFSET : constant := 68;
   txRungEpoch : Unsigned_32 := 0;

   --  Tell the driver the ring has frames (or the RX ring has room), if it
   --  is idle: a busy driver drains the ring before it waits again.
   procedure ringTXDoorbell is
      doorbell : constant Message :=
        (tag      => (label  => OP_NET_TX,
                      length => 0,
                      flags  => 1,      -- no reply
                      reserved  => 0),
         authorityTag => 0,
         words    => [others => 0]);
      ignore : Boolean;
   begin
      if txDoorbell and then interfaces (0).driverPID /= NO_PROCESS and then
        interfaces (0).pktBuf /= System.Null_Address
      then
         txDoorbell := False;
         --  The counts were published before the epoch is read.
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         declare
            epoch : Unsigned_32 with Volatile, Import,
              Address => interfaces (0).pktBuf +
                         Storage_Offset (TX_AREA_OFFSET + TX_DOORBELL_OFFSET);
            seen : constant Unsigned_32 := epoch;
         begin
            if seen /= 0 and then seen /= txRungEpoch then
               txRungEpoch := seen;
               ignore := capSubmit (CAP_SLOT_NET_DRV, doorbell, Unsigned_64'Last);
            end if;
         end;
      end if;
   end ringTXDoorbell;

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

      ok := doSendFrame (deferredTX (deferredHead).data'Address,
                          deferredTX (deferredHead).len);
      if ok then
         deferredHead := (deferredHead + 1) mod MAX_DEFERRED_TX;
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

      --  Older frames go first: sending around them would reorder the wire.
      while deferredCount > 0 and then flushOneDeferredTX loop
         null;
      end loop;
      ok := deferredCount = 0 and then doSendFrame (frameAddr, frameLen);
      if not ok then
         --  Driver mailbox full, buffer for later
         if deferredCount < MAX_DEFERRED_TX then
            declare
               slot : constant Natural :=
                 (deferredHead + deferredCount) mod MAX_DEFERRED_TX;
               src : array (0 .. frameLen - 1) of Unsigned_8 with
                  Import, Address => frameAddr;
            begin
               for i in src'Range loop
                  deferredTX (slot).data (i) := src (i);
               end loop;
               deferredTX (slot).len := frameLen;
            end;
            deferredCount := deferredCount + 1;
         else
            --  Reported, not printed per frame: the console is slow.
            txDropped := txDropped + 1;
            if (txDropped and 16#FFF#) = 1 then
               debugPrint ("netstack: deferred TX full, dropped ");
               printDec (Unsigned_32 (txDropped and 16#FFFF_FFFF#));
               debugPrint (" frames so far" & LF);
            end if;
         end if;
      end if;
   end sendFrame;

   procedure sendIPv6 (frame : IPv6_Header.Bytes)
     with Pre => IPv6_Frame.Emittable (frame);
   procedure sendIPv6 (frame : IPv6_Header.Bytes) is
   begin
      sendFrame (frame'Address, frame'Length);
   end sendIPv6;

   --  IPv6 on the link: address configuration and neighbors (ipv6_link.ads,
   --  proved through tests/tcp-session/ipv6_link_proof.ads).
   package IPv6 is new IPv6_Link (Send => sendIPv6, Log => debugPrint);

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

      --  The reply to this, and only it, may resolve targetIP.
      ARP_Cache.Request (interfaces (0).arpCache, arpIP (targetIP), syscall (SYSCALL_GETTIME));
      sendFrame (fAddr, 42);
   end sendARPRequest;

   --  How often an unanswered neighbour is asked again (RFC 1122 2.3.2.1:
   --  at most one request per second per destination).
   ARP_RETRY_MS : constant Unsigned_64 := 1_000;

   --  Ask for hop's link address, unless we asked within ARP_RETRY_MS.
   procedure requestNeighbour (hop : Net.IPv4Address) is
      pos : constant Integer := ARP_Cache.Position (interfaces (0).arpCache, arpIP (hop));
      use type ARP_Cache.State;
   begin
      if pos >= 0 and then interfaces (0).arpCache (pos).St = ARP_Cache.Pending and then
        clockNow < deadlineAfter (interfaces (0).arpCache (pos).Since, ARP_RETRY_MS)
      then
         return;
      end if;
      sendARPRequest (hop);
   end requestNeighbour;

   --  The link address to send to dstIP by: its next hop's (the route
   --  table's gateway, or dstIP itself on a connected network), from the
   --  ARP cache. If it is not resolved yet, a request goes out and found
   --  is False: the caller drops the frame, and TCP's retransmission or
   --  the resolver's retry sends it again.
   procedure neighbourMAC (dstIP : Net.IPv4Address;
                           mac   : out Net.MACAddress;
                           found : out Boolean) is
      ifIdx : Integer;
      hop   : Net.IPv4Address;
   begin
      mac := Net.ZERO_MAC;
      found := False;
      routeLookup (dstIP, ifIdx, hop);
      if ifIdx < 0 then
         return;   --  no route
      end if;
      found := arpLookup (0, hop, mac);
      if not found then
         requestNeighbour (hop);
      end if;
   end neighbourMAC;

   --  dstIP's next-hop link address, or zero if not resolved yet (asked).
   function resolvedNeighbour (dstIP : Net.IPv4Address) return Net.MACAddress is
      mac   : Net.MACAddress;
      found : Boolean;
   begin
      neighbourMAC (dstIP, mac, found);
      return mac;
   end resolvedNeighbour;

   --  The next hop for dstIP, or 0.0.0.0 with no route.
   function nextHopOf (dstIP : Net.IPv4Address) return Net.IPv4Address is
      ifIdx : Integer;
      hop   : Net.IPv4Address;
   begin
      routeLookup (dstIP, ifIdx, hop);
      return (if ifIdx < 0 then [others => 0] else hop);
   end nextHopOf;

   --  The IPv4 header of a frame, written by the proved IPv4_Header.Build.
   procedure writeIPv4Header (fAddr    : System.Address;
                              frameLen : Natural;
                              proto    : Unsigned_8;
                              srcIP    : Net.IPv4Address;
                              dstIP    : Net.IPv4Address;
                              df       : Boolean) is
      packet : IPv4_Header.Bytes (0 .. frameLen - 15)
        with Import, Address => fAddr + 14;
   begin
      IPv4_Header.Build
        ((Size         => IPv4_Header.Minimum_Size,
          Total_Length => frameLen - 14,
          Protocol     => proto,
          TTL          => 64,
          Source       => [srcIP (0), srcIP (1), srcIP (2), srcIP (3)],
          Destination  => [dstIP (0), dstIP (1), dstIP (2), dstIP (3)]),
         df, packet);
   end writeIPv4Header;

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
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      --  Ethernet header
      Net.putMAC   (fAddr, 0,  dstMAC);
      Net.putMAC   (fAddr, 6,  interfaces (ifIdx).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      writeIPv4Header (fAddr, FRAME_LEN, Net.PROTO_ICMP, interfaces (ifIdx).ipv4, dstIP, False);

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
   --  The Ethernet and IPv4 headers of a frame whose payload is in place.
   procedure fillIPv4Header (dstMAC   : Net.MACAddress;
                             dstIP    : Net.IPv4Address;
                             proto    : Unsigned_8;
                             fAddr    : System.Address;
                             frameLen : Natural) is
   begin
      --  Ethernet header
      Net.putMAC   (fAddr, 0,  dstMAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      writeIPv4Header (fAddr, frameLen, proto, interfaces (0).ipv4, dstIP, True);
   end fillIPv4Header;

   procedure sendIPv4Frame (dstMAC   : Net.MACAddress;
                            dstIP    : Net.IPv4Address;
                            proto    : Unsigned_8;
                            fAddr    : System.Address;
                            frameLen : Natural) is
   begin
      fillIPv4Header (dstMAC, dstIP, proto, fAddr, frameLen);
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
   --  A query for an A record of name (wire form, from DNS_Name.Encode),
   --  from the query's own source port.
   procedure sendDNSQuery (name   : DNS_Name.Bytes; txid, port : Unsigned_16;
                           server : Net.IPv4Address) is
      HEADER : constant := 12;
      payload : array (0 .. HEADER + DNS_Name.Maximum_Wire + 4 - 1) of Unsigned_8 :=
        [others => 0];
      pAddr   : constant System.Address := payload'Address;
      off     : constant Natural := HEADER + name'Length;
      dnsMAC  : Net.MACAddress;
      found   : Boolean;
   begin
      Net.putU16BE (pAddr, 0, txid);
      Net.putU16BE (pAddr, 2, 16#0100#);  -- flags: RD=1
      Net.putU16BE (pAddr, 4, 1);         -- QDCOUNT=1
      for I in 0 .. name'Length - 1 loop
         payload (HEADER + I) := name (name'First + I);
      end loop;
      Net.putU16BE (pAddr, off, 1);       -- QTYPE: A
      Net.putU16BE (pAddr, off + 2, 1);   -- QCLASS: IN

      --  The server's next hop; unresolved, this attempt is lost and the
      --  next one (or an ARP reply) sends the question again.
      neighbourMAC (server, dnsMAC, found);
      if found then
         sendUDP (server, dnsMAC, port, DNS_SERVER_PORT, pAddr, off + 4);
      end if;
   end sendDNSQuery;

   NO_ADDRESS : constant Net.IPv4Address := [others => 0];

   --  The server for a query's Nth attempt: the primary, then the
   --  secondary if there is one, and so on alternately.
   function dnsServer (attempt : DNS_Attempt) return Net.IPv4Address is
     (if attempt mod 2 = 1 and then secondaryDNS /= NO_ADDRESS then secondaryDNS
      else primaryDNS);

   function isDNSServer (ip : Net.IPv4Address) return Boolean is
     (ip /= NO_ADDRESS and then (ip = primaryDNS or else ip = secondaryDNS));

   --  Send a pending query's next attempt.
   procedure sendAttempt (P : in out PendingRequest; Now : Unsigned_64) is
   begin
      sendDNSQuery (P.question (0 .. P.questionLen - 1), P.txid, P.queryPort,
                    dnsServer (P.attempts));
      P.attempts := P.attempts + 1;
      P.nextSend :=
        (if P.attempts < DNS_ATTEMPTS
         then deadlineAfter (Now, DNS_FIRST_WAIT_MS * 2 ** (P.attempts - 1))
         else Unsigned_64'Last);
   end sendAttempt;


   --  Forward declarations for functions used by handleDNSResponse
   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstMAC  : Net.MACAddress;
                        dstPort : Unsigned_16) return Integer;
   procedure replyError
     (to   : ProcessID;
      slot : CapabilitySlot := CapabilitySlot'Last);

   --  A query that will not be answered: its requester gets an error, and
   --  an OPEN's channel is released.
   procedure failQuery (P : in out PendingRequest) is
   begin
      replyError (P.sender, P.replySlot);
      if P.kind = PENDING_OPEN and then P.channelIdx in channels'Range then
         releaseChannel (P.channelIdx);
      end if;
      P.kind := PENDING_NONE;
   end failQuery;

   procedure openDatagram
     (chIdx : Network_Channel_Handles.Channel_Index;
      owner : ProcessID;
      dstIP : Net.IPv4Address;
      port  : Unsigned_16;
      slot  : CapabilitySlot);

   ---------------------------------------------------------------------------
   --  handleDNSResponse - parse DNS A-record response, extract IP
   --
   --  Called with raw DNS payload bytes (after UDP header).
   ---------------------------------------------------------------------------
   procedure handleDNSResponse (dnsBuf : System.Address;
                                dnsLen : Natural;
                                port   : Unsigned_16) is
      response : DNS_Response.Response;
      valid    : Boolean;
      match    : Integer := -1;   --  the one pending query this answers
   begin
      if dnsLen > DNS_Response.Maximum_Message then
         return;
      end if;
      --  Every byte is read by the proved parser (dns_response.ads).
      declare
         message : constant DNS_Response.Bytes (0 .. dnsLen - 1)
           with Import, Address => dnsBuf;
      begin
         DNS_Response.Parse (message, response, valid);
      end;
      if not valid then
         return;
      end if;

      --  The pending query with this ID, on this source port, for this name.
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind in PENDING_RESOLVE | PENDING_OPEN and then
           pendingReqs (i).txid = response.Id and then
           pendingReqs (i).queryPort = port and then
           nameHashOf (response.Name (0 .. response.Name_Length - 1)) =
             pendingReqs (i).nameHash
         then
            match := i;
            exit;
         end if;
      end loop;
      if match < 0 then
         return;
      end if;
      case response.Rcode is
         when DNS_Response.No_Error =>
            --  An answer with no A record (NODATA) will not improve.
            if not response.Has_Address then
               failQuery (pendingReqs (match));
               return;
            end if;
         when DNS_Response.Server_Failure | DNS_Response.Refused =>
            --  This server cannot answer: try the next at once, if any
            --  attempts remain.
            if pendingReqs (match).attempts < DNS_ATTEMPTS then
               sendAttempt (pendingReqs (match), syscall (SYSCALL_GETTIME));
            else
               failQuery (pendingReqs (match));
            end if;
            return;
         when others =>
            --  NXDOMAIN and the rest are final.
            failQuery (pendingReqs (match));
            return;
      end case;
      resolvedIP := [for k in 0 .. 3 => response.Address (k)];

      --  Complete any pending RESOLVE request matching this TXID
      for i in pendingReqs'Range loop
         if i = match and then pendingReqs (i).kind = PENDING_RESOLVE then
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
                               reserved  => 0),
                  authorityTag => 0,
                  words    => [0 => ipPacked, others => 0]);
               ignore : Unsigned_64;
            begin
               ignore := replyCap (pendingReqs (i).replySlot, replyMsg);
            end;
            pendingReqs (i).kind := PENDING_NONE;
            exit;
         end if;
      end loop;

      --  Complete any pending OPEN request matching this TXID
      --  (DNS phase done → initiate TCP connect, transition to
      --  PENDING_CONNECT so completePendingConnect picks it up)
      for i in pendingReqs'Range loop
         if i = match and then pendingReqs (i).kind = PENDING_OPEN then
            declare
               chIdx   : constant Integer := pendingReqs (i).channelIdx;
               connIdx : Integer;
            begin
               if chIdx in channels'Range and then
                 channels (chIdx).proto = Net.PROTO_UDP
               then
                  openDatagram (chIdx, pendingReqs (i).sender, resolvedIP,
                                pendingReqs (i).dstPort,
                                pendingReqs (i).replySlot);
                  pendingReqs (i).kind := PENDING_NONE;
               elsif chIdx >= 0 and then chIdx <= channels'Last and then
                 Network_Grants.Allows
                   (networkGrants, pendingReqs (i).sender,
                    channels (chIdx).authorityTag, Network_Authority.Connect_TCP,
                    policyAddress (resolvedIP), pendingReqs (i).dstPort)
               then
                  channels (chIdx).remoteIP := resolvedIP;
                  connIdx := tcpConnect (resolvedIP, resolvedNeighbour (resolvedIP),
                                         pendingReqs (i).dstPort);
                  if connIdx < 0 then
                     releaseChannel (chIdx);
                     replyError (pendingReqs (i).sender,
                                 pendingReqs (i).replySlot);
                     pendingReqs (i).kind := PENDING_NONE;
                  else
                     channels (chIdx).connIdx := connIdx;
                     connectionAuthority (connIdx) := channels (chIdx).authorityTag;
                     --  Transition: PENDING_OPEN -> PENDING_CONNECT
                     --  so completePendingConnect will reply with
                     --  the channel handle.
                     pendingReqs (i).kind := PENDING_CONNECT;
                     pendingReqs (i).connIdx := connIdx;
                  end if;
               else
                  if chIdx in channels'Range then
                     releaseChannel (chIdx);
                  end if;
                  replyError (pendingReqs (i).sender,
                              pendingReqs (i).replySlot);
                  pendingReqs (i).kind := PENDING_NONE;
               end if;
            end;
            exit;
         end if;
      end loop;
   end handleDNSResponse;


   ---------------------------------------------------------------------------
   --  deliverDatagram - put a datagram into the receive ring of the
   --  matching connected UDP channel. A datagram with no exactly matching
   --  channel, or for a full ring, is dropped, as UDP permits.
   ---------------------------------------------------------------------------
   procedure putDatagram (chIdx : Network_Channel_Handles.Channel_Index;
                          payload : System.Address; len : Natural);

   procedure deliverDatagram (srcIP   : Net.IPv4Address;
                              srcPort : Unsigned_16;
                              dstPort : Unsigned_16;
                              payload : System.Address;
                              len     : Natural) is
      index  : UDP_Channels.Channel_Index;
      result : UDP_Channels.Delivery;
      use type UDP_Channels.Delivery;
   begin
      UDP_Channels.Deliver
        (udpChannels, dstPort, policyAddress (srcIP), srcPort, len, index, result);
      if result = UDP_Channels.Matched then
         putDatagram (index, payload, len);
      end if;
   end deliverDatagram;

   ---------------------------------------------------------------------------
   --  handleUDP - parse UDP datagram via RecordFlux, dispatch on port
   ---------------------------------------------------------------------------
   --  A resolver query is waiting on this source port.
   function dnsPortPending (port : Unsigned_16) return Boolean is
     (for some P of pendingReqs =>
        P.kind in PENDING_RESOLVE | PENDING_OPEN and then P.queryPort = port);

   procedure handleUDP (pktBuf     : System.Address;
                        ipOff      : Natural;
                        ipHdrLen   : Natural;
                        srcIP      : Net.IPv4Address;
                        dstIP      : Net.IPv4Address;
                        totalIPLen : Natural) is
      udpOff : constant Natural := ipOff + ipHdrLen;
      udpLen : constant Natural := totalIPLen - ipHdrLen;
      --  The datagram in place (proved codec).
      datagram : UDP_Header.Bytes (0 .. udpLen - 1)
         with Import, Address => pktBuf + Storage_Offset (udpOff);
      h : UDP_Header.Header;
   begin
      if udpLen < UDP_Header.Size or else not UDP_Header.Well_Formed (datagram) then
         return;
      end if;
      UDP_Header.Parse (datagram, h);
      --  RFC 768: a zero checksum means none was computed (allowed over
      --  IPv4); any other must verify over the pseudo-header and datagram.
      if h.Checksum /= 0 and then
        Net.transportChecksum (srcIP, dstIP, Net.PROTO_UDP,
                               pktBuf + Storage_Offset (udpOff), h.Length) /= 0
      then
         return;
      end if;

      if Trace_Packets then
         debugPrint ("UDP: ");
         printIP (srcIP);
         debugPrint (":");
         printDec (Unsigned_32 (h.Source_Port));
         debugPrint (" -> port ");
         printDec (Unsigned_32 (h.Destination_Port));
         debugPrint ("" & LF);
      end if;

      --  The UDP length field, not the IPv4 length, bounds the payload.
      --  Resolver replies must come from a configured server's port 53 to
      --  netstack's own resolver port.
      if h.Source_Port = DNS_SERVER_PORT and then isDNSServer (srcIP) and then
        dnsPortPending (h.Destination_Port)
      then
         handleDNSResponse
            (pktBuf + Storage_Offset (udpOff + UDP_Header.Size), h.Length - UDP_Header.Size,
             h.Destination_Port);
      else
         deliverDatagram
            (srcIP, h.Source_Port, h.Destination_Port,
             pktBuf + Storage_Offset (udpOff + UDP_Header.Size), h.Length - UDP_Header.Size);
      end if;
   end handleUDP;

   ---------------------------------------------------------------------------
   --  sendTCPSegment - build TCP segment via RecordFlux, wrap in IPv4
   --
   --  flags encoding: bit 0=FIN, 1=SYN, 2=RST, 3=PSH, 4=ACK. synOptions
   --  adds our MSS option and, with windowScale, our window scale (SYN
   --  segments only).
   ---------------------------------------------------------------------------
   --  Offset of a data segment's payload in its frame: Ethernet, IPv4 and a
   --  TCP header without options.
   TCP_PAYLOAD_OFFSET : constant := 14 + 20 + 20;

   --  Build a TCP segment in its frame at fAddr (Ethernet, IPv4, TCP). With
   --  payloadInPlace the payload is already at fAddr + TCP_PAYLOAD_OFFSET
   --  (no options) and is not copied.
   procedure buildTCPSegment (fAddr   : System.Address;
                              conn    : TCP_Slots.Slot;
                              flags   : Unsigned_8;
                              seqNum  : Unsigned_32;
                              ackNum  : Unsigned_32;
                              window  : Unsigned_16;
                              payload : System.Address;
                              payLen  : Natural;
                              synOptions  : Boolean := False;
                              windowScale : Boolean := False;
                              payloadInPlace : Boolean := False) is
      --  MSS, then NOP and window scale (a multiple of four bytes).
      optLen   : constant Natural :=
        (if not synOptions then 0
         elsif windowScale then TCP_Header.SYN_Options_Size_Scale
         else TCP_Header.SYN_Options_Size_MSS);
      tcpLen   : constant Natural := TCP_Header.Fixed_Size + optLen + payLen;
      frameLen : constant Natural := 14 + 20 + tcpLen;
      --  The segment, in place in its frame (the proved codec writes it).
      segment  : TCP_Header.Bytes (0 .. tcpLen - 1) with Import, Address => fAddr + 34;
   begin
      TCP_Header.Write
        (segment,
         (Source_Port      => conn.localPort,
          Destination_Port => conn.remotePort,
          Seq_No           => seqNum,
          Ack_No           => ackNum,
          Size             => TCP_Header.Fixed_Size + optLen,
          SYN => (flags and TCP_Wire.Flag_SYN) /= 0,
          FIN => (flags and TCP_Wire.Flag_FIN) /= 0,
          RST => (flags and TCP_Wire.Flag_RST) /= 0,
          PSH => (flags and TCP_Wire.Flag_PSH) /= 0,
          ACK => (flags and TCP_Wire.Flag_ACK) /= 0,
          Window           => window,
          others           => <>));
      if synOptions then
         TCP_Header.Write_SYN_Options
           (segment, LINK_MSS, windowScale, Unsigned_8 (OUR_OFFER.Shift_Count));
      end if;
      if payLen > 0 and then not payloadInPlace then
         declare
            src : TCP_Header.Bytes (0 .. payLen - 1) with Import, Address => payload;
         begin
            segment (TCP_Header.Fixed_Size + optLen .. tcpLen - 1) := src;
         end;
      end if;

      --  Compute TCP checksum over pseudo-header + serialized segment
      declare
         cksum : Unsigned_16;
      begin
         cksum := Net.transportChecksum
            (interfaces (0).ipv4, conn.remoteIP, Net.PROTO_TCP, fAddr + 34, tcpLen);
         Net.putU16BE (fAddr, 50, cksum);  -- TCP checksum at 34+16
      end;

      fillIPv4Header (conn.remoteMAC, conn.remoteIP, Net.PROTO_TCP, fAddr, frameLen);
   end buildTCPSegment;

   --  Build a segment straight into the next TX ring slot; only when the
   --  ring is full does it go through a stack frame and the deferred queue.
   procedure sendTCPSegment (slotIn  : TCP_Slots.Slot;
                             flags   : Unsigned_8;
                             seqNum  : Unsigned_32;
                             ackNum  : Unsigned_32;
                             window  : Unsigned_16;
                             payload : System.Address;
                             payLen  : Natural;
                             synOptions  : Boolean := False;
                             windowScale : Boolean := False)
   is
      optLen   : constant Natural :=
        (if not synOptions then 0
         elsif windowScale then TCP_Header.SYN_Options_Size_Scale
         else TCP_Header.SYN_Options_Size_MSS);
      frameLen : constant Natural := 14 + 20 + 20 + optLen + payLen;
      slot     : System.Address;
      conn     : TCP_Slots.Slot := slotIn;
      found    : Boolean;
   begin
      --  A connection opened before its next hop answered ARP: resolve now,
      --  or drop the segment (retransmission sends it again).
      if conn.remoteMAC = Net.ZERO_MAC then
         neighbourMAC (conn.remoteIP, conn.remoteMAC, found);
         if not found then
            return;
         end if;
      end if;
      slot := txReserve (frameLen);
      if slot /= System.Null_Address then
         buildTCPSegment (slot, conn, flags, seqNum, ackNum, window, payload, payLen,
                          synOptions, windowScale);
         txCommit (frameLen);
      else
         declare
            frame : array (0 .. frameLen - 1) of Unsigned_8;
         begin
            buildTCPSegment (frame'Address, conn, flags, seqNum, ackNum, window, payload, payLen,
                             synOptions, windowScale);
            sendFrame (frame'Address, frameLen);
         end;
      end if;
   end sendTCPSegment;

   ---------------------------------------------------------------------------
   --  The TCP engine glue: segments out of each slot's flow
   ---------------------------------------------------------------------------

   function tcpState (Index : TCP_Slots.Connection_Index) return TCP_Connection.State is
     (tcpFlows (Index).E.C.St);

   --  The window we advertise: the receive queue's free space, scaled
   --  (rounded down) once the SYN exchange agreed scaling.
   function windowOf (Index : TCP_Slots.Connection_Index) return Unsigned_16 is
     (TCP_Options.Advertise (Unsigned_32 (tcpFlows (Index).E.C.Rcv_Wnd),
                             agreements (Index).Rcv_Shift));

   --  SYN segments carry an unscaled window (RFC 7323 2.2).
   function synWindowOf (Index : TCP_Slots.Connection_Index) return Unsigned_16 is
     (Unsigned_16 (Seq'Min (tcpFlows (Index).E.C.Rcv_Wnd, Seq (Unsigned_16'Last))));

   procedure sendSyn (Index : TCP_Slots.Connection_Index) is
   begin
      sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_SYN,
                      Unsigned_32 (tcpFlows (Index).E.C.ISS), 0, synWindowOf (Index),
                      System.Null_Address, 0, synOptions => True, windowScale => True);
   end sendSyn;

   procedure sendSynAck (Index : TCP_Slots.Connection_Index) is
   begin
      sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_SYN or TCP_Wire.Flag_ACK,
                      Unsigned_32 (tcpFlows (Index).E.C.ISS),
                      Unsigned_32 (tcpFlows (Index).E.C.Rcv_Nxt), synWindowOf (Index),
                      System.Null_Address, 0, synOptions => True,
                      windowScale => agreements (Index).Scaling);
   end sendSynAck;

   procedure sendAck (Index : TCP_Slots.Connection_Index) is
   begin
      ackOwed (Index) := 0;
      ackDeadline (Index) := Unsigned_64'Last;
      sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_ACK,
                      Unsigned_32 (tcpFlows (Index).E.C.Snd_Nxt),
                      Unsigned_32 (tcpFlows (Index).E.C.Rcv_Nxt), windowOf (Index),
                      System.Null_Address, 0);
   end sendAck;

   --  A window probe (RFC 9293 3.8.6.1): an ACK with the sequence number
   --  just before SND.UNA, which the peer must answer with its current
   --  window, without our sending data outside it.
   procedure sendWindowProbe (Index : TCP_Slots.Connection_Index) is
   begin
      sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_ACK,
                      Unsigned_32 (tcpFlows (Index).E.C.Snd_Una - 1),
                      Unsigned_32 (tcpFlows (Index).E.C.Rcv_Nxt), windowOf (Index),
                      System.Null_Address, 0);
   end sendWindowProbe;

   --  Initial sequence numbers (RFC 6528): a 4-microsecond clock plus a
   --  keyed hash of the connection's addresses and ports.
   function newISS (Index : TCP_Slots.Connection_Index) return Seq is
      function mapped (A : Net.IPv4Address) return TCP_Isn.Address is
        [11 => 16#FF#, 12 => 16#FF#, 13 => A (0), 14 => A (1), 15 => A (2), 16 => A (3),
         others => 0];
      clock : constant Unsigned_64 := syscall (SYSCALL_GETTIME) * 250;
   begin
      return TCP_Isn.Initial_Sequence
        (isnKey,
         (Local       => mapped (interfaces (0).ipv4),
          Remote      => mapped (tcpConns (Index).remoteIP),
          Local_Port  => tcpConns (Index).localPort,
          Remote_Port => tcpConns (Index).remotePort),
         Unsigned_32 (clock and 16#FFFF_FFFF#));
   end newISS;

   --  What our SYN and the peer's (SYN or SYN-ACK) agree.
   function agree (seg : TCP_Slots.SegmentInfo) return TCP_Options.Agreement is
     (TCP_Options.Negotiate (OUR_OFFER, seg.opts, LINK_MSS, IPv6 => False));

   procedure deliverAllArrivals;
   procedure deliverArrivals (lIdx : Network_Channel_Handles.Channel_Index);
   procedure completePendingError (connIdx : Natural);
   procedure failChannels (connIdx : Natural; status : Unsigned_32);
   function addPending (req : PendingRequest) return Boolean;
   procedure tcpPump (Index : TCP_Slots.Connection_Index; ackNeeded : Boolean);

   --  The slot's connection is gone: its chunks go back to the pool.
   procedure freeSlot (Index : TCP_Slots.Connection_Index) is
   begin
      Sends.Release_All (tcpFlows (Index).E.S, tcpPool);
      --  CLOSED, nothing queued or timed; Open_Active or Open_Passive
      --  initializes the rest for the next connection.
      tcpFlows (Index).E.C := (others => <>);
      tcpFlows (Index).Armed := False;
      dropSlot (Index);
      lingerDeadline (Index) := Unsigned_64'Last;
      connectionAuthority (Index) := 0;
      ackOwed (Index) := 0;
      ackDeadline (Index) := Unsigned_64'Last;
      txBlocked (Index) := False;
      agreements (Index) := (others => <>);
   end freeSlot;

   --  TIME-WAIT (RFC 9293 3.6) needs only the addresses and the sequence
   --  numbers, so the slot and its buffers are freed at once.
   function waitKey (ip : Net.IPv4Address; remotePort, localPort : Unsigned_16)
     return TCP_Time_Wait.Tuple is
     ((Remote_IP => policyAddress (ip), Remote_Port => remotePort, Local_Port => localPort));

   procedure enterTimeWait (Index : TCP_Slots.Connection_Index; Now : Unsigned_64) is
      mac : constant Net.MACAddress := tcpConns (Index).remoteMAC;
   begin
      TCP_Time_Wait.Enter
        (timeWaits,
         waitKey (tcpConns (Index).remoteIP, tcpConns (Index).remotePort,
                  tcpConns (Index).localPort),
         [mac (0), mac (1), mac (2), mac (3), mac (4), mac (5)],
         tcpFlows (Index).E.C.Snd_Nxt, tcpFlows (Index).E.C.Rcv_Nxt, Now);
      freeSlot (Index);
   end enterTimeWait;

   --  A segment for a connection in TIME-WAIT: acknowledge it (restarting
   --  the wait for a retransmitted FIN). A RST is ignored (RFC 1337); a new
   --  SYN beyond the old sequence space ends the wait. True if consumed.
   function timeWaitSegment (seg : TCP_Slots.SegmentInfo; Now : Unsigned_64) return Boolean is
      use TCP_Time_Wait;
      found : constant Maybe_Index := Find (timeWaits, waitKey (seg.srcIP, seg.srcPort, seg.dstPort));
      d     : Decision;
   begin
      if found = No_Entry then
         return False;
      end if;
      Arrive (timeWaits, found, seg.flagSYN, seg.flagACK, seg.flagFIN, seg.flagRST,
              Seq (seg.seqNum), Now, d);
      case d is
         when Ignore => return True;
         when Reopen => return False;
         when Acknowledge =>
            declare
               w : constant Waiting := timeWaits (found);
            begin
               sendTCPSegment ((inUse => True, reserved => False, localPort => w.Key.Local_Port,
                                remotePort => w.Key.Remote_Port, remoteIP => seg.srcIP,
                                remoteMAC => [w.Remote_MAC (0), w.Remote_MAC (1), w.Remote_MAC (2),
                                              w.Remote_MAC (3), w.Remote_MAC (4), w.Remote_MAC (5)]),
                               TCP_Wire.Flag_ACK, Unsigned_32 (w.Snd_Nxt), Unsigned_32 (w.Rcv_Nxt),
                               0, System.Null_Address, 0);
            end;
            return True;
      end case;
   end timeWaitSegment;

   --  Caller has detached the backlog/channel before discarding the TCP slot.
   procedure discardConnection (Index : TCP_Slots.Connection_Index) is
   begin
      if tcpConns (Index).inUse and then tcpState (Index) in TCP_Connection.Synchronized_State then
         sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_RST or TCP_Wire.Flag_ACK,
                         Unsigned_32 (tcpFlows (Index).E.C.Snd_Nxt),
                         Unsigned_32 (tcpFlows (Index).E.C.Rcv_Nxt), 0,
                         System.Null_Address, 0);
      end if;
      TCP_Listeners.Remove (listeners, Index);
      parentListener (Index) := 0;
      freeSlot (Index);
   end discardConnection;

   --  After an event: a closed connection's slot is freed once no owner
   --  refers to it; TIME-WAIT moves out of the slot.
   procedure tcpSettle (Index : TCP_Slots.Connection_Index; Now : Unsigned_64) is
   begin
      if not tcpConns (Index).inUse then
         return;
      end if;
      case tcpState (Index) is
         when TCP_Connection.Closed =>
            if not tcpConns (Index).reserved then
               freeSlot (Index);
            end if;
         when TCP_Connection.Time_Wait =>
            if not tcpConns (Index).reserved then
               enterTimeWait (Index, Now);
            elsif lingerDeadline (Index) = Unsigned_64'Last then
               lingerDeadline (Index) := deadlineAfter (Now, TIME_WAIT_MS);
            end if;
         when TCP_Connection.Listen =>
            --  A reset undid a passive open (SYN-RECEIVED to LISTEN).
            discardConnection (Index);
         when others =>
            null;
      end case;
   end tcpSettle;

   --  Give up on a connection (too many retransmissions, or an orphan that
   --  outlived its linger time): reset it and fail waiting requests.
   procedure abortConnection (Index  : TCP_Slots.Connection_Index; Now : Unsigned_64;
                              status : Unsigned_32 := Layout.Status_Timed_Out) is
   begin
      if parentListener (Index) /= 0 then
         discardConnection (Index);
         return;
      end if;
      if tcpState (Index) in TCP_Connection.Synchronized_State then
         sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_RST or TCP_Wire.Flag_ACK,
                         Unsigned_32 (tcpFlows (Index).E.C.Snd_Nxt),
                         Unsigned_32 (tcpFlows (Index).E.C.Rcv_Nxt), 0,
                         System.Null_Address, 0);
      end if;
      --  TODO: an Abort operation in TCP_Flow. CLOSED with an empty queue
      --  keeps the flow valid (every state invariant holds in CLOSED).
      Sends.Release_All (tcpFlows (Index).E.S, tcpPool);
      tcpFlows (Index).E.C.St := TCP_Connection.Closed;
      tcpFlows (Index).Armed := False;
      lingerDeadline (Index) := Unsigned_64'Last;
      completePendingError (Index);
      failChannels (Index, status);
      tcpSettle (Index, Now);
   end abortConnection;

   --  The owner (channel) let go. Handshakes in progress are abandoned; an
   --  established connection finishes closing on its own, for at most
   --  ORPHAN_LINGER_MS.
   procedure releaseConnection (Index : TCP_Slots.Connection_Index) is
      Now : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
   begin
      if not tcpConns (Index).inUse then
         return;
      end if;
      tcpConns (Index).reserved := False;
      connectionAuthority (Index) := 0;
      case tcpState (Index) is
         when TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received | TCP_Connection.Listen =>
            discardConnection (Index);
         when TCP_Connection.Closed | TCP_Connection.Time_Wait =>
            tcpSettle (Index, Now);
         when others =>
            lingerDeadline (Index) := deadlineAfter (Now, ORPHAN_LINGER_MS);
      end case;
   end releaseConnection;

   --  Every slot is taken: say which states hold them (once per failure,
   --  so a flood of opens prints one line each, not per packet).
   procedure reportSlots is
      counts : array (TCP_Connection.State) of Natural := [others => 0];
      owned  : Natural := 0;
   begin
      for Index in tcpConns'Range loop
         if tcpConns (Index).inUse then
            counts (tcpState (Index)) := counts (tcpState (Index)) + 1;
            if tcpConns (Index).reserved then
               owned := owned + 1;
            end if;
         end if;
      end loop;
      debugPrint ("netstack: slots owned=");
      printDec (Unsigned_32 (owned));
      for St in counts'Range loop
         if counts (St) > 0 then
            debugPrint (" " & TCP_Connection.State'Image (St) & "=");
            printDec (Unsigned_32 (counts (St)));
         end if;
      end loop;
      debugPrint ("" & LF);
   end reportSlots;

   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstMAC  : Net.MACAddress;
                        dstPort : Unsigned_16) return Integer is
      lport : Unsigned_16;
      idx   : Integer;
   begin
      lport := nextEphemeralPort;
      --  At most Maximum_Listeners ports are bound, so one more candidate
      --  suffices. Never originate a connection from an admitted listen port.
      for Attempt in 1 .. TCP_Listeners.Maximum_Listeners + 1 loop
         exit when TCP_Listeners.Find (listeners, policyAddress (interfaces (0).ipv4), lport) = 0;
         lport := lport + 1;
         if lport = 0 then lport := 49152; end if;
      end loop;
      nextEphemeralPort := lport + 1;
      if nextEphemeralPort = 0 then
         nextEphemeralPort := 49152;
      end if;

      claimSlot (interfaces (0).ipv4, dstIP, lport, dstPort, dstMAC, idx);
      if idx < 0 then
         return -1;
      end if;
      --  A new incarnation of a 4-tuple still in TIME-WAIT replaces it.
      TCP_Time_Wait.Remove (timeWaits, waitKey (dstIP, dstPort, lport));

      if Trace_Packets then
         debugPrint ("TCP: SYN to ");
         printIP (dstIP);
         debugPrint (":");
         printDec (Unsigned_32 (dstPort));
         debugPrint ("" & LF);
      end if;

      agreements (idx) := (others => <>);
      Flows.Open_Active
        (tcpFlows (idx), tcpPool, TCP_Engine.Owner (idx), newISS (idx),
         TCP_Limits.Default_IPv4_MSS, syscall (SYSCALL_GETTIME));
      lingerDeadline (idx) := Unsigned_64'Last;
      sendSyn (idx);
      return idx;
   end tcpConnect;

   ---------------------------------------------------------------------------
   --  tcpClose - the application closes: FIN after the queued data
   ---------------------------------------------------------------------------
   procedure tcpClose (connIdx : Natural) is
   begin
      if connIdx > tcpConns'Last or else not tcpConns (connIdx).inUse then
         return;
      end if;
      case tcpState (connIdx) is
         when TCP_Connection.Closed | TCP_Connection.Listen =>
            null;
         when TCP_Connection.Syn_Received =>
            --  Not handed to an application yet: abandon the handshake.
            discardConnection (connIdx);
         when others =>
            EP.Close (tcpFlows (connIdx).E, tcpPool);
            tcpPump (connIdx, False);
      end case;
   end tcpClose;

   --  Forward declaration for reply helper (defined later in file)
   procedure replyOKWord
     (to   : ProcessID;
      w0   : Unsigned_64;
      slot : CapabilitySlot := CapabilitySlot'Last);

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
                            Unsigned_64 (Network_Channel_Handles.Value
                              (channelHandles, pendingReqs (i).channelIdx)),
                            pendingReqs (i).replySlot);
            else
               --  Legacy API: reply with raw connIdx
               replyOKWord (pendingReqs (i).sender,
                            Unsigned_64 (connIdx),
                            pendingReqs (i).replySlot);
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
                         Unsigned_64 (Network_Channel_Handles.Value
                           (channelHandles, pendingReqs (i).channelIdx)),
                         pendingReqs (i).replySlot);
            pendingReqs (i).kind := PENDING_NONE;
            exit;
         end if;
      end loop;
   end completePendingConnect;


   --  In-order bytes the application can read now.
   function readable (connIdx : Natural) return Natural is
     (if connIdx not in tcpConns'Range or else
         tcpState (connIdx) in TCP_Connection.Closed | TCP_Connection.Listen |
                                TCP_Connection.Syn_Sent
      then 0 else Receives.Ready (tcpFlows (connIdx).E.R));

   --  No more bytes will arrive: the peer's FIN was consumed, or the
   --  connection is closed.
   function peerFinished (connIdx : Natural) return Boolean is
     (connIdx not in tcpConns'Range or else
      tcpState (connIdx) in TCP_Connection.Closed | EP.Peer_Closed_State);

   --  The application may queue bytes to send.
   function writable (connIdx : Natural) return Boolean is
     (connIdx in tcpConns'Range and then tcpConns (connIdx).inUse and then
      tcpState (connIdx) in TCP_Connection.Established | TCP_Connection.Close_Wait and then
      not tcpFlows (connIdx).E.C.Fin_Pending);

   --  Reading reopened the window: tell the peer once it has grown by two
   --  segments, or from below one segment (receiver SWS avoidance, RFC 1122
   --  4.2.3.3).
   procedure windowUpdate (connIdx : TCP_Slots.Connection_Index; before : Seq) is
      after : constant Seq := tcpFlows (connIdx).E.C.Rcv_Wnd;
   begin
      if tcpState (connIdx) in TCP_Connection.Established | TCP_Connection.Fin_Wait_1 |
                               TCP_Connection.Fin_Wait_2 and then
        TCP_Sequence.Gt (after, before) and then
        (after - before >= 2 * LINK_MSS or else
         (before < LINK_MSS and then after >= LINK_MSS))
      then
         sendAck (connIdx);
      end if;
   end windowUpdate;




   ---------------------------------------------------------------------------
   --  completePendingError - reply ERR to requests waiting on connIdx
   ---------------------------------------------------------------------------
   procedure completePendingError (connIdx : Natural) is
   begin
      for i in pendingReqs'Range loop
         if (pendingReqs (i).kind = PENDING_CONNECT or
             pendingReqs (i).kind = PENDING_OPEN) and
            pendingReqs (i).connIdx = connIdx
         then
            replyError (pendingReqs (i).sender,
                        pendingReqs (i).replySlot);
            --  A failed OPEN never delivered its handle. Return its buffer
            --  acquisition and reservation here; the caller cannot SHUT it.
            if pendingReqs (i).kind in PENDING_CONNECT | PENDING_OPEN and then
              pendingReqs (i).channelIdx in channels'Range
            then
               releaseChannel (pendingReqs (i).channelIdx);
            end if;
            pendingReqs (i).kind := PENDING_NONE;
         end if;
      end loop;
   end completePendingError;

   ---------------------------------------------------------------------------
   --  Sending: queue the application's bytes, and send what the flow allows
   ---------------------------------------------------------------------------

   --  Queue up to len bytes at addr; the count queued (the queue may be full).
   function queueData (connIdx : TCP_Slots.Connection_Index;
                       addr    : System.Address;
                       len     : Natural) return Natural
   is
      n        : constant Natural := Natural'Min (len, Sends.Free (tcpFlows (connIdx).E.S));
      accepted : Sends.Byte_Count := 0;
   begin
      if n > 0 and then writable (connIdx) then
         declare
            data : Sends.Byte_Array (1 .. n) with Import, Address => addr;
         begin
            EP.Write (tcpFlows (connIdx).E, tcpPool, data, accepted);
         end;
      end if;
      return accepted;
   end queueData;

   --  Segments per pump call: bounds the work of one event.
   MAX_BURST : constant := 64;
   txData : Sends.Byte_Array (1 .. LINK_MSS);

   procedure tcpPump (Index : TCP_Slots.Connection_Index; ackNeeded : Boolean) is
      F     : Flows.Flow renames tcpFlows (Index);
      Now   : constant Unsigned_64 := clockNow;
      first : Seq;
      taken : Sends.Byte_Count;
      fin   : Boolean;
      sent  : Boolean := False;
   begin
      --  Nothing to retransmit, nothing unsent and no FIN: no segment to
      --  build (the usual case for a data segment arriving on a receiving
      --  connection). TCP_Endpoint.Send resends from Rtx up to SND.NXT.
      --  CLOSING too: our FIN is unacknowledged there and is retransmitted.
      if F.E.C.St in TCP_Connection.Established | TCP_Connection.Close_Wait |
                     TCP_Connection.Fin_Wait_1 | TCP_Connection.Closing |
                     TCP_Connection.Last_Ack and then
        (F.E.Rtx /= F.E.C.Snd_Nxt or else Sends.Sent (F.E.S) < Sends.Count (F.E.S) or else
         F.E.C.Fin_Pending)
      then
         txBlocked (Index) := False;
         for Burst in 1 .. MAX_BURST loop
            if deferredCount >= TX_HIGH_WATER then
               txBlocked (Index) := True;   --  resumed by tcpResumeBlocked
               exit;
            end if;
            declare
               --  Room for a full segment in the next ring slot: the flow
               --  copies the payload straight to its place in the frame.
               slot : constant System.Address :=
                 txReserve (TCP_PAYLOAD_OFFSET + LINK_MSS);
               inPlace : Sends.Byte_Array (1 .. LINK_MSS)
                  with Import, Address => slot + Storage_Offset (TCP_PAYLOAD_OFFSET);
            begin
               if slot /= System.Null_Address then
                  Flows.Send (F, tcpPool, Now, inPlace, first, taken, fin);
                  exit when taken = 0 and then not fin;
                  buildTCPSegment
                    (slot, tcpConns (Index),
                     TCP_Wire.Flag_ACK or (if taken > 0 then TCP_Wire.Flag_PSH else 0) or
                       (if fin then TCP_Wire.Flag_FIN else 0),
                     Unsigned_32 (first), Unsigned_32 (F.E.C.Rcv_Nxt), windowOf (Index),
                     slot + Storage_Offset (TCP_PAYLOAD_OFFSET), taken, payloadInPlace => True);
                  txCommit (TCP_PAYLOAD_OFFSET + taken);
                  sent := True;
                  goto Next_Segment;
               end if;
            end;
            Flows.Send (F, tcpPool, Now, txData, first, taken, fin);
            exit when taken = 0 and then not fin;
            sendTCPSegment
              (tcpConns (Index),
               TCP_Wire.Flag_ACK or (if taken > 0 then TCP_Wire.Flag_PSH else 0) or
                 (if fin then TCP_Wire.Flag_FIN else 0),
               Unsigned_32 (first), Unsigned_32 (F.E.C.Rcv_Nxt), windowOf (Index),
               txData'Address, taken);
            sent := True;
            <<Next_Segment>>
         end loop;
      end if;
      --  Data segments carry the acknowledgement.
      if sent then
         ackOwed (Index) := 0;
         ackDeadline (Index) := Unsigned_64'Last;
      elsif ackNeeded then
         sendAck (Index);
      end if;
      Flows.Update_Persist (F, clockNow);
   end tcpPump;

   --  The transmit queue drained: connections that stopped send again.
   procedure tcpResumeBlocked is
   begin
      for Index in tcpConns'Range loop
         exit when deferredCount >= TX_HIGH_WATER;
         if txBlocked (Index) and then tcpConns (Index).inUse then
            tcpPump (Index, False);
         end if;
      end loop;
   end tcpResumeBlocked;

   ---------------------------------------------------------------------------
   --  Stream channels: the application and netstack share a send ring and
   --  a receive ring in the channel's grant (CuBit.Net_Channel_Layout,
   --  docs/netstack-redesign.md "Async channels"). The client's indices are
   --  read once per step and accepted only through CuBit.Channel_Rings, so
   --  a client that writes arbitrary values cannot move netstack outside
   --  the rings; it ends its own connection (Status_Protocol_Error).
   ---------------------------------------------------------------------------

   --  x86 may perform a load before an earlier store: order publishing a
   --  flag against re-reading the client's index.
   procedure fullFence with Inline is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end fullFence;

   --  Ring bytes are written before the index that publishes them.
   procedure compilerFence with Inline is
   begin
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
   end compilerFence;

   function headerWord (chIdx : Network_Channel_Handles.Channel_Index; offset : Natural)
     return Unsigned_32
   is
      word : Unsigned_32 with Import, Volatile,
        Address => channels (chIdx).bufAddr + Storage_Offset (offset);
   begin
      return word;
   end headerWord;

   procedure setHeaderWord (chIdx : Network_Channel_Handles.Channel_Index; offset : Natural;
                            value : Unsigned_32)
   is
      word : Unsigned_32 with Import, Volatile,
        Address => channels (chIdx).bufAddr + Storage_Offset (offset);
   begin
      word := value;
   end setHeaderWord;

   --  Ring bases inside the grant (proved in Channel_Geometry: every ring
   --  position is an offset inside the grant setupRings accepted).
   function sendRing (chIdx : Network_Channel_Handles.Channel_Index) return System.Address is
     (channels (chIdx).bufAddr + Storage_Offset
        (Channel_Geometry.Send_Offset
           (channels (chIdx).bufSize, channels (chIdx).tx.Size, channels (chIdx).rx.Size, 0)));

   function receiveRing (chIdx : Network_Channel_Handles.Channel_Index) return System.Address is
     (channels (chIdx).bufAddr + Storage_Offset
        (Channel_Geometry.Receive_Offset
           (channels (chIdx).bufSize, channels (chIdx).tx.Size, channels (chIdx).rx.Size, 0)));

   --  Set up a stream channel's rings from the sizes and wait bit in its
   --  header, read once, here. False if they are invalid or do not fit.
   function setupRings (chIdx : Network_Channel_Handles.Channel_Index) return Boolean is
      C      : NetChannel renames channels (chIdx);
      txSize : constant Unsigned_32 := headerWord (chIdx, Layout.Tx_Size_At);
      rxSize : constant Unsigned_32 := headerWord (chIdx, Layout.Rx_Size_At);
      bit    : constant Unsigned_32 := headerWord (chIdx, Layout.Wait_Bit_At);
   begin
      if C.bufSize > Channel_Geometry.Maximum_Grant or else
        not Rings.Valid_Size (Unsigned_64 (txSize)) or else
        not Rings.Valid_Size (Unsigned_64 (rxSize)) or else
        bit > Layout.Maximum_Wait_Bit or else
        not Channel_Geometry.Fits (C.bufSize, Natural (txSize), Natural (rxSize))
      then
         return False;
      end if;
      C.stream := True;
      C.tx := Rings.New_Consumer (Natural (txSize));
      C.rx := Rings.New_Producer (Natural (rxSize));
      C.waitBit := Natural (bit);
      C.status := Layout.Status_Opening;
      --  Nothing to send yet: the client's first write kicks us.
      C.kickFlags := Layout.Kick_On_Send;
      setHeaderWord (chIdx, Layout.Tx_Consumed_At, 0);
      setHeaderWord (chIdx, Layout.Rx_Produced_At, 0);
      setHeaderWord (chIdx, Layout.Kick_Wanted_At, Layout.Kick_On_Send);
      setHeaderWord (chIdx, Layout.Status_At, Layout.Status_Opening);
      return True;
   end setupRings;

   function finalStatus (status : Unsigned_32) return Boolean is
     (status >= Layout.Status_Peer_Finished);

   function failedStatus (status : Unsigned_32) return Boolean is
     (status >= Layout.Status_Reset);

   procedure setStatus (chIdx : Network_Channel_Handles.Channel_Index; status : Unsigned_32) is
   begin
      if channels (chIdx).status /= status then
         channels (chIdx).status := status;
         setHeaderWord (chIdx, Layout.Status_At, status);
      end if;
   end setStatus;

   --  The connection's state as the channel reports it. A failure is final;
   --  the peer's end of stream shows once its last byte is in the ring.
   function streamStatus (chIdx : Network_Channel_Handles.Channel_Index) return Unsigned_32 is
      C : NetChannel renames channels (chIdx);
   begin
      if failedStatus (C.status) then
         return C.status;
      elsif C.kind = CHANNEL_LISTENER then
         return Layout.Status_Open;
      elsif C.proto = Net.PROTO_UDP then
         return (if UDP_Channels.Active (udpChannels, chIdx) then Layout.Status_Open
                 else Layout.Status_Opening);
      elsif C.connIdx not in tcpConns'Range or else
        tcpState (C.connIdx) in TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received |
                                TCP_Connection.Listen
      then
         return Layout.Status_Opening;
      elsif peerFinished (C.connIdx) and then readable (C.connIdx) = 0 then
         return Layout.Status_Peer_Finished;
      else
         return Layout.Status_Open;
      end if;
   end streamStatus;

   --  Whether the channel has what its client asked to hear about.
   function channelReady (chIdx : Network_Channel_Handles.Channel_Index) return Boolean is
      C    : NetChannel renames channels (chIdx);
      want : constant Unsigned_32 := headerWord (chIdx, Layout.Want_At);
   begin
      return C.stream and then
        (((want and Layout.Want_Readable) /= 0 and then
          (C.rx.Fill > 0 or else finalStatus (C.status))) or else
         ((want and Layout.Want_Writable) /= 0 and then
          (C.tx.Available < C.tx.Size or else failedStatus (C.status))));
   end channelReady;

   function waitBitOf (chIdx : Network_Channel_Handles.Channel_Index) return Unsigned_64 is
     (Shift_Left (Unsigned_64'(1), channels (chIdx).waitBit));

   --  The wait bits of owner's ready channels among interest.
   function readyMask (owner : ProcessID; interest : Unsigned_64) return Unsigned_64 is
      mask : Unsigned_64 := 0;
   begin
      for I in channels'Range loop
         if channels (I).pid = owner and then (interest and waitBitOf (I)) /= 0 and then
           channelReady (I)
         then
            mask := mask or waitBitOf (I);
         end if;
      end loop;
      return mask;
   end readyMask;

   --  Complete owner's waiting WAIT if any of its channels is ready.
   procedure notifyWaiter (owner : ProcessID) is
      mask : Unsigned_64;
   begin
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then P.sender = owner then
            mask := readyMask (owner, P.waitMask);
            if mask /= 0 then
               replyOKWord (P.sender, mask, P.replySlot);
               P.kind := PENDING_NONE;
            end if;
            return;
         end if;
      end loop;
   end notifyWaiter;

   --  Move queued send-ring bytes into the connection's send queue.
   procedure pullSend (chIdx : Network_Channel_Handles.Channel_Index) is
      C     : NetChannel renames channels (chIdx);
      conn  : constant TCP_Slots.Connection_Index := C.connIdx;
      first, l1, l2 : Natural;
      took  : Natural;
   begin
      if C.tx.Available = 0 or else not writable (conn) then
         return;
      end if;
      Rings.Data_Slices (C.tx, first, l1, l2);
      took := queueData (conn, sendRing (chIdx) + Storage_Offset (first), l1);
      if took = l1 and then l2 > 0 then
         took := took + queueData (conn, sendRing (chIdx), l2);
      end if;
      if took > 0 then
         Rings.Consume (C.tx, took);
         setHeaderWord (chIdx, Layout.Tx_Consumed_At, Unsigned_32 (C.tx.Consumed));
         tcpPump (conn, False);
      end if;
   end pullSend;

   --  Move received in-order bytes into the receive ring.
   procedure pushReceive (chIdx : Network_Channel_Handles.Channel_Index) is
      C      : NetChannel renames channels (chIdx);
      conn   : constant TCP_Slots.Connection_Index := C.connIdx;
      first, l1, l2 : Natural;
      total  : Natural := 0;
      before : Seq;
   begin
      if readable (conn) = 0 or else Rings.Space (C.rx) = 0 then
         return;
      end if;
      before := tcpFlows (conn).E.C.Rcv_Wnd;
      Rings.Free_Slices (C.rx, first, l1, l2);
      for Part in 1 .. 2 loop
         declare
            room : constant Natural := (if Part = 1 then l1 else l2);
            len  : constant Natural := Natural'Min (readable (conn), room);
            got  : Receives.Byte_Count := 0;
         begin
            exit when len = 0;
            declare
               output : Receives.Byte_Array (1 .. len) with Import,
                 Address => receiveRing (chIdx) +
                            Storage_Offset (if Part = 1 then first else 0);
            begin
               EP.Read (tcpFlows (conn).E, tcpPool, output, got);
            end;
            total := total + got;
            exit when got < room;
         end;
      end loop;
      if total > 0 then
         Rings.Commit (C.rx, total);
         compilerFence;
         setHeaderWord (chIdx, Layout.Rx_Produced_At, Unsigned_32 (C.rx.Produced));
         windowUpdate (conn, before);
      end if;
   end pushReceive;

   --  Connected UDP: send the datagrams queued in the send ring. One
   --  longer than an unfragmented datagram is dropped; a malformed record
   --  (the client broke the ring rules) ends the channel.
   procedure pullDatagrams (chIdx : Network_Channel_Handles.Channel_Index) is
      C      : NetChannel renames channels (chIdx);
      ring   : Rings.Bytes (0 .. C.tx.Size - 1)
        with Import, Address => sendRing (chIdx);
      buffer : Rings.Bytes (1 .. UDP_Channels.Maximum_Payload);
      len    : Natural;
      truncated : Boolean;
      result : Datagrams.Take_Result;
      took   : Boolean := False;
      use type Datagrams.Take_Result;
   begin
      loop
         Datagrams.Take (C.tx, ring, buffer, len, truncated, result);
         exit when result /= Datagrams.Taken;
         took := True;
         if not truncated then
            declare
               mac   : Net.MACAddress;
               found : Boolean;
            begin
               --  Unresolved, the datagram is lost, as UDP allows; the
               --  request sent now resolves the next one.
               neighbourMAC (C.remoteIP, mac, found);
               if found then
                  sendUDP (C.remoteIP, mac, C.localPort, C.remotePort, buffer'Address, len);
               end if;
            end;
         end if;
      end loop;
      if result = Datagrams.Malformed then
         setStatus (chIdx, Layout.Status_Protocol_Error);
      end if;
      if took then
         setHeaderWord (chIdx, Layout.Tx_Consumed_At, Unsigned_32 (C.tx.Consumed));
      end if;
   end pullDatagrams;

   --  A datagram for a connected UDP channel: into its receive ring, or
   --  dropped if the ring is full.
   procedure putDatagram (chIdx : Network_Channel_Handles.Channel_Index;
                          payload : System.Address; len : Natural) is
      C      : NetChannel renames channels (chIdx);
      data   : Rings.Bytes (1 .. len) with Import, Address => payload;
      result : Datagrams.Put_Result;
      ok     : Boolean;
      use type Datagrams.Put_Result;
   begin
      if not C.stream or else failedStatus (C.status) then
         return;
      end if;
      Rings.Accept_Consumed
        (C.rx, Rings.Index (headerWord (chIdx, Layout.Rx_Consumed_At)), ok);
      if not ok then
         setStatus (chIdx, Layout.Status_Protocol_Error);
         return;
      end if;
      declare
         ring : Rings.Bytes (0 .. C.rx.Size - 1)
           with Import, Address => receiveRing (chIdx);
      begin
         Datagrams.Put (C.rx, ring, data, result);
      end;
      if result = Datagrams.Put then
         compilerFence;
         setHeaderWord (chIdx, Layout.Rx_Produced_At, Unsigned_32 (C.rx.Produced));
         if channelReady (chIdx) then
            notifyWaiter (C.pid);
         end if;
      end if;
   end putDatagram;

   --  Bring a stream channel up to date: take the client's new indices,
   --  move data both ways, send FIN once asked and drained, publish the
   --  status and which kicks we need, and wake the client if it waits.
   procedure serviceChannel (chIdx : Network_Channel_Handles.Channel_Index) is
      C     : NetChannel renames channels (chIdx);
      ok    : Boolean;
      flags : Unsigned_32;
   begin
      if not C.stream then
         return;
      end if;
      --  A second pass only if the client moved an index while we set a
      --  kick flag (it may have looked at the flag before we set it).
      for Pass in 1 .. 2 loop
         Rings.Accept_Produced
           (C.tx, Rings.Index (headerWord (chIdx, Layout.Tx_Produced_At)), ok);
         --  With nothing of a stream's left unconsumed in its receive ring
         --  as last seen, the client's consumed index (on a line it writes)
         --  has nothing to tell: it is not read. Otherwise it is, since
         --  readiness (channelReady) counts what the client has not read.
         if ok and then
           (C.kind = CHANNEL_LISTENER or else C.connIdx not in tcpConns'Range
            or else C.rx.Fill > 0)
         then
            Rings.Accept_Consumed
              (C.rx, Rings.Index (headerWord (chIdx, Layout.Rx_Consumed_At)), ok);
         end if;
         if not ok then
            setStatus (chIdx, Layout.Status_Protocol_Error);
            if C.connIdx in tcpConns'Range then
               abortConnection (C.connIdx, clockNow);
            end if;
         end if;
         if not failedStatus (C.status) and then C.kind = CHANNEL_LISTENER then
            deliverArrivals (chIdx);
         elsif not failedStatus (C.status) and then C.proto = Net.PROTO_UDP then
            pullDatagrams (chIdx);
         elsif not failedStatus (C.status) and then C.connIdx in tcpConns'Range then
            pullSend (chIdx);
            pushReceive (chIdx);
            if not C.shutDone and then C.tx.Available = 0 and then
              headerWord (chIdx, Layout.Shut_Write_At) /= 0 and then
              tcpState (C.connIdx) not in TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received
            then
               C.shutDone := True;
               tcpClose (C.connIdx);
            end if;
         end if;
         setStatus (chIdx, streamStatus (chIdx));

         flags := 0;
         if not failedStatus (C.status) then
            if C.tx.Available = 0 then
               flags := flags or Layout.Kick_On_Send;
            end if;
            if C.connIdx in tcpConns'Range and then readable (C.connIdx) > 0 and then
              Rings.Space (C.rx) = 0
            then
               flags := flags or Layout.Kick_On_Receive;
            end if;
         end if;
         if flags /= C.kickFlags then
            C.kickFlags := flags;
            setHeaderWord (chIdx, Layout.Kick_Wanted_At, flags);
         end if;
         exit when flags = 0;
         fullFence;
         exit when
           ((flags and Layout.Kick_On_Send) = 0 or else
            headerWord (chIdx, Layout.Tx_Produced_At) = Unsigned_32 (Rings.Produced (C.tx))) and then
           ((flags and Layout.Kick_On_Receive) = 0 or else
            headerWord (chIdx, Layout.Rx_Consumed_At) = Unsigned_32 (Rings.Consumed (C.rx)));
      end loop;
      if channelReady (chIdx) then
         notifyWaiter (C.pid);
      end if;
   end serviceChannel;

   --  The channels of a connection whose state or queues changed.
   procedure tcpNotify (connIdx : TCP_Slots.Connection_Index) is
   begin
      for I in channels'Range loop
         if channels (I).stream and then channels (I).connIdx = connIdx then
            declare
               mark : constant Unsigned_64 := Prof.Now;
            begin
               serviceChannel (I);
               Prof.Charge (Prof.Service, mark);
            end;
         end if;
      end loop;
   end tcpNotify;

   --  The connection failed: its channels report why.
   procedure failChannels (connIdx : Natural; status : Unsigned_32) is
   begin
      for I in channels'Range loop
         if channels (I).stream and then channels (I).connIdx = connIdx and then
           not failedStatus (channels (I).status)
         then
            setStatus (I, status);
            if channelReady (I) then
               notifyWaiter (channels (I).pid);
            end if;
         end if;
      end loop;
   end failChannels;

   --  Complete owner's waiting WAIT now, whatever is ready.
   procedure endWait (owner : ProcessID) is
   begin
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then P.sender = owner then
            replyOKWord (P.sender, readyMask (owner, P.waitMask), P.replySlot);
            P.kind := PENDING_NONE;
         end if;
      end loop;
   end endWait;

   --  A client kicked (or waited with a kick mask): service its channels
   --  whose wait bits are in mask.
   procedure kickChannels (owner : ProcessID; mask : Unsigned_64) is
   begin
      if mask = 0 then
         return;
      end if;
      for I in channels'Range loop
         if channels (I).stream and then channels (I).pid = owner and then
           (mask and waitBitOf (I)) /= 0
         then
            serviceChannel (I);
         end if;
      end loop;
   end kickChannels;

   --  WAIT: complete now if a channel is ready, else when one becomes
   --  ready or at the deadline. One WAIT per process.
   procedure handleNetWait (snd : ProcessID; m : Message) is
      mask : Unsigned_64;
   begin
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then P.sender = snd then
            replyError (snd);
            return;
         end if;
      end loop;
      kickChannels (snd, m.words (0));
      mask := readyMask (snd, m.words (2));
      if mask /= 0 or else m.words (1) <= clockNow then
         replyOKWord (snd, mask);
      elsif not addPending ((kind => PENDING_WAIT, sender => snd,
                             waitDeadline => m.words (1), waitMask => m.words (2),
                             others => <>))
      then
         replyError (snd);
      end if;
   end handleNetWait;

   ---------------------------------------------------------------------------
   --  Events: an arriving segment's results, and the timers
   ---------------------------------------------------------------------------

   --  delayable: the segment carried only new in-order data, so its
   --  acknowledgement may wait for the next segment or the delayed-ACK timer.
   procedure tcpEvent (Index     : TCP_Slots.Connection_Index;
                       O         : TCP_Connection.Outcome;
                       was       : TCP_Connection.State;
                       Now       : Unsigned_64;
                       delayable : Boolean := False)
   is
      ackNeeded : Boolean := False;
   begin
      case O.Answer is
         when TCP_Connection.No_Reply =>
            null;
         when TCP_Connection.Send_Ack =>
            if delayable and then inRxBatch then
               ackOwed (Index) := ackOwed (Index) + 1;
               if ackOwed (Index) = 1 then
                  ackDeadline (Index) := deadlineAfter (Now, DELAYED_ACK_MS);
               end if;
            elsif delayable and then ackOwed (Index) = 0 then
               ackOwed (Index) := 1;
               ackDeadline (Index) := deadlineAfter (Now, DELAYED_ACK_MS);
            else
               ackNeeded := True;
            end if;
         when TCP_Connection.Send_Challenge_Ack =>
            ackNeeded := True;
         when TCP_Connection.Send_Syn_Ack =>
            sendSynAck (Index);
         when TCP_Connection.Send_Reset =>
            sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_RST, Unsigned_32 (O.Reset_Seq), 0, 0,
                            System.Null_Address, 0);
         when TCP_Connection.Send_Reset_Ack =>
            sendTCPSegment (tcpConns (Index), TCP_Wire.Flag_RST or TCP_Wire.Flag_ACK, 0,
                            Unsigned_32 (O.Reset_Ack), 0, System.Null_Address, 0);
      end case;

      if was in TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received and then
        tcpState (Index) in TCP_Connection.Established .. TCP_Connection.Time_Wait
      then
         if Trace_Packets then
            debugPrint ("TCP: ESTABLISHED with ");
            printIP (tcpConns (Index).remoteIP);
            debugPrint (":");
            printDec (Unsigned_32 (tcpConns (Index).remotePort));
            debugPrint ("" & LF);
         end if;
         completePendingConnect (Index);
         TCP_Listeners.Mark_Ready (listeners, Index);
      end if;

      if O.Happened in TCP_Connection.Refused | TCP_Connection.Reset then
         debugPrint (if O.Happened = TCP_Connection.Refused then "TCP: refused by "
                     else "TCP: reset by ");
         printIP (tcpConns (Index).remoteIP);
         debugPrint (":");
         printDec (Unsigned_32 (tcpConns (Index).remotePort));
         debugPrint ("" & LF);
         completePendingError (Index);
         failChannels (Index, Layout.Status_Reset);
         if parentListener (Index) /= 0 then
            discardConnection (Index);
            return;
         end if;
      end if;

      if tcpConns (Index).inUse then
         tcpPump (Index, ackNeeded);
         if inRxBatch then
            if not batchTouched (Index) then
               batchTouched (Index) := True;
               touchedList (touchedCount) := Index;
               touchedCount := touchedCount + 1;
            end if;
         else
            tcpNotify (Index);
         end if;
         tcpSettle (Index, Now);
      end if;
   end tcpEvent;

   --  The end of a received batch: the acknowledgements owed (at least
   --  every second segment, RFC 5681 4.2) and the waiting readers.
   procedure tcpBatchDone is
   begin
      inRxBatch := False;
      for K in 0 .. touchedCount - 1 loop
         declare
            Index : constant TCP_Slots.Connection_Index := touchedList (K);
         begin
            batchTouched (Index) := False;
            if tcpConns (Index).inUse then
               --  Move the data to its reader first, so the acknowledgement
               --  advertises the window that frees (one ACK, not two).
               tcpNotify (Index);
               if tcpConns (Index).inUse and then ackOwed (Index) >= 2 then
                  sendAck (Index);
               end if;
            end if;
         end;
      end loop;
      touchedCount := 0;
   end tcpBatchDone;

   procedure tcpArrive (Index  : TCP_Slots.Connection_Index;
                        seg    : TCP_Slots.SegmentInfo;
                        pktBuf : System.Address;
                        Now    : Unsigned_64)
   is
      F   : Flows.Flow renames tcpFlows (Index);
      was : constant TCP_Connection.State := F.E.C.St;
      S   : constant TCP_Connection.Segment :=
        (Seq_No => Seq (seg.seqNum), Ack_No => Seq (seg.ackNum),
         SYN => seg.flagSYN, ACK => seg.flagACK, FIN => seg.flagFIN, RST => seg.flagRST,
         Length => Seq (seg.dataLen),
         Window => Seq (TCP_Options.Peer_Window (seg.winSize, seg.flagSYN, agreements (Index))));
      payload : Receives.Byte_Array (1 .. seg.dataLen)
         with Import, Address => pktBuf + Storage_Offset (seg.dataOff);
      O : TCP_Connection.Outcome;
      mark : Unsigned_64 := Prof.Now;
   begin
      Flows.Arrive (F, tcpPool, Now, S, payload,
                    (if was = TCP_Connection.Listen then newISS (Index) else 0), O);
      --  The peer is answering; whether data is still blocked by its
      --  window is settled after this segment (and any sending it allows).
      if seg.flagACK then
         Flows.Probe_Answered (F);
      end if;
      Flows.Update_Persist (F, Now);
      Prof.Charge (Prof.Arrive, mark);
      mark := Prof.Now;
      if was = TCP_Connection.Syn_Sent and then
        F.E.C.St in TCP_Connection.Established .. TCP_Connection.Time_Wait
      then
         --  The SYN-ACK's options: our segment size (the flow opened with
         --  the default) and window scaling.
         agreements (Index) := agree (seg);
         TCP_Congestion.Initialize
           (F.CC, TCP_Congestion.Segment_Size (agreements (Index).Send_MSS), F.E.C.Snd_Una);
      end if;
      tcpEvent (Index, O, was, Now,
                delayable => seg.dataLen > 0 and then not seg.flagFIN and then not seg.flagSYN and then
                             O.Skip = 0 and then Natural (O.Count) = seg.dataLen);
      Prof.Charge (Prof.Event, mark);
   end tcpArrive;

   --  Retransmission timers, TIME-WAIT, and orphaned connections' linger.
   procedure tcpTimers (Now : Unsigned_64) is
   begin
      for Index in tcpConns'Range loop
         if tcpConns (Index).inUse then
            declare
               F : Flows.Flow renames tcpFlows (Index);
            begin
               if Now >= lingerDeadline (Index) then
                  if F.E.C.St = TCP_Connection.Time_Wait then
                     TCP_Connection.Time_Wait_Expired (F.E.C);
                     lingerDeadline (Index) := Unsigned_64'Last;
                     tcpNotify (Index);
                     tcpSettle (Index, Now);
                  else
                     abortConnection (Index, Now);
                  end if;
               elsif ackOwed (Index) > 0 and then Now >= ackDeadline (Index) then
                  sendAck (Index);
               elsif F.Persisting and then Now >= F.Persist_At then
                  declare
                     giveUp : Boolean;
                  begin
                     Flows.Persist_Timeout (F, Now, giveUp);
                     if giveUp then
                        abortConnection (Index, Now);
                     else
                        sendWindowProbe (Index);
                     end if;
                  end;
               elsif F.Armed and then Now >= F.Deadline and then
                 F.E.C.St in TCP_Connection.Syn_Sent | TCP_Connection.Synchronized_State
               then
                  if Flows.Exhausted (F) then
                     abortConnection (Index, Now);
                  else
                     Flows.Retransmit_Timeout (F, tcpPool, Now);
                     case F.E.C.St is
                        when TCP_Connection.Syn_Sent     => sendSyn (Index);
                        when TCP_Connection.Syn_Received => sendSynAck (Index);
                        when others                      => tcpPump (Index, False);
                     end case;
                  end if;
               end if;
            end;
         end if;
      end loop;
      TCP_Time_Wait.Expire (timeWaits, Now);
   end tcpTimers;

   function tcpNextDeadline return Unsigned_64 is
      deadline : Unsigned_64 := Unsigned_64'Last;
   begin
      for Index in tcpConns'Range loop
         if tcpConns (Index).inUse then
            deadline := Unsigned_64'Min (deadline, lingerDeadline (Index));
            if ackOwed (Index) > 0 then
               deadline := Unsigned_64'Min (deadline, ackDeadline (Index));
            end if;
            if tcpFlows (Index).Armed and then
              tcpState (Index) in TCP_Connection.Syn_Sent | TCP_Connection.Synchronized_State
            then
               deadline := Unsigned_64'Min (deadline, tcpFlows (Index).Deadline);
            end if;
            if tcpFlows (Index).Persisting then
               deadline := Unsigned_64'Min (deadline, tcpFlows (Index).Persist_At);
            end if;
         end if;
      end loop;
      return Unsigned_64'Min (deadline, TCP_Time_Wait.Next_Deadline (timeWaits));
   end tcpNextDeadline;

   --  Resets for segments that belong to no connection (TCP_Reset, RFC
   --  9293 3.10.7.1), at most RST_PER_MS per millisecond in bursts of
   --  RST_BURST, so a flood cannot turn netstack into a reflector.
   RST_PER_MS  : constant := 1;
   RST_BURST   : constant := 50;
   rstTokens   : Natural := RST_BURST;
   rstRefillAt : Unsigned_64 := 0;

   procedure refuseSegment (seg    : TCP_Slots.SegmentInfo;
                            dstIP  : Net.IPv4Address;
                            srcMAC : Net.MACAddress;
                            Now    : Unsigned_64) is
      reply : constant TCP_Reset.Reply :=
        TCP_Reset.For_Closed
          ((SYN => seg.flagSYN, ACK => seg.flagACK, FIN => seg.flagFIN, RST => seg.flagRST,
            Seq_No => Seq (seg.seqNum), Ack_No => Seq (seg.ackNum), Length => seg.dataLen));
   begin
      if not reply.Send or else findInterfaceForIP (dstIP) < 0 then
         return;
      end if;
      if Now > rstRefillAt then
         rstTokens := Natural'Min
           (RST_BURST, rstTokens + Natural (Unsigned_64'Min
              (Unsigned_64 (RST_BURST), (Now - rstRefillAt) * RST_PER_MS)));
         rstRefillAt := Now;
      end if;
      if rstTokens = 0 then
         return;
      end if;
      rstTokens := rstTokens - 1;
      sendTCPSegment ((inUse => True, reserved => False, localPort => seg.dstPort,
                       remotePort => seg.srcPort, remoteIP => seg.srcIP, remoteMAC => srcMAC),
                      (if reply.With_ACK then TCP_Wire.Flag_RST or TCP_Wire.Flag_ACK
                       else TCP_Wire.Flag_RST),
                      Unsigned_32 (reply.Seq_No), Unsigned_32 (reply.Ack_No), 0,
                      System.Null_Address, 0);
   end refuseSegment;

   ---------------------------------------------------------------------------
   --  handleTCP - parse TCP segment via RecordFlux, drive state machine
   ---------------------------------------------------------------------------
   procedure handleTCP (pktBuf     : System.Address;
                        ipOff      : Natural;
                        ipHdrLen   : Natural;
                        srcIP      : Net.IPv4Address;
                        dstIP      : Net.IPv4Address;
                        srcMAC     : Net.MACAddress;
                        totalIPLen : Natural) is
      tcpOff : constant Natural := ipOff + ipHdrLen;
      tcpLen : constant Natural := totalIPLen - ipHdrLen;

      seg     : TCP_Slots.SegmentInfo;
      connIdx : Integer := -1;
      dataLen : Natural;
      Now     : constant Unsigned_64 := clockNow;
      mark    : Unsigned_64 := Prof.Now;
   begin
      --  The connection table/TX path currently supports interface zero only.
      --  Broadcast and another interface must not match a unicast TCP tuple.
      if tcpLen < 20 or else numIfaces = 0 or else interfaces (0).state /= IF_UP or else
        dstIP /= interfaces (0).ipv4
      then
         return;
      end if;

      --  RecordFlux validates the layout, not the TCP pseudo-header checksum.
      --  Validate before a packet can acknowledge data or change TCP state.
      if Net.transportChecksum
        (srcIP, dstIP, Net.PROTO_TCP,
         pktBuf + Storage_Offset (tcpOff), tcpLen) /= 0
      then
         return;
      end if;
      Prof.Charge (Prof.Checksum, mark);
      mark := Prof.Now;

      declare
         --  The segment in place in the received frame (proved codec).
         segment : TCP_Header.Bytes (0 .. tcpLen - 1)
            with Import, Address => pktBuf + Storage_Offset (tcpOff);
         h : TCP_Header.Header;
      begin
         if not TCP_Header.Well_Formed (segment) then
            --  Counted, not printed per segment: a flood must not become a
            --  console flood.
            tcpMalformed := tcpMalformed + 1;
            if (tcpMalformed and 16#FFF#) = 1 then
               debugPrint ("TCP: malformed segments so far: ");
               printDec (Unsigned_32 (tcpMalformed and 16#FFFF_FFFF#));
               debugPrint ("" & LF);
            end if;
            return;
         end if;
         TCP_Header.Parse (segment, h);

         seg.srcIP   := srcIP;
         seg.srcPort := h.Source_Port;
         seg.dstPort := h.Destination_Port;
         seg.seqNum  := h.Seq_No;
         seg.ackNum  := h.Ack_No;
         seg.flagSYN := h.SYN;
         seg.flagACK := h.ACK;
         seg.flagFIN := h.FIN;
         seg.flagRST := h.RST;
         seg.winSize := h.Window;
         dataLen     := tcpLen - h.Size;
         seg.dataLen := dataLen;
         seg.dataOff := tcpOff + h.Size;
         --  Options matter only on a SYN (RFC 9293 3.7.1, RFC 7323 2.2).
         if seg.flagSYN and then h.Size > TCP_Header.Fixed_Size then
            declare
               opts : TCP_Wire.Bytes (0 .. h.Size - TCP_Header.Fixed_Size - 1)
                  with Import, Address => pktBuf + Storage_Offset (tcpOff + TCP_Header.Fixed_Size);
            begin
               TCP_Wire.Parse (opts, seg.opts);
            end;
         end if;
      end;

      Prof.Charge (Prof.Parse, mark);
      mark := Prof.Now;
      declare
         s : constant Conns.Maybe_Slot :=
           Conns.Find (connTable, tupleOf (dstIP, srcIP, seg.dstPort, seg.srcPort));
      begin
         connIdx := (if s = Conns.No_Slot then -1 else s - 1);
      end;
      Prof.Charge (Prof.Lookup, mark);
      Prof.Packet;
      if connIdx < 0 then
         if timeWaitSegment (seg, Now) then
            return;
         end if;
         --  Nothing is ever sent to a zero, multicast or broadcast source.
         if seg.srcPort = 0 or else srcIP (0) = 0 or else srcIP (0) >= 224 then
            return;
         end if;
         --  Only a plain SYN can open; anything else here is for no
         --  connection, and is refused (a RST never is).
         if not seg.flagSYN or else seg.flagACK or else seg.flagRST or else seg.flagFIN then
            refuseSegment (seg, dstIP, srcMAC, Now);
            return;
         end if;
         declare
            listener : constant TCP_Listeners.Handle :=
              TCP_Listeners.Find (listeners, policyAddress (dstIP), seg.dstPort);
            reserved : Boolean;
         begin
            if listener = TCP_Listeners.No_Handle then
               refuseSegment (seg, dstIP, srcMAC, Now);   --  a closed port
               return;
            end if;
            claimSlot (dstIP, srcIP, seg.dstPort, seg.srcPort, srcMAC, connIdx);
            if connIdx < 0 then return; end if;
            TCP_Listeners.Reserve
              (listeners, listener, connIdx,
               deadlineAfter (Now, HANDSHAKE_TIMEOUT_MS), reserved);
            if not reserved then
               dropSlot (connIdx);
               return;
            end if;
            parentListener (connIdx) := listener;
            connectionAuthority (connIdx) := 0;
            lingerDeadline (connIdx) := Unsigned_64'Last;
            agreements (connIdx) := agree (seg);
            Flows.Open_Passive
              (tcpFlows (connIdx), tcpPool, TCP_Engine.Owner (connIdx),
               TCP_Congestion.Segment_Size (agreements (connIdx).Send_MSS));
         end;
      end if;

      tcpArrive (connIdx, seg, pktBuf, Now);
      --  Finish buffering all final-ACK payload/FIN actions before handing the
      --  connection to its owner.
      deliverAllArrivals;
   end handleTCP;

   --  hop's link address is now known: connections that opened before it
   --  was take it, and resolver questions to a server behind it go again
   --  now instead of at their next retry.
   procedure neighbourResolved (hop : Net.IPv4Address; mac : Net.MACAddress) is
   begin
      for I in tcpConns'Range loop
         if tcpConns (I).inUse and then tcpConns (I).remoteMAC = Net.ZERO_MAC and then
           nextHopOf (tcpConns (I).remoteIP) = hop
         then
            tcpConns (I).remoteMAC := mac;
         end if;
      end loop;
      for P of pendingReqs loop
         if P.kind in PENDING_RESOLVE | PENDING_OPEN and then P.attempts in 1 .. DNS_ATTEMPTS - 1
           and then nextHopOf (dnsServer (P.attempts - 1)) = hop
         then
            sendAttempt (P, clockNow);
         end if;
      end loop;
   end neighbourResolved;

   ---------------------------------------------------------------------------
   --  handleARP
   ---------------------------------------------------------------------------
   procedure handleARP (pktBuf : System.Address; pktLen : Natural) is
      packet : ARP_Packet.Bytes (0 .. pktLen - 15)
         with Import, Address => pktBuf + 14;
      p      : ARP_Packet.Packet;
      ours   : Boolean;
      mac    : Net.MACAddress;
      senderIP, targetIP : Net.IPv4Address;
   begin
      if pktLen < 14 + ARP_Packet.Size or else not ARP_Packet.Well_Formed (packet) then
         return;
      end if;
      ARP_Packet.Parse (packet, p);
      senderIP := [p.Sender_IP (0), p.Sender_IP (1), p.Sender_IP (2), p.Sender_IP (3)];
      targetIP := [p.Target_IP (0), p.Target_IP (1), p.Target_IP (2), p.Target_IP (3)];
      ours := findInterfaceForIP (targetIP) >= 0;
      --  Learn only what the proved cache accepts (no unsolicited replies,
      --  no rewriting a resolved address, no bogus senders).
      ARP_Cache.Learn (interfaces (0).arpCache, p, ours, syscall (SYSCALL_GETTIME));

      if p.Op = ARP_Packet.Request and then ours and then
        ARP_Cache.Usable_Sender (p.Sender_IP, p.Sender_HW)
      then
         sendARPReply (netMAC (p.Sender_HW), senderIP);
      end if;
      --  What the cache now holds for the sender (only what it accepted).
      if arpLookup (0, senderIP, mac) then
         for i in 0 .. numIfaces - 1 loop
            if interfaces (i).gateway = senderIP then
               interfaces (i).gwMAC := mac;
            end if;
         end loop;
         neighbourResolved (senderIP, mac);
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
   begin
      for i in frame'Range loop
         frame (i) := 0;
      end loop;

      Net.putMAC   (fAddr, 0,  srcMAC);
      Net.putMAC   (fAddr, 6,  interfaces (0).mac);
      Net.putU16BE (fAddr, 12, Net.ETHERTYPE_IPV4);

      writeIPv4Header (fAddr, frameLen, Net.PROTO_ICMP, interfaces (0).ipv4, srcIP, False);

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

   --  An ICMP error about one of our packets (RFC 1122 4.2.3.9, RFC 1191,
   --  RFC 5927). It must quote a packet we sent: our address, and for TCP
   --  a live connection's addresses and ports and a sequence number it has
   --  sent and not had acknowledged, so a blind forger cannot shrink or
   --  end connections.
   --  - Too_Big: the connection's segments shrink to the path (never below
   --    TCP's default of 536) and the unacknowledged data goes again.
   --  - Hard (port or protocol unreachable, prohibited): a connection
   --    attempt ends as unreachable; a connected UDP channel is marked so.
   --  - Soft errors change nothing: retransmission goes on.
   procedure handleICMPError (message : ICMPv4_Error.Bytes; Now : Unsigned_64) is
      use ICMPv4_Error;
      e  : Error;
      ok : Boolean;
      ours : constant Net.IPv4Address := interfaces (0).ipv4;
      peer : Net.IPv4Address;
   begin
      Parse (message, e, ok);
      if not ok or else [e.Source (0), e.Source (1), e.Source (2), e.Source (3)] /= ours then
         return;
      end if;
      peer := [e.Destination (0), e.Destination (1), e.Destination (2), e.Destination (3)];
      if e.Protocol = Net.PROTO_TCP then
         declare
            s : constant Conns.Maybe_Slot :=
              Conns.Find (connTable, tupleOf (ours, peer, e.Source_Port, e.Destination_Port));
            index : TCP_Slots.Connection_Index;
         begin
            if s = Conns.No_Slot then
               return;
            end if;
            index := s - 1;
            declare
               F : Flows.Flow renames tcpFlows (index);
               sent : constant Seq := Seq (e.Sequence);
            begin
               --  In flight: SND.UNA <= SEG.SEQ < SND.NXT.
               if not (TCP_Sequence.Le (F.E.C.Snd_Una, sent) and then TCP_Sequence.Lt (sent, F.E.C.Snd_Nxt)) then
                  return;
               end if;
               case e.Of_Kind is
                  when Too_Big =>
                     declare
                        mtu  : constant Natural := Natural (e.Next_Hop_MTU);
                        size : constant Natural :=
                          (if mtu = 0 then TCP_Limits.Default_IPv4_MSS
                           else Natural'Max
                             (mtu - Natural'Min (mtu, TCP_Limits.IPv4_Header_Size +
                                                      TCP_Limits.TCP_Header_Size),
                              TCP_Limits.Default_IPv4_MSS));
                     begin
                        if F.E.C.St in TCP_Connection.Synchronized_State and then
                          size < F.CC.SMSS
                        then
                           Flows.Path_MTU_Reduced (F, tcpPool, size);
                           agreements (index).Send_MSS := TCP_Options.MSS_Value (size);
                           tcpPump (index, False);
                        end if;
                     end;
                  when Hard =>
                     if F.E.C.St = TCP_Connection.Syn_Sent then
                        abortConnection (index, Now, Layout.Status_Unreachable);
                     end if;
                  when Soft =>
                     null;
               end case;
            end;
         end;
      elsif e.Protocol = Net.PROTO_UDP and then e.Of_Kind = Hard then
         declare
            index  : UDP_Channels.Channel_Index;
            result : UDP_Channels.Delivery;
            use type UDP_Channels.Delivery;
         begin
            UDP_Channels.Deliver
              (udpChannels, e.Source_Port, policyAddress (peer), e.Destination_Port, 0,
               index, result);
            if result = UDP_Channels.Matched then
               setStatus (index, Layout.Status_Unreachable);
            end if;
         end;
      end if;
   end handleICMPError;

   ---------------------------------------------------------------------------
   --  handleICMP
   ---------------------------------------------------------------------------
   --  Echo replies: at most ICMP_PER_MS per millisecond, bursts of ICMP_BURST.
   ICMP_PER_MS  : constant := 1;
   ICMP_BURST   : constant := 50;
   icmpTokens   : Natural := ICMP_BURST;
   icmpRefillAt : Unsigned_64 := 0;

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
      --  Well formed: long enough, and the checksum over the whole message.
      if icmpLen < 8 or else
        Net.internetChecksum (pktBuf + Storage_Offset (icmpOff), icmpLen) /= 0
      then
         return;
      end if;

      icmpType := Net.getU8 (pktBuf, icmpOff);

      if icmpType = Net.ICMP_ECHO_REQUEST then
         --  Answered within a token bucket, so a flood cannot take netstack.
         declare
            nowMs : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
         begin
            if nowMs > icmpRefillAt then
               icmpTokens := Natural'Min
                 (ICMP_BURST, icmpTokens + Natural (Unsigned_64'Min
                    (Unsigned_64 (ICMP_BURST), (nowMs - icmpRefillAt) * ICMP_PER_MS)));
               icmpRefillAt := nowMs;
            end if;
            if icmpTokens > 0 then
               icmpTokens := icmpTokens - 1;
               sendICMPReply (srcIP, srcMAC, pktBuf, icmpOff, icmpLen);
            end if;
         end;

      elsif icmpType in ICMPv4_Error.Destination_Unreachable | ICMPv4_Error.Time_Exceeded then
         declare
            message : constant ICMPv4_Error.Bytes (0 .. icmpLen - 1)
              with Import, Address => pktBuf + Storage_Offset (icmpOff);
         begin
            handleICMPError (message, clockNow);
         end;

      elsif icmpType = Net.ICMP_ECHO_REPLY then
         icmpSeq := Net.getU16BE (pktBuf, icmpOff + 6);

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
                           srcPacked : constant Unsigned_64 :=
                              Net.packIPv4 (srcIP);
                           replyMsg : constant Message :=
                             (tag      => (label  => REPLY_OK,
                                           length => 3,
                                           flags  => 0,
                                           reserved  => 0),
                              authorityTag => 0,
                              words    => [0 => Unsigned_64 (icmpSeq),
                                           1 => srcPacked,
                                           2 => (if nowMs > sendTs
                                                 then nowMs - sendTs
                                                 else 0),
                                           3 => 0]);
                           ignore : Unsigned_64;
                        begin
                           ignore := replyCap
                             (pendingReqs (i).replySlot, replyMsg);
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
      ipHdrLen : Natural;
      proto    : Unsigned_8;
      srcIP    : Net.IPv4Address;
      dstIP    : Net.IPv4Address;
      srcMAC   : Net.MACAddress;
      totalLen : Natural;
   begin
      if pktLen < ipOff + IPv4_Header.Minimum_Size then
         return;
      end if;
      declare
         --  The packet after the Ethernet header, in place (proved codec).
         packet : IPv4_Header.Bytes (0 .. pktLen - ipOff - 1)
            with Import, Address => pktBuf + Storage_Offset (ipOff);
         h : IPv4_Header.Header;
      begin
         --  Version, lengths (a truncated datagram is never read as a
         --  shorter valid one), and no fragments: there is no reassembly
         --  yet. Then the header checksum.
         if not IPv4_Header.Well_Formed (packet) or else
           Net.internetChecksum (pktBuf + Storage_Offset (ipOff), IPv4_Header.Stated_Size (packet)) /= 0
         then
            return;
         end if;
         IPv4_Header.Parse (packet, h);
         ipHdrLen := h.Size;
         totalLen := h.Total_Length;
         proto    := h.Protocol;
         srcIP    := [h.Source (0), h.Source (1), h.Source (2), h.Source (3)];
         dstIP    := [h.Destination (0), h.Destination (1), h.Destination (2), h.Destination (3)];
      end;
      Net.getMAC (pktBuf, 6, srcMAC);

      if findInterfaceForIP (dstIP) < 0 then
         return;
      end if;

      if proto = Net.PROTO_ICMP then
         handleICMP (pktBuf, ipOff, ipHdrLen, srcIP, srcMAC, totalLen);
      elsif proto = Net.PROTO_UDP then
         handleUDP (pktBuf, ipOff, ipHdrLen, srcIP, dstIP, totalLen);
      elsif proto = Net.PROTO_TCP then
         handleTCP (pktBuf, ipOff, ipHdrLen, srcIP, dstIP, srcMAC, totalLen);
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
      elsif etherType = Net.ETHERTYPE_IPV6 then
         declare
            --  The driver's frame, in place: the one unproved step.
            frame : constant IPv6_Header.Bytes (0 .. pktLen - 1)
              with Import, Address => pktBuf;
         begin
            IPv6.Receive (frame, clockNow);
         end;
      end if;
   end handlePacket;

   --  State machine removed: netmgr now handles IP configuration.
   --  Interface goes IF_DOWN -> IF_UP via OP_NET_CONFIGURE.

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
      --  "@net:tcp-listen:<address>:<port>": a listener, not a connection.
      listen     : Boolean := False;
      --  isIPLiteral: the address (IPv4 until netstack speaks IPv6).
      address    : Net.IPv4Address := [others => 0];
   end record;

   --  The client's target text, parsed by the proved CuBit.Net_Locator.
   procedure parseNetScheme (addr   : System.Address;
                             len    : Natural;
                             result : out ParsedScheme) is
      text : String (1 .. Natural'Min (len, CuBit.Locators.Maximum_Length))
        with Import, Address => addr;
      parsed : CuBit.Net_Locator.Target;
      use type CuBit.Net_Locator.Protocol;
   begin
      result := (valid => False, proto => 0,
                 hostname => [others => ' '], hostLen => 0,
                 port => 0, isIPLiteral => False, listen => False,
                 address => [others => 0]);
      if len > CuBit.Locators.Maximum_Length then
         return;
      end if;
      CuBit.Net_Locator.Parse (text, parsed);
      --  IPv6 addresses are refused until netstack speaks IPv6.
      if not parsed.Valid or else
        (parsed.Is_Address and then not CuBit.Net_Address.Is_Mapped (parsed.Address))
      then
         return;
      end if;
      result :=
        (valid => True,
         proto => (if parsed.Proto = CuBit.Net_Locator.UDP then Net.PROTO_UDP
                   else Net.PROTO_TCP),
         hostname => parsed.Name,
         hostLen => parsed.Name_Len,
         port => Unsigned_16 (parsed.Port),
         isIPLiteral => parsed.Is_Address,
         listen => parsed.Proto = CuBit.Net_Locator.TCP_Listen,
         address => (if parsed.Is_Address then
                       [parsed.Address (12), parsed.Address (13),
                        parsed.Address (14), parsed.Address (15)]
                     else [others => 0]));
   end parseNetScheme;


   ---------------------------------------------------------------------------
   --  addPending - store a pending request in the first free slot
   ---------------------------------------------------------------------------
   function addPending (req : PendingRequest) return Boolean is
      savedSlot : CapabilitySlot;
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_NONE then
            savedSlot := CapabilitySlot (16 + i);

            --  Deferral is valid only if the kernel moved the current
            --  one-use reply authority into our selected pending slot.
            if saveReplyCap (Unsigned_64 (savedSlot)) /= 1 then
               return False;
            end if;

            pendingReqs (i) := req;
            pendingReqs (i).replySlot := savedSlot;
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
   procedure replyError
     (to   : ProcessID;
      slot : CapabilitySlot := CapabilitySlot'Last)
   is
      errMsg : constant Message :=
        (tag      => (label  => REPLY_ERR,
                      length => 0,
                      flags  => 0,
                      reserved  => 0),
         authorityTag => 0,
         words    => [others => 0]);
      ignore : Unsigned_64;
   begin
      pragma Unreferenced (to);
      ignore := replyCap (slot, errMsg);
   end replyError;

   ---------------------------------------------------------------------------
   --  replyOK - send REPLY_OK with word0 to a sender
   ---------------------------------------------------------------------------
   procedure replyOKWord
     (to   : ProcessID;
      w0   : Unsigned_64;
      slot : CapabilitySlot := CapabilitySlot'Last)
   is
      okMsg : constant Message :=
        (tag      => (label  => REPLY_OK,
                      length => 1,
                      flags  => 0,
                      reserved  => 0),
         authorityTag => 0,
         words    => [0 => w0, others => 0]);
      ignore : Unsigned_64;
   begin
      pragma Unreferenced (to);
      ignore := replyCap (slot, okMsg);
   end replyOKWord;

   ---------------------------------------------------------------------------
   --  deliverArrivals: hand listener lIdx's established connections to its
   --  owner, each in a buffer the owner offered (Layout, "Listeners"). A
   --  connection stays in the backlog while there is no offer, no room for
   --  its arrival record, no channel, or no connection left in the scope's
   --  declaration. A bad offer (not the owner's free buffer, or a header
   --  that does not fit) is dropped; the connection waits for the next.
   --  The caller has accepted the listener's ring indices (serviceChannel).
   ---------------------------------------------------------------------------
   procedure deliverArrivals (lIdx : Network_Channel_Handles.Channel_Index) is
      L        : NetChannel renames channels (lIdx);
      offers   : Rings.Bytes (0 .. L.tx.Size - 1)
        with Import, Address => sendRing (lIdx);
      arrivals : Rings.Bytes (0 .. L.rx.Size - 1)
        with Import, Address => receiveRing (lIdx);
      offer    : Rings.Bytes (0 .. Layout.Offer_Bytes - 1) := [others => 0];
      arrival  : Rings.Bytes (0 .. Layout.Arrival_Bytes - 1);
      len      : Natural;
      truncated, ready, took, put : Boolean := False;
      taken    : Datagrams.Take_Result;
      result   : Datagrams.Put_Result;
      chIdx    : Network_Channel_Handles.Channel_Reference;
      connIdx  : TCP_Listeners.Connection_Index;
      use type Datagrams.Take_Result, Datagrams.Put_Result;

      function field (pos : Natural; bytes : Positive) return Unsigned_64 is
         v : Unsigned_64 := 0;
      begin
         for k in reverse 0 .. bytes - 1 loop
            v := Shift_Left (v, 8) or Unsigned_64 (offer (pos + k));
         end loop;
         return v;
      end field;

      procedure store (pos : Natural; bytes : Positive; value : Unsigned_64) is
      begin
         for k in 0 .. bytes - 1 loop
            arrival (pos + k) := Unsigned_8 (Shift_Right (value, 8 * k) and 16#FF#);
         end loop;
      end store;
   begin
      while not failedStatus (L.status) and then
        TCP_Listeners.Has_Ready (listeners, L.authorityTag, L.listener) and then
        --  Room for an arrival record wherever the ring wraps.
        Rings.Space (L.rx) >= 2 * Datagrams.Record_Bytes (Layout.Arrival_Bytes)
      loop
         Network_Channel_Handles.Allocate (channelHandles, L.pid, L.authorityTag, chIdx);
         exit when chIdx < 0;
         channels (chIdx) :=
           (kind => CHANNEL_SERVER, proto => Net.PROTO_TCP, pid => L.pid,
            connIdx => -1, authorityTag => L.authorityTag, others => <>);
         if not chargeChannel (chIdx) then
            releaseChannel (chIdx);
            exit;
         end if;
         Datagrams.Take (L.tx, offers, offer, len, truncated, taken);
         if taken /= Datagrams.Taken then
            releaseChannel (chIdx);
            if taken = Datagrams.Malformed then
               setStatus (lIdx, Layout.Status_Protocol_Error);
            end if;
            exit;
         end if;
         took := True;
         if len = Layout.Offer_Bytes and then not truncated and then
           claimBuffer (chIdx, L.pid, field (Layout.Offer_Arena_At, 8),
                        field (Layout.Offer_Buffer_At, 4)) and then
           setupRings (chIdx)
         then
            TCP_Listeners.Accept_Ready
              (listeners, L.authorityTag, L.listener, connIdx, ready);
         else
            ready := False;
         end if;
         if ready then
            channels (chIdx).connIdx := connIdx;
            channels (chIdx).remoteIP := tcpConns (connIdx).remoteIP;
            channels (chIdx).remotePort := tcpConns (connIdx).remotePort;
            channels (chIdx).localPort := tcpConns (connIdx).localPort;
            parentListener (connIdx) := 0;
            connectionAuthority (connIdx) := L.authorityTag;
            store (Layout.Arrival_Channel_At, 8,
                   Unsigned_64 (Network_Channel_Handles.Value (channelHandles, chIdx)));
            store (Layout.Arrival_Arena_At, 8, field (Layout.Offer_Arena_At, 8));
            store (Layout.Arrival_Buffer_At, 4, field (Layout.Offer_Buffer_At, 4));
            store (Layout.Arrival_Port_At, 4, Unsigned_64 (tcpConns (connIdx).remotePort));
            declare
               peer : constant CuBit.Net_Address.Address :=
                 CuBit.Net_Address.Mapped (policyAddress (tcpConns (connIdx).remoteIP));
            begin
               for k in peer'Range loop
                  arrival (Layout.Arrival_Address_At + k) := peer (k);
               end loop;
            end;
            Datagrams.Put (L.rx, arrivals, arrival, result);
            put := put or else result = Datagrams.Put;
            serviceChannel (chIdx);
         else
            releaseChannel (chIdx);
         end if;
      end loop;
      if took then
         setHeaderWord (lIdx, Layout.Tx_Consumed_At, Unsigned_32 (L.tx.Consumed));
      end if;
      if put then
         setHeaderWord (lIdx, Layout.Rx_Produced_At, Unsigned_32 (L.rx.Produced));
      end if;
   end deliverArrivals;

   --  Every listener with a connection waiting.
   procedure deliverAllArrivals is
   begin
      for I in channels'Range loop
         if channels (I).kind = CHANNEL_LISTENER and then
           TCP_Listeners.Has_Ready (listeners, channels (I).authorityTag, channels (I).listener)
         then
            serviceChannel (I);
         end if;
      end loop;
   end deliverAllArrivals;

   ---------------------------------------------------------------------------
   --  openListener: OPEN of "@net:tcp-listen:<address>:<port>". The address
   --  must be the interface's, within the caller's tcp-listen scope, and
   --  the port not in use by an outbound connection.
   ---------------------------------------------------------------------------
   procedure openListener
     (chIdx  : Network_Channel_Handles.Channel_Index;
      snd    : ProcessID;
      tag    : Unsigned_64;
      scheme : ParsedScheme)
   is
      address  : Net.IPv4Address;
      ipOK     : Boolean;
      conflict : Boolean := False;
      handle   : TCP_Listeners.Handle;
      status   : TCP_Listeners.Bind_Status;
   begin
      address := scheme.address;
      ipOK := scheme.isIPLiteral;
      for C of channels loop
         if C.kind = CHANNEL_CLIENT and then C.connIdx in tcpConns'Range and then
           tcpConns (C.connIdx).localPort = scheme.port
         then
            conflict := True;
         end if;
      end loop;
      if not ipOK or else conflict or else numIfaces = 0 or else
        interfaces (0).state /= IF_UP or else
        policyAddress (interfaces (0).ipv4) /= policyAddress (address) or else
        not Network_Grants.Allows
          (networkGrants, snd, tag, Network_Authority.Listen_TCP,
           policyAddress (address), scheme.port)
      then
         debugPrint ("netstack: listen: address or port outside the caller's scope" & LF);
         releaseChannel (chIdx);
         replyError (snd);
         return;
      end if;
      TCP_Listeners.Bind (listeners, tag, policyAddress (address), scheme.port, handle, status);
      if status /= TCP_Listeners.Bound then
         releaseChannel (chIdx);
         replyError (snd);
         return;
      end if;
      channels (chIdx).kind := CHANNEL_LISTENER;
      channels (chIdx).listener := handle;
      channels (chIdx).localPort := scheme.port;
      setStatus (chIdx, Layout.Status_Open);
      replyOKWord (snd, Unsigned_64 (Network_Channel_Handles.Value (channelHandles, chIdx)));
   end openListener;

   procedure expireRequests (Now : Unsigned_64) is
      children : TCP_Listeners.Connection_List;
   begin
      tcpTimers (Now);
      TCP_Listeners.Expire (listeners, Now, children);
      for I in children'Range loop
         if children (I) then discardConnection (I); end if;
      end loop;
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then Now >= P.waitDeadline then
            replyOKWord (P.sender, 0, P.replySlot);
            P.kind := PENDING_NONE;
         elsif P.kind = PENDING_PING and then
           Now >= deadlineAfter (Unsigned_64 (To_Integer (P.bufAddr)), PING_TIMEOUT_MS)
         then
            replyError (P.sender, P.replySlot);
            P.kind := PENDING_NONE;
         elsif P.kind in PENDING_RESOLVE | PENDING_OPEN and then Now >= P.queryDeadline then
            --  An unanswered query: fail it; an OPEN's channel goes too.
            failQuery (P);
         elsif P.kind in PENDING_RESOLVE | PENDING_OPEN and then Now >= P.nextSend then
            sendAttempt (P, Now);
         end if;
      end loop;
   end expireRequests;

   function nextDeadline return Unsigned_64 is
      deadline : Unsigned_64 :=
        Unsigned_64'Min (Unsigned_64'Min (TCP_Listeners.Next_Deadline (listeners), tcpNextDeadline),
                         IPv6.Next_Deadline);
   begin
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT then
            deadline := Unsigned_64'Min (deadline, P.waitDeadline);
         elsif P.kind = PENDING_PING then
            deadline := Unsigned_64'Min
              (deadline, deadlineAfter (Unsigned_64 (To_Integer (P.bufAddr)), PING_TIMEOUT_MS));
         elsif P.kind in PENDING_RESOLVE | PENDING_OPEN then
            deadline := Unsigned_64'Min
              (deadline, Unsigned_64'Min (P.queryDeadline, P.nextSend));
         end if;
      end loop;
      return deadline;
   end nextDeadline;

   --  Start resolving name for a RESOLVE or OPEN request: record it with
   --  a random ID and source port, and send the first attempt.
   procedure startQuery (kind    : PendingKind;
                         snd     : ProcessID;
                         chIdx   : Integer;
                         bufAddr : System.Address;
                         dstPort : Unsigned_16;
                         name    : DNS_Name.Wire;
                         nameLen : DNS_Name.Wire_Length;
                         ok      : out Boolean) is
      now  : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
      port : constant Unsigned_16 :=
        DNS_PORT_FIRST + Unsigned_16 (dnsRandom mod DNS_PORT_COUNT);
      txid : constant Unsigned_16 := Unsigned_16 (dnsRandom and 16#FFFF#);
   begin
      ok := False;
      if primaryDNS = NO_ADDRESS then
         return;   --  no server configured: fail now, not after a timeout
      end if;
      ok := addPending (
         (kind       => kind,
          sender     => snd,
          connIdx    => -1,
          channelIdx => chIdx,
          bufAddr    => bufAddr,
          bufOff     => 0,
          maxLen     => 0,
          txid       => txid,
          dstPort    => dstPort,
          replySlot  => 0,
          queryPort  => port,
          nameHash   => nameHashOf (name (0 .. nameLen - 1)),
          queryDeadline => deadlineAfter (now, DNS_TIMEOUT_MS),
          question    => name,
          questionLen => nameLen,
          attempts    => 0,
          nextSend    => Unsigned_64'Last,
          waitDeadline  => Unsigned_64'Last,
          waitMask      => 0));
      if not ok then
         return;
      end if;
      for P of pendingReqs loop
         if P.kind = kind and then P.sender = snd and then P.queryPort = port and then
           P.txid = txid and then P.attempts = 0
         then
            sendAttempt (P, now);
            exit;
         end if;
      end loop;
   end startQuery;

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
      ok   : Boolean;
   begin
      if nameLen = 0 or nameLen > 32 then
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

      declare
         name : DNS_Name.Wire;
         wireLen : DNS_Name.Wire_Length;
         valid : Boolean;
      begin
      DNS_Name.Encode (hostname (1 .. nameLen), name, wireLen, valid);
      if not valid then
         replyError (snd);
         return;
      end if;
      startQuery (PENDING_RESOLVE, snd, -1, System.Null_Address, 0, name, wireLen, ok);
      if not ok then
         replyError (snd);
         return;
      end if;
      end;
   end handleAppResolve;

   ---------------------------------------------------------------------------
   --  handleNetOpen - open a network channel (DNS + connect in one call)
   --
   --  Request: tag.label=OP_NET_OPEN, tag.length=scheme string length,
   --           tag.flags=channel kind (0=client),
   --           words(0)=grant slot, words(1)=buffer size, words(3)=generation
   --  Scheme string is at offset 0 of the grant buffer.
   --  Reply: deferred until DNS+TCP handshake completes
   ---------------------------------------------------------------------------
   ---------------------------------------------------------------------------
   --  openDatagram - finish opening a connected UDP channel once its
   --  destination is known. Checks the caller's Connect_UDP scope, assigns
   --  an ephemeral local port and replies with the channel handle on slot.
   --  On failure the channel is released and REPLY_ERR is sent.
   ---------------------------------------------------------------------------
   procedure openDatagram
     (chIdx : Network_Channel_Handles.Channel_Index;
      owner : ProcessID;
      dstIP : Net.IPv4Address;
      port  : Unsigned_16;
      slot  : CapabilitySlot)
   is
      ok : Boolean := False;
   begin
      if Network_Grants.Allows
        (networkGrants, owner, channels (chIdx).authorityTag,
         Network_Authority.Connect_UDP, policyAddress (dstIP), port)
      then
         UDP_Channels.Open (udpChannels, chIdx, policyAddress (dstIP), port, ok);
      end if;
      if not ok then
         releaseChannel (chIdx);
         replyError (owner, slot);
         return;
      end if;
      channels (chIdx).remoteIP := dstIP;
      channels (chIdx).localPort := UDP_Channels.Local_Port (udpChannels, chIdx);
      replyOKWord (owner,
                   Unsigned_64 (Network_Channel_Handles.Value (channelHandles, chIdx)),
                   slot);
   end openDatagram;

   ---------------------------------------------------------------------------
   --  releaseOwner: a process has exited. Its deferred requests are answered
   --  (consuming their reply capabilities), its channels released, its
   --  listeners closed with their unaccepted children, and its scopes and
   --  their reservations released. Nothing it held survives to be found
   --  through a reused PID.
   ---------------------------------------------------------------------------
   procedure releaseOwner (owner : ProcessID) is
      tags : Network_Grants.Tag_List;
      children : TCP_Listeners.Connection_List;
   begin
      for P of pendingReqs loop
         if P.kind /= PENDING_NONE and then P.sender = owner then
            replyError (P.sender, P.replySlot);
            P.kind := PENDING_NONE;
         end if;
      end loop;
      for I in channels'Range loop
         if channels (I).pid = owner then
            releaseChannel (I);
         end if;
      end loop;
      declare
         gone : Channel_Arenas.Arena_List;
         returned : Boolean;
      begin
         Channel_Arenas.Release_Owner (arenas, Unsigned_64 (owner), gone);
         for A in gone'Range loop
            if gone (A) then
               CuBit.Memory_Grants.Return_Acquisition (arenaGrant (A), returned);
               arenaBase (A) := System.Null_Address;
            end if;
         end loop;
      end;
      Network_Grants.Release_Owner (networkGrants, Unsigned_64 (owner), tags);
      if (for some T of tags => T /= 0) then
         debugPrint ("netstack: released the scopes of exited process" &
                     Unsigned_64'Image (Unsigned_64 (owner)) & LF);
      end if;
      for T of tags loop
         if T /= 0 then
            TCP_Listeners.Close_Owned (listeners, T, children);
            for C in children'Range loop
               if children (C) then discardConnection (C); end if;
            end loop;
         end if;
      end loop;
   end releaseOwner;

   ---------------------------------------------------------------------------
   --  handleArena: a process lends a grant cut into channel buffers
   --  (Layout.OP_NET_ARENA). Admission checked the ring sizes and count.
   ---------------------------------------------------------------------------
   procedure handleArena (snd : ProcessID; m : Message) is
      size : constant Natural :=
        Layout.Header_Bytes + Natural (m.words (2) and 16#FFFF_FFFF#) +
        Natural (Shift_Right (m.words (2), 32));
      count : constant Natural := Natural (m.words (3));
      reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => m.words (0), generation => m.words (1));
      handle : Channel_Arenas.Handle;
      index : Channel_Arenas.Arena_Index;
      address : System.Address;
      ok, returned : Boolean;
   begin
      if not Channel_Arenas.Fits (size, count) then
         replyError (snd); return;
      end if;
      CuBit.Memory_Grants.Acquire
        (reference, snd, 0, Unsigned_64 (size * count),
         CuBit.Memory_Grants.Write_Access, address, ok);
      if not ok then replyError (snd); return; end if;
      Channel_Arenas.Register
        (arenas, Unsigned_64 (snd), size, count, handle, index, ok);
      if not ok then
         CuBit.Memory_Grants.Return_Acquisition (reference, returned);
         replyError (snd); return;
      end if;
      arenaGrant (index) := reference;
      arenaBase (index) := address;
      replyOKWord (snd, Unsigned_64 (handle));
   end handleArena;

   procedure handleNetOpen (snd : ProcessID; m : Message) is
      schemeLen : constant Natural := Natural (m.tag.length);
      scheme    : ParsedScheme;
      chIdx     : Integer;
      ok        : Boolean;
   begin
      Network_Channel_Handles.Allocate (channelHandles, snd, m.authorityTag, chIdx);
      if chIdx < 0 then
         debugPrint ("netstack: open: no free channels" & LF);
         replyError (snd);
         return;
      end if;
      channels (chIdx) :=
         (kind       => CHANNEL_CLIENT,
          pid        => snd,
          connIdx    => -1,
          authorityTag => m.authorityTag,
          others => <>);

      --  words 0 = arena handle, 1 = buffer index. The target is read from
      --  the buffer's header once it is ours.
      if not claimBuffer (chIdx, snd, m.words (0), m.words (1)) then
         debugPrint ("netstack: open: no such free arena buffer" & LF);
         releaseChannel (chIdx);
         replyError (snd);
         return;
      end if;
      parseNetScheme (channels (chIdx).bufAddr + Storage_Offset (Layout.Target_At),
                      schemeLen, scheme);
      if not scheme.valid or else
        (scheme.proto /= Net.PROTO_TCP and scheme.proto /= Net.PROTO_UDP)
      then
         debugPrint ("netstack: open: invalid target" & LF);
         releaseChannel (chIdx);
         replyError (snd);
         return;
      end if;
      channels (chIdx).proto := scheme.proto;
      channels (chIdx).remotePort := scheme.port;
      if not chargeChannel (chIdx) or else not setupRings (chIdx) then
         releaseChannel (chIdx);
         replyError (snd);
         return;
      end if;
      if scheme.listen then
         openListener (chIdx, snd, m.authorityTag, scheme);
         return;
      end if;

      if scheme.isIPLiteral and scheme.proto = Net.PROTO_UDP then
         declare
            dstIP : Net.IPv4Address;
            ipOK  : Boolean;
         begin
            dstIP := scheme.address;
            ipOK := True;
            if not ipOK then
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;
            --  No handshake: the reply is immediate.
            openDatagram (chIdx, snd, dstIP, scheme.port, CapabilitySlot'Last);
         end;
      elsif scheme.isIPLiteral then
         --  Parse IP directly, skip DNS
         declare
            dstIP : Net.IPv4Address;
            ipOK  : Boolean;
            connIdx : Integer;
         begin
            dstIP := scheme.address;
            ipOK := True;
            if not ipOK or else not Network_Grants.Allows
              (networkGrants, snd, m.authorityTag, Network_Authority.Connect_TCP,
               policyAddress (dstIP), scheme.port)
            then
               debugPrint ("netstack: open: address or port outside the caller's scope" & LF);
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;

            channels (chIdx).remoteIP := dstIP;
            connIdx := tcpConnect (dstIP, resolvedNeighbour (dstIP), scheme.port);
            if connIdx < 0 then
               debugPrint ("netstack: open: no free TCP connection slot" & LF);
               reportSlots;
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;
            channels (chIdx).connIdx := connIdx;
            connectionAuthority (connIdx) := m.authorityTag;

            ok := addPending (
               (kind       => PENDING_CONNECT,
                sender     => snd,
                connIdx    => connIdx,
                channelIdx => chIdx,
                bufAddr    => channels (chIdx).bufAddr,
                bufOff     => 0,
                maxLen     => 0,
                txid       => 0,
                dstPort    => scheme.port,
                replySlot  => 0,
          others     => <>));
            if not ok then
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;
         end;
      else
         --  Need DNS resolution first
         if not Network_Grants.May_Resolve (networkGrants, snd, m.authorityTag) then
            releaseChannel (chIdx);
            replyError (snd);
            return;
         end if;
         declare
            name : DNS_Name.Wire;
            nameLen : DNS_Name.Wire_Length;
            valid : Boolean;
         begin
            DNS_Name.Encode (scheme.hostname (1 .. scheme.hostLen), name, nameLen, valid);
            if not valid then
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;
            startQuery (PENDING_OPEN, snd, chIdx, channels (chIdx).bufAddr, scheme.port,
                        name, nameLen, ok);
            if not ok then
               releaseChannel (chIdx);
               replyError (snd);
               return;
            end if;
         end;
      end if;
      --  Reply deferred
   end handleNetOpen;


   ---------------------------------------------------------------------------
   --  handleNetShut - close a channel
   --
   --  Request: words(0)=channel handle
   --  Reply: immediate REPLY_OK
   ---------------------------------------------------------------------------
   procedure handleNetShut (snd : ProcessID; m : Message) is
      chHandle : constant Network_Channel_Handles.Channel_Reference :=
        Network_Channel_Handles.Resolve
          (channelHandles, snd, m.authorityTag, Network_Channel_Handles.Handle (m.words (0)));
   begin
      if chHandle not in channels'Range or else
         channels (chHandle).kind = CHANNEL_NONE or else
         channels (chHandle).pid /= snd or else
         channels (chHandle).authorityTag /= m.authorityTag
      then
         replyError (snd);
         return;
      end if;

      --  A listener closes, with the connections nobody took.
      if channels (chHandle).kind = CHANNEL_LISTENER then
         releaseChannel (chHandle);
         replyOKWord (snd, 0);
         return;
      end if;

      --  Connected UDP: send what the ring holds, then release the channel
      --  and its local port.
      if channels (chHandle).proto = Net.PROTO_UDP then
         serviceChannel (chHandle);
         releaseChannel (chHandle);
         replyOKWord (snd, 0);
         return;
      end if;

      --  A stream channel: take what is left in its send ring (the grant
      --  goes back now), then FIN after the queued data.
      if channels (chHandle).connIdx in tcpConns'Range and then
        connectionAuthority (channels (chHandle).connIdx) = m.authorityTag
      then
         serviceChannel (chHandle);
         tcpClose (channels (chHandle).connIdx);
      end if;

      --  Free the channel slot
      releaseChannel (chHandle);

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
                         reserved  => 0),
            authorityTag => 0,
            words    => [0 => interfaces (ifIdx).pktGrant,
                         1 => Unsigned_64 (PACKET_BUF_SIZE),
                         others => 0]);
         ignore : Unsigned_64;
      begin
         ignore := replyCap (CapabilitySlot'Last, replyMsg);
      end;

      debugPrint ("netstack: attached to driver pid=");
      printDec (Unsigned_32 (interfaces (ifIdx).driverPID));
      debugPrint ("" & LF);
   end handleAttach;

   ---------------------------------------------------------------------------
   --  Received frames: a ring in the grant's RX half that the driver fills
   --  (virtio-net's processRX has the same layout). Slot 0 holds the
   --  counts and flags; frame N is in slot (N mod RX_RING_SLOTS) + 1, its
   --  length (4 bytes, little endian) first and the frame RX_SLOT_HEADER
   --  bytes in.
   --  - produced (byte 0, the driver's) and consumed (byte 64, ours) are
   --    free-running frame counts; a produced count more than a ring ahead
   --    is refused;
   --  - space wanted (byte 4, the driver's): it found the ring full; we
   --    ring its doorbell (OP_NET_TX) after freeing slots;
   --  - doorbell wanted (byte 68, ours): a new nonzero epoch each time we
   --    are about to go idle; the driver sends one OP_NET_RX per epoch.
   --  No call and no reply per batch: neither side waits for the other.
   ---------------------------------------------------------------------------
   RX_SLOT_BYTES  : constant := 2048;
   RX_SLOT_HEADER : constant := 16;
   RX_RING_SLOTS  : constant := (PACKET_BUF_SIZE / 2) / RX_SLOT_BYTES - 1;
   RX_SPACE_WANTED_OFFSET : constant := 4;
   RX_CONSUMER_OFFSET     : constant := 64;
   RX_DOORBELL_OFFSET     : constant := 68;
   rxConsumed : Unsigned_32 := 0;
   rxEpoch    : Unsigned_32 := 0;

   procedure drainRXRing is
      ifIdx : constant Natural := 0;   --  the ring is interface zero's
      area : constant System.Address := interfaces (ifIdx).pktBuf;
      produced : Unsigned_32 with Volatile, Import, Address => area;
      consumed : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (RX_CONSUMER_OFFSET);
      spaceWanted : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (RX_SPACE_WANTED_OFFSET);
      doorbell : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (RX_DOORBELL_OFFSET);
      available : Unsigned_32;
      mark : Unsigned_64;
   begin
      if area = System.Null_Address then
         return;
      end if;
      --  Busy: no doorbells needed until we are about to go idle again.
      doorbell := 0;
      loop
         available := produced;
         --  The frames were written before the count was: read them after.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         if Frame_Ring.Fill (available, rxConsumed) > RX_RING_SLOTS then
            debugPrint ("netstack: RX ring count out of range" & LF);
            return;
         end if;
         if available /= rxConsumed then
            inRxBatch := True;
            clockNow := syscall (SYSCALL_GETTIME);
            mark := Prof.Now;
            while rxConsumed /= available loop
               declare
                  slot : constant System.Address :=
                    area + Storage_Offset
                      (Frame_Ring.Slot_Of (rxConsumed, RX_RING_SLOTS) * RX_SLOT_BYTES);
                  len  : Unsigned_32 with Import, Address => slot;
               begin
                  if len >= 14 and then Frame_Ring.Fits (len, RX_SLOT_BYTES, RX_SLOT_HEADER) then
                     handlePacket (slot + RX_SLOT_HEADER, Natural (len));
                  end if;
               end;
               rxConsumed := rxConsumed + 1;
            end loop;
            Prof.Charge (Prof.Batch, mark);
            --  The frames are handled before their slots are handed back.
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            consumed := rxConsumed;
            mark := Prof.Now;
            tcpBatchDone;
            Prof.Charge (Prof.Batch_Done, mark);
            System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
            if spaceWanted /= 0 then
               txDoorbell := True;   --  the driver waits for slots
            end if;
         end if;
         --  About to go idle: ask for a doorbell, then look once more (the
         --  driver may have published before it could see the request).
         rxEpoch := (if rxEpoch = Unsigned_32'Last then 1 else rxEpoch + 1);
         doorbell := rxEpoch;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         exit when produced = rxConsumed;
         doorbell := 0;
      end loop;
   end drainRXRing;

   --  64 random bits from the CPU (RDRAND), or, without it, a mix of the
   --  time-stamp counter and the clock: weak, and reported. To come from
   --  the entropy service.
   function hardwareRandom return Unsigned_64 is
      use System.Machine_Code;
      EAX, EBX, ECX, EDX : Unsigned_32;
      value : Unsigned_64 := 0;
      carry : Unsigned_8 := 0;
      low, high : Unsigned_32;
   begin
      Asm ("cpuid",
           Outputs => [Unsigned_32'Asm_Output ("=a", EAX),
                       Unsigned_32'Asm_Output ("=b", EBX),
                       Unsigned_32'Asm_Output ("=c", ECX),
                       Unsigned_32'Asm_Output ("=d", EDX)],
           Inputs => [Unsigned_32'Asm_Input ("a", 1),
                      Unsigned_32'Asm_Input ("c", 0)],
           Volatile => True);
      if (ECX and 16#4000_0000#) /= 0 then   --  CPUID.01H:ECX.RDRAND
         for Attempt in 1 .. 10 loop
            Asm ("rdrand %0; setc %1",
                 Outputs => [Unsigned_64'Asm_Output ("=r", value),
                             Unsigned_8'Asm_Output ("=qm", carry)],
                 Volatile => True);
            if carry = 1 then
               return value;
            end if;
         end loop;
      end if;
      debugPrint ("netstack: no RDRAND; TCP sequence numbers are weakly keyed" & LF);
      Asm ("rdtsc",
           Outputs => [Unsigned_32'Asm_Output ("=a", low),
                       Unsigned_32'Asm_Output ("=d", high)],
           Volatile => True);
      value := (Shift_Left (Unsigned_64 (high), 32) or Unsigned_64 (low)) xor
               syscall (SYSCALL_GETTIME) * 16#9E37_79B9_7F4A_7C15#;
      return value;
   end hardwareRandom;

   --  Failed flushes in a row before netstack sleeps (see the main loop).
   FLUSH_SPIN_LIMIT : constant := 4_096;
   flushFailures : Natural := 0;


   procedure Run is
      lastExpiry : Unsigned_64 := Unsigned_64'Last;
      sender  : ProcessID;
      msg     : Message;
      found   : Boolean;
   begin
   debugPrint ("netstack: starting..." & LF);
   TCP_Engine.Pool.Initialize (tcpPool);
   isnKey := (K0 => hardwareRandom, K1 => hardwareRandom);
   Conns.Initialize (connTable, (K0 => hardwareRandom, K1 => hardwareRandom));
   dnsKey := (K0 => hardwareRandom, K1 => hardwareRandom);

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
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]));
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
      --  Timers must run even under a continuous stream of service requests.
      clockNow := syscall (SYSCALL_GETTIME);
      --  Deadlines are in milliseconds: look at them once per tick, not on
      --  every message.
      if clockNow /= lastExpiry then
         declare
            mark : constant Unsigned_64 := Prof.Now;
         begin
            expireRequests (clockNow);
            IPv6.Tick (clockNow);
            lastExpiry := clockNow;
            Prof.Charge (Prof.Timers, mark);
         end;
      end if;
      --  Hand queued frames to the driver as fast as it takes them, and let
      --  connections that stopped for room send again.
      while deferredCount > 0 and then flushOneDeferredTX loop
         null;
      end loop;
      if deferredCount <= TX_LOW_WATER then
         tcpResumeBlocked;
      end if;
      ringTXDoorbell;

      --  1. Try non-blocking service-request receive to keep responsiveness.
      --     Network driver events and completions stay on their own lanes.
      Poll_Service_Request (sender, msg, found);

      --  2. If no message but deferred TX pending, try to flush one frame.
      --  The driver empties its mailbox within microseconds, so poll again
      --  at once; sleep (1 ms) only if it stays full, e.g. a stalled device.
      if not found and deferredCount > 0 then
         if flushOneDeferredTX then
            flushFailures := 0;
         else
            flushFailures := flushFailures + 1;
            if flushFailures >= FLUSH_SPIN_LIMIT then
               flushFailures := 0;
               declare
                  ignore : Unsigned_64;
               begin
                  ignore := syscall (SYSCALL_SLEEP, 1);
               end;
            end if;
         end if;

      --  3. Block on messages or the earliest actual resource deadline.
      elsif not found then
         receiveUntil (nextDeadline, sender, msg, found);
      end if;

      --  4. Dispatch message
      if found then
         if not admittedRequest (sender, msg) then
            replyError (sender);
         else
         case msg.tag.label is
            when Network_Authority.OP_INSTALL_SCOPE =>
               declare
                  item : Network_Authority.Scope;
                  valid, installed : Boolean;
                  authorityTag : Unsigned_64;
               begin
                  Network_Authority.Decode (msg.words (1), msg.words (2), item, valid);
                  if msg.tag.length /= 3 or else not valid or else msg.words (0) = 0 then
                     replyError (sender);
                  else
                     Network_Grants.Install
                       (networkGrants, msg.words (0), item,
                        Network_Channel_Handles.Maximum_Channels,
                        authorityTag, installed);
                     if installed then replyOKWord (sender, authorityTag);
                     else replyError (sender); end if;
                  end if;
               end;

            when Network_Authority.OP_RELEASE_SCOPE =>
               --  Used to roll back a failed capability mint. Do not silently
               --  revoke live channels; general revocation needs full teardown.
               if msg.tag.length = 2 then
                  Network_Grants.Release (networkGrants, msg.words (0), msg.words (1));
                  replyOKWord (sender, 0);
               else replyError (sender); end if;

            when Network_Authority.OP_RELEASE_OWNER =>
               if msg.tag.length = 1 and then msg.words (0) /= 0 and then
                 msg.words (0) <= Unsigned_64 (ProcessID'Last)
               then
                  releaseOwner (ProcessID (msg.words (0)));
                  replyOKWord (sender, 0);
               else replyError (sender); end if;

            when OP_NET_ATTACH =>
               handleAttach (sender);
               --  Publish our first doorbell epoch: the RX ring is empty.
               drainRXRing;

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
                     --  IPv6 needs only the link: start now. Its self-test
                     --  pings the router once, when there is one.
                     IPv6.Start (interfaces (idx).mac,
                                 (K0 => hardwareRandom, K1 => hardwareRandom),
                                 syscall (SYSCALL_GETTIME), Self_Test => True);
                  end;
               end if;

            when OP_NET_RX =>
               --  A doorbell (one-way): the frames are in the RX ring.
               drainRXRing;

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
                                        reserved  => 0),
                           authorityTag => 0,
                           words    => [
                              0 => ipPacked or
                                   Shift_Left (stateVal, 32),
                              1 => maskPacked or
                                   Shift_Left (gwPacked, 32),
                              2 => macPacked,
                              3 => dnsPri or
                                   Shift_Left (dnsSec, 32)]);
                        ignore : Unsigned_64;
                     begin
                        ignore := replyCap (CapabilitySlot'Last, detailMsg);
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
                  packed : array (0 .. 3) of Unsigned_64 := [others => 0];
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
                                     reserved  => 0),
                        authorityTag => 0,
                        words    => [0 => packed (0),
                                     1 => packed (1),
                                     2 => packed (2),
                                     3 => packed (3)]);
                     ignore : Unsigned_64;
                  begin
                     ignore := replyCap (CapabilitySlot'Last, routeReply);
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
                                        reserved  => 0),
                           authorityTag => 0,
                           words    => [0 => Unsigned_64 (seq),
                                        1 => msg.words (0),
                                        2 => rtt,
                                        others => 0]);
                        ignore : Unsigned_64;
                     begin
                        ignore := replyCap (CapabilitySlot'Last, loopReply);
                     end;
                  else
                     routeLookup (dstIP, rIfIdx, nextHop);
                     if rIfIdx < 0 then
                        replyError (sender);
                     else
                        --  The next hop's link address; unresolved, the echo
                        --  is lost and the ping times out (asked meanwhile).
                        dstMAC := resolvedNeighbour (dstIP);

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
                            replySlot  => 0,
          others     => <>));

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

            when OP_NET_OPEN =>
               handleNetOpen (sender, msg);

            when Layout.OP_NET_ARENA =>
               handleArena (sender, msg);

            when Layout.OP_NET_SCOPE =>
               --  The scope over the 128-bit address space (Layout).
               declare
                  item : constant Network_Authority.Scope :=
                    Network_Grants.Scope_Of (networkGrants, sender, msg.authorityTag);
                  network : constant CuBit.Net_Address.Address :=
                    CuBit.Net_Address.Mapped (item.Network);
                  prefixBits : constant Unsigned_64 := Shift_Left (255, 32);
                  function half (first : Natural) return Unsigned_64 is
                     w : Unsigned_64 := 0;
                  begin
                     for k in reverse 0 .. 7 loop
                        w := Shift_Left (w, 8) or Unsigned_64 (network (first + k));
                     end loop;
                     return w;
                  end half;
                  answer : constant Message :=
                    (tag => (label => REPLY_OK, length => 3, flags => 0, reserved => 0),
                     authorityTag => 0,
                     words => [half (0), half (8),
                               (Network_Authority.Descriptor (item) and not prefixBits) or
                                 Shift_Left (Unsigned_64 (CuBit.Net_Address.Mapped_Prefix +
                                                          item.Prefix), 32),
                               0]);
                  ignore : Unsigned_64;
               begin
                  ignore := replyCap (CapabilitySlot'Last, answer);
               end;

            when Layout.OP_NET_ARENA_RELEASE =>
               declare
                  index : Channel_Arenas.Arena_Index;
                  ok, returned : Boolean;
               begin
                  Channel_Arenas.Unregister
                    (arenas, Unsigned_64 (sender), Channel_Arenas.Handle (msg.words (0)),
                     index, ok);
                  if ok then
                     CuBit.Memory_Grants.Return_Acquisition (arenaGrant (index), returned);
                     arenaBase (index) := System.Null_Address;
                     replyOKWord (sender, 0);
                  else
                     replyError (sender);
                  end if;
               end;


            when OP_NET_SHUT =>
               handleNetShut (sender, msg);

            when Layout.OP_NET_WAIT =>
               handleNetWait (sender, msg);

            when Layout.OP_NET_KICK =>
               --  One-way: no reply.
               kickChannels (sender, msg.words (0));
               if msg.words (1) = Layout.End_Wait then
                  endWait (sender);
               end if;

            when others =>
               declare
                  replyMsg : constant Message :=
                    (tag      => (label  => REPLY_ERR,
                                  length => 0,
                                  flags  => 0,
                                  reserved  => 0),
                     authorityTag => 0,
                     words    => [others => 0]);
                  ignore : Unsigned_64;
               begin
                  ignore := replyCap (CapabilitySlot'Last, replyMsg);
               end;
         end case;
         end if;
      end if;
   end loop;

   end Run;

end Netstack_Service;
