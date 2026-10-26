------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace network stack service (netstack.svc).
--
--  Owns all protocol processing (ARP, IPv4, ICMP, IPv6 on the link, UDP,
--  TCP). Network drivers attach interfaces (CuBit.Net_Link, as many as
--  memory holds): each gets a frame grant (CuBit.Frame_Rings) with its rings
--  and the driver's link report (docs/net-link.md). netmgr configures them
--  (CuBit.Net_Management) and hears of their changes through a Watch it
--  parks here.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.IPC;
with Startup;
with CuBit.Process_IDs;
with CuBit.Busy_Poll;
with Net;
with SipHash;
with TCP_Congestion;
with TCP_Header;
with DNS_Name;
with DNS_Response;
with DNS_Query;
with IPv4_Header;
with ICMPv4_Error;
with IPv4_Frame;
with IPv4_ICMP;
with UDP_Frame;
with TCP_Frame;
with TCP_Dispatch;
with Channel_Service;
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
with CuBit.Frame_Rings;
with CuBit.Net_Control_Queues;
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
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Channel_Rings;
with CuBit.Datagram_Rings;
with CuBit.Net_Channel_Layout;
with Netstack_Profile;
with CuBit.Net_Link;
with CuBit.Net_Management;
with CuBit.Wifi_Scan;
with CuBit.Control_Events;
with Link_Tracker;
with Interface_Table;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;

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

   --  Channel_Service (proved without the runtime) names the same kick bits.
   pragma Compile_Time_Error
     (Channel_Service.Kick_On_Send /= Layout.Kick_On_Send or else
      Channel_Service.Kick_On_Receive /= Layout.Kick_On_Receive,
      "Channel_Service kick bits differ from CuBit.Net_Channel_Layout");
   package Rings renames CuBit.Channel_Rings;
   package Datagrams renames CuBit.Datagram_Rings;
   package Prof renames Netstack_Profile;
   use type TCP_Listeners.Bind_Status;
   networkGrants : Network_Grants.Table;
   listeners : TCP_Listeners.Table;

   --  Where an answer goes (CAP-002): the request being handled now
   --  (answered by reply to its sender), or one saved with Save_Reply.
   type Reply_Target is record
      Saved  : Boolean := False;
      Handle : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   end record;
   Current_Request : constant Reply_Target := (others => <>);
   --  The sender of the request being handled (set by the message loop).
   Current_Sender : Process_ID := No_Process;

   function Send_Answer (Target : Reply_Target; Msg : Message) return Unsigned_64 is
     (if Target.Saved then replyCap (Target.Handle, Msg)
      else reply (Current_Sender, Msg));

   --  Each interface's frame grant (docs/net-link.md): a header page with
   --  the driver's link block, then the receive and transmit frame rings
   --  (CuBit.Frame_Rings). The driver drains the transmit ring; netstack
   --  answers the driver's parked Transmit_Wait as its doorbell.
   package Frames renames CuBit.Frame_Rings;
   package Frame_Ring renames CuBit.Frame_Rings.Rings;
   use type Frame_Ring.Index;
   package NL renames CuBit.Net_Link;
   package NM renames CuBit.Net_Management;
   use type NL.Interface_Handle, NL.Link_Condition, NL.Link_Kind;
   use type NL.Operation, NM.Operation, NL.Address_Outcome;
   use type Link_Tracker.Transition, Link_Tracker.Outcome;
   PACKET_BUF_PAGES : constant := Frames.Grant_Pages;
   PACKET_BUF_SIZE  : constant := Frames.Grant_Bytes;

   function arpIP (A : Net.IPv4Address) return ARP_Packet.IPv4 is ([A (0), A (1), A (2), A (3)]);
   function netMAC (M : ARP_Packet.MAC) return Net.MACAddress is
     ([M (0), M (1), M (2), M (3), M (4), M (5)]);
   function linkAddress (M : Net.MACAddress) return NL.Hardware_Address is
     ([M (0), M (1), M (2), M (3), M (4), M (5)]);
   function netAddress (M : NL.Hardware_Address) return Net.MACAddress is
     ([M (0), M (1), M (2), M (3), M (4), M (5)]);

   --  IPv6 on each interface (ipv6_link.ads, proved through
   --  tests/tcp-session/ipv6_link_proof.ads): its frames go out on the
   --  interface its Link names.
   procedure sendIPv6Frame (Owner : NL.Interface_Handle; Frame : IPv6_Header.Bytes)
     with Pre => IPv6_Frame.Emittable (Frame);
   package IPv6 is new IPv6_Link
     (Link_Owner => NL.Interface_Handle, Send => sendIPv6Frame, Log => debugPrint);

   --  The IPv4 self-test (tests/headless network-authority): once an
   --  interface is configured with a gateway, ping it once and log the reply.
   type SelfTest is (TEST_OFF, TEST_WAITING, TEST_SENT, TEST_DONE);
   SELF_TEST_TRIES : constant := 3;

   --  A request parked until netstack has something for it (a driver's
   --  Transmit_Wait, netmgr's DHCP_Await): its saved reply.
   type Parked is record
      present : Boolean := False;
      handle  : CuBit.Handles.Reply := CuBit.Handles.No_Reply;
   end record;

   type ARP_Asked is array (ARP_Cache.Index) of Unsigned_64;

   type InterfaceRecord is record
      handle    : NL.Interface_Handle;
      kind      : NL.Link_Kind := NL.Ethernet;
      mtu       : NL.MTU := NL.Minimum_MTU;
      offload   : NL.Offloads := NL.No_Offloads;
      identity  : NL.Stable_Identity := (Kind => NL.Factory_Address);
      mac       : Net.MACAddress := [others => 0];
      --  The address it attached with, and its name (from its identity):
      --  neither changes with Set_Address.
      factory   : Net.MACAddress := [others => 0];
      name      : NL.Interface_Name;
      driverPID : Process_ID := No_Process;
      --  IPv4 configuration (netmgr's Configure): its routes are the
      --  connected network and, with a gateway, a default route.
      configured : Boolean := False;
      ipv4      : Net.IPv4Address := [others => 0];
      prefix    : NM.IPv4_Prefix := 0;
      netmask   : Net.IPv4Address := [others => 0];
      gateway   : Net.IPv4Address := [others => 0];
      metric    : NM.Route_Metric := 0;
      --  Neighbours (proved: unsolicited replies never change it), and when
      --  each slot's neighbour was last asked about.
      arpCache  : ARP_Cache.Table;
      arpAskedAt : ARP_Asked := [others => 0];
      gwMAC     : Net.MACAddress := Net.ZERO_MAC;
      --  The frame grant (owned memory) and our view of its rings.
      pktBuf    : System.Address := System.Null_Address;
      pktGrant  : CuBit.Memory_Grants.Grant_Reference;
      txRing    : Frame_Ring.Producer;
      rxRing    : Frame_Ring.Consumer;
      rxEpoch   : Unsigned_32 := 0;
      txRungEpoch : Unsigned_32 := 0;
      txDoorbell : Boolean := False;   --  frames (or freed slots) since told
      txWait    : Parked;              --  the driver's Transmit_Wait
      --  The link, as the driver last reported it (Link_Tracker), and
      --  whether traffic may flow (its last report says Up).
      link      : Link_Tracker.Observation;
      usable    : Boolean := False;
      --  How many times the link has come Up (netmgr's Watch shows bounces).
      upCount   : NM.Up_Count := 0;
      --  Frames dropped because the link was not Up (reported, never
      --  printed per frame).
      linkDrops : Unsigned_64 := 0;
      --  The event sequence of the interface's last change (netmgr's Watch).
      lastChange : Unsigned_32 := 0;
      v6        : IPv6.Link;
      ipv6Started : Boolean := False;
      --  netmgr's DHCP page for this interface (DHCP_Open), its parked
      --  DHCP_Await, and a message that arrived with none parked.
      dhcpBase  : System.Address := System.Null_Address;
      dhcpGrant : CuBit.Memory_Grants.Grant_Reference;
      dhcpAwait : Parked;
      dhcpHeld  : Natural := 0;
      ipv4Test      : SelfTest := TEST_OFF;
      ipv4TestTries : Natural range 0 .. SELF_TEST_TRIES := 0;
      ipv4TestAt    : Unsigned_64 := 0;
   end record;

   --  The interfaces: no cap; as many as netstack's memory holds.
   package Ifaces is new Interface_Table (InterfaceRecord);
   subtype Interface_Access is Ifaces.Element_Access;
   use type Ifaces.Element_Access;
   No_Interface : constant Interface_Access := null;

   --  The interface a handle names, or null if it is gone.
   function interfaceOf (H : NL.Interface_Handle) return Interface_Access is
      Slot  : Natural;
      Found : Boolean;
   begin
      Ifaces.Find (H, Slot, Found);
      return (if Found then Ifaces.Get (Slot) else No_Interface);
   end interfaceOf;

   --  The neighbour's hardware address, if resolved.
   function arpLookup (ifc : Interface_Access; ip : Net.IPv4Address; mac : out Net.MACAddress)
     return Boolean
   is
      hw    : ARP_Packet.MAC;
      found : Boolean;
   begin
      ARP_Cache.Lookup (ifc.arpCache, arpIP (ip), hw, found);
      mac := (if found then netMAC (hw) else Net.ZERO_MAC);
      return found;
   end arpLookup;

   --  Interface events for netmgr (CuBit.Net_Management, Watch): one
   --  sequence; each interface keeps the sequence of its last change, and
   --  a detached interface leaves a record until netmgr's Seen passes it.
   eventSequence : Unsigned_32 := 0;
   type Detach_Record is record
      handle   : NL.Interface_Handle;
      sequence : Unsigned_32 := 0;
   end record;
   --  Detach records, in a CuAlloc-backed list (Detach_Log below).

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
   --  The largest TCP segment an IPv4 packet can carry.
   TCP_MAXIMUM_SEGMENT : constant := 65_535 - 20;
   secondaryDNS : Net.IPv4Address := [others => 0];

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
                        egress : NL.Interface_Handle;
                        index : out TCP_Slots.Connection_Reference);
   --  The slot's connection is gone from the table.
   procedure dropSlot (index : TCP_Slots.Connection_Index);

   procedure claimSlot (localIP, remoteIP : Net.IPv4Address;
                        localPort, remotePort : Unsigned_16;
                        remoteMAC : Net.MACAddress;
                        egress : NL.Interface_Handle;
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
                           remoteIP => remoteIP, remoteMAC => remoteMAC,
                           localIP => localIP, routed => True, egress => egress);
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
   --  Ephemeral ports (RFC 6056 3.3.3, "double-hash"): a keyed hash of the
   --  destination picks where a connection's search starts, and a counter
   --  per hash bucket moves it on, so ports are unpredictable to others
   --  and successive connections to one destination do not reuse a port.
   EPHEMERAL_FIRST : constant := 49_152;
   EPHEMERAL_COUNT : constant := 16_384;
   EPHEMERAL_TRIES : constant := 64;
   PORT_BUCKETS    : constant := 256;
   portKey   : SipHash.Key := (K0 => 0, K1 => 0);
   portSteps : array (0 .. PORT_BUCKETS - 1) of Unsigned_32 := [others => 0];
   --  Datagram channels opened: mixed into each one's starting port.
   udpOpens  : Unsigned_32 := 0;

   --  Application operations (must match kernel ipc_labels.ads). The
   --  driver's operations are CuBit.Net_Link's, netmgr's CuBit.Net_Management's.
   OP_NET_RESOLVE : constant Unsigned_32 := 16#0410#;
   OP_NET_OPEN    : constant Unsigned_32 := 16#0420#;
   OP_NET_SHUT    : constant Unsigned_32 := 16#0423#;

   REPLY_OK       : constant Unsigned_32 := 16#F000#;
   REPLY_ERR      : constant Unsigned_32 := 16#F001#;

   --  Static routes (netmgr's Route_Add). Each configured interface's own
   --  routes (its connected network and its default route) are part of
   --  the interface, so they need no room here; this table is only for
   --  routes netmgr adds beyond them.
   MAX_STATIC_ROUTES : constant := 16;
   type RouteEntry is record
      active  : Boolean := False;
      dest    : Net.IPv4Address := [others => 0];
      prefix  : NM.IPv4_Prefix := 0;
      gateway : Net.IPv4Address := [others => 0];
      via     : NL.Interface_Handle;
      metric  : NM.Route_Metric := 0;
   end record;
   routeTable : array (0 .. MAX_STATIC_ROUTES - 1) of RouteEntry;

   --  Detached interfaces netmgr has not seen yet (Watch).
   package Detach_Log is new Interface_Table (Detach_Record);

   --  Deferred TX queue: frames that found their interface's transmit ring
   --  full wait here, flushed between message receives.
   --  STOPGAP (2026-09-25): deep enough that Servo's parallel downloads do
   --  not drop frames while the driver's mailbox is full. TCP here has no
   --  retransmission yet, so a dropped segment stalls its connection. To be
   --  replaced by the netstack redesign (docs/servo-port.md).
   MAX_DEFERRED_TX  : constant := 256;
   MAX_DEFERRED_LEN : constant := 1514;  -- Ethernet header + 1500-byte MTU

   type FrameData is array (0 .. MAX_DEFERRED_LEN - 1) of Unsigned_8;

   --  One queue for every interface, in order; each frame names its
   --  interface (dropped when that interface is gone or not Up).
   type DeferredFrame is record
      data : FrameData;
      len  : Natural := 0;
      via  : NL.Interface_Handle;
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
      pid        : Process_ID := No_Process;
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
      Owner  : Process_ID;
      Arena  : Unsigned_64;
      Buffer : Unsigned_64) return Boolean
   is
      A : Channel_Arenas.Arena_Index;
      S : Channel_Arenas.Buffer_Index;
      Offset : Channel_Arenas.Span_Bytes;
      OK : Boolean;
   begin
      Channel_Arenas.Claim
        (arenas, To_Word (Owner), Channel_Arenas.Handle (Arena), Buffer,
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

   ---------------------------------------------------------------------------
   --  Control queues (Layout, "Control queues"): OPEN and SHUT as entries
   --  in a queue pair a process lent (CuBit.Net_Control_Queues). A queue
   --  acts with the authority tag of the QUEUE request that set it up.
   --  A request's answer goes back the way the request came: a reply
   --  capability, or its queue with the request's token.
   ---------------------------------------------------------------------------
   package Control renames CuBit.Net_Control_Queues.Queues;
   MAX_QUEUES : constant := 16;
   subtype Queue_Count is Natural range 0 .. MAX_QUEUES;
   subtype Queue_Index is Queue_Count range 1 .. MAX_QUEUES;
   No_Queue : constant Queue_Count := 0;

   type Reply_Route is record
      queue : Queue_Count := No_Queue;
      token : Control.Token := 0;
   end record;

   type ControlQueue is record
      owner  : Process_ID := No_Process;
      tag    : Unsigned_64 := 0;
      grant  : CuBit.Memory_Grants.Grant_Reference;
      base   : System.Address := System.Null_Address;
      server : Control.Server;
   end record;
   queues : array (Queue_Index) of ControlQueue;
   queueEpoch : Unsigned_32 := 0;

   --  The request being handled came from this queue entry (only while a
   --  queue entry is dispatched): immediate replies go to it.
   curRoute : Reply_Route;

   --  Answer on a queue; the owner's WAIT completes (defined below).
   procedure answerQueue (route : Reply_Route; ok : Boolean; value : Unsigned_64);

   --  Pending request queue (deferred reply for blocking ops)
   type PendingKind is (PENDING_NONE, PENDING_RESOLVE,
                        PENDING_CONNECT,
                        PENDING_OPEN, PENDING_PING,
                        PENDING_WAIT);
   DNS_ATTEMPTS : constant := 3;
   subtype DNS_Attempt is Natural range 0 .. DNS_ATTEMPTS;

   type PendingRequest is record
      kind       : PendingKind := PENDING_NONE;
      sender     : Process_ID := No_Process;
      connIdx    : Integer := -1;
      channelIdx : Integer := -1;
      bufAddr    : System.Address := System.Null_Address;
      bufOff     : Natural := 0;
      maxLen     : Natural := 0;
      txid       : Unsigned_16 := 0;
      dstPort    : Unsigned_16 := 0;
      replySlot  : Reply_Target := Current_Request;
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
      --  Where the answer goes: replySlot, or a control queue.
      route        : Reply_Route;
   end record;

   --  How long a resolver query waits for its answer, in all. Within it,
   --  a query is sent up to DNS_ATTEMPTS times, alternating servers, the
   --  wait doubling from DNS_FIRST_WAIT_MS (1 s, then 2 s: RFC 1536 3).
   DNS_TIMEOUT_MS    : constant Unsigned_64 := 5_000;
   DNS_FIRST_WAIT_MS : constant Unsigned_64 := 1_000;

   --  Deferred replies are saved in capability slots 16 .. 16 + MAX_PENDING - 1.
   MAX_PENDING : constant := 32;
   pendingReqs : array (0 .. MAX_PENDING - 1) of PendingRequest;

   --  A deferred request's answer, the way it came (defined below).
   procedure answerError (P : PendingRequest);
   procedure answerOK (P : PendingRequest; w0 : Unsigned_64);
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
   function admittedRequest (Owner : Process_ID; Request : Message)
                             return Boolean is
   begin
      --  A driver's (CuBit.Net_Link): the driver's badge and the
      --  operation's exact length. Handles are checked when used.
      if NL.Is_Operation (Request.tag.label) then
         return Request.authorityTag = Network_Authority.Driver_Authority_Tag and then
           Natural (Request.tag.length) =
             NL.Request_Words (NL.Operation_Of (Request.tag.label));
      end if;
      --  netmgr's (CuBit.Net_Management): the manager's badge, except
      --  the inspection operations any holder of the endpoint may use.
      if NM.Is_Operation (Request.tag.label) then
         return Natural (Request.tag.length) =
             NM.Request_Words (NM.Operation_Of (Request.tag.label)) and then
           (not NM.Manager_Only (NM.Operation_Of (Request.tag.label)) or else
            Request.authorityTag = Network_Authority.Manager_Authority_Tag);
      end if;
      case Request.tag.label is
         when Network_Authority.OP_INSTALL_SCOPE |
              Network_Authority.OP_RELEASE_SCOPE |
              Network_Authority.OP_RELEASE_OWNER =>
            return Request.authorityTag = Network_Authority.Policy_Authority_Tag;
         when OP_NET_RESOLVE =>
            return Request.tag.length in 1 .. 32 and then Network_Grants.May_Resolve
              (networkGrants, Owner, Request.authorityTag);
         when OP_NET_OPEN =>
            return Request.tag.protocol = 0 and then
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
         when Layout.OP_NET_QUEUE =>
            return Request.tag.length = 2 and then
              Request.words (0) <= CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT and then
              Request.words (1) in 1 .. CuBit.Memory_Grants.MAXIMUM_GENERATION and then
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
   --  printHex8
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  printMACAddr
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  printDec - print a small unsigned number in decimal
   ---------------------------------------------------------------------------
   --  A process as ps shows it, without the secondary stack.
   procedure printProcess (Process : Process_ID) is
      Text : CuBit.Process_IDs.Image_Text;
      Last : Positive;
   begin
      CuBit.Process_IDs.Image (Process, Text, Last);
      debugPrint (Text (1 .. Last));
   end printProcess;

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


   NO_ADDRESS : constant Net.IPv4Address := [others => 0];

   ---------------------------------------------------------------------------
   --  Logging (coordination/release-stack.md, "bring-up logging
   --  convention"): link transitions and interface changes as one line
   --  each on serial, built in a fixed buffer. netstack has no secondary
   --  stack, so it cannot link the log publisher; netmgr logs each
   --  transition to logstore from the interface events, with the same
   --  fields (docs/net-link.md section 5).
   ---------------------------------------------------------------------------
   type Log_Level is (Information, Warning);

   logText   : String (1 .. 200);
   logLength : Natural range 0 .. 200 := 0;

   procedure logPut (Text : String) is
      Kept : constant Natural := Natural'Min (Text'Length, logText'Last - logLength);
   begin
      logText (logLength + 1 .. logLength + Kept) := Text (Text'First .. Text'First + Kept - 1);
      logLength := logLength + Kept;
   end logPut;

   procedure logDec (Value : Unsigned_64) is
      Digits_Of : String (1 .. 20);
      Pos : Natural := Digits_Of'Last;
      V : Unsigned_64 := Value;
   begin
      loop
         Digits_Of (Pos) := Character'Val (Character'Pos ('0') + Natural (V mod 10));
         V := V / 10;
         exit when V = 0;
         Pos := Pos - 1;
      end loop;
      logPut (Digits_Of (Pos .. Digits_Of'Last));
   end logDec;

   procedure logMAC (M : Net.MACAddress) is
      Hex : constant String := "0123456789abcdef";
   begin
      for I in M'Range loop
         if I > M'First then
            logPut (":");
         end if;
         logPut ([1 => Hex (Natural (Shift_Right (M (I), 4)) + 1),
                  2 => Hex (Natural (M (I) and 16#F#) + 1)]);
      end loop;
   end logMAC;

   procedure logIP (IP : Net.IPv4Address) is
   begin
      for I in IP'Range loop
         if I > IP'First then
            logPut (".");
         end if;
         logDec (Unsigned_64 (IP (I)));
      end loop;
   end logIP;

   procedure logSend (Level : Log_Level := Information) is
      pragma Unreferenced (Level);   --  serial has no levels
   begin
      debugPrint (logText (1 .. logLength));
      debugPrint ("" & LF);
      logLength := 0;
   end logSend;

   --  The interface configured with ip, or null (any link state: frames for
   --  an interface that is down do not arrive).
   function interfaceForIP (ip : Net.IPv4Address) return Interface_Access is
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) and then Ifaces.Get (S).configured and then
           Ifaces.Get (S).ipv4 = ip
         then
            return Ifaces.Get (S);
         end if;
      end loop;
      return No_Interface;
   end interfaceForIP;

   function isOurAddress (ip : Net.IPv4Address) return Boolean is
     (interfaceForIP (ip) /= No_Interface);

   ---------------------------------------------------------------------------
   --  Routing (docs/net-link.md section 6): every configured interface whose
   --  link is Up offers its connected network and, with a gateway, a
   --  default route, both at its metric; static routes add to them. The
   --  longest prefix wins, then the lowest metric. Routes through an
   --  interface whose link is not Up are skipped, so traffic fails over to
   --  another interface's route when there is one.
   ---------------------------------------------------------------------------

   --  The netmask of a prefix length.
   function maskOf (prefix : NM.IPv4_Prefix) return Net.IPv4Address is
      m : Net.IPv4Address := [others => 0];
   begin
      for I in m'Range loop
         m (I) := (if prefix >= 8 * (I + 1) then 16#FF#
                   elsif prefix <= 8 * I then 0
                   else Shift_Left (Unsigned_8'Last, 8 - (prefix - 8 * I)));
      end loop;
      return m;
   end maskOf;

   procedure routeLookup (dstIP   : Net.IPv4Address;
                          via     : out Interface_Access;
                          nextHop : out Net.IPv4Address)
   is
      bestPrefix : Integer := -1;
      bestMetric : NM.Route_Metric := NM.Route_Metric'Last;

      procedure consider (ifc : Interface_Access; prefix : NM.IPv4_Prefix;
                          metric : NM.Route_Metric; gateway : Net.IPv4Address) is
      begin
         if prefix > bestPrefix or else (prefix = bestPrefix and then metric < bestMetric) then
            bestPrefix := prefix;
            bestMetric := metric;
            via := ifc;
            nextHop := (if gateway = NO_ADDRESS then dstIP else gateway);
         end if;
      end consider;
   begin
      via := No_Interface;
      nextHop := NO_ADDRESS;
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            declare
               ifc : constant Interface_Access := Ifaces.Get (S);
            begin
               if ifc.configured and then ifc.usable then
                  if Net.matchesPrefix (dstIP, ifc.ipv4, ifc.prefix) then
                     consider (ifc, ifc.prefix, ifc.metric, NO_ADDRESS);
                  end if;
                  if ifc.gateway /= NO_ADDRESS then
                     consider (ifc, 0, ifc.metric, ifc.gateway);
                  end if;
               end if;
            end;
         end if;
      end loop;
      for R of routeTable loop
         if R.active and then Net.matchesPrefix (dstIP, R.dest, R.prefix) then
            declare
               ifc : constant Interface_Access := interfaceOf (R.via);
            begin
               if ifc /= No_Interface and then ifc.configured and then ifc.usable then
                  consider (ifc, R.prefix, R.metric, R.gateway);
               end if;
            end;
         end if;
      end loop;
   end routeLookup;

   ---------------------------------------------------------------------------
   --  Sending frames: each interface's transmit ring. A frame for an
   --  interface whose link is not Up is dropped (counted): docs/net-link.md
   --  section 5. Checking costs one field of the interface's record, never
   --  a read of shared memory.
   ---------------------------------------------------------------------------
   --  The transmit direction's header words.
   function txWord (ifc : Interface_Access; Offset : Natural) return System.Address is
     (ifc.pktBuf + Storage_Offset (Frames.Transmit_Header_At + Offset));

   --  Take the driver's consumed count if it is sane (a bad one frees
   --  nothing); True if a frame fits in the ring now.
   function txRoom (ifc : Interface_Access; frameLen : Natural) return Boolean is
      consumed : Unsigned_32 with Volatile, Import,
        Address => txWord (ifc, Frames.Consumed_At);
      ignore : Boolean;
   begin
      Frame_Ring.Accept_Consumed (ifc.txRing, Frame_Ring.Index (consumed), ignore);
      return Frame_Ring.Space (ifc.txRing) > 0 and then frameLen <= Frames.Maximum_Frame;
   end txRoom;

   --  The next transmit slot's start.
   function txSlot (ifc : Interface_Access) return System.Address is
     (ifc.pktBuf + Storage_Offset
        (Frames.Transmit_Slot_At (Frame_Ring.Next_Slot (ifc.txRing))));

   --  Hand the frame in the next slot (frameLen bytes) to the driver.
   procedure txCommit (ifc : Interface_Access; frameLen : Natural) is
      produced : Unsigned_32 with Volatile, Import,
        Address => txWord (ifc, Frames.Produced_At);
      len : Unsigned_32 with Volatile, Import,
        Address => txSlot (ifc) + Storage_Offset (Frames.Length_At);
   begin
      len := Unsigned_32 (frameLen);
      --  The frame is written before the count that hands it over.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      Frame_Ring.Commit (ifc.txRing);
      produced := Unsigned_32 (ifc.txRing.Produced);
      ifc.txDoorbell := True;
   end txCommit;

   function doSendFrame (ifc : Interface_Access; frameAddr : System.Address;
                         frameLen  : Natural) return Boolean is
   begin
      if not txRoom (ifc, frameLen) then
         return False;   --  the ring is full: the caller defers the frame
      end if;
      declare
         type Frame is array (0 .. frameLen - 1) of Unsigned_8;
         src : Frame with Import, Address => frameAddr;
         dst : Frame with Import, Address => txSlot (ifc) + Storage_Offset (Frames.Frame_At);
      begin
         dst := src;
      end;
      txCommit (ifc, frameLen);
      return True;
   end doSendFrame;

   --  A frame for ifc is dropped: its link is not Up.
   procedure linkDrop (ifc : Interface_Access) is
   begin
      ifc.linkDrops := ifc.linkDrops + 1;
   end linkDrop;

   --  Zero-copy transmit: the next ring slot's frame area, to build a frame
   --  in place, or Null_Address if the link is not Up, the ring is full, or
   --  older frames still wait in the deferred queue (they go first).
   --  txCommit hands it over.
   function txReserve (ifc : Interface_Access; frameLen : Natural) return System.Address is
   begin
      if ifc = No_Interface or else not ifc.usable or else deferredCount > 0 or else
        not txRoom (ifc, frameLen)
      then
         return System.Null_Address;
      end if;
      return txSlot (ifc) + Storage_Offset (Frames.Frame_At);
   end txReserve;

   --  The answer to a parked request: empty, REPLY_OK.
   procedure answerParked (p : in out Parked; w0 : Unsigned_64 := 0; ok : Boolean := True) is
      msg : constant Message :=
        (tag      => (label => (if ok then REPLY_OK else REPLY_ERR), length => 1,
                      protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words    => [0 => w0, others => 0]);
      ignore : Unsigned_64;
   begin
      if p.present then
         p.present := False;
         ignore := replyCap (p.handle, msg);
      end if;
   end answerParked;

   --  Tell each idle driver its transmit ring has frames (or its receive
   --  ring room): answer its parked Transmit_Wait once per wake epoch it
   --  publishes. A busy driver drains the ring before it waits again.
   procedure ringTXDoorbell is
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            declare
               ifc : constant Interface_Access := Ifaces.Get (S);
            begin
               if ifc.txDoorbell and then ifc.txWait.present then
                  --  The counts were published before the epoch is read.
                  System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
                  declare
                     epoch : Unsigned_32 with Volatile, Import,
                       Address => txWord (ifc, Frames.Wake_At);
                     seen : constant Unsigned_32 := epoch;
                  begin
                     ifc.txDoorbell := False;
                     if seen /= 0 and then seen /= ifc.txRungEpoch then
                        ifc.txRungEpoch := seen;
                        answerParked (ifc.txWait);
                     end if;
                  end;
               end if;
            end;
         end if;
      end loop;
   end ringTXDoorbell;

   ---------------------------------------------------------------------------
   --  flushOneDeferredTX - hand the oldest deferred frame to its interface
   --
   --  True if it went (or was dropped: its interface is gone or down), False
   --  if its ring is still full.
   ---------------------------------------------------------------------------
   function flushOneDeferredTX return Boolean is
      ok  : Boolean;
      ifc : Interface_Access;
   begin
      if deferredCount = 0 then
         return True;
      end if;
      ifc := interfaceOf (deferredTX (deferredHead).via);
      if ifc = No_Interface or else not ifc.usable then
         ok := True;   --  nowhere to go: dropped
      else
         ok := doSendFrame (ifc, deferredTX (deferredHead).data'Address,
                            deferredTX (deferredHead).len);
      end if;
      if ok then
         deferredHead := (deferredHead + 1) mod MAX_DEFERRED_TX;
         deferredCount := deferredCount - 1;
      end if;
      return ok;
   end flushOneDeferredTX;

   ---------------------------------------------------------------------------
   --  sendFrame - put a frame in ifc's transmit ring, or the deferred queue
   --  if the ring is full (older frames go first: sending around them would
   --  reorder the wire).
   ---------------------------------------------------------------------------
   procedure sendFrame (ifc : Interface_Access; frameAddr : System.Address; frameLen : Natural) is
      ok : Boolean;
   begin
      if ifc = No_Interface or else ifc.pktBuf = System.Null_Address then
         return;
      end if;
      if not ifc.usable then
         linkDrop (ifc);
         return;
      end if;
      if frameLen > MAX_DEFERRED_LEN then
         debugPrint ("netstack: sendFrame: frame too large" & LF);
         return;
      end if;

      while deferredCount > 0 and then flushOneDeferredTX loop
         null;
      end loop;
      ok := deferredCount = 0 and then doSendFrame (ifc, frameAddr, frameLen);
      if not ok then
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
               deferredTX (slot).via := ifc.handle;
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

   function v4 (a : Net.IPv4Address) return IPv4_Header.Address is
     ([a (0), a (1), a (2), a (3)]);

   --  The interface IPv4 frames built by the proved generics (ICMPv4) go
   --  out on: set by whoever asks them to send.
   currentIf : Interface_Access := No_Interface;

   procedure sendIPv4 (frame : IPv4_Header.Bytes)
     with Pre => IPv4_Frame.Emittable (frame);
   procedure sendIPv4 (frame : IPv4_Header.Bytes) is
   begin
      sendFrame (currentIf, frame'Address, frame'Length);
   end sendIPv4;

   procedure sendIPv6Frame (Owner : NL.Interface_Handle; Frame : IPv6_Header.Bytes) is
   begin
      sendFrame (interfaceOf (Owner), Frame'Address, Frame'Length);
   end sendIPv6Frame;

   ---------------------------------------------------------------------------
   --  ARP on each interface
   ---------------------------------------------------------------------------
   --  An ARP frame from ifc's address, built by the proved ARP_Packet.Build.
   procedure sendARP (ifc      : Interface_Access;
                      op       : ARP_Packet.Operation;
                      ethDest  : Net.MACAddress;
                      targetHW : Net.MACAddress;
                      targetIP : Net.IPv4Address) is
      frame : ARP_Packet.Frame_Bytes;
   begin
      ARP_Packet.Build
        ((Op        => op,
          Sender_HW => ARP_Packet.MAC (ifc.mac),
          Sender_IP => arpIP (ifc.ipv4),
          Target_HW => ARP_Packet.MAC (targetHW),
          Target_IP => arpIP (targetIP)),
         ARP_Packet.MAC (ethDest), frame);
      sendFrame (ifc, frame'Address, frame'Length);
   end sendARP;

   procedure sendARPReply (ifc    : Interface_Access;
                           dstMAC : Net.MACAddress;
                           dstIP  : Net.IPv4Address) is
   begin
      sendARP (ifc, ARP_Packet.Reply, dstMAC, dstMAC, dstIP);
      if Trace_Packets then
         debugPrint ("NET: sent ARP reply to ");
         printIP (dstIP);
         debugPrint ("" & LF);
      end if;
   end sendARPReply;

   --  An ARP announcement (RFC 5227 2.3): a request whose sender and
   --  target addresses are both ours, to everyone.
   procedure sendGratuitousARP (ifc : Interface_Access) is
   begin
      sendARP (ifc, ARP_Packet.Request, Net.BROADCAST_MAC, Net.ZERO_MAC, ifc.ipv4);
      debugPrint ("NET: sent gratuitous ARP for ");
      printIP (ifc.ipv4);
      debugPrint ("" & LF);
   end sendGratuitousARP;

   procedure sendARPRequest (ifc : Interface_Access; targetIP : Net.IPv4Address) is
   begin
      --  The reply to this, and only it, may resolve targetIP.
      ARP_Cache.Request (ifc.arpCache, arpIP (targetIP), syscall (SYSCALL_GETTIME));
      declare
         pos : constant Integer := ARP_Cache.Position (ifc.arpCache, arpIP (targetIP));
      begin
         if pos >= 0 then
            ifc.arpAskedAt (pos) := clockNow;
         end if;
      end;
      sendARP (ifc, ARP_Packet.Request, Net.BROADCAST_MAC, Net.ZERO_MAC, targetIP);
   end sendARPRequest;

   --  How often an unanswered neighbour is asked again (RFC 1122 2.3.2.1:
   --  at most one request per second per destination).
   ARP_RETRY_MS : constant Unsigned_64 := 1_000;
   --  A mapping unconfirmed this long is asked again on its next use, and
   --  a question unanswered this long is given up (RFC 4861's
   --  REACHABLE_TIME and its probe window, as Linux applies them to ARP).
   ARP_REACHABLE_MS : constant Unsigned_64 := 30_000;
   ARP_PROBE_MS     : constant Unsigned_64 := 3_000;

   --  hop's mapping on ifc is doubted (stale, or traffic through it
   --  stalls): ask again. The answer may change the address (a replaced
   --  router).
   procedure doubtNeighbour (ifc : Interface_Access; hop : Net.IPv4Address) is
      use type ARP_Cache.State;
   begin
      if ifc = No_Interface then
         return;
      end if;
      declare
         pos : constant Integer := ARP_Cache.Position (ifc.arpCache, arpIP (hop));
      begin
         --  Only a confirmed mapping is doubted; one already being asked
         --  about is left to its question.
         if hop /= NO_ADDRESS and then pos >= 0 and then
           ifc.arpCache (pos).St = ARP_Cache.Resolved
         then
            ARP_Cache.Reconfirm (ifc.arpCache, arpIP (hop), clockNow);
            sendARPRequest (ifc, hop);
         end if;
      end;
   end doubtNeighbour;

   --  Ask for hop's link address, unless we asked within ARP_RETRY_MS.
   procedure requestNeighbour (ifc : Interface_Access; hop : Net.IPv4Address) is
      pos : constant Integer := ARP_Cache.Position (ifc.arpCache, arpIP (hop));
      use type ARP_Cache.State;
   begin
      if pos >= 0 and then
        (ifc.arpCache (pos).St = ARP_Cache.Resolved or else
         clockNow < deadlineAfter (ifc.arpAskedAt (pos), ARP_RETRY_MS))
      then
         return;   --  confirmed, or asked within the last second
      end if;
      sendARPRequest (ifc, hop);
   end requestNeighbour;

   --  hop's mapping has gone unconfirmed for ARP_REACHABLE_MS: ask again,
   --  using the address meanwhile.
   procedure doubtIfStale (ifc : Interface_Access; hop : Net.IPv4Address) is
      pos : constant Integer := ARP_Cache.Position (ifc.arpCache, arpIP (hop));
      use type ARP_Cache.State;
   begin
      if pos >= 0 and then ifc.arpCache (pos).St = ARP_Cache.Resolved and then
        clockNow >= deadlineAfter (ifc.arpCache (pos).Since, ARP_REACHABLE_MS)
      then
         doubtNeighbour (ifc, hop);
      end if;
   end doubtIfStale;

   --  The interface and link address to send to dstIP by: the route's
   --  interface and its next hop's address (the gateway, or dstIP itself on
   --  a connected network), from that interface's ARP cache. If it is not
   --  resolved yet, a request goes out and found is False: the caller drops
   --  the frame, and TCP's retransmission or the resolver's retry sends it
   --  again. via is null when no usable route exists.
   procedure neighbourMAC (dstIP : Net.IPv4Address;
                           via   : out Interface_Access;
                           mac   : out Net.MACAddress;
                           found : out Boolean) is
      hop : Net.IPv4Address;
   begin
      mac := Net.ZERO_MAC;
      found := False;
      routeLookup (dstIP, via, hop);
      if via = No_Interface then
         return;   --  no usable route
      end if;
      found := arpLookup (via, hop, mac);
      --  Unresolved or being reconfirmed: ask (at most once a second). A
      --  confirmed mapping gone stale is doubted.
      requestNeighbour (via, hop);
      if found then
         doubtIfStale (via, hop);
      end if;
   end neighbourMAC;

   --  The next hop for dstIP and its interface (null: no usable route).
   procedure nextHopOf (dstIP : Net.IPv4Address; via : out Interface_Access;
                        hop : out Net.IPv4Address) is
   begin
      routeLookup (dstIP, via, hop);
   end nextHopOf;

   ---------------------------------------------------------------------------
   --  sendUDP - build and send a UDP datagram inside an IPv4 frame
   --
   --  The whole frame is built by the proved UDP_Frame.
   ---------------------------------------------------------------------------
   procedure sendUDP (ifc        : Interface_Access;
                      dstIP      : Net.IPv4Address;
                      dstMAC     : Net.MACAddress;
                      srcPort    : Unsigned_16;
                      dstPort    : Unsigned_16;
                      payload    : System.Address;
                      payloadLen : Natural) is
      ours : constant IPv4_Header.Address := v4 (ifc.ipv4);
      dest : constant IPv4_Header.Address := v4 (dstIP);
   begin
      --  Built whole by the proved UDP_Frame (only unicast is sent).
      if payloadLen > UDP_Frame.Maximum_Payload or else
        not IPv4_Frame.Unicast (ours) or else not IPv4_Frame.Unicast (dest)
      then
         return;
      end if;
      declare
         data  : constant IPv4_Header.Bytes (0 .. payloadLen - 1)
           with Import, Address => payload;
         frame : IPv4_Header.Bytes (0 .. UDP_Frame.Payload_At + payloadLen - 1);
      begin
         UDP_Frame.Build
           (IPv4_Frame.MAC (ifc.mac), IPv4_Frame.MAC (dstMAC), ours, dest,
            srcPort, dstPort, data, frame);
         sendFrame (ifc, frame'Address, frame'Length);
      end;
   end sendUDP;

   ---------------------------------------------------------------------------
   --  sendDNSQuery - send a DNS A-record query for a given hostname
   --
   --  Encodes hostname as DNS labels (split on '.'), uses given TXID.
   ---------------------------------------------------------------------------
   --  A query for an A record of name (wire form, from DNS_Name.Encode),
   --  from the query's own source port.
   procedure sendDNSQuery (name    : DNS_Name.Wire;
                           nameLen : DNS_Name.Wire_Length;
                           txid, port : Unsigned_16;
                           server  : Net.IPv4Address) is
      query  : DNS_Query.Message;
      length : Natural;
      dnsMAC : Net.MACAddress;
      found  : Boolean;
      via    : Interface_Access;
   begin
      if nameLen = 0 then
         return;
      end if;
      --  Built by the proved DNS_Query.
      DNS_Query.Build (txid, name, nameLen, query, length);
      --  The server's next hop; unresolved, this attempt is lost and the
      --  next one (or an ARP reply) sends the question again.
      neighbourMAC (server, via, dnsMAC, found);
      if found then
         sendUDP (via, server, dnsMAC, port, DNS_SERVER_PORT, query'Address, length);
      end if;
   end sendDNSQuery;


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
      sendDNSQuery (P.question, P.questionLen, P.txid, P.queryPort,
                    dnsServer (P.attempts));
      P.attempts := P.attempts + 1;
      P.nextSend :=
        (if P.attempts < DNS_ATTEMPTS
         then deadlineAfter (Now, DNS_FIRST_WAIT_MS * 2 ** (P.attempts - 1))
         else Unsigned_64'Last);
   end sendAttempt;


   --  Forward declarations for functions used by handleDNSResponse
   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstPort : Unsigned_16) return Integer;
   procedure replyError
     (to   : Process_ID;
      slot : Reply_Target := Current_Request);

   --  A query that will not be answered: its requester gets an error, and
   --  an OPEN's channel is released.
   procedure failQuery (P : in out PendingRequest) is
   begin
      answerError (P);
      if P.kind = PENDING_OPEN and then P.channelIdx in channels'Range then
         releaseChannel (P.channelIdx);
      end if;
      P.kind := PENDING_NONE;
   end failQuery;

   procedure openDatagram
     (chIdx : Network_Channel_Handles.Channel_Index;
      owner : Process_ID;
      dstIP : Net.IPv4Address;
      port  : Unsigned_16;
      slot  : Reply_Target);

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
                               protocol  => 0, ipc => <>,
                               reserved  => 0),
                  authorityTag => 0,
                  words    => [0 => ipPacked, others => 0]);
               ignore : Unsigned_64;
            begin
               ignore := Send_Answer (pendingReqs (i).replySlot, replyMsg);
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
                  connIdx := tcpConnect (resolvedIP, pendingReqs (i).dstPort);
                  if connIdx < 0 then
                     releaseChannel (chIdx);
                     answerError (pendingReqs (i));
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
                  answerError (pendingReqs (i));
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
   --  handleUDP - parse a UDP datagram (UDP_Header), dispatch on port
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
      if h.Length > UDP_Frame.Maximum_Datagram or else
        not UDP_Frame.Checksum_OK (v4 (srcIP), v4 (dstIP), IPv4_Header.Bytes (datagram (0 .. h.Length - 1)))
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
   --  sendTCPSegment - write a TCP segment (TCP_Header, TCP_Frame)
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
                              ifc     : Interface_Access;
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

      --  The checksum, IPv4 and Ethernet headers around the segment, by the
      --  proved TCP_Frame (the frame is proved to have unicast addresses;
      --  anything else is not sent).
      declare
         frame : IPv4_Header.Bytes (0 .. frameLen - 1) with Import, Address => fAddr;
         ours  : constant IPv4_Header.Address := v4 (conn.localIP);
         peer  : constant IPv4_Header.Address := v4 (conn.remoteIP);
      begin
         --  sendTCPSegment sends only between unicast addresses.
         if IPv4_Frame.Unicast (ours) and then IPv4_Frame.Unicast (peer) then
            TCP_Frame.Finish (frame, IPv4_Frame.MAC (ifc.mac),
                              IPv4_Frame.MAC (conn.remoteMAC), ours, peer);
         end if;
      end;
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
      ifc      : Interface_Access := No_Interface;
      hop      : Net.IPv4Address;
   begin
      --  Only between unicast addresses (connect refuses any other peer;
      --  arriving segments from one are dropped).
      if not IPv4_Frame.Unicast (v4 (conn.localIP)) or else
        not IPv4_Frame.Unicast (v4 (conn.remoteIP))
      then
         return;
      end if;
      --  The connection's interface (one record lookup), or for a segment
      --  without a slot (TIME-WAIT) the route's. Gone or down: dropped;
      --  retransmission sends it again once the link is back.
      if conn.routed then
         ifc := interfaceOf (conn.egress);
      else
         routeLookup (conn.remoteIP, ifc, hop);
      end if;
      if ifc = No_Interface then
         return;
      end if;
      --  A connection opened before its next hop answered ARP (or whose
      --  link went down and came back): resolve now, or drop the segment.
      if conn.remoteMAC = Net.ZERO_MAC then
         declare
            via : Interface_Access;
         begin
            neighbourMAC (conn.remoteIP, via, conn.remoteMAC, found);
            if not found or else via /= ifc then
               return;
            end if;
         end;
      end if;
      slot := txReserve (ifc, frameLen);
      if slot /= System.Null_Address then
         buildTCPSegment (slot, ifc, conn, flags, seqNum, ackNum, window, payload, payLen,
                          synOptions, windowScale);
         txCommit (ifc, frameLen);
      else
         declare
            frame : array (0 .. frameLen - 1) of Unsigned_8;
         begin
            buildTCPSegment (frame'Address, ifc, conn, flags, seqNum, ackNum, window, payload, payLen,
                             synOptions, windowScale);
            sendFrame (ifc, frame'Address, frameLen);
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
         (Local       => mapped (tcpConns (Index).localIP),
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
                                              w.Remote_MAC (3), w.Remote_MAC (4), w.Remote_MAC (5)],
                                localIP => seg.dstIP, routed => False, egress => <>),
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

   --  The interface a new connection to dstIP leaves by: the route's.
   function egressFor (dstIP : Net.IPv4Address) return Interface_Access is
      via : Interface_Access;
      hop : Net.IPv4Address;
   begin
      routeLookup (dstIP, via, hop);
      return via;
   end egressFor;

   --  A connection to dstIP from the address of the interface its route
   --  leaves by (source selection: the egress interface's address); -1
   --  with no usable route or no free slot.
   function tcpConnect (dstIP   : Net.IPv4Address;
                        dstPort : Unsigned_16) return Integer is
      via   : constant Interface_Access := egressFor (dstIP);
      ours  : constant Net.IPv4Address := (if via = No_Interface then NO_ADDRESS else via.ipv4);
      h     : constant Unsigned_64 := SipHash.Hash
        (portKey, [ours (0), ours (1), ours (2), ours (3), dstIP (0), dstIP (1), dstIP (2),
                   dstIP (3), Unsigned_8 (dstPort / 256), Unsigned_8 (dstPort mod 256)]);
      offset : constant Unsigned_32 := Unsigned_32 (h and 16#FFFF_FFFF#);
      bucket : constant Natural := Natural (Shift_Right (h, 32) mod PORT_BUCKETS);
      lport : Unsigned_16 := 0;
      found : Boolean := False;
      idx   : Integer;
   begin
      if not IPv4_Frame.Unicast (v4 (dstIP)) or else via = No_Interface then
         return -1;   --  never a broadcast, multicast, loopback or zero peer
      end if;
      --  A port is free for this destination if nothing listens on it, no
      --  connection uses the 4-tuple, and it is not in TIME-WAIT (whose
      --  protection a new incarnation would defeat).
      for Attempt in 0 .. EPHEMERAL_TRIES - 1 loop
         lport := Unsigned_16
           (EPHEMERAL_FIRST +
            (offset + portSteps (bucket) + Unsigned_32 (Attempt)) mod EPHEMERAL_COUNT);
         if TCP_Listeners.Find (listeners, policyAddress (ours), lport) = 0 and then
           Conns.Find (connTable, tupleOf (ours, dstIP, lport, dstPort)) = Conns.No_Slot and then
           TCP_Time_Wait.Find (timeWaits, waitKey (dstIP, dstPort, lport)) = TCP_Time_Wait.No_Entry
         then
            portSteps (bucket) := portSteps (bucket) + Unsigned_32 (Attempt) + 1;
            found := True;
            exit;
         end if;
      end loop;
      if not found then
         return -1;
      end if;

      declare
         dstMAC : Net.MACAddress;
         found  : Boolean;
         hopVia : Interface_Access;
      begin
         --  Unresolved: the SYN waits for ARP (sendTCPSegment resolves).
         neighbourMAC (dstIP, hopVia, dstMAC, found);
         claimSlot (ours, dstIP, lport, dstPort, dstMAC, via.handle, idx);
      end;
      if idx < 0 then
         return -1;
      end if;

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
     (to   : Process_ID;
      w0   : Unsigned_64;
      slot : Reply_Target := Current_Request);

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
               answerOK (pendingReqs (i),
                         Unsigned_64 (Network_Channel_Handles.Value
                           (channelHandles, pendingReqs (i).channelIdx)));
            else
               --  Legacy API: reply with raw connIdx
               answerOK (pendingReqs (i), Unsigned_64 (connIdx));
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
            answerOK (pendingReqs (i),
                      Unsigned_64 (Network_Channel_Handles.Value
                        (channelHandles, pendingReqs (i).channelIdx)));
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
            answerError (pendingReqs (i));
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
      --  The connection's interface, for building segments in its ring;
      --  none (gone, down, or the next hop unresolved): sendTCPSegment.
      egress : constant Interface_Access :=
        (if tcpConns (Index).routed and then tcpConns (Index).remoteMAC /= Net.ZERO_MAC
         then interfaceOf (tcpConns (Index).egress) else No_Interface);
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
                 txReserve (egress, TCP_PAYLOAD_OFFSET + LINK_MSS);
               inPlace : Sends.Byte_Array (1 .. LINK_MSS)
                  with Import, Address => slot + Storage_Offset (TCP_PAYLOAD_OFFSET);
            begin
               if slot /= System.Null_Address then
                  Flows.Send (F, tcpPool, Now, inPlace, first, taken, fin);
                  exit when taken = 0 and then not fin;
                  buildTCPSegment
                    (slot, egress, tcpConns (Index),
                     TCP_Wire.Flag_ACK or (if taken > 0 then TCP_Wire.Flag_PSH else 0) or
                       (if fin then TCP_Wire.Flag_FIN else 0),
                     Unsigned_32 (first), Unsigned_32 (F.E.C.Rcv_Nxt), windowOf (Index),
                     slot + Storage_Offset (TCP_PAYLOAD_OFFSET), taken, payloadInPlace => True);
                  txCommit (egress, TCP_PAYLOAD_OFFSET + taken);
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
   function readyMask (owner : Process_ID; interest : Unsigned_64) return Unsigned_64 is
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

   --  A word of queue q's grant.
   function queueWord (q : Queue_Index; offset : Natural) return System.Address is
     (queues (q).base + Storage_Offset (offset));

   --  Take the client's reaped index: its answers' slots are free again.
   procedure acceptReaped (q : Queue_Index) is
      consumed : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, Layout.Queue_Completions_At + Layout.Queue_Consumed_At);
      ignore : Boolean;
   begin
      Control.Accept_Reaped
        (queues (q).server, Control.Completions.Index (consumed), ignore);
   end acceptReaped;

   --  owner has answers it has not reaped.
   function answersWaiting (owner : Process_ID) return Boolean is
   begin
      for q in queues'Range loop
         if queues (q).owner = owner then
            acceptReaped (q);
            if queues (q).server.Answers.Fill > 0 then
               return True;
            end if;
         end if;
      end loop;
      return False;
   end answersWaiting;

   --  Complete a WAIT with the ready channels and whether answers wait.
   procedure replyWait (P : PendingRequest; mask : Unsigned_64; answers : Boolean) is
      msg : constant Message :=
        (tag      => (label  => REPLY_OK,
                      length => 2,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
         authorityTag => 0,
         words    => [0 => mask,
                      1 => (if answers then Layout.Wait_Answers else 0),
                      others => 0]);
      ignore : Unsigned_64;
   begin
      ignore := Send_Answer (P.replySlot, msg);
   end replyWait;

   procedure answerQueue (route : Reply_Route; ok : Boolean; value : Unsigned_64) is
      q : constant Queue_Count := route.queue;
   begin
      --  The queue went with its owner, or owes nothing: nowhere to answer.
      if q = No_Queue or else queues (q).owner = No_Process or else
        queues (q).server.Owed = 0
      then
         return;
      end if;
      declare
         ring : Control.Completions.Ring with Import,
           Address => queueWord (q, Layout.Queue_Answers_At);
         produced : Unsigned_32 with Volatile, Import,
           Address => queueWord (q, Layout.Queue_Completions_At + Layout.Queue_Produced_At);
      begin
         --  Owed <= Space (Control.Valid): the answer has its slot.
         Control.Complete
           (queues (q).server, ring, route.token,
            (Status => (if ok then Layout.Answer_OK else Layout.Answer_Refused),
             Value  => value,
             others => <>));
         --  The answer is written before the count that hands it over.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         produced := Unsigned_32 (queues (q).server.Answers.Produced);
      end;
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then P.sender = queues (q).owner then
            replyWait (P, readyMask (P.sender, P.waitMask), True);
            P.kind := PENDING_NONE;
         end if;
      end loop;
   end answerQueue;

   --  Complete owner's waiting WAIT if any of its channels is ready.
   procedure notifyWaiter (owner : Process_ID) is
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
               via   : Interface_Access;
            begin
               --  No usable route (no interface Up reaches the peer): the
               --  channel ends Unreachable (docs/net-link.md section 5).
               --  Unresolved, the datagram is lost, as UDP allows; the
               --  request sent now resolves the next one.
               neighbourMAC (C.remoteIP, via, mac, found);
               if via = No_Interface then
                  setStatus (chIdx, Layout.Status_Unreachable);
                  exit;
               elsif found then
                  sendUDP (via, C.remoteIP, mac, C.localPort, C.remotePort, buffer'Address, len);
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
            if Channel_Service.Close_Due
              (Already_Closed  => C.shutDone,
               Send_Unconsumed => C.tx.Available,
               Shutdown_Asked  => headerWord (chIdx, Layout.Shut_Write_At) /= 0,
               In_Handshake    => tcpState (C.connIdx) in
                                    TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received)
            then
               C.shutDone := True;
               tcpClose (C.connIdx);
            end if;
         end if;
         setStatus (chIdx, streamStatus (chIdx));

         --  The kicks to ask for (the proved Channel_Service rule).
         flags := Channel_Service.Kicks
           (Failed          => failedStatus (C.status),
            Send_Unconsumed => C.tx.Available,
            Readable        => (if C.connIdx in tcpConns'Range then readable (C.connIdx) else 0),
            Receive_Space   => Rings.Space (C.rx),
            Has_Connection  => C.connIdx in tcpConns'Range);
         if flags /= C.kickFlags then
            C.kickFlags := flags;
            setHeaderWord (chIdx, Layout.Kick_Wanted_At, flags);
         end if;
         exit when flags = 0;
         fullFence;
         exit when not Channel_Service.Look_Again
           (Flags            => flags,
            Tx_Produced_Seen => Unsigned_32 (Rings.Produced (C.tx)),
            Tx_Produced_Now  => headerWord (chIdx, Layout.Tx_Produced_At),
            Rx_Consumed_Seen => Unsigned_32 (Rings.Consumed (C.rx)),
            Rx_Consumed_Now  => headerWord (chIdx, Layout.Rx_Consumed_At));
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
   procedure endWait (owner : Process_ID) is
   begin
      for P of pendingReqs loop
         if P.kind = PENDING_WAIT and then P.sender = owner then
            replyOKWord (P.sender, readyMask (owner, P.waitMask), P.replySlot);
            P.kind := PENDING_NONE;
         end if;
      end loop;
   end endWait;

   --  A kick is a wakeup, not a fact: a client's kick can be refused by a
   --  full mailbox, and then nothing else names its channel. Before
   --  sleeping, take up every channel whose client moved an index since its
   --  kick was asked for (the proved Look_Again rule), or whose requested
   --  shutdown is due (Close_Due; one not yet due waits for the peer's
   --  segments, which service it). docs/ipc-delivery.md, "Audit of refused
   --  sends". True when it serviced any; each rule is false once serviced,
   --  so this never keeps the netstack from sleeping.
   function serviceArmedChannels return Boolean is
      Serviced : Boolean := False;
   begin
      for I in channels'Range loop
         declare
            C : NetChannel renames channels (I);
         begin
            if C.stream and then
              (Channel_Service.Look_Again
                 (Flags            => C.kickFlags,
                  Tx_Produced_Seen => Unsigned_32 (Rings.Produced (C.tx)),
                  Tx_Produced_Now  => headerWord (I, Layout.Tx_Produced_At),
                  Rx_Consumed_Seen => Unsigned_32 (Rings.Consumed (C.rx)),
                  Rx_Consumed_Now  => headerWord (I, Layout.Rx_Consumed_At))
               or else
               (C.connIdx in tcpConns'Range and then
                Channel_Service.Close_Due
                  (Already_Closed  => C.shutDone,
                   Send_Unconsumed => C.tx.Available,
                   Shutdown_Asked  => headerWord (I, Layout.Shut_Write_At) /= 0,
                   In_Handshake    => tcpState (C.connIdx) in
                                        TCP_Connection.Syn_Sent | TCP_Connection.Syn_Received)))
            then
               serviceChannel (I);
               Serviced := True;
            end if;
         end;
      end loop;
      return Serviced;
   end serviceArmedChannels;

   --  A client kicked (or waited with a kick mask): service its channels
   --  whose wait bits are in mask.
   procedure kickChannels (owner : Process_ID; mask : Unsigned_64) is
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
   procedure handleNetWait (snd : Process_ID; m : Message) is
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
      if answersWaiting (snd) then
         replyWait ((replySlot => Current_Request, others => <>), mask, True);
      elsif mask /= 0 or else m.words (1) <= clockNow then
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
                     --  No answer for an RTO: the next hop may have changed.
                     declare
                        via : Interface_Access;
                        hop : Net.IPv4Address;
                     begin
                        nextHopOf (tcpConns (Index).remoteIP, via, hop);
                        doubtNeighbour (via, hop);
                     end;
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

   procedure refuseSegment (rxIf   : Interface_Access;
                            seg    : TCP_Slots.SegmentInfo;
                            dstIP  : Net.IPv4Address;
                            srcMAC : Net.MACAddress;
                            Now    : Unsigned_64) is
      reply : constant TCP_Reset.Reply :=
        TCP_Reset.For_Closed
          ((SYN => seg.flagSYN, ACK => seg.flagACK, FIN => seg.flagFIN, RST => seg.flagRST,
            Seq_No => Seq (seg.seqNum), Ack_No => Seq (seg.ackNum), Length => seg.dataLen));
   begin
      if not reply.Send or else not rxIf.configured or else dstIP /= rxIf.ipv4 then
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
                       remotePort => seg.srcPort, remoteIP => seg.srcIP, remoteMAC => srcMAC,
                       localIP => dstIP, routed => True, egress => rxIf.handle),
                      (if reply.With_ACK then TCP_Wire.Flag_RST or TCP_Wire.Flag_ACK
                       else TCP_Wire.Flag_RST),
                      Unsigned_32 (reply.Seq_No), Unsigned_32 (reply.Ack_No), 0,
                      System.Null_Address, 0);
   end refuseSegment;

   ---------------------------------------------------------------------------
   --  handleTCP - parse a TCP segment (TCP_Header), drive the state machine
   ---------------------------------------------------------------------------
   --  A TCP peer sent a segment since netstack last went idle: more is
   --  likely on its way, a stream's next data or a server's next client
   --  (arrivalExpected). Control requests and timers do not set it.
   peerActive : Boolean := False;

   procedure handleTCP (rxIf       : Interface_Access;
                        pktBuf     : System.Address;
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
      --  Only to the receiving interface's own address: a broadcast, or
      --  another interface's address, must not match a unicast TCP tuple.
      if tcpLen < 20 or else not rxIf.configured or else dstIP /= rxIf.ipv4 then
         return;
      end if;

      --  TCP_Header validates the layout, not the pseudo-header checksum.
      --  Validate before a packet can acknowledge data or change TCP state
      --  (the proved TCP_Frame.Checksum_OK, over netstack's own copy).
      if tcpLen > TCP_MAXIMUM_SEGMENT then
         return;
      end if;
      declare
         segment : constant IPv4_Header.Bytes (0 .. tcpLen - 1)
           with Import, Address => pktBuf + Storage_Offset (tcpOff);
      begin
         if not TCP_Frame.Checksum_OK (v4 (srcIP), v4 (dstIP), segment) then
            return;
         end if;
      end;
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
         seg.dstIP   := dstIP;
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
      --  Where the segment goes: the proved TCP_Dispatch table.
      declare
         use TCP_Dispatch;
         listener : constant TCP_Listeners.Handle :=
           (if connIdx < 0 then TCP_Listeners.Find (listeners, policyAddress (dstIP), seg.dstPort)
            else TCP_Listeners.No_Handle);
         action : constant TCP_Dispatch.Action := Decide
           ((Has_Connection  => connIdx >= 0,
             Time_Wait_Taken => connIdx < 0 and then timeWaitSegment (seg, Now),
             Usable_Source   => seg.srcPort /= 0 and then IPv4_Frame.Unicast (v4 (srcIP)),
             SYN => seg.flagSYN, ACK => seg.flagACK, RST => seg.flagRST, FIN => seg.flagFIN,
             Listening       => listener /= TCP_Listeners.No_Handle));
         reserved : Boolean;
      begin
         case action is
            when To_Connection =>
               null;
            when Taken_By_Time_Wait | Drop =>
               return;
            when Refuse =>
               refuseSegment (rxIf, seg, dstIP, srcMAC, Now);
               return;
            when Open_Passive =>
               claimSlot (dstIP, srcIP, seg.dstPort, seg.srcPort, srcMAC, rxIf.handle, connIdx);
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
         end case;
      end;

      tcpArrive (connIdx, seg, pktBuf, Now);
      peerActive := True;
      --  Finish buffering all final-ACK payload/FIN actions before handing the
      --  connection to its owner.
      deliverAllArrivals;
   end handleTCP;

   --  hop's link address is now known: connections that opened before it
   --  was take it, and resolver questions to a server behind it go again
   --  now instead of at their next retry.
   procedure neighbourResolved (ifc : Interface_Access; hop : Net.IPv4Address;
                                mac : Net.MACAddress) is
      via : Interface_Access;
      next : Net.IPv4Address;
   begin
      --  Every connection through hop on ifc takes its (possibly new) address.
      for I in tcpConns'Range loop
         if tcpConns (I).inUse and then tcpConns (I).remoteMAC /= mac and then
           tcpConns (I).routed and then tcpConns (I).egress = ifc.handle
         then
            nextHopOf (tcpConns (I).remoteIP, via, next);
            if via = ifc and then next = hop then
               tcpConns (I).remoteMAC := mac;
            end if;
         end if;
      end loop;
      for P of pendingReqs loop
         if P.kind in PENDING_RESOLVE | PENDING_OPEN and then P.attempts in 1 .. DNS_ATTEMPTS - 1
         then
            nextHopOf (dnsServer (P.attempts - 1), via, next);
            if via = ifc and then next = hop then
               sendAttempt (P, clockNow);
            end if;
         end if;
      end loop;
   end neighbourResolved;

   ---------------------------------------------------------------------------
   --  handleARP: an ARP packet that arrived on ifc
   ---------------------------------------------------------------------------
   procedure handleARP (ifc : Interface_Access; pktBuf : System.Address; pktLen : Natural) is
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
      --  For this interface's own address only.
      ours := ifc.configured and then targetIP = ifc.ipv4;
      --  Learn only what the proved cache accepts (no unsolicited replies,
      --  no rewriting a resolved address, no bogus senders).
      ARP_Cache.Learn (ifc.arpCache, p, ours, syscall (SYSCALL_GETTIME));

      if p.Op = ARP_Packet.Request and then ours and then
        ARP_Cache.Usable_Sender (p.Sender_IP, p.Sender_HW)
      then
         sendARPReply (ifc, netMAC (p.Sender_HW), senderIP);
      end if;
      --  What the cache now holds for the sender (only what it accepted).
      if arpLookup (ifc, senderIP, mac) then
         if ifc.gateway = senderIP then
            ifc.gwMAC := mac;
         end if;
         neighbourResolved (ifc, senderIP, mac);
      end if;
   end handleARP;

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
      ours : Net.IPv4Address;
      peer : Net.IPv4Address;
   begin
      Parse (message, e, ok);
      if not ok then
         return;
      end if;
      --  It must quote one of our addresses as the source.
      ours := [e.Source (0), e.Source (1), e.Source (2), e.Source (3)];
      if not isOurAddress (ours) then
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
   --  ICMP for IPv4 (ipv4_icmp.ads, proved through
   --  tests/tcp-session/ipv4_icmp_proof.ads): echo, errors, our pings.
   ---------------------------------------------------------------------------

   --  The self-test (tests/headless network-authority): once configured,
   --  ping the gateway once and log its reply.
   SELF_TEST_SEQUENCE : constant Unsigned_16 := 16#FFFF#;
   SELF_TEST_POLL_MS  : constant Unsigned_64 := 100;

   --  A reply to our ping Sequence: complete the request waiting for it.
   procedure pingAnswered (From : IPv4_Header.Address; Sequence : Unsigned_16) is
      source : constant Net.IPv4Address := [From (0), From (1), From (2), From (3)];
   begin
      if Sequence = SELF_TEST_SEQUENCE then
         for S in 0 .. Ifaces.Slot_Count - 1 loop
            if Ifaces.In_Use (S) and then Ifaces.Get (S).ipv4Test = TEST_SENT and then
              Ifaces.Get (S).gateway = source
            then
               Ifaces.Get (S).ipv4Test := TEST_DONE;
               debugPrint ("netstack: IPv4 echo reply from ");
               printIP (source);
               debugPrint ("" & LF);
               return;
            end if;
         end loop;
      end if;
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_PING and then pendingReqs (i).txid = Sequence then
            declare
               sentAt : constant Unsigned_64 :=
                 Unsigned_64 (To_Integer (pendingReqs (i).bufAddr));
               replyMsg : constant Message :=
                 (tag          => (label => REPLY_OK, length => 3, protocol => 0, ipc => <>, reserved => 0),
                  authorityTag => 0,
                  words        => [0 => Unsigned_64 (Sequence),
                                   1 => Net.packIPv4 (source),
                                   2 => (if clockNow > sentAt then clockNow - sentAt else 0),
                                   3 => 0]);
               ignore : Unsigned_64;
            begin
               ignore := Send_Answer (pendingReqs (i).replySlot, replyMsg);
            end;
            pendingReqs (i).kind := PENDING_NONE;
            return;
         end if;
      end loop;
   end pingAnswered;

   procedure icmpError (Message : IPv4_Header.Bytes) is
   begin
      handleICMPError (Message, clockNow);
   end icmpError;

   package ICMPv4 is new IPv4_ICMP
     (Send => sendIPv4, Echo_Answered => pingAnswered, Error_Arrived => icmpError);

   --  The self-test's next step on ifc: ping its gateway once its address
   --  is known, up to SELF_TEST_TRIES times a second apart.
   procedure ipv4SelfTest (ifc : Interface_Access; Now : Unsigned_64) is
      gw  : constant Net.IPv4Address := ifc.gateway;
      mac : Net.MACAddress;
   begin
      if ifc.ipv4Test not in TEST_WAITING | TEST_SENT or else
        ifc.ipv4TestTries = SELF_TEST_TRIES or else Now < ifc.ipv4TestAt or else
        not ifc.usable
      then
         return;
      end if;
      if not arpLookup (ifc, gw, mac) then
         ifc.ipv4TestAt := deadlineAfter (Now, SELF_TEST_POLL_MS);   --  not resolved yet
         return;
      end if;
      currentIf := ifc;
      ICMPv4.Echo (ifc.mac, mac, v4 (ifc.ipv4), v4 (gw), SELF_TEST_SEQUENCE);
      ifc.ipv4Test := TEST_SENT;
      ifc.ipv4TestTries := ifc.ipv4TestTries + 1;
      ifc.ipv4TestAt := deadlineAfter (Now, ARP_RETRY_MS);
   end ipv4SelfTest;

   ---------------------------------------------------------------------------
   --  DHCP: a datagram to the client port on ifc goes to netmgr's page for
   --  ifc (CuBit.Net_Management, DHCP_Await), whatever its destination
   --  address (the server may answer by broadcast or to the address it is
   --  offering). Its UDP header and checksum are checked here; netmgr
   --  parses the message.
   ---------------------------------------------------------------------------
   procedure handleDHCP (ifc        : Interface_Access;
                         pktBuf     : System.Address;
                         ipOff      : Natural;
                         ipHdrLen   : Natural;
                         srcIP      : Net.IPv4Address;
                         dstIP      : Net.IPv4Address;
                         totalIPLen : Natural)
   is
      udpOff : constant Natural := ipOff + ipHdrLen;
      udpLen : constant Natural := totalIPLen - ipHdrLen;
      datagram : UDP_Header.Bytes (0 .. udpLen - 1)
         with Import, Address => pktBuf + Storage_Offset (udpOff);
      h : UDP_Header.Header;
      len : Natural;
   begin
      if ifc.dhcpBase = System.Null_Address or else udpLen < UDP_Header.Size or else
        not UDP_Header.Well_Formed (datagram)
      then
         return;
      end if;
      UDP_Header.Parse (datagram, h);
      if h.Source_Port /= NM.DHCP_Server_Port or else h.Length > UDP_Frame.Maximum_Datagram or else
        not UDP_Frame.Checksum_OK (v4 (srcIP), v4 (dstIP), IPv4_Header.Bytes (datagram (0 .. h.Length - 1)))
      then
         return;
      end if;
      len := h.Length - UDP_Header.Size;
      if len not in NM.Message_Length then
         return;
      end if;
      declare
         src : constant IPv4_Header.Bytes (0 .. len - 1)
           with Import, Address => pktBuf + Storage_Offset (udpOff + UDP_Header.Size);
         dst : IPv4_Header.Bytes (0 .. len - 1)
           with Import, Address => ifc.dhcpBase + Storage_Offset (NM.Incoming_At);
      begin
         --  The newest message replaces one netmgr has not taken yet.
         dst := src;
      end;
      if ifc.dhcpAwait.present then
         ifc.dhcpHeld := 0;
         answerParked (ifc.dhcpAwait, Unsigned_64 (len));
      else
         ifc.dhcpHeld := len;
      end if;
   end handleDHCP;

   ---------------------------------------------------------------------------
   --  handleIPv4: an IPv4 packet that arrived on ifc
   ---------------------------------------------------------------------------
   procedure handleIPv4 (ifc : Interface_Access; pktBuf : System.Address; pktLen : Natural) is
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

      --  DHCP replies arrive before the interface has an address.
      if proto = Net.PROTO_UDP and then ifc.dhcpBase /= System.Null_Address and then
        totalLen >= ipHdrLen + UDP_Header.Size and then
        Net.getU16BE (pktBuf, ipOff + ipHdrLen + 2) = NM.DHCP_Client_Port
      then
         handleDHCP (ifc, pktBuf, ipOff, ipHdrLen, srcIP, dstIP, totalLen);
         return;
      end if;

      --  Only for this interface's address (or broadcast, which nothing
      --  here listens to yet).
      if not ifc.configured or else dstIP /= ifc.ipv4 then
         return;
      end if;

      if proto = Net.PROTO_ICMP then
         declare
            packet : constant IPv4_Header.Bytes (0 .. pktLen - ipOff - 1)
              with Import, Address => pktBuf + Storage_Offset (ipOff);
         begin
            currentIf := ifc;   --  echo replies go back the way they came
            ICMPv4.Receive (packet, ifc.mac, srcMAC, clockNow);
         end;
      elsif proto = Net.PROTO_UDP then
         handleUDP (pktBuf, ipOff, ipHdrLen, srcIP, dstIP, totalLen);
      elsif proto = Net.PROTO_TCP then
         handleTCP (ifc, pktBuf, ipOff, ipHdrLen, srcIP, dstIP, srcMAC, totalLen);
      end if;
   end handleIPv4;

   ---------------------------------------------------------------------------
   --  handlePacket - dispatch a frame that arrived on ifc on its EtherType
   ---------------------------------------------------------------------------
   procedure handlePacket (ifc : Interface_Access; pktBuf : System.Address; pktLen : Natural) is
      etherType : Unsigned_16;
   begin
      if pktLen < 14 then
         return;
      end if;

      etherType := Net.getU16BE (pktBuf, 12);

      if etherType = Net.ETHERTYPE_ARP then
         handleARP (ifc, pktBuf, pktLen);
      elsif etherType = Net.ETHERTYPE_IPV4 then
         handleIPv4 (ifc, pktBuf, pktLen);
      elsif etherType = Net.ETHERTYPE_IPV6 then
         declare
            --  The driver's frame, in place: the one unproved step.
            frame : constant IPv6_Header.Bytes (0 .. pktLen - 1)
              with Import, Address => pktBuf;
         begin
            IPv6.Receive (ifc.v6, frame, clockNow);
         end;
      end if;
   end handlePacket;

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
   begin
      for i in pendingReqs'Range loop
         if pendingReqs (i).kind = PENDING_NONE and then curRoute.queue /= No_Queue then
            --  A queue entry: its answer goes to the queue, not a capability.
            pendingReqs (i) := req;
            pendingReqs (i).route := curRoute;
            return True;
         elsif pendingReqs (i).kind = PENDING_NONE then
            --  Deferral is valid only if the kernel moved the current
            --  one-use reply authority into a reply handle.
            declare
               Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
            begin
               if not Saved.Saved then
                  return False;
               end if;
               pendingReqs (i) := req;
               pendingReqs (i).replySlot := (Saved => True, Handle => Saved.Handle);
            end;
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
     (to   : Process_ID;
      slot : Reply_Target := Current_Request)
   is
      errMsg : constant Message :=
        (tag      => (label  => REPLY_ERR,
                      length => 0,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
         authorityTag => 0,
         words    => [others => 0]);
      ignore : Unsigned_64;
   begin
      pragma Unreferenced (to);
      if not slot.Saved and then curRoute.queue /= No_Queue then
         answerQueue (curRoute, False, 0);
         return;
      end if;
      ignore := Send_Answer (slot, errMsg);
   end replyError;

   ---------------------------------------------------------------------------
   --  replyOK - send REPLY_OK with word0 to a sender
   ---------------------------------------------------------------------------
   procedure replyOKWord
     (to   : Process_ID;
      w0   : Unsigned_64;
      slot : Reply_Target := Current_Request)
   is
      okMsg : constant Message :=
        (tag      => (label  => REPLY_OK,
                      length => 1,
                      protocol  => 0, ipc => <>,
                      reserved  => 0),
         authorityTag => 0,
         words    => [0 => w0, others => 0]);
      ignore : Unsigned_64;
   begin
      pragma Unreferenced (to);
      if not slot.Saved and then curRoute.queue /= No_Queue then
         answerQueue (curRoute, True, w0);
         return;
      end if;
      ignore := Send_Answer (slot, okMsg);
   end replyOKWord;

   --  Deferred answers: to the request's queue, or its saved capability.
   procedure answerError (P : PendingRequest) is
   begin
      if P.route.queue /= No_Queue then
         answerQueue (P.route, False, 0);
      else
         replyError (P.sender, P.replySlot);
      end if;
   end answerError;

   procedure answerOK (P : PendingRequest; w0 : Unsigned_64) is
   begin
      if P.route.queue /= No_Queue then
         answerQueue (P.route, True, w0);
      else
         replyOKWord (P.sender, w0, P.replySlot);
      end if;
   end answerOK;

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
      snd    : Process_ID;
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
      --  Only on an address one of our interfaces is configured with.
      if not ipOK or else conflict or else not isOurAddress (address) or else
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
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            ARP_Cache.Expire (Ifaces.Get (S).arpCache, Now, ARP_PROBE_MS);
            ipv4SelfTest (Ifaces.Get (S), Now);
         end if;
      end loop;
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
        Unsigned_64'Min (TCP_Listeners.Next_Deadline (listeners), tcpNextDeadline);
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            declare
               ifc : constant Interface_Access := Ifaces.Get (S);
            begin
               if ifc.ipv6Started then
                  deadline := Unsigned_64'Min (deadline, IPv6.Next_Deadline (ifc.v6));
               end if;
               if ifc.ipv4Test in TEST_WAITING | TEST_SENT and then
                 ifc.ipv4TestTries < SELF_TEST_TRIES and then ifc.usable
               then
                  deadline := Unsigned_64'Min (deadline, ifc.ipv4TestAt);
               end if;
            end;
         end if;
      end loop;
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
                         snd     : Process_ID;
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
          replySlot  => Current_Request,
          queryPort  => port,
          nameHash   => nameHashOf (name (0 .. nameLen - 1)),
          queryDeadline => deadlineAfter (now, DNS_TIMEOUT_MS),
          question    => name,
          questionLen => nameLen,
          attempts    => 0,
          nextSend    => Unsigned_64'Last,
          waitDeadline  => Unsigned_64'Last,
          waitMask      => 0,
          route         => <>));   --  addPending sets it
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
   procedure handleAppResolve (snd : Process_ID; m : Message) is
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
   --           tag.protocol=channel kind (0=client),
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
      owner : Process_ID;
      dstIP : Net.IPv4Address;
      port  : Unsigned_16;
      slot  : Reply_Target)
   is
      ok : Boolean := False;
   begin
      if Network_Grants.Allows
        (networkGrants, owner, channels (chIdx).authorityTag,
         Network_Authority.Connect_UDP, policyAddress (dstIP), port)
      then
         udpOpens := udpOpens + 1;
         UDP_Channels.Open
           (udpChannels, chIdx, policyAddress (dstIP), port,
            Unsigned_16 (SipHash.Hash
              (portKey, [dstIP (0), dstIP (1), dstIP (2), dstIP (3),
                         Unsigned_8 (port / 256), Unsigned_8 (port mod 256),
                         Unsigned_8 (udpOpens and 16#FF#),
                         Unsigned_8 (Shift_Right (udpOpens, 8) and 16#FF#),
                         Unsigned_8 (Shift_Right (udpOpens, 16) and 16#FF#),
                         Unsigned_8 (Shift_Right (udpOpens, 24) and 16#FF#)]) and 16#FFFF#),
            ok);
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
   procedure releaseOwner (owner : Process_ID) is
      tags : Network_Grants.Tag_List;
      children : TCP_Listeners.Connection_List;
   begin
      for P of pendingReqs loop
         if P.kind /= PENDING_NONE and then P.sender = owner then
            --  A queued request's answer has nowhere to go: its queue is
            --  released below.
            if P.route.queue = No_Queue then
               replyError (P.sender, P.replySlot);
            end if;
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
         Channel_Arenas.Release_Owner (arenas, To_Word (owner), gone);
         for A in gone'Range loop
            if gone (A) then
               CuBit.Memory_Grants.Return_Acquisition (arenaGrant (A), returned);
               arenaBase (A) := System.Null_Address;
            end if;
         end loop;
      end;
      for q in queues'Range loop
         if queues (q).owner = owner then
            declare
               returned : Boolean;
            begin
               CuBit.Memory_Grants.Return_Acquisition (queues (q).grant, returned);
            end;
            queues (q) := (others => <>);
         end if;
      end loop;
      Network_Grants.Release_Owner (networkGrants, owner, tags);
      if (for some T of tags => T /= 0) then
         debugPrint ("netstack: released the scopes of exited process ");
         printProcess (owner);
         debugPrint ("" & LF);
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

   --  A request word as a count or index. netstack has no run-time checks,
   --  so a word beyond Natural'Last must not be converted as it is: it
   --  saturates, and then fails whatever range check follows.
   function wordNatural (w : Unsigned_64) return Natural is
     (if w > Unsigned_64 (Natural'Last) then Natural'Last else Natural (w));

   ---------------------------------------------------------------------------
   --  handleArena: a process lends a grant cut into channel buffers
   --  (Layout.OP_NET_ARENA). Admission checked the ring sizes and count.
   ---------------------------------------------------------------------------
   procedure handleArena (snd : Process_ID; m : Message) is
      --  Each ring size is 32 bits; their sum with the header is checked
      --  in 64 bits before it becomes a Natural.
      size64 : constant Unsigned_64 :=
        Unsigned_64 (Layout.Header_Bytes) + (m.words (2) and 16#FFFF_FFFF#) +
        Shift_Right (m.words (2), 32);
      size : constant Natural := wordNatural (size64);
      count : constant Natural := wordNatural (m.words (3));
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
        (arenas, To_Word (snd), size, count, handle, index, ok);
      if not ok then
         CuBit.Memory_Grants.Return_Acquisition (reference, returned);
         replyError (snd); return;
      end if;
      arenaGrant (index) := reference;
      arenaBase (index) := address;
      replyOKWord (snd, Unsigned_64 (handle));
   end handleArena;

   ---------------------------------------------------------------------------
   --  handleQueue: a process lends a control queue (Layout.OP_NET_QUEUE),
   --  one per endpoint it holds. The queue acts with this request's
   --  authority tag.
   ---------------------------------------------------------------------------
   procedure handleQueue (snd : Process_ID; m : Message) is
      reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => m.words (0), generation => m.words (1));
      address : System.Address;
      free : Queue_Count := No_Queue;
      ok : Boolean;
   begin
      for q in queues'Range loop
         if queues (q).owner = snd and then queues (q).tag = m.authorityTag then
            replyError (snd); return;
         end if;
         if free = No_Queue and then queues (q).owner = No_Process then
            free := q;
         end if;
      end loop;
      if free = No_Queue then replyError (snd); return; end if;
      CuBit.Memory_Grants.Acquire
        (reference, snd, 0, Layout.Queue_Bytes,
         CuBit.Memory_Grants.Write_Access, address, ok);
      if not ok then replyError (snd); return; end if;
      queues (free) := (owner  => snd,
                        tag    => m.authorityTag,
                        grant  => reference,
                        base   => address,
                        server => <>);
      debugPrint ("netstack: control queue for pid");
      printProcess (snd);
      debugPrint ("" & LF);
      replyOKWord (snd, 0);
   end handleQueue;

   procedure handleNetOpen (snd : Process_ID; m : Message) is
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
            openDatagram (chIdx, snd, dstIP, scheme.port, Current_Request);
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
            connIdx := tcpConnect (dstIP, scheme.port);
            if connIdx < 0 then
               debugPrint ("netstack: open: no route or no free TCP connection slot" & LF);
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
                replySlot  => Current_Request,
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
   procedure handleNetShut (snd : Process_ID; m : Message) is
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

   --  Take queue q's requests and handle each as its IPC twin would be
   --  (same admission, same handler), answering on the queue.
   procedure serviceQueue (q : Queue_Index) is
      produced : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, Layout.Queue_Submissions_At + Layout.Queue_Produced_At);
      consumed : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, Layout.Queue_Submissions_At + Layout.Queue_Consumed_At);
      wake : Unsigned_32 with Volatile, Import,
        Address => queueWord (q, Layout.Queue_Submissions_At + Layout.Queue_Wake_At);
      requests : Control.Submissions.Ring with Import,
        Address => queueWord (q, Layout.Queue_Requests_At);
      item  : Control.Submission;
      ok    : Boolean;
      took  : Boolean := False;
      owner : constant Process_ID := queues (q).owner;
      tag   : constant Unsigned_64 := queues (q).tag;
   begin
      --  Awake: the client need not kick until we arm the word again.
      if wake /= 0 then
         wake := 0;
      end if;
      acceptReaped (q);
      --  The client's count, read once; one that goes back or overfills
      --  the ring is ignored.
      Control.Submissions.Accept_Produced
        (queues (q).server.Requests, Control.Submissions.Index (produced), ok);
      if not ok then
         return;
      end if;
      --  The requests were written before the count was: read them after.
      System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
      while Control.Can_Take (queues (q).server) loop
         --  A private copy of the entry: what is checked is what is used.
         Control.Take (queues (q).server, requests, item);
         took := True;
         curRoute := (queue => q, token => item.Tag);
         declare
            m : Message :=
              (tag => (label => 0, length => 0, protocol => 0, ipc => <>, reserved => 0),
               authorityTag => tag,
               words => [others => 0]);
         begin
            if item.Item.Operation = Layout.Queue_Open and then
              item.Item.Length in 1 .. Layout.Target_Maximum and then
              Network_Grants.Owned (networkGrants, owner, tag)
            then
               m.tag := (label => OP_NET_OPEN, length => Unsigned_8 (item.Item.Length),
                         protocol => 0, ipc => <>, reserved => 0);
               m.words (0) := item.Item.Object;
               m.words (1) := Unsigned_64 (item.Item.Buffer);
               handleNetOpen (owner, m);
            elsif item.Item.Operation = Layout.Queue_Shut and then
              Network_Grants.Owned (networkGrants, owner, tag)
            then
               m.tag := (label => OP_NET_SHUT, length => 1, protocol => 0, ipc => <>, reserved => 0);
               m.words (0) := item.Item.Object;
               handleNetShut (owner, m);
            else
               answerQueue (curRoute, False, 0);
            end if;
         end;
         curRoute := (others => <>);
      end loop;
      if took then
         --  The entries were copied out before their slots are handed back.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         consumed := Unsigned_32 (queues (q).server.Requests.Consumed);
      end if;
   end serviceQueue;

   procedure serviceQueues is
   begin
      for q in queues'Range loop
         if queues (q).owner /= No_Process then
            serviceQueue (q);
         end if;
      end loop;
   end serviceQueues;

   --  About to sleep: arm each queue's wake word so its client kicks, then
   --  look once more (it may have submitted before it could see the
   --  word). True if requests wait: do not sleep.
   function armQueues return Boolean is
      found : Boolean := False;
   begin
      queueEpoch := (if queueEpoch = Unsigned_32'Last then 1 else queueEpoch + 1);
      for q in queues'Range loop
         if queues (q).owner /= No_Process then
            declare
               produced : Unsigned_32 with Volatile, Import,
                 Address => queueWord (q, Layout.Queue_Submissions_At + Layout.Queue_Produced_At);
               wake : Unsigned_32 with Volatile, Import,
                 Address => queueWord (q, Layout.Queue_Submissions_At + Layout.Queue_Wake_At);
            begin
               wake := queueEpoch;
               System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
               if Control.Submissions.Index (produced) /=
                 queues (q).server.Requests.Consumed
               then
                  found := True;
               end if;
            end;
         end if;
      end loop;
      return found;
   end armQueues;

   function hardwareRandom return Unsigned_64;

   ---------------------------------------------------------------------------
   --  Interfaces: logging, events, configuration
   ---------------------------------------------------------------------------

   --  The interface changed (attached, its link, its configuration): the
   --  next event sequence is its, and netmgr's parked Watch is answered.
   watchWait : Parked;
   watchSeen : Unsigned_32 := 0;
   procedure answerWatch;

   procedure interfaceChanged (ifc : Interface_Access) is
   begin
      eventSequence := eventSequence + 1;
      ifc.lastChange := eventSequence;
      answerWatch;
   end interfaceChanged;

   --  The interface's address changed: connections from the old one end,
   --  and connected datagram channels whose route leaves by it are told
   --  Unreachable (their peers knew the old address).
   procedure addressWithdrawn (ifc : Interface_Access; old : Net.IPv4Address) is
   begin
      for I in tcpConns'Range loop
         if tcpConns (I).inUse and then tcpConns (I).localIP = old then
            abortConnection (I, clockNow, Layout.Status_Unreachable);
         end if;
      end loop;
      for C in channels'Range loop
         if channels (C).kind /= CHANNEL_NONE and then channels (C).proto = Net.PROTO_UDP and then
           egressFor (channels (C).remoteIP) = ifc
         then
            setStatus (C, Layout.Status_Unreachable);
         end if;
      end loop;
   end addressWithdrawn;

   --  What the link knows about its neighbours may be stale (another
   --  network, another router, a new address): forget it, in the ARP cache
   --  and in the connections through ifc (they resolve again).
   procedure forgetNeighbours (ifc : Interface_Access) is
   begin
      ifc.arpCache := [others => (others => <>)];
      ifc.arpAskedAt := [others => 0];
      ifc.gwMAC := Net.ZERO_MAC;
      for I in tcpConns'Range loop
         if tcpConns (I).inUse and then tcpConns (I).routed and then
           tcpConns (I).egress = ifc.handle
         then
            tcpConns (I).remoteMAC := Net.ZERO_MAC;
         end if;
      end loop;
   end forgetNeighbours;

   --  Tell the link about ourselves: our IPv4 address to everyone (RFC 5227
   --  2.3) and a question for the gateway (reconfirming the router, as
   --  RFC 4436 does); IPv6 announces each address once detection passes.
   procedure announce (ifc : Interface_Access) is
   begin
      if ifc.configured and then ifc.usable then
         sendGratuitousARP (ifc);
         if ifc.gateway /= NO_ADDRESS then
            sendARPRequest (ifc, ifc.gateway);
         end if;
      end if;
   end announce;

   --  The deferred queue's frames for ifc (sent when its link was Up) go:
   --  they would reach another network late, or no one.
   procedure dropDeferredFor (ifc : Interface_Access) is
   begin
      for K in 0 .. deferredCount - 1 loop
         declare
            slot : constant Natural := (deferredHead + K) mod MAX_DEFERRED_TX;
         begin
            if deferredTX (slot).via = ifc.handle then
               deferredTX (slot).via := (Slot => NL.Interface_Slot'Last,
                                         Epoch => NL.Interface_Epoch'Last);
               --  flushOneDeferredTX drops a frame whose interface is gone.
            end if;
         end;
      end loop;
   end dropDeferredFor;

   procedure logCondition (S : NL.Link_State) is
   begin
      case S.Condition is
         when NL.Down    => logPut ("Down");
         when NL.Dormant => logPut ("Dormant");
         when NL.Up      => logPut ("Up");
      end case;
   end logCondition;

   --  One line per link transition (docs/net-link.md section 5):
   --  netstack: <name> link <new> reason=<why> was=<old> kind=<kind>
   --    mac=<address> gen=<generation> [speed=<Mb/s>] [bssid=<address>]
   procedure logLink (ifc : Interface_Access; old : NL.Link_State;
                      change : Link_Tracker.Transition) is
      S : constant NL.Link_State := ifc.link.State;
   begin
      logPut ("netstack: ");
      logPut (ifc.name.Text (1 .. ifc.name.Length));
      logPut (" link ");
      logCondition (S);
      logPut (" reason=");
      case S.Condition is
         when NL.Down =>
            case S.Reason is
               when NL.Not_Reported          => logPut ("Not_Reported");
               when NL.No_Carrier            => logPut ("No_Carrier");
               when NL.Administratively_Down => logPut ("Administratively_Down");
               when NL.Not_Associated        => logPut ("Not_Associated");
               when NL.Authentication_Failed => logPut ("Authentication_Failed");
               when NL.Hardware_Fault        => logPut ("Hardware_Fault");
            end case;
         when NL.Dormant | NL.Up =>
            case change is
               when Link_Tracker.Bounced         => logPut ("Bounced");
               when Link_Tracker.Moved           => logPut ("Moved");
               when Link_Tracker.Went_Up         => logPut ("Went_Up");
               when Link_Tracker.Went_Down       => logPut ("Went_Down");
               when Link_Tracker.Inactive_Change => logPut ("Changed");
               when Link_Tracker.Unchanged       => logPut ("Unchanged");
            end case;
      end case;
      logPut (" was=");
      logCondition (old);
      logPut (if ifc.kind = NL.Ethernet then " kind=Ethernet" else " kind=Wireless");
      logPut (" mac=");
      logMAC (ifc.mac);
      logPut (" gen=");
      logDec (Unsigned_64 (ifc.link.Seen));
      if S.Condition = NL.Up then
         logPut (" speed=");
         logDec (Unsigned_64 (S.Detail.Speed));
         if S.Detail.Kind = NL.Wireless then
            logPut (" bssid=");
            logMAC (netAddress (S.Detail.BSSID));
         end if;
      end if;
      logSend (if S.Condition = NL.Up then Information
               else Warning);
   end logLink;

   ---------------------------------------------------------------------------
   --  The link block: once per pass over the receive ring and before
   --  netstack sleeps, never per frame (docs/net-link.md section 3).
   ---------------------------------------------------------------------------
   procedure observeLink (ifc : Interface_Access) is
      function word (W : Frames.Link_Word_Index) return System.Address is
        (ifc.pktBuf + Storage_Offset (Frames.Link_Word_At (W)));
      generation : Unsigned_32 with Volatile, Import,
        Address => word (Frames.Link_Generation_Word);
      before, after : NL.Generation;
      words : NL.Report_Words;
      step  : Link_Tracker.Step;
      old   : constant NL.Link_State := ifc.link.State;
   begin
      before := generation;
      if before = ifc.link.Seen then
         return;   --  nothing new: the common case, one load
      end if;
      for Attempt in 1 .. NL.Read_Attempts loop
         before := generation;
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         for W in NL.Report_Index loop
            declare
               value : Unsigned_32 with Volatile, Import, Address => word (W);
            begin
               words (W) := value;
            end;
         end loop;
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         after := generation;
         Link_Tracker.Observe (ifc.link, before, words, after, step);
         exit when step.Found /= Link_Tracker.Unstable;
      end loop;
      if step.Found in Link_Tracker.Taken | Link_Tracker.Refused then
         if step.Found = Link_Tracker.Refused then
            logPut ("netstack: ");
            logPut (ifc.name.Text (1 .. ifc.name.Length));
            logPut (" link report malformed: treated as down gen=");
            logDec (Unsigned_64 (before));
            logSend (Warning);
         end if;
         ifc.usable := Link_Tracker.Usable (ifc.link);
         if Link_Tracker.Stops (step.Change) then
            --  Stop transmitting; routes through ifc are skipped while it
            --  is not usable; TCP connections stay (a flap must not reset
            --  them).
            dropDeferredFor (ifc);
            forgetNeighbours (ifc);
            if ifc.ipv6Started then
               IPv6.Link_Down (ifc.v6);
            end if;
         end if;
         if Link_Tracker.Starts (step.Change) then
            ifc.upCount := (if ifc.upCount = NM.Up_Count'Last then 0 else ifc.upCount + 1);
            if ifc.ipv6Started then
               IPv6.Link_Up (ifc.v6, clockNow);
            else
               IPv6.Start (ifc.v6, ifc.handle, ifc.mac,
                           (K0 => hardwareRandom, K1 => hardwareRandom),
                           clockNow, Self_Test => True);
               ifc.ipv6Started := True;
            end if;
            announce (ifc);
            ifc.txDoorbell := True;
         end if;
         if step.Change /= Link_Tracker.Unchanged then
            logLink (ifc, old, step.Change);
            interfaceChanged (ifc);
         end if;
      end if;
   end observeLink;

   ---------------------------------------------------------------------------
   --  Attach (CuBit.Net_Link): a new interface, its frame grant in owned
   --  memory, its handle in the answer.
   ---------------------------------------------------------------------------
   procedure refuseAttach (why : NL.Attach_Refusal) is
      msg : constant Message :=
        (tag => (label => REPLY_ERR, length => 1, protocol => 0, ipc => <>, reserved => 0),
         authorityTag => 0,
         words => [0 => Unsigned_64 (NL.Attach_Refusal'Enum_Rep (why)), others => 0]);
      ignore : Unsigned_64;
   begin
      ignore := Send_Answer (Current_Request, msg);
   end refuseAttach;

   --  Owned memory: a grant's pages are released only once the kernel
   --  confirms the grant retired (no mapping left), so a driver that is
   --  slow to let go never sees them reused.
   type Retiring_Region is record
      base  : Unsigned_64 := 0;
      bytes : Unsigned_64 := 0;
      grant : CuBit.Memory_Grants.Grant_Reference;
   end record;
   package Retiring is new Interface_Table (Retiring_Region);

   procedure releaseOwned (base, bytes : Unsigned_64) is
      ignore : Unsigned_64;
   begin
      ignore := CuBit.Kernel_Calls.Call (CuBit.Kernel_ABI.Release_Owned_Memory, base, bytes);
   end releaseOwned;

   procedure releaseRetired is
   begin
      for S in 0 .. Retiring.Slot_Count - 1 loop
         if Retiring.In_Use (S) and then
           CuBit.Memory_Grants.Retirement_Confirmed (Retiring.Get (S).grant)
         then
            releaseOwned (Retiring.Get (S).base, Retiring.Get (S).bytes);
            Retiring.Remove (S);
         end if;
      end loop;
   end releaseRetired;

   procedure retire (base, bytes : Unsigned_64; grant : CuBit.Memory_Grants.Grant_Reference) is
      ok : Boolean;
      ignoreHandle : NL.Interface_Handle;
   begin
      CuBit.Memory_Grants.Revoke (grant, ok);
      if CuBit.Memory_Grants.Retirement_Confirmed (grant) then
         releaseOwned (base, bytes);
         return;
      end if;
      Retiring.Insert ((base => base, bytes => bytes, grant => grant), ignoreHandle, ok);
      --  Not remembered (no memory): the pages stay ours, never reused.
   end retire;

   procedure handleAttach (sender : Process_ID; m : Message) is
      decoded : constant NL.Decoded_Attach :=
        NL.Decode ([m.words (0), m.words (1), m.words (2)]);
      base    : Unsigned_64;
      handle  : NL.Interface_Handle;
      ok      : Boolean;
      ifc     : Interface_Access;
   begin
      if not decoded.Valid then
         refuseAttach (NL.Malformed);
         return;
      end if;
      base := CuBit.Kernel_Calls.Call
        (CuBit.Kernel_ABI.Allocate_Owned_Memory, Unsigned_64 (PACKET_BUF_SIZE));
      if base = CuBit.Kernel_ABI.Failed then
         refuseAttach (NL.Out_Of_Memory);
         return;
      end if;
      declare
         R : InterfaceRecord;
      begin
         R.kind := decoded.Request.Kind;
         R.mtu := decoded.Request.Size;
         R.offload := decoded.Request.Offload;
         R.identity := decoded.Request.Identity;
         R.mac := netAddress (decoded.Request.Address);
         R.factory := R.mac;
         R.name := NL.Name_Of (R.identity, decoded.Request.Address);
         R.driverPID := sender;
         R.pktBuf := To_Address (Integer_Address (base));
         Ifaces.Insert (R, handle, ok);
      end;
      if not ok then
         releaseOwned (base, Unsigned_64 (PACKET_BUF_SIZE));
         refuseAttach (NL.Out_Of_Memory);
         return;
      end if;
      ifc := interfaceOf (handle);
      ifc.handle := handle;
      --  Owned memory is fresh (zero): the rings are empty and the link
      --  block says Never_Reported (Down) until the driver reports.
      CuBit.Memory_Grants.Create_For_Process
        (grantee   => sender,
         localAddr => ifc.pktBuf,
         numPages  => PACKET_BUF_PAGES,
         readWrite => True,
         reference => ifc.pktGrant,
         success   => ok,
         notify    => True);   --  its return (the driver let go or exited) detaches
      if not ok then
         releaseOwned (base, Unsigned_64 (PACKET_BUF_SIZE));
         declare
            S : Natural;
            F : Boolean;
         begin
            Ifaces.Find (handle, S, F);
            Ifaces.Remove (S);
         end;
         refuseAttach (NL.Grant_Refused);
         return;
      end if;
      declare
         replyMsg : constant Message :=
           (tag      => (label => REPLY_OK, length => 2, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0,
            words    => [0 => NL.Handle_Word (handle),
                         1 => CuBit.Grant_References.Encode (ifc.pktGrant),
                         others => 0]);
         ignore : Unsigned_64;
      begin
         ignore := Send_Answer (Current_Request, replyMsg);
      end;
      logPut ("netstack: ");
      logPut (ifc.name.Text (1 .. ifc.name.Length));
      logPut (if ifc.kind = NL.Ethernet then " attached kind=Ethernet" else " attached kind=Wireless");
      logPut (" mac=");
      logMAC (ifc.mac);
      logPut (" mtu=");
      logDec (Unsigned_64 (ifc.mtu));
      logPut (" handle=");
      logDec (Unsigned_64 (handle.Slot));
      logPut (".");
      logDec (Unsigned_64 (handle.Epoch));
      logSend;
      --  Publish our first receive wake epoch: the ring is empty.
      ifc.rxEpoch := 1;
      declare
         doorbell : Unsigned_32 with Volatile, Import,
           Address => ifc.pktBuf + Storage_Offset (Frames.Receive_Header_At + Frames.Wake_At);
      begin
         doorbell := ifc.rxEpoch;
      end;
      interfaceChanged (ifc);
   end handleAttach;

   --  The interface a driver's request names, if the driver owns it.
   function driverInterface (sender : Process_ID; word : Unsigned_64) return Interface_Access is
      ifc : Interface_Access;
   begin
      if not NL.Valid_Handle_Word (word) then
         return No_Interface;
      end if;
      ifc := interfaceOf (NL.Handle_Of (word));
      return (if ifc /= No_Interface and then ifc.driverPID = sender then ifc else No_Interface);
   end driverInterface;

   --  The interface is gone (its driver returned the grant or exited):
   --  connections through it end, its parked requests are answered, its
   --  memory goes back, and netmgr hears of it.
   procedure detachInterface (S : Natural) is
      ifc : constant Interface_Access := Ifaces.Get (S);
      handle : constant NL.Interface_Handle := ifc.handle;
      ok : Boolean;
      ignoreHandle : NL.Interface_Handle;
   begin
      logPut ("netstack: ");
      logPut (ifc.name.Text (1 .. ifc.name.Length));
      logPut (" detached mac=");
      logMAC (ifc.mac);
      logSend (Warning);
      ifc.usable := False;
      for I in tcpConns'Range loop
         if tcpConns (I).inUse and then tcpConns (I).routed and then tcpConns (I).egress = handle then
            abortConnection (I, clockNow, Layout.Status_Unreachable);
         end if;
      end loop;
      dropDeferredFor (ifc);
      answerParked (ifc.txWait, ok => False);
      answerParked (ifc.dhcpAwait, ok => False);
      if ifc.dhcpBase /= System.Null_Address then
         CuBit.Memory_Grants.Return_Acquisition (ifc.dhcpGrant, ok);
      end if;
      retire (Unsigned_64 (To_Integer (ifc.pktBuf)), Unsigned_64 (PACKET_BUF_SIZE), ifc.pktGrant);
      for R of routeTable loop
         if R.active and then R.via = handle then
            R.active := False;
         end if;
      end loop;
      Ifaces.Remove (S);
      eventSequence := eventSequence + 1;
      Detach_Log.Insert ((handle => handle, sequence => eventSequence), ignoreHandle, ok);
      --  Not recorded (no memory): netmgr finds it gone when it next asks.
      answerWatch;
   end detachInterface;

   --  A kernel notice: a frame grant came back (detach).
   procedure handleEvent (m : Message) is
      E : constant CuBit.Control_Events.Event :=
        CuBit.Control_Events.Decode (m.tag.label, m.tag.length, m.words (0), m.words (1), m.words (2));
      use type CuBit.Control_Events.Event_Kind;
   begin
      if E.Kind /= CuBit.Control_Events.Grant_Returned then
         return;
      end if;
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) and then Ifaces.Get (S).pktGrant.slot = E.Slot and then
           Ifaces.Get (S).pktGrant.generation = E.Generation
         then
            detachInterface (S);
            return;
         end if;
      end loop;
   end handleEvent;

   --  Set_Address: only while the link is not Up (per-network random
   --  addresses are chosen before associating).
   procedure handleSetAddress (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := driverInterface (sender, m.words (0));
      outcome : NL.Address_Outcome;
   begin
      if ifc = No_Interface then
         outcome := NL.Refused_Unknown_Interface;
      elsif not NL.Valid_Address_Word (m.words (1)) or else
        not NL.Assignable (NL.Address_Of (m.words (1)))
      then
         outcome := NL.Refused_Not_Assignable;
      elsif ifc.usable then
         outcome := NL.Refused_Link_Up;
      else
         outcome := NL.Address_Set;
         ifc.mac := netAddress (NL.Address_Of (m.words (1)));
         forgetNeighbours (ifc);
         --  A new address on the link: IPv6 starts over with it (new
         --  stable identifiers, detected again once the link is Up).
         declare
            Fresh : IPv6.Link;
         begin
            ifc.v6 := Fresh;
         end;
         ifc.ipv6Started := False;
         logPut ("netstack: ");
         logPut (ifc.name.Text (1 .. ifc.name.Length));
         logPut (" address set mac=");
         logMAC (ifc.mac);
         logSend;
         interfaceChanged (ifc);
      end if;
      declare
         msg : constant Message :=
           (tag => (label => (if outcome = NL.Address_Set then REPLY_OK else REPLY_ERR),
                    length => 1, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0,
            words => [0 => Unsigned_64 (NL.Address_Outcome'Enum_Rep (outcome)), others => 0]);
         ignore : Unsigned_64;
      begin
         ignore := Send_Answer (Current_Request, msg);
      end;
   end handleSetAddress;

   --  The receive direction's header words.
   function rxWord (ifc : Interface_Access; Offset : Natural) return System.Address is
     (ifc.pktBuf + Storage_Offset (Frames.Receive_Header_At + Offset));

   --  Every receive slot holds a frame netstack has not taken.
   function rxFull (ifc : Interface_Access) return Boolean is
     (ifc.rxRing.Available = Frame_Ring.Fill_Count'Last);

   --  Park a request until netstack has something for it.
   procedure park (p : in out Parked; ok : out Boolean) is
      Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
   begin
      ok := Saved.Saved;
      if ok then
         if p.present then
            answerParked (p, ok => False);   --  a second one replaces the first
         end if;
         p := (present => True, handle => Saved.Handle);
      end if;
   end park;

   procedure handleTransmitWait (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := driverInterface (sender, m.words (0));
      ok  : Boolean;
   begin
      if ifc = No_Interface then
         replyError (sender);
         return;
      end if;
      park (ifc.txWait, ok);
      if not ok then
         replyError (sender);
         return;
      end if;
      --  Work already waiting (frames it has not taken, or receive slots
      --  it asked for that are free): answer now. netstack may have read
      --  the driver's epoch before this wait arrived, so the next pass
      --  could otherwise miss it. Only real work answers: a wait answered
      --  for nothing would come straight back.
      declare
         consumed : Unsigned_32 with Volatile, Import,
           Address => txWord (ifc, Frames.Consumed_At);
         spaceWanted : Unsigned_32 with Volatile, Import,
           Address => rxWord (ifc, Frames.Space_Wanted_At);
      begin
         if Frame_Ring.Index (consumed) /= ifc.txRing.Produced or else
           (spaceWanted /= 0 and then not rxFull (ifc))
         then
            ifc.txRungEpoch := 0;
            ifc.txDoorbell := False;
            answerParked (ifc.txWait);
         end if;
      end;
   end handleTransmitWait;

   ---------------------------------------------------------------------------
   --  netmgr (CuBit.Net_Management)
   ---------------------------------------------------------------------------
   --  The interface a handle word names, or null.
   function managedInterface (word : Unsigned_64) return Interface_Access is
     (if NL.Valid_Handle_Word (word) then interfaceOf (NL.Handle_Of (word)) else No_Interface);

   --  The oldest change netmgr has not seen: an interface whose last change
   --  is after watchSeen, or a detach record after it. Detach records at or
   --  before watchSeen are acknowledged: they go.
   procedure answerWatch is
      best : Unsigned_32 := Unsigned_32'Last;
      report : NM.Event_Report;
      found : Boolean := False;
   begin
      for S in 0 .. Detach_Log.Slot_Count - 1 loop
         if Detach_Log.In_Use (S) and then Detach_Log.Get (S).sequence <= watchSeen then
            Detach_Log.Remove (S);
         end if;
      end loop;
      if not watchWait.present or else eventSequence = watchSeen then
         return;
      end if;
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) and then Ifaces.Get (S).lastChange > watchSeen and then
           Ifaces.Get (S).lastChange < best
         then
            best := Ifaces.Get (S).lastChange;
            report := (Sequence => best, Handle => Ifaces.Get (S).handle,
                       Event => NM.Changed,
                       Condition => Ifaces.Get (S).link.State.Condition,
                       Ups => Ifaces.Get (S).upCount,
                       Reason => (if Ifaces.Get (S).link.State.Condition = NL.Down
                                  then Ifaces.Get (S).link.State.Reason else NL.Not_Reported),
                       Latest => eventSequence,
                       Generation => Ifaces.Get (S).link.Seen);
            found := True;
         end if;
      end loop;
      for S in 0 .. Detach_Log.Slot_Count - 1 loop
         if Detach_Log.In_Use (S) and then Detach_Log.Get (S).sequence < best then
            best := Detach_Log.Get (S).sequence;
            report := (Sequence => best, Handle => Detach_Log.Get (S).handle,
                       Event => NM.Detached, Condition => NL.Down, Ups => 0,
                       Reason => NL.Not_Reported, Latest => eventSequence,
                       Generation => NL.Never_Reported);
            found := True;
         end if;
      end loop;
      if not found then
         return;
      end if;
      declare
         W : constant NM.Words4 := NM.Encode (report);
         msg : constant Message :=
           (tag => (label => REPLY_OK, length => 4, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0, words => [W (0), W (1), W (2), W (3)]);
         ignore : Unsigned_64;
      begin
         watchWait.present := False;
         ignore := replyCap (watchWait.handle, msg);
      end;
   end answerWatch;

   procedure handleWatch (sender : Process_ID; m : Message) is
      ok : Boolean;
   begin
      if m.words (0) > Unsigned_64 (eventSequence) then
         replyError (sender);
         return;
      end if;
      watchSeen := Unsigned_32 (m.words (0));
      park (watchWait, ok);
      if not ok then
         replyError (sender);
         return;
      end if;
      answerWatch;
   end handleWatch;

   procedure handleConfigure (sender : Process_ID; m : Message) is
      D : constant NM.Decoded_Configuration :=
        NM.Decode_Configuration ([m.words (0), m.words (1), m.words (2), m.words (3)]);
      ifc : Interface_Access;
   begin
      if not D.Valid then
         replyError (sender);
         return;
      end if;
      ifc := interfaceOf (D.Handle);
      if ifc = No_Interface then
         replyError (sender);
         return;
      end if;
      declare
         newAddr : constant Net.IPv4Address := Net.unpackIPv4 (Unsigned_64 (D.Item.Address));
         newGW   : constant Net.IPv4Address := Net.unpackIPv4 (Unsigned_64 (D.Item.Gateway));
         clear   : constant Boolean := D.Item.Address = NM.No_IPv4;
      begin
         --  A new address (or none): what was bound to the old one cannot
         --  go on (its peers know the old address).
         if ifc.configured and then (clear or else ifc.ipv4 /= newAddr) then
            addressWithdrawn (ifc, ifc.ipv4);
         end if;
         --  A new address or gateway: what the link told us may be stale.
         if clear or else ifc.ipv4 /= newAddr or else ifc.gateway /= newGW then
            forgetNeighbours (ifc);
         end if;
         ifc.configured := not clear;
         ifc.ipv4 := (if clear then NO_ADDRESS else newAddr);
         ifc.prefix := (if clear then 0 else D.Item.Prefix);
         ifc.netmask := maskOf (ifc.prefix);
         ifc.gateway := (if clear then NO_ADDRESS else newGW);
         ifc.metric := D.Item.Metric;
         if not clear and then newGW /= NO_ADDRESS and then ifc.ipv4Test = TEST_OFF then
            ifc.ipv4Test := TEST_WAITING;
         end if;
         announce (ifc);
         logPut ("netstack: ");
         logPut (ifc.name.Text (1 .. ifc.name.Length));
         if clear then
            logPut (" unconfigured");
         else
            logPut (" configured: ");
            logIP (ifc.ipv4);
            logPut ("/");
            logDec (Unsigned_64 (ifc.prefix));
            logPut (" gateway=");
            logIP (ifc.gateway);
            logPut (" metric=");
            logDec (Unsigned_64 (ifc.metric));
         end if;
         logSend;
         interfaceChanged (ifc);
         replyOKWord (sender, 0);
      end;
   end handleConfigure;

   procedure handleInterfaceNext (sender : Process_ID; m : Message) is
      first : Natural := 0;
   begin
      if m.words (0) /= 0 then
         if not NL.Valid_Handle_Word (m.words (0)) then
            replyError (sender);
            return;
         end if;
         first := Natural (NL.Handle_Of (m.words (0)).Slot) + 1;
      end if;
      for S in first .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            replyOKWord (sender, NL.Handle_Word (Ifaces.Get (S).handle));
            return;
         end if;
      end loop;
      replyError (sender);   --  no more
   end handleInterfaceNext;

   procedure handleInterfaceDetail (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := managedInterface (m.words (0));
   begin
      if ifc = No_Interface then
         replyError (sender);
         return;
      end if;
      declare
         S : constant NM.Interface_Summary :=
           (Identity  => ifc.identity,
            Address   => linkAddress (ifc.factory),
            Kind      => ifc.kind,
            Condition => ifc.link.State.Condition,
            Settings  => (Address => (if ifc.configured
                                      then NM.IPv4_Word (Net.packIPv4 (ifc.ipv4)) else NM.No_IPv4),
                          Prefix  => ifc.prefix,
                          Gateway => NM.IPv4_Word (Net.packIPv4 (ifc.gateway)),
                          Metric  => ifc.metric),
            Sequence  => ifc.lastChange);
         W : constant NM.Words4 := NM.Encode (S);
         msg : constant Message :=
           (tag => (label => REPLY_OK, length => 4, protocol => 0, ipc => <>, reserved => 0),
            authorityTag => 0, words => [W (0), W (1), W (2), W (3)]);
         ignore : Unsigned_64;
      begin
         ignore := Send_Answer (Current_Request, msg);
      end;
   end handleInterfaceDetail;

   --  Scan_Results: an inspector lends one page; the Wireless interface's
   --  scan block is copied into it as read under the block's seqlock. Never
   --  interpreted here: the inspector decodes it (CuBit.Wifi_Scan).
   procedure handleScanResults (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := managedInterface (m.words (0));
      loan : CuBit.Memory_Grants.Grant_Reference;
      mapped : System.Address;
      ok, returned : Boolean;
      before, after : NL.Generation := NL.Never_Reported;
      stable : Boolean := False;
   begin
      if ifc = No_Interface or else ifc.kind /= NL.Wireless
        or else ifc.pktBuf = System.Null_Address
        or else not CuBit.Grant_References.Valid_Wire (m.words (1))
      then
         replyError (sender);
         return;
      end if;
      loan := CuBit.Grant_References.Decode (m.words (1));
      CuBit.Memory_Grants.Acquire
        (loan, sender, 0, NM.Scan_Page_Bytes, CuBit.Memory_Grants.Write_Access, mapped, ok);
      if not ok then
         replyError (sender);
         return;
      end if;
      declare
         function blockWord (W : Natural) return System.Address is
           (ifc.pktBuf + Storage_Offset (Frames.Scan_At + W * CuBit.Wifi_Scan.Word_Bytes));
         function pageWord (W : Natural) return System.Address is
           (mapped + Storage_Offset (W * CuBit.Wifi_Scan.Word_Bytes));
         generation : Unsigned_32 with Volatile, Import,
           Address => blockWord (CuBit.Wifi_Scan.Generation_Word);
      begin
         for Attempt in 1 .. NL.Read_Attempts loop
            before := generation;
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            for W in CuBit.Wifi_Scan.Generation_Word + 1 .. CuBit.Wifi_Scan.Block_Words - 1 loop
               declare
                  source : Unsigned_32 with Volatile, Import, Address => blockWord (W);
                  target : Unsigned_32 with Volatile, Import, Address => pageWord (W);
               begin
                  target := source;
               end;
            end loop;
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            after := generation;
            stable := NL.Stable (before, after);
            exit when stable;
         end loop;
         if stable then
            declare
               target : Unsigned_32 with Volatile, Import,
                 Address => pageWord (CuBit.Wifi_Scan.Generation_Word);
            begin
               target := before;
            end;
         end if;
      end;
      CuBit.Memory_Grants.Return_Acquisition (loan, returned);
      if stable then
         replyOKWord (sender, Unsigned_64 (before));
      else
         replyError (sender);
      end if;
   end handleScanResults;

   --  DHCP_Open: netmgr lends one page for ifc's DHCP messages.
   procedure handleDHCPOpen (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := managedInterface (m.words (0));
      mapped : System.Address;
      ok : Boolean;
   begin
      if ifc = No_Interface or else not CuBit.Grant_References.Valid_Wire (m.words (1)) then
         replyError (sender);
         return;
      end if;
      if ifc.dhcpBase /= System.Null_Address then
         CuBit.Memory_Grants.Return_Acquisition (ifc.dhcpGrant, ok);
         ifc.dhcpBase := System.Null_Address;
      end if;
      ifc.dhcpGrant := CuBit.Grant_References.Decode (m.words (1));
      CuBit.Memory_Grants.Acquire
        (ifc.dhcpGrant, sender, 0, NM.DHCP_Page_Bytes,
         CuBit.Memory_Grants.Write_Access, mapped, ok);
      if not ok then
         replyError (sender);
         return;
      end if;
      ifc.dhcpBase := mapped;
      ifc.dhcpHeld := 0;
      replyOKWord (sender, 0);
   end handleDHCPOpen;

   --  DHCP_Send: the message at Outgoing_At, broadcast from 0.0.0.0:68.
   procedure handleDHCPSend (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := managedInterface (m.words (0));
   begin
      if ifc = No_Interface or else ifc.dhcpBase = System.Null_Address or else
        m.words (1) not in Unsigned_64 (NM.Message_Length'First) .. Unsigned_64 (NM.Message_Length'Last)
        or else not ifc.usable
      then
         replyError (sender);
         return;
      end if;
      declare
         len : constant Natural := Natural (m.words (1));
         --  The page is shared with netmgr: copied once, then framed.
         shared : constant IPv4_Header.Bytes (0 .. len - 1)
           with Import, Address => ifc.dhcpBase + Storage_Offset (NM.Outgoing_At);
         payload : constant IPv4_Header.Bytes (0 .. len - 1) := shared;
         frame : IPv4_Header.Bytes (0 .. UDP_Frame.Payload_At + len - 1);
      begin
         UDP_Frame.Build_Discovery
           (IPv4_Frame.MAC (ifc.mac), NM.DHCP_Client_Port, NM.DHCP_Server_Port, payload, frame);
         sendFrame (ifc, frame'Address, frame'Length);
      end;
      replyOKWord (sender, 0);
   end handleDHCPSend;

   procedure handleDHCPAwait (sender : Process_ID; m : Message) is
      ifc : constant Interface_Access := managedInterface (m.words (0));
      ok  : Boolean;
   begin
      if ifc = No_Interface or else ifc.dhcpBase = System.Null_Address then
         replyError (sender);
         return;
      end if;
      if ifc.dhcpHeld > 0 then
         replyOKWord (sender, Unsigned_64 (ifc.dhcpHeld));
         ifc.dhcpHeld := 0;
         return;
      end if;
      park (ifc.dhcpAwait, ok);
      if not ok then
         replyError (sender);
      end if;
   end handleDHCPAwait;

   ---------------------------------------------------------------------------
   --  Received frames: each interface's receive ring, which its driver
   --  fills (layout in CuBit.Frame_Rings).
   --  - produced (the driver's) and consumed (ours) are free-running frame
   --    counts; a produced count that goes back or more than a ring ahead
   --    is refused (Frame_Ring.Accept_Produced);
   --  - space wanted (the driver's): it found the ring full; its parked
   --    Transmit_Wait is answered after we free slots;
   --  - wake (ours): a new nonzero epoch each time we are about to go idle;
   --    the driver sends one Receive_Doorbell per epoch.
   --  No call and no reply per batch: neither side waits for the other.
   ---------------------------------------------------------------------------
   --  Received frames are handled from netstack's own copy (drainRXRing).
   type Frame_Bytes is array (Natural range <>) of Unsigned_8;
   rxPrivate : Frame_Bytes (0 .. Frames.Maximum_Frame - 1) := [others => 0]
     with Alignment => 8;

   --  The last time netstack had work (TSC), for polling while traffic
   --  flows (CuBit.Busy_Poll).
   lastActivity : Unsigned_64 := 0;

   --  Frames wait in some interface's receive ring.
   function rxPending return Boolean is
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            declare
               ifc : constant Interface_Access := Ifaces.Get (S);
               produced : Unsigned_32 with Volatile, Import,
                 Address => rxWord (ifc, Frames.Produced_At);
            begin
               if Frame_Ring.Index (produced) /= ifc.rxRing.Consumed then
                  return True;
               end if;
            end;
         end if;
      end loop;
      return False;
   end rxPending;

   --  An arrival is due soon, so a short poll will likely find it: a
   --  connection on a sub-millisecond path awaits a SYN answer or an
   --  acknowledgement, a TCP peer was active since netstack last slept, or a resolver query
   --  awaits its answer. With nothing
   --  due, netstack arms its doorbells and sleeps at once instead of
   --  spinning (the io_uring SQPOLL idea, driven by what netstack knows).
   function arrivalExpected return Boolean is
      use TCP_Connection;
   begin
      for I in tcpConns'Range loop
         declare
            C : TCP_Connection.Connection renames tcpFlows (I).E.C;
         begin
            if (C.St in Syn_Sent | Syn_Received or else
                (C.St in Synchronized_State and then C.Snd_Una /= C.Snd_Nxt))
              and then tcpFlows (I).RTO.SRTT = 0
            then
               return True;
            end if;
         end;
      end loop;
      if peerActive then
         return True;
      end if;
      for R of pendingReqs loop
         if R.kind = PENDING_RESOLVE then
            return True;
         end if;
      end loop;
      return False;
   end arrivalExpected;

   --  Handle every frame in ifc's receive ring, and its link block. With
   --  arm, then ask the driver for a doorbell before going idle (and look
   --  once more, at the ring and the link block); without, the caller is
   --  polling and no doorbell is wanted.
   procedure drainRXRing (ifc : Interface_Access; arm : Boolean := True) is
      area : constant System.Address := ifc.pktBuf;
      mark : Unsigned_64;
      ok   : Boolean;
      produced : Unsigned_32 with Volatile, Import,
        Address => rxWord (ifc, Frames.Produced_At);
      consumed : Unsigned_32 with Volatile, Import,
        Address => rxWord (ifc, Frames.Consumed_At);
      spaceWanted : Unsigned_32 with Volatile, Import,
        Address => rxWord (ifc, Frames.Space_Wanted_At);
      doorbell : Unsigned_32 with Volatile, Import,
        Address => rxWord (ifc, Frames.Wake_At);
      linkGeneration : Unsigned_32 with Volatile, Import,
        Address => area + Storage_Offset (Frames.Link_Word_At (Frames.Link_Generation_Word));
   begin
      --  Busy: no doorbells needed until we are about to go idle again.
      doorbell := 0;
      loop
         --  The link block: one load when nothing changed.
         observeLink (ifc);
         --  The driver's count, read once and accepted only if sane.
         Frame_Ring.Accept_Produced (ifc.rxRing, Frame_Ring.Index (produced), ok);
         if not ok then
            debugPrint ("netstack: RX ring count out of range" & LF);
            return;
         end if;
         --  The frames were written before the count was: read them after.
         System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
         if ifc.rxRing.Available > 0 then
            inRxBatch := True;
            clockNow := syscall (SYSCALL_GETTIME);
            mark := Prof.Now;
            while ifc.rxRing.Available > 0 loop
               declare
                  slot : constant System.Address :=
                    area + Storage_Offset
                      (Frames.Receive_Slot_At (Frame_Ring.Head_Slot (ifc.rxRing)));
                  shared : Unsigned_32 with Volatile, Import,
                    Address => slot + Storage_Offset (Frames.Length_At);
                  --  The driver shares this memory: its length is read once,
                  --  and the frame is copied before anything parses it, so
                  --  what was checked is what is used (no double fetch).
                  len : constant Unsigned_32 := shared;
               begin
                  if Frames.Fits (len) and then ifc.usable then
                     declare
                        frame : Frame_Bytes (0 .. Natural (len) - 1)
                          with Import, Address => slot + Storage_Offset (Frames.Frame_At);
                     begin
                        rxPrivate (0 .. Natural (len) - 1) := frame;
                     end;
                     handlePacket (ifc, rxPrivate'Address, Natural (len));
                  end if;
               end;
               Frame_Ring.Release (ifc.rxRing);
            end loop;
            Prof.Charge (Prof.Batch, mark);
            --  The frames are handled before their slots are handed back.
            System.Machine_Code.Asm ("", Clobber => "memory", Volatile => True);
            consumed := Unsigned_32 (ifc.rxRing.Consumed);
            mark := Prof.Now;
            tcpBatchDone;
            Prof.Charge (Prof.Batch_Done, mark);
            System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
            if spaceWanted /= 0 then
               ifc.txDoorbell := True;   --  the driver waits for slots
            end if;
         end if;
         exit when not arm and then Frame_Ring.Index (produced) = ifc.rxRing.Consumed;
         --  About to go idle: ask for a doorbell, then look once more (the
         --  driver may have published frames or a link report before it
         --  could see the request).
         ifc.rxEpoch := (if ifc.rxEpoch = Unsigned_32'Last then 1 else ifc.rxEpoch + 1);
         doorbell := ifc.rxEpoch;
         System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
         --  A report being written (odd generation) rings the doorbell
         --  once it is complete: no need to wait for it here.
         exit when Frame_Ring.Index (produced) = ifc.rxRing.Consumed and then
                   (NL.Generation (linkGeneration) = ifc.link.Seen or else
                    NL.Writing (NL.Generation (linkGeneration)));
         doorbell := 0;
      end loop;
   end drainRXRing;

   --  Every interface's receive ring.
   procedure drainAll (arm : Boolean := True) is
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) then
            drainRXRing (Ifaces.Get (S), arm);
         end if;
      end loop;
   end drainAll;

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


   ---------------------------------------------------------------------------
   --  The driver's and netmgr's requests (CuBit.Net_Link, CuBit.Net_Management)
   ---------------------------------------------------------------------------
   procedure handleRoutes (sender : Process_ID; m : Message; add : Boolean) is
      D : constant NM.Decoded_Configuration :=
        NM.Decode_Configuration ([m.words (0), m.words (1), m.words (2), m.words (3)]);
      dest : Net.IPv4Address;
      done : Boolean := False;
   begin
      if not D.Valid or else interfaceOf (D.Handle) = No_Interface then
         replyError (sender);
         return;
      end if;
      dest := Net.unpackIPv4 (Unsigned_64 (D.Item.Address));
      for R of routeTable loop
         if add and then not R.active then
            R := (active => True, dest => dest, prefix => D.Item.Prefix,
                  gateway => Net.unpackIPv4 (Unsigned_64 (D.Item.Gateway)),
                  via => D.Handle, metric => D.Item.Metric);
            done := True;
            exit;
         elsif not add and then R.active and then R.dest = dest and then
           R.prefix = D.Item.Prefix and then R.via = D.Handle
         then
            R.active := False;
            done := True;
            exit;
         end if;
      end loop;
      if done then
         replyOKWord (sender, 0);
      else
         replyError (sender);
      end if;
   end handleRoutes;

   --  Route_List: every interface's own routes, then the static ones.
   procedure handleRouteList (m : Message) is
      startIdx : constant Natural := wordNatural (m.words (0));
      seen     : Natural := 0;
      total    : Natural := 0;
      packed   : NM.Words4 := [others => 0];
      put      : Natural := 0;
      nextStart : Natural := 0;

      procedure offer (R : NM.Route_Entry) is
      begin
         if seen >= startIdx and then put < NM.Routes_Per_Answer then
            declare
               W : constant NM.Words2 := NM.Encode (R);
            begin
               packed (2 * put) := W (0);
               packed (2 * put + 1) := W (1);
            end;
            put := put + 1;
            nextStart := seen + 1;
         end if;
         seen := seen + 1;
         total := total + 1;
      end offer;
   begin
      for S in 0 .. Ifaces.Slot_Count - 1 loop
         if Ifaces.In_Use (S) and then Ifaces.Get (S).configured then
            declare
               ifc : constant Interface_Access := Ifaces.Get (S);
               network : Net.IPv4Address;
            begin
               for I in network'Range loop
                  network (I) := ifc.ipv4 (I) and ifc.netmask (I);
               end loop;
               offer ((Destination => NM.IPv4_Word (Net.packIPv4 (network)), Prefix => ifc.prefix,
                       Usable => ifc.usable, Metric => ifc.metric, Gateway => NM.No_IPv4,
                       Slot => ifc.handle.Slot));
               if ifc.gateway /= NO_ADDRESS then
                  offer ((Destination => NM.No_IPv4, Prefix => 0, Usable => ifc.usable,
                          Metric => ifc.metric,
                          Gateway => NM.IPv4_Word (Net.packIPv4 (ifc.gateway)),
                          Slot => ifc.handle.Slot));
               end if;
            end;
         end if;
      end loop;
      for R of routeTable loop
         if R.active then
            declare
               ifc : constant Interface_Access := interfaceOf (R.via);
            begin
               offer ((Destination => NM.IPv4_Word (Net.packIPv4 (R.dest)), Prefix => R.prefix,
                       Usable => ifc /= No_Interface and then ifc.usable, Metric => R.metric,
                       Gateway => NM.IPv4_Word (Net.packIPv4 (R.gateway)),
                       Slot => R.via.Slot));
            end;
         end if;
      end loop;
      declare
         routeReply : constant Message :=
           (tag      => (label  => REPLY_OK,
                         length => Unsigned_8 (put),
                         --  The next page's first index (0: no more); tag
                         --  flags are not data (IPC layer bits).
                         protocol  => 0, ipc => <>,
                         reserved  => (if nextStart < total then Unsigned_16 (nextStart) else 0)),
            authorityTag => 0,
            words    => [packed (0), packed (1), packed (2), packed (3)]);
         ignore : Unsigned_64;
      begin
         ignore := Send_Answer (Current_Request, routeReply);
      end;
   end handleRouteList;

   procedure handlePing (sender : Process_ID; m : Message) is
      dstIP  : constant Net.IPv4Address := Net.unpackIPv4 (m.words (0));
      seq    : constant Unsigned_16 := Unsigned_16 (m.words (1) and 16#FFFF#);
      sendTs : constant Unsigned_64 := m.words (2);
      via    : Interface_Access;
      dstMAC : Net.MACAddress;
      found  : Boolean;
      ok     : Boolean;
   begin
      --  Loopback: 127.0.0.0/8 or one of our addresses: answered at once.
      if dstIP (0) = IPv4_Frame.Loopback_Net or else isOurAddress (dstIP) then
         declare
            nowMs : constant Unsigned_64 := syscall (SYSCALL_GETTIME);
            loopReply : constant Message :=
              (tag      => (label => REPLY_OK, length => 3, protocol => 0, ipc => <>, reserved => 0),
               authorityTag => 0,
               words    => [0 => Unsigned_64 (seq), 1 => m.words (0),
                            2 => (if nowMs >= sendTs then nowMs - sendTs else 0),
                            others => 0]);
            ignore : Unsigned_64;
         begin
            ignore := Send_Answer (Current_Request, loopReply);
         end;
         return;
      end if;
      neighbourMAC (dstIP, via, dstMAC, found);
      if via = No_Interface then
         replyError (sender);
         return;
      end if;
      ok := addPending ((kind => PENDING_PING, sender => sender,
                         bufAddr => To_Address (Integer_Address (sendTs)),
                         txid => seq, replySlot => Current_Request, others => <>));
      if not ok then
         replyError (sender);
         return;
      end if;
      --  Unresolved, the echo is lost and the ping times out (asked meanwhile).
      if found then
         currentIf := via;
         ICMPv4.Echo (via.mac, dstMAC, v4 (via.ipv4), v4 (dstIP), seq);
      end if;
   end handlePing;

   procedure dispatchDriver (sender : Process_ID; m : Message) is
   begin
      case NL.Operation_Of (m.tag.label) is
         when NL.Attach =>
            handleAttach (sender, m);
         when NL.Receive_Doorbell =>
            --  One-way: frames or a link report wait in its grant.
            declare
               ifc : constant Interface_Access := driverInterface (sender, m.words (0));
            begin
               if ifc /= No_Interface then
                  drainRXRing (ifc);
               end if;
            end;
         when NL.Transmit_Wait =>
            handleTransmitWait (sender, m);
         when NL.Set_Address =>
            handleSetAddress (sender, m);
      end case;
   end dispatchDriver;

   procedure dispatchManager (sender : Process_ID; m : Message) is
   begin
      case NM.Operation_Of (m.tag.label) is
         when NM.Configure =>
            handleConfigure (sender, m);
         when NM.Set_DNS =>
            primaryDNS := Net.unpackIPv4 (m.words (0));
            secondaryDNS := Net.unpackIPv4 (m.words (1));   --  0.0.0.0: none
            debugPrint ("netstack: DNS set to ");
            printIP (primaryDNS);
            debugPrint ("" & LF);
            replyOKWord (sender, 0);
         when NM.Route_Add =>
            handleRoutes (sender, m, add => True);
         when NM.Route_Delete =>
            handleRoutes (sender, m, add => False);
         when NM.Interface_Next =>
            handleInterfaceNext (sender, m);
         when NM.Interface_Detail =>
            handleInterfaceDetail (sender, m);
         when NM.Route_List =>
            handleRouteList (m);
         when NM.Ping =>
            handlePing (sender, m);
         when NM.Watch =>
            handleWatch (sender, m);
         when NM.DHCP_Open =>
            handleDHCPOpen (sender, m);
         when NM.DHCP_Send =>
            handleDHCPSend (sender, m);
         when NM.DHCP_Await =>
            handleDHCPAwait (sender, m);
         when NM.Scan_Results =>
            handleScanResults (sender, m);
      end case;
   end dispatchManager;

   procedure Run is
      lastExpiry : Unsigned_64 := Unsigned_64'Last;
      sender  : Process_ID;
      msg     : Message;
      found   : Boolean;
   begin
   debugPrint ("netstack: starting..." & LF);
   CuBit.Busy_Poll.Calibrate;
   TCP_Engine.Pool.Initialize (tcpPool);
   isnKey := (K0 => hardwareRandom, K1 => hardwareRandom);
   portKey := (K0 => hardwareRandom, K1 => hardwareRandom);
   Conns.Initialize (connTable, (K0 => hardwareRandom, K1 => hardwareRandom));
   dnsKey := (K0 => hardwareRandom, K1 => hardwareRandom);

   --  Register as DRIVER_NETSTACK
   declare
      ret : Unsigned_64;
   begin
      ret := registerDriver (DRIVER_NETSTACK, Startup.Registration);
      debugPrint ("netstack: registered as driver ");
      printDec (Unsigned_32 (ret));
      debugPrint ("" & LF);
   end;

   --  Signal devmgr that we are ready
   declare
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      rdyIgnore : MessageTag;
   begin
      rdyIgnore := capSend (Startup.Ready,
         (tag      => (label => OP_READY, length => 0,
                       protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0,
          words    => [others => 0]), CuBit.Messages.Wait_Forever);
   end;

   debugPrint ("netstack: waiting for driver attach..." & LF);

   --  Message loop
   --
   --  netstack never calls or submits to a driver: frames go through the
   --  rings, and a driver's doorbell is the answer to its parked
   --  Transmit_Wait. A full ring defers frames to deferredTX, retried
   --  between message dispatches.
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
            for S in 0 .. Ifaces.Slot_Count - 1 loop
               if Ifaces.In_Use (S) and then Ifaces.Get (S).ipv6Started then
                  IPv6.Tick (Ifaces.Get (S).v6, clockNow);
               end if;
            end loop;
            releaseRetired;
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
      serviceQueues;

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
      --  While traffic flows, first poll the receive ring and requests for
      --  a short window, with no doorbell armed (the driver then sends no
      --  IPC); only a quiet window arms it and sleeps.
      elsif not found then
         declare
            dueAt : constant Unsigned_64 := nextDeadline;
         begin
            while arrivalExpected and then CuBit.Busy_Poll.Within
              (lastActivity, CuBit.Busy_Poll.Default_Window_Microseconds)
            loop
               if rxPending then
                  drainAll (arm => False);
                  lastActivity := CuBit.Busy_Poll.Now;
               end if;
               serviceQueues;
               Poll_Service_Request (sender, msg, found);
               exit when found or else syscall (SYSCALL_GETTIME) >= dueAt;
               CuBit.Busy_Poll.Relax;
            end loop;
         end;
         if not found then
            peerActive := False;
            drainAll;   --  arm the doorbells (and take anything that came)
            Poll_Service_Request (sender, msg, found);
            --  Queue requests that came meanwhile: the next pass takes them.
            --  Channels whose kick never came: take them up, then look again
            --  rather than sleep.
            if not found and then not armQueues and then not serviceArmedChannels then
               --  Frames the drain above queued (acknowledgements) reach
               --  their drivers before netstack sleeps.
               ringTXDoorbell;
               receiveUntil (nextDeadline, sender, msg, found);
            end if;
         end if;
      end if;
      if found then
         lastActivity := CuBit.Busy_Poll.Now;
      end if;

      --  4. Dispatch message
      --  A kernel notice (a frame grant came back) has no sender: only
      --  the sleeping receive takes these.
      if found and then sender = No_Process then
         handleEvent (msg);
         found := False;
      end if;
      if found then
         Current_Sender := sender;
         if not admittedRequest (sender, msg) then
            replyError (sender);
         elsif NL.Is_Operation (msg.tag.label) then
            dispatchDriver (sender, msg);
         elsif NM.Is_Operation (msg.tag.label) then
            dispatchManager (sender, msg);
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
                       (networkGrants, From_Word (msg.words (0)), item,
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
                  Network_Grants.Release (networkGrants, From_Word (msg.words (0)), msg.words (1));
                  replyOKWord (sender, 0);
               else replyError (sender); end if;

            when Network_Authority.OP_RELEASE_OWNER =>
               if msg.tag.length = 1 and then Is_Process (From_Word (msg.words (0))) then
                  releaseOwner (From_Word (msg.words (0)));
                  replyOKWord (sender, 0);
               else replyError (sender); end if;

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
                    (tag => (label => REPLY_OK, length => 3, protocol => 0, ipc => <>, reserved => 0),
                     authorityTag => 0,
                     words => [half (0), half (8),
                               (Network_Authority.Descriptor (item) and not prefixBits) or
                                 Shift_Left (Unsigned_64 (CuBit.Net_Address.Mapped_Prefix +
                                                          item.Prefix), 32),
                               0]);
                  ignore : Unsigned_64;
               begin
                  ignore := Send_Answer (Current_Request, answer);
               end;

            when Layout.OP_NET_QUEUE =>
               handleQueue (sender, msg);

            when Layout.OP_NET_ARENA_RELEASE =>
               declare
                  index : Channel_Arenas.Arena_Index;
                  ok, returned : Boolean;
               begin
                  Channel_Arenas.Unregister
                    (arenas, To_Word (sender), Channel_Arenas.Handle (msg.words (0)),
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
               if (msg.words (1) and Layout.Kick_Queue) /= 0 then
                  serviceQueues;
               end if;
               if (msg.words (1) and Layout.End_Wait) /= 0 then
                  endWait (sender);
               end if;

            when others =>
               declare
                  replyMsg : constant Message :=
                    (tag      => (label  => REPLY_ERR,
                                  length => 0,
                                  protocol  => 0, ipc => <>,
                                  reserved  => 0),
                     authorityTag => 0,
                     words    => [others => 0]);
                  ignore : Unsigned_64;
               begin
                  ignore := Send_Answer (Current_Request, replyMsg);
               end;
         end case;
         end if;
      end if;
   end loop;

   end Run;

end Netstack_Service;
