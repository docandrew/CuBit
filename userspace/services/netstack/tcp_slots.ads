------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's TCP connection slots: each connection's addressing and
--  lifetime. The protocol itself (state, sequence numbers, queues,
--  retransmission and congestion control) is a proved TCP_Flow from
--  userspace/net/src, one per slot (TCP_Engine).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Net;
with TCP_Options;

package TCP_Slots with SPARK_Mode is

   type Slot is record
      --  The slot holds a connection, in any TCP state.
      inUse      : Boolean := False;
      --  A channel or listener backlog still refers to the slot. A closed
      --  connection's slot is reused only once that owner releases it.
      reserved   : Boolean := False;
      localPort  : Unsigned_16 := 0;
      remotePort : Unsigned_16 := 0;
      remoteIP   : Net.IPv4Address := [others => 0];
      remoteMAC  : Net.MACAddress := [others => 0];
   end record;

   --  Connections, including ones their owner released that are still
   --  closing (FIN-WAIT, LAST-ACK): short connections at thousands per
   --  second keep dozens closing at once. STOPGAP until the rings become
   --  the receive buffers and a connection's own state is small
   --  (docs/netstack-redesign.md, "Copies"). Each costs a 64 KiB receive
   --  queue that the loader backs at start, so 64 (8.5 MB) is what fits
   --  the 128 MB desktop profile beside ccl-workbench; the count becomes a
   --  startup parameter (docs/ccl-launch-parameters.md).
   MAX_TCP_CONNS : constant := 64;
   subtype Connection_Index is Natural range 0 .. MAX_TCP_CONNS - 1;
   subtype Connection_Reference is Integer range -1 .. Connection_Index'Last;
   type Table is array (Connection_Index) of Slot;

   --  Parsed fields of an arriving segment (from TCP_Header).
   type SegmentInfo is record
      srcIP   : Net.IPv4Address;
      srcPort : Unsigned_16;
      dstPort : Unsigned_16;
      seqNum  : Unsigned_32;
      ackNum  : Unsigned_32;
      flagSYN : Boolean;
      flagACK : Boolean;
      flagFIN : Boolean;
      flagRST : Boolean;
      winSize : Unsigned_16;
      dataLen : Natural range 0 .. 65_535; -- bounded IPv4 TCP payload
      dataOff : Natural;       -- byte offset of payload in packet buffer
      --  Options (parsed on SYN segments only).
      opts    : TCP_Options.Received := (others => <>);
   end record;

   --  The connection with this remote address and ports, or -1.

end TCP_Slots;
