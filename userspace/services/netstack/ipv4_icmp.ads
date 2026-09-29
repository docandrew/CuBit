------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's ICMP for IPv4: answering echo requests, sending its own, and
--  handing errors and echo replies to the rest of netstack. Every byte of a
--  received message is read here, from netstack's own copy of the frame;
--  every frame it sends is built with IPv4_Header.Build.
--
--  - Echo requests are answered only between unicast addresses (never to
--    or from a broadcast, multicast, loopback or zero address) and within
--    a token bucket, so a flood cannot take netstack.
--  - Replies carry DF, so the zero identification is legal (RFC 6864).
--  - Errors (destination unreachable, time exceeded) go to Error_Arrived,
--    which checks them against live connections (ICMPv4_Error, RFC 5927).
--  - Replies to our echo requests go to Echo_Answered.
--
--  Proved (tests/tcp-session, level 1): no run-time errors on any packet;
--  every frame sent is IPv4_Frame.Emittable (unicast source and
--  destination).
--  Assumed: Send, Echo_Answered and Error_Arrived leave this package's
--  state alone (the proof instance gives them state of their own).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv4_Header;
with IPv4_Frame;
with Net;

generic
   --  A complete Ethernet frame to put on the link.
   with procedure Send (Frame : IPv4_Header.Bytes) with
     Pre => IPv4_Frame.Emittable (Frame);
   --  A reply to our echo request Sequence arrived from From.
   with procedure Echo_Answered (From : IPv4_Header.Address; Sequence : Unsigned_16);
   --  An ICMP error arrived (its checksum verified).
   with procedure Error_Arrived (Message : IPv4_Header.Bytes);
package IPv4_ICMP with
  SPARK_Mode,
  Abstract_State => State,
  Initializes    => State
is

   Echo_Identifier : constant := 16#CB17#;
   Maximum_Message : constant :=
     IPv4_Frame.Maximum_Frame - IPv4_Frame.Ethernet_Header - IPv4_Header.Minimum_Size;

   --  An ICMP packet for us: Packet is the IPv4 packet (header first),
   --  From_MAC the frame's source. Now in milliseconds.
   procedure Receive
     (Packet : IPv4_Header.Bytes; Our_MAC, From_MAC : Net.MACAddress; Now : Unsigned_64)
   with Pre => Packet'First = 0 and then IPv4_Header.Well_Formed (Packet);

   --  Send echo request Sequence from Ours to Destination.
   procedure Echo
     (Our_MAC, To_MAC : Net.MACAddress; Ours, Destination : IPv4_Header.Address;
      Sequence : Unsigned_16);

end IPv4_ICMP;
