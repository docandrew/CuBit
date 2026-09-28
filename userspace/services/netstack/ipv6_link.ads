------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's IPv6 on one Ethernet link: address configuration and
--  neighbors. Every received byte is checked by the proved units before
--  use (IPv6_Header, ND_Message, RA_Message), and every decision about
--  addresses and neighbors is theirs (SLAAC_Table, Neighbor_Cache).
--  This package is the glue: it frames, checksums, sends and times.
--
--  - A link-local address (fe80::/64) and one per advertised prefix, each
--    with an RFC 7217 stable identifier, each used only after duplicate
--    address detection (RFC 4862 5.4) finds no other claimant.
--  - Solicitations for our addresses are answered; router solicitations
--    are sent at start.
--  - Echo requests to our addresses are answered.
--  - Self_Test: once a global address is usable, resolve the router and
--    send it one echo request, logging the reply (tests/headless).
--  Not yet: IPv6 transport (TCP and UDP over IPv6), routing beyond the
--  link's default router, and extension headers (packets with any are
--  dropped).
--
--  Proved (tests/tcp-session, level 1): no run-time errors on any frame or
--  clock value; the tables keep their invariants; every frame sent is
--  Emittable (IPv6_Frame: never an IPv4-mapped address on IPv6).
--  Assumed: Send and Log touch none of this package's state (the proof
--  instance gives them state of their own).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header;
with IPv6_Frame;
with Net;
with SipHash;

generic
   --  A complete Ethernet frame to put on the link.
   with procedure Send (Frame : IPv6_Header.Bytes) with
     Pre => IPv6_Frame.Emittable (Frame);
   with procedure Log (Text : String);
package IPv6_Link with
  SPARK_Mode,
  Abstract_State => State,
  Initializes    => State
is

   --  The address and neighbor tables' invariants.
   function Valid return Boolean with Ghost, Global => State;

   procedure Start
     (MAC : Net.MACAddress; Secret : SipHash.Key; Now : Unsigned_64;
      Self_Test : Boolean)
   with Pre => Valid, Post => Valid;

   --  An Ethernet frame of type 86DD. Now in milliseconds.
   procedure Receive (Frame : IPv6_Header.Bytes; Now : Unsigned_64)
   with Pre => Valid, Post => Valid;

   --  Timers: detection periods, lifetimes, retries. Now in milliseconds.
   procedure Tick (Now : Unsigned_64)
   with Pre => Valid, Post => Valid;

   function Next_Deadline return Unsigned_64;

end IPv6_Link;
