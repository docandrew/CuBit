------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  netstack's IPv6 on one Ethernet link (one Link value per interface):
--  address configuration and neighbors. Every received byte is checked by
--  the proved units before use (IPv6_Header, ND_Message, RA_Message), and
--  every decision about addresses and neighbors is theirs (SLAAC_Table,
--  Neighbor_Cache). This package is the glue: it frames, checksums, sends
--  and times.
--
--  - A link-local address (fe80::/64) and one per advertised prefix, each
--    with an RFC 7217 stable identifier, each used only after duplicate
--    address detection (RFC 4862 5.4) finds no other claimant. Once an
--    address passes detection it is announced with an unsolicited neighbor
--    advertisement (RFC 4861 7.2.6).
--  - Solicitations for our addresses are answered; router solicitations
--    are sent at start and whenever the link comes back.
--  - Echo requests to our addresses are answered.
--  - Link_Down forgets neighbors and the router; Link_Up runs detection
--    again for every address in use (docs/net-link.md section 5).
--  - Self_Test: once a global address is usable, resolve the router and
--    send it one echo request, logging the reply (tests/headless).
--  Not yet: IPv6 transport (TCP and UDP over IPv6), routing beyond the
--  link's default router, and extension headers (packets with any are
--  dropped).
--
--  Proved (tests/tcp-session, level 1): no run-time errors on any frame or
--  clock value; the tables keep their invariants; every frame sent is
--  Emittable (IPv6_Frame: never an IPv4-mapped address on IPv6).
--  Assumed: Send and Log touch no Link (the proof instance gives them
--  state of their own).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header;
with IPv6_Frame;
with Net;
with SipHash;
with SLAAC_Table;
with Neighbor_Cache;

generic
   --  Which interface a Link belongs to, handed back to Send.
   type Link_Owner is private;
   --  A complete Ethernet frame to put on Owner's link.
   with procedure Send (Owner : Link_Owner; Frame : IPv6_Header.Bytes) with
     Pre => IPv6_Frame.Emittable (Frame);
   with procedure Log (Text : String);
package IPv6_Link with SPARK_Mode is

   type Link is private;

   --  The address and neighbor tables' invariants.
   function Valid (L : Link) return Boolean;

   procedure Start
     (L : in out Link; Owner : Link_Owner; MAC : Net.MACAddress;
      Secret : SipHash.Key; Now : Unsigned_64; Self_Test : Boolean)
   with Pre => Valid (L), Post => Valid (L);

   --  An Ethernet frame of type 86DD. Now in milliseconds.
   procedure Receive (L : in out Link; Frame : IPv6_Header.Bytes; Now : Unsigned_64)
   with Pre => Valid (L), Post => Valid (L);

   --  Timers: detection periods, lifetimes, retries. Now in milliseconds.
   procedure Tick (L : in out Link; Now : Unsigned_64)
   with Pre => Valid (L), Post => Valid (L);

   function Next_Deadline (L : Link) return Unsigned_64;

   --  The link stopped carrying traffic: neighbors and the router are
   --  forgotten (they may be on another network when it comes back).
   procedure Link_Down (L : in out Link)
   with Pre => Valid (L), Post => Valid (L);

   --  The link carries traffic again: every address in use is detected
   --  again, then announced; routers are solicited again.
   procedure Link_Up (L : in out Link; Now : Unsigned_64)
   with Pre => Valid (L), Post => Valid (L);

private
   type Link_Local_State is (Absent, Tentative, Usable, Duplicate);
   type Test_State is (Off, Waiting, Resolving, Sent, Done);
   Maximum_Retries : constant := 3;
   subtype Retry_Count is Natural range 0 .. Maximum_Retries;
   type Flags is array (SLAAC_Table.Index) of Boolean;

   type Link is record
      Owner         : Link_Owner;
      Our_MAC       : Net.MACAddress := Net.ZERO_MAC;
      Key           : SipHash.Key := (K0 => 0, K1 => 0);
      Link_Local    : IPv6_Header.Address := [others => 0];
      LL_State      : Link_Local_State := Absent;
      LL_Until      : Unsigned_64 := 0;
      Addresses     : SLAAC_Table.Table;
      Probed        : Flags := [others => False];
      Announced     : Flags := [others => False];
      Neighbors     : Neighbor_Cache.Table;
      Router        : IPv6_Header.Address := [others => 0];
      Have_Router   : Boolean := False;
      Solicits_Sent : Retry_Count := 0;
      Next_Solicit  : Unsigned_64 := Unsigned_64'Last;
      Test          : Test_State := Off;
      Test_Tries    : Retry_Count := 0;
      Test_Until    : Unsigned_64 := Unsigned_64'Last;
   end record;

   function Valid (L : Link) return Boolean is
     (SLAAC_Table.Unique (L.Addresses) and then Neighbor_Cache.Unique (L.Neighbors));
end IPv6_Link;
