------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The IPv6 neighbor cache (RFC 4861 7.3), hardened as ARP_Cache is:
--  which link-layer address to use for an on-link IPv6 neighbor.
--
--  An address is learned in two cases only:
--  - a solicited advertisement (the S flag) for an address we asked about
--    (Pending), carrying its link address: it becomes Resolved;
--  - a solicitation for our own address from a specified sender carrying
--    its link address: the sender is talking to us (it will expect our
--    advertisement), so it is added or refreshed.
--  An unsolicited advertisement never creates or changes an entry, even
--  with the Override flag, which RFC 4861 would honour: that is the ND
--  form of ARP poisoning. A resolved entry's link address never changes; a
--  changed one is learned only by asking again. Unspecified, multicast and
--  IPv4-mapped senders and multicast or zero link addresses are never
--  learned.
--
--  Proved (tests/net-tcp): the rules above; an entry only ever changes for
--  its own address; at most one entry per address; Lookup finds exactly
--  the resolved entries.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header; use IPv6_Header;
with ND_Message;

package Neighbor_Cache with SPARK_Mode is

   subtype MAC is ND_Message.MAC;
   subtype Kind is ND_Message.Kind;
   use all type ND_Message.Kind;
   use type ND_Message.MAC;

   Capacity : constant := 32;
   subtype Index is Natural range 0 .. Capacity - 1;
   type State is (Free, Pending, Resolved);

   type Neighbor is record
      St    : State := Free;
      IP    : Address := [others => 0];
      Link  : MAC := [others => 0];
      Since : Unsigned_64 := 0;   --  when solicited or last learned
   end record;
   type Table is array (Index) of Neighbor;

   --  What arrived: an advertisement (Peer = its target) or a solicitation
   --  (Peer = its IPv6 source; Ours = its target is our address).
   type Event is record
      Of_Kind   : Kind := Solicitation;
      Solicited : Boolean := False;
      Ours      : Boolean := False;
      Peer      : Address := [others => 0];
      Has_Link  : Boolean := False;
      Link      : MAC := [others => 0];
   end record;

   function Unique (T : Table) return Boolean is
     (for all I in Index =>
        (for all J in Index =>
           (if I /= J and then T (I).St /= Free and then T (J).St /= Free
            then T (I).IP /= T (J).IP)));

   --  A neighbor whose addresses may be learned.
   function Usable (E : Event) return Boolean is
     (E.Has_Link and then not Is_Unspecified (E.Peer) and then
      not Is_Multicast (E.Peer) and then not Is_Mapped (E.Peer) and then
      (E.Link (0) and 1) = 0 and then E.Link /= [0, 0, 0, 0, 0, 0]);

   function Position (T : Table; IP : Address) return Integer with
     Post => (if Position'Result < 0 then
                (for all I in Index => not (T (I).St /= Free and then T (I).IP = IP))
              else Position'Result in Index and then
                   T (Position'Result).St /= Free and then T (Position'Result).IP = IP);

   --  We are about to solicit IP: it becomes Pending unless already known.
   --  A full table gives up its oldest entry.
   procedure Solicit (T : in out Table; IP : Address; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then Position (T, IP) >= 0 and then
             (if Position (T'Old, IP) >= 0 then T = T'Old) and then
             (for all I in Index =>
                (if T (I) /= T'Old (I) then
                   T (I).IP = IP or else T (I).St = Free or else T'Old (I).St = Free));

   procedure Learn (T : in out Table; E : Event; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             --  Nothing changes for a neighbor we may not learn, for an
             --  unsolicited advertisement, or for an advertisement we did
             --  not ask for.
             (if not Usable (E) or else
                 (E.Of_Kind = Advertisement and then
                  (not E.Solicited or else Position (T'Old, E.Peer) < 0 or else
                   T'Old (Position (T'Old, E.Peer)).St /= Pending))
              then T = T'Old) and then
             --  Stated on its own: an IPv4-mapped or multicast neighbor
             --  is never learned.
             (if Is_Mapped (E.Peer) or else Is_Multicast (E.Peer) then T = T'Old) and then
             --  A solicitation not for us changes nothing either.

             (if E.Of_Kind = Solicitation and then not E.Ours then T = T'Old) and then
             --  Only the neighbor's own entry can change.
             (for all I in Index =>
                (if T (I) /= T'Old (I) then
                   T (I).IP = E.Peer or else T'Old (I).St = Free or else T (I).St = Free)) and then
             --  A resolved entry keeps its link address.
             (for all I in Index =>
                (if T'Old (I).St = Resolved and then T (I).St /= Free and then
                    T (I).IP = T'Old (I).IP then T (I).Link = T'Old (I).Link));

   --  The link address to use for IP, if resolved.
   procedure Lookup (T : Table; IP : Address; Link : out MAC; Found : out Boolean) with
     Post => Found = (Position (T, IP) >= 0 and then T (Position (T, IP)).St = Resolved) and then
             (if Found then Link = T (Position (T, IP)).Link);

end Neighbor_Cache;
