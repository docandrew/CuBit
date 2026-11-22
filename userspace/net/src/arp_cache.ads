------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The ARP cache, hardened against spoofing (RFC 826 with RFC 5227's
--  cautions): which link-layer address to use for an IPv4 neighbour.
--
--  An address is learned in two cases only:
--  - a reply for an address we asked about (Pending): it becomes Resolved;
--  - a request for our own address: its sender is talking to us, so RFC
--    826's merge adds or refreshes the sender (it will expect our reply).
--  An unsolicited reply never creates or changes an entry (the classic
--  poisoning: a forged reply claiming the gateway's address), and a
--  resolved entry's hardware address never changes at all: a forged
--  request cannot rewrite it either (a changed address is learned only by
--  asking again, which makes the entry Pending). Zero,
--  loopback, multicast and broadcast IPv4 senders and zero, broadcast and
--  multicast hardware senders are never learned.
--
--  Proved (tests/net-tcp): the rules above; an entry only ever changes for
--  its own address; at most one entry per address; Lookup finds exactly
--  the resolved entries.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with ARP_Packet; use ARP_Packet;

package ARP_Cache with SPARK_Mode is

   Capacity : constant := 32;
   subtype Index is Natural range 0 .. Capacity - 1;

   --  Probing: resolved, but not confirmed for a while, so we asked again;
   --  its address stays usable meanwhile, and the answer to our question
   --  may change it (the one way a mapping's address ever changes).
   type State is (Free, Pending, Resolved, Probing);
   subtype Usable is State range Resolved .. Probing;

   type Neighbour is record
      St    : State := Free;
      IP    : IPv4 := [others => 0];
      HW    : MAC := [others => 0];
      Since : Unsigned_64 := 0;   --  when requested or last learned
   end record;

   type Table is array (Index) of Neighbour;

   function Unique (T : Table) return Boolean is
     (for all I in Index =>
        (for all J in Index =>
           (if I /= J and then T (I).St /= Free and then T (J).St /= Free
            then T (I).IP /= T (J).IP)));

   --  A sender whose addresses may be learned.
   function Usable_Sender (IP : IPv4; HW : MAC) return Boolean is
     (IP (0) /= 0 and then IP (0) /= 127 and then IP (0) < 224 and then
      (HW (0) and 1) = 0 and then HW /= [0, 0, 0, 0, 0, 0]);

   function Position (T : Table; IP : IPv4) return Integer with
     Post => (if Position'Result < 0 then
                (for all I in Index => not (T (I).St /= Free and then T (I).IP = IP))
              else Position'Result in Index and then
                   T (Position'Result).St /= Free and then T (Position'Result).IP = IP);

   --  We are about to ask for IP: it becomes Pending unless already known.
   --  A full table gives up its oldest entry.
   procedure Request (T : in out Table; IP : IPv4; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then Position (T, IP) >= 0 and then
             (if Position (T'Old, IP) >= 0 then T = T'Old) and then
             (for all I in Index =>
                (if T (I) /= T'Old (I) then
                   T (I).IP = IP or else T (I).St = Free or else T'Old (I).St = Free));

   --  An ARP packet arrived; Ours is whether its target is our address.
   procedure Learn (T : in out Table; P : Packet; Ours : Boolean; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             --  Nothing changes for a sender we may not learn, or for an
             --  unsolicited reply.
             (if not Usable_Sender (P.Sender_IP, P.Sender_HW) or else
                 (P.Op = Reply and then
                  (Position (T'Old, P.Sender_IP) < 0 or else
                   T'Old (Position (T'Old, P.Sender_IP)).St not in Pending | Probing))
              then T = T'Old) and then
             --  A request not for us changes nothing either.
             (if P.Op = Request and then not Ours then T = T'Old) and then
             --  Only the sender's own entry can change.
             (for all I in Index =>
                (if T (I) /= T'Old (I) then
                   T (I).IP = P.Sender_IP or else T'Old (I).St = Free or else T (I).St = Free)) and then
             --  A resolved mapping keeps its hardware address: only the
             --  answer to our own question (Probing) may change it.
             (for all I in Index =>
                (if T'Old (I).St = Resolved and then T (I).St /= Free and then
                    T (I).IP = T'Old (I).IP then T (I).HW = T'Old (I).HW)) and then
             (for all I in Index =>
                (if T'Old (I).St = Probing and then T (I).St /= Free and then
                    T (I).IP = T'Old (I).IP and then T (I).HW /= T'Old (I).HW
                 then P.Op = Reply));

   --  The link-layer address to use for IP, if resolved (or being
   --  reconfirmed).
   procedure Lookup (T : Table; IP : IPv4; HW : out MAC; Found : out Boolean) with
     Post => Found = (Position (T, IP) >= 0 and then T (Position (T, IP)).St in Usable) and then
             (if Found then HW = T (Position (T, IP)).HW);

   --  IP's mapping is doubted (too old, or traffic through it stalls): the
   --  caller asks again, and until the answer the address stays in use.
   procedure Reconfirm (T : in out Table; IP : IPv4; Now : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                (if T'Old (I).St = Resolved and then T'Old (I).IP = IP then
                   T (I) = (T'Old (I) with delta St => Probing, Since => Now)
                 else T (I) = T'Old (I)));

   --  Questions unanswered for Timeout: a Pending entry is dropped, and so
   --  is a Probing one (the neighbour has gone or changed without saying).
   procedure Expire (T : in out Table; Now, Timeout : Unsigned_64) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                T (I) = T'Old (I) or else
                (T (I).St = Free and then T'Old (I).St in Pending | Probing and then
                 Now - Timeout >= T'Old (I).Since and then Now >= Timeout));

end ARP_Cache;
