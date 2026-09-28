------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  IPv6 stateless address autoconfiguration (RFC 4862 5.4, 5.5.3), with
--  RFC 7217 stable interface identifiers: the addresses netstack forms from
--  advertised prefixes, and which of them it may send from.
--
--  - An address is formed Tentative and becomes Preferred only once its
--    duplicate address detection period has passed with no conflict; a
--    conflict makes it Duplicate, and a Duplicate address is never used
--    and never becomes Preferred (the caller may form another with the
--    next DAD counter).
--  - RFC 4862 5.5.3 (e), the two-hour rule: an advertisement never
--    shortens an address's remaining valid lifetime below two hours, so a
--    forged advertisement cannot expire it.
--  - The interface identifier is a keyed hash of the prefix and a counter
--    (SipHash under a secret key), not the hardware address, and never an
--    identifier RFC 5453 reserves.
--
--  Proved (tests/net-tcp): the rules above; at most one entry per prefix;
--  only Preferred and Deprecated addresses are usable as a source, and only
--  Preferred ones for new connections.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header; use IPv6_Header;
with RA_Message;
with SipHash;

package SLAAC_Table with SPARK_Mode is

   Capacity    : constant := 8;
   Two_Hours   : constant := 7_200;         --  seconds (RFC 4862 5.5.3 e)
   DAD_Seconds : constant := 1;             --  one probe, RetransTimer 1 s
   Forever     : constant Unsigned_32 := Unsigned_32'Last;
   Latest_Time : constant := 2 ** 62;       --  seconds; beyond any real clock

   subtype Index is Natural range 0 .. Capacity - 1;
   subtype Time is Unsigned_64 range 0 .. Latest_Time;
   type Interface_ID is array (0 .. 7) of Unsigned_8;

   type State is (Free, Tentative, Preferred, Deprecated, Duplicate);

   type Address_Entry is record
      St              : State := Free;
      Network         : Address := [others => 0];   --  the /64
      Addr            : Address := [others => 0];
      Valid_Until     : Unsigned_64 := 0;           --  Unsigned_64'Last: forever
      Preferred_Until : Unsigned_64 := 0;
      DAD_Until       : Unsigned_64 := 0;
   end record;
   type Table is array (Index) of Address_Entry;

   function Unique (T : Table) return Boolean is
     (for all I in Index =>
        (for all J in Index =>
           (if I /= J and then T (I).St /= Free and then T (J).St /= Free
            then T (I).Network /= T (J).Network)));

   --  When a lifetime of Seconds from Now ends.
   function Deadline (Now : Time; Seconds : Unsigned_32) return Unsigned_64 is
     (if Seconds = Forever then Unsigned_64'Last else Now + Unsigned_64 (Seconds));

   --  RFC 5453: the all-zero identifier and the subnet anycast range.
   function Reserved (I : Interface_ID) return Boolean is
     ((for all K in Interface_ID'Range => I (K) = 0) or else
      (I (0) = 16#FD# and then (for all K in 1 .. 6 => I (K) = 16#FF#) and then
       I (7) >= 16#80#));

   --  RFC 7217: the identifier for Network, from a secret key and the DAD
   --  counter (the caller tries the next counter after a conflict, or if
   --  the result is Reserved).
   function Stable_ID
     (Secret : SipHash.Key; Network : Address; Counter : Unsigned_8)
      return Interface_ID;

   function Position (T : Table; Network : Address) return Integer with
     Post => (if Position'Result < 0 then
                (for all I in Index =>
                   not (T (I).St /= Free and then T (I).Network = Network))
              else Position'Result in Index and then
                   T (Position'Result).St /= Free and then
                   T (Position'Result).Network = Network);

   --  A usable prefix was advertised; ID is the identifier to form a new
   --  address with, if there is none for this prefix yet.
   procedure Advertised
     (T : in out Table; P : RA_Message.Prefix; ID : Interface_ID; Now : Time)
   with
     Pre  => Unique (T) and then RA_Message.Usable (P) and then not Reserved (ID),
     Post => Unique (T) and then
             --  Only this prefix's entry changes.
             (for all I in Index =>
                (if T (I) /= T'Old (I) then
                   T (I).Network = P.Network or else T'Old (I).St = Free)) and then
             --  A duplicate stays duplicate.
             (for all I in Index =>
                (if T'Old (I).St = Duplicate then T (I) = T'Old (I))) and then
             --  The two-hour rule.
             (for all I in Index =>
                (if T'Old (I).St in Tentative | Preferred | Deprecated then
                   T (I).Valid_Until >=
                     Unsigned_64'Min (T'Old (I).Valid_Until, Now + Two_Hours))) and then
             --  A new address starts tentative, awaiting detection.
             (for all I in Index =>
                (if T'Old (I).St = Free and then T (I).St /= Free then
                   T (I).St = Tentative and then T (I).DAD_Until = Now + DAD_Seconds));

   --  Another node claims Addr (a solicitation or advertisement for it
   --  arrived during detection).
   procedure Conflict (T : in out Table; Addr : Address) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                (if T'Old (I).St = Tentative and then T'Old (I).Addr = Addr then
                   T (I).St = Duplicate
                 else T (I) = T'Old (I)));

   --  Time moves on: detection ends, lifetimes expire.
   procedure Tick (T : in out Table; Now : Time) with
     Pre  => Unique (T),
     Post => Unique (T) and then
             (for all I in Index =>
                (T (I).St = T'Old (I).St or else T (I).St = Free or else
                 (T'Old (I).St = Tentative and then T'Old (I).DAD_Until <= Now and then
                  T (I).St in Preferred | Deprecated) or else
                 (T'Old (I).St = Preferred and then T (I).St = Deprecated))) and then
             --  A duplicate never becomes usable.
             (for all I in Index =>
                (if T'Old (I).St = Duplicate then T (I).St in Duplicate | Free));

   --  Addr may be the source of new connections.
   function Preferred_Source (T : Table; Addr : Address) return Boolean is
     (for some I in Index => T (I).St = Preferred and then T (I).Addr = Addr);

end SLAAC_Table;
