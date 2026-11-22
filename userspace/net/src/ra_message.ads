------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Router Advertisement (RFC 4861 4.2, 6.1.2) and its Prefix Information
--  options (4.6.2), as stateless autoconfiguration (RFC 4862 5.5.3) uses
--  them. The ICMPv6 checksum is the caller's.
--
--  Accepted: an IPv6 hop limit of 255 (from the link, not across a
--  router); a link-local source (fe80::/10: only a router on this link may
--  advertise); type 134, code 0; the 16 fixed bytes; options that each
--  have a nonzero length and lie inside the message. A prefix is returned
--  only when it may configure an address: the autonomous flag set, a /64,
--  not link-local and not multicast, and a preferred lifetime no longer
--  than the valid one. At most Maximum_Prefixes are kept.
--
--  Proved (tests/net-tcp): no access outside the message; the option walk
--  ends; every returned prefix satisfies the rule above, with its lifetimes
--  and bits taken from its option.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header;

package RA_Message with SPARK_Mode is

   subtype Bytes is IPv6_Header.Bytes;
   subtype Address is IPv6_Header.Address;

   Advertisement_Type : constant := 134;
   Required_Hop_Limit : constant := 255;
   Fixed_Size         : constant := 16;
   Prefix_Option      : constant := 3;
   Prefix_Option_Size : constant := 32;
   Autonomous_Flag    : constant := 16#40#;
   SLAAC_Prefix       : constant := 64;
   Maximum_Prefixes   : constant := 4;

   function Is_Link_Local (A : Address) return Boolean is
     (A (0) = 16#FE# and then (A (1) and 16#C0#) = 16#80#);

   type Prefix is record
      Network   : Address := [others => 0];   --  the /64, rest zero
      Valid     : Unsigned_32 := 0;           --  seconds; all ones = forever
      Preferred : Unsigned_32 := 0;
   end record;

   function Usable (P : Prefix) return Boolean is
     (not Is_Link_Local (P.Network) and then
      not IPv6_Header.Is_Multicast (P.Network) and then
      (for all I in 8 .. 15 => P.Network (I) = 0) and then
      P.Preferred <= P.Valid);

   type Prefix_List is array (1 .. Maximum_Prefixes) of Prefix;

   type Advertisement is record
      Router_Lifetime : Unsigned_16 := 0;   --  seconds; 0 = not a default router
      Count           : Natural range 0 .. Maximum_Prefixes := 0;
      Prefixes        : Prefix_List := [others => <>];
   end record;

   procedure Parse
     (B : Bytes; Hop_Limit : Unsigned_8; Source : Address;
      A : out Advertisement; OK : out Boolean)
   with
     Pre  => B'First = 0 and then B'Last < 2 ** 16,
     Post => (if OK then
                Hop_Limit = Required_Hop_Limit and then Is_Link_Local (Source) and then
                B'Length >= Fixed_Size and then B (0) = Advertisement_Type and then
                B (1) = 0 and then
                (for all I in 1 .. A.Count => Usable (A.Prefixes (I))) and then
                --  Stated on its own: no link-local or multicast prefix.
                (for all I in 1 .. A.Count =>
                   not Is_Link_Local (A.Prefixes (I).Network) and then
                   not IPv6_Header.Is_Multicast (A.Prefixes (I).Network)));

end RA_Message;
