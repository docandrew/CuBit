------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  IPv6 Neighbor Discovery messages (RFC 4861 4.3, 4.4, 7.1): Neighbor
--  Solicitation and Neighbor Advertisement, the ICMPv6 message after the
--  IPv6 header (its checksum is the caller's).
--
--  Accepted: an IPv6 hop limit of 255 (the message cannot have crossed a
--  router: RFC 4861 7.1.1); type 135 or 136, code 0; at least the 24 fixed
--  bytes; a target that is not multicast; options that each have a
--  nonzero length and lie inside the message (a zero-length option would
--  make the walk loop forever, so it rejects the whole message). The
--  first source or target link-layer address option for Ethernet (length
--  one: eight bytes, a six-byte address) is returned.
--
--  Proved (tests/net-tcp): no access outside the message; the option walk
--  ends; an accepted message has the stated hop limit, type and code, its
--  target is its bytes 8 .. 23 and is not multicast, and a returned link
--  address is six bytes from inside an option of the right type.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header;

package ND_Message with SPARK_Mode is

   use type IPv6_Header.Address;

   subtype Bytes is IPv6_Header.Bytes;
   subtype Address is IPv6_Header.Address;
   type MAC is array (0 .. 5) of Unsigned_8;

   Solicitation_Type  : constant := 135;
   Advertisement_Type : constant := 136;
   Required_Hop_Limit : constant := 255;
   Fixed_Size         : constant := 24;
   Source_Link_Option : constant := 1;
   Target_Link_Option : constant := 2;

   type Kind is (Solicitation, Advertisement);

   type Message is record
      Of_Kind   : Kind := Solicitation;
      Router    : Boolean := False;   --  NA flags (RFC 4861 4.4)
      Solicited : Boolean := False;
      Override  : Boolean := False;
      Target    : Address := [others => 0];
      Has_Link  : Boolean := False;   --  a link-layer address option
      Link      : MAC := [others => 0];
   end record;

   procedure Parse
     (B : Bytes; Hop_Limit : Unsigned_8; M : out Message; OK : out Boolean)
   with
     Pre  => B'First = 0 and then B'Last < 2 ** 16,
     Post => (if OK then
                Hop_Limit = Required_Hop_Limit and then
                B'Length >= Fixed_Size and then
                B (0) in Solicitation_Type | Advertisement_Type and then
                B (1) = 0 and then
                M.Of_Kind = (if B (0) = Solicitation_Type then Solicitation
                             else Advertisement) and then
                M.Target = IPv6_Header.Address_At (B, 8) and then
                not IPv6_Header.Is_Multicast (M.Target));

end ND_Message;
