------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The IPv6 header on the wire (RFC 8200 3): what netstack accepts from a
--  received frame, parsed in place, and what it builds to send.
--
--  CuBit holds every address as IPv6, IPv4 in the mapped form
--  ::ffff:a.b.c.d (tests/locators). A mapped address therefore must never
--  appear in an IPv6 packet: arriving, it would let an IPv6 peer pose as
--  an IPv4 one and pass an IPv4 scope; leaving, it would be a packet for
--  the IPv4 path sent on the wrong one. Both directions are refused here.
--
--  Accepted: version 6; the stated payload inside the bytes received (a
--  truncated packet is never read as a shorter valid one; link padding
--  after it is ignored); a source that is not IPv4-mapped and not
--  multicast (RFC 4291 2.7); a destination that is not IPv4-mapped and not
--  unspecified. Extension headers are the caller's (Next_Header).
--
--  Proved (tests/net-tcp): Well_Formed is exactly that rule; every parsed
--  field is its bytes on the wire, big-endian; Build writes a header that
--  Well_Formed accepts and Parse reads back unchanged, and it cannot be
--  asked to write a mapped address.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package IPv6_Header with SPARK_Mode is

   type Bytes is array (Natural range <>) of Unsigned_8;

   Size : constant := 40;
   Maximum_Payload : constant := 16#FFFF#;   --  no jumbograms (RFC 2675)
   type Address is array (0 .. 15) of Unsigned_8;
   subtype Flow_Label_Value is Unsigned_32 range 0 .. 16#F_FFFF#;

   type Header is record
      Traffic_Class  : Unsigned_8 := 0;
      Flow_Label     : Flow_Label_Value := 0;
      Payload_Length : Natural range 0 .. Maximum_Payload := 0;
      Next_Header    : Unsigned_8 := 0;
      Hop_Limit      : Unsigned_8 := 0;
      Source, Destination : Address := [others => 0];
   end record;

   --  ::ffff:0:0/96
   function Is_Mapped (A : Address) return Boolean is
     ((for all I in 0 .. 9 => A (I) = 0) and then
      A (10) = 16#FF# and then A (11) = 16#FF#);
   --  ff00::/8
   function Is_Multicast (A : Address) return Boolean is (A (0) = 16#FF#);
   --  ::
   function Is_Unspecified (A : Address) return Boolean is
     (for all I in Address'Range => A (I) = 0);

   function Acceptable_Source (A : Address) return Boolean is
     (not Is_Mapped (A) and then not Is_Multicast (A));
   function Acceptable_Destination (A : Address) return Boolean is
     (not Is_Mapped (A) and then not Is_Unspecified (A));

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function Address_At (B : Bytes; I : Natural) return Address is
     [for K in Address'Range => B (I + K)]
   with Pre => I >= B'First and then B'Last >= 15 and then I <= B'Last - 15;

   --  An IPv6 packet (header and payload) netstack accepts. B holds the
   --  bytes after the Ethernet header, possibly with link padding.
   function Well_Formed (B : Bytes) return Boolean is
     (B'First = 0 and then B'Length >= Size and then B'Length <= 2 ** 17 and then
      Shift_Right (B (0), 4) = 6 and then
      Size + Natural (U16 (B, 4)) <= B'Length and then
      Acceptable_Source (Address_At (B, 8)) and then
      Acceptable_Destination (Address_At (B, 24)));

   procedure Parse (B : Bytes; H : out Header) with
     Pre  => Well_Formed (B),
     Post => H.Payload_Length = Natural (U16 (B, 4)) and then
             Size + H.Payload_Length <= B'Length and then
             H.Traffic_Class =
               (Shift_Left (B (0), 4) or Shift_Right (B (1), 4)) and then
             H.Flow_Label =
               (Shift_Left (Unsigned_32 (B (1) and 16#0F#), 16) or
                Shift_Left (Unsigned_32 (B (2)), 8) or Unsigned_32 (B (3))) and then
             H.Next_Header = B (6) and then H.Hop_Limit = B (7) and then
             H.Source = Address_At (B, 8) and then
             H.Destination = Address_At (B, 24) and then
             Acceptable_Source (H.Source) and then
             Acceptable_Destination (H.Destination) and then
             --  Stated on its own: no IPv4-mapped address comes in on IPv6.
             not Is_Mapped (H.Source) and then not Is_Mapped (H.Destination);

   --  Write H as a header into B (0 .. 39); the payload follows it.
   procedure Build (H : Header; B : in out Bytes) with
     Pre  => B'First = 0 and then B'Last < 2 ** 17 and then
             B'Length >= Size + H.Payload_Length and then
             Acceptable_Source (H.Source) and then
             Acceptable_Destination (H.Destination),
     Post => Well_Formed (B) and then
             --  Stated on its own: no IPv4-mapped address goes out on IPv6.
             not Is_Mapped (Address_At (B, 8)) and then
             not Is_Mapped (Address_At (B, 24)) and then
             Natural (U16 (B, 4)) = H.Payload_Length and then
             B (6) = H.Next_Header and then B (7) = H.Hop_Limit and then
             Address_At (B, 8) = H.Source and then
             Address_At (B, 24) = H.Destination and then
             B (Size .. B'Last) = B'Old (Size .. B'Last);

end IPv6_Header;
