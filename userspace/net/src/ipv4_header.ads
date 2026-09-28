------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The IPv4 header on the wire (RFC 791): what netstack accepts from a
--  received frame, parsed in place.
--
--  Accepted: version 4; a header of 20 to 60 bytes inside the packet; a
--  total length covering the header and within the bytes received (a
--  truncated datagram is never read as a shorter valid one); no fragment
--  (MF clear, offset zero; there is no reassembly yet) and the reserved
--  flag clear (DF alone is fine). The header checksum is checked by the
--  caller. specs/ipv4.rflx stays the specification; tests/net-headers
--  compares the two.
--
--  Proved (tests/net-tcp): Well_Formed is exactly that rule; every parsed
--  field is its bytes on the wire, big-endian. Build writes a version-4,
--  20-byte, unfragmented header whose fields are what it was given,
--  leaving the payload alone. Tested, not proved: its checksum verifies.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package IPv4_Header with SPARK_Mode is

   type Bytes is array (Natural range <>) of Unsigned_8;

   Minimum_Size : constant := 20;
   Maximum_Size : constant := 60;
   subtype Header_Size is Natural range Minimum_Size .. Maximum_Size;
   type Address is array (0 .. 3) of Unsigned_8;

   type Header is record
      Size         : Header_Size := Minimum_Size;   --  IHL * 4
      Total_Length : Natural := Minimum_Size;       --  header and payload
      Protocol     : Unsigned_8 := 0;
      TTL          : Unsigned_8 := 0;
      Source, Destination : Address := [others => 0];
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function Stated_Size (B : Bytes) return Natural is
     (Natural (B (B'First) and 16#0F#) * 4)
   with Pre => B'Length >= 1;

   --  An IPv4 packet (header and payload) netstack accepts. B holds the
   --  bytes after the Ethernet header, possibly with link padding after
   --  the datagram.
   function Well_Formed (B : Bytes) return Boolean is
     (B'Length >= Minimum_Size and then B'Length <= 2 ** 16 and then
      Shift_Right (B (B'First), 4) = 4 and then
      Stated_Size (B) >= Minimum_Size and then
      Natural (U16 (B, B'First + 2)) >= Stated_Size (B) and then
      Natural (U16 (B, B'First + 2)) <= B'Length and then
      --  Reserved flag, MF and the fragment offset all zero (DF allowed).
      (U16 (B, B'First + 6) and 16#BFFF#) = 0);

   procedure Parse (B : Bytes; H : out Header) with
     Pre  => B'First = 0 and then Well_Formed (B),
     Post => H.Size = Stated_Size (B) and then H.Total_Length = Natural (U16 (B, 2)) and then
             H.Size <= H.Total_Length and then H.Total_Length <= B'Length and then
             H.TTL = B (8) and then H.Protocol = B (9) and then
             H.Source = [B (12), B (13), B (14), B (15)] and then
             H.Destination = [B (16), B (17), B (18), B (19)];

   Version_IHL    : constant := 16#45#;   --  version 4, a 20-byte header
   Dont_Fragment  : constant := 16#4000#;
   Maximum_Length : constant := 16#FFFF#;
   Checksum_At    : constant := 10;

   --  Write a 20-byte header (no options) for H into B (0 .. 19); the
   --  payload follows it. DF set as asked; identification zero (RFC 6864:
   --  atomic datagrams need none).
   procedure Build (H : Header; DF : Boolean; B : in out Bytes) with
     Pre  => B'First = 0 and then B'Length <= 2 ** 16 and then H.Size = Minimum_Size and then
             H.Total_Length in Minimum_Size .. Maximum_Length and then
             H.Total_Length <= B'Length,
     Post => Shift_Right (B (0), 4) = 4 and then
             Stated_Size (B) = Minimum_Size and then
             (U16 (B, 6) and 16#BFFF#) = 0 and then
             Natural (U16 (B, 2)) = H.Total_Length and then
             B (8) = H.TTL and then B (9) = H.Protocol and then
             [B (12), B (13), B (14), B (15)] = H.Source and then
             [B (16), B (17), B (18), B (19)] = H.Destination and then
             B (Minimum_Size .. B'Last) = B'Old (Minimum_Size .. B'Last);

end IPv4_Header;
