------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The UDP header on the wire (RFC 768), parsed in place: a datagram whose
--  length field covers the 8-byte header and lies within the bytes the IP
--  packet carries. The checksum is checked by the caller (zero means none
--  was computed, which IPv4 permits; RFC 768).
--
--  Proved (tests/net-tcp): Well_Formed is exactly that rule; every parsed
--  field is its bytes on the wire.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package UDP_Header with SPARK_Mode is

   type Bytes is array (Natural range <>) of Unsigned_8;

   Size : constant := 8;

   type Header is record
      Source_Port, Destination_Port : Unsigned_16 := 0;
      Length   : Natural := Size;   --  header and payload
      Checksum : Unsigned_16 := 0;
   end record;

   function U16 (B : Bytes; I : Natural) return Unsigned_16 is
     (Shift_Left (Unsigned_16 (B (I)), 8) or Unsigned_16 (B (I + 1)))
   with Pre => I >= B'First and then I < B'Last;

   function Well_Formed (B : Bytes) return Boolean is
     (B'First = 0 and then B'Length >= Size and then B'Length <= 2 ** 16 and then
      Natural (U16 (B, 4)) >= Size and then Natural (U16 (B, 4)) <= B'Length);

   procedure Parse (B : Bytes; H : out Header) with
     Pre  => Well_Formed (B),
     Post => H.Source_Port = U16 (B, 0) and then H.Destination_Port = U16 (B, 2) and then
             H.Length = Natural (U16 (B, 4)) and then H.Checksum = U16 (B, 6) and then
             H.Length >= Size and then H.Length <= B'Length;

end UDP_Header;
