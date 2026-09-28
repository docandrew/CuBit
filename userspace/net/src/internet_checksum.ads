------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Internet checksum (RFC 1071): the complement of the one's-complement
--  sum of the bytes as big-endian 16-bit words, an odd last byte padded
--  with zero. The result is a number whose high byte goes first on the
--  wire; summing a message that carries a correct checksum gives 0.
--
--  Proved (tests/net-tcp): no overflow and no access outside the bytes for
--  any length up to Maximum_Length.
--  Tested, not proved: agreement with netstack's word-at-a-time sum
--  (Net.internetChecksum) on random messages of every length to 1,500.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with IPv6_Header;

package Internet_Checksum with SPARK_Mode is

   subtype Bytes is IPv6_Header.Bytes;

   --  An IPv6 payload with its pseudo-header, and then some.
   Maximum_Length : constant := 2 ** 17;

   function Of_Bytes (B : Bytes) return Unsigned_16 with
     Pre => B'Length <= Maximum_Length and then B'Last < Natural'Last;

end Internet_Checksum;
