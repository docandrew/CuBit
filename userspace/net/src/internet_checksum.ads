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

   --  The same sum in parts, for data not in one array (a pseudo-header
   --  and a segment in place): Fold (Add_Bytes (Add_Bytes (0, A), B)).
   --  B's length must be even except for the last part.
   Maximum_Partial : constant := 2 ** 40;
   type Partial_Sum is range 0 .. Maximum_Partial;
   Part_Limit : constant := 2 ** 32;

   function Add_Bytes (Sum : Partial_Sum; B : Bytes) return Partial_Sum with
     Pre  => Sum <= Part_Limit and then B'Length <= Maximum_Length and then
             B'Last < Natural'Last,
     Post => Add_Bytes'Result <= Sum + Part_Limit;

   function Fold (Sum : Partial_Sum) return Unsigned_16;

end Internet_Checksum;
