------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  SipHash-2-4 (Aumasson and Bernstein, 2012): a keyed pseudorandom
--  function for short inputs, as Linux uses for TCP initial sequence
--  numbers and SYN cookies.
--
--  Proved: absence of runtime errors. Checked against the paper's
--  reference vectors (tests/net-tcp); SipHash's security is the paper's
--  claim, not something proved here.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package SipHash with SPARK_Mode, Pure is

   type Key is record
      K0, K1 : Unsigned_64;   --  the 16 key bytes, little-endian halves
   end record;

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   function Hash (K : Key; Message : Byte_Array) return Unsigned_64
     with Pre => Message'Length < 2 ** 16 and then Message'First in 0 .. 2 ** 24;

   --  Key bytes 0 .. 15 as the reference implementation reads them.
   function To_Key (Bytes : Byte_Array) return Key
     with Pre => Bytes'Length = 16 and then Bytes'First in 0 .. 2 ** 24;
end SipHash;
