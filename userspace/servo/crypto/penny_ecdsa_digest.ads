------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ECDSA's bits2int for Penny's TLS provider (FIPS 186-5 6.4.2, RFC 6979
--  2.3.2), for the hash/curve pairs certificates use: the digest as an
--  integer of the curve order's bit length (256 or 384, whole bytes), its
--  leftmost bits when it is longer (SHA-384 with P-256), zero-extended on
--  the left when it is shorter (SHA-256 with P-384). SPARKTLSCrypto's
--  ECDSA verification takes that integer as bytes; it reduces modulo n.
--  Proved (absence of runtime errors and the postcondition).
------------------------------------------------------------------------------
with Interfaces; use type Interfaces.Integer_32; use type Interfaces.Unsigned_8;
with SPARKNaCl; use SPARKNaCl;

package Penny_ECDSA_Digest with SPARK_Mode, Pure is

   subtype Order_Bytes is N32 with Static_Predicate => Order_Bytes in 32 | 48;
   --  The digests TLS pairs with ECDSA: SHA-256, -384, -512.
   subtype Digest_Bytes is N32 range 32 .. 64;

   function Bits2Int (H : Byte_Seq; Q : Order_Bytes) return Byte_Seq
   with Pre  => H'First = 0 and then H'Length in Digest_Bytes,
        Post => Bits2Int'Result'First = 0
                and then Bits2Int'Result'Length = Q
                and then (if H'Length >= Q
                          then (for all I in 0 .. Q - 1 => Bits2Int'Result (I) = H (I))
                          else (for all I in 0 .. Q - 1 =>
                                  Bits2Int'Result (I) =
                                    (if I < Q - H'Length then 0 else H (I - (Q - H'Length)))));

end Penny_ECDSA_Digest;
