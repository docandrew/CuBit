-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- P-384 signing keys for the builder certificates and the program signer
-- (SIGN-001, docs/signed-binaries.md "Keypair hygiene"). A weak or
-- degenerate private scalar is refused wherever a key is made, read or
-- used: zero, at least the group order n, or within Small_Multiple_Bound
-- of 0 or of n (so d = 4, the eMudhra key, and its negation are refused,
-- and so is every public key that is a small multiple of G). Every key
-- passes a pairwise consistency test (FIPS 140-3) before use.
--
-- Key files hold SEC 1 ECPrivateKey DER (RFC 5915) with the named curve
-- and the public key, exactly the 167 bytes of one fixed template; any
-- other encoding is refused.
-------------------------------------------------------------------------------
with Interfaces;
with SPARKNaCl;
with SPARKTLSCrypto.P384.Point;

package CuBit.Signing_Keys with SPARK_Mode => On is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Integer_32;

   Scalar_Length : constant := 48;
   Point_Length  : constant := 1 + 2 * Scalar_Length;
   Blind_Length  : constant := SPARKTLSCrypto.P384.Point.Blind_Len;

   subtype Scalar is SPARKNaCl.Bytes_48;
   subtype Point is SPARKNaCl.Byte_Seq (0 .. Point_Length - 1);
   subtype Blind is SPARKNaCl.Byte_Seq (0 .. Blind_Length - 1);

   --  The P-384 group order n, big-endian.
   Order : constant Scalar :=
     [16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#C7#, 16#63#, 16#4D#, 16#81#, 16#F4#, 16#37#, 16#2D#, 16#DF#,
      16#58#, 16#1A#, 16#0D#, 16#B2#, 16#48#, 16#B0#, 16#A7#, 16#7A#,
      16#EC#, 16#EC#, 16#19#, 16#6A#, 16#CC#, 16#C5#, 16#29#, 16#73#];

   --  Keys d with d <= 2^16 or n - d <= 2^16 are refused.
   Small_Multiple_Bound : constant := 2 ** 16;
   Lowest_Strong : constant Scalar :=   --  2^16 + 1
     [45 => 16#01#, 47 => 16#01#, others => 0];
   Highest_Strong : constant Scalar :=  --  n - 2^16 - 1
     [16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#, 16#FF#,
      16#C7#, 16#63#, 16#4D#, 16#81#, 16#F4#, 16#37#, 16#2D#, 16#DF#,
      16#58#, 16#1A#, 16#0D#, 16#B2#, 16#48#, 16#B0#, 16#A7#, 16#7A#,
      16#EC#, 16#EC#, 16#19#, 16#6A#, 16#CC#, 16#C4#, 16#29#, 16#72#];

   --  Big-endian A <= B.
   function At_Most (A, B : Scalar) return Boolean
   with Post => At_Most'Result =
                  ((for all I in Scalar'Range => A (I) = B (I))
                   or else (for some I in Scalar'Range =>
                              A (I) < B (I) and then (for all J in 0 .. I - 1 => A (J) = B (J))));

   function Strong (D : Scalar) return Boolean is
     (At_Most (Lowest_Strong, D) and then At_Most (D, Highest_Strong));

   --  Q = d G (blinded scalar multiplication).
   procedure Public_Of (D : Scalar; Mask : Blind; Q : out Point);

   --  FIPS 140-3 pairwise consistency: d signs a fixed digest and Q
   --  verifies it.
   procedure Pairwise_Consistent (D : Scalar; Q : Point; Mask : Blind; OK : out Boolean);

   --  The key is strong, Q is d G, and the pair passes the pairwise test.
   procedure Check_Pair (D : Scalar; Q : Point; Mask : Blind; OK : out Boolean)
   with Post => (if OK then Strong (D));

   Key_File_Length : constant := 167;
   subtype Key_File is SPARKNaCl.Byte_Seq (0 .. Key_File_Length - 1);

   procedure Encode_Key (D : Scalar; Q : Point; Encoded : out Key_File);

   --  The template, the scalar and the point out of a key file; OK only
   --  for exactly the template with a strong scalar (not yet checked
   --  against Q: Check_Pair does that).
   procedure Decode_Key (Encoded : SPARKNaCl.Byte_Seq; D : out Scalar; Q : out Point; OK : out Boolean)
   with Pre  => Encoded'First = 0,
        Post => (if OK then Strong (D));

end CuBit.Signing_Keys;
