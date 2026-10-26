--  P-256 hashing to the curve (RFC 9380)
--
--  * Map_To_Curve: the simplified SWU map for P-256 (RFC 9380 6.6.2),
--    in the straight-line form of appendix F.2 with sqrt_ratio_3mod4
--    (F.2.1.2), Z = -10 (section 8.2).
--  * Map_Pair_To_Curve: SSWU (u0) + SSWU (u1). This is both the second
--    half of hash_to_curve and SAE hash-to-element's
--    PT = SSWU (u1) + SSWU (u2) (IEEE Std 802.11-2020 12.4.4.2.3).
--  * Reduce_Wide: OS2IP of a 48-octet string mod p, as RFC 9380
--    hash_to_field (L = 48) and SAE H2E (len = olen(p) + olen(p)/2) both
--    need.
--  * Expand_Message_XMD over SHA-256 (RFC 9380 5.3.1) and Hash_To_Curve,
--    the P256_XMD:SHA-256_SSWU_RO_ suite (section 8.2).
--
--  Built on the Fiat-Crypto field arithmetic (SPARKTLSCrypto.P256 and
--  WiFi_Crypto.P256_Field) and the public point addition, doubling and
--  affine conversion of SPARKTLSCrypto.P256.Point.
--
--  Timing: every step is a fixed sequence of field operations; the three
--  data-dependent choices of F.2 (CMOV on tv2 /= 0, on is_gx1_square and
--  on the sign) and the doubling case of the point addition are masked
--  selections (CT_Copy, CT_Copy_Point). No branch or memory index depends
--  on u, so a password-derived u (SAE) does not leak through timing.
--  Expand_Message_XMD's control flow depends on lengths only.

with SPARKTLSCrypto.P256; use SPARKTLSCrypto.P256;
with SPARKTLSCrypto.P256.Point; use SPARKTLSCrypto.P256.Point;

package WiFi_Crypto.P256_SSWU with
   SPARK_Mode => On
is
   --  1 if (X, Y) satisfies y^2 = x^3 - 3x + b, else 0.
   function On_Curve (X, Y : P256_FE) return U32
   with Post => On_Curve'Result <= 1;

   --  (X, Y) := map_to_curve_simple_swu (U), affine.
   procedure Map_To_Curve (X, Y : out P256_FE; U : P256_FE);

   --  P := map_to_curve (U0) + map_to_curve (U1), in affine form (Z = 1),
   --  or all zero in the (negligible) case that the sum is the identity.
   procedure Map_Pair_To_Curve (P : out P256_Jacobian; U0, U1 : P256_FE);

   --  L in RFC 9380 hash_to_field for P-256.
   Wide_Bytes : constant := 48;

   --  U := OS2IP (Src) mod p.
   procedure Reduce_Wide (U : out P256_FE; Src : Byte_Seq)
   with Pre => Src'Length = Wide_Bytes;

   SHA256_Bytes : constant := 32;
   Max_DST_Bytes : constant := 255;
   --  ell <= 255 blocks of SHA-256 output (RFC 9380 5.3.1 step 2).
   Max_Expand_Bytes : constant := 255 * SHA256_Bytes;

   --  Largest index the message may end at (SHA-256 Update's bound).
   Max_Message_Last : constant N32 := N32'Last - 129;

   --  Output := expand_message_xmd (Msg, DST, Output'Length), SHA-256.
   procedure Expand_Message_XMD
     (Output : out Byte_Seq;
      Msg    : Byte_Seq;
      DST    : Byte_Seq)
   with Pre => Output'Length in 1 .. Max_Expand_Bytes
               and then Output'Last < N32'Last - SHA256_Bytes
               and then DST'Length <= Max_DST_Bytes
               and then DST'Last <= Max_Message_Last
               and then Msg'Last <= Max_Message_Last;

   --  P := hash_to_curve (Msg) for P256_XMD:SHA-256_SSWU_RO_ with DST,
   --  in affine form (see Map_Pair_To_Curve).
   procedure Hash_To_Curve
     (P   : out P256_Jacobian;
      Msg : Byte_Seq;
      DST : Byte_Seq)
   with Pre => DST'Length <= Max_DST_Bytes
               and then DST'Last <= Max_Message_Last
               and then Msg'Last <= Max_Message_Last;

end WiFi_Crypto.P256_SSWU;
