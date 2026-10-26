--  P-256 field operations beyond SPARKTLSCrypto.P256's add, sub, mul and
--  square: the curve coefficient b, negation, constant-time equality,
--  inversion, the square root and Legendre symbol, and sgn0. SAE (IEEE
--  Std 802.11-2020 12.4) and RFC 9380 hashing to the curve need them.
--
--  Copied, not reused: FE_B and Inv_F256 are private to SPARKTLSCrypto
--  (P256_B and P256_Inv in src/sparktlscrypto-p256-point.adb, crate
--  commit b89c8be), so they are copied here unchanged in substance, with
--  their proof redone by GNATprove on this unit. Everything else is new
--  composition over the public SPARKTLSCrypto.P256 and Fiat_P256.
--
--  All are built on the Fiat-Crypto field arithmetic and are
--  constant-time: fixed operation sequences (the exponentiations are
--  fixed addition chains), results combined arithmetically or selected
--  with masks, never with branches or memory indices on field values.

with SPARKTLSCrypto.P256; use SPARKTLSCrypto.P256;

package WiFi_Crypto.P256_Field with
   SPARK_Mode => On
is
   --  Curve coefficient b in Montgomery representation (P256_B, copied
   --  from src/sparktlscrypto-p256-point.adb at b89c8be).
   --  b = 0x5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B
   --  b_mont = b * R mod p where R = 2^256
   FE_B : constant P256_FE :=
     (16#D89CDF6229C4BDDF#,
      16#ACF005CD78843090#,
      16#E5A220ABF7212ED6#,
      16#DC30061D04874834#);

   --  -A mod p
   function Neg_F256 (A : P256_FE) return P256_FE with Inline;

   --  1 if A = B, else 0. Inputs are canonical (below p), as every
   --  operation here returns them, so equal values have equal limbs.
   function FE_Equal (A, B : P256_FE) return U32
   with Post => FE_Equal'Result <= 1;

   --  1 if A = 0, else 0.
   function FE_Zero_Flag (A : P256_FE) return U32
   with Post => FE_Zero_Flag'Result <= 1;

   --  A^(p-2): the inverse of a nonzero A, and 0 for 0.
   function Inv_F256 (A : P256_FE) return P256_FE;

   --  A^((p-3)/4), the exponent shared by the square root and the
   --  Legendre symbol (p = 3 mod 4), and by RFC 9380 sqrt_ratio_3mod4.
   function Pow_P_Minus_3_Div_4 (A : P256_FE) return P256_FE;

   --  R := A^((p+1)/4), a square root of A whenever one exists.
   --  Self-checking: Is_Square is 1 exactly when R^2 = A, so the result
   --  rests on the proved multiply, not on a proof about the exponent.
   procedure Sqrt_F256
     (R         : out P256_FE;
      Is_Square : out U32;
      A         : in  P256_FE)
   with Post => Is_Square = FE_Equal (Square_F256 (R), A);

   --  Legendre symbol (A / p): 0 for A = 0, 1 for a nonzero square,
   --  -1 otherwise (A^((p-1)/2) is 0, 1 or p - 1).
   subtype Legendre_Symbol is I32 range -1 .. 1;
   function Legendre_F256 (A : P256_FE) return Legendre_Symbol;

   --  RFC 9380 sgn0 for m = 1: the parity of A's canonical value.
   function Sgn0_F256 (A : P256_FE) return U32
   with Post => Sgn0_F256'Result <= 1;

end WiFi_Crypto.P256_Field;
