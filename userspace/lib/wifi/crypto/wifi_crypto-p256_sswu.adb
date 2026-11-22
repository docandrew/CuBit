--  P-256 hashing to the curve (body). See the spec.

with SPARKTLSCrypto.Hashing.SHA256;
with WiFi_Crypto.P256_Field; use WiFi_Crypto.P256_Field;

package body WiFi_Crypto.P256_SSWU with
   SPARK_Mode => On
is
   pragma Warnings
     (GNATprove, Off, "is set by ""Final"" but not used after the call",
      Reason => "a hash state is dead after Final");

   --  Constants in Montgomery form (x * 2^256 mod p), as FE_B.

   --  A = -3 = p - 3
   FE_A : constant P256_FE :=
     (16#FFFF_FFFF_FFFF_FFFC#, 16#0000_0003_FFFF_FFFF#,
      16#0000_0000_0000_0000#, 16#FFFF_FFFC_0000_0004#);

   --  Z = -10 = p - 10 (RFC 9380 8.2)
   FE_Z : constant P256_FE :=
     (16#FFFF_FFFF_FFFF_FFF5#, 16#0000_000A_FFFF_FFFF#,
      16#0000_0000_0000_0000#, 16#FFFF_FFF5_0000_000B#);

   --  c2 = sqrt (-Z) = sqrt (10) for sqrt_ratio_3mod4 (F.2.1.2):
   --  0x25AC71C31E27646736870398AE7F554D8472E008B3AA2A49D332CBD81BCC3B80
   FE_Sqrt_Minus_Z : constant P256_FE :=
     (16#5E02_C711_675E_6A02#, 16#87BF_F529_BDC2_308F#,
      16#96EC_3770_6157_2011#, 16#6FAE_2D90_ED57_0CFC#);

   --  2^192, the weight of the high 24 octets in Reduce_Wide.
   FE_Two_192 : constant P256_FE :=
     (16#FFFF_FFFE_FFFF_FFFF#, 16#FFFF_FFFF_FFFF_FFFE#,
      16#0000_0002_0000_0000#, 16#0000_0000_0000_0003#);

   --  R := (if Ctl = 1 then B else A), without a branch.
   function CMov (A, B : P256_FE; Ctl : U32) return P256_FE is
      R : P256_FE := A;
   begin
      CT_Copy (Ctl, R, B);
      return R;
   end CMov;

   function On_Curve (X, Y : P256_FE) return U32 is
      --  x^3 - 3x + b - y^2
      RHS : constant P256_FE :=
        Add_F256 (Add_F256 (Mul_F256 (Square_F256 (X), X), Mul_F256 (FE_A, X)), FE_B);
   begin
      return FE_Equal (RHS, Square_F256 (Y));
   end On_Curve;

   --  RFC 9380 F.2.1.2: (Is_QR, Y) with Y = sqrt (U / V) when U / V is a
   --  square, else Y = sqrt (Z * U / V).
   procedure Sqrt_Ratio_3Mod4
     (Is_QR : out U32;
      Y     : out P256_FE;
      U, V  : P256_FE)
   with Post => Is_QR <= 1
   is
      Tv1, Tv2, Tv3, Y1, Y2 : P256_FE;
   begin
      Tv1 := Square_F256 (V);                    --  1. tv1 = v^2
      Tv2 := Mul_F256 (U, V);                    --  2. tv2 = u * v
      Tv1 := Mul_F256 (Tv1, Tv2);                --  3. tv1 = tv1 * tv2
      Y1  := Pow_P_Minus_3_Div_4 (Tv1);          --  4. y1 = tv1^c1
      Y1  := Mul_F256 (Y1, Tv2);                 --  5. y1 = y1 * tv2
      Y2  := Mul_F256 (Y1, FE_Sqrt_Minus_Z);     --  6. y2 = y1 * c2
      Tv3 := Square_F256 (Y1);                   --  7. tv3 = y1^2
      Tv3 := Mul_F256 (Tv3, V);                  --  8. tv3 = tv3 * v
      Is_QR := FE_Equal (Tv3, U);                --  9. isQR = tv3 == u
      Y := CMov (Y2, Y1, Is_QR);                 --  10. y = CMOV (y2, y1, isQR)
   end Sqrt_Ratio_3Mod4;

   procedure Map_To_Curve (X, Y : out P256_FE; U : P256_FE) is
      Tv1, Tv2, Tv3, Tv4, Tv5, Tv6, Y1 : P256_FE;
      Is_Gx1_Square, E1 : U32;
   begin
      Tv1 := Square_F256 (U);                    --  1. tv1 = u^2
      Tv1 := Mul_F256 (FE_Z, Tv1);               --  2. tv1 = Z * tv1
      Tv2 := Square_F256 (Tv1);                  --  3. tv2 = tv1^2
      Tv2 := Add_F256 (Tv2, Tv1);                --  4. tv2 = tv2 + tv1
      Tv3 := Add_F256 (Tv2, FE_One);             --  5. tv3 = tv2 + 1
      Tv3 := Mul_F256 (FE_B, Tv3);               --  6. tv3 = B * tv3
      --  7. tv4 = CMOV (Z, -tv2, tv2 != 0)
      Tv4 := CMov (FE_Z, Neg_F256 (Tv2), 1 - FE_Zero_Flag (Tv2));
      Tv4 := Mul_F256 (FE_A, Tv4);               --  8. tv4 = A * tv4
      Tv2 := Square_F256 (Tv3);                  --  9. tv2 = tv3^2
      Tv6 := Square_F256 (Tv4);                  --  10. tv6 = tv4^2
      Tv5 := Mul_F256 (FE_A, Tv6);               --  11. tv5 = A * tv6
      Tv2 := Add_F256 (Tv2, Tv5);                --  12. tv2 = tv2 + tv5
      Tv2 := Mul_F256 (Tv2, Tv3);                --  13. tv2 = tv2 * tv3
      Tv6 := Mul_F256 (Tv6, Tv4);                --  14. tv6 = tv6 * tv4
      Tv5 := Mul_F256 (FE_B, Tv6);               --  15. tv5 = B * tv6
      Tv2 := Add_F256 (Tv2, Tv5);                --  16. tv2 = tv2 + tv5
      X   := Mul_F256 (Tv1, Tv3);                --  17. x = tv1 * tv3
      Sqrt_Ratio_3Mod4 (Is_Gx1_Square, Y1, Tv2, Tv6);  --  18.
      Y := Mul_F256 (Tv1, U);                    --  19. y = tv1 * u
      Y := Mul_F256 (Y, Y1);                     --  20. y = y * y1
      X := CMov (X, Tv3, Is_Gx1_Square);         --  21. x = CMOV (x, tv3, is_gx1_square)
      Y := CMov (Y, Y1, Is_Gx1_Square);          --  22. y = CMOV (y, y1, is_gx1_square)
      E1 := CT_EQ (Sgn0_F256 (U), Sgn0_F256 (Y));     --  23. e1 = sgn0 (u) == sgn0 (y)
      Y := CMov (Neg_F256 (Y), Y, E1);           --  24. y = CMOV (-y, y, e1)
      X := Mul_F256 (X, Inv_F256 (Tv4));         --  25. x = x / tv4
   end Map_To_Curve;

   procedure Map_Pair_To_Curve (P : out P256_Jacobian; U0, U1 : P256_FE) is
      Q1      : P256_Jacobian;
      Doubled : P256_Jacobian;
      Ret     : U32;
      Same    : U32;
   begin
      Map_To_Curve (P.X, P.Y, U0);
      P.Z := FE_One;
      Map_To_Curve (Q1.X, Q1.Y, U1);
      Q1.Z := FE_One;

      --  The mixed addition is wrong only for Q0 = Q1, which shows as
      --  r = 0 (Ret = 0) and h = 0 (Z = h * 1 = 0): take the doubling
      --  then, by mask. Q0 = -Q1 gives the identity (Z = 0, Ret = 1).
      Doubled := P;
      P256_Double (Doubled);
      P256_Add_Mixed (P, Q1, Ret);
      Same := (1 - CT_NEQ (Ret, 0)) and FE_Zero_Flag (P.Z);
      CT_Copy_Point (Same, P, Doubled);
      P256_To_Affine (P);
   end Map_Pair_To_Curve;

   procedure Reduce_Wide (U : out P256_FE; Src : Byte_Seq) is
      Half_Bytes : constant := Wide_Bytes / 2;  --  24 octets = 192 bits
      Field_Bytes : constant := 32;
      Pad_Bytes  : constant := Field_Bytes - Half_Bytes;
      High, Low  : Bytes_32 := (others => 0);
      FE_High, FE_Low : P256_FE;
   begin
      --  OS2IP (Src) = High * 2^192 + Low, both below 2^192 < p, so each
      --  converts exactly and the result is reduced by the field ops.
      High (Pad_Bytes .. High'Last) := Src (Src'First .. Src'First + Half_Bytes - 1);
      Low (Pad_Bytes .. Low'Last) := Src (Src'First + Half_Bytes .. Src'Last);
      Bytes_To_FE (FE_High, High);
      Bytes_To_FE (FE_Low, Low);
      U := Add_F256 (Mul_F256 (FE_High, FE_Two_192), FE_Low);
   end Reduce_Wide;

   procedure Expand_Message_XMD
     (Output : out Byte_Seq;
      Msg    : Byte_Seq;
      DST    : Byte_Seq)
   is
      use SPARKTLSCrypto.Hashing.SHA256;

      SHA256_Block_Bytes : constant := 64;
      Byte_Modulus       : constant := 256;

      Z_Pad      : constant Byte_Seq (0 .. SHA256_Block_Bytes - 1) := (others => 0);
      Length_BE  : constant Byte_Seq (0 .. 1) :=
        (Byte (N32 (Output'Length) / Byte_Modulus),
         Byte (N32 (Output'Length) mod Byte_Modulus));
      Zero_Octet : constant Byte_Seq (0 .. 0) := (0 => 0);
      DST_Length : constant Byte_Seq (0 .. 0) := (0 => Byte (DST'Length));
      Ell : constant N32 := (N32 (Output'Length) + SHA256_Bytes - 1) / SHA256_Bytes;

      Ctx     : Context;
      B_0     : Digest;
      B_I     : Digest;
      Chain   : Digest;
      Index   : Byte_Seq (0 .. 0) := (0 => 0);
      Offset  : N32;
      Count   : N32;
   begin
      Output := (others => 0);

      --  b_0 = H (Z_pad || msg || l_i_b_str || I2OSP (0, 1) || DST_prime)
      Init (Ctx);
      Update (Ctx, Z_Pad);
      Update (Ctx, Msg);
      Update (Ctx, Length_BE);
      Update (Ctx, Zero_Octet);
      Update (Ctx, DST);
      Update (Ctx, DST_Length);
      Final (Ctx, B_0);

      B_I := (others => 0);
      for I in 1 .. Ell loop
         pragma Warnings (Off, "condition is always True",
                          Reason => "kept as the proof's loop cut point");
         pragma Loop_Invariant (I <= Ell);
         pragma Warnings (On, "condition is always True");
         --  b_1 = H (b_0 || 1 || DST_prime);
         --  b_i = H (strxor (b_0, b_(i-1)) || i || DST_prime)
         for J in Digest'Range loop
            Chain (J) := B_0 (J) xor B_I (J);
         end loop;
         Index (0) := Byte (I);
         Init (Ctx);
         Update (Ctx, Chain);
         Update (Ctx, Index);
         Update (Ctx, DST);
         Update (Ctx, DST_Length);
         Final (Ctx, B_I);

         Offset := (I - 1) * SHA256_Bytes;
         Count := N32'Min (SHA256_Bytes, N32 (Output'Length) - Offset);
         Output (Output'First + Offset .. Output'First + Offset + Count - 1) :=
           B_I (0 .. Count - 1);
      end loop;
   end Expand_Message_XMD;

   procedure Hash_To_Curve
     (P   : out P256_Jacobian;
      Msg : Byte_Seq;
      DST : Byte_Seq)
   is
      --  count = 2 field elements of L = 48 octets each
      Uniform : Byte_Seq (0 .. 2 * Wide_Bytes - 1);
      U0, U1  : P256_FE;
   begin
      Expand_Message_XMD (Uniform, Msg, DST);
      Reduce_Wide (U0, Uniform (0 .. Wide_Bytes - 1));
      Reduce_Wide (U1, Uniform (Wide_Bytes .. Uniform'Last));
      Map_Pair_To_Curve (P, U0, U1);
   end Hash_To_Curve;

end WiFi_Crypto.P256_SSWU;
