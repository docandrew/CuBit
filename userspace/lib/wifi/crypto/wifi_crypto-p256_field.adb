--  P-256 field operations (body). See the spec.

with SPARKTLSCrypto.Fiat_P256;

package body WiFi_Crypto.P256_Field with
   SPARK_Mode => On
is
   function Neg_F256 (A : P256_FE) return P256_FE is
     (SPARKTLSCrypto.Fiat_P256.Opp (A));

   function FE_Zero_Flag (A : P256_FE) return U32 is
      Half_Bits : constant := 32;
      Sign_Bit  : constant := 31;
      T : Unsigned_64;
      L : U32;
   begin
      SPARKTLSCrypto.Fiat_P256.Nonzero (T, A);
      --  T is 0 for zero and nonzero otherwise; fold to 32 bits, then
      --  L or -L has its top bit set exactly when L is nonzero.
      T := T or Shift_Right (T, Half_Bits);
      L := U32 (T and 16#FFFF_FFFF#);
      return 1 - Shift_Right (L or (0 - L), Sign_Bit);
   end FE_Zero_Flag;

   function FE_Equal (A, B : P256_FE) return U32 is
     (FE_Zero_Flag (Sub_F256 (A, B)));

   --  Modular inversion in GF(p): A^(p-2) mod p. Copied from P256_Inv in
   --  SPARKTLSCrypto's src/sparktlscrypto-p256-point.adb (crate commit
   --  b89c8be, private there), as a function.
   function Inv_F256 (A : P256_FE) return P256_FE is
      T1, T2 : P256_FE;
   begin
      --  Compute a^(2^31-1) via square-and-multiply
      T1 := A;
      for I in 0 .. 29 loop
         T1 := Square_F256 (T1);
         T1 := Mul_F256 (T1, A);
      end loop;

      --  Main exponentiation loop for p-2
      T2 := A;
      for I in 1 .. 255 loop
         T2 := Square_F256 (T2);
         case I is
            when 31 | 190 | 221 | 252 =>
               T2 := Mul_F256 (T2, T1);
            when 63 | 253 | 255 =>
               T2 := Mul_F256 (T2, A);
            when others =>
               null;
         end case;
      end loop;
      return T2;
   end Inv_F256;

   --  Repeated squaring: A^(2^N).
   function Square_Times (A : P256_FE; N : Positive) return P256_FE is
      R : P256_FE := A;
   begin
      for I in 1 .. N loop
         R := Square_F256 (R);
      end loop;
      return R;
   end Square_Times;

   --  (p-3)/4 = 2^254 - 2^222 + 2^190 + 2^94 - 1: in binary, 32 ones
   --  (bits 222 .. 253), a one at bit 190, and 94 ones (bits 0 .. 93).
   --  X_k below is A^(2^k - 1), a run of k ones; runs are joined by
   --  X_(j+k) = X_j^(2^k) * X_k. 254 squarings and 12 multiplications.
   function Pow_P_Minus_3_Div_4 (A : P256_FE) return P256_FE is
      Top_Run_Low : constant := 222;  --  lowest bit of the 32-one run
      Single_Bit  : constant := 190;  --  the lone one
      Low_Run     : constant := 94;   --  ones at bits 0 .. 93
      X2, X4, X8, X16, X32, X64, X80, X88, X92, X94, Acc : P256_FE;
   begin
      X2  := Mul_F256 (Square_F256 (A), A);
      X4  := Mul_F256 (Square_Times (X2, 2), X2);
      X8  := Mul_F256 (Square_Times (X4, 4), X4);
      X16 := Mul_F256 (Square_Times (X8, 8), X8);
      X32 := Mul_F256 (Square_Times (X16, 16), X16);
      X64 := Mul_F256 (Square_Times (X32, 32), X32);
      X80 := Mul_F256 (Square_Times (X64, 16), X16);
      X88 := Mul_F256 (Square_Times (X80, 8), X8);
      X92 := Mul_F256 (Square_Times (X88, 4), X4);
      X94 := Mul_F256 (Square_Times (X92, 2), X2);
      pragma Assert (64 + 16 + 8 + 4 + 2 = Low_Run);
      --  X32 shifted to sit just above the lone one, plus the lone one;
      --  then both shifted past the low run, plus the low run.
      Acc := Mul_F256 (Square_Times (X32, Top_Run_Low - Single_Bit), A);
      Acc := Mul_F256 (Square_Times (Acc, Single_Bit), X94);
      return Acc;
   end Pow_P_Minus_3_Div_4;

   procedure Sqrt_F256
     (R         : out P256_FE;
      Is_Square : out U32;
      A         : in  P256_FE)
   is
   begin
      --  A^((p-3)/4) * A = A^((p+1)/4)
      R := Mul_F256 (Pow_P_Minus_3_Div_4 (A), A);
      Is_Square := FE_Equal (Square_F256 (R), A);
   end Sqrt_F256;

   function Legendre_F256 (A : P256_FE) return Legendre_Symbol is
      T : constant P256_FE := Pow_P_Minus_3_Div_4 (A);
      --  (A^((p-3)/4))^2 * A = A^((p-1)/2)
      L : constant P256_FE := Mul_F256 (Square_F256 (T), A);
      Is_One       : constant U32 := FE_Equal (L, FE_One);
      Is_Minus_One : constant U32 := FE_Equal (L, Neg_F256 (FE_One));
   begin
      return I32 (Is_One) - I32 (Is_Minus_One);
   end Legendre_F256;

   function Sgn0_F256 (A : P256_FE) return U32 is
      Canonical : constant P256_FE :=
        SPARKTLSCrypto.Fiat_P256.From_Montgomery (A);
   begin
      return U32 (Canonical (0) and 1);
   end Sgn0_F256;

end WiFi_Crypto.P256_Field;
