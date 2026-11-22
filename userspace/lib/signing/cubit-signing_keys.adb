-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
with SPARKTLSCrypto.P384.ECDSA;
with SPARKTLSCrypto.RFC6979;

package body CuBit.Signing_Keys with SPARK_Mode => On is
   use type SPARKNaCl.Byte_Seq;

   function At_Most (A, B : Scalar) return Boolean is
   begin
      for I in Scalar'Range loop
         pragma Loop_Invariant (for all J in 0 .. I - 1 => A (J) = B (J));
         if A (I) /= B (I) then
            return A (I) < B (I);
         end if;
      end loop;
      return True;
   end At_Most;

   procedure Public_Of (D : Scalar; Mask : Blind; Q : out Point) is
   begin
      SPARKTLSCrypto.P384.Point.P384_Mulgen_Blinded (PK_Out => Q, SK => D, Blind => Mask);
   end Public_Of;

   --  The digest the pairwise test signs: SHA-384 ("abc"), FIPS 180-4's
   --  example, so the value is recognisable in a debugger.
   Test_Digest : constant SPARKNaCl.Bytes_48 :=
     [16#CB#, 16#00#, 16#75#, 16#3F#, 16#45#, 16#A3#, 16#5E#, 16#8B#,
      16#B5#, 16#A0#, 16#3D#, 16#69#, 16#9A#, 16#C6#, 16#50#, 16#07#,
      16#27#, 16#2C#, 16#32#, 16#AB#, 16#0E#, 16#DE#, 16#D1#, 16#63#,
      16#1A#, 16#8B#, 16#60#, 16#5A#, 16#43#, 16#FF#, 16#5B#, 16#ED#,
      16#80#, 16#86#, 16#07#, 16#2B#, 16#A1#, 16#E7#, 16#CC#, 16#23#,
      16#58#, 16#BA#, 16#EC#, 16#A1#, 16#34#, 16#C8#, 16#25#, 16#A7#];

   procedure Pairwise_Consistent (D : Scalar; Q : Point; Mask : Blind; OK : out Boolean) is
      K : SPARKNaCl.Bytes_48;
      R, S : SPARKNaCl.Byte_Seq (0 .. Scalar_Length - 1) := [others => 0];
      Qx, Qy : SPARKNaCl.Byte_Seq (0 .. Scalar_Length - 1);
      Nonce_OK, Signed : Boolean;
   begin
      OK := False;
      if Q (0) /= 16#04# then
         return;
      end if;
      SPARKTLSCrypto.RFC6979.Derive_K_P384 (D, Test_Digest, K, Nonce_OK);
      if not Nonce_OK then
         return;
      end if;
      SPARKTLSCrypto.P384.ECDSA.Sign (Test_Digest, D, K, Mask, R, S, Signed);
      if not Signed then
         return;
      end if;
      Qx := Q (1 .. Scalar_Length);
      Qy := Q (Scalar_Length + 1 .. 2 * Scalar_Length);
      OK := SPARKTLSCrypto.P384.ECDSA.Verify (Test_Digest, Qx, Qy, R, S);
   end Pairwise_Consistent;

   procedure Check_Pair (D : Scalar; Q : Point; Mask : Blind; OK : out Boolean) is
      Computed : Point;
   begin
      OK := False;
      if not Strong (D) then
         return;
      end if;
      Public_Of (D, Mask, Computed);
      if Computed /= Q then
         return;
      end if;
      Pairwise_Consistent (D, Q, Mask, OK);
   end Check_Pair;

   Head : constant SPARKNaCl.Byte_Seq (0 .. 7) :=
     [16#30#, 16#81#, 16#A4#, 16#02#, 16#01#, 16#01#, 16#04#, 16#30#];
   Middle : constant SPARKNaCl.Byte_Seq (0 .. 13) :=
     [16#A0#, 16#07#, 16#06#, 16#05#, 16#2B#, 16#81#, 16#04#, 16#00#, 16#22#,
      16#A1#, 16#64#, 16#03#, 16#62#, 16#00#];
   Scalar_At : constant := Head'Length;
   Middle_At : constant := Scalar_At + Scalar_Length;
   Point_At  : constant := Middle_At + Middle'Length;
   pragma Assert (Point_At + Point_Length = Key_File_Length);

   procedure Encode_Key (D : Scalar; Q : Point; Encoded : out Key_File) is
   begin
      Encoded := [others => 0];
      Encoded (0 .. Scalar_At - 1) := Head;
      Encoded (Scalar_At .. Middle_At - 1) := D;
      Encoded (Middle_At .. Point_At - 1) := Middle;
      Encoded (Point_At .. Key_File_Length - 1) := Q;
   end Encode_Key;

   procedure Decode_Key (Encoded : SPARKNaCl.Byte_Seq; D : out Scalar; Q : out Point; OK : out Boolean) is
   begin
      D := [others => 0];
      Q := [others => 0];
      OK := False;
      if Encoded'Length /= Key_File_Length
        or else Encoded (0 .. Scalar_At - 1) /= Head
        or else Encoded (Middle_At .. Point_At - 1) /= Middle
      then
         return;
      end if;
      D := Encoded (Scalar_At .. Middle_At - 1);
      Q := Encoded (Point_At .. Key_File_Length - 1);
      OK := Strong (D);
      if not OK then
         D := [others => 0];
      end if;
   end Decode_Key;

end CuBit.Signing_Keys;
