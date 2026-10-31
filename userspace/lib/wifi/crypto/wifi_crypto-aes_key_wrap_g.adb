--  AES key wrap (RFC 3394), generic over the AES block cipher (body).
--  The index-based form of RFC 3394 sections 2.2.1 and 2.2.2.

package body WiFi_Crypto.AES_Key_Wrap_G with
   SPARK_Mode => On
is
   --  RFC 3394 2.2.3.1 default initial value.
   Default_IV_Byte : constant Byte := 16#A6#;

   --  Each semiblock is enciphered six times (j = 0 .. 5).
   Rounds : constant := 6;

   subtype Semiblock_Index is N32 range 0 .. Semiblock_Bytes - 1;
   subtype Semiblock is Byte_Seq (Semiblock_Index);

   --  The two halves of a cipher block: MSB64 and LSB64.
   subtype Upper_Half is N32 range 0 .. Semiblock_Bytes - 1;
   subtype Lower_Half is N32 range Semiblock_Bytes .. 2 * Semiblock_Bytes - 1;

   Default_IV : constant Semiblock := (others => Default_IV_Byte);

   Byte_Bits     : constant := 8;

   --  A := A xor t, t = n * j + i as a 64-bit big-endian integer (at
   --  most 6 * Max_Semiblocks, so the modular U64 arithmetic is exact).
   procedure Xor_Step (A : in out Semiblock; T : U64) is
      V : U64 := T;
   begin
      for I in reverse Semiblock'Range loop
         A (I) := A (I) xor Byte (V mod 2 ** Byte_Bits);
         V := V / 2 ** Byte_Bits;
      end loop;
   end Xor_Step;

   procedure Wrap
     (Wrapped : out Byte_Seq;
      RK      : Round_Keys;
      Plain   : Byte_Seq)
   is
      N : constant Semiblock_Count := N32 (Plain'Length) / Semiblock_Bytes;
      A : Semiblock := Default_IV;
      B : Bytes_16;
      Input : Bytes_16 := (others => 0);
      Offset : N32;
      Last   : N32;
   begin
      pragma Assert (N * Semiblock_Bytes = N32 (Plain'Length));
      --  R[i] lives in Wrapped's semiblock i; A in semiblock 0.
      Wrapped := (others => 0);
      Wrapped (Wrapped'First + Semiblock_Bytes .. Wrapped'Last) := Plain;

      for J in 0 .. Rounds - 1 loop
         for I in 1 .. N loop
            Offset := I * Semiblock_Bytes;
            Last := Wrapped'First + (Offset + Semiblock_Bytes - 1);
            Input (Upper_Half) := A;
            Input (Lower_Half) := Wrapped (Wrapped'First + Offset .. Last);
            Cipher (B, Input, RK);
            A := B (Upper_Half);
            Xor_Step (A, U64 (N) * U64 (J) + U64 (I));
            Wrapped (Wrapped'First + Offset .. Last) := B (Lower_Half);
         end loop;
      end loop;

      Wrapped (Wrapped'First .. Wrapped'First + Semiblock_Bytes - 1) := A;
   end Wrap;

   procedure Unwrap
     (Plain   : out Byte_Seq;
      Valid   : out Boolean;
      RK      : Round_Keys;
      Wrapped : Byte_Seq)
   is
      N : constant Semiblock_Count := N32 (Plain'Length) / Semiblock_Bytes;
      A : Semiblock := Wrapped (Wrapped'First .. Wrapped'First + Semiblock_Bytes - 1);
      B : Bytes_16;
      Input : Bytes_16 := (others => 0);
      Offset : N32;
      Last   : N32;
   begin
      pragma Assert (N * Semiblock_Bytes = N32 (Plain'Length));
      --  R[i] lives in Plain's semiblock i - 1.
      Plain := Wrapped (Wrapped'First + Semiblock_Bytes .. Wrapped'Last);

      for J in reverse 0 .. Rounds - 1 loop
         for I in reverse 1 .. N loop
            Offset := (I - 1) * Semiblock_Bytes;
            Xor_Step (A, U64 (N) * U64 (J) + U64 (I));
            Last := Plain'First + (Offset + Semiblock_Bytes - 1);
            Input (Upper_Half) := A;
            Input (Lower_Half) := Plain (Plain'First + Offset .. Last);
            Inv_Cipher (B, Input, RK);
            A := B (Upper_Half);
            Plain (Plain'First + Offset .. Last) := B (Lower_Half);
         end loop;
      end loop;

      Valid := Equal (A, Default_IV);
      if not Valid then
         Plain := (others => 0);
      end if;
   end Unwrap;

end WiFi_Crypto.AES_Key_Wrap_G;
