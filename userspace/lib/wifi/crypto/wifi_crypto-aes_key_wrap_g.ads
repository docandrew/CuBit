--  AES key wrap (RFC 3394; NIST SP 800-38F KW), generic over the AES
--  block cipher.
--
--  802.11 uses it to carry the GTK / IGTK in EAPOL-Key frames under the
--  KEK (IEEE Std 802.11-2020 12.7.2, key descriptor versions 2 and 3).
--  The instances in WiFi_Crypto.AES_Key_Wrap run over SPARKNaCl.AES
--  (proved, table-free).
--
--  Proof: absence of run-time errors. Unwrap (Wrap (P)) = P is tested, not
--  proved (it rests on Inv_Cipher inverting Cipher, which SPARKNaCl does
--  not state).
--
--  Timing: control flow depends on the data length only. Unwrap compares
--  the integrity check value without an early exit and clears the output
--  on failure, so a forged input learns nothing beyond "invalid".


generic
   type Round_Keys is limited private;
   with procedure Cipher
     (Output : out Bytes_16; Input : Bytes_16; RK : Round_Keys);
   with procedure Inv_Cipher
     (Output : out Bytes_16; Input : Bytes_16; RK : Round_Keys);
package WiFi_Crypto.AES_Key_Wrap_G with
   SPARK_Mode => On
is
   Semiblock_Bytes : constant := 8;

   --  RFC 3394 needs n >= 2 semiblocks of plaintext; 64 KiB is far above
   --  any 802.11 Key Data field.
   Max_Semiblocks : constant := 8192;
   subtype Semiblock_Count is N32 range 2 .. Max_Semiblocks;

   Max_Plain_Bytes : constant := Max_Semiblocks * Semiblock_Bytes;

   --  Plain must be a whole number (2 .. Max_Semiblocks) of semiblocks.
   Min_Plain_Bytes : constant := Semiblock_Count'First * Semiblock_Bytes;

   --  Wrapped := KW-AE (K, Plain); one semiblock longer than Plain.
   procedure Wrap
     (Wrapped : out Byte_Seq;
      RK      : Round_Keys;
      Plain   : Byte_Seq)
   with Global => null,
        Pre    => Plain'Length in Min_Plain_Bytes .. Max_Plain_Bytes
                  and then Plain'Length mod Semiblock_Bytes = 0
                  and then Wrapped'Length = Plain'Length + Semiblock_Bytes;

   --  Plain := KW-AD (K, Wrapped). Valid is False, and Plain all zero,
   --  when the integrity check fails.
   procedure Unwrap
     (Plain   : out Byte_Seq;
      Valid   : out Boolean;
      RK      : Round_Keys;
      Wrapped : Byte_Seq)
   with Global => null,
        Pre    => Plain'Length in Min_Plain_Bytes .. Max_Plain_Bytes
                  and then Plain'Length mod Semiblock_Bytes = 0
                  and then Wrapped'Length = Plain'Length + Semiblock_Bytes;

end WiFi_Crypto.AES_Key_Wrap_G;
