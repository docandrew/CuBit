--  AES-CCM (NIST SP 800-38C; RFC 3610), generic over the AES block
--  cipher, with the parameters 802.11 CCMP uses (IEEE Std 802.11-2020
--  12.5.3): a 13-octet nonce, so a 2-octet length field (L = 2) and
--  payloads up to 65535 octets, and tags of 4 .. 16 octets (CCMP-128
--  uses 8, CCMP-256 uses 16). Additional data uses the 2-octet length
--  encoding, so it is shorter than 2^16 - 2^8 octets.
--
--  Building the CCMP nonce and AAD from an MPDU is the caller's job; this
--  unit is the CCM transform. The instances in WiFi_Crypto.AES_CCM
--  run over SPARKNaCl.AES (proved, table-free).
--
--  Proof: absence of run-time errors.
--
--  Timing: control flow depends on lengths only. Decrypt compares tags
--  without an early exit (SPARKNaCl.Equal) and clears the plaintext when
--  they differ, so no unauthenticated plaintext is released.


generic
   type Round_Keys is limited private;
   with procedure Cipher
     (Output : out Bytes_16; Input : Bytes_16; RK : Round_Keys);
package WiFi_Crypto.AES_CCM_G with
   SPARK_Mode => On
is
   Byte_Bits          : constant := 8;
   Block_Bytes        : constant := 16;
   Flags_Bytes        : constant := 1;
   Length_Field_Bytes : constant := 2;                  --  L
   Nonce_Bytes        : constant := Block_Bytes - Flags_Bytes - Length_Field_Bytes;
   Max_Payload_Bytes  : constant := 2 ** (Byte_Bits * Length_Field_Bytes) - 1;
   --  SP 800-38C A.2.2: the 2-octet encoding covers 0 < a < 2^16 - 2^8.
   AAD_Length_Bytes   : constant := 2;
   Max_AAD_Bytes      : constant :=
     2 ** (Byte_Bits * AAD_Length_Bytes) - 2 ** Byte_Bits - 1;

   subtype Nonce is Byte_Seq (0 .. Nonce_Bytes - 1);
   subtype Payload_Length is N32 range 0 .. Max_Payload_Bytes;
   subtype AAD_Length is N32 range 0 .. Max_AAD_Bytes;
   --  SP 800-38C A.1: Tlen is one of 32, 48, ..., 128 bits.
   subtype Tag_Length is N32 range 4 .. Block_Bytes
     with Static_Predicate => Tag_Length in 4 | 6 | 8 | 10 | 12 | 14 | 16;

   --  Ciphertext and Tag := CCM-Encrypt (K, N, AAD, Plaintext).
   procedure Encrypt
     (Ciphertext : out Byte_Seq;
      Tag        : out Byte_Seq;
      RK         : Round_Keys;
      N          : Nonce;
      AAD        : Byte_Seq;
      Plaintext  : Byte_Seq)
   with Global => null,
        Pre    => Plaintext'Length in Payload_Length
                  and then Ciphertext'Length = Plaintext'Length
                  and then AAD'Length in AAD_Length
                  and then Tag'Length in Tag_Length;

   --  Plaintext := CCM-Decrypt (K, N, AAD, Ciphertext, Tag). Valid is
   --  False, and Plaintext all zero, when the tag does not verify.
   procedure Decrypt
     (Plaintext  : out Byte_Seq;
      Valid      : out Boolean;
      RK         : Round_Keys;
      N          : Nonce;
      AAD        : Byte_Seq;
      Ciphertext : Byte_Seq;
      Tag        : Byte_Seq)
   with Global => null,
        Pre    => Ciphertext'Length in Payload_Length
                  and then Plaintext'Length = Ciphertext'Length
                  and then AAD'Length in AAD_Length
                  and then Tag'Length in Tag_Length;

end WiFi_Crypto.AES_CCM_G;
