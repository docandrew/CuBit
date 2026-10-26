--  AES-CMAC (RFC 4493; NIST SP 800-38B), generic over the AES block
--  cipher.
--
--  802.11 uses AES-128-CMAC as the EAPOL-Key MIC for AKMs 3-6 and 8
--  (key descriptor version 3, IEEE Std 802.11-2020 12.7.2) and as
--  BIP-CMAC-128 for protected management frames (12.5.4). The instances
--  in WiFi_Crypto.AES_CMAC run over SPARKNaCl.AES (proved, table-free).
--
--  Streaming: Start derives the subkeys, Absorb takes the message in any
--  number of parts (BIP MACs AAD || frame body without a copy), Finish
--  returns the full 16-byte tag; callers truncate (BIP: 8 bytes).
--
--  Proof: absence of run-time errors.
--
--  Timing: the subkey doubling uses a mask, not a branch, on the secret
--  top bit; all other control flow depends on lengths only.


generic
   type Round_Keys is limited private;
   with procedure Cipher
     (Output : out Bytes_16; Input : Bytes_16; RK : Round_Keys);
package WiFi_Crypto.AES_CMAC_G with
   SPARK_Mode => On
is
   Block_Bytes : constant := 16;
   subtype Tag is Bytes_16;

   type State is private;

   procedure Start (S : out State; RK : Round_Keys)
   with Global => null;

   procedure Absorb (S : in out State; RK : Round_Keys; Data : Byte_Seq)
   with Global => null;

   procedure Finish (S : in out State; RK : Round_Keys; Output : out Tag)
   with Global => null;

   --  Output := CMAC (K, M).
   procedure Compute (Output : out Tag; RK : Round_Keys; M : Byte_Seq)
   with Global => null;

private
   subtype Fill_Count is N32 range 0 .. Block_Bytes;

   --  The last complete block is held in Buffer until Finish, which XORs
   --  it with K1 (complete) or pads it and XORs K2 (partial or empty).
   type State is record
      K1, K2 : Bytes_16    := (others => 0);
      X      : Bytes_16    := (others => 0);
      Buffer : Bytes_16    := (others => 0);
      Fill   : Fill_Count  := 0;
   end record;
end WiFi_Crypto.AES_CMAC_G;
