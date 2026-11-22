--  WiFi_Crypto.Legacy_WPA2 -- HMAC-SHA-1 for WPA2 key derivation ONLY
--
--  SCOPE RESTRICTION. SPARKTLSCrypto.Hashing.SHA1 is scoped to OCSP
--  identifiers and forbids SHA-1 HMACs and KDFs in the TLS stack, which is
--  why this lives in the Wi-Fi driver and not in the (FIPS-only) crate.
--  It reuses the crate's public SHA-1 for one purpose: joining WPA2-Personal
--  networks (AKM 2, IEEE Std 802.11-2020 12.7), which mandate
--    * PBKDF2-HMAC-SHA1 to turn a passphrase into the PMK (J.4.1);
--    * the 802.11 PRF over HMAC-SHA1 to expand the PMK into the PTK
--      (12.7.1.2);
--    * HMAC-SHA1-128 as the EAPOL-Key MIC (key descriptor version 2).
--  Nothing else may use it: not TLS, not X.509, not WPA3 (which uses
--  SHA-256 and up), not any CuBit protocol. Delete it when WPA2 is
--  retired.
--
--  SHA-1's collision weakness does not carry over to HMAC or PBKDF2,
--  whose security rests on the compression function as a PRF.
--
--  No new algorithm code lives here: HMAC_SHA1 is an instance of the
--  proved WiFi_Crypto.HMAC_G over the existing SHA-1, PBKDF2_HMAC_SHA1
--  an instance of WiFi_Crypto.PBKDF2_G over that, and PRF a loop of
--  HMAC_SHA1 message parts.
--
--  Timing: control flow depends on lengths only (passphrase, SSID, label,
--  data, output), never on secret bytes.

with SPARKTLSCrypto.Hashing.SHA1;
with WiFi_Crypto.HMAC_G;
with WiFi_Crypto.PBKDF2_G;

package WiFi_Crypto.Legacy_WPA2 with
   SPARK_Mode => On
is
   package HMAC_SHA1 is new WiFi_Crypto.HMAC_G
     (Block_Bytes   => SHA_Block_Bytes,
      Digest_Bytes  => SHA1_Digest_Bytes,
      Update_Margin => Hash_Update_Margin,
      Context       => SPARKTLSCrypto.Hashing.SHA1.Context,
      Init          => SPARKTLSCrypto.Hashing.SHA1.Init,
      Update        => SPARKTLSCrypto.Hashing.SHA1.Update,
      Final         => SPARKTLSCrypto.Hashing.SHA1.Final);

   package PBKDF2_HMAC_SHA1 is new WiFi_Crypto.PBKDF2_G
     (Digest_Bytes  => SHA1_Digest_Bytes,
      Max_Data_Last => HMAC_SHA1.Max_Data_Last,
      Keyed_State   => HMAC_SHA1.Keyed_State,
      Message_State => HMAC_SHA1.Message_State,
      Set_Key       => HMAC_SHA1.Set_Key,
      Start         => HMAC_SHA1.Start,
      Absorb        => HMAC_SHA1.Absorb,
      Finish        => HMAC_SHA1.Finish);

   ---------------------------------------------------------------
   --  Passphrase to PMK (802.11-2020 J.4.1)
   ---------------------------------------------------------------

   --  The passphrase is 8 .. 63 ASCII characters (J.4.1); the SSID is
   --  1 .. 32 octets (9.4.2.2).
   subtype Passphrase_Length is N32 range 8 .. 63;
   subtype SSID_Length is N32 range 1 .. 32;

   PSK_Iterations : constant := 4096;
   PMK_Bytes      : constant := 32;
   subtype PMK is Byte_Seq (0 .. PMK_Bytes - 1);

   --  PMK := PBKDF2-HMAC-SHA1 (Passphrase, SSID, 4096, 256 bits)
   procedure Passphrase_To_PMK
     (Output     : out PMK;
      Passphrase : Byte_Seq;
      SSID       : Byte_Seq)
   with Global => null,
        Pre    => Passphrase'Length in Passphrase_Length
                  and then SSID'Length in SSID_Length
                  and then Passphrase'Last <= HMAC_SHA1.Max_Data_Last
                  and then SSID'Last <= HMAC_SHA1.Max_Data_Last;

   ---------------------------------------------------------------
   --  PRF-Len (802.11-2020 12.7.1.2)
   ---------------------------------------------------------------

   --  The counter X is one octet, so at most 256 SHA-1 outputs; 802.11
   --  asks for 128 .. 704 bits.
   PRF_Counter_Values : constant := 256;
   Max_PRF_Bytes : constant N32 := PRF_Counter_Values * SHA1_Digest_Bytes;
   subtype PRF_Length is N32 range 1 .. Max_PRF_Bytes;

   --  Output := PRF-Len (K, A, B), Len = 8 * Output'Length:
   --  the leading bytes of H (K, A, B, 0) || H (K, A, B, 1) || ...,
   --  where H (K, A, B, X) = HMAC-SHA1 (K, A || 0x00 || B || X).
   procedure PRF
     (Output : out Byte_Seq;
      K      : Byte_Seq;
      A      : Byte_Seq;
      B      : Byte_Seq)
   with Global => null,
        Pre    => Output'Length in PRF_Length
                  and then Output'Last < N32'Last - SHA1_Digest_Bytes
                  and then K'Last <= HMAC_SHA1.Max_Data_Last
                  and then A'Last <= HMAC_SHA1.Max_Data_Last
                  and then B'Last <= HMAC_SHA1.Max_Data_Last;

end WiFi_Crypto.Legacy_WPA2;
