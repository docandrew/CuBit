--  WiFi_Crypto: the 802.11 cryptography CuBit's Wi-Fi driver needs and
--  SPARKTLSCrypto, which stays FIPS-only, does not carry: HMAC over a
--  streaming hash, PBKDF2, the WPA2 legacy KDFs (HMAC-SHA1, PBKDF2-HMAC-
--  SHA1, the 802.11 PRF), AES key wrap, AES-CMAC, AES-CCM, and the P-256
--  field helpers and SSWU hash-to-curve for WPA3-SAE.
--
--  The primitives themselves are reused, never rewritten: SHA-1, SHA-256
--  and the P-256 Fiat-Crypto field and point arithmetic come from
--  SPARKTLSCrypto's public API, the AES block cipher and constant-time
--  comparison from SPARKNaCl. The units here are compositions of those,
--  each proved free of run-time errors at level 1 or 2
--  (tests/wifi-crypto/run.sh --prove).
--
--  These units began on a SPARKTLSCrypto branch (wcrypto, on top of
--  b89c8be) that was never merged; they moved here renamed, unchanged in
--  substance.

--  As in SPARKTLSCrypto's root, these context clauses serve the children:
--  N32, Byte_Seq and their operators are visible in every WiFi_Crypto unit.
pragma Warnings (Off, "no entities of ""Interfaces"" are referenced");
pragma Warnings (Off, "use clause for package ""Interfaces"" has no effect");
with Interfaces; use Interfaces;
pragma Warnings (On, "no entities of ""Interfaces"" are referenced");
pragma Warnings (On, "use clause for package ""Interfaces"" has no effect");
with SPARKNaCl;  use SPARKNaCl;

package WiFi_Crypto with
   Pure,
   SPARK_Mode => On
is
   --  SHA-1 and SHA-256 both take 64-byte input blocks (FIPS 180-4 5.1.1).
   SHA_Block_Bytes : constant N32 := 64;
   --  SPARKTLSCrypto's SHA-1 and SHA-256 Update accept Data when
   --  Data'Last < N32'Last - Hash_Update_Margin (their preconditions).
   Hash_Update_Margin : constant N32 := 128;

   SHA1_Digest_Bytes   : constant N32 := 20;
   SHA256_Digest_Bytes : constant N32 := 32;
end WiFi_Crypto;
