--  HMAC-SHA-256 (RFC 2104 / RFC 4231) with the keyed-state and multi-part
--  interface of WiFi_Crypto.HMAC_G, over SPARKTLSCrypto's public streaming
--  SHA-256. 802.11 uses it for the SHA-256 AKMs (KDF-SHA-256 and the
--  EAPOL-Key MIC of AKMs 6 and 8, IEEE Std 802.11-2020 12.7.1.6.2) and
--  for SAE's KCK/PMK derivation (12.4.5.4). SPARKTLSCrypto.MAC.HMAC_SHA_256
--  covers the one-shot case; this instance adds keyed reuse without a
--  copy of the message parts.

pragma SPARK_Mode (On);
with SPARKTLSCrypto.Hashing.SHA256;
with WiFi_Crypto.HMAC_G;

package WiFi_Crypto.HMAC_SHA256 is new WiFi_Crypto.HMAC_G
  (Block_Bytes   => WiFi_Crypto.SHA_Block_Bytes,
   Digest_Bytes  => WiFi_Crypto.SHA256_Digest_Bytes,
   Update_Margin => WiFi_Crypto.Hash_Update_Margin,
   Context       => SPARKTLSCrypto.Hashing.SHA256.Context,
   Init          => SPARKTLSCrypto.Hashing.SHA256.Init,
   Update        => SPARKTLSCrypto.Hashing.SHA256.Update,
   Final         => SPARKTLSCrypto.Hashing.SHA256.Final);
