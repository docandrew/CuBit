--  AES-CMAC (RFC 4493) over SPARKNaCl's proved, table-free AES block cipher, for
--  128- and 256-bit keys. Callers expand the key once with
--  SPARKNaCl.AES.Key_Expansion and pass the round keys.

with SPARKNaCl.AES;
with WiFi_Crypto.AES_CMAC_G;

package WiFi_Crypto.AES_CMAC with
   SPARK_Mode => On
is
   package AES128 is new WiFi_Crypto.AES_CMAC_G
     (Round_Keys => SPARKNaCl.AES.AES128_Round_Keys,
      Cipher     => SPARKNaCl.AES.Cipher);

   package AES256 is new WiFi_Crypto.AES_CMAC_G
     (Round_Keys => SPARKNaCl.AES.AES256_Round_Keys,
      Cipher     => SPARKNaCl.AES.Cipher);
end WiFi_Crypto.AES_CMAC;
