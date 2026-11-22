--  WiFi_Crypto.Self_Test -- known-answer tests run at driver start
--
--  One published vector per primitive family the driver composes, so a
--  miscompiled or mislinked crate shows up as a logged failure before any
--  key is derived (a power-on self-test, as FIPS 140 modules run):
--    * PMK: PBKDF2-HMAC-SHA1, IEEE Std 802.11-2020 J.4.2 ("password",
--      "IEEE"): SPARKTLSCrypto's SHA-1, HMAC_G, PBKDF2_G, Legacy_WPA2;
--    * CMAC: AES-CMAC, RFC 4493 4 example 2: SPARKNaCl's AES, AES_CMAC_G;
--    * XMD: expand_message_xmd (SHA-256), RFC 9380 K.1 ("abc", 32 bytes):
--      SPARKTLSCrypto's SHA-256 and P256_SSWU (whose body links the P-256
--      field and point units).
--  The full vector sets are tests/wifi-crypto (Linux-hosted).

package WiFi_Crypto.Self_Test with
   SPARK_Mode => On
is
   type KAT is (PMK, CMAC, XMD);
   type Outcomes is array (KAT) of Boolean;

   procedure Run (Passed : out Outcomes)
   with Global => null;

   function All_Passed (Passed : Outcomes) return Boolean is
     (for all Test in KAT => Passed (Test));

   function Name (Test : KAT) return String is
     (case Test is
        when PMK  => "pbkdf2-sha1-pmk",
        when CMAC => "aes-cmac",
        when XMD  => "xmd-sha256");

   function Verdict (Passed : Boolean) return String is
     (if Passed then "ok" else "fail");

   --  "pbkdf2-sha1-pmk=ok aes-cmac=ok xmd-sha256=ok", for log lines.
   function Image (Passed : Outcomes) return String is
     (Name (PMK) & "=" & Verdict (Passed (PMK)) & " "
      & Name (CMAC) & "=" & Verdict (Passed (CMAC)) & " "
      & Name (XMD) & "=" & Verdict (Passed (XMD)));
end WiFi_Crypto.Self_Test;
