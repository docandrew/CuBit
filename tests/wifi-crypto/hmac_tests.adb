with Interfaces; use Interfaces;
with SPARKNaCl; use SPARKNaCl;
with SPARKNaCl.MAC;
with SPARKTLSCrypto.Hashing.SHA256;
with WiFi_Crypto; use WiFi_Crypto;
with WiFi_Crypto.HMAC_SHA256;
with WiFi_Crypto.Legacy_WPA2; use WiFi_Crypto.Legacy_WPA2;
with SPARKTLSCrypto.MAC;
with Checks; use Checks;

package body HMAC_Tests is

   --  HMAC_SHA256 is WiFi_Crypto's own SHA-256 instance, checked against
   --  the crate's MAC.HMAC_SHA_256 (SHA-NI or software) and SPARKNaCl's.

   SHA1_Truncated_Bytes  : constant := 12;   --  RFC 2202 digest-96
   SHA256_Truncated_Bytes : constant := 16;  --  RFC 4231 case 5

   procedure HMAC_SHA1_Case
     (Name : String; Key, Data : Byte_Seq; Expected : String;
      Truncate : N32 := SHA1_Digest_Bytes)
   is
      Output : HMAC_SHA1.Digest;
   begin
      HMAC_SHA1.Compute (Output, Data, Key);
      Check ("RFC 2202 HMAC-SHA1 " & Name, Output (0 .. Truncate - 1), Hex (Expected));
   end HMAC_SHA1_Case;

   procedure HMAC_SHA256_Case
     (Name : String; Key, Data : Byte_Seq; Expected : String;
      Truncate : N32 := SHA256_Digest_Bytes)
   is
      Output : SPARKTLSCrypto.Hashing.SHA256.Digest;
      NaCl   : Bytes_32;
   begin
      SPARKTLSCrypto.MAC.HMAC_SHA_256 (Output, Data, Key);
      Check ("RFC 4231 MAC.HMAC_SHA_256 " & Name, Output (0 .. Truncate - 1),
             Hex (Expected));
      HMAC_SHA256.Compute (Output, Data, Key);
      Check ("RFC 4231 WiFi_Crypto.HMAC_SHA256 " & Name, Output (0 .. Truncate - 1),
             Hex (Expected));
      SPARKNaCl.MAC.HMAC_SHA_256 (NaCl, Data, Key);
      Check ("WiFi_Crypto.HMAC_SHA256 = SPARKNaCl " & Name, Output, NaCl);
   end HMAC_SHA256_Case;

   procedure PBKDF2_Case
     (Name : String; P, S : String; C : PBKDF2_HMAC_SHA1.Iteration_Count;
      Expected : String)
   is
      E  : constant Byte_Seq := Hex (Expected);
      DK : Byte_Seq (0 .. E'Last);
   begin
      PBKDF2_HMAC_SHA1.Derive (DK, Text (P), Text (S), C);
      Check ("RFC 6070 PBKDF2-HMAC-SHA1 " & Name, DK, E);
   end PBKDF2_Case;

   procedure PMK_Case (Passphrase, SSID, Expected : String) is
      Output : PMK;
   begin
      Passphrase_To_PMK (Output, Text (Passphrase), Text (SSID));
      Check ("802.11 J.4 passphrase " & Passphrase & " / " & SSID, Output,
             Hex (Expected));
   end PMK_Case;

   PRF_512_Bytes : constant := 64;
   PRF_Label : constant String := "prefix";

   procedure PRF_Case (Name : String; Key, Data : Byte_Seq; Expected : String) is
      Output : Byte_Seq (0 .. PRF_512_Bytes - 1);
   begin
      PRF (Output, Key, Text (PRF_Label), Data);
      Check ("802.11 J.3 PRF-512 " & Name, Output, Hex (Expected));
   end PRF_Case;

   --  Streaming in parts equals one-shot over the concatenation.
   procedure Streaming_Case is
      Key  : constant Byte_Seq := Text ("key");
      Part1 : constant Byte_Seq := Text ("The quick brown fox ");
      Part2 : constant Byte_Seq := Text ("jumps over the lazy dog");
      KS : HMAC_SHA1.Keyed_State;
      MS : HMAC_SHA1.Message_State;
      Streamed, One_Shot : HMAC_SHA1.Digest;
   begin
      HMAC_SHA1.Set_Key (KS, Key);
      HMAC_SHA1.Start (MS, KS);
      HMAC_SHA1.Absorb (MS, Part1);
      HMAC_SHA1.Absorb (MS, Part2);
      HMAC_SHA1.Finish (MS, Streamed);
      HMAC_SHA1.Compute (One_Shot, Part1 & Part2, Key);
      Check ("HMAC-SHA1 streamed = one-shot", Streamed, One_Shot);
      --  Wikipedia / widely published value for this message and key.
      Check ("HMAC-SHA1 quick brown fox", One_Shot,
             Hex ("de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9"));
   end Streaming_Case;

   procedure Run is
      Long_Key_Bytes : constant := 80;
   begin
      --  RFC 2202 section 3
      HMAC_SHA1_Case ("1", Repeat (16#0B#, 20), Text ("Hi There"),
                      "b617318655057264e28bc0b6fb378c8ef146be00");
      HMAC_SHA1_Case ("2", Text ("Jefe"), Text ("what do ya want for nothing?"),
                      "effcdf6ae5eb2fa2d27416d5f184df9c259a7c79");
      HMAC_SHA1_Case ("3", Repeat (16#AA#, 20), Repeat (16#DD#, 50),
                      "125d7342b9ac11cd91a39af48aa17b4f63f175d3");
      HMAC_SHA1_Case ("4", Hex ("0102030405060708090a0b0c0d0e0f10111213141516171819"),
                      Repeat (16#CD#, 50),
                      "4c9007f4026250c6bc8414f9bf50c86c2d7235da");
      HMAC_SHA1_Case ("5", Repeat (16#0C#, 20), Text ("Test With Truncation"),
                      "4c1a03424b55e07fe7f27be1d58bb9324a9a5a04");
      HMAC_SHA1_Case ("5 digest-96", Repeat (16#0C#, 20), Text ("Test With Truncation"),
                      "4c1a03424b55e07fe7f27be1", SHA1_Truncated_Bytes);
      HMAC_SHA1_Case ("6", Repeat (16#AA#, Long_Key_Bytes),
                      Text ("Test Using Larger Than Block-Size Key - Hash Key First"),
                      "aa4ae5e15272d00e95705637ce8a3b55ed402112");
      HMAC_SHA1_Case ("7", Repeat (16#AA#, Long_Key_Bytes),
                      Text ("Test Using Larger Than Block-Size Key and Larger "
                            & "Than One Block-Size Data"),
                      "e8e99d0f45237d786d6bbaa7965c7808bbff1a91");
      Streaming_Case;

      --  RFC 4231 section 4 (HMAC-SHA-256): the crate's MAC.HMAC_SHA_256
      --  and WiFi_Crypto.HMAC_SHA256.
      HMAC_SHA256_Case ("1", Repeat (16#0B#, 20), Text ("Hi There"),
        "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
      HMAC_SHA256_Case ("2", Text ("Jefe"), Text ("what do ya want for nothing?"),
        "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843");
      HMAC_SHA256_Case ("3", Repeat (16#AA#, 20), Repeat (16#DD#, 50),
        "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe");
      HMAC_SHA256_Case ("4", Hex ("0102030405060708090a0b0c0d0e0f10111213141516171819"),
        Repeat (16#CD#, 50),
        "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b");
      HMAC_SHA256_Case ("5", Repeat (16#0C#, 20), Text ("Test With Truncation"),
        "a3b6167473100ee06e0c796c2955552b", SHA256_Truncated_Bytes);
      HMAC_SHA256_Case ("6", Repeat (16#AA#, 131),
        Text ("Test Using Larger Than Block-Size Key - Hash Key First"),
        "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54");
      HMAC_SHA256_Case ("7", Repeat (16#AA#, 131),
        Text ("This is a test using a larger than block-size key and a larger "
              & "than block-size data. The key needs to be hashed before being "
              & "used by the HMAC algorithm."),
        "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2");

      --  RFC 6070 section 2
      PBKDF2_Case ("c=1", "password", "salt", 1,
                   "0c60c80f961f0e71f3a9b524af6012062fe037a6");
      PBKDF2_Case ("c=2", "password", "salt", 2,
                   "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957");
      PBKDF2_Case ("c=4096", "password", "salt", 4096,
                   "4b007901b765489abead49d926f721d065a429c1");
      PBKDF2_Case ("c=16777216", "password", "salt", 16_777_216,
                   "eefe3d61cd4da4e4e9945b3d6ba2158c2634e984");
      PBKDF2_Case ("dkLen=25", "passwordPASSWORDpassword",
                   "saltSALTsaltSALTsaltSALTsaltSALTsalt", 4096,
                   "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038");
      PBKDF2_Case ("NUL", "pass" & ASCII.NUL & "word", "sa" & ASCII.NUL & "lt",
                   4096, "56fa6aa75548099dcc37d7f03425e0c3");

      --  IEEE Std 802.11-2020 J.4 (passphrase to PSK / PMK)
      PMK_Case ("password", "IEEE",
        "f42c6fc52df0ebef9ebb4b90b38a5f902e83fe1b135a70e23aed762e9710a12e");
      PMK_Case ("ThisIsAPassword", "ThisIsASSID",
        "0dc0d6eb90555ed6419756b9a15ec3e3209b63df707dd508d14581f8982721af");
      PMK_Case ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ",
        "becb93866bb8c3832cb777c2f559807c8c59afcb6eae734885001300a981cc62");

      --  IEEE Std 802.11-2020 J.3 (PRF, label "prefix")
      PRF_Case ("1", Repeat (16#0B#, 20), Text ("Hi There"),
        "bcd4c650b30b9684951829e0d75f9d54b862175ed9f00606e17d8da35402ffee"
        & "75df78c3d31e0f889f012120c0862beb67753e7439ae242edb8373698356cf5a");
      PRF_Case ("2", Text ("Jefe"), Text ("what do ya want for nothing?"),
        "51f4de5b33f249adf81aeb713a3c20f4fe631446fabdfa58244759ae58ef9009"
        & "a99abf4eac2ca5fa87e692c440eb40023e7babb206d61de7b92f41529092b8fc");
      PRF_Case ("3", Repeat (16#AA#, 20), Repeat (16#DD#, 50),
        "e1ac546ec4cb636f9976487be5c86be17a0252ca5d8d8df12cfb0473525249ce"
        & "9dd8d177ead710bc9b590547239107aef7b4abd43d87f0a68f1cbd9e2b6f7607");
   end Run;
end HMAC_Tests;
