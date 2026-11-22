with Interfaces; use Interfaces;
with SPARKNaCl; use SPARKNaCl;
with SPARKNaCl.AES; use SPARKNaCl.AES;
with WiFi_Crypto.AES_CCM;
with WiFi_Crypto.AES_CMAC;
with WiFi_Crypto.AES_Key_Wrap;
with Checks; use Checks;

package body AES_Tests is
   package KW128 renames WiFi_Crypto.AES_Key_Wrap.AES128;
   package KW256 renames WiFi_Crypto.AES_Key_Wrap.AES256;
   package CMAC128 renames WiFi_Crypto.AES_CMAC.AES128;
   package CMAC256 renames WiFi_Crypto.AES_CMAC.AES256;
   package CCM128 renames WiFi_Crypto.AES_CCM.AES128;
   package CCM256 renames WiFi_Crypto.AES_CCM.AES256;

   Semiblock_Bytes : constant := KW128.Semiblock_Bytes;

   function Flip_First (B : Byte_Seq) return Byte_Seq is
      R : Byte_Seq := B;
   begin
      R (R'First) := R (R'First) xor 1;
      return R;
   end Flip_First;

   function All_Zero (B : Byte_Seq) return Boolean is
     (for all X of B => X = 0);

   ---------------------------------------------------------------
   --  AES key wrap
   ---------------------------------------------------------------

   procedure KW128_Case (Name, KEK, Plain, Wrapped : String) is
      RK : constant AES128_Round_Keys := Key_Expansion (Construct (Hex (KEK)));
      P  : constant Byte_Seq := Hex (Plain);
      W  : Byte_Seq (0 .. P'Last + Semiblock_Bytes);
      U  : Byte_Seq (P'Range);
      Valid : Boolean;
   begin
      KW128.Wrap (W, RK, P);
      Check ("RFC 3394 " & Name & " wrap", W, Hex (Wrapped));
      KW128.Unwrap (U, Valid, RK, Hex (Wrapped));
      Check ("RFC 3394 " & Name & " unwrap valid", Valid);
      Check ("RFC 3394 " & Name & " unwrap", U, P);
      KW128.Unwrap (U, Valid, RK, Flip_First (Hex (Wrapped)));
      Check ("RFC 3394 " & Name & " tampered rejected", not Valid and All_Zero (U));
   end KW128_Case;

   procedure KW256_Case (Name, KEK, Plain, Wrapped : String) is
      RK : constant AES256_Round_Keys := Key_Expansion (Construct (Hex (KEK)));
      P  : constant Byte_Seq := Hex (Plain);
      W  : Byte_Seq (0 .. P'Last + Semiblock_Bytes);
      U  : Byte_Seq (P'Range);
      Valid : Boolean;
   begin
      KW256.Wrap (W, RK, P);
      Check ("RFC 3394 " & Name & " wrap", W, Hex (Wrapped));
      KW256.Unwrap (U, Valid, RK, Hex (Wrapped));
      Check ("RFC 3394 " & Name & " unwrap valid", Valid);
      Check ("RFC 3394 " & Name & " unwrap", U, P);
      KW256.Unwrap (U, Valid, RK, Flip_First (Hex (Wrapped)));
      Check ("RFC 3394 " & Name & " tampered rejected", not Valid and All_Zero (U));
   end KW256_Case;

   ---------------------------------------------------------------
   --  AES-CMAC
   ---------------------------------------------------------------

   --  RFC 4493 4 / NIST SP 800-38B D.1 (AES-128) and D.3 (AES-256) message.
   CMAC_Message : constant String :=
     "6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e51"
     & "30c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710";

   procedure CMAC128_Case (Name : String; Length : Natural; Expected : String) is
      RK : constant AES128_Round_Keys :=
        Key_Expansion (Construct (Hex ("2b7e151628aed2a6abf7158809cf4f3c")));
      M  : constant Byte_Seq := Hex (CMAC_Message);
      T  : CMAC128.Tag;
      S  : CMAC128.State;
      Split : constant N32 := N32 (Length / 3);
   begin
      CMAC128.Compute (T, RK, M (0 .. N32 (Length) - 1));
      Check ("RFC 4493 AES-CMAC " & Name, T, Hex (Expected));
      --  The same message in three parts.
      CMAC128.Start (S, RK);
      CMAC128.Absorb (S, RK, M (0 .. Split - 1));
      CMAC128.Absorb (S, RK, M (Split .. 2 * Split - 1));
      CMAC128.Absorb (S, RK, M (2 * Split .. N32 (Length) - 1));
      CMAC128.Finish (S, RK, T);
      Check ("RFC 4493 AES-CMAC " & Name & " streamed", T, Hex (Expected));
   end CMAC128_Case;

   procedure CMAC256_Case (Name : String; Length : Natural; Expected : String) is
      RK : constant AES256_Round_Keys :=
        Key_Expansion (Construct (Hex
          ("603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4")));
      M  : constant Byte_Seq := Hex (CMAC_Message);
      T  : CMAC256.Tag;
   begin
      CMAC256.Compute (T, RK, M (0 .. N32 (Length) - 1));
      Check ("SP 800-38B D.3 AES-256-CMAC " & Name, T, Hex (Expected));
   end CMAC256_Case;

   ---------------------------------------------------------------
   --  AES-CCM
   ---------------------------------------------------------------

   procedure CCM_Case (Name, Key, Nonce, AAD, Plain, Cipher, Tag : String) is
      RK : constant AES128_Round_Keys := Key_Expansion (Construct (Hex (Key)));
      N  : constant CCM128.Nonce := Hex (Nonce);
      P  : constant Byte_Seq := Hex (Plain);
      E_Tag : constant Byte_Seq := Hex (Tag);
      C  : Byte_Seq (P'Range);
      T  : Byte_Seq (E_Tag'Range);
      D  : Byte_Seq (P'Range);
      Valid : Boolean;
   begin
      CCM128.Encrypt (C, T, RK, N, Hex (AAD), P);
      Check (Name & " ciphertext", C, Hex (Cipher));
      Check (Name & " tag", T, E_Tag);
      CCM128.Decrypt (D, Valid, RK, N, Hex (AAD), Hex (Cipher), E_Tag);
      Check (Name & " decrypt valid", Valid);
      Check (Name & " decrypt", D, P);
      CCM128.Decrypt (D, Valid, RK, N, Hex (AAD), Hex (Cipher), Flip_First (E_Tag));
      Check (Name & " forged tag rejected", not Valid and All_Zero (D));
      if P'Length > 0 then
         CCM128.Decrypt (D, Valid, RK, N, Hex (AAD), Flip_First (Hex (Cipher)), E_Tag);
         Check (Name & " forged ciphertext rejected", not Valid and All_Zero (D));
      end if;
   end CCM_Case;

   procedure CCM256_Case (Name, Key, Nonce, AAD, Plain, Cipher, Tag : String) is
      RK : constant AES256_Round_Keys := Key_Expansion (Construct (Hex (Key)));
      N  : constant CCM256.Nonce := Hex (Nonce);
      P  : constant Byte_Seq := Hex (Plain);
      E_Tag : constant Byte_Seq := Hex (Tag);
      C  : Byte_Seq (P'Range);
      T  : Byte_Seq (E_Tag'Range);
      D  : Byte_Seq (P'Range);
      Valid : Boolean;
   begin
      CCM256.Encrypt (C, T, RK, N, Hex (AAD), P);
      Check (Name & " ciphertext", C, Hex (Cipher));
      Check (Name & " tag", T, E_Tag);
      CCM256.Decrypt (D, Valid, RK, N, Hex (AAD), Hex (Cipher), E_Tag);
      Check (Name & " decrypt", Valid and then D = P);
   end CCM256_Case;

   function Count_Bytes (First, Count : Natural) return String is
      Digit : constant String := "0123456789abcdef";
      R : String (1 .. 2 * Count);
   begin
      for I in 0 .. Count - 1 loop
         R (2 * I + 1) := Digit ((First + I) / 16 mod 16 + 1);
         R (2 * I + 2) := Digit ((First + I) mod 16 + 1);
      end loop;
      return R;
   end Count_Bytes;

   procedure Run is
   begin
      --  RFC 3394 section 4 (the 192-bit KEK cases need AES-192, which
      --  SPARKNaCl does not provide and 802.11 does not use).
      KW128_Case ("4.1 128-bit KEK", "000102030405060708090a0b0c0d0e0f",
                  "00112233445566778899aabbccddeeff",
                  "1fa68b0a8112b447aef34bd8fb5a7b829d3e862371d2cfe5");
      KW256_Case ("4.3 256-bit KEK, 128-bit data",
                  "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
                  "00112233445566778899aabbccddeeff",
                  "64e8c3f9ce0f5ba263e9777905818a2a93c8191e7d6e8ae7");
      KW256_Case ("4.5 256-bit KEK, 192-bit data",
                  "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
                  "00112233445566778899aabbccddeeff0001020304050607",
                  "a8f9bc1612c68b3ff6e6f4fbe30e71e4769c8b80a32cb8958cd5d17d6b254da1");
      KW256_Case ("4.6 256-bit KEK, 256-bit data",
                  "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
                  "00112233445566778899aabbccddeeff000102030405060708090a0b0c0d0e0f",
                  "28c9f404c4b810f4cbccb35cfb87f8263f5786e2d80ed326"
                  & "cbc7f0e71a99f43bfb988b9b7a02dd21");

      --  RFC 4493 section 4
      CMAC128_Case ("example 1 (len 0)", 0, "bb1d6929e95937287fa37d129b756746");
      CMAC128_Case ("example 2 (len 16)", 16, "070a16b46b4d4144f79bdd9dd04a287c");
      CMAC128_Case ("example 3 (len 40)", 40, "dfa66747de9ae63030ca32611497c827");
      CMAC128_Case ("example 4 (len 64)", 64, "51f0bebf7e3b9d92fc49741779363cfe");
      --  NIST SP 800-38B appendix D.3 (AES-256)
      CMAC256_Case ("example 9 (len 0)", 0, "028962f61b7bf89efc6b551f4667d983");
      CMAC256_Case ("example 10 (len 16)", 16, "28a7023f452e8f82bd4bf28d8c37c35c");
      CMAC256_Case ("example 11 (len 40)", 40, "aaf3d8f1de5640c232f5b169b9c911e6");
      CMAC256_Case ("example 12 (len 64)", 64, "e1992190549f6ed5696a2c056c315410");

      --  RFC 3610 section 8 (all 24 packet vectors), then the CCMP MPDU
      --  vectors: AAD and nonce are the muted header and CCM nonce the
      --  standard lists for each MPDU.
      CCM_Case ("RFC 3610 packet vector #1",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000003020100a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e",
        Cipher => "588c979a61c663d2f066d0c2c0f989806d5f6b61dac384",
        Tag => "17e8d12cfdf926e0");
      CCM_Case ("RFC 3610 packet vector #2",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000004030201a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
        Cipher => "72c91a36e135f8cf291ca894085c87e3cc15c439c9e43a3b",
        Tag => "a091d56e10400916");
      CCM_Case ("RFC 3610 packet vector #3",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000005040302a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
        Cipher => "51b1e5f44a197d1da46b0f8e2d282ae871e838bb64da859657",
        Tag => "4adaa76fbd9fb0c5");
      CCM_Case ("RFC 3610 packet vector #4",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000006050403a0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e",
        Cipher => "a28c6865939a9a79faaa5c4c2a9d4a91cdac8c",
        Tag => "96c861b9c9e61ef1");
      CCM_Case ("RFC 3610 packet vector #5",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000007060504a0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e1f",
        Cipher => "dcf1fb7b5d9e23fb9d4e131253658ad86ebdca3e",
        Tag => "51e83f077d9c2d93");
      CCM_Case ("RFC 3610 packet vector #6",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000008070605a0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e1f20",
        Cipher => "6fc1b011f006568b5171a42d953d469b2570a4bd87",
        Tag => "405a0443ac91cb94");
      CCM_Case ("RFC 3610 packet vector #7",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00000009080706a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e",
        Cipher => "0135d1b2c95f41d5d1d4fec185d166b8094e999dfed96c",
        Tag => "048c56602c97acbb7490");
      CCM_Case ("RFC 3610 packet vector #8",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "0000000a090807a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
        Cipher => "7b75399ac0831dd2f0bbd75879a2fd8f6cae6b6cd9b7db24",
        Tag => "c17b4433f434963f34b4");
      CCM_Case ("RFC 3610 packet vector #9",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "0000000b0a0908a0a1a2a3a4a5",
        AAD => "0001020304050607",
        Plain => "08090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20",
        Cipher => "82531a60cc24945a4b8279181ab5c84df21ce7f9b73f42e197",
        Tag => "ea9c07e56b5eb17e5f4e");
      CCM_Case ("RFC 3610 packet vector #10",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "0000000c0b0a09a0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e",
        Cipher => "07342594157785152b074098330abb141b947b",
        Tag => "566aa9406b4d999988dd");
      CCM_Case ("RFC 3610 packet vector #11",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "0000000d0c0b0aa0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e1f",
        Cipher => "676bb20380b0e301e8ab79590a396da78b834934",
        Tag => "f53aa2e9107a8b6c022c");
      CCM_Case ("RFC 3610 packet vector #12",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "0000000e0d0c0ba0a1a2a3a4a5",
        AAD => "000102030405060708090a0b",
        Plain => "0c0d0e0f101112131415161718191a1b1c1d1e1f20",
        Cipher => "c0ffa0d6f05bdb67f24d43a4338d2aa4bed7b20e43",
        Tag => "cd1aa31662e7ad65d6db");
      CCM_Case ("RFC 3610 packet vector #13",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00412b4ea9cdbe3c9696766cfa",
        AAD => "0be1a88bace018b1",
        Plain => "08e8cf97d820ea258460e96ad9cf5289054d895ceac47c",
        Cipher => "4cb97f86a2a4689a877947ab8091ef5386a6ffbdd080f8",
        Tag => "e78cf7cb0cddd7b3");
      CCM_Case ("RFC 3610 packet vector #14",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "0033568ef7b2633c9696766cfa",
        AAD => "63018f76dc8a1bcb",
        Plain => "9020ea6f91bdd85afa0039ba4baff9bfb79c7028949cd0ec",
        Cipher => "4ccb1e7ca981befaa0726c55d378061298c85c92814abc33",
        Tag => "c52ee81d7d77c08a");
      CCM_Case ("RFC 3610 packet vector #15",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00103fe41336713c9696766cfa",
        AAD => "aa6cfa36cae86b40",
        Plain => "b916e0eacc1c00d7dcec68ec0b3bbb1a02de8a2d1aa346132e",
        Cipher => "b1d23a2220ddc0ac900d9aa03c61fcf4a559a4417767089708",
        Tag => "a776796edb723506");
      CCM_Case ("RFC 3610 packet vector #16",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00764c63b8058e3c9696766cfa",
        AAD => "d0d0735c531e1becf049c244",
        Plain => "12daac5630efa5396f770ce1a66b21f7b2101c",
        Cipher => "14d253c3967b70609b7cbb7c49916028324526",
        Tag => "9a6f49975bcadeaf");
      CCM_Case ("RFC 3610 packet vector #17",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00f8b678094e3b3c9696766cfa",
        AAD => "77b60f011c03e1525899bcae",
        Plain => "e88b6a46c78d63e52eb8c546efb5de6f75e9cc0d",
        Cipher => "5545ff1a085ee2efbf52b2e04bee1e2336c73e3f",
        Tag => "762c0c7744fe7e3c");
      CCM_Case ("RFC 3610 packet vector #18",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00d560912d3f703c9696766cfa",
        AAD => "cd9044d2b71fdb8120ea60c0",
        Plain => "6435acbafb11a82e2f071d7ca4a5ebd93a803ba87f",
        Cipher => "009769ecabdf48625594c59251e6035722675e04c8",
        Tag => "47099e5ae0704551");
      CCM_Case ("RFC 3610 packet vector #19",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "0042fff8f1951c3c9696766cfa",
        AAD => "d85bc7e69f944fb8",
        Plain => "8a19b950bcf71a018e5e6701c91787659809d67dbedd18",
        Cipher => "bc218daa947427b6db386a99ac1aef23ade0b52939cb6a",
        Tag => "637cf9bec2408897c6ba");
      CCM_Case ("RFC 3610 packet vector #20",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "00920f40e56cdc3c9696766cfa",
        AAD => "74a0ebc9069f5b37",
        Plain => "1761433c37c5a35fc1f39f406302eb907c6163be38c98437",
        Cipher => "5810e6fd25874022e80361a478e3e9cf484ab04f447efff6",
        Tag => "f0a477cc2fc9bf548944");
      CCM_Case ("RFC 3610 packet vector #21",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "0027ca0c7120bc3c9696766cfa",
        AAD => "44a3aa3aae6475ca",
        Plain => "a434a8e58500c6e41530538862d686ea9e81301b5ae4226bfa",
        Cipher => "f2beed7bc5098e83feb5b31608f8e29c38819a89c8e776f154",
        Tag => "4d4151a4ed3a8b87b9ce");
      CCM_Case ("RFC 3610 packet vector #22",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "005b8ccbcd9af83c9696766cfa",
        AAD => "ec46bb63b02520c33c49fd70",
        Plain => "b96b49e21d621741632875db7f6c9243d2d7c2",
        Cipher => "31d750a09da3ed7fddd49a2032aabf17ec8ebf",
        Tag => "7d22c8088c666be5c197");
      CCM_Case ("RFC 3610 packet vector #23",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "003ebe94044b9a3c9696766cfa",
        AAD => "47a65ac78b3d594227e85e71",
        Plain => "e2fcfbb880442c731bf95167c8ffd7895e337076",
        Cipher => "e882f1dbd38ce3eda7c23f04dd65071eb41342ac",
        Tag => "df7e00dccec7ae52987d");
      CCM_Case ("RFC 3610 packet vector #24",
        Key => "d7828d13b2b0bdc325a76236df93cc6b", Nonce => "008d493b30ae8b3c9696766cfa",
        AAD => "6e37a6ef546d955d34ab6059",
        Plain => "abf21c0b02feb88f856df4a37381bce3cc128517d4",
        Cipher => "f32905b88a641b04b9c9ffb58cc390900f3da12ab1",
        Tag => "6dce9e82efa16da62059");
      CCM_Case ("IEEE Std 802.11-2020 J.6.4 CCMP",
        Key => "c97c1f67ce371185514a8a19f2bdd52f", Nonce => "005030f1844408b5039776e70c",
        AAD => "08400fd2e128a57c5030f1844408abaea5b8fcba0000",
        Plain => "f8ba1a55d02f85ae967bb62fb6cda8eb7e78a050",
        Cipher => "f3d0a2fe9a3dbf2342a643e43246e80c3c04d019",
        Tag => "7845ce0b16f97623");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #2",
        Key => "8f7a053fa577a5597529272097a603d5", Nonce => "00eec1762c88de31f3cbba97ea",
        AAD => "08c0ea100c846850eec1762c88deaf2ee9f46a070000",
        Plain => "83a0634b5ed7627eb9df225e05740342de194117",
        Cipher => "814b6965d05bf2b2ed38d4beb069fe82714a610b",
        Tag => "542fbf8da06aa4ae");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #3",
        Key => "40cfb7a62e88013bd6d3affcc191041e", Nonce => "00b6a88adf36912fdca0f3a5ae",
        AAD => "88c0d9577df763c8b6a88adf3691dc4a8bca94dd00000000",
        Plain => "2c1bd036831c95496c5f4dbf3d559e72de802a18",
        Cipher => "89d8580340b626a0b6d4d013bf18f291b89646c8",
        Tag => "fd1f1f61a9fb4bb3");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #4",
        Key => "8c89a2ebc96c7602707fcf24b32d3833", Nonce => "078ef822734701f670a55a0fe3",
        AAD => "88c2712a9ddf11db8ef82273470159140dd646a200000700",
        Plain => "4fad2b1c290fa5ebd872fbc3f3a074898f8b2fbb",
        Cipher => "9d59b15f371448c230f4d739052e13ab3b1a7b10",
        Tag => "31fc88004f35ee3d");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #5",
        Key => "a574d5143bb25efddeff30122fdfd066", Nonce => "0bf351946bc96ba7ffe03c0e37",
        AAD => "88c245dec69a7480f351946bc96be276fbe6c12700000b00",
        Plain => "28969b954f263a8018a9ef70a8b051462481922e",
        Cipher => "eb4ae4956a801da9624b7e0c18b23e615ec03af6",
        Tag => "ce0c3be197d305eb");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #6",
        Key => "f71eea4e1f58804b9717230ad0614641", Nonce => "0dbff943b9f9a66b81eca48989",
        AAD => "88425af28430fdabbff943b9f9a6ab1d98c7fe7300000d00",
        Plain => "abfda22d3a0bfc9cc1fc079363c2fca143e6eb1d",
        Cipher => "9a709b60a39d40b1dfb612e18b5f114badb6cc86",
        Tag => "309a8d5c466bbb71");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #7",
        Key => "1bdb34980e038124a1db1a892bec366a", Nonce => "00efec952016915eec4073e723",
        AAD => "08419b50f4fd56f6efec9520169183570c4ccdee0000",
        Plain => "98beca86f4b38da20cfdf24724c58eb835665339",
        Cipher => "12c537ebf3ab584ef1fef9a1f3547a8c13b3225a",
        Tag => "2d0957ecfabe95b9");
      CCM_Case ("IEEE P802.11i/D7.0 CCMP MPDU #8",
        Key => "6eac1bf54bd54edb2321754303024c71", Nonce => "0aca3f3aae60c4cefd996eccdd",
        AAD => "88c1552d5f72bb70ca3f3aae60c48ba9b5f82c2f00000a00",
        Plain => "57cb5c0e5fcd885e9a4239e9b9cad60d64375979",
        Cipher => "4bf281ef8ec7739f91591b97a87dc14b3fa17462",
        Tag => "6dba8ef7f08087dd");

      --  Cross-checks against pyca/cryptography (OpenSSL), not published
      --  vectors: CCMP-256 parameters, an empty message, no AAD.
      CCM256_Case ("CCM AES-256 tag 16 (cross-check)",
        Key => Count_Bytes (16#40#, 32), Nonce => "00112233445566778899aabbcc",
        AAD => "88c245dec69a7480f351946bc96be276fbe6c12700000b00",
        Plain => Count_Bytes (0, 100),
        Cipher => "e16f751767e602a689ad8fe406857ab9f880b10090056687cd827759ab8b1658"
          & "89c8a949ced08ac82b21c3e420aa12002edc0b3ecd5c3893ed31c545d25b6bdc"
          & "22f6c8054b68c58244efb5e3a93db9bcba870635c3c5c2e8db09356e0d27ab69"
          & "d0319900",
        Tag => "ef1963f0ae0a0e21301981bdb25987dd");
      CCM_Case ("CCM empty message (cross-check)",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00112233445566778899aabbcc",
        AAD => "", Plain => "", Cipher => "", Tag => "6f586abd2cb335a2");
      CCM_Case ("CCM no AAD, tag 16 (cross-check)",
        Key => "c0c1c2c3c4c5c6c7c8c9cacbcccdcecf", Nonce => "00112233445566778899aabbcc",
        AAD => "", Plain => Count_Bytes (0, 17),
        Cipher => "9a5938906f3ee8adf1fa07ebdcaeb5ad13",
        Tag => "30f35e3941d1309e80eea28c335c3426");
   end Run;
end AES_Tests;
