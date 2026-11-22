with SPARKNaCl.AES; use SPARKNaCl.AES;
with WiFi_Crypto.AES_CMAC;
with WiFi_Crypto.Legacy_WPA2;
with WiFi_Crypto.P256_SSWU;

package body WiFi_Crypto.Self_Test with
   SPARK_Mode => On
is
   --  The longest text a vector here spells out (the RFC 9380 DST).
   Max_Text : constant := 64;

   --  ASCII text as bytes, indexed from 0.
   function Text (S : String) return Byte_Seq
   with Pre  => S'Length in 1 .. Max_Text,
        Post => Text'Result'First = 0
                and then Text'Result'Length = S'Length
   is
      Result : Byte_Seq (0 .. N32 (S'Length) - 1);
   begin
      for I in Result'Range loop
         Result (I) := Character'Pos (S (S'First + Natural (I)));
      end loop;
      return Result;
   end Text;

   --  IEEE Std 802.11-2020 J.4.2, first vector.
   PMK_Passphrase : constant String := "password";
   PMK_SSID       : constant String := "IEEE";
   PMK_Expected   : constant Legacy_WPA2.PMK :=
     [16#f4#, 16#2c#, 16#6f#, 16#c5#, 16#2d#, 16#f0#, 16#eb#, 16#ef#,
      16#9e#, 16#bb#, 16#4b#, 16#90#, 16#b3#, 16#8a#, 16#5f#, 16#90#,
      16#2e#, 16#83#, 16#fe#, 16#1b#, 16#13#, 16#5a#, 16#70#, 16#e2#,
      16#3a#, 16#ed#, 16#76#, 16#2e#, 16#97#, 16#10#, 16#a1#, 16#2e#];

   --  RFC 4493 4, example 2 (Mlen = 16).
   CMAC_Key : constant Bytes_16 :=
     [16#2b#, 16#7e#, 16#15#, 16#16#, 16#28#, 16#ae#, 16#d2#, 16#a6#,
      16#ab#, 16#f7#, 16#15#, 16#88#, 16#09#, 16#cf#, 16#4f#, 16#3c#];
   CMAC_Message : constant Bytes_16 :=
     [16#6b#, 16#c1#, 16#be#, 16#e2#, 16#2e#, 16#40#, 16#9f#, 16#96#,
      16#e9#, 16#3d#, 16#7e#, 16#11#, 16#73#, 16#93#, 16#17#, 16#2a#];
   CMAC_Expected : constant Bytes_16 :=
     [16#07#, 16#0a#, 16#16#, 16#b4#, 16#6b#, 16#4d#, 16#41#, 16#44#,
      16#f7#, 16#9b#, 16#dd#, 16#9d#, 16#d0#, 16#4a#, 16#28#, 16#7c#];

   --  RFC 9380 K.1, msg "abc", len_in_bytes 0x20.
   XMD_DST     : constant String := "QUUX-V01-CS02-with-expander-SHA256-128";
   XMD_Message : constant String := "abc";
   XMD_Bytes   : constant := 32;
   XMD_Expected : constant Bytes_32 :=
     [16#d8#, 16#cc#, 16#ab#, 16#23#, 16#b5#, 16#98#, 16#5c#, 16#ce#,
      16#a8#, 16#65#, 16#c6#, 16#c9#, 16#7b#, 16#6e#, 16#5b#, 16#83#,
      16#50#, 16#e7#, 16#94#, 16#e6#, 16#03#, 16#b4#, 16#b9#, 16#79#,
      16#02#, 16#f5#, 16#3a#, 16#8a#, 16#0d#, 16#60#, 16#56#, 16#15#];

   procedure Run (Passed : out Outcomes) is
      Key  : Legacy_WPA2.PMK;
      Tag  : AES_CMAC.AES128.Tag;
      RK   : constant AES128_Round_Keys := Key_Expansion (Construct (CMAC_Key));
      Uniform : Byte_Seq (0 .. XMD_Bytes - 1);
   begin
      Legacy_WPA2.Passphrase_To_PMK (Key, Text (PMK_Passphrase), Text (PMK_SSID));
      AES_CMAC.AES128.Compute (Tag, RK, CMAC_Message);
      P256_SSWU.Expand_Message_XMD (Uniform, Text (XMD_Message), Text (XMD_DST));
      Passed := [PMK  => Equal (Key, PMK_Expected),
                 CMAC => Equal (Tag, CMAC_Expected),
                 XMD  => Equal (Uniform, XMD_Expected)];
   end Run;
end WiFi_Crypto.Self_Test;
