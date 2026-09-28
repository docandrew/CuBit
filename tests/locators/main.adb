--  Locators: the @net grammar on known cases, and address parsing compared
--  with Linux's inet_pton over structured and random text. Linux-hosted
--  checks; the proofs are what cover all inputs.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with System;
with CuBit.Net_Address; use CuBit.Net_Address;
with CuBit.Locators; use CuBit.Locators;
with CuBit.Net_Locator; use CuBit.Net_Locator;

procedure Main is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; Name : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & Name);
      end if;
   end Check;

   AF_INET  : constant := 2;
   AF_INET6 : constant := 10;
   function inet_pton (Family : int; Src : char_array; Dst : System.Address) return int
     with Import, Convention => C, External_Name => "inet_pton";

   --  What Linux makes of S as an address, in our representation.
   procedure Reference (S : String; A : out Address; OK : out Boolean) is
      V4 : array (0 .. 3) of Unsigned_8 := [others => 0];
   begin
      A := Unspecified;
      if (for some C of S => C = ':') then
         OK := inet_pton (AF_INET6, To_C (S), A'Address) = 1;
      else
         OK := inet_pton (AF_INET, To_C (S), V4'Address) = 1;
         if OK then
            A := [0 .. 9 => 0, 10 => 16#FF#, 11 => 16#FF#,
                  12 => V4 (0), 13 => V4 (1), 14 => V4 (2), 15 => V4 (3)];
         end if;
      end if;
   end Reference;

   function Parsed (S : String) return Target is
      T : Target;
      Text : constant String (1 .. S'Length) := S;
   begin
      Parse (Text, T);
      return T;
   end Parsed;

   R : Target;
   Seed : Unsigned_32 := 7;
   function Next return Unsigned_32 is
   begin
      Seed := Seed * 1_103_515_245 + 12_345;
      return Shift_Right (Seed, 8);
   end Next;
   Disagreements : Natural := 0;
   Compared : Natural := 0;
begin
   --  The @net grammar.
   R := Parsed ("@net:tcp:10.0.2.2:18443");
   Check (R.Valid and then R.Proto = TCP and then R.Is_Address and then
          R.Address = Mapped (16#0A00_0202#) and then R.Port = 18443, "tcp IPv4");
   R := Parsed ("@net:tcp:[2001:db8::1]:443");
   Check (R.Valid and then R.Is_Address and then R.Address (0) = 16#20# and then
          R.Address (15) = 1 and then R.Port = 443, "tcp IPv6 in brackets");
   R := Parsed ("@net:tcp-listen:[::1]:8080");
   Check (R.Valid and then R.Proto = TCP_Listen and then R.Address (15) = 1, "listen ::1");
   R := Parsed ("@net:udp:example.com:53");
   Check (R.Valid and then not R.Is_Address and then
          R.Name (1 .. R.Name_Len) = "example.com", "named host");
   Check (not Parsed ("@net:tcp:[::ffff:10.0.2.2]:80").Valid, "mapped IPv4 in brackets refused");
   Check (not Parsed ("@net:tcp:[10.0.2.2]:80").Valid, "bracketed IPv4 refused");
   Check (not Parsed ("@net:tcp:10.0.2.4294967298:80").Valid, "long octet refused");
   Check (not Parsed ("@net:tcp:10..2.2:80").Valid, "empty octet refused");
   Check (not Parsed ("@net:tcp:1.2.3.4:80:junk").Valid, "trailing field refused");
   Check (not Parsed ("@net:tcp:1.2.3.4:0").Valid, "port 0 refused");
   Check (not Parsed ("@net:tcp:1.2.3.4:65536").Valid, "port over 65535 refused");
   Check (not Parsed ("@net:tcp:2001:db8::1:443").Valid, "unbracketed IPv6 refused");
   Check (not Parsed ("@net:tcp:[2001:db8::1:443").Valid, "unclosed bracket refused");
   Check (not Parsed ("@net:tcp::80").Valid, "empty host refused");
   Check (not Parsed ("@net:sctp:1.2.3.4:80").Valid, "unknown protocol refused");
   Check (not Parsed ("@nett:tcp:1.2.3.4:80").Valid, "other authority refused");
   Check (not Parsed ("@net:tcp:" & [1 .. 65 => 'a'] & ":80").Valid, "long name refused");
   Check (Parsed ("@net:tcp:" & [1 .. 64 => 'a'] & ":80").Valid, "64-character name");
   Check (not Parsed ("@net:tcp:ex/ample:80").Valid, "name with '/' refused");

   --  Address text: ours against inet_pton.
   declare
      type Text is access constant String;
      Cases : constant array (1 .. 24) of Text :=
        [new String'("::"), new String'("::1"), new String'("1::"),
         new String'("2001:db8::1"), new String'("fe80::1:2:3:4"),
         new String'("1:2:3:4:5:6:7:8"), new String'("1:2:3:4:5:6:7::"),
         new String'("::2:3:4:5:6:7:8"), new String'("1:2:3:4:5:6:7:8:9"),
         new String'("1::2::3"), new String'(":1::2"), new String'("1::2:"),
         new String'("12345::1"), new String'("::ffff:10.0.2.2"),
         new String'("::10.0.2.2"), new String'("1:2:3:4:5:6:1.2.3.4"),
         new String'("1:2:3:4:5:6:7:1.2.3.4"), new String'("::1.2.3"),
         new String'("::1.2.3.256"), new String'("10.0.2.2"),
         new String'("255.255.255.255"), new String'("01.2.3.4"),
         new String'("1.2.3.4.5"), new String'("g::1")];
      Ours, Theirs : Address;
      Ours_OK, Theirs_OK : Boolean;
   begin
      for C of Cases loop
         Parse_Address (C.all, Ours, Ours_OK);
         Reference (C.all, Theirs, Theirs_OK);
         if Ours_OK /= Theirs_OK or else (Ours_OK and then Ours /= Theirs) then
            Put_Line ("  differs from inet_pton: """ & C.all & """ ours " &
                      Ours_OK'Image & " theirs " & Theirs_OK'Image);
            Disagreements := Disagreements + 1;
         end if;
         Compared := Compared + 1;
      end loop;
   end;
   declare
      Alphabet : constant String := "0123456789abcdef:.:";
      S : String (1 .. 45);
      Ours, Theirs : Address;
      Ours_OK, Theirs_OK : Boolean;
      Accepted : Natural := 0;
   begin
      for Round in 1 .. 2_000_000 loop
         declare
            Len : constant Natural := 1 + Natural (Next mod 45);
         begin
            for I in 1 .. Len loop
               S (I) := Alphabet (1 + Natural (Next mod Alphabet'Length));
            end loop;
            Parse_Address (S (1 .. Len), Ours, Ours_OK);
            Reference (S (1 .. Len), Theirs, Theirs_OK);
            if Ours_OK then
               Accepted := Accepted + 1;
            end if;
            if Ours_OK /= Theirs_OK or else (Ours_OK and then Ours /= Theirs) then
               if Disagreements < 10 then
                  Put_Line ("  differs from inet_pton: """ & S (1 .. Len) & """ ours " &
                            Ours_OK'Image & " theirs " & Theirs_OK'Image);
               end if;
               Disagreements := Disagreements + 1;
            end if;
            Compared := Compared + 1;
         end;
      end loop;
      Put_Line ("random address texts accepted:" & Accepted'Image);
   end;
   Check (Disagreements = 0, "address parsing agrees with inet_pton");
   Put_Line ("compared" & Compared'Image & " texts with inet_pton," &
             Disagreements'Image & " disagreements");

   --  Prefixes.
   Check (Matches (Mapped (16#0A00_0203#), Mapped (16#0A00_0200#), Mapped_Prefix + 24),
          "10.0.2.3 in 10.0.2.0/24");
   Check (not Matches (Mapped (16#0A00_0303#), Mapped (16#0A00_0200#), Mapped_Prefix + 24),
          "10.0.3.3 not in 10.0.2.0/24");
   Check (Contains (Mapped (0), Mapped_Prefix, Mapped (16#0A00_0200#), Mapped_Prefix + 24),
          "IPv4 /24 inside all of IPv4");
   Check (not Matches (Unspecified, Mapped (0), Mapped_Prefix), ":: is not IPv4");

   Put_Line (if Failures = 0 then "LOCATORS: PASS" else "LOCATORS: FAIL");
end Main;
