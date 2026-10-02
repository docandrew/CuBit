with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Header; use Intel_GPU_ADS_Header;
procedure ADS_Header_Tests is
   Registers : Register_Descriptors;
   Capture : Capture_Pointers;
   Golden, Sizes : Class_Values;
   Actual, Expected : Header_Bytes;
   procedure Word (Offset : Natural; Value : Unsigned_32) is
      Remaining : Unsigned_32 := Value;
   begin
      for J in Natural range 0 .. 3 loop
         Expected (Offset + J) := Unsigned_8 (Remaining mod 256);
         Remaining := Remaining / 256;
      end loop;
   end Word;
   Policy, Info, Private_Address : Unsigned_32;
begin
   for Seed in Natural range 0 .. 255 loop
      for I in Registers'Range loop Registers (I) := Unsigned_8 ((I + Seed) mod 256); end loop;
      for I in Capture'Range loop Capture (I) := Unsigned_8 ((3 * I + Seed) mod 256); end loop;
      for C in Class_Values'Range loop
         Golden (C) := Unsigned_32 (Seed) * 16#01010101# + Unsigned_32 (C);
         Sizes (C) := not Golden (C);
      end loop;
      Policy := Unsigned_32 (Seed) * 16#01010101#;
      Info := not Policy;
      Private_Address := Policy xor 16#13579BDF#;
      Expected := [others => 0];
      for I in Registers'Range loop Expected (I) := Registers (I); end loop;
      Word (4100, Policy);
      Word (4104, Info);
      for C in Class_Values'Range loop
         Word (4116 + C * 4, Golden (C));
         Word (4180 + C * 4, Sizes (C));
      end loop;
      Word (4244, Private_Address);
      for I in Capture'Range loop Expected (4252 + I) := Capture (I); end loop;
      Actual := Encode (Registers, Policy, Info, Private_Address, Golden, Sizes, Capture);
      pragma Assert (Actual = Expected);
   end loop;
   Ada.Text_IO.Put_Line ("ADS header: PASS (256 full-byte patterns)");
end ADS_Header_Tests;
