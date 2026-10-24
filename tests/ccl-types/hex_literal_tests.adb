with Ada.Text_IO; use Ada.Text_IO;
with CCL.Evaluation; use CCL.Evaluation;
with CCL.Language; use CCL.Language;
with CCL.Sessions;

--  Hexadecimal integer literals (HW-002: PCI and USB IDs in manifests are
--  written 0x8086). 0x or 0X, then hex digits of either case; the value is
--  the same Integer a decimal literal gives, with the same overflow rule.
procedure Hex_Literal_Tests is
   R : Interpretation_Result;
   Checks : Natural := 0;
   procedure Accept_Value (Source, Expected : String) is
   begin
      Evaluate (Source, 4096, R);
      if R.Status /= Succeeded or else CCL.Sessions.Result_Image (R) /= Expected then
         Put_Line ("FAIL: " & Source & " gave " & CCL.Sessions.Result_Image (R));
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Accept_Value;
   procedure Refuse (Source : String) is
   begin
      Evaluate (Source, 4096, R);
      if R.Status = Succeeded then
         Put_Line ("FAIL: accepted " & Source);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Refuse;
begin
   Accept_Value ("0x10", "Integer: 16");
   Accept_Value ("0X1f", "Integer: 31");
   Accept_Value ("0x8086", "Integer: 32902");
   Accept_Value ("(+ 0x46D0 4)", "Integer: 18132");
   Accept_Value ("-0x10", "Integer: -16");
   Accept_Value ("0x0", "Integer: 0");
   Accept_Value ("0x7FFFFFFFFFFFFFFF", "Integer: 9223372036854775807");
   Accept_Value ("-0x8000000000000000", "Integer: -9223372036854775808");
   Accept_Value ("0", "Integer: 0");
   Accept_Value ("010", "Integer: 10");
   Refuse ("0x8000000000000000");
   Refuse ("0x10000000000000000");
   Refuse ("0xG1");
   Refuse ("(+ 0x 1)");
   Refuse ("0x1G");
   Put_Line ("hex literal tests:" & Natural'Image (Checks) & " checks passed");
end Hex_Literal_Tests;
