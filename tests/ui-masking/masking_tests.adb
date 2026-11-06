--  Masked text fields (CuBit.UI_Masking): a Wi-Fi passphrase shows one mask
--  character per character (laptop-18: it "stopped showing * after 6"),
--  while Fixed_Length still hides the length.
with Ada.Text_IO; use Ada.Text_IO;
with CuBit.UI_Masking; use CuBit.UI_Masking;
procedure Masking_Tests is
   Failures : Natural := 0;
   procedure Check (OK : Boolean; What : String) is
   begin
      if OK then Put_Line ("PASS: " & What);
      else Put_Line ("FAIL: " & What); Failures := Failures + 1; end if;
   end Check;
   Long_Passphrase : constant Natural := 20;
begin
   Check (Shown_Length (0, Per_Character) = 0, "empty per-character shows nothing");
   Check (Shown_Length (0, Fixed_Length) = 0, "empty fixed shows nothing");
   Check (Shown_Length (Long_Passphrase, Per_Character) = Long_Passphrase,
          "per-character shows one mask character per character (20)");
   Check (Shown_Length (1, Per_Character) = 1, "per-character, one character");
   Check (Shown_Length (Long_Passphrase, Fixed_Length) = Fixed_Mask'Length,
          "fixed hides the length");
   Check (Shown_Length (3, Fixed_Length) = Fixed_Mask'Length, "fixed hides a short length");
   Check (Shown_Position (5, Long_Passphrase, Per_Character) = 5,
          "per-character caret stays where it is in the text");
   Check (Shown_Position (Long_Passphrase, Long_Passphrase, Per_Character) = Long_Passphrase,
          "per-character caret at the end");
   Check (Shown_Position (5, Long_Passphrase, Fixed_Length) = Fixed_Mask'Length,
          "fixed caret sits after the mask");
   if Failures = 0 then Put_Line ("MASKING: PASS"); else Put_Line ("MASKING: FAIL"); end if;
end Masking_Tests;
