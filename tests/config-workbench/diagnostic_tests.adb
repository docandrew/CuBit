with Ada.Text_IO;
with Ada.Strings.Fixed;
with CCL.Diagnostics;
with CCL.Language;
with CCL.VM;

procedure Diagnostic_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "diagnostic check" & Checks'Image; end if;
   end Check;
   procedure Readable (Text : String) is
   begin
      Check (Text'Length > 0 and then Text (Text'First) in 'A' .. 'Z');
   end Readable;
begin
   for Status in CCL.VM.Execution_Status loop
      Readable (CCL.Diagnostics.Message (Status));
   end loop;
   for Status in CCL.Language.Interpretation_Status loop
      Readable (CCL.Diagnostics.Message (Status));
   end loop;
   for Code in CCL.Language.Diagnostic_Code loop
      -- Some diagnostics intentionally start with the spelling "to-string".
      Check (CCL.Diagnostics.Message (Code)'Length > 0);
   end loop;
   Check (CCL.Diagnostics.Message (CCL.VM.Completed) = "Completed");
   Check (CCL.Diagnostics.Message (CCL.VM.Waiting_For_Host) = "Waiting for service");
   Check (CCL.Diagnostics.Message (CCL.VM.Host_Call_Failed) = "Service call failed");
   Check (Ada.Strings.Fixed.Index
     (CCL.Diagnostics.Message (CCL.Language.Expected_Name), "32 characters") > 0);
   Check (Ada.Strings.Fixed.Index
     (CCL.Diagnostics.Message (CCL.Language.Host_Contract_Unsupported), "lifecycle contract") > 0);
   Ada.Text_IO.Put_Line ("Readable CCL diagnostics: PASS" & Checks'Image & " checks");
end Diagnostic_Tests;
