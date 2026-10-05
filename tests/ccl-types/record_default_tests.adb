with Ada.Text_IO; use Ada.Text_IO;
with CCL.Language; use CCL.Language;
with CCL.Language.Views; use CCL.Language.Views;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Format;
with CCL.Sessions;
with CCL.VM; use CCL.VM;

--  Record field defaults and named construction (docs/ccl-launch-parameters.md):
--  each through the interpreter, BASIC and back, compiler, verifier, VM and CCLB.
procedure Record_Default_Tests is
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Format.Format_Error;
   use type CCL.Format.Byte_Array;
   Catalog : CCL.Catalog.Interface_Catalog;
   R : Interpretation_Result;
   A, B : Conversion;
   Analysis : Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Checked : Validated_Program;
   Validation : Validation_Error;
   Executed : Execution_Result;
   Data, Encoded : CCL.Format.Byte_Array;
   Length, Encoded_Length : CCL.Format.Module_Length;
   Error : CCL.Format.Format_Error;
   Limits : CCL.Format.Resource_Limits;
   Decoded : Program;
   Linkage : CCL.Catalog.Linkage_Table;
   Prefix : constant String :=
     "(type Mode (enum Fast Safe)) (type Count (range 1 4096)) " &
     "(type Limits (record (name String) (connections Count 1024) (verbose Boolean false) " &
     "(mode Mode Mode.Fast) (ports (List Integer) []))) ";
   function Mode_Of (Expression : String) return String is
     ("(if (= (field " & Expression & " mode) Mode.Fast) 1 2)");
   procedure Check (Source, Expected, VM_Expected : String) is
   begin
      Interpret (Source, 4096, R);
      Put_Line ("checking: " & Source);
      if R.Status /= Succeeded then Put_Line (CCL.Sessions.Result_Image (R)); end if;
      pragma Assert (R.Status = Succeeded);
      pragma Assert (CCL.Sessions.Result_Image (R) = Expected);
      Convert (Source, Lisp, Basic, Catalog, A);
      pragma Assert (A.Status = Converted);
      Convert (A.Rendered.Data (1 .. A.Rendered.Length), Basic, Lisp, Catalog, B);
      if B.Status /= Converted or else A.Canonical /= B.Canonical then
         Put_Line (A.Rendered.Data (1 .. A.Rendered.Length));
         Put_Line (B.Status'Image & " " & B.Diagnostic'Image & B.Position'Image);
         Put_Line ("A: " & A.Canonical.Data (1 .. A.Canonical.Length));
         Put_Line ("B: " & B.Canonical.Data (1 .. B.Canonical.Length));
      end if;
      pragma Assert (B.Status = Converted and A.Canonical = B.Canonical);
      Interpret (B.Canonical.Data (1 .. B.Canonical.Length), 4096, R);
      pragma Assert (R.Status = Succeeded and CCL.Sessions.Result_Image (R) = Expected);
      Analyze (Source, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      pragma Assert (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Verify (Compiled.Program, Checked, Validation);
      if Validation /= Valid then Put_Line (Validation'Image); end if;
      pragma Assert (Validation = Valid);
      Execute (Checked, 4096, Executed);
      pragma Assert (Executed.Status = Completed);
      pragma Assert (Value_Image (Compiled.Program.Data_Types, Executed.Result_Value) = VM_Expected);
      CCL.Format.Encode (Compiled.Program, (4096, 4096, 1), Data, Length, Error, Validation);
      pragma Assert (Error = CCL.Format.Format_Valid);
      CCL.Format.Decode (Data, Length, Decoded, Linkage, Limits, Error, Validation);
      pragma Assert (Error = CCL.Format.Format_Valid);
      Verify (Decoded, Checked, Validation);
      pragma Assert (Validation = Valid);
      Execute (Checked, 4096, Executed);
      pragma Assert (Executed.Status = Completed);
      pragma Assert (Value_Image (Decoded.Data_Types, Executed.Result_Value) = VM_Expected);
      CCL.Format.Encode (Decoded, Limits, Encoded, Encoded_Length, Error, Validation);
      pragma Assert (Error = CCL.Format.Format_Valid and Length = Encoded_Length and Data = Encoded);
   end Check;
   procedure Reject (Source : String; Code : Diagnostic_Code) is
   begin
      Interpret (Source, 4096, R);
      if R.Diagnostic /= Code then Put_Line (Source & " -> " & R.Diagnostic'Image & "; expected " & Code'Image); end if;
      pragma Assert (R.Status in Parse_Failed | Type_Check_Failed);
      pragma Assert (R.Diagnostic = Code and not R.Has_Value);
      pragma Assert (R.Fuel_Remaining = 4096);
   end Reject;
begin
   CCL.Catalog.Initialize (Catalog);
   --  Every field but name has a default.
   Check (Prefix & "(field (Limits ""a"") connections)", "Integer: 1024", " 1024");
   Check (Prefix & "(field (Limits name => ""a"") verbose)", "Boolean: false", "false");
   Check (Prefix & Mode_Of ("(Limits ""a"")"), "Integer: 1", " 1");
   Check (Prefix & "(length (field (Limits ""a"") ports))", "Integer: 0", " 0");
   --  Named fields (Ada's field => value) in any order; positional first.
   Check (Prefix & "(field (Limits connections => 7 name => ""a"") connections)", "Integer: 7", " 7");
   Check (Prefix & "(field (Limits ""a"" 9 mode => Mode.Safe) connections)", "Integer: 9", " 9");
   Check (Prefix & Mode_Of ("(Limits ""a"" mode => Mode.Safe)"), "Integer: 2", " 2");
   Check (Prefix & "(field (Limits ""a"" 5 true Mode.Safe [1 2]) verbose)", "Boolean: true", "true");
   Check (Prefix & "(sum (field (Limits ports => [3 4] name => ""b"") ports))", "Integer: 7", " 7");
   --  A named value is checked against its field's type.
   Reject (Prefix & "(Limits name => ""a"" connections => 0)", Value_Out_Of_Range);
   Reject (Prefix & "(Limits name => ""a"" verbose => 3)", Field_Type_Mismatch);
   Reject (Prefix & "(Limits name => ""a"" bogus => 3)", Unknown_Field_Argument);
   Reject (Prefix & "(Limits ""a"" name => ""b"")", Repeated_Field_Argument);
   Reject (Prefix & "(Limits connections => 3 connections => 4 name => ""b"")", Repeated_Field_Argument);
   Reject (Prefix & "(Limits connections => 3)", Missing_Field_Argument);
   Reject (Prefix & "(Limits name => ""a"" 5)", Positional_After_Named);
   --  Defaults are constants of their field's type.
   Reject ("(type R (record (x Integer true))) 0", Invalid_Field_Default);
   Reject ("(type P (range 1 10)) (type R (record (p P 11))) 0", Invalid_Field_Default);
   Reject ("(type C (enum Red Blue)) (type D (enum Green)) (type R (record (c C D.Green))) 0", Invalid_Field_Default);
   Reject ("(type R (record (xs (List Integer) 0))) 0", Invalid_Field_Default);
   Reject ("(type R (record (b Boolean []))) 0", Invalid_Field_Default);
   Put_Line ("CCL record defaults: interpreter, BASIC, compiler, verifier, VM and CCLB roundtrip PASS");
end Record_Default_Tests;
