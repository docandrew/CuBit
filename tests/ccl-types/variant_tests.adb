with Ada.Text_IO; use Ada.Text_IO;
with CCL.Language; use CCL.Language;
with CCL.Language.Views; use CCL.Language.Views;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Format;
with CCL.Sessions;
with CCL.VM; use CCL.VM;

procedure Variant_Tests is
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
     "(type Reading (variant (Value Integer) (Unavailable) (Flag Boolean))) ";
   function Matched (Expression : String) return String is
     ("(match " & Expression &
      " ((Reading.Value n) (+ n 1))" &
      " ((Reading.Unavailable) 0)" &
      " ((Reading.Flag b) (if b 7 8)))");
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
      if B.Status /= Converted then Put_Line (A.Rendered.Data (1 .. A.Rendered.Length)); end if;
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
   Check (Prefix & "(Reading.Value 41)", "Reading.Value( 41)", "Reading.Value( 41)");
   Check (Prefix & "Reading.Unavailable", "Reading.Unavailable", "Reading.Unavailable");
   Check (Prefix & "(Reading.Flag true)", "Reading.Flag(true)", "Reading.Flag(true)");
   Check (Prefix & Matched ("(Reading.Value 41)"), "Integer: 42", " 42");
   Check (Prefix & Matched ("Reading.Unavailable"), "Integer: 0", " 0");
   Check (Prefix & Matched ("(Reading.Flag true)"), "Integer: 7", " 7");
   Check (Prefix & Matched ("(Reading.Flag false)"), "Integer: 8", " 8");
   Check (Prefix & "(+ 10 " & Matched ("(Reading.Value 41)") & ")", "Integer: 52", " 52");
   Check (Prefix & "(let ((r (Reading.Value 41))) " & Matched ("r") & ")", "Integer: 42", " 42");
   Check (Prefix & "(match (Reading.Value 21) ((Reading.Flag b) 0) ((Reading.Value n) (+ n n)) ((Reading.Unavailable) 1))", "Integer: 42", " 42");
   Check (Prefix & "(match (Reading.Value 20) ((Reading.Value n) (+ n " & Matched ("(Reading.Value n)") & ")) ((Reading.Flag b) 0) ((Reading.Unavailable) 0))", "Integer: 41", " 41");
   Check (Prefix & "(match Reading.Unavailable ((Reading.Value n) (/ n 0)) ((Reading.Unavailable) 42) ((Reading.Flag b) (/ 1 0)))", "Integer: 42", " 42");
   Check (Prefix & "(match (Reading.Value 21) ((Reading.Value n) (Reading.Value (+ n n))) ((Reading.Unavailable) Reading.Unavailable) ((Reading.Flag b) (Reading.Flag b)))", "Reading.Value( 42)", "Reading.Value( 42)");
   Check ("(type Color (enum Red Blue)) (= Color.Red Color.Blue)", "Boolean: false", "false");
   Check ("(type Color (enum Red Blue)) (let ((c Color.Blue)) (if (= c Color.Red) Color.Red c))", "Color.Blue", "Color.Blue");
   Reject (Prefix & "(Reading.Value true)", Invalid_Variant_Payload);
   Reject (Prefix & "Reading.Value", Invalid_Variant_Payload);
   Reject (Prefix & "(match (Reading.Value 1) ((Reading.Value n) n))", Nonexhaustive_Match);
   Reject (Prefix & "(match (Reading.Value 1) ((Reading.Value n) n) ((Reading.Value m) m))", Duplicate_Match_Arm);
   Reject (Prefix & "(match Reading.Unavailable ((Reading.Value) 1) ((Reading.Unavailable) 0) ((Reading.Flag b) 0))", Invalid_Match_Pattern);
   Reject (Prefix & "(match Reading.Unavailable ((Reading.Value n) n) ((Reading.Unavailable x) 0) ((Reading.Flag b) 0))", Invalid_Match_Pattern);
   Reject (Prefix & "(match Reading.Unavailable ((Reading.Value n) n) ((Reading.Unavailable) true) ((Reading.Flag b) 0))", Branch_Type_Mismatch);
   Reject (Prefix & "(type Other (enum None)) (match Other.None ((Reading.Value n) n) ((Reading.Unavailable) 0) ((Reading.Flag b) 0))", Invalid_Match_Pattern);
   Put_Line ("CCL variants: interpreter, BASIC, compiler, verifier, VM and CCLB roundtrip PASS");
end Variant_Tests;
