with CCL.Evaluation;
with Ada.Text_IO;
with CCL.Types; use CCL.Types;
with CCL.Catalog;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with CCL.Format;

procedure Metadata_Tests is
   use type CCL.Language.Interpretation_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Value;
   use type CCL.Format.Format_Error;
   Source : Registry;
   Record_Type, Sum_Type, Ref : Type_Reference;
   Defined_As : Definition_Result;
   Imported_As : Import_Result;
   Catalog : CCL.Catalog.Interface_Catalog;
   Analysis : CCL.Language.Analysis_Result;
   Interpreted : CCL.Language.Interpretation_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Bad : CCL.VM.Program;
   Program : CCL.VM.Validated_Program;
   Valid : CCL.VM.Validation_Error;
   Executed : CCL.VM.Execution_Result;
   Data : CCL.Format.Byte_Array;
   Length : CCL.Format.Module_Length;
   Limits : CCL.Format.Resource_Limits := (4096, 4096, 1);
   Encoded_As : CCL.Format.Format_Error;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "VM metadata check" & Checks'Image; end if;
   end Check;
begin
   Define (Source, (Identifier => Named ("Document"), Form => Product, Count => 2,
     Parts => [1 => (Named ("text"), String_Type), 2 => (Named ("enabled"), Boolean_Type), others => <>]),
     Record_Type, Defined_As); Check (Defined_As = Defined);
   Define (Source, (Identifier => Named ("MaybeDocument"), Form => Sum, Count => 2,
     Parts => [1 => (Named ("Found"), Record_Type), 2 => (Named ("Missing"), Unit_Type), others => <>]),
     Sum_Type, Defined_As); Check (Defined_As = Defined);
   CCL.Catalog.Publish_Type (Catalog, Source, Sum_Type, Ref, Imported_As);
   Check (Imported_As = Imported);
   CCL.Evaluation.Evaluate ("MaybeDocument.Missing", 4096, Catalog, Interpreted);
   -- A general aggregate result comes out as its canonical literal.
   Check (Interpreted.Status = CCL.Language.Succeeded and Interpreted.Has_Literal and
          Interpreted.Literal.Data (1 .. Interpreted.Literal.Length) = "MaybeDocument.Missing");
   --  Compiled code builds the same value in its arena and prints the same
   --  literal (docs/ccl-bytecode-format.md, step 4).
   CCL.Language.Analyze ("MaybeDocument.Missing", Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled);
   Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
   CCL.VM.Execute (Program, 4096, Executed);
   Check (Executed.Status = CCL.VM.Completed and Executed.Has_Literal and
          Executed.Literal.Data (1 .. Executed.Literal.Length) = "MaybeDocument.Missing");
   Define (Source, (Identifier => Named ("TextReading"), Form => Sum, Count => 1,
     Parts => [1 => (Named ("Value"), String_Type), others => <>]), Ref, Defined_As);
   Check (Defined_As = Defined);
   declare
      Local : Type_Reference;
   begin
      CCL.Catalog.Publish_Type (Catalog, Source, Ref, Local, Imported_As);
      Check (Imported_As = Imported);
   end;
   CCL.Evaluation.Evaluate ("(TextReading.Value ""hello"")", 4096, Catalog, Interpreted);
   Check (Interpreted.Status = CCL.Language.Succeeded and Interpreted.Has_Literal and
          Interpreted.Literal.Data (1 .. Interpreted.Literal.Length) = "(TextReading.Value ""hello"")");
   CCL.Language.Analyze ("(TextReading.Value ""hello"")", Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
   CCL.VM.Execute (Program, 4096, Executed);
   Check (Executed.Status = CCL.VM.Completed and Executed.Has_Literal and
          Executed.Literal.Data (1 .. Executed.Literal.Length) = "(TextReading.Value ""hello"")");
   CCL.Evaluation.Evaluate
     ("(match (TextReading.Value ""hello"") ((TextReading.Value text) (length text)))", 4096, Catalog, Interpreted);
   Check (Interpreted.Status = CCL.Language.Succeeded and Interpreted.Result_Value = CCL.VM.Integer_Constant (5));
   CCL.Evaluation.Evaluate ("42", 4096, Catalog, Interpreted);
   Check (Interpreted.Status = CCL.Language.Succeeded);
   CCL.Language.Analyze ("42", Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
   CCL.VM.Execute (Program, 4096, Executed);
   Check (Executed.Status = CCL.VM.Completed and Executed.Result_Value = CCL.VM.Integer_Constant (42));
   CCL.Format.Encode (Compiled.Program, Limits, Data, Length, Encoded_As, Valid);
   Check (Encoded_As = CCL.Format.Format_Valid and Valid = CCL.VM.Valid);
   CCL.Format.Decode (Data, Length, Program, Limits, Encoded_As, Valid);
   Check (Encoded_As = CCL.Format.Format_Valid and Valid = CCL.VM.Valid);
   CCL.VM.Execute (Program, 4096, Executed);
   Check (Executed.Status = CCL.VM.Completed and Executed.Result_Value = CCL.VM.Integer_Constant (42));
   -- These are valid metadata but not executable VM values. Every entry path
   -- must still reject attempts to use either as a runtime scalar variant.
   for Unsupported in Record_Type .. Sum_Type loop
      for Entry_Path in 1 .. 5 loop
         Bad := Compiled.Program;
         case Entry_Path is
            when 1 => Bad.Code (0) := (Op => CCL.VM.Make_Variant, Data_Type => Unsupported, Alternative => 1, others => <>);
            when 2 => Bad.Code (0) := (Op => CCL.VM.Equal_Variant, Data_Type => Unsupported, others => <>);
            when 3 => Bad.Locals_Length := 1; Bad.Local_Kinds (0) := CCL.VM.Variant_Value;
               Bad.Local_Data_Types (0) := Unsupported;
            when 4 => Bad.Matches_Length := 1; Bad.Matches (0).Data_Type := Unsupported;
               -- General sums now execute with native storage, but reserved
               -- target slots must remain canonical even in unused tables.
               Bad.Matches (0).Targets (Maximum_Components) := 1;
            when others => Bad.Code (0).Data_Type := Unsupported;
         end case;
         CCL.VM.Verify (Bad, Program, Valid);
         Check (Valid = (if Entry_Path = 4 then CCL.VM.Invalid_Match else CCL.VM.Invalid_Data_Type));
         Check (not CCL.VM.Is_Valid (Program));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("VM type metadata versus executable values: PASS" & Checks'Image & " checks");
end Metadata_Tests;
