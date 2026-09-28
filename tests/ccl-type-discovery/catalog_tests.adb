with Ada.Text_IO;
with Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Catalog;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with CCL.Format;

procedure Catalog_Tests is
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Link_Result;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Language.Interpretation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Value_Kind;
   use type CCL.VM.Value;
   use type CCL.Format.Format_Error;
   use type Interfaces.Integer_64;
   Source : Registry;
   Root, Ref : Type_Reference;
   Defined_As : Definition_Result;
   Imported_As : Import_Result;
   Catalog, Empty_Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "catalog type check" & Checks'Image; end if;
   end Check;
   procedure Run
     (Text : String; Expected : CCL.Types.Component_Index; Bytecode : Boolean := True;
      Flag : Boolean := True) is
      Analysis : CCL.Language.Analysis_Result;
      Interpreted : CCL.Language.Interpretation_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
      Executed : CCL.VM.Execution_Result;
      Decoded : CCL.VM.Execution_Result;
      Data : CCL.Format.Byte_Array;
      Length : CCL.Format.Module_Length;
      Limits : CCL.Format.Resource_Limits := (4096, 4096, 1);
      Encoded_As : CCL.Format.Format_Error;
   begin
      CCL.Language.Interpret (Text, 4096, Catalog, Interpreted);
      Check (Interpreted.Status = CCL.Language.Succeeded and Interpreted.Has_Value);
      Check (Same (Interpreted.Variant_Member_Name, Describe (Source, Root).Parts (Expected).Identifier));
      Check (Interpreted.Result_Value.Copyable and Interpreted.Result_Value.Type_Tag = 0);
      case Describe (Source, Root).Parts (Expected).Payload is
         when Integer_Type =>
            Check (Interpreted.Result_Value.Kind = CCL.VM.Integer_Value and then
                   Interpreted.Result_Value.Integer = 42);
         when Boolean_Type =>
            Check (Interpreted.Result_Value.Kind = CCL.VM.Boolean_Value and then
                   Interpreted.Result_Value.Boolean = Flag);
         when Unit_Type => null;
         when others => raise Program_Error with "unexpected fixture payload";
      end case;
      CCL.Language.Analyze (Text, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      if not Bytecode then
         -- Functions remain interpreter-only in the existing compiler.
         Check (Compiled.Status = CCL.Compiler.Unsupported_Form); return;
      end if;
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
      CCL.VM.Execute (Program, 4096, Executed); Check (Executed.Status = CCL.VM.Completed);
      Check (Executed.Result_Value.Kind = CCL.VM.Variant_Value and Executed.Result_Value.Alternative = Expected);
      Check (Same (Describe (Compiled.Program.Data_Types, Executed.Result_Value.Data_Type).Identifier, Named ("Reading")));
      CCL.Format.Encode (Compiled.Program, Limits, Data, Length, Encoded_As, Valid);
      Check (Encoded_As = CCL.Format.Format_Valid and Valid = CCL.VM.Valid);
      CCL.Format.Decode (Data, Length, Program, Limits, Encoded_As, Valid);
      Check (Encoded_As = CCL.Format.Format_Valid and Valid = CCL.VM.Valid);
      CCL.VM.Execute (Program, 4096, Decoded);
      Check (Decoded.Status = CCL.VM.Completed and Decoded.Result_Value = Executed.Result_Value);
   end Run;
begin
   Define (Source, (Identifier => Named ("Hidden"), Form => Product, others => <>), Ref, Defined_As);
   Check (Defined_As = Defined);
   Define (Source, (Identifier => Named ("Reading"), Form => Sum, Count => 3,
     Parts => [1 => (Named ("Value"), Integer_Type), 2 => (Named ("Unavailable"), Unit_Type),
               3 => (Named ("Flag"), Boolean_Type), others => <>]), Root, Defined_As);
   Check (Defined_As = Defined);
   CCL.Catalog.Publish_Type (Catalog, Source, Root, Ref, Imported_As);
   Check (Imported_As = Imported and Ref /= Root);
   Check (Find (CCL.Catalog.Visible_Types (Catalog), Named ("Hidden")) = Invalid_Type);
   Check (Find (CCL.Catalog.Visible_Types (Empty_Catalog), Named ("Reading")) = Invalid_Type);
   Run ("(Reading.Value 42)", 1);
   Run ("Reading.Unavailable", 2);
   Run ("(Reading.Flag true)", 3);
   Run ("(Reading.Flag false)", 3, Flag => False);
   Run ("(define (id (x Reading)) Reading x) (id (Reading.Value 42))", 1, Bytecode => False);
   Run ("(type Local (enum Other)) (Reading.Value 42)", 1);
   declare
      R : CCL.Language.Interpretation_Result;
   begin
      CCL.Language.Interpret ("Reading.Unavailable", 4096, Empty_Catalog, R);
      Check (R.Status /= CCL.Language.Succeeded and not R.Has_Value);
      CCL.Language.Interpret ("(type Reading (enum Fake)) Reading.Fake", 4096, Catalog, R);
      Check (R.Status = CCL.Language.Parse_Failed and not R.Has_Value);
      CCL.Language.Interpret ("(Reading.Value true)", 4096, Catalog, R);
      Check (R.Status = CCL.Language.Type_Check_Failed and not R.Has_Value);
   end;
   -- Type/operation discovery alone must not turn into a runnable host import.
   declare
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation : CCL.Catalog.Operation_Descriptor;
      Error : CCL.Catalog.Catalog_Error;
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Linked : CCL.Catalog.Link_Result;
   begin
      CCL.Catalog.Define_Interface ("settings", 1, 0, [1, 2, 3, 4], Descriptor, Error);
      Check (Error = CCL.Catalog.Catalog_Valid);
      CCL.Catalog.Define_Operation ("read", 0, (others => <>), Operation, Error);
      Check (Error = CCL.Catalog.Catalog_Valid);
      CCL.Catalog.Add_Operation (Descriptor, Operation, Error); Check (Error = CCL.Catalog.Catalog_Valid);
      CCL.Catalog.Publish (Catalog, Descriptor, Error); Check (Error = CCL.Catalog.Catalog_Valid);
      CCL.Language.Analyze ("(settings.read)", Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      CCL.Catalog.Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked);
      Check (Linked = CCL.Catalog.Authority_Not_Granted);
      CCL.Catalog.Initialize (Catalog);
      Check (Last (CCL.Catalog.Visible_Types (Catalog)) = Unit_Type);
   end;
   Ada.Text_IO.Put_Line ("Catalog type discovery/frontend/VM: PASS" & Checks'Image & " checks");
end Catalog_Tests;
