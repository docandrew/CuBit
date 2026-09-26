with Ada.Text_IO; use Ada.Text_IO;
with CCL.Language; use CCL.Language;
with CCL.Language.Views; use CCL.Language.Views;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Sessions;

procedure Enum_Tests is
   use type CCL.Compiler.Compilation_Status;
   Catalog : CCL.Catalog.Interface_Catalog;
   R : Interpretation_Result;
   A, B : Conversion;
   Compiled : CCL.Compiler.Compilation_Result;
   Analysis : Analysis_Result;
   Prefix : constant String := "(type Color (enum Red Blue Green)) ";
   procedure Check (Source, Expected : String) is
   begin
      Interpret (Source, 4096, R);
      if R.Status /= Succeeded then Put_Line (CCL.Sessions.Result_Image (R)); end if;
      pragma Assert (R.Status = Succeeded);
      pragma Assert (CCL.Sessions.Result_Image (R) = Expected);
      Convert (Source, Lisp, Basic, Catalog, A);
      pragma Assert (A.Status = Converted);
      Convert (A.Rendered.Data (1 .. A.Rendered.Length), Basic, Lisp, Catalog, B);
      pragma Assert (B.Status = Converted and A.Canonical = B.Canonical);
      Interpret (B.Canonical.Data (1 .. B.Canonical.Length), 4096, R);
      pragma Assert (CCL.Sessions.Result_Image (R) = Expected);
   end Check;
   procedure Reject (Source : String; Code : Diagnostic_Code) is
   begin
      Interpret (Source, 4096, R);
      if R.Diagnostic /= Code then
         Put_Line (Source & " -> " & R.Diagnostic'Image & "; expected " & Code'Image);
      end if;
      pragma Assert (R.Status in Parse_Failed | Type_Check_Failed);
      pragma Assert (R.Diagnostic = Code and not R.Has_Value);
      pragma Assert (R.Fuel_Remaining = 4096);
   end Reject;
begin
   CCL.Catalog.Initialize (Catalog);
   Check (Prefix & "Color.Red", "Color.Red");
   pragma Assert (not Has_Scalar (R));
   Check (Prefix & "(to-string Color.Green)", "String: Green");
   Check (Prefix & "(= Color.Red Color.Red)", "Boolean: true");
   Check (Prefix & "(= Color.Red Color.Blue)", "Boolean: false");
   Check (Prefix & "(let ((x Color.Blue)) (if (= x Color.Red) Color.Green x))", "Color.Blue");
   Check (Prefix & "(define (identity (x Color)) Color x) (identity Color.Green)", "Color.Green");
   Check ("(type Color (variant (Red) (Blue) (Green))) Color.Red", "Color.Red");
   Check (Prefix & "(define (color) Color Color.Red) (type Size (enum Small Large)) (color)", "Color.Red");
   Check ("(type TYPE (enum RETURN END)) TYPE.RETURN", "TYPE.RETURN");
   Check ("(type Color (enum A B C D E F G H I J K L M N O P)) Color.P", "Color.P");
   Check (Prefix & "(define (choose (color Color)) Color " &
     "(if (= color Color.Red) Color.Green Color.Blue)) (choose Color.Red)", "Color.Green");
   Reject (Prefix & "Red", Unknown_Name);
   Reject (Prefix & "Color.Yellow", Unknown_Name);
   Reject (Prefix & "(+ Color.Red 1)", Expected_Integer);
   Reject (Prefix & "(= Color.Red 0)", Expected_Comparable);
   Reject (Prefix & "(type Other (enum Red)) (= Color.Red Other.Red)", Expected_Comparable);
   Reject (Prefix & "(if true Color.Red 0)", Branch_Type_Mismatch);
   Reject (Prefix & "(define (id (x Color)) Color x) (id 0)", Function_Argument_Mismatch);
   Reject (Prefix & "(define (bad) Integer Color.Red) (bad)", Function_Result_Mismatch);
   Reject ("(define (bad) Integer Color.Red) " & Prefix & "(bad)", Unknown_Name);
   Reject ("(define (bad) Color Color.Red) " & Prefix & "(bad)", Expected_Type_Name);
   Reject ("(type Color (enum)) 0", Invalid_Type_Declaration);
   Reject ("(type Color (enum Red Red)) 0", Invalid_Type_Declaration);
   Reject (Prefix & "(type Color (enum Red)) 0", Invalid_Type_Declaration);
   Reject ("(type Integer (enum Zero)) 0", Invalid_Type_Declaration);
   Reject ("(type Bad.Name (enum Red)) 0", Invalid_Type_Declaration);
   Reject ("(type Color (enum A B C D E F G H I J K L M N O P Q)) 0", Invalid_Type_Declaration);
   Reject ("(type VeryLongTypeNameForAnEnumeration (enum Red)) 0", Invalid_Type_Declaration);
   Reject ("(type Color (enum Red", Expected_Close);
   -- Strings are now ordinary payloads; live callbacks remain nonpersistable.
   Reject ("(type Color (variant (Red Handler))) 0", Expected_Type_Name);
   Reject ("(type Color (record (Red Integer) (Red Boolean))) 0", Invalid_Type_Declaration);
   Reject ("(type Color (enum Red)) (Color.Red)", Unknown_Form);
   -- Every truncated declaration must fail without an unchecked exception.
   for Last in 0 .. Prefix'Length loop
      Interpret (Prefix (1 .. Last), 4096, R);
      pragma Assert (R.Status = Parse_Failed and not R.Has_Value);
   end loop;
   Interpret (Prefix & "Color.Red", 1, R);
   pragma Assert (R.Status = Evaluation_Fuel_Exhausted and not R.Has_Value);
   Interpret (Prefix & "Color.Red", 2, R);
   pragma Assert (R.Status = Succeeded and R.Fuel_Remaining = 0);
   Analyze (Prefix & "Color.Red", Analysis);
   CCL.Compiler.Compile (Analysis, Compiled);
   pragma Assert (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   Put_Line ("CCL enums: nominal typing, functions, equality, BASIC roundtrip, rejection PASS");
end Enum_Tests;
