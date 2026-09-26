with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Compiler;
with CCL.Host_Values;
with CCL.Interfaces.Clock;
with CCL.Interfaces.Workbench_UI;
with CCL.Language; use CCL.Language;
with CCL.Language.Views;
with CCL.UI_Labels;

procedure Function_Tests is
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Language.Views.Conversion_Status;
   use type CCL.Language.Views.Text;
   type Host_State is record
      Calls : Natural := 0;
      Label : CCL.UI_Labels.Model;
   end record;
   procedure Invoke
     (Context : in out Host_State; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result) is
   begin
      Context.Calls := Context.Calls + 1;
      Reply.Success := True;
      if Binding = 77 then
         Reply.Value := CCL.Host_Values.Integer_Constant (3661000);
      elsif Binding = 78 then
         CCL.UI_Labels.Apply_Value (Context.Label, CCL.UI_Labels.Set_Text, Argument, Reply.Success);
         Reply.Value := CCL.Host_Values.Boolean_Constant (Reply.Success);
      else
         Reply.Value := CCL.Host_Values.Boolean_Constant (False);
         Reply.Success := False;
      end if;
   end Invoke;
   procedure Run_Host is new Interpret_With_Values (Host_State, Invoke);
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Error : Catalog_Error;
   Resolved : Resolved_Operation;
   Granted : Grant_Result;
   Found : Boolean;
   Host : Host_State;
   Outcome : Interpretation_Result;
   Checked : Natural := 0;
   Clock_Label : constant String :=
     "(define (seconds (ms Integer)) Integer (/ ms 1000)) " &
     "(define (label (s Integer)) String (concat ""Uptime seconds: "" (to-string s))) " &
     "(ui.label-text (label (seconds (clock.monotonic-ms))))";
   procedure Check (Source : String; Expected : Integer_64) is
   begin
      Interpret (Source, 4096, Outcome);
      if Outcome.Status /= Succeeded then
         Put_Line (Source & " -> " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
      pragma Assert (Outcome.Status = Succeeded and Outcome.Has_Value);
      pragma Assert (Outcome.Result_Value.Integer = Expected);
      Checked := Checked + 1;
   end Check;
   procedure Reject (Source : String; Code : Diagnostic_Code) is
   begin
      Run_Host (Source, 4096, Catalog, Grants, Host, Outcome);
      if Outcome.Diagnostic /= Code then
         Put_Line (Source & " -> " & Outcome.Status'Image & " " & Outcome.Diagnostic'Image);
      end if;
      pragma Assert (Outcome.Status in Parse_Failed | Type_Check_Failed);
      pragma Assert (Outcome.Diagnostic = Code and Host.Calls = 0);
      Checked := Checked + 1;
   end Reject;
begin
   Initialize (Catalog); Initialize (Grants);
   CCL.Interfaces.Clock.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   CCL.Interfaces.Workbench_UI.Publish (Catalog, Error); pragma Assert (Error = Catalog_Valid);
   Resolve (Catalog, "clock.monotonic-ms", Resolved, Found); pragma Assert (Found);
   Install (Grants, Resolved, 77, Granted); pragma Assert (Granted = Grant_Added);
   Check ("(define (answer) Integer 42) (answer)", 42);
   Check ("(define (sum (x Integer) (y Integer)) Integer (+ x y)) (sum 20 22)", 42);
   Check ("(define (id (x Integer)) Integer x) (let ((x 40)) (+ (id 2) x))", 42);
   Check ("(define (id (x Integer)) Integer x) (id (id 42))", 42);
   Check ("(define (id (x Integer)) Integer x) " &
          "(define (sum (x Integer) (y Integer)) Integer (+ (id x) y)) (sum 20 22)", 42);
   Check ("(define (inc (x Integer)) Integer (+ x 1)) " &
          "(define (twice (x Integer)) Integer (inc (inc x))) (twice 40)", 42);
   Check ("(define (choose (b Boolean) (x Integer) (y Integer)) Integer (if b x y)) " &
          "(choose false 20 42)", 42);
   Check ("(define (unused) Integer (/ 1 0)) 42", 42);
   Check ("(define (f (a Integer) (b Integer) (c Integer) (d Integer) (e Integer) " &
          "(f Integer) (g Integer) (h Integer)) Integer (+ a h)) (f 20 0 0 0 0 0 0 22)", 42);
   Interpret ("(define (hello (s String)) String (concat ""Hi "" s)) " &
              "(hello ""Cubie"")", 4096, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Outcome.Has_Text);
   pragma Assert (Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length) = "Hi Cubie");
   Interpret ("(define (id (c Character)) Character c) (id (at ""Cubie"" 1))", 4096, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Outcome.Has_Character and Outcome.Result_Character = 'C');
   Interpret ("(define (flip (b Boolean)) Boolean (not b)) (flip false)", 4096, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Outcome.Result_Value.Boolean);
   declare
      Half : constant String (1 .. MAX_TEXT_BYTES / 2) := [others => 'x'];
   begin
      Interpret ("(define (double (s String)) String (concat s s)) (double """ & Half & """)", 4096, Outcome);
      pragma Assert (Outcome.Status = Succeeded and Outcome.Has_Text and Outcome.Result_Text.Length = MAX_TEXT_BYTES);
   end;
   Reject ("(define (f (x Integer)) Integer x) (f true)", Function_Argument_Mismatch);
   Reject ("(define (f (x Integer)) Integer x) (f)", Function_Arity_Mismatch);
   Reject ("(define (f) Integer 1) (f (clock.monotonic-ms))", Function_Arity_Mismatch);
   Reject ("(define (f) Integer true) (clock.monotonic-ms)", Function_Result_Mismatch);
   Reject ("(define (f) Integer hidden) (let ((hidden 42)) (f))", Unknown_Name);
   Reject ("(define (f) Integer (f)) (f)", Unknown_Form);
   Reject ("(define (f) Integer (g)) (define (g) Integer 42) (f)", Unknown_Form);
   Reject ("(define (f) Integer 1) (define (f) Integer 2) (f)", Duplicate_Declaration);
   Reject ("(define (f (x Integer) (x Integer)) Integer x) (f 1 2)", Duplicate_Declaration);
   Reject ("(define (f (true Boolean)) Boolean true) (f false)", Duplicate_Declaration);
   Reject ("(define (f (123 Integer)) Integer 1) (f 2)", Duplicate_Declaration);
   Reject ("(define (concat) Integer 42) 42", Duplicate_Declaration);
   Reject ("(define (clock.monotonic-ms) Integer 42) 42", Duplicate_Declaration);
   Reject ("(define (f (x Pointer)) Integer 1) (f 1)", Expected_Type_Name);
   Reject ("(define (f) Integer 42)", Unexpected_End);
   Reject ("(define (f) Integer 42) 1 2", Trailing_Input);
   Reject ("(let ((x 1)) (define (f) Integer x))", Unknown_Form);
   Reject ("(define (f (a Integer) (b Integer) (c Integer) (d Integer) (e Integer) " &
           "(f Integer) (g Integer) (h Integer) (i Integer)) Integer a) 1", Too_Many_Parameters);
   declare
      Source : String (1 .. MAX_SOURCE_LENGTH) := [others => ' '];
      Length : Natural := 0;
      procedure Add (S : String) is
      begin Source (Length + 1 .. Length + S'Length) := S; Length := Length + S'Length; end Add;
   begin
      for I in 1 .. MAX_FUNCTIONS loop
         Add ("(define (f" & Character'Val (Character'Pos ('a') + I - 1) & ") Integer 42) ");
      end loop;
      Check (Source (1 .. Length) & "(fp)", 42);
      Reject (Source (1 .. Length) & "(define (extra) Integer 1) 42", Too_Many_Functions);
   end;
   Interpret ("(define (twice (x Integer)) Integer (+ x x)) (twice 21)", 0, Outcome);
   pragma Assert (Outcome.Status = Evaluation_Fuel_Exhausted and Outcome.Fuel_Remaining = 0);
   Interpret ("(define (bad (x Integer)) Integer (/ x 0)) (bad 1)", 4096, Outcome);
   pragma Assert (Outcome.Status = Evaluation_Division_By_Zero);
   declare
      Source : String (1 .. MAX_SOURCE_LENGTH) := [others => ' '];
      Length : Natural := 0;
      procedure Add (S : String) is
      begin Source (Length + 1 .. Length + S'Length) := S; Length := Length + S'Length; end Add;
   begin
      Add ("(define (fa) Integer 1) ");
      for I in 2 .. MAX_FUNCTIONS loop
         Add ("(define (f" & Character'Val (Character'Pos ('a') + I - 1) &
              ") Integer (+ 0 (f" & Character'Val (Character'Pos ('a') + I - 2) & "))) ");
      end loop;
      Interpret (Source (1 .. Length) & "(fp)", 4096, Outcome);
      pragma Assert (Outcome.Status = Evaluation_Depth_Exhausted);
      Interpret (Source (1 .. Length) & "(fp)", 20, Outcome);
      pragma Assert (Outcome.Status = Evaluation_Fuel_Exhausted);
   end;
   --  Even an unused definition is admitted before effects in the main expression.
   Run_Host ("(define (unused) Boolean (ui.label-text ""blocked"")) (clock.monotonic-ms)",
             4096, Catalog, Grants, Host, Outcome);
   pragma Assert (Outcome.Status = Host_Authority_Denied and Host.Calls = 0);
   Resolve (Catalog, "ui.label-text", Resolved, Found); pragma Assert (Found);
   Install (Grants, Resolved, 78, Granted); pragma Assert (Granted = Grant_Added);
   Run_Host (Clock_Label, 4096, Catalog, Grants, Host, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Host.Calls = 2);
   pragma Assert (CCL.UI_Labels.Image (Host.Label) = " Uptime seconds: 3661");
   Run_Host ("(define (f (x Integer)) Integer (+ x x)) (f (clock.monotonic-ms))",
             4096, Catalog, Grants, Host, Outcome);
   pragma Assert (Outcome.Status = Succeeded and Host.Calls = 3);
   pragma Assert (Outcome.Result_Value.Integer = 7322000); -- argument executed once
   declare
      A, B : CCL.Language.Views.Conversion;
      Compiled : CCL.Compiler.Compilation_Result;
      Analysis : Analysis_Result;
   begin
      CCL.Language.Views.Convert (Clock_Label, CCL.Language.Views.Lisp,
        CCL.Language.Views.Basic, Catalog, A);
      pragma Assert (A.Status = CCL.Language.Views.Converted);
      Put_Line (A.Rendered.Data (1 .. A.Rendered.Length));
      CCL.Language.Views.Convert (A.Rendered.Data (1 .. A.Rendered.Length),
        CCL.Language.Views.Basic, CCL.Language.Views.Lisp, Catalog, B);
      pragma Assert (B.Status = CCL.Language.Views.Converted and A.Canonical = B.Canonical);
      Run_Host (B.Canonical.Data (1 .. B.Canonical.Length), 4096, Catalog, Grants, Host, Outcome);
      pragma Assert (Outcome.Status = Succeeded and Host.Calls = 5);
      Analyze (Clock_Label, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      pragma Assert (Compiled.Status = CCL.Compiler.Unsupported_Form);
      --  Hostile editing fixtures: every truncation and delimiter replacement
      --  must return a bounded analysis result, never raise a host exception.
      for Last in 0 .. Clock_Label'Length loop
         Analyze (Clock_Label (1 .. Last), Catalog, Analysis);
      end loop;
      for Position in Clock_Label'Range loop
         for Replacement of String'("()""# " & ASCII.NUL) loop
            declare
               Edited : String := Clock_Label;
            begin
               Edited (Position) := Replacement;
               Analyze (Edited, Catalog, Analysis);
            end;
         end loop;
      end loop;
   end;
   Put_Line ("PASS: typed functions, isolated parameters, bounds, fuel, admission and BASIC roundtrip; checks" & Checked'Image);
end Function_Tests;
