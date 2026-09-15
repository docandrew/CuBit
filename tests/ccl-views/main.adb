with Ada.Text_IO; use Ada.Text_IO;
with CCL.Catalog;
with CCL.Language.Views; use CCL.Language.Views;
with CCL.Interfaces.Clock;
with CCL.VM;
with Interfaces;

procedure Main is
   use type CCL.Language.Interpretation_Status;
   use type Interfaces.Integer_64;
   Catalog : CCL.Catalog.Interface_Catalog;
   Error : CCL.Catalog.Catalog_Error;
   Count : Natural := 0;
   procedure Check (Source : String; Style : Surface := Lisp) is
      A, B, C : Conversion;
   begin
      Convert (Source, Style, (if Style = Lisp then Basic else Lisp), Catalog, A);
      if A.Status /= Converted then
         Put_Line (Source & " -> " & A.Status'Image & " " & A.Diagnostic'Image);
      end if;
      pragma Assert (A.Status = Converted);
      Convert (A.Rendered.Data (1 .. A.Rendered.Length),
        (if Style = Lisp then Basic else Lisp), Style, Catalog, B);
      pragma Assert (B.Status = Converted);
      pragma Assert (A.Output_Nodes = B.Input_Nodes);
      pragma Assert (A.Canonical = B.Canonical);
      Convert (B.Rendered.Data (1 .. B.Rendered.Length), Style,
        (if Style = Lisp then Basic else Lisp), Catalog, C);
      pragma Assert (C.Status = Converted);
      pragma Assert (A.Rendered = C.Rendered);
      Count := Count + 1;
   end Check;
   R : Conversion;
   procedure Value_Is (Source : String; Expected : Interfaces.Integer_64) is
      View : Conversion;
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Convert (Source, Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Converted);
      CCL.Language.Interpret (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Succeeded);
      pragma Assert (Outcome.Result_Value.Integer = Expected);
   end Value_Is;
   procedure Reject (Source : String) is
      View : Conversion;
   begin
      Convert (Source, Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Invalid_Source);
   end Reject;
begin
   CCL.Catalog.Initialize (Catalog);
   CCL.Interfaces.Clock.Publish (Catalog, Error);
   Check ("""hello world""");
   Check ("42");
   Check ("-9223372036854775808");
   Check ("(let ((name ""Cubie"")) (concat ""Hello, "" name))");
   Check ("(let ((LET 42)) LET)");
   Check ("(let ((x=y 42)) x=y)");
   Check ("(if (= (mod (* 20 3) 7) 4) (/ 8 2) (+ 1 2))");
   Check ("(not false)");
   Check ("(let ((elapsed-ms 3661000)) (% elapsed-ms 60000))");
   Convert ("(mod 3661000 60000)", Lisp, Lisp, Catalog, R);
   pragma Assert (R.Status = Converted);
   pragma Assert (R.Rendered.Data (1 .. R.Rendered.Length) = "(mod 3661000 60000)");
   Convert ("mod(3661000, 60000)", Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Converted);
   pragma Assert (R.Rendered.Data (1 .. R.Rendered.Length) = "(mod 3661000 60000)");
   Convert ("(% 3661000 60000)", Lisp, Lisp, Catalog, R);
   pragma Assert (R.Status = Converted);
   pragma Assert (R.Rendered.Data (1 .. R.Rendered.Length) = "(mod 3661000 60000)");
   Check ("(at (concat ""he"" ""llo"") 1)");
   Check ("(length ""a\n\t\""b\\c"")");
   Check ("(to-string (clock.monotonic-ms))");
   Check ("# before" & ASCII.LF & "(+ 1 # middle" & ASCII.LF & "2) # after");
   Check ("LET name = concat(""Cub"", ""ie"") IN concat(""Hello "", name) END", Basic);
   Check ("if(equal(1, 1), 42, 0)", Basic);
   Check ("20+2*11", Basic);
   Check ("(20+2)*11", Basic);
   Check ("20/(2/2)", Basic);
   Check ("20/2/2", Basic);
   Check ("(20 MOD 7) / 2", Basic);
   Check ("20 MOD (7 / 2)", Basic);
   Check ("1 + (2 + 3)", Basic);
   Check ("(1 + 2) + 3", Basic);
   Check ("IF 1 + 2 * 3 = 7 THEN 42 ELSE 0 END", Basic);
   Check ("IF true THEN IF false THEN 1 ELSE 2 END ELSE 3 END", Basic);
   Check ("10 * IF true THEN 2 ELSE 3 END + 1", Basic);
   Check ("LET text = ""7"" IN IF length(text) = 1 THEN concat(""0"", text) ELSE text END END", Basic);
   Check ("(let ((IF 1)) (let ((x+y IF)) (+ x+y IF)))");
   Check ("(let ((MODulus 1)) (let ((MOD MODulus)) (mod MOD 3)))");
   Check ("(let ((answer (+ 1 # arithmetic comment" & ASCII.LF &
     "2))) (if (= answer 3) answer 0))");
   Value_Is ("20 + 2 * 11", 42);
   Value_Is ("(20 + 2) * 11", 242);
   Value_Is ("20 / (2 / 2)", 20);
   Value_Is ("20 / 2 / 2", 5);
   Value_Is ("3661000 MOD 60000 / 1000", 1);
   Value_Is ("IF true THEN 42 ELSE 1 / 0 END", 42);
   Value_Is ("IF false THEN 1 / 0 ELSE 42 END", 42);
   Value_Is ("IF true THEN 2 ELSE 3 END*21", 42);
   Value_Is ("9223372036854775807 + (1 + -1)", Interfaces.Integer_64'Last);
   Reject ("IF true THEN 42 END");
   Reject ("IF true THEN 42 ELSE false END");
   Reject ("IF 1 THEN 42 ELSE 0 END");
   Reject ("IF true THEN42 ELSE 0 END");
   Reject ("IF true THEN 42 ELSE 0 ENDless");
   Reject ("1 +");
   Reject ("1 / / 2");
   Reject ("(1 + 2");
   Reject ("1 MODulus 2");
   Reject ("1 - 2"); -- subtraction is not in the core language yet
   declare
      use type CCL.Catalog.Grant_Result;
      use type CCL.VM.Value_Kind;
      type Host_State is record
         Calls : Natural := 0;
      end record;
      procedure Invoke
        (Context : in out Host_State; Binding : Interfaces.Unsigned_32;
         Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean) is
         use type Interfaces.Unsigned_32;
      begin
         Context.Calls := Context.Calls + 1;
         Value := CCL.VM.Integer_Constant (Interfaces.Integer_64 (Context.Calls));
         Success := Binding = 77 and then Argument.Kind = CCL.VM.Integer_Value and then
           Argument.Integer = 0;
      end Invoke;
      procedure Run is new CCL.Language.Interpret_With_Host (Host_State, Invoke);
      Grants : CCL.Catalog.Granted_Bindings;
      Operation : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
      Grant : CCL.Catalog.Grant_Result;
      Host : Host_State;
      View : Conversion;
      Outcome : CCL.Language.Interpretation_Result;
   begin
      CCL.Catalog.Initialize (Grants);
      Convert ("IF true THEN clock.monotonic-ms() * 10 + clock.monotonic-ms() ELSE 0 END",
        Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Converted and Host.Calls = 0);
      Run (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Catalog, Grants, Host, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Host_Authority_Denied and Host.Calls = 0);
      CCL.Catalog.Resolve (Catalog, "clock.monotonic-ms", Operation, Found);
      pragma Assert (Found);
      CCL.Catalog.Install (Grants, Operation, 77, Grant);
      pragma Assert (Grant = CCL.Catalog.Grant_Added);
      Run (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Catalog, Grants, Host, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Succeeded and Host.Calls = 2);
      pragma Assert (Outcome.Result_Value.Integer = 12); -- left-to-right, once each
      Convert ("IF false THEN clock.monotonic-ms() ELSE 42 END", Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Converted);
      Run (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Catalog, Grants, Host, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Succeeded and Host.Calls = 2);
      pragma Assert (Outcome.Result_Value.Integer = 42);
   end;
   declare
      View : Conversion;
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Convert ("(9223372036854775807 + 1) + -1", Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Converted);
      CCL.Language.Interpret (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Evaluation_Overflow);
   end;
   Check ("(let ((x 1)) (let ((x (+ x 1))) (if (= x 2) x 0)))");
   Check ("(concat ""a long string that should force a line break in its containing call"" ""another string"")");
   Convert ("LET name = ""Cubie"" IN concat(""Hello, "", name) END",
     Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Converted);
   pragma Assert (R.Rendered.Data (1 .. R.Rendered.Length) =
     "(let ((name ""Cubie""))" & ASCII.LF & "  (concat ""Hello, "" name))");
   Convert ("(if true (+ 20 22) 0)", Lisp, Lisp, Catalog, R);
   pragma Assert (R.Rendered.Data (1 .. R.Rendered.Length) =
     "(if true" & ASCII.LF & "  (+ 20 22)" & ASCII.LF & "  0)");
   --  Formatting is idempotent and changes neither executable text nor
   --  node numbering; the latter is used to remap a paused debugger.
   declare
      Again : Conversion;
   begin
      Convert (R.Rendered.Data (1 .. R.Rendered.Length), Lisp, Lisp, Catalog, Again);
      pragma Assert (Again.Status = Converted);
      pragma Assert (Again.Rendered = R.Rendered and Again.Canonical = R.Canonical);
      pragma Assert (Again.Input_Nodes = R.Output_Nodes);
   end;
   Convert ("(concat ""unfinished""", Lisp, Basic, Catalog, R);
   pragma Assert (R.Status = Invalid_Source and R.Rendered.Length = 0);
   Convert ("LET x = 1 IN add(x, 2)", Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Invalid_Source and R.Rendered.Length = 0);
   Convert ("concat(1, 2)", Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Invalid_Source);
   Convert ("unknown.call()", Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Invalid_Source);
   Convert ("LET `x)) (+ 1 2` = 1 IN 2 END", Basic, Lisp, Catalog, R);
   pragma Assert (R.Status = Invalid_Source);
   Put_Line ("PASS: CCL syntax views round trips" & Count'Image & " + rejection tests");
end Main;
