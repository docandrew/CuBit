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
      if B.Status /= Converted then
         Put_Line (Source & " -> " & A.Rendered.Data (1 .. A.Rendered.Length) &
                   " -> " & B.Status'Image & " " & B.Diagnostic'Image);
      end if;
      pragma Assert (B.Status = Converted);
      if A.Output_Nodes /= B.Input_Nodes or else A.Canonical /= B.Canonical then
         Put_Line ("MISMATCH " & Source);
         for I in A.Output_Nodes'Range loop
            if A.Output_Nodes (I) /= B.Input_Nodes (I) then
               Put_Line ("  node" & I'Image & " rendered" & A.Output_Nodes (I).First'Image &
                         A.Output_Nodes (I).After_Last'Image & " reparsed" &
                         B.Input_Nodes (I).First'Image & B.Input_Nodes (I).After_Last'Image);
               exit;
            end if;
         end loop;
         Put_Line ("  A canonical: " & A.Canonical.Data (1 .. A.Canonical.Length));
         Put_Line ("  B canonical: " & B.Canonical.Data (1 .. B.Canonical.Length));
      end if;
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
   procedure Truth_Is (Source : String; Expected : Boolean) is
      View : Conversion;
      Outcome : CCL.Language.Interpretation_Result;
   begin
      Convert (Source, Basic, Lisp, Catalog, View);
      pragma Assert (View.Status = Converted);
      CCL.Language.Interpret (View.Canonical.Data (1 .. View.Canonical.Length), 4096, Outcome);
      pragma Assert (Outcome.Status = CCL.Language.Succeeded);
      pragma Assert (Outcome.Result_Value.Boolean = Expected);
   end Truth_Is;
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
   Check ("(define (answer) Integer 42) (answer)");
   Check ("(define (seconds (ms Integer)) Integer (/ ms 1000)) (seconds 3661000)");
   Check ("(define (a (s String)) String s) (define (b (s String)) String (a s)) (b ""Cubie"")");
   Check ("(define (FUNCTION (AS Integer) (RETURN Integer)) Integer (+ AS RETURN)) (FUNCTION 20 22)");
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
   --  Subtraction, comparisons and connectives, both dialects.
   Check ("(- 50 8)");
   Check ("(< (- 10 3) (* 2 4))");
   Check ("(or (and (<= 1 2) (/= 3 4)) (>= 5 6))");
   Check ("(let ((sort-by 3)) (- sort-by 1))");
   Check ("50 - 8", Basic);
   Check ("LET a = 1 IN a <> 2 AND a >= 0 OR not(a = 1) END", Basic);
   Check ("(1 < 2) = (3 > 2)", Basic);
   Value_Is ("1 - 2", -1);
   Value_Is ("10 - 3 - 2", 5);
   Value_Is ("10 - (3 - 2)", 9);
   Value_Is ("2 * 3 - 1", 5);
   Value_Is ("LET sort-by = 3 IN sort-by - 1 END", 2);
   Truth_Is ("1 < 2 AND 3 > 2", True);
   Truth_Is ("1 > 2 OR 2 >= 2", True);
   Truth_Is ("1 <> 1", False);
   Truth_Is ("1<2", True);
   Truth_Is ("5 - 3 <= 2", True);
   Truth_Is ("true OR 1 / 0 = 1", True);
   Truth_Is ("false AND 1 / 0 = 1", False);
   --  Lists: [a b c] in Lisp, [a, b, c] in BASIC, the same node.
   Check ("[1 2 3]");
   Check ("(length [(- 5 1) 2])");
   Check ("(at [""a"" ""b""] 2)");
   Check ("[1, 2 + 3, 4 * 5]", Basic);
   Check ("LET xs = [10, 20, 30] IN length(xs) + at(xs, 2) END", Basic);
   Value_Is ("length([1, 2, 3])", 3);
   --  Typed empty lists and literals longer than one syntax node's chunk.
   Check ("(list-of Integer)");
   Check ("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18]");
   Value_Is ("length(list-of(Integer))", 0);
   Value_Is ("length([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18])", 18);
   Check ("(type C (record (a Integer) (s String))) 1");
   Check ("(type C (record (a Integer) (s String))) [(C 1 ""x"") (C 2 ""y"")]");
   Check ("(type R (record (xs (List Integer)))) (R (list-of Integer))");
   Check ("(type Priority (range 1 10)) (type Neg (range -5 -1)) " &
          "(type L (record (p Priority) (n Neg))) (L 3 -2)");
   Check ("TYPE Small = RANGE 0 TO 3" & ASCII.LF & "1", Basic);
   Check ("(type Launch (record (name String) (after (List Launch)))) " &
          "(Launch ""b"" [(Launch ""a"" (list-of Launch))])");
   Value_Is ("at([7, 8, 9], 3) - 1", 8);
   Reject ("[1, 2");
   Reject ("[1 2]");
   --  First-class functions: function-typed parameters, named functions as
   --  values, calls through values.
   Check ("(define (double (x Integer)) Integer (* x 2)) " &
          "(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x)) " &
          "(apply double 21)");
   Check ("FUNCTION inc(x AS Integer) AS Integer RETURN x + 1 END " &
          "FUNCTION twice(f AS FUNCTION(Integer) AS Integer, x AS Integer) AS Integer RETURN f(f(x)) END " &
          "twice(inc, 5)", Basic);
   Value_Is ("FUNCTION inc(x AS Integer) AS Integer RETURN x + 1 END " &
             "FUNCTION twice(f AS FUNCTION(Integer) AS Integer, x AS Integer) AS Integer RETURN f(f(x)) END " &
             "twice(inc, 5)", 7);
   --  Anonymous functions: (fn ((x T)) body) / FUNCTION(x AS T) body.
   Check ("(let ((twice (fn ((n Integer)) (+ n n)))) (twice 21))");
   Check ("(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x)) " &
          "(apply (fn ((n Integer)) (* n n)) 9)");
   Check ("LET twice = FUNCTION(n AS Integer) n + n IN twice(21) END", Basic);
   Check ("FUNCTION apply(f AS FUNCTION(Integer) AS Integer, x AS Integer) AS Integer RETURN f(x) END " &
          "apply(FUNCTION(n AS Integer) n * n, 9)", Basic);
   Value_Is ("FUNCTION apply(f AS FUNCTION(Integer) AS Integer, x AS Integer) AS Integer RETURN f(x) END " &
             "apply(FUNCTION(n AS Integer) n * n, 9)", 81);
   Value_Is ("LET twice = FUNCTION(n AS Integer) n + n IN twice(21) END", 42);
   --  List builtins: (each f xs) / each(f, xs), collection last.
   Check ("(each (fn ((n Integer)) (* n n)) [1 2 3])");
   Check ("(fold (fn ((a Integer) (n Integer)) (+ a n)) 0 (range 1 10))");
   Check ("(sum (where (fn ((n Integer)) (> n 2)) [1 2 3 4]))");
   Check ("(first 2 (range 1 5))");
   Check ("each(FUNCTION(n AS Integer) n * n, [1, 2, 3])", Basic);
   Check ("sum(where(FUNCTION(n AS Integer) n MOD 2 = 0, range(1, 10)))", Basic);
   Check ("any(FUNCTION(n AS Integer) n > 3, [1, 5])", Basic);
   Value_Is ("sum(where(FUNCTION(n AS Integer) n MOD 2 = 0, range(1, 10)))", 30);
   Value_Is ("fold(FUNCTION(a AS Integer, n AS Integer) a * n, 1, range(1, 5))", 120);
   --  List types in declarations: (List T) / LIST(T); captures need no syntax.
   Check ("(define (scale (k Integer) (xs (List Integer))) (List Integer) (each (fn ((n Integer)) (* n k)) xs)) (scale 10 [1 2])");
   Check ("FUNCTION scale(k AS Integer, xs AS LIST(Integer)) AS LIST(Integer) RETURN each(FUNCTION(n AS Integer) n * k, xs) END " &
          "scale(10, [1, 2])", Basic);
   Value_Is ("FUNCTION total(k AS Integer, xs AS LIST(Integer)) AS Integer RETURN sum(each(FUNCTION(n AS Integer) n * k, xs)) END " &
             "total(10, [1, 2])", 30);
   --  Strings and list builtins, round 2: hyphenated names are ordinary calls.
   Check ("(join "", "" (sort (split """" ""pear apple fig"")))");
   Check ("(starts-with ""ap"" (lower (trim "" APPLE "")))");
   Check ("join("", "", sort-by(FUNCTION(s AS String) length(s), split("" "", ""pear fig apple"")))", Basic);
   Check ("parse-int(replace("","", """", ""1,234""))", Basic);
   Value_Is ("parse-int(replace("","", """", ""1,234"")) + 1", 1235);
   Value_Is ("count(FUNCTION(w AS String) starts-with(""a"", w), split("""", ""an apple a day""))", 3);
   Value_Is ("max(reverse([3, 9, 4]))", 9);
   --  Untyped parameters print as written in both dialects.
   Check ("(each (fn (n) (* n n)) [1 2 3])");
   Check ("each(FUNCTION(w) length(w), split("""", ""a bb ccc""))", Basic);
   Check ("fold(FUNCTION(acc, n) acc + n, 0, range(1, 10))", Basic);
   Value_Is ("fold(FUNCTION(acc, n) acc + n, 0, range(1, 10))", 55);
   --  Pipelines: each stage takes the piped value last; printed as written.
   Check ("(->> (range 1 20) (where (fn (n) (= (mod n 3) 0))) sum)");
   Check ("(->> ""hello"" upper reverse)");
   Check ("range(1, 20) | where(FUNCTION(n) n MOD 3 = 0) | sum", Basic);
   Check ("split("""", ""a bb c"") | sort-by(FUNCTION(w) length(w)) | first(2) | length", Basic);
   Value_Is ("range(1, 20) | where(FUNCTION(n) n MOD 3 = 0) | sum", 63);
   Value_Is ("[3, 1, 2] | sort | reverse | first(1) | sum", 3);
   Reject ("1 <");
   Reject ("1 AND");
   declare
      use type CCL.Catalog.Grant_Result;
      use type CCL.VM.Value_Kind;
      type Reply_Mode is (Normal, With_Tag, Noncopyable, Variant, Wrong_Kind, Failed);
      type Host_State is record
         Calls : Natural := 0;
         Mode : Reply_Mode := Normal;
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
         if Binding = 78 then
            Success := Argument.Kind = CCL.VM.Boolean_Value;
            Value := CCL.VM.Boolean_Constant (Argument.Boolean);
         end if;
         case Context.Mode is
            when Normal => null;
            when With_Tag => Value.Type_Tag := 1;
            when Noncopyable => Value.Copyable := False;
            when Variant => Value.Kind := CCL.VM.Variant_Value;
            when Wrong_Kind => Value := CCL.VM.Boolean_Constant (True);
            when Failed => Success := False;
         end case;
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
      -- Scalar-copy imports must not silently erase ownership metadata or
      -- reinterpret a nominal variant as a primitive. Failed calls export no
      -- value; a plain but wrong primitive retains the type-mismatch outcome.
      for Mode in Reply_Mode range With_Tag .. Failed loop
         Host.Mode := Mode;
         Run ("(clock.monotonic-ms)", 4096, Catalog, Grants, Host, Outcome);
         pragma Assert
           (Outcome.Status = (if Mode = Wrong_Kind then CCL.Language.Host_Result_Type_Mismatch
                              else CCL.Language.Host_Call_Failed));
         pragma Assert (not Outcome.Has_Value);
      end loop;
      declare
         Interface_Item : CCL.Catalog.Interface_Descriptor;
         Method : CCL.Catalog.Operation_Descriptor;
         use type CCL.Catalog.Catalog_Error;
      begin
         Host.Mode := Normal;
         CCL.Catalog.Define_Interface ("scalar", 1, 0, [1, 2, 3, 4], Interface_Item, Error);
         pragma Assert (Error = CCL.Catalog.Catalog_Valid);
         CCL.Catalog.Define_Operation
           ("echo", 1, (Argument => CCL.VM.Boolean_Value, Result => CCL.VM.Boolean_Value,
                        Authority => CCL.VM.Observe_Authority, others => <>), Method, Error);
         pragma Assert (Error = CCL.Catalog.Catalog_Valid);
         CCL.Catalog.Add_Operation (Interface_Item, Method, Error);
         pragma Assert (Error = CCL.Catalog.Catalog_Valid);
         CCL.Catalog.Publish (Catalog, Interface_Item, Error);
         pragma Assert (Error = CCL.Catalog.Catalog_Valid);
         CCL.Catalog.Resolve (Catalog, "scalar.echo", Operation, Found);
         pragma Assert (Found);
         CCL.Catalog.Install (Grants, Operation, 78, Grant);
         pragma Assert (Grant = CCL.Catalog.Grant_Added);
         for Expected in Boolean loop
            Run ("(define (echo (x Boolean)) Boolean (scalar.echo x)) " &
                 "(echo " & (if Expected then "true" else "false") & ")",
                 4096, Catalog, Grants, Host, Outcome);
            pragma Assert (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Value);
            pragma Assert (Outcome.Result_Value.Kind = CCL.VM.Boolean_Value and then
                           Outcome.Result_Value.Boolean = Expected);
            pragma Assert (Outcome.Result_Value.Copyable and Outcome.Result_Value.Type_Tag = 0);
         end loop;
      end;
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
