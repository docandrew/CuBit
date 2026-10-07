with GNAT.Source_Info;
with CCL.Evaluation;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Catalog; use CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Language.Views;
with CCL.Compiler;
with CCL.VM;
with CCL.VM.Native_Objects;
with Config_Read_Outcomes;
with Config_Object_Messages;

procedure Read_Source_Tests is
   package L renames CCL.Language;
   package R renames Config_Read_Outcomes;
   package W renames Config_Object_Messages;
   use type L.Interpretation_Status;
   use type L.Analysis_Status;
   use type L.Views.Conversion_Status;
   use type CCL.VM.Value;
   use type CCL.VM.Execution_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Host_Values.Value;
   use type CCL.Objects.Catalog.Publication_Result;
   Types : Registry;
   Product_Type, Variant_Type : Type_Reference;
   Declared : Definition_Result;
   Contract : Binding;
   Definition : R.Description;
   Value : CCL.Objects.Image;
   Built : Build_Result;
   Good : Boolean;
   Catalog : Interface_Catalog;
   Grants, Read_Only, No_Grants : Granted_Bindings;
   Description : Interface_Descriptor;
   Operation : Operation_Descriptor;
   Error : Catalog_Error;
   Published : CCL.Objects.Catalog.Publication_Result;
   Resolved : Resolved_Operation;
   Installed : Grant_Result;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Line : Natural := GNAT.Source_Info.Line) is
   begin
      Checks := Checks + 1;
      if not OK then
         raise Program_Error with "CCL structured read check" & Checks'Image & " at line" & Line'Image;
      end if;
   end Check;
   type Context is record
      Code : W.Status := W.Success;
      Calls : Natural := 0;
      Writes : Natural := 0;
      Corrupt : Boolean := False;
   end record;
   State : Context;
   procedure Invoke
     (State : in out Context; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      Image : CCL.Objects.Image;
      Accepted : Boolean;
   begin
      if Binding = 78 then
         Check (Argument = CCL.Host_Values.Object_Constant (Value));
         State.Writes := State.Writes + 1;
         Reply := (Value => CCL.Host_Values.Integer_Constant (23), Success => True, Why => <>);
         return;
      end if;
      Check (Binding = 77 and Argument = CCL.Host_Values.Integer_Constant (0));
      State.Calls := State.Calls + 1;
      R.Build (Definition, True, State.Code, (if State.Code in W.Success | W.Stale then 42 else 0),
        Value, Image, Accepted);
      if State.Corrupt then Image.Reserved := 1; end if;
      Reply := (Value => CCL.Host_Values.Object_Constant (Image), Success => Accepted, Why => <>);
   end Invoke;
   procedure Evaluate is new CCL.Evaluation.Evaluate_With_Values (Context, Invoke);
   procedure Evaluate_Object is new CCL.Evaluation.Evaluate_Object_With_Values (Context, Invoke);
   function Source (Body_Text : String) return String is
     ("(match (config-test.read) ((ConfigRead.Found snapshot) " & Body_Text & ") " &
      "((ConfigRead.Stale snapshot) (field snapshot revision)) " &
      "((ConfigRead.Missing) 3) ((ConfigRead.Denied) 4) ((ConfigRead.Busy) 5) " &
      "((ConfigRead.Unavailable) 6) ((ConfigRead.SchemaMismatch) 7) " &
      "((ConfigRead.InvalidRequest) 8) ((ConfigRead.InvalidCompletion) 9))");
   Result : L.Interpretation_Result;
   Object_Result : L.Object_Interpretation_Result;
   Analysis : L.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Basic, Lisp : L.Views.Conversion;
   Empty_Catalog : Interface_Catalog;
   Wrong : Binding;
   procedure Pure (Program : String; Expected : Integer_64) is
   begin
      CCL.Evaluation.Evaluate (Program, 1024, Result);
      if Result.Status /= L.Succeeded then
         Ada.Text_IO.Put_Line (Program & " => " & Result.Status'Image & " / " & Result.Diagnostic'Image);
      end if;
      Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (Expected));
      L.Views.Convert (Program, L.Views.Lisp, L.Views.Basic, Empty_Catalog, Basic);
      Check (Basic.Status = L.Views.Converted);
      L.Views.Convert (Basic.Rendered.Data (1 .. Basic.Rendered.Length), L.Views.Basic, L.Views.Lisp, Empty_Catalog, Lisp);
      Check (Lisp.Status = L.Views.Converted);
      CCL.Evaluation.Evaluate (Lisp.Rendered.Data (1 .. Lisp.Rendered.Length), 1024, Result);
      Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (Expected));
      --  Compiled code that accepts the program agrees with the interpreter
      --  (some forms, such as anonymous functions, compile from step 6 on).
      L.Analyze (Program, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status = CCL.Compiler.Compilation_Succeeded then
         declare
            Checked : CCL.VM.Validated_Program;
            Error : CCL.VM.Validation_Error;
            Ran : CCL.VM.Execution_Result;
         begin
            CCL.VM.Verify (Compiled.Program, Checked, Error);
            Check (CCL.VM."=" (Error, CCL.VM.Valid));
            if CCL.VM."=" (Error, CCL.VM.Valid) then
               CCL.VM.Execute (Checked, 1024, Ran);
               Check (CCL.VM."=" (Ran.Status, CCL.VM.Completed) and then
                      CCL.VM."=" (Ran.Result_Value, CCL.VM.Integer_Constant (Expected)));
            end if;
         end;
      end if;
   end Pure;
   procedure Reject (Program : String) is
   begin
      CCL.Evaluation.Evaluate (Program, 1024, Result);
      Check (Result.Status in L.Parse_Failed | L.Type_Check_Failed and not Result.Has_Value);
   end Reject;
   procedure Compiled_Read
     (Body_Text : String; Code : W.Status; Expected : Integer_64;
      Write_Back : Boolean := False; Corrupt : Boolean := False)
   is
      package N renames CCL.VM.Native_Objects;
      Machine : N.Machine;
      Program : CCL.VM.Program;
      Checked : CCL.VM.Validated_Program;
      Validity : CCL.VM.Validation_Error;
      Linked : Link_Result;
      Step : CCL.VM.Execution_Result;
      Reply : CCL.Host_Values.Call_Result;
      Argument : CCL.Objects.Image;
      Exported : Boolean;
   begin
      L.Analyze (Source (Body_Text), Catalog, Analysis);
      Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      Program := Compiled.Program;
      -- Missing authority fails linking, before a read or write can escape.
      Link_Program (No_Grants, Compiled.Linkage, Program, Linked, Catalog);
      Check (Linked /= Link_Valid);
      if Write_Back then
         Link_Program (Read_Only, Compiled.Linkage, Program, Linked, Catalog);
         Check (Linked /= Link_Valid);
      end if;
      Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
      Check (Linked = Link_Valid);
      CCL.VM.Verify (Program, Checked, Validity);
      Check (Validity = CCL.VM.Valid);
      N.Initialize (Checked, 128, Machine);
      N.Continue_Execution_For (Checked, Machine, 128, Step);
      Check (Step.Status = CCL.VM.Waiting_For_Host and Step.Requested_Binding = 77);
      State := (Code => Code, Corrupt => Corrupt, others => <>);
      Invoke (State, Step.Requested_Binding, CCL.Host_Values.Integer_Constant (0), Reply);
      N.Complete_Object (Checked, Machine, R.Schema (Definition), Reply.Value.Object, Reply.Success);
      N.Continue_Execution_For (Checked, Machine, 128, Step);
      if Corrupt then
         Check (Step.Status = CCL.VM.Host_Call_Failed and not Step.Has_Value);
      else
         if Write_Back then
            Check (Step.Status = CCL.VM.Waiting_For_Host and Step.Requested_Binding = 78);
            N.Export_Argument (Checked, Machine, Contract, Argument, Exported);
            Check (Exported and Argument = Value);
            Invoke (State, Step.Requested_Binding, CCL.Host_Values.Object_Constant (Argument), Reply);
            N.Complete_Scalar (Checked, Machine, CCL.VM.Integer_Constant (Reply.Value.Integer), Reply.Success);
            N.Continue_Execution_For (Checked, Machine, 128, Step);
         end if;
         Check (Step.Status = CCL.VM.Completed and Step.Has_Value and
           Step.Result_Value = CCL.VM.Integer_Constant (Expected));
      end if;
      Check (State.Calls = 1 and State.Writes = (if Write_Back and not Corrupt then 1 else 0));
      N.Stop (Machine);
   end Compiled_Read;
begin
   Pure ("(type Note (variant (Text String) (Absent))) " &
     "(match (Note.Text ""hello"") ((Note.Text text) (length text)) ((Note.Absent) 0))", 5);
   Pure ("(type Pair (record (first Integer) (second String))) " &
     "(let ((p (Pair 42 ""hello""))) (+ (field p first) (length (field p second))))", 47);
   Pure ("(type Note (variant (Text String) (Absent))) " &
     "(type Pair (record (first Note) (second String))) " &
     "(let ((p (Pair (Note.Text ""hello"") ""world""))) " &
     "(match (field p first) ((Note.Text text) (+ (length text) (length (field p second)))) ((Note.Absent) 0)))", 10);
   Pure ("(type Pair (record (first Integer) (second String))) " &
     "(define (make (n Integer)) Pair (Pair n ""hello"")) (field (make 42) first)", 42);
   Pure ("(type Empty (record)) (let ((e (Empty))) 1)", 1);
   Pure ("(type ExtremelyLongNamedRecordType (record (longFieldName Integer))) " &
     "(field (ExtremelyLongNamedRecordType 42) longFieldName)", 42);
   Pure ("(type Note (variant (Text String) (Absent Unit))) " &
     "(type Box (record (note Note))) " &
     "(match (field (Box Note.Absent) note) ((Note.Text text) (length text)) ((Note.Absent) 7))", 7);
   Pure ("(type Note (variant (Text String) (Absent))) " &
     "(type Box (record (note Note))) " &
     "(match (field (Box Note.Absent) note) ((Note.Text text) (length text)) ((Note.Absent) 7))", 7);
   Reject ("(type Box (record (x Integer))) (field (Box true) x)");
   Reject ("(type Box (record (x Integer))) (field (Box) x)");
   Reject ("(type Box (record (x Integer))) (field (Box 1 2) x)");
   Reject ("(type Box (record (x Handler))) 1");
   Reject ("(type Box (record (x))) 1");
   Reject ("(type Box (record (x Integer) (x Integer))) 1");
   Reject ("(type if (record (x Integer))) 1");
   Reject ("(type Box (record (x Integer))) (define (Box) Integer 1) (Box)");
   Reject ("(type Left (record (x Integer))) (type Right (record (x Integer))) " &
     "(type Box (record (x Left))) (field (Box (Right 1)) x)");
   declare
      Prefix : constant String := "(type Wide (record (a String) (b String) (c String) (d String) " &
        "(e String) (f String) (g String) (h String) (i String) (j String) (k String) (l String) " &
        "(m String) (n String) (o String) (p String))) ";
      Slots : constant String := "(Wide t t t t t t t t t t t t t t t t)";
      Text : constant String := "(let ((s """ & String'(1 .. 256 => 'x') & """)) ";
   begin
      -- Exact 8 KiB aggregate budget; each language string stays within its
      -- own bound. Sixteen constructor fields are independent of function arity.
      Pure (Prefix & Text & "(let ((t (concat s s))) (length (field " & Slots & " p))))", 512);
      -- Record fields refer to the evaluation's text region, not copies:
      -- sixteen fields naming one 513-byte string store 513 bytes.
      Pure (Prefix & Text & "(let ((t (concat (concat s s) ""x""))) (length (field " & Slots & " p))))", 513);
   end;
   declare
      -- Records and payload variants come out as canonical literals that
      -- read back as the same value.
      procedure Literal (Program, Expected : String) is
         Again : L.Interpretation_Result;
      begin
         CCL.Evaluation.Evaluate (Program, 4096, Result);
         Check (Result.Status = L.Succeeded and Result.Has_Literal and
                Result.Literal.Data (1 .. Result.Literal.Length) = Expected);
         CCL.Evaluation.Evaluate (Program (Program'First .. Program'Last - Expected'Length) & Expected, 4096, Again);
         Check (Again.Status = L.Succeeded and Again.Has_Literal and
                Again.Literal.Data (1 .. Again.Literal.Length) = Expected);
      end Literal;
      Types_Source : constant String :=
        "(type C (record (a Integer) (b Boolean))) " &
        "(type Note (variant (Text String) (Absent) (Count Integer) (Inner C))) " &
        "(type L (record (c C) (n Note) (s String))) ";
   begin
      Literal (Types_Source & "(L (C 1 true) (Note.Inner (C -2 false)) ""q\""\\\n\t"")",
               "(L (C 1 true) (Note.Inner (C -2 false)) ""q\""\\\n\t"")");
      Literal (Types_Source & "(L (C 0 false) Note.Absent """")", "(L (C 0 false) Note.Absent """")");
      Literal (Types_Source & "(L (C 0 false) (Note.Count 7) ""x"")", "(L (C 0 false) (Note.Count 7) ""x"")");
      Literal (Types_Source & "(Note.Text ""hi"")", "(Note.Text ""hi"")");
      -- No literal spelling exists for characters yet: refused, not guessed.
      CCL.Evaluation.Evaluate ("(type K (record (c Character))) (K (at ""ab"" 1))", 4096, Result);
      Check (Result.Status = L.Host_Contract_Unsupported and not Result.Has_Literal);
   end;
   declare
      -- Each record is an arena node; more than MAX_VALUE_NODES of them in
      -- one evaluation is a typed exhaustion, never an overwrite.
      function Boxes (N : Positive) return String is
        ("(type Box (record (x Integer))) " &
         "(length (each (fn ((n Integer)) (field (Box n) x)) (range 1" & N'Image & ")))");
   begin
      CCL.Evaluation.Evaluate (Boxes (L.MAX_VALUE_NODES + 1), 1_000_000, Result);
      Check (Result.Status = L.Evaluation_Object_Storage_Exhausted and not Result.Has_Value);
      CCL.Evaluation.Evaluate (Boxes (L.MAX_VALUE_NODES), 1_000_000, Result);
      Check (Result.Status = L.Succeeded and
             Result.Result_Value = CCL.VM.Integer_Constant (Integer_64 (L.MAX_VALUE_NODES)));
   end;
   Define (Types, (Identifier => Named ("Reading"), Form => Sum, Count => 2,
     Parts => [1 => (Named ("Text"), String_Type), 2 => (Named ("Absent"), Unit_Type), others => <>]),
     Variant_Type, Declared); Check (Declared = Defined);
   Define (Types, (Identifier => Named ("Settings"), Form => Product, Count => 3,
     Parts => [1 => (Named ("title"), String_Type), 2 => (Named ("reading"), Variant_Type),
       3 => (Named ("enabled"), Boolean_Type), others => <>]), Product_Type, Declared);
   Check (Declared = Defined);
   Bind (Types, Product_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   Value := Empty (Contract);
   Append (Value, Product_Cell (3), Built); Check (Built = Added);
   Append_Text (Value, "hello", Built); Check (Built = Added);
   Append (Value, Variant_Cell (1), Built); Check (Built = Added);
   Append_Text (Value, "world", Built); Check (Built = Added);
   Append (Value, Boolean_Cell (True), Built); Check (Built = Added);
   R.Define (Contract, Named ("Snapshot"), Named ("ConfigRead"), [5, 6, 7, 8], Definition, Good); Check (Good);
   Publish_Schema (Catalog, R.Schema (Definition), Published); Check (Published = CCL.Objects.Catalog.Published);
   Publish_Schema (Catalog, Contract, Published); Check (Published = CCL.Objects.Catalog.Published);
   Define_Interface ("config-test", 1, 0, [91, 92, 93, 94], Description, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("read", 0,
     (Result => CCL.Host_Values.Object_Value, Result_Schema => Identity (R.Schema (Definition)), others => <>),
     Operation, Error); Check (Error = Catalog_Valid);
   Add_Operation (Description, Operation, Error); Check (Error = Catalog_Valid);
   Define_Host_Operation ("write", 1,
     (Argument => CCL.Host_Values.Object_Value, Argument_Schema => Identity (Contract), others => <>),
     Operation, Error); Check (Error = Catalog_Valid);
   Add_Operation (Description, Operation, Error); Check (Error = Catalog_Valid);
   Publish (Catalog, Description, Error); Check (Error = Catalog_Valid);
   Resolve (Catalog, "config-test.read", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 77, Installed); Check (Installed = Grant_Added);
   Read_Only := Grants;
   Resolve (Catalog, "config-test.write", Resolved, Good); Check (Good);
   Install (Grants, Resolved, 78, Installed); Check (Installed = Grant_Added);
   -- The separate typed result owns its image after the evaluator clears its
   -- snapshot pool. Pure local definitions need no discovery or call grants.
   CCL.Evaluation.Evaluate_Object
     ("(type Reading (variant (Text String) (Absent))) " &
      "(type Settings (record (title String) (reading Reading) (enabled Boolean))) " &
      "(Settings ""hello"" (Reading.Text ""world"") true)", 128, Contract, Object_Result);
   Check (Object_Result.Status = L.Succeeded and Object_Result.Has_Value and Object_Result.Value = Value);
   State := (others => <>);
   Evaluate_Object ("(Settings ""hello"" (Reading.Text ""world"") true)",
     128, Catalog, No_Grants, State, Contract, Object_Result);
   Check (Object_Result.Status = L.Succeeded and Object_Result.Has_Value and Object_Result.Value = Value
     and State.Calls = 0 and State.Writes = 0);
   for Code in W.Status loop
      declare
         Expected : CCL.Objects.Image;
      begin
         State := (Code => Code, others => <>);
         R.Build (Definition, True, Code, (if Code in W.Success | W.Stale then 42 else 0), Value, Expected, Good);
         Check (Good);
         Evaluate_Object ("(config-test.read)", 128, Catalog, Grants, State, R.Schema (Definition), Object_Result);
         Check (Object_Result.Status = L.Succeeded and Object_Result.Has_Value and
           Object_Result.Value = Expected and State.Calls = 1);
      end;
   end loop;
   -- Expected result metadata is checked before even a read effect. Equal
   -- digest words are not evidence of matching definitions.
   Bind (Types, Integer_Type, Identity (R.Schema (Definition)), Wrong, Good); Check (Good);
   State := (others => <>);
   Evaluate_Object ("(config-test.read)", 128, Catalog, Grants, State, Wrong, Object_Result);
   Check (Object_Result.Status = L.Type_Check_Failed and not Object_Result.Has_Value and
     Object_Result.Value = Empty (Wrong) and State.Calls = 0);
   State := (others => <>);
   Evaluate_Object ("(config-test.read)", 128, Catalog, No_Grants, State, R.Schema (Definition), Object_Result);
   Check (Object_Result.Status = L.Host_Authority_Denied and not Object_Result.Has_Value and State.Calls = 0);
   State := (Corrupt => True, others => <>);
   Evaluate_Object ("(config-test.read)", 128, Catalog, Grants, State, R.Schema (Definition), Object_Result);
   Check (Object_Result.Status = L.Host_Result_Type_Mismatch and not Object_Result.Has_Value and
     Object_Result.Value = Empty (R.Schema (Definition)) and State.Calls = 1);
   State := (others => <>);
   Evaluate_Object ("(config-test.read)", 0, Catalog, Grants, State, R.Schema (Definition), Object_Result);
   Check (Object_Result.Status = L.Evaluation_Fuel_Exhausted and not Object_Result.Has_Value and State.Calls = 0);
   CCL.Evaluation.Evaluate_Object ("(unfinished", 128, Contract, Object_Result);
   Check (Object_Result.Status = L.Parse_Failed and not Object_Result.Has_Value and Object_Result.Value = Empty (Contract));
   State := (others => <>);
   Evaluate ("(config-test.write (Settings ""hello"" (Reading.Text ""world"") true))", 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and State.Calls = 0 and State.Writes = 1);
   State := (others => <>);
   Evaluate ("(config-test.write (Settings ""hello"" (Reading.Text ""world"") true))", 128, Catalog, Read_Only, State, Result);
   Check (Result.Status = L.Host_Authority_Denied and State.Calls = 0 and State.Writes = 0);
   for Code in W.Status loop
      State := (Code => Code, others => <>);
      Evaluate (Source ("(+ (field snapshot revision) (length (field (field snapshot value) title)))"),
        128, Catalog, Grants, State, Result);
      Check (State.Calls = 1 and Result.Status = L.Succeeded and Result.Has_Value);
      Check (Result.Result_Value = CCL.VM.Integer_Constant
        (case Code is when W.Success => 47, when W.Stale => 42, when W.Missing => 3,
          when W.Denied => 4, when W.Busy => 5, when W.Unavailable => 6, when W.Schema_Mismatch => 7,
          when W.Invalid_Request => 8, when others => 9));
      Compiled_Read ("(field snapshot revision)", Code,
        (case Code is when W.Success | W.Stale => 42, when W.Missing => 3,
          when W.Denied => 4, when W.Busy => 5, when W.Unavailable => 6, when W.Schema_Mismatch => 7,
          when W.Invalid_Request => 8, when others => 9));
   end loop;
   Compiled_Read ("(if (field (field snapshot value) enabled) 1 0)", W.Success, 1);
   Compiled_Read ("(config-test.write (field snapshot value))", W.Success, 23, Write_Back => True);
   Compiled_Read ("(config-test.write (field snapshot value))", W.Missing, 3);
   Compiled_Read ("(config-test.write (field snapshot value))", W.Success, 0, Corrupt => True);
   State := (others => <>);
   Evaluate (Source ("(config-test.write (field snapshot value))"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (23)
     and State.Calls = 1 and State.Writes = 1);
   State := (others => <>);
   Evaluate (Source ("(config-test.write (field snapshot value))"), 128, Catalog, Read_Only, State, Result);
   -- Preflight checks every import before any host effects, including reads.
   Check (Result.Status = L.Host_Authority_Denied and State.Calls = 0 and State.Writes = 0);
   State := (others => <>);
   Evaluate (Source ("(config-test.write snapshot)"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Type_Check_Failed and State.Calls = 0 and State.Writes = 0);
   State := (Code => W.Missing, others => <>);
   Evaluate (Source ("(config-test.write (field snapshot value))"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and State.Calls = 1 and State.Writes = 0);
   State := (others => <>);
   Evaluate (Source ("(match (field (field snapshot value) reading) ((Reading.Text text) (length text)) ((Reading.Absent) 0))"),
     128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (5));
   State := (others => <>);
   Evaluate (Source ("(if (field (field snapshot value) enabled) 1 0)"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (1));
   State := (others => <>);
   Evaluate (Source ("(field snapshot absent)"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Type_Check_Failed and State.Calls = 0);
   Evaluate (Source ("(field 42 revision)"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Type_Check_Failed and State.Calls = 0);
   Evaluate (Source ("(field snapshot revision)"), 128, Catalog, No_Grants, State, Result);
   Check (Result.Status = L.Host_Authority_Denied and State.Calls = 0);
   State := (Corrupt => True, others => <>);
   Evaluate (Source ("(field snapshot revision)"), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Host_Result_Type_Mismatch and State.Calls = 1);
   L.Analyze (Source ("(field snapshot revision)"), Catalog, Analysis);
   Check (L.Analysis_Status_Of (Analysis) = L.Analysis_Succeeded);
   CCL.Compiler.Compile (Analysis, Compiled);
   Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded); -- native match/field instructions
   L.Views.Convert (Source ("(field snapshot revision)"), L.Views.Lisp, L.Views.Basic, Catalog, Basic);
   Check (Basic.Status = L.Views.Converted);
   L.Views.Convert (Basic.Rendered.Data (1 .. Basic.Rendered.Length), L.Views.Basic, L.Views.Lisp, Catalog, Lisp);
   Check (Lisp.Status = L.Views.Converted);
   State := (others => <>);
   Evaluate (Lisp.Rendered.Data (1 .. Lisp.Rendered.Length), 128, Catalog, Grants, State, Result);
   Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (42));
   -- Each read's snapshot lives in the run's value arena, which admits a
   -- result before the host runs (CCL.VM.Native_Objects); more reads than
   -- the old interpreter's object pool held all succeed. This recursive
   -- source generator just builds finite source; CCL itself does not recurse.
   declare
      function Calls (Count : Positive) return String is
        (if Count = 1 then "(read)" else "(+ (read) " & Calls (Count - 1) & ")");
      Program : constant String := "(define (read) Integer " & Source ("(field snapshot revision)") & ") " &
        Calls (L.MAX_OBJECT_VALUES + 1);
   begin
      State := (others => <>);
      Evaluate (Program, 1024, Catalog, Grants, State, Result);
      Check (Result.Status = L.Succeeded and State.Calls = L.MAX_OBJECT_VALUES + 1 and
        Result.Result_Value = CCL.VM.Integer_Constant (42 * Integer_64 (L.MAX_OBJECT_VALUES + 1)));
   end;
   -- Native strings have the object's full text bound. The scalar UI and
   -- ordinary Text_Value endpoints retain their separately declared limits.
   declare
      Text_Contract : CCL.Objects.Binding;
      Text_Image : CCL.Objects.Image;
      Text_Catalog : Interface_Catalog;
      Text_Grants : Granted_Bindings;
      Text_Interface : Interface_Descriptor;
      Text_State : Context;
      procedure Text_Invoke
        (State : in out Context; Binding : Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result) is
      begin
         if Binding = 1 then
            State.Calls := State.Calls + 1;
            Reply := (Value => CCL.Host_Values.Object_Constant (Text_Image), Success => True, Why => <>);
         elsif Binding = 2 then
            Check (Argument = CCL.Host_Values.Object_Constant (Text_Image));
            State.Writes := State.Writes + 1;
            Reply := (Value => CCL.Host_Values.Integer_Constant (23), Success => True, Why => <>);
         else
            State.Writes := State.Writes + 1;
            Reply := (Value => CCL.Host_Values.Integer_Constant (23), Success => True, Why => <>);
         end if;
      end Text_Invoke;
      procedure Text_Evaluate is new CCL.Evaluation.Evaluate_With_Values (Context, Text_Invoke);
      procedure Text_Object is new CCL.Evaluation.Evaluate_Object_With_Values (Context, Text_Invoke);
      -- Eight doublings exercise both storage paths without a large literal.
      Seed : constant String := String'(1 .. 32 => 'x');
      Prefix : constant String :=
        "(let ((a """ & Seed & """)) (let ((b (concat a a))) " &
        "(let ((c (concat b b))) (let ((d (concat c c))) " &
        "(let ((e (concat d d))) (let ((f (concat e e))) " &
        "(let ((g (concat f f))) (let ((h (concat g g))) (let ((i (concat h h))) ";
      Suffix : constant String := ")))))))))";
   begin
      Bind (Types, String_Type, [41, 42, 43, 44], Text_Contract, Good); Check (Good);
      Text_Image := Empty (Text_Contract);
      Append_Text (Text_Image, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
      Publish_Schema (Text_Catalog, Text_Contract, Published); Check (Published = CCL.Objects.Catalog.Published);
      Define_Interface ("strings", 1, 0, [45, 46, 47, 48], Text_Interface, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("get", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => Identity (Text_Contract), others => <>),
        Operation, Error); Check (Error = Catalog_Valid);
      Add_Operation (Text_Interface, Operation, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("put", 1,
        (Argument => CCL.Host_Values.Object_Value, Argument_Schema => Identity (Text_Contract), others => <>),
        Operation, Error); Check (Error = Catalog_Valid);
      Add_Operation (Text_Interface, Operation, Error); Check (Error = Catalog_Valid);
      Define_Host_Operation ("short", 1,
        (Argument => CCL.Host_Values.Text_Value, Argument_Text_Limit => L.MAX_TEXT_BYTES, others => <>),
        Operation, Error); Check (Error = Catalog_Valid);
      Add_Operation (Text_Interface, Operation, Error); Check (Error = Catalog_Valid);
      Publish (Text_Catalog, Text_Interface, Error); Check (Error = Catalog_Valid);
      Resolve (Text_Catalog, "strings.get", Resolved, Good); Check (Good);
      Install (Text_Grants, Resolved, 1, Installed); Check (Installed = Grant_Added);
      Resolve (Text_Catalog, "strings.put", Resolved, Good); Check (Good);
      Install (Text_Grants, Resolved, 2, Installed); Check (Installed = Grant_Added);
      Resolve (Text_Catalog, "strings.short", Resolved, Good); Check (Good);
      Install (Text_Grants, Resolved, 3, Installed); Check (Installed = Grant_Added);
      Text_Evaluate ("(length (strings.get))", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (Maximum_Text_Bytes));
      Text_Evaluate ("(at (strings.get) 8192)",
        128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Succeeded and Result.Has_Character and Result.Result_Character = 'x');
      Text_Evaluate ("(at (strings.get) 8193)", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Evaluation_Index_Error);
      Text_Object ("(concat """" (strings.get))", 128, Text_Catalog, Text_Grants, Text_State, Text_Contract, Object_Result);
      Check (Object_Result.Status = L.Succeeded and Object_Result.Has_Value and Object_Result.Value = Text_Image);
      Text_Evaluate ("(strings.put (strings.get))", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Succeeded and Text_State.Writes = 1);
      Text_Evaluate ("(strings.short (strings.get))", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Host_Argument_Out_Of_Bounds and Text_State.Writes = 1);
      Text_Evaluate ("(strings.short ""hello"")", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Succeeded and Text_State.Writes = 2);
      Text_Evaluate ("(strings.get)", 128, Text_Catalog, Text_Grants, Text_State, Result);
      Check (Result.Status = L.Evaluation_Text_Storage_Exhausted and not Result.Has_Value);
      Text_Object ("(concat (strings.get) ""x"")", 128, Text_Catalog, Text_Grants, Text_State, Text_Contract, Object_Result);
      Check (Object_Result.Status = L.Evaluation_Text_Storage_Exhausted and not Object_Result.Has_Value);
      CCL.Evaluation.Evaluate_Object (Prefix & "i" & Suffix, 512, Text_Contract, Object_Result);
      Check (Object_Result.Status = L.Succeeded and Object_Result.Has_Value and Object_Result.Value = Text_Image);
      CCL.Evaluation.Evaluate (Prefix & "(length i)" & Suffix, 512, Result);
      Check (Result.Status = L.Succeeded and Result.Result_Value = CCL.VM.Integer_Constant (Maximum_Text_Bytes));
      CCL.Evaluation.Evaluate_Object (Prefix & "(concat i ""x"")" & Suffix, 512, Text_Contract, Object_Result);
      Check (Object_Result.Status = L.Evaluation_Text_Storage_Exhausted and not Object_Result.Has_Value);
   end;
   Ada.Text_IO.Put_Line ("CCL structured Config read source: PASS" & Checks'Image & " checks");
end Read_Source_Tests;
