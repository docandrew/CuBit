with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.VM; use CCL.VM;
with CCL.Language;
with CCL.Types;
with CCL.Catalog;
with CCL.Interfaces.Clock;
with CCL.Compiler;
with CCL.Debug_Maps;
with CCL.Scheduler; use CCL.Scheduler;
with CCL.Format; use CCL.Format;
with CCL.Ownership; use CCL.Ownership;
with CCL.Ownership.Bytecode;
with CCL.Imports;
with CCL.Host_Values;

procedure Main is
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Diagnostic_Code;
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Node_Kind;
   use type CCL.Language.Static_Type;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Descriptor_Digest;
   use type CCL.Catalog.Intern_Result;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Catalog.Link_Result;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Debug_Maps.Validation_Error;

   TEST_INTERFACE_DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#5445_5354_2D49_4643#,
      16#0000_0000_0000_0001#,
      16#0000_0000_0000_0002#,
      16#0000_0000_0000_0003#];
   TEST_INCREMENT_BINDING : constant Unsigned_32 := 42;
   TEST_MONOTONIC_BINDING : constant Unsigned_32 := 43;

   Failures : Natural := 0;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      if Condition then
         Put_Line ("PASS " & Name);
      else
         Put_Line ("FAIL " & Name);
         Failures := Failures + 1;
      end if;
   end Check;

   function Ins
     (Op        : Op_Code;
      Immediate : Integer_64 := 0;
      Target    : Instruction_Index := 0;
      Import    : Import_Index := 0) return Instruction
   is ((Op => Op, Immediate => Immediate, Target => Target, Import => Import,
        others => <>));

   procedure Make_Test_Catalog
     (Item  : out CCL.Catalog.Interface_Catalog;
      Error : out CCL.Catalog.Catalog_Error)
   is
      Descriptor : CCL.Catalog.Interface_Descriptor;
      Operation  : CCL.Catalog.Operation_Descriptor;
   begin
      CCL.Catalog.Initialize (Item);
      CCL.Catalog.Define_Interface
        ("test.service", 1, 0, TEST_INTERFACE_DIGEST, Descriptor, Error);
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Operation
           ("increment", 1,
            (Argument => Integer_Value,
             Result => Integer_Value,
             Authority => Observe_Authority,
             others => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Define_Operation
           ("monotonic", 0,
            (Argument => Integer_Value,
             Result => Integer_Value,
             Authority => Observe_Authority,
             others => <>),
            Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Add_Operation (Descriptor, Operation, Error);
      end if;
      if Error = CCL.Catalog.Catalog_Valid then
         CCL.Catalog.Publish (Item, Descriptor, Error);
      end if;
   end Make_Test_Catalog;

   procedure Test_Interface_Catalog is
      Catalog    : CCL.Catalog.Interface_Catalog;
      Error      : CCL.Catalog.Catalog_Error;
      Resolution : CCL.Catalog.Resolved_Operation;
      Found      : Boolean;
      Linkage    : CCL.Catalog.Linkage_Table;
      Index      : Import_Index;
      Interned   : CCL.Catalog.Intern_Result;
   begin
      Make_Test_Catalog (Catalog, Error);
      Check
        (Error = CCL.Catalog.Catalog_Valid and then
         CCL.Catalog.Length (Catalog) = 1,
         "publish bounded interface catalog");

      CCL.Catalog.Resolve
        (Catalog, "test.service.increment", Resolution, Found);
      Check
        (Found and then
         Resolution.Interface_Digest = TEST_INTERFACE_DIGEST and then
         Resolution.Interface_Major = 1 and then
         Resolution.Operation = 0 and then
         Resolution.Parameters = 1 and then
         Resolution.Import.Binding = 0 and then
         Resolution.Import.Authority = Observe_Authority,
         "resolve qualified operation from pinned descriptor");

      CCL.Catalog.Initialize (Linkage);
      CCL.Catalog.Intern (Linkage, Resolution, Index, Interned);
      Check
        (Interned = CCL.Catalog.Linkage_Added and then Index = 0 and then
         CCL.Catalog.Length (Linkage) = 1,
         "intern resolved operation into compiler linkage");
      CCL.Catalog.Intern (Linkage, Resolution, Index, Interned);
      Check
        (Interned = CCL.Catalog.Linkage_Existing and then Index = 0 and then
         CCL.Catalog.Length (Linkage) = 1,
         "deduplicate compiler linkage by descriptor identity");

      CCL.Catalog.Resolve
        (Catalog, "test.service.missing", Resolution, Found);
      Check (not Found, "hide operations absent from catalog view");
   end Test_Interface_Catalog;

   procedure Test_Clock_Interface is
      Catalog    : CCL.Catalog.Interface_Catalog;
      Error      : CCL.Catalog.Catalog_Error;
      Resolution : CCL.Catalog.Resolved_Operation;
      Found      : Boolean;
      Grants     : CCL.Catalog.Granted_Bindings;
      Grant      : CCL.Catalog.Grant_Result;
   begin
      CCL.Catalog.Initialize (Catalog);
      CCL.Catalog.Initialize (Grants);
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms
        (Catalog, Resolution, Found);
      Check (not Found, "clock is absent without an explicit catalog view");

      CCL.Interfaces.Clock.Publish (Catalog, Error);
      CCL.Interfaces.Clock.Resolve_Monotonic_Ms
        (Catalog, Resolution, Found);
      Check
        (Error = CCL.Catalog.Catalog_Valid and then Found and then
         Resolution.Interface_Digest =
           CCL.Interfaces.Clock.DESCRIPTOR_DIGEST and then
         Resolution.Interface_Major = 1 and then
         Resolution.Interface_Minor = 0 and then
         Resolution.Operation = 0 and then
         Resolution.Parameters = 0 and then
         Resolution.Import.Binding = 0 and then
         Resolution.Import.Authority = Observe_Authority,
         "publish the canonical authority-free Clock descriptor");

      Check
        (CCL.Catalog.Length (Catalog) = 1 and then
         CCL.Catalog.Length (Grants) = 0,
         "keep visible Clock metadata separate from invocation grants");
      CCL.Catalog.Install (Grants, Resolution, 1, Grant);
      Check
        (Grant = CCL.Catalog.Grant_Added and then
         CCL.Catalog.Length (Grants) = 1,
         "inspect bounded Clock grant count after trusted admission");
   end Test_Clock_Interface;

   procedure Test_Addition is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      Outcome   : Execution_Result;
   begin
      Candidate.Length := 4;
      Candidate.Code (0) := Ins (Push_Integer, 20);
      Candidate.Code (1) := Ins (Push_Integer, 22);
      Candidate.Code (2) := Ins (Add_Integer);
      Candidate.Code (3) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify integer addition");
      if Error = Valid then
         Execute (Checked, 4, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 42,
            "execute integer addition");
         Check
           (Outcome.Steps = 4 and then Outcome.Fuel_Remaining = 0,
            "account exact fuel");
      end if;
   end Test_Addition;

   procedure Test_Debug_Stepping is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      State     : Machine_State;
      Outcome   : Execution_Result;
      View      : Machine_Snapshot;
      Inspection : Inspection_Snapshot;
   begin
      Candidate.Length := 4;
      Candidate.Code (0) := Ins (Push_Integer, 20);
      Candidate.Code (1) := Ins (Push_Integer, 22);
      Candidate.Code (2) := Ins (Add_Integer);
      Candidate.Code (3) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify debug program");
      if Error = Valid then
         Initialize (Checked, 8, State);
         Continue_Execution_For (Checked, State, 1, Outcome);
         View := Snapshot (State);
         Inspect (Checked, State, Inspection);
         Check
           (Outcome.Status = Paused and then View.Instruction = 1 and then
            View.Steps = 1 and then View.Fuel_Remaining = 7 and then
            not View.Terminal,
            "pause after one instruction");
         Check
           (Inspection.Stack_Length = 1 and then
            Inspection.Stack (0).Kind = Integer_Value and then
            Inspection.Stack (0).Integer = 20 and then
            Inspection.Locals_Length = 0,
            "inspect copied operand stack without mutating VM");

         Continue_Execution_For (Checked, State, 2, Outcome);
         View := Snapshot (State);
         Check
           (Outcome.Status = Paused and then View.Instruction = 3 and then
            View.Steps = 3 and then View.Fuel_Remaining = 5,
            "resume bounded instruction slice");

         Continue_Execution_For (Checked, State, 1, Outcome);
         View := Snapshot (State);
         Check
           (Outcome.Status = Completed and then Outcome.Has_Value and then
            Outcome.Result_Value.Integer = 42 and then View.Terminal,
            "complete stepped program");

         Initialize (Checked, 8, State);
         Stop (State);
         Continue_Execution_For (Checked, State, 1, Outcome);
         View := Snapshot (State);
         Check
           (Outcome.Status = Stopped and then View.Terminal and then
            View.Steps = 0 and then View.Fuel_Remaining = 8,
            "stop without consuming another instruction");
      end if;
   end Test_Debug_Stepping;

   procedure Test_Lexical_Local is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      Outcome   : Execution_Result;
      State     : Machine_State;
      Inspection : Inspection_Snapshot;
   begin
      Candidate.Length := 4;
      Candidate.Types_Length := 1;
      Candidate.Locals_Length := 1;
      Candidate.Dynamic_Locals_Length := 1;
      Candidate.Local_Kinds (0) := Integer_Value;
      Candidate.Local_Types (0) := 0;
      Candidate.Code (0) := Ins (Push_Integer, 42);
      Candidate.Code (1) :=
        (Op => Initialize_Local, Local => 0, others => <>);
      Candidate.Code (2) := (Op => Copy_Local, Local => 0, others => <>);
      Candidate.Code (3) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify lexical local initialization");
      if Error = Valid then
         Initialize (Checked, 8, State);
         Continue_Execution_For (Checked, State, 2, Outcome);
         Inspect (Checked, State, Inspection);
         Check
           (Inspection.Stack_Length = 0 and then
            Inspection.Locals_Length = 1 and then
            Inspection.Locals (0).Kind = Integer_Value and then
            Inspection.Locals (0).Value.Integer = 42 and then
            Inspection.Locals (0).Ownership_State = Available and then
            Inspection.Locals (0).Read_Borrows = 0 and then
            not Inspection.Locals (0).Write_Borrow,
            "inspect initialized local and ownership state");
         Execute (Checked, 8, Outcome);
         Check
           (Outcome.Status = Completed and then Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 42,
            "execute initialized lexical local");
      end if;

      Candidate.Code (0) := (Op => Copy_Local, Local => 0, others => <>);
      Candidate.Code (1) := Ins (Halt);
      Candidate.Length := 2;
      Verify (Candidate, Checked, Error);
      Check
        (Error = Invalid_Ownership,
         "reject lexical local use before initialization");
   end Test_Lexical_Local;

   procedure Test_Branch is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      Outcome   : Execution_Result;
   begin
      Candidate.Length := 6;
      Candidate.Code (0) := Ins (Push_Boolean, 0);
      Candidate.Code (1) := Ins (Jump_If_False, Target => 4);
      Candidate.Code (2) := Ins (Push_Integer, 1);
      Candidate.Code (3) := Ins (Jump, Target => 5);
      Candidate.Code (4) := Ins (Push_Integer, 2);
      Candidate.Code (5) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify converging branch");
      if Error = Valid then
         Execute (Checked, 10, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Integer = 2,
            "execute false branch");
      end if;
   end Test_Branch;

   procedure Test_Rejections is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
   begin
      Verify (Candidate, Checked, Error);
      Check (Error = Empty_Program, "reject empty program");

      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Push_Boolean, 1);
      Candidate.Code (1) := Ins (Add_Integer);
      Verify (Candidate, Checked, Error);
      Check (Error = Type_Mismatch, "reject operand type mismatch");

      Candidate := (others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Jump, Target => 0);
      Candidate.Code (1) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Backward_Jump, "reject backward jump");

      Candidate := (others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Push_Integer, 1);
      Candidate.Code (1) := Ins (Drop);
      Verify (Candidate, Checked, Error);
      Check (Error = Missing_Halt, "reject fallthrough without halt");

      Candidate := (others => <>);
      Candidate.Length := 5;
      Candidate.Code (0) := Ins (Push_Boolean, 1);
      Candidate.Code (1) := Ins (Jump_If_False, Target => 4);
      Candidate.Code (2) := Ins (Push_Integer, 1);
      Candidate.Code (3) := Ins (Jump, Target => 4);
      Candidate.Code (4) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Inconsistent_Stack, "reject inconsistent branch join");

      Candidate := (others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Halt);
      Candidate.Code (1) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Unreachable_Instruction, "reject unreachable instruction");

      Candidate := (others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Jump, Target => 9);
      Candidate.Code (1) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Invalid_Jump_Target, "reject invalid jump target");

      Candidate := (others => <>);
      Candidate.Length := Program_Length (MAX_STACK_DEPTH + 2);
      for I in Instruction_Index range
        0 .. Instruction_Index (MAX_STACK_DEPTH)
      loop
         Candidate.Code (I) := Ins (Push_Integer, 1);
      end loop;
      Candidate.Code (Instruction_Index (MAX_STACK_DEPTH + 1)) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Stack_Overflow, "reject verifier stack overflow");
   end Test_Rejections;

   procedure Test_Runtime_Limits is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      Outcome   : Execution_Result;
   begin
      Candidate.Length := 4;
      Candidate.Code (0) := Ins (Push_Integer, Integer_64'Last);
      Candidate.Code (1) := Ins (Push_Integer, 1);
      Candidate.Code (2) := Ins (Add_Integer);
      Candidate.Code (3) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify overflowing expression structurally");
      if Error = Valid then
         Execute (Checked, 4, Outcome);
         Check
           (Outcome.Status = Arithmetic_Overflow,
            "trap integer overflow");
         Execute (Checked, 2, Outcome);
         Check
           (Outcome.Status = Fuel_Exhausted and then Outcome.Steps = 2,
            "enforce fuel limit");
      end if;

      Candidate.Code (0) := Ins (Push_Integer, Integer_64'First);
      Candidate.Code (1) := Ins (Push_Integer, -1);
      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify negative overflowing expression");
      if Error = Valid then
         Execute (Checked, 4, Outcome);
         Check
           (Outcome.Status = Arithmetic_Overflow,
            "trap negative integer overflow");
      end if;
   end Test_Runtime_Limits;

   procedure Test_Source_Language is
      Outcome  : CCL.Language.Interpretation_Result;
      Analysis : CCL.Language.Analysis_Result;
      Catalog  : CCL.Catalog.Interface_Catalog;
      Catalog_Error : CCL.Catalog.Catalog_Error;
   begin
      CCL.Language.Analyze ("(+ 20 22)", Analysis);
      Check
        (CCL.Language.Analysis_Status_Of (Analysis) =
           CCL.Language.Analysis_Succeeded and then
         CCL.Language.Analysis_Root (Analysis) <
           CCL.Language.Analysis_Node_Count (Analysis) and then
         CCL.Language.Analysis_Node
           (Analysis,
            CCL.Language.Node_Index
              (CCL.Language.Analysis_Root (Analysis))).Kind =
             CCL.Language.Add_Form and then
         CCL.Language.Analysis_Node
           (Analysis,
            CCL.Language.Node_Index
              (CCL.Language.Analysis_Root (Analysis))).Static_Kind =
             CCL.Language.Integer_Type and then
         CCL.Language.Analysis_Node
           (Analysis,
            CCL.Language.Node_Index
              (CCL.Language.Analysis_Root (Analysis))).Source_Position = 1,
         "analyze typed source tree");

      CCL.Language.Analyze ("(+ true 4)", Analysis);
      Check
        (CCL.Language.Analysis_Status_Of (Analysis) =
           CCL.Language.Analysis_Type_Check_Failed and then
         CCL.Language.Analysis_Diagnostic (Analysis) =
           CCL.Language.Expected_Integer and then
         CCL.Language.Analysis_Diagnostic_Position (Analysis) = 1,
         "report shared frontend type diagnostic");

      CCL.Language.Interpret ("(+ 20 22)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Value and then
         Outcome.Result_Value.Kind = Integer_Value and then
         Outcome.Result_Value.Integer = 42,
         "interpret integer expression");

      CCL.Language.Interpret ("(* 6 7)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 42,
         "interpret checked integer multiplication");
      CCL.Language.Interpret ("(/ 3661000 3600000)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 1,
         "interpret integer division");
      CCL.Language.Interpret ("(mod 3661000 3600000)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 61_000,
         "interpret integer modulo");
      CCL.Language.Interpret ("(/ 1 0)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Division_By_Zero,
         "report typed division-by-zero failure");
      CCL.Language.Interpret ("(* 9223372036854775807 2)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Overflow,
         "trap multiplication overflow");

      --  Subtraction, comparisons and short-circuit connectives.
      CCL.Language.Interpret ("(- 50 8)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 42,
         "interpret checked integer subtraction");
      CCL.Language.Interpret ("(- 5 8)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = -3,
         "subtraction below zero");
      CCL.Language.Interpret ("(- -9223372036854775807 2)", 16, Outcome);
      Check
        (Outcome.Status /= CCL.Language.Succeeded,
         "reject or trap subtraction below the integer range");
      CCL.Language.Interpret ("(- 0 9223372036854775807)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = -9223372036854775807,
         "subtract to the most negative representable sum");
      declare
         type Case_Item is record
            Source : access constant String;
            Expected : Boolean;
         end record;
         S1 : aliased constant String := "(< 1 2)";
         S2 : aliased constant String := "(< 2 2)";
         S3 : aliased constant String := "(<= 2 2)";
         S4 : aliased constant String := "(> 3 2)";
         S5 : aliased constant String := "(>= 1 2)";
         S6 : aliased constant String := "(/= 1 2)";
         S7 : aliased constant String := "(/= 2 2)";
         S8 : aliased constant String := "(and (< 1 2) (> 3 2))";
         S9 : aliased constant String := "(or (> 1 2) (= 2 2))";
         S10 : aliased constant String := "(or true (= (/ 1 0) 1))";
         S11 : aliased constant String := "(and false (= (/ 1 0) 1))";
         S12 : aliased constant String := "(less-equal 7 (subtract 10 3))";
         Cases : constant array (1 .. 12) of Case_Item :=
           [(S1'Access, True), (S2'Access, False), (S3'Access, True),
            (S4'Access, True), (S5'Access, False), (S6'Access, True),
            (S7'Access, False), (S8'Access, True), (S9'Access, True),
            (S10'Access, True), (S11'Access, False), (S12'Access, True)];
      begin
         for C of Cases loop
            CCL.Language.Interpret (C.Source.all, 32, Outcome);
            Check
              (Outcome.Status = CCL.Language.Succeeded and then
               Outcome.Result_Value.Kind = Boolean_Value and then
               Outcome.Result_Value.Boolean = C.Expected,
               "comparison " & C.Source.all);
         end loop;
      end;
      --  Lists: [a b c] and (list a b c), length, at, element types.
      CCL.Language.Interpret ("[10 20 30]", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_List and then
         Outcome.List_Length = 3 and then
         Outcome.List_Values (1).Integer = 10 and then
         Outcome.List_Values (3).Integer = 30 and then
         not CCL.Language.Has_Scalar (Outcome),
         "list literal result");
      CCL.Language.Interpret ("(length (list 1 2 3 4))", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 4,
         "length of a list");
      CCL.Language.Interpret ("(at [7 8 9] 2)", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 8,
         "at reads an element with 1-based bounds");
      CCL.Language.Interpret ("(at [7 8 9] 4)", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Index_Error,
         "at past the bounds is a typed index error");
      CCL.Language.Interpret ("(at [7 8 9] 0)", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Index_Error,
         "at below the bounds is a typed index error");
      CCL.Language.Interpret ("[1 true]", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.List_Element_Mismatch,
         "list elements share one type");
      CCL.Language.Interpret ("[]", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Empty_List_Needs_Type,
         "an empty list needs a declared type");
      CCL.Language.Interpret ("(let ((xs [1 2 3])) (+ (at xs 1) (length xs)))", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 4,
         "a let-bound list");
      CCL.Language.Interpret ("[""alpha"" ""be"" (concat ""g"" ""amma"")]", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_List and then
         Outcome.List_Length = 3 and then
         Outcome.List_Text.Data (1 .. Outcome.List_Text_Ends (1)) = "alpha" and then
         Outcome.List_Text.Data (Outcome.List_Text_Ends (1) + 1 .. Outcome.List_Text_Ends (2)) = "be" and then
         Outcome.List_Text.Data (Outcome.List_Text_Ends (2) + 1 .. Outcome.List_Text_Ends (3)) = "gamma",
         "a list of strings");
      CCL.Language.Interpret ("(length (at [""cubit"" ""os""] 1))", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 5,
         "at on a string list gives a string");
      CCL.Language.Interpret ("[(< 1 2) (> 1 2)]", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Length = 2 and then
         Outcome.List_Values (1).Boolean and then not Outcome.List_Values (2).Boolean,
         "a list of Booleans");
      CCL.Language.Interpret ("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17]", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Parse_Failed and then
         Outcome.Diagnostic = CCL.Language.Too_Many_List_Elements,
         "literal element bound");
      --  First-class functions: named functions as values, calls through
      --  values, function-typed parameters.
      CCL.Language.Interpret
        ("(define (double (x Integer)) Integer (* x 2)) " &
         "(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x)) " &
         "(apply double 21)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 42,
         "pass a named function and call it through a parameter");
      CCL.Language.Interpret
        ("(define (double (x Integer)) Integer (* x 2)) " &
         "(let ((f double)) (f 5))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 10,
         "a let-bound function value");
      CCL.Language.Interpret
        ("(define (double (x Integer)) Integer (* x 2)) double", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Function and then
         CCL.Types.Image (Outcome.Function_Name) = "double" and then
         not CCL.Language.Has_Scalar (Outcome),
         "a function value as the result");
      CCL.Language.Interpret
        ("(define (positive (x Integer)) Boolean (> x 0)) " &
         "(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x)) " &
         "(apply positive 3)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Function_Argument_Mismatch,
         "a function of the wrong type is rejected");
      CCL.Language.Interpret
        ("(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x true)) " &
         "0", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Function_Arity_Mismatch,
         "a call through a value checks its arity");
      CCL.Language.Interpret
        ("(define (twice (f (Function (Integer) Integer)) (x Integer)) Integer (f (f x))) " &
         "(define (inc (x Integer)) Integer (+ x 1)) " &
         "(define (compose-test (g (Function ((Function (Integer) Integer) Integer) Integer))) Integer (g inc 5)) " &
         "(compose-test twice)", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 7,
         "higher-order function types nest");
      --  Anonymous functions.
      CCL.Language.Interpret
        ("(define (apply (f (Function (Integer) Integer)) (x Integer)) Integer (f x)) " &
         "(apply (fn ((n Integer)) (* n n)) 9)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 81,
         "pass an anonymous function");
      CCL.Language.Interpret
        ("(let ((twice (fn ((n Integer)) (+ n n)))) (twice 21))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 42,
         "a let-bound anonymous function");
      CCL.Language.Interpret
        ("(define (make-test (limit Integer)) Boolean " &
         "  (let ((above (fn ((n Integer)) (> n 10)))) (above limit))) " &
         "(make-test 11)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Boolean,
         "an anonymous function inside a define takes its own slot");
      CCL.Language.Interpret ("(fn ((s String)) (length s))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Function,
         "an anonymous function as the result");
      CCL.Language.Interpret ("(fn (n) (* n 2))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Parse_Failed and then
         Outcome.Diagnostic = CCL.Language.Lambda_Parameter_Needs_Type,
         "an untyped parameter asks for its type (inference comes later)");
      CCL.Language.Interpret
        ("(let ((limit 10)) (let ((above (fn ((n Integer)) (> n limit)))) (above 11)))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Boolean,
         "an anonymous function captures an enclosing let");
      CCL.Language.Interpret
        ("(let ((k 3)) (each (fn ((n Integer)) (* n k)) [1 2 3]))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Length = 3 and then
         Outcome.List_Values (3).Integer = 9,
         "a builtin calls a capturing function");
      CCL.Language.Interpret
        ("(define (scale-all (k Integer) (xs (List Integer))) (List Integer) " &
         "  (each (fn ((n Integer)) (* n k)) xs)) (scale-all 10 [1 2])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.List_Values (2).Integer = 20,
         "a function parameter is captured");
      CCL.Language.Interpret
        ("(let ((k 5)) (let ((f (fn ((n Integer)) (+ n k)))) (let ((k 100)) (f 1))))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 6,
         "captures are by value at creation: a later let does not change them");
      CCL.Language.Interpret
        ("(let ((a 1)) (each (fn ((x Integer)) (sum (each (fn ((y Integer)) (+ a y)) [x x]))) [1 2]))",
         512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.List_Values (2).Integer = 6,
         "a nested function captures through its enclosing function");
      CCL.Language.Interpret
        ("(let ((p ""ab"")) (each (fn ((s String)) (concat p s)) [""x"" ""y""]))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Length = 2 and then
         Outcome.List_Text.Data (1 .. Outcome.List_Text_Ends (1)) = "abx",
         "a String is captured");
      CCL.Language.Interpret
        ("(let ((xs [1 2])) (let ((f (fn ((n Integer)) (length xs)))) (f 1)))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Lambda_Capture_Unsupported,
         "capturing a list is reported clearly");
      CCL.Language.Interpret
        ("(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (let ((e 5)) " &
         "(let ((f (fn ((n Integer)) (+ a (+ b (+ c (+ d (+ e n)))))))) (f 0)))))))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Too_Many_Captures,
         "at most four captures");
      CCL.Language.Interpret
        ("(define (sum (x Integer) (y Integer)) Integer (+ x y)) (sum 20 22)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 42,
         "a defined function shadows the builtin of the same name");
      --  List builtins taking functions; the collection comes last.
      CCL.Language.Interpret ("(each (fn ((n Integer)) (* n n)) [1 2 3 4])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_List and then
         Outcome.List_Length = 4 and then Outcome.List_Values (4).Integer = 16,
         "each maps a function over a list");
      CCL.Language.Interpret ("(where (fn ((n Integer)) (= (mod n 2) 0)) (range 1 10))", 1024, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Length = 5 and then
         Outcome.List_Values (1).Integer = 2 and then Outcome.List_Values (5).Integer = 10,
         "where keeps matching elements");
      CCL.Language.Interpret ("(fold (fn ((acc Integer) (n Integer)) (+ acc n)) 0 (range 1 100))", 4096, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 5050,
         "fold accumulates");
      CCL.Language.Interpret ("(sum (each (fn ((s String)) (length s)) [""ab"" ""cde"" """"]))", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 5,
         "each changes the element type; sum adds");
      CCL.Language.Interpret ("(any (fn ((n Integer)) (> n 3)) [1 5 (/ 1 0)])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Division_By_Zero,
         "operands are evaluated before the builtin runs");
      CCL.Language.Interpret ("(any (fn ((n Integer)) (> (/ 10 n) 3)) [1 0])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Boolean,
         "any stops at the first match");
      CCL.Language.Interpret ("(all (fn ((n Integer)) (> n 0)) [1 2 -3])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then not Outcome.Result_Value.Boolean,
         "all finds a counterexample");
      CCL.Language.Interpret ("(first 2 [""x"" ""y"" ""z""])", 512, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Length = 2 and then
         Outcome.List_Text.Data (1 .. Outcome.List_Text_Ends (2)) = "xy",
         "first takes a prefix");
      CCL.Language.Interpret ("(length (range 5 1))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 0,
         "an empty range");
      CCL.Language.Interpret ("(range 1 100000)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_List_Storage_Exhausted,
         "a range beyond the list region is a typed failure");
      CCL.Language.Interpret ("(where (fn ((n Integer)) (* n 2)) [1 2])", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Function_Argument_Mismatch,
         "where needs a Boolean-valued function");
      CCL.Language.Interpret ("(each (fn ((s String)) (length s)) [1 2])", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Function_Argument_Mismatch,
         "each checks the function's parameter against the elements");
      CCL.Language.Interpret ("(each (fn ((n Integer)) (* n n)) (range 1 500))", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Fuel_Exhausted,
         "builtins spend fuel per element, so they always end");
      --  Strings (subject last) and list builtins, round 2.
      declare
         type Integer_Array is array (Positive range <>) of Integer_64;
         procedure Text_Is (Source, Expected, Label : String) is
         begin
            CCL.Language.Interpret (Source, 100_000, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Text and then
                   Outcome.Result_Text.Data (1 .. Outcome.Result_Text.Length) = Expected, Label);
         end Text_Is;
         procedure Integer_Is (Source : String; Expected : Integer_64; Label : String) is
         begin
            CCL.Language.Interpret (Source, 100_000, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then
                   Outcome.Result_Value.Integer = Expected, Label);
         end Integer_Is;
         procedure Boolean_Is (Source : String; Expected : Boolean; Label : String) is
         begin
            CCL.Language.Interpret (Source, 100_000, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then
                   Outcome.Result_Value.Boolean = Expected, Label);
         end Boolean_Is;
         procedure Integers_Are (Source : String; Expected : Integer_Array; Label : String) is
            Same : Boolean;
         begin
            CCL.Language.Interpret (Source, 100_000, Outcome);
            Same := Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_List and then
              Outcome.List_Length = Expected'Length;
            if Same then
               for I in Expected'Range loop
                  Same := Same and then Outcome.List_Values (I - Expected'First + 1).Integer = Expected (I);
               end loop;
            end if;
            Check (Same, Label);
         end Integers_Are;
         procedure Status_Is (Source : String; Expected : CCL.Language.Interpretation_Status; Label : String) is
         begin
            CCL.Language.Interpret (Source, 100_000, Outcome);
            Check (Outcome.Status = Expected, Label);
         end Status_Is;
      begin
         Text_Is ("(upper ""Hello, World 42"")", "HELLO, WORLD 42", "upper");
         Text_Is ("(lower ""MiXeD"")", "mixed", "lower");
         Text_Is ("(trim ""  padded" & ASCII.HT & " "")", "padded", "trim blanks and tabs");
         Text_Is ("(trim ""   "")", "", "trim to empty");
         Text_Is ("(replace ""o"" ""0"" ""foo boo"")", "f00 b00", "replace every occurrence");
         Text_Is ("(replace ""ab"" """" ""abcab"")", "c", "replace with nothing");
         Text_Is ("(replace """" ""x"" ""abc"")", "abc", "an empty pattern changes nothing");
         Text_Is ("(join ""-"" (split "" "" ""a b c""))", "a-b-c", "split then join");
         Text_Is ("(join "", "" (split """" ""  the quick   fox ""))", "the, quick, fox",
                  "an empty separator splits words");
         Integer_Is ("(length (split "","" ""a,b,,c,""))", 5, "split keeps empty pieces");
         Text_Is ("(first 3 ""abcdef"")", "abc", "first on text");
         Text_Is ("(last 2 ""abcdef"")", "ef", "last on text");
         Text_Is ("(skip 2 ""abcdef"")", "cdef", "skip on text");
         Text_Is ("(first 99 ""ab"")", "ab", "first clamps");
         Text_Is ("(reverse ""stressed"")", "desserts", "reverse text");
         Boolean_Is ("(contains ""ell"" ""hello"")", True, "substring");
         Boolean_Is ("(contains ""xyz"" ""hello"")", False, "missing substring");
         Boolean_Is ("(starts-with ""he"" ""hello"")", True, "starts-with");
         Boolean_Is ("(ends-with ""hello"" ""lo"")", False, "ends-with longer than subject");
         Integer_Is ("(index-of ""l"" ""hello"")", 3, "index-of is 1-based");
         Integer_Is ("(index-of ""z"" ""hello"")", 0, "index-of absent is 0");
         Integer_Is ("(parse-int "" -42 "")", -42, "parse-int with blanks and sign");
         Integer_Is ("(+ 1 (parse-int ""9223372036854775806""))", 9_223_372_036_854_775_807, "parse-int at the range edge");
         Status_Is ("(parse-int ""4x2"")", CCL.Language.Evaluation_Invalid_Number, "parse-int rejects non-digits");
         Status_Is ("(parse-int ""-"")", CCL.Language.Evaluation_Invalid_Number, "parse-int rejects a bare sign");
         Status_Is ("(parse-int ""99999999999999999999"")", CCL.Language.Evaluation_Overflow, "parse-int overflow is typed");
         Boolean_Is ("(contains 3 [1 2 3])", True, "list membership");
         Boolean_Is ("(contains ""c"" [""a"" ""b""])", False, "string list membership");
         Integers_Are ("(reverse [1 2 3])", [3, 2, 1], "reverse a list");
         Integers_Are ("(sort [5 3 9 1 3])", [1, 3, 3, 5, 9], "sort integers");
         Integers_Are ("(last 2 (range 1 10))", [9, 10], "last on a list");
         Integers_Are ("(skip 8 (range 1 10))", [9, 10], "skip on a list");
         Integers_Are ("(sort-by (fn ((n Integer)) (- 0 n)) [2 9 4])", [9, 4, 2], "sort-by a computed key");
         Text_Is ("(join "" "" (sort [""pear"" ""apple"" ""fig""]))", "apple fig pear", "sort strings");
         Text_Is ("(join "" "" (sort-by (fn ((s String)) (length s)) [""pear"" ""apple"" ""fig""]))",
                  "fig pear apple", "sort-by string length");
         Integer_Is ("(count (fn ((n Integer)) (> n 2)) [1 2 3 4])", 2, "count matching");
         Integer_Is ("(min [4 -2 8])", -2, "min");
         Integer_Is ("(max [4 -2 8])", 8, "max");
         Status_Is ("(min (where (fn ((n Integer)) (> n 9)) [1 2]))", CCL.Language.Evaluation_Index_Error,
                    "min of an empty list is a typed error");
         Integer_Is ("(sum (sort (range 1 400)))", 80_200, "sort four hundred elements");
         Status_Is ("(sort (range 1 400))", CCL.Language.Succeeded, "sort within the default REPL fuel");
         CCL.Language.Interpret ("(sort (range 1 400))", 2_000, Outcome);
         Check (Outcome.Status = CCL.Language.Evaluation_Fuel_Exhausted,
                "sorting charges fuel per comparison");
         CCL.Language.Interpret ("(upper 42)", 256, Outcome);
         Check (Outcome.Status = CCL.Language.Type_Check_Failed, "upper needs text");
         CCL.Language.Interpret ("(sort [true false])", 256, Outcome);
         Check (Outcome.Status = CCL.Language.Type_Check_Failed, "Booleans have no order");
      end;
      CCL.Language.Interpret ("(and 1 true)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Expected_Boolean,
         "and requires Boolean operands");
      CCL.Language.Interpret ("(< true 1)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Expected_Integer,
         "ordering requires Integer operands");

      CCL.Language.Interpret ("""clock""", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Value and then Outcome.Has_Text and then
         Outcome.Result_Text.Length = 5 and then
         Outcome.Result_Text.Data (1 .. 5) = "clock",
         "interpret immutable string literal");
      CCL.Language.Interpret ("(length ""clock"")", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 5,
         "read string length");
      CCL.Language.Interpret ("(at ""clock"" 2)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Character and then Outcome.Result_Character = 'l',
         "index string from one");
      CCL.Language.Interpret
        ("(let ((left ""human"")) (concat left "" time""))", 24, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Text and then Outcome.Result_Text.Length = 10 and then
         Outcome.Result_Text.Data (1 .. 10) = "human time",
         "concatenate constrained string values");
      CCL.Language.Interpret ("""line\nnext""", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Text and then Outcome.Result_Text.Length = 9 and then
         Outcome.Result_Text.Data (5) = ASCII.LF,
         "decode bounded string escapes");
      CCL.Language.Interpret ("(at ""clock"" 0)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Index_Error,
         "reject string index outside actual bounds");
      CCL.Language.Interpret ("(length 42)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Expected_String,
         "type-check string operation operand");
      CCL.Language.Interpret
        ("(to-string -9223372036854775808)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Text and then Outcome.Result_Text.Length = 20 and then
         Outcome.Result_Text.Data (1 .. 20) = "-9223372036854775808",
         "format full-range signed integer text");
      CCL.Language.Interpret
        ("(let ((ms 3661000)) " &
         "(let ((h (/ ms 3600000))) " &
         "(let ((m (/ (mod ms 3600000) 60000))) " &
         "(let ((s (/ (mod ms 60000) 1000))) " &
         "(concat " &
         "(if (= (length (to-string h)) 1) " &
         "(concat ""0"" (to-string h)) (to-string h)) " &
         "(concat "":"" " &
         "(concat (if (= (length (to-string m)) 1) " &
         "(concat ""0"" (to-string m)) (to-string m)) " &
         "(concat "":"" (if (= (length (to-string s)) 1) " &
         "(concat ""0"" (to-string s)) (to-string s))))))))))",
         256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Text and then Outcome.Result_Text.Length = 8 and then
         Outcome.Result_Text.Data (1 .. 8) = "01:01:01",
         "express human-readable clock formatting with typed primitives");

      CCL.Language.Interpret
        ("# Whole-line comment" & ASCII.LF &
         "(+ 20 # Comment between operands" & ASCII.LF &
         "   22) # Trailing comment at end of input",
         16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Value and then
         Outcome.Result_Value.Kind = Integer_Value and then
         Outcome.Result_Value.Integer = 42,
         "ignore hash line comments as source trivia");

      CCL.Language.Interpret
        ("(let ((answer (+ 20 22))) (= answer 42))", 32, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Has_Value and then
         Outcome.Result_Value.Kind = Boolean_Value and then
         Outcome.Result_Value.Boolean,
         "interpret lexical binding");

      CCL.Language.Interpret ("(if false 10 20)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then
         Outcome.Result_Value.Integer = 20,
         "interpret conditional lazily");

      Make_Test_Catalog (Catalog, Catalog_Error);
      Check
        (Catalog_Error = CCL.Catalog.Catalog_Valid,
         "prepare visible source interface catalog");

      CCL.Language.Analyze ("(test.service.increment 41)", Analysis);
      Check
        (CCL.Language.Analysis_Status_Of (Analysis) =
           CCL.Language.Analysis_Parse_Failed and then
         CCL.Language.Analysis_Diagnostic (Analysis) =
           CCL.Language.Unknown_Form,
         "default analysis has no ambient interface discovery");

      CCL.Language.Analyze
        ("(test.service.increment 41)", Catalog, Analysis);
      Check
        (CCL.Language.Analysis_Status_Of (Analysis) =
           CCL.Language.Analysis_Succeeded and then
         CCL.Language.Analysis_Node
           (Analysis,
            CCL.Language.Node_Index
              (CCL.Language.Analysis_Root (Analysis))).Kind =
           CCL.Language.Host_Import_Form and then
         CCL.Language.Analysis_Node
           (Analysis,
            CCL.Language.Node_Index
              (CCL.Language.Analysis_Root (Analysis))).Static_Kind =
           CCL.Language.Integer_Type,
         "analyze host form through explicit catalog view");
      CCL.Language.Interpret
        ("(test.service.increment 41)", 8, Catalog, Outcome);
      Check
        (Outcome.Status = CCL.Language.Host_Import_Required and then
         not Outcome.Has_Value,
         "direct interpreter cannot turn discovery into authority");

      CCL.Language.Analyze
        ("(test.service.increment true)", Catalog, Analysis);
      Check
        (CCL.Language.Analysis_Status_Of (Analysis) =
           CCL.Language.Analysis_Type_Check_Failed and then
         CCL.Language.Analysis_Diagnostic (Analysis) =
           CCL.Language.Expected_Integer,
         "type-check catalog operation argument");

      CCL.Language.Interpret ("(+ true 4)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Expected_Integer and then
         Outcome.Diagnostic_Position = 1,
         "reject source operand type mismatch");

      CCL.Language.Interpret ("(if true 1 false)", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Branch_Type_Mismatch and then
         Outcome.Diagnostic_Position = 1,
         "reject mismatched conditional branches");

      CCL.Language.Interpret ("missing", 16, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Unknown_Name and then
         Outcome.Diagnostic_Position = 1,
         "reject unbound name");

      CCL.Language.Interpret ("(+ 1 2)", 2, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Fuel_Exhausted,
         "bound source evaluation with fuel");

      CCL.Language.Interpret ("(+ 9223372036854775807 1)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Overflow,
         "trap source arithmetic overflow");

      CCL.Language.Interpret ("(+ -9223372036854775808 -1)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Evaluation_Overflow,
         "trap negative source arithmetic overflow");

      CCL.Language.Interpret ("(+ 1)", 8, Outcome);
      Check
        (Outcome.Status = CCL.Language.Parse_Failed and then
         Outcome.Diagnostic_Position = 5,
         "reject malformed source");
   end Test_Source_Language;

   procedure Test_Source_Compiler is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Catalog  : CCL.Catalog.Interface_Catalog;
      Catalog_Error : CCL.Catalog.Catalog_Error;
      Grants   : CCL.Catalog.Granted_Bindings;
      Grant_Status : CCL.Catalog.Grant_Result;
      Link_Status  : CCL.Catalog.Link_Result;
      Tampered : CCL.VM.Program;
      Checked  : Validated_Program;
      Error    : Validation_Error;
      Outcome  : Execution_Result;
      State    : Machine_State;
      Debug_Error : CCL.Debug_Maps.Validation_Error;
      Debug_Match : CCL.Debug_Maps.Debug_Entry;
      Debug_Found : Boolean;
      Debug_Name  : CCL.Language.Name;
   begin
      CCL.Language.Analyze ("(not (= (+ 20 22) 41))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Length = 7 and then
         Compiled.Program.Code (0).Op = Push_Integer and then
         Compiled.Program.Code (2).Op = Add_Integer and then
         Compiled.Program.Code (4).Op = Equal_Integer and then
         Compiled.Program.Code (5).Op = Not_Boolean and then
         Compiled.Program.Code (6).Op = Halt,
         "compile scalar typed tree to CCLB");
      CCL.Debug_Maps.Validate
        (Compiled.Debug, Compiled.Program.Length, Debug_Error);
      CCL.Debug_Maps.Find_Innermost
        (Compiled.Debug, 2, Debug_Match, Debug_Found);
      Check
        (Debug_Error = CCL.Debug_Maps.Debug_Map_Valid and then
         Debug_Found and then Debug_Match.First_PC <= 2 and then
         Debug_Match.End_PC > 2 and then
         Debug_Match.Source_First > 0 and then
         Debug_Match.Source_End > Debug_Match.Source_First,
         "validate and resolve innermost CCL debug mapping");

      Verify (Compiled.Program, Checked, Error);
      Check (Error = Valid, "verify compiled scalar CCLB");
      if Error = Valid then
         Execute (Checked, 16, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Boolean_Value and then
            Outcome.Result_Value.Boolean,
            "execute compiled scalar CCLB");
      end if;

      CCL.Language.Analyze ("(+ (* 6 7) (mod 10 3))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Length = 8 and then
         Compiled.Program.Code (2).Op = Multiply_Integer and then
         Compiled.Program.Code (5).Op = Modulo_Integer and then
         Compiled.Program.Code (6).Op = Add_Integer,
         "compile extended arithmetic to CCLB");
      Verify (Compiled.Program, Checked, Error);
      if Error = Valid then
         Execute (Checked, 16, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Result_Value.Integer = 43,
            "execute compiled multiplication and modulo");
      else
         Check (False, "execute compiled multiplication and modulo");
      end if;

      CCL.Language.Analyze ("(/ 1 0)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Verify (Compiled.Program, Checked, Error);
      if Error = Valid then
         Execute (Checked, 8, Outcome);
         Check
           (Outcome.Status = Division_By_Zero,
            "report compiled division-by-zero failure");
      else
         Check (False, "report compiled division-by-zero failure");
      end if;

      --  Operators in CCLB: each source compiles, verifies and runs to the
      --  interpreter's result (differential check).
      declare
         procedure Same (Source : String; Label : String) is
            Interpreted : CCL.Language.Interpretation_Result;
         begin
            CCL.Language.Interpret (Source, 256, Interpreted);
            CCL.Language.Analyze (Source, Analysis);
            CCL.Compiler.Compile (Analysis, Compiled);
            if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then
               Check (False, Label & " (compile)"); return;
            end if;
            Verify (Compiled.Program, Checked, Error);
            if Error /= Valid then
               Check (False, Label & " (verify)"); return;
            end if;
            Execute (Checked, 256, Outcome);
            if Interpreted.Status /= CCL.Language.Succeeded then
               Check (Outcome.Status /= Completed, Label);
            elsif Outcome.Status /= Completed or else not Outcome.Has_Value then
               Check (False, Label & " (execute)");
            elsif Outcome.Result_Value.Kind = Integer_Value then
               Check (Outcome.Result_Value.Integer = Interpreted.Result_Value.Integer, Label);
            else
               Check (Outcome.Result_Value.Kind = Boolean_Value and then
                      Outcome.Result_Value.Boolean = Interpreted.Result_Value.Boolean, Label);
            end if;
         end Same;
      begin
         Same ("(- 50 8)", "CCLB subtraction");
         Same ("(- 5 8)", "CCLB subtraction below zero");
         Same ("(- 0 9223372036854775807)", "CCLB subtraction at the range edge");
         Same ("(- -9223372036854775807 2)", "CCLB subtraction overflow traps");
         Same ("(< 1 2)", "CCLB less");
         Same ("(< 2 2)", "CCLB less, equal operands");
         Same ("(<= 2 2)", "CCLB less or equal");
         Same ("(> 3 2)", "CCLB greater");
         Same ("(> 2 2)", "CCLB greater, equal operands");
         Same ("(>= 2 3)", "CCLB greater or equal");
         Same ("(/= 1 2)", "CCLB integer inequality");
         Same ("(/= true true)", "CCLB Boolean inequality");
         Same ("(= false false)", "CCLB Boolean equality");
         Same ("(and (< 1 2) (> 3 2))", "CCLB and");
         Same ("(or false (= 1 1))", "CCLB or");
         Same ("(and false (= (/ 1 0) 1))", "CCLB and short-circuits");
         Same ("(or true (= (/ 1 0) 1))", "CCLB or short-circuits");
         Same ("(and true (= (/ 1 0) 1))", "CCLB and evaluates its right operand when needed");
         Same ("(if (and (>= 5 1) (not (/= 4 4))) (- 10 3) 0)", "CCLB operators in a conditional");
         Same ("(define (double (n Integer)) Integer (* n 2)) (double 21)", "CCLB function call");
         Same ("(define (sub (a Integer) (b Integer)) Integer (- a b)) (sub 50 8)",
               "CCLB parameters keep their order");
         Same ("(define (sq (n Integer)) Integer (* n n)) " &
               "(define (sum-sq (a Integer) (b Integer)) Integer (+ (sq a) (sq b))) (sum-sq 3 4)",
               "CCLB a function calls an earlier function");
         Same ("(define (clamp (n Integer)) Integer (if (< n 0) 0 (if (> n 10) 10 n))) " &
               "(+ (clamp -5) (+ (clamp 7) (clamp 99)))", "CCLB conditionals inside a function");
         Same ("(define (f (n Integer)) Integer (let ((m (+ n 1))) (let ((k (* m 2))) (- k n)))) (f 5)",
               "CCLB lets inside a function live on the stack");
         Same ("(define (pos (n Integer)) Boolean (and (> n 0) (/= n 13))) (or (pos 13) (pos 2))",
               "CCLB a Boolean function with short-circuits");
         Same ("(define (inc (n Integer)) Integer (+ n 1)) (let ((x 40)) (inc (inc x)))",
               "CCLB a call inside a let");
         Same ("(define (boom (n Integer)) Integer (/ n 0)) (boom 1)", "CCLB a trap inside a function");
      end;

      --  Function-region rules, checked on tampered programs.
      CCL.Language.Analyze
        ("(define (sq (n Integer)) Integer (* n n)) " &
         "(define (quad (n Integer)) Integer (sq (sq n))) (quad 3)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
             Compiled.Program.Functions_Length = 2, "compile two functions");
      Verify (Compiled.Program, Checked, Error);
      Check (Error = Valid, "verify two functions");
      declare
         Call_Later : Boolean := False;
      begin
         --  sq (function 0) calling quad (function 1) would be a cycle.
         Tampered := Compiled.Program;
         for PC in CCL.VM.Instruction_Index range
           Tampered.Functions (0).Entry_PC .. Tampered.Functions (1).Entry_PC - 1
         loop
            if Tampered.Code (PC).Op = Multiply_Integer then
               Tampered.Code (PC) := (Op => Call_Function, Immediate => 1, others => <>);
               Call_Later := True;
               exit;
            end if;
         end loop;
         Verify (Tampered, Checked, Error);
         Check (Call_Later and then Error /= Valid, "reject a call to a later function");
      end;
      Tampered := Compiled.Program;
      Tampered.Code (CCL.VM.Instruction_Index (Tampered.Length - 1)) := (Op => Halt, others => <>);
      Verify (Tampered, Checked, Error);
      Check (Error /= Valid, "reject Halt inside a function");
      Tampered := Compiled.Program;
      Tampered.Functions (1).Count := 2;
      Verify (Tampered, Checked, Error);
      Check (Error /= Valid, "reject a frame that does not balance");
      Tampered := Compiled.Program;
      Tampered.Functions (1).Entry_PC := Tampered.Functions (0).Entry_PC;
      Verify (Tampered, Checked, Error);
      Check (Error = Invalid_Function, "reject overlapping function regions");
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Encode_Validation : Validation_Error;
      begin
         CCL.Format.Encode (Compiled.Program, Compiled.Linkage, (Fuel => 64, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Encode_Validation);
         Check (Format_Status = CCL.Format.Unsupported_Functions,
                "version 7 modules refuse functions until version 8");
      end;


      CCL.Language.Analyze ("(concat ""a"" ""b"")", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Unsupported_Form,
         "reject strings until CCLB has a variable-sized value representation");

      CCL.Language.Analyze ("(if false 1 (+ 20 22))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Length = 8 and then
         Compiled.Program.Code (1).Op = Jump_If_False and then
         Compiled.Program.Code (1).Target = 4 and then
         Compiled.Program.Code (3).Op = Jump and then
         Compiled.Program.Code (3).Target = 7,
         "compile forward conditional branches");
      Verify (Compiled.Program, Checked, Error);
      Check (Error = Valid, "verify compiled conditional CCLB");
      if Error = Valid then
         Execute (Checked, 16, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 42,
            "execute compiled conditional CCLB");
      end if;

      CCL.Language.Analyze
        ("(let ((answer 40)) " &
         "(let ((answer (+ answer 2))) (if true answer 0)))",
         Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Locals_Length = 2 and then
         Compiled.Program.Dynamic_Locals_Length = 2,
         "compile nested lexical locals and shadowing");
      Debug_Name := CCL.Debug_Maps.Local_Name (Compiled.Debug, 0);
      Check
        (CCL.Debug_Maps.Has_Local_Name (Compiled.Debug, 0) and then
         Debug_Name.Length = 6 and then
         Debug_Name.Data (1 .. Debug_Name.Length) = "answer",
         "preserve source local names in compiler debug metadata");
      Verify (Compiled.Program, Checked, Error);
      Check (Error = Valid, "verify compiled lexical-local CCLB");
      if Error = Valid then
         Execute (Checked, 32, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 42,
            "execute compiled lexical-local CCLB");
      end if;

      CCL.Language.Analyze
        ("(if true (let ((branch-only 1)) branch-only) 2)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Unsupported_Form and then
         Compiled.Source_Position = 10,
         "reject branch-local lifetime without ownership join semantics");

      Make_Test_Catalog (Catalog, Catalog_Error);
      Check
        (Catalog_Error = CCL.Catalog.Catalog_Valid,
         "prepare compiler interface catalog");
      CCL.Language.Analyze
        ("(test.service.increment 41)", Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Length = 3 and then
         Compiled.Program.Imports_Length = 1 and then
         CCL.Catalog.Length (Compiled.Linkage) = 1 and then
         CCL.Catalog.Element (Compiled.Linkage, 0).Interface_Digest =
           TEST_INTERFACE_DIGEST and then
         Compiled.Program.Imports (0).Argument = Integer_Value and then
         Compiled.Program.Imports (0).Result = Integer_Value and then
         Compiled.Program.Imports (0).Authority = Observe_Authority and then
         Compiled.Program.Imports (0).Binding = 0 and then
         Compiled.Program.Code (0).Op = Push_Integer and then
         Compiled.Program.Code (0).Immediate = 41 and then
         Compiled.Program.Code (1).Op = Invoke_Import and then
         Compiled.Program.Code (2).Op = Halt,
         "compile unresolved catalog operation with pinned linkage metadata");
      CCL.Catalog.Initialize (Grants);
      CCL.Catalog.Link_Program
        (Grants, Compiled.Linkage, Compiled.Program, Link_Status);
      Check
        (Link_Status = CCL.Catalog.Authority_Not_Granted and then
         Compiled.Program.Imports (0).Binding = 0,
         "refuse to turn interface discovery into invocation authority");
      CCL.Catalog.Install
        (Grants, CCL.Catalog.Element (Compiled.Linkage, 0),
         TEST_INCREMENT_BINDING, Grant_Status);
      Check
        (Grant_Status = CCL.Catalog.Grant_Added,
         "install authorized runtime binding");
      Tampered := Compiled.Program;
      Tampered.Imports (0).Authority := Control_Authority;
      CCL.Catalog.Link_Program
        (Grants, Compiled.Linkage, Tampered, Link_Status);
      Check
        (Link_Status = CCL.Catalog.Import_Contract_Mismatch and then
         Tampered.Imports (0).Binding = 0,
         "reject substituted import contract without partial linking");
      CCL.Catalog.Link_Program
        (Grants, Compiled.Linkage, Compiled.Program, Link_Status);
      Check
        (Link_Status = CCL.Catalog.Link_Valid and then
         Compiled.Program.Imports (0).Binding = TEST_INCREMENT_BINDING,
         "link exact catalog operation to granted binding");
      Verify (Compiled.Program, Checked, Error);
      Check (Error = Valid, "verify compiled catalog import");
      if Error = Valid then
         Initialize (Checked, 8, State);
         Continue_Execution (Checked, State, Outcome);
         Check
           (Outcome.Status = Waiting_For_Host and then
            Outcome.Requested_Authority = Observe_Authority and then
            Outcome.Requested_Binding = TEST_INCREMENT_BINDING and then
            Outcome.Request_Argument.Kind = Integer_Value and then
            Outcome.Request_Argument.Integer = 41,
            "suspend compiled catalog import at typed host boundary");
         Complete_Host_Call
           (Checked, State, Integer_Constant (1_234), Accepted => True);
         Continue_Execution (Checked, State, Outcome);
         Check
           (Outcome.Status = Completed and then Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 1_234,
            "resume compiled catalog import with typed result");
      end if;

      CCL.Language.Analyze
        ("(test.service.monotonic)", Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded and then
         Compiled.Program.Imports_Length = 1 and then
         Compiled.Program.Imports (0).Binding = 0 and then
         Compiled.Program.Code (0).Op = Push_Integer and then
         Compiled.Program.Code (0).Immediate = 0,
         "lower zero-parameter catalog operation through scalar ABI sentinel");
      CCL.Catalog.Install
        (Grants, CCL.Catalog.Element (Compiled.Linkage, 0),
         TEST_MONOTONIC_BINDING, Grant_Status);
      CCL.Catalog.Link_Program
        (Grants, Compiled.Linkage, Compiled.Program, Link_Status);
      Check
        (Grant_Status = CCL.Catalog.Grant_Added and then
         Link_Status = CCL.Catalog.Link_Valid and then
         Compiled.Program.Imports (0).Binding = TEST_MONOTONIC_BINDING,
         "link zero-parameter operation only after authority admission");

      CCL.Language.Analyze ("(+ true 1)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Analysis_Failed,
         "refuse compilation after failed analysis");
   end Test_Source_Compiler;

   procedure Test_Typed_Host_Import is
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      State     : Machine_State;
      Outcome   : Execution_Result;
   begin
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument  => Integer_Value,
         Result    => Integer_Value,
         Authority => Observe_Authority,
         Binding   => 42, others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) := Ins (Push_Integer, 41);
      Candidate.Code (1) := Ins (Invoke_Import, Import => 0);
      Candidate.Code (2) := Ins (Halt);

      Verify (Candidate, Checked, Error);
      Check (Error = Valid, "verify typed host import");
      if Error = Valid then
         Initialize (Checked, 8, State);
         Continue_Execution (Checked, State, Outcome);
         Check
           (Outcome.Status = Waiting_For_Host and then
            Outcome.Requested_Import = 0 and then
            Outcome.Requested_Authority = Observe_Authority and then
            Outcome.Requested_Binding = 42 and then
            Outcome.Request_Argument.Kind = Integer_Value and then
            Outcome.Request_Argument.Integer = 41,
            "suspend with typed host request");

         --  Deterministic Workbench mock for binding 42: increment its input.
         Complete_Host_Call
           (Checked, State,
            Integer_Constant (Outcome.Request_Argument.Integer + 1), True);
         Continue_Execution (Checked, State, Outcome);
         Check
           (Outcome.Status = Completed and then
            Outcome.Has_Value and then
            Outcome.Result_Value.Kind = Integer_Value and then
            Outcome.Result_Value.Integer = 42,
            "resume after Workbench host completion");
      end if;

      Candidate := (others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := Ins (Push_Integer, 1);
      Candidate.Code (1) := Ins (Invoke_Import, Import => 0);
      Verify (Candidate, Checked, Error);
      Check (Error = Invalid_Import, "reject undeclared host import");

      Candidate := (others => <>);
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Boolean_Value, Result => Integer_Value,
         Authority => Observe_Authority, Binding => 42, others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) := Ins (Push_Integer, 1);
      Candidate.Code (1) := Ins (Invoke_Import, Import => 0);
      Candidate.Code (2) := Ins (Halt);
      Verify (Candidate, Checked, Error);
      Check (Error = Type_Mismatch, "reject host argument type mismatch");
   end Test_Typed_Host_Import;

   procedure Test_Isolate_Scheduler is
      Import_Program : Program;
      Plain_Program  : Program;
      Import_Checked : Validated_Program;
      Plain_Checked  : Validated_Program;
      Error          : Validation_Error;
      Scheduler      : Scheduler_State;
      Event          : Scheduler_Event;
      Started        : Boolean;
      Import_Isolate : Isolate_Index;
      Plain_Isolate  : Isolate_Index;
      Matched        : Boolean;
      Token          : Unsigned_64;
   begin
      Import_Program.Imports_Length := 1;
      Import_Program.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Observe_Authority, Binding => 42, others => <>);
      Import_Program.Length := 3;
      Import_Program.Code (0) := Ins (Push_Integer, 41);
      Import_Program.Code (1) := Ins (Invoke_Import, Import => 0);
      Import_Program.Code (2) := Ins (Halt);
      Verify (Import_Program, Import_Checked, Error);
      Check (Error = Valid, "verify scheduled import program");

      Plain_Program.Length := 2;
      Plain_Program.Code (0) := Ins (Push_Integer, 7);
      Plain_Program.Code (1) := Ins (Halt);
      Verify (Plain_Program, Plain_Checked, Error);
      Check (Error = Valid, "verify scheduled plain program");

      Initialize (Scheduler);
      Start (Scheduler, Import_Checked, 8, Started, Import_Isolate);
      Check (Started and then Import_Isolate = 0, "start first isolate");
      Start (Scheduler, Plain_Checked, 8, Started, Plain_Isolate);
      Check (Started and then Plain_Isolate = 1, "start second isolate");

      Dispatch_One (Scheduler, Event);
      Check
        (Event.Kind = Host_Request and then Event.Isolate = Import_Isolate and then
         Event.Token /= 0 and then Event.Binding = 42,
         "suspend one scheduled isolate");
      Token := Event.Token;

      Dispatch_One (Scheduler, Event);
      Check
        (Event.Kind = Isolate_Completed and then
         Event.Isolate = Plain_Isolate and then Event.Has_Value and then
         Event.Value.Integer = 7,
         "run another isolate while import waits");

      Complete
        (Scheduler, Token + 1, Integer_Constant (42), True, Matched);
      Check
        (not Matched and then Status (Scheduler, Import_Isolate) = Waiting,
         "reject unknown completion token");
      Complete
        (Scheduler, Token, Integer_Constant (42), True, Matched);
      Check
        (Matched and then Status (Scheduler, Import_Isolate) = Runnable,
         "match completion to waiting isolate");

      Dispatch_One (Scheduler, Event);
      Check
        (Event.Kind = Isolate_Completed and then
         Event.Isolate = Import_Isolate and then Event.Has_Value and then
         Event.Value.Integer = 42,
         "resume scheduled isolate");
   end Test_Isolate_Scheduler;

   procedure Test_Module_Format is
      Candidate : Program;
      Decoded_Candidate : Program;
      Decoded   : Validated_Program;
      Data      : Byte_Array;
      Data_2    : Byte_Array;
      Length    : Module_Length;
      Length_2  : Module_Length;
      Limits    : constant Resource_Limits :=
        (Fuel => 16, Memory => 4_096, In_Flight => 1);
      Decoded_Limits : Resource_Limits;
      Error     : Format_Error;
      Validation : Validation_Error;
      Outcome   : Execution_Result;
      State     : Machine_State;
      Values    : Local_Value_Array := [others => (others => <>)];
      Accepted  : Boolean;
      SEND      : constant Disposition_Id := 1;
      Linkage   : CCL.Catalog.Linkage_Table;
      Decoded_Linkage : CCL.Catalog.Linkage_Table;
      Resolution : CCL.Catalog.Resolved_Operation;
      Link_Index : Import_Index;
      Interned   : CCL.Catalog.Intern_Result;
      Grants     : CCL.Catalog.Granted_Bindings;
      Grant_Status : CCL.Catalog.Grant_Result;
      Link_Status : CCL.Catalog.Link_Result;
   begin
      CCL.Catalog.Initialize (Linkage);
      Candidate.Length := 4;
      Candidate.Code (0) := Ins (Push_Integer, -5);
      Candidate.Code (1) := Ins (Push_Integer, 47);
      Candidate.Code (2) := Ins (Add_Integer);
      Candidate.Code (3) := Ins (Halt);
      Encode (Candidate, Limits, Data, Length, Error, Validation);
      Check
        (Error = Format_Valid and then Length > HEADER_SIZE,
         "encode canonical module");
      Encode (Candidate, Limits, Data_2, Length_2, Error, Validation);
      Check
        (Length = Length_2 and then Data = Data_2,
         "encode module deterministically");
      Decode
        (Data, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Format_Valid and then Decoded_Limits = Limits,
         "decode canonical module and limits");
      if Error = Format_Valid then
         Execute (Decoded, Decoded_Limits.Fuel, Outcome);
         Check
           (Outcome.Status = Completed and then Outcome.Has_Value and then
            Outcome.Result_Value.Integer = 42,
            "execute decoded module");
      end if;

      Data_2 := Data;
      Data_2 (MAGIC_OFFSET) := 0;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Bad_Magic, "reject bad module magic");

      Data_2 := Data;
      Data_2 (HEADER_RESERVED_OFFSET) := 1;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Bad_Reserved_Field, "reject nonzero module reserved field");

      Data_2 := Data;
      Data_2 (HEADER_SIZE + INSTRUCTION_OPCODE_OFFSET) := 99;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Invalid_Opcode, "reject invalid serialized opcode");

      Data_2 := Data;
      Data_2 (HEADER_SIZE + INSTRUCTION_OPCODE_OFFSET) :=
        Unsigned_8 (Op_Code'Enum_Rep (Add_Integer));
      for I in HEADER_SIZE + INSTRUCTION_IMMEDIATE_OFFSET ..
        HEADER_SIZE + INSTRUCTION_IMMEDIATE_OFFSET + 7
      loop
         Data_2 (I) := 0;
      end loop;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Bytecode_Invalid and then Validation = Stack_Underflow,
         "verify decoded bytecode before execution");

      Data_2 := Data;
      for I in FUEL_LIMIT_OFFSET .. FUEL_LIMIT_OFFSET + 3 loop
         Data_2 (I) := 0;
      end loop;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Invalid_Resource_Limit, "reject zero module fuel");

      Data_2 := Data;
      Data_2
        (HEADER_SIZE + 2 * INSTRUCTION_SIZE +
         INSTRUCTION_IMMEDIATE_OFFSET) := 1;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Noncanonical_Instruction,
         "reject noncanonical serialized instruction");

      Candidate := (others => <>);
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Observe_Authority, Binding => 0, others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) := Ins (Push_Integer, 41);
      Candidate.Code (1) := Ins (Invoke_Import, Import => 0);
      Candidate.Code (2) := Ins (Halt);
      Resolution :=
        (Interface_Digest => TEST_INTERFACE_DIGEST,
         Interface_Major => 1,
         Interface_Minor => 0,
         Operation => 0,
         Parameters => 1,
         Import => CCL.Host_Values.From_Bytecode (Candidate.Imports (0)));
      CCL.Catalog.Intern (Linkage, Resolution, Link_Index, Interned);
      Candidate.Imports (0).Binding := 42;
      Encode
        (Candidate, Linkage, Limits, Data_2, Length_2, Error, Validation);
      Check
        (Error = Runtime_Binding_In_Module and then Length_2 = 0,
         "refuse to serialize a runtime-local import binding");
      Candidate.Imports (0).Binding := 0;
      Encode
        (Candidate, Linkage, Limits, Data, Length, Error, Validation);
      if Error = Format_Valid then
         Data_2 := Data;
         Data_2 (HEADER_SIZE + IMPORT_TRANSFER_OFFSET) := 99;
         Decode
           (Data_2, Length, Decoded_Candidate, Decoded_Linkage,
            Decoded_Limits, Error, Validation);
         Check
           (Error = Invalid_Transfer_Mode,
            "reject invalid portable import transfer mode");

         Data_2 := Data;
         for I in HEADER_SIZE + IMPORT_DIGEST_OFFSET ..
           HEADER_SIZE + IMPORT_SIZE - 1
         loop
            Data_2 (I) := 0;
         end loop;
         Decode
           (Data_2, Length, Decoded_Candidate, Decoded_Linkage,
            Decoded_Limits, Error, Validation);
         Check
           (Error = Invalid_Linkage,
            "reject portable import without descriptor identity");

         Decode
           (Data, Length, Decoded_Candidate, Decoded_Linkage,
            Decoded_Limits, Error, Validation);
      end if;
      if Error = Format_Valid then
         CCL.Catalog.Initialize (Grants);
         CCL.Catalog.Install
           (Grants, CCL.Catalog.Element (Decoded_Linkage, 0), 42,
            Grant_Status);
         CCL.Catalog.Link_Program
           (Grants, Decoded_Linkage, Decoded_Candidate, Link_Status);
         Verify (Decoded_Candidate, Decoded, Validation);
         if Link_Status = CCL.Catalog.Link_Valid and then Validation = Valid
         then
            Execute (Decoded, Decoded_Limits.Fuel, Outcome);
         end if;
      end if;
      Check
        (Error = Format_Valid and then
         CCL.Catalog.Length (Decoded_Linkage) = 1 and then
         CCL.Catalog.Element (Decoded_Linkage, 0).Interface_Digest =
           TEST_INTERFACE_DIGEST and then
         Grant_Status = CCL.Catalog.Grant_Added and then
         Link_Status = CCL.Catalog.Link_Valid and then
         Outcome.Status = Waiting_For_Host and then
         Outcome.Requested_Authority = Observe_Authority and then
         Outcome.Requested_Binding = 42,
         "round-trip and explicitly link portable module import");

      Candidate := (others => <>);
      Candidate.Types_Length := 3;
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 1;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 1;
      Candidate.Local_Types (0) := 2;
      Candidate.Local_Kinds (0) := Integer_Value;
      Candidate.Length := 2;
      Candidate.Code (0) :=
        (Op => Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Candidate.Code (1) := (Op => Halt, others => <>);
      Encode (Candidate, Limits, Data, Length, Error, Validation);
      if Error = Format_Valid then
         Decode
           (Data, Length, Decoded, Decoded_Limits, Error, Validation);
      end if;
      Check (Error = Format_Valid, "round-trip owned module metadata");
      if Error = Format_Valid then
         Initialize (Decoded, 4, State);
         Continue_Execution (Decoded, State, Outcome);
         Check
           (Outcome.Status = Invalid_Bytecode,
            "decoded owned module requires host binding");
         Initialize_With_Locals
           (Decoded, 4, Values, 1, State, Accepted);
         Check (not Accepted, "decoded owned module rejects wrong type tag");
         Values (0) := With_Type (Integer_Constant (7), 2);
         Initialize_With_Locals
           (Decoded, 4, Values, 1, State, Accepted);
         if Accepted then
            Continue_Execution (Decoded, State, Outcome);
         end if;
         Check
           (Accepted and then Outcome.Status = Completed,
            "execute decoded owned module after exact injection");
      end if;

      Data_2 := Data;
      --  Type 2 begins after the two preceding fixed-size type entries.
      Data_2 (HEADER_SIZE + 2 * TYPE_SIZE + 1) := 2;
      Data_2 (HEADER_SIZE + 2 * TYPE_SIZE + 8) := SEND;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Invalid_Ownership_Metadata,
         "reject duplicate serialized disposition verb");

      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Control_Authority, Binding => 0,
         Ownership_Argument => True, Local => 0,
         Transfer => CCL.Imports.Move_Argument,
         Cancellation => CCL.Imports.Not_Cancellable,
         Success_Verb => SEND, Failure_Verb => SEND,
         Cancel_Verb => 0, others => <>);
      CCL.Catalog.Initialize (Linkage);
      Resolution :=
        (Interface_Digest => TEST_INTERFACE_DIGEST,
         Interface_Major => 1,
         Interface_Minor => 0,
         Operation => 1,
         Parameters => 1,
         Import => CCL.Host_Values.From_Bytecode (Candidate.Imports (0)));
      CCL.Catalog.Intern (Linkage, Resolution, Link_Index, Interned);
      Encode
        (Candidate, Linkage, Limits, Data, Length, Error, Validation);
      if Error = Format_Valid then
         Decode
           (Data, Length, Decoded_Candidate, Decoded_Linkage,
            Decoded_Limits, Error, Validation);
      end if;
      Check
        (Error = Format_Valid and then
         CCL.Catalog.Length (Decoded_Linkage) = 1 and then
         Decoded_Candidate.Imports (0).Ownership_Argument and then
         Decoded_Candidate.Imports (0).Transfer =
           CCL.Imports.Move_Argument and then
         Decoded_Candidate.Imports (0).Success_Verb = SEND,
         "v3 preserves owned import and portable linkage metadata");
   end Test_Module_Format;

   procedure Test_Ownership_Checker is
      SEND     : constant Disposition_Id := 1;
      CANCEL   : constant Disposition_Id := 2;
      RETURN_VALUE : constant Disposition_Id := 3;
      COMMIT   : constant Disposition_Id := 4;
      Types    : Type_Table := [others => (others => <>)];
      Env      : Environment;
      Left     : Environment;
      Right    : Environment;
      Joined   : Environment;
      Error    : Ownership_Error;
   begin
      Types (0).Mode := Unrestricted;
      Types (1).Mode := Move_Only;
      Types (2).Mode := Must_Handle;
      Types (2).Dispositions_Length := 3;
      Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Types (2).Dispositions (1) :=
        (Verb => CANCEL, Effect => Consume, Next_Type => 0);
      Types (2).Dispositions (2) :=
        (Verb => RETURN_VALUE, Effect => Transfer, Next_Type => 0);
      Types (3).Mode := Must_Handle;
      Types (3).Dispositions_Length := 1;
      Types (3).Dispositions (0) :=
        (Verb => COMMIT, Effect => Transition, Next_Type => 4);
      Types (4).Mode := Unrestricted;

      Initialize (Env);
      Declare_Binding (Env, 0, 0, Error);
      Copy_Value (Env, Types, 0, Error);
      Check (Error = Ownership_Valid, "copy unrestricted value");

      Declare_Binding (Env, 1, 1, Error);
      Copy_Value (Env, Types, 1, Error);
      Check
        (Error = Copy_Requires_Unrestricted, "reject copy of move-only value");
      Move_Value (Env, 1, Error);
      Check (Error = Ownership_Valid, "move move-only value");
      Move_Value (Env, 1, Error);
      Check (Error = Value_Not_Available, "reject use after move");

      Declare_Binding (Env, 2, 2, Error);
      Drop_Value (Env, Types, 2, Error);
      Check
        (Error = Drop_Requires_Unrestricted_Or_Move_Only,
         "reject drop of must-handle value");
      Apply_Disposition (Env, Types, 2, 99, Error);
      Check (Error = Unknown_Disposition, "reject undeclared disposition verb");
      Apply_Disposition (Env, Types, 2, SEND, Error);
      Check
        (Error = Ownership_Valid and then State (Env, 2) = Handled,
         "consume must-handle value with declared verb");
      Apply_Disposition (Env, Types, 2, SEND, Error);
      Check (Error = Value_Not_Available, "reject second disposition");

      Initialize (Env);
      Declare_Binding (Env, 0, 3, Error);
      Apply_Disposition (Env, Types, 0, COMMIT, Error);
      Check
        (Error = Ownership_Valid and then State (Env, 0) = Available and then
         Kind (Env, 0) = 4,
         "transition must-handle protocol state");
      Check_Scope (Env, Types, Error);
      Check
        (Error = Ownership_Valid,
         "accept terminal unrestricted protocol state");

      Initialize (Env);
      Declare_Binding (Env, 0, 2, Error);
      Borrow_RO (Env, 0, Error);
      Borrow_RO (Env, 0, Error);
      Check (Error = Ownership_Valid, "allow multiple borrowed-ro views");
      Borrow_RW (Env, 0, Error);
      Check (Error = Borrow_Conflict, "reject borrowed-rw during borrowed-ro");
      Return_RO (Env, 0, Error);
      Return_RO (Env, 0, Error);
      Borrow_RW (Env, 0, Error);
      Check (Error = Ownership_Valid, "allow exclusive borrowed-rw view");
      Borrow_RO (Env, 0, Error);
      Check (Error = Borrow_Conflict, "reject borrowed-ro during borrowed-rw");
      Move_Value (Env, 0, Error);
      Check (Error = Value_Not_Available, "reject move during borrow");
      Return_RW (Env, 0, Error);
      Apply_Disposition (Env, Types, 0, RETURN_VALUE, Error);
      Check (Error = Ownership_Valid, "return must-handle value after borrow");

      Initialize (Env);
      Declare_Binding (Env, 0, 0, Error);
      Borrow_RW (Env, 0, Error);
      Copy_Value (Env, Types, 0, Error);
      Check
        (Error = Borrow_Conflict,
         "reject unrestricted copy during borrowed-rw");

      Initialize (Left);
      Declare_Binding (Left, 0, 2, Error);
      Right := Left;
      Apply_Disposition (Left, Types, 0, CANCEL, Error);
      Join (Left, Right, Joined, Error);
      Check
        (Error = Branch_Ownership_Mismatch,
         "reject branch ownership mismatch");
      Apply_Disposition (Right, Types, 0, CANCEL, Error);
      Join (Left, Right, Joined, Error);
      Check (Error = Ownership_Valid, "join matching branch ownership");

      Initialize (Env);
      Declare_Binding (Env, 0, 2, Error);
      Check_Scope (Env, Types, Error);
      Check
        (Error = Outstanding_Must_Handle,
         "reject unhandled must-handle at scope exit");
      Initialize (Env);
      Declare_Binding (Env, 0, 1, Error);
      Check_Scope (Env, Types, Error);
      Check
        (Error = Outstanding_Move_Only,
         "require explicit move-only discard at scope exit");
      Drop_Value (Env, Types, 0, Error);
      Check_Scope (Env, Types, Error);
      Check (Error = Ownership_Valid, "accept explicit move-only discard");

      Check
        (Combine (Unrestricted, Move_Only) = Move_Only and then
         Combine (Move_Only, Must_Handle) = Must_Handle,
         "aggregate inherits strictest ownership mode");
   end Test_Ownership_Checker;

   procedure Test_Ownership_Bytecode is
      package OB renames CCL.Ownership.Bytecode;
      use type OB.Verification_Error;
      SEND   : constant Disposition_Id := 1;
      CANCEL : constant Disposition_Id := 2;
      Candidate : OB.Program;
      Result    : OB.Verification_Result;
   begin
      Candidate.Types (0).Mode := Unrestricted;
      Candidate.Types (1).Mode := Move_Only;
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 2;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Candidate.Types (2).Dispositions (1) :=
        (Verb => CANCEL, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 2;
      Candidate.Local_Types (0) := 2;
      Candidate.Local_Types (1) := 0;
      Candidate.Length := 5;
      Candidate.Code (0) := (Op => OB.Jump_If, Target => 3, others => <>);
      Candidate.Code (1) :=
        (Op => OB.Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Candidate.Code (2) := (Op => OB.Jump, Target => 4, others => <>);
      Candidate.Code (3) :=
        (Op => OB.Apply_Local_Disposition, Local => 0, Verb => CANCEL,
         others => <>);
      Candidate.Code (4) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Bytecode_Valid,
         "verify must-handle dispositions on both branches");

      Candidate.Code (3) := (Op => OB.Copy_Local, Local => 1, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Ownership_Join_Failure and then
         Result.Ownership_Error = Branch_Ownership_Mismatch,
         "reject bytecode branch ownership mismatch");

      Candidate := (others => <>);
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 1;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 1;
      Candidate.Local_Types (0) := 2;
      Candidate.Length := 2;
      Candidate.Code (0) := (Op => OB.Drop_Local, Local => 0, others => <>);
      Candidate.Code (1) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Ownership_Failure and then
         Result.Ownership_Error = Drop_Requires_Unrestricted_Or_Move_Only,
         "reject bytecode drop of must-handle local");

      Candidate.Code (0) :=
        (Op => OB.Borrow_Local_RO, Local => 0, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Ownership_Failure and then
         Result.Ownership_Error = Outstanding_Borrow,
         "reject bytecode halt with outstanding borrow");

      Candidate.Length := 4;
      Candidate.Code (0) :=
        (Op => OB.Borrow_Local_RW, Local => 0, others => <>);
      Candidate.Code (1) :=
        (Op => OB.Return_Local_RW, Local => 0, others => <>);
      Candidate.Code (2) :=
        (Op => OB.Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Candidate.Code (3) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Bytecode_Valid,
         "verify borrow return then disposition bytecode");

      Candidate.Length := 2;
      Candidate.Code (0) := (Op => OB.Move_Local, Local => 0, others => <>);
      Candidate.Code (1) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Bytecode_Valid,
         "verify bytecode transfer of must-handle local");

      Candidate.Code (0) := (Op => OB.Copy_Local, Local => 0, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Ownership_Failure and then
         Result.Ownership_Error = Copy_Requires_Unrestricted,
         "reject bytecode copy of must-handle local");

      Candidate := (others => <>);
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 2;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Candidate.Types (2).Dispositions (1) :=
        (Verb => CANCEL, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 1;
      Candidate.Local_Types (0) := 2;
      Candidate.Length := 2;
      Candidate.Code (0) :=
        (Op => OB.Import_Local, Local => 0,
         Import_Mode => OB.Move_Argument,
         Success_Verb => SEND, Failure_Verb => CANCEL, others => <>);
      Candidate.Code (1) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Bytecode_Valid,
         "verify moved import handles success and failure");

      Candidate.Code (0).Failure_Verb := 99;
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Ownership_Failure and then
         Result.Ownership_Error = Unknown_Disposition,
         "reject moved import with unhandled failure");

      Candidate.Code (0) :=
        (Op => OB.Import_Local, Local => 0,
         Import_Mode => OB.Borrowed_RW_Argument, others => <>);
      Candidate.Length := 3;
      Candidate.Code (1) :=
        (Op => OB.Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Candidate.Code (2) := (Op => OB.Halt, others => <>);
      OB.Verify (Candidate, Result);
      Check
        (Result.Error = OB.Bytecode_Valid,
         "verify mutable import borrow returns before continuation");
   end Test_Ownership_Bytecode;

   procedure Test_VM_Ownership_Admission is
      SEND : constant Disposition_Id := 1;
      Candidate : Program;
      Checked   : Validated_Program;
      Error     : Validation_Error;
      Outcome   : Execution_Result;
      State     : Machine_State;
      Values    : Local_Value_Array := [others => (others => <>)];
      Accepted  : Boolean;
   begin
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 1;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => SEND, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 1;
      Candidate.Local_Types (0) := 2;
      Candidate.Length := 2;
      Candidate.Code (0) :=
        (Op => Apply_Local_Disposition, Local => 0, Verb => SEND,
         others => <>);
      Candidate.Code (1) := (Op => Halt, others => <>);
      Verify (Candidate, Checked, Error);
      Check
        (Error = Valid,
         "admit ownership-verified executable VM program");
      if Error = Valid then
         Initialize (Checked, 4, State);
         Continue_Execution (Checked, State, Outcome);
         Check
           (Outcome.Status = Invalid_Bytecode,
            "reject owned locals without host injection");
         Initialize_With_Locals
           (Checked, 4, Values, 1, State, Accepted);
         Check (not Accepted, "reject mismatched injected local type");
         Values (0) := With_Type (Integer_Constant (99), 2);
         Initialize_With_Locals
           (Checked, 4, Values, 1, State, Accepted);
         if Accepted then
            Continue_Execution (Checked, State, Outcome);
         end if;
         Check
           (Accepted and then Outcome.Status = Completed,
            "execute ownership transitions defensively in VM");
      end if;

      Candidate.Code (0) := (Op => Drop_Local, Local => 0, others => <>);
      Verify (Candidate, Checked, Error);
      Check
        (Error = Invalid_Ownership,
         "reject invalid ownership in primary VM verifier");

      Candidate.Code (0) :=
        (Op => Borrow_Local_RO, Local => 0, others => <>);
      Verify (Candidate, Checked, Error);
      Check
        (Error = Invalid_Ownership,
         "reject outstanding borrow in primary VM verifier");

      Candidate.Code (0) := (Op => Move_Local, Local => 0, others => <>);
      Verify (Candidate, Checked, Error);
      Check
        (Error = Valid,
         "admit transfer of must-handle local from VM");

      Candidate := (others => <>);
      Candidate.Types_Length := 3;
      Candidate.Types (2).Mode := Must_Handle;
      Candidate.Types (2).Dispositions_Length := 2;
      Candidate.Types (2).Dispositions (0) :=
        (Verb => 1, Effect => Consume, Next_Type => 0);
      Candidate.Types (2).Dispositions (1) :=
        (Verb => 2, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 1;
      Candidate.Local_Types (0) := 2;
      Candidate.Local_Kinds (0) := Integer_Value;
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Control_Authority, Binding => 77,
         Ownership_Argument => True, Local => 0,
         Transfer => CCL.Imports.Move_Argument,
         Cancellation => CCL.Imports.Not_Cancellable,
         Success_Verb => 1, Failure_Verb => 2, Cancel_Verb => 0, others => <>);
      Candidate.Length := 2;
      Candidate.Code (0) := (Op => Invoke_Import, Import => 0, others => <>);
      Candidate.Code (1) := (Op => Halt, others => <>);
      Verify (Candidate, Checked, Error);
      Values (0) := With_Type (Integer_Constant (55), 2);
      if Error = Valid then
         Initialize_With_Locals
           (Checked, 8, Values, 1, State, Accepted);
         Continue_Execution (Checked, State, Outcome);
      end if;
      Check
        (Error = Valid and then Accepted and then
         Outcome.Status = Waiting_For_Host and then
         Outcome.Request_Argument.Integer = 55,
         "offer owned VM import without transferring early");
      if Error = Valid and then Accepted then
         Acknowledge_Host_Submission (Checked, State, True);
         Complete_Host_Call
           (Checked, State, Integer_Constant (56), True);
         Continue_Execution (Checked, State, Outcome);
      end if;
      Check
        (Outcome.Status = Completed and then Outcome.Has_Value and then
         Outcome.Result_Value.Integer = 56,
         "accept complete and resume owned VM import");

      if Error = Valid then
         Initialize_With_Locals
           (Checked, 8, Values, 1, State, Accepted);
         Continue_Execution (Checked, State, Outcome);
         Acknowledge_Host_Submission (Checked, State, False);
         Continue_Execution (Checked, State, Outcome);
      end if;
      Check
        (Outcome.Status = Host_Call_Failed,
         "reject owned VM import submission before transfer");

      if Error = Valid then
         Initialize_With_Locals
           (Checked, 8, Values, 1, State, Accepted);
         Continue_Execution (Checked, State, Outcome);
         Acknowledge_Host_Submission (Checked, State, True);
         Complete_Host_Call
           (Checked, State, Boolean_Constant (True), True);
         Continue_Execution (Checked, State, Outcome);
      end if;
      Check
        (Outcome.Status = Invalid_Bytecode,
         "complete owned import before rejecting wrong response type");
   end Test_VM_Ownership_Admission;

   procedure Test_Import_Lifecycle is
      package CI renames CCL.Imports;
      use type CI.Import_Error;
      use type CI.Import_Phase;
      Types : Type_Table := [others => (others => <>)];
      Env   : Environment;
      Life  : CI.Lifecycle;
      Error : CI.Import_Error;
      Own_Error : Ownership_Error;
   begin
      Types (2).Mode := Must_Handle;
      Types (2).Dispositions_Length := 3;
      Types (2).Dispositions (0) :=
        (Verb => 1, Effect => Consume, Next_Type => 0);
      Types (2).Dispositions (1) :=
        (Verb => 2, Effect => Consume, Next_Type => 0);
      Types (2).Dispositions (2) :=
        (Verb => 3, Effect => Consume, Next_Type => 0);
      Initialize (Env);
      Declare_Binding (Env, 0, 2, Own_Error);
      CI.Initialize (Life);
      CI.Offer
        (Life, 0, CI.Move_Argument, CI.Best_Effort_Cancellation,
         Success_Verb => 1, Failure_Verb => 2, Cancel_Verb => 3,
         Error => Error);
      CI.Reject_Submission (Life, Error);
      Check
        (Error = CI.Import_Valid and then
         CI.Phase (Life) = CI.Import_Idle and then
         State (Env, 0) = Available,
         "rejected import submission preserves ownership");

      CI.Offer
        (Life, 0, CI.Move_Argument, CI.Best_Effort_Cancellation,
         Success_Verb => 1, Failure_Verb => 2, Cancel_Verb => 3,
         Error => Error);
      CI.Accept_Submission (Life, Env, Types, Error);
      Check
        (Error = CI.Import_Valid and then State (Env, 0) = Moved,
         "accepted moved import suspends caller ownership");
      CI.Request_Cancellation (Life, Error);
      Check
        (Error = CI.Import_Valid and then
         CI.Phase (Life) = CI.Cancellation_Requested and then
         State (Env, 0) = Moved,
         "cancellation request does not release ownership");
      CI.Complete (Life, Env, Types, CI.Import_Cancelled, Error);
      Check
        (Error = CI.Import_Valid and then State (Env, 0) = Handled,
         "cancellation completion applies declared disposition");
      CI.Complete (Life, Env, Types, CI.Import_Cancelled, Error);
      Check
        (Error = CI.Invalid_Import_Phase,
         "reject duplicate import completion");

      Initialize (Env);
      Declare_Binding (Env, 0, 2, Own_Error);
      CI.Initialize (Life);
      CI.Offer
        (Life, 0, CI.Move_Argument, CI.Guaranteed_Cancellation_Request,
         Success_Verb => 1, Failure_Verb => 2, Cancel_Verb => 3,
         Error => Error);
      CI.Accept_Submission (Life, Env, Types, Error);
      CI.Request_Cancellation (Life, Error);
      CI.Complete (Life, Env, Types, CI.Import_Succeeded, Error);
      Check
        (Error = CI.Invalid_Import_Phase and then State (Env, 0) = Moved,
         "guaranteed cancellation rejects racing success completion");
      CI.Complete (Life, Env, Types, CI.Import_Cancelled, Error);
      Check
        (Error = CI.Import_Valid and then State (Env, 0) = Handled,
         "guaranteed cancellation accepts cancellation completion");

      Initialize (Env);
      Declare_Binding (Env, 0, 2, Own_Error);
      CI.Initialize (Life);
      CI.Offer
        (Life, 0, CI.Borrowed_RO_Argument, CI.Not_Cancellable,
         Success_Verb => 0, Failure_Verb => 0, Cancel_Verb => 0,
         Error => Error);
      CI.Accept_Submission (Life, Env, Types, Error);
      CI.Request_Cancellation (Life, Error);
      Check
        (Error = CI.Cancellation_Not_Supported and then
         CI.Phase (Life) = CI.Import_Accepted,
         "non-cancellable import remains accepted");
      CI.Complete (Life, Env, Types, CI.Import_Succeeded, Error);
      Check
        (Error = CI.Import_Valid and then State (Env, 0) = Available,
         "borrow returns only on terminal completion");
   end Test_Import_Lifecycle;
begin
   Test_Interface_Catalog;
   Test_Clock_Interface;
   Test_Addition;
   Test_Debug_Stepping;
   Test_Lexical_Local;
   Test_Branch;
   Test_Rejections;
   Test_Runtime_Limits;
   Test_Source_Language;
   Test_Source_Compiler;
   Test_Typed_Host_Import;
   Test_Isolate_Scheduler;
   Test_Module_Format;
   Test_Ownership_Checker;
   Test_Ownership_Bytecode;
   Test_Import_Lifecycle;
   Test_VM_Ownership_Admission;

   if Failures = 0 then
      Put_Line ("All CCL VM tests passed");
   else
      Put_Line (Natural'Image (Failures) & " CCL VM test(s) failed");
      raise Program_Error;
   end if;
end Main;
