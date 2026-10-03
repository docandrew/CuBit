with CCL.List_Operations;
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Sessions;
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

with Module_Patches;
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
      --  Literals longer than one syntax node's 16 components chain chunks.
      CCL.Language.Interpret ("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17]", 64, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.List_Total = 17 and then
         Outcome.List_Values (17).Integer = 17,
         "a literal longer than one chunk");
      --  Lists of records and variants, and record fields that are lists,
      --  live in the value arena and leave as literals that read back.
      declare
         Types_Source : constant String :=
           "(type C (record (a Integer) (s String))) " &
           "(type K (variant (A) (B Integer) (Boxed C))) " &
           "(type R (record (xs (List Integer)) (cs (List C)))) ";
         procedure Literal (Program, Expected, Name : String) is
            Again : CCL.Language.Interpretation_Result;
         begin
            CCL.Language.Interpret (Types_Source & Program, 100_000, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Literal and then
                   Outcome.Literal.Data (1 .. Outcome.Literal.Length) = Expected, Name);
            CCL.Language.Interpret (Types_Source & Expected, 100_000, Again);
            Check (Again.Status = CCL.Language.Succeeded and then Again.Has_Literal and then
                   Again.Literal.Data (1 .. Again.Literal.Length) = Expected, Name & " reads back");
         end Literal;
         procedure Count (Program : String; Expected : Integer_64; Name : String) is
         begin
            CCL.Language.Interpret (Types_Source & Program, 100_000, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then
                   Outcome.Result_Value = CCL.VM.Integer_Constant (Expected), Name);
         end Count;
      begin
         Literal ("[(C 1 ""x"") (C 2 ""y"")]", "[(C 1 ""x"") (C 2 ""y"")]", "a list of records");
         Literal ("[K.A (K.B 2) (K.Boxed (C 3 ""z""))]", "[K.A (K.B 2) (K.Boxed (C 3 ""z""))]",
                  "a list of variants");
         Literal ("(R [1 2] [(C 1 ""x"")])", "(R [1 2] [(C 1 ""x"")])", "list fields");
         Literal ("(R (list-of Integer) (list-of C))", "(R (list-of Integer) (list-of C))",
                  "empty list fields");
         Literal ("(each (fn ((n Integer)) (C n ""k"")) (range 1 3))",
                  "[(C 1 ""k"") (C 2 ""k"") (C 3 ""k"")]", "each builds records");
         Literal ("(sort-by (fn ((c C)) (field c a)) [(C 3 ""z"") (C 1 ""x"")])",
                  "[(C 1 ""x"") (C 3 ""z"")]", "sort-by orders records");
         Literal ("(reverse [(C 1 ""x"") (C 2 ""y"")])", "[(C 2 ""y"") (C 1 ""x"")]",
                  "reverse keeps records");
         Count ("(length (where (fn ((c C)) (> (field c a) 1)) [(C 1 ""x"") (C 2 ""y"") (C 3 ""z"")]))",
                2, "where filters records");
         Count ("(fold (fn ((t Integer) (c C)) (+ t (field c a))) 0 [(C 1 ""x"") (C 2 ""y"")])",
                3, "fold over records");
         Count ("(length (field (R [5 6 7] (list-of C)) xs))", 3, "a list field's length");
         Count ("(length (list-of C))", 0, "an empty typed list");
         Literal ("(first 1 (skip 1 [(C 1 ""x"") (C 9 ""y"")]))", "[(C 9 ""y"")]",
                  "first and skip keep records");
         --  Recursive types: a record or variant may hold a list of itself.
         declare
            Launch_Type : constant String :=
              "(type Launch (record (name String) (after (List Launch)))) ";
            Tree_Type : constant String :=
              "(type Tree (variant (Leaf Integer) (Node (List Tree)))) ";
            Again : CCL.Language.Interpretation_Result;
            procedure Recursive (Types, Program, Expected, Name : String) is
            begin
               CCL.Language.Interpret (Types & Program, 100_000, Outcome);
               Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Literal and then
                      Outcome.Literal.Data (1 .. Outcome.Literal.Length) = Expected, Name);
               CCL.Language.Interpret (Types & Expected, 100_000, Again);
               Check (Again.Status = CCL.Language.Succeeded and then Again.Has_Literal and then
                      Again.Literal.Data (1 .. Again.Literal.Length) = Expected, Name & " reads back");
            end Recursive;
         begin
            Recursive (Launch_Type,
              "(let ((a (Launch ""a"" (list-of Launch)))) (Launch ""b"" [a a]))",
              "(Launch ""b"" [(Launch ""a"" (list-of Launch)) (Launch ""a"" (list-of Launch))])",
              "a record holding a list of itself");
            Recursive (Tree_Type, "(Tree.Node [(Tree.Leaf 1) (Tree.Node [(Tree.Leaf 2)])])",
              "(Tree.Node [(Tree.Leaf 1) (Tree.Node [(Tree.Leaf 2)])])", "a recursive variant");
            Count (Launch_Type &
                   "(length (field (Launch ""c"" [(Launch ""a"" (list-of Launch))]) after))",
                   1, "a recursive field's length");
            --  A direct self field has no base case; only a list of itself.
            CCL.Language.Interpret ("(type T (record (next T))) 1", 1024, Outcome);
            Check (Outcome.Status = CCL.Language.Parse_Failed, "no direct self field");
            CCL.Language.Interpret ("(type T (record (x (List (List T))))) 1", 1024, Outcome);
            Check (Outcome.Status = CCL.Language.Parse_Failed, "no list of a list of itself");
            CCL.Language.Interpret ("(type T (record (x Integer))) (type U (record (y (List T)))) " &
              "(field (U [(T 1)]) y)", 1024, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Literal,
                   "ordinary list fields still work beside self lists");
         end;
         --  Range types: subtypes of Integer that constrain positions.
         declare
            Ranges : constant String :=
              "(type Priority (range 1 10)) (type Neg (range -5 -1)) " &
              "(type L (record (name String) (pri Priority))) " &
              "(define (bump (p Priority)) Priority (+ p 1)) ";
            procedure Status (Program : String; Expected : CCL.Language.Interpretation_Status;
                              Name : String) is
            begin
               CCL.Language.Interpret (Ranges & Program, 100_000, Outcome);
               Check (Outcome.Status = Expected, Name);
            end Status;
         begin
            Status ("(L ""a"" 10)", CCL.Language.Succeeded, "a literal inside the range");
            Status ("(L ""a"" 11)", CCL.Language.Type_Check_Failed, "a literal above the range");
            Check (Outcome.Diagnostic = CCL.Language.Value_Out_Of_Range, "out of range is typed");
            Status ("(L ""a"" 0)", CCL.Language.Type_Check_Failed, "a literal below the range");
            Status ("(L ""a"" (+ 5 5))", CCL.Language.Succeeded, "a computed value inside");
            Status ("(L ""a"" (+ 5 6))", CCL.Language.Evaluation_Range_Error,
                    "a computed value outside is a typed run-time error");
            Status ("(bump 9)", CCL.Language.Succeeded, "a range parameter and result");
            Status ("(bump 10)", CCL.Language.Evaluation_Range_Error, "a range result is checked");
            Status ("(bump 0)", CCL.Language.Type_Check_Failed, "a range argument literal is checked");
            Status ("(each (fn ((p Priority)) (L ""x"" p)) (range 8 11))",
                    CCL.Language.Evaluation_Range_Error, "function values check their ranges");
            Status ("(type R (record (n Neg))) (R -6)", CCL.Language.Type_Check_Failed,
                    "negative bounds");
            Count ("(type Priority (range 1 10)) (type L (record (name String) (pri Priority))) " &
                   "(+ (field (L ""a"" 7) pri) 100)", 107, "a range field reads as an Integer");
            declare
               Decls : constant String :=
                 "(type Priority (range 1 10)) (type L (record (name String) (pri Priority))) ";
            begin
               CCL.Language.Interpret (Decls & "(L ""a"" (+ 3 4))", 100_000, Outcome);
               Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Has_Literal and then
                      Outcome.Literal.Data (1 .. Outcome.Literal.Length) = "(L ""a"" 7)",
                      "a record with a range field");
               CCL.Language.Interpret (Decls & "(L ""a"" 7)", 100_000, Outcome);
               Check (Outcome.Status = CCL.Language.Succeeded and then
                      Outcome.Literal.Data (1 .. Outcome.Literal.Length) = "(L ""a"" 7)",
                      "a record with a range field reads back");
            end;
            CCL.Language.Interpret ("(type Bad (range 5 1)) 1", 1024, Outcome);
            Check (Outcome.Status = CCL.Language.Parse_Failed, "an empty range is refused");
            CCL.Language.Interpret ("(type P (range 1 10)) (type R (record (xs (List P)))) 1", 1024, Outcome);
            Check (Outcome.Status = CCL.Language.Parse_Failed and then
                   Outcome.Diagnostic = CCL.Language.Unsupported_List_Element,
                   "no lists of range types yet");
         end;
         --  Stream types (docs/ccl-streams.md): data elements only, never stored.
         declare
            procedure Refused (Source : String; Code : CCL.Language.Diagnostic_Code; Name : String) is
            begin
               CCL.Language.Interpret (Source, 4096, Outcome);
               Check (Outcome.Status = CCL.Language.Parse_Failed and then
                      Outcome.Diagnostic = Code, Name);
            end Refused;
         begin
            CCL.Language.Interpret ("(define (f (s (Stream Integer))) Integer 1) 1", 4096, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded, "a stream parameter type-checks");
            CCL.Language.Interpret
              ("(type P (record (x Integer) (y Integer))) " &
               "(define (f (s (Stream P)) (t (Stream (List P)))) Integer 1) 1", 4096, Outcome);
            Check (Outcome.Status = CCL.Language.Succeeded, "streams of records and of lists");
            Refused ("(define (f (s (Stream (Stream Integer)))) Integer 1) 1",
                     CCL.Language.Unsupported_Stream_Element, "no streams of streams");
            Refused ("(define (f (s (Stream (Function (Integer) Integer)))) Integer 1) 1",
                     CCL.Language.Unsupported_Stream_Element, "no streams of functions");
            Refused ("(type R (record (s (Stream Integer)))) 1",
                     CCL.Language.Stream_Not_Data, "a stream is not a record field");
            Refused ("(type V (variant (live (Stream Integer)) (none))) 1",
                     CCL.Language.Stream_Not_Data, "a stream is not a variant payload");
            Refused ("(define (f (s (List (Stream Integer)))) Integer 1) 1",
                     CCL.Language.Unsupported_List_Element, "no lists of streams");
         end;
         --  Still refused: lists of lists, and lists across a host boundary.
         CCL.Language.Interpret ("[[1] [2]]", 1024, Outcome);
         Check (Outcome.Status = CCL.Language.Type_Check_Failed, "no lists of lists yet");
         CCL.Language.Interpret ("(list-of Nope)", 1024, Outcome);
         Check (Outcome.Status in CCL.Language.Parse_Failed | CCL.Language.Type_Check_Failed,
                "list-of an unknown type");
      end;
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
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Lambda_Parameter_Needs_Type,
         "an untyped parameter with nothing to infer from asks for its type");
      CCL.Language.Interpret ("(sum (each (fn (n) (* n n)) [1 2 3]))", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 14,
         "each infers its function's parameter from the elements");
      CCL.Language.Interpret ("(fold (fn (acc n) (+ acc n)) 0 (range 1 10))", 1024, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 55,
         "fold infers the accumulator from init and the element from the list");
      CCL.Language.Interpret ("(each (fn (s) (upper s)) [1 2])", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Type_Check_Failed and then
         Outcome.Diagnostic = CCL.Language.Expected_String,
         "an inferred parameter is checked like a written one");
      CCL.Language.Interpret
        ("(define (twice (f (Function (Integer) Integer)) (x Integer)) Integer (f (f x))) " &
         "(twice (fn (n) (* n 3)) 2)", 256, Outcome);
      Check
        (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 18,
         "a call infers from the declared function type");
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
      --  Pipelines: (->> x (f a) g) threads x through each stage as its last
      --  argument; a bare name stage is a one-argument call.
      CCL.Language.Interpret ("(->> (range 1 20) (where (fn (n) (= (mod n 3) 0))) sum)", 10_000, Outcome);
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 63,
             "a pipeline threads the value through builtins");
      CCL.Language.Interpret ("(define (double (n Integer)) Integer (* n 2)) (->> 5 double double)", 256, Outcome);
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 20,
             "a bare-name stage calls a defined function");
      CCL.Language.Interpret ("(->> ""abc"" length)", 256, Outcome);
      Check (Outcome.Status = CCL.Language.Succeeded and then Outcome.Result_Value.Integer = 3,
             "length is a stage");
      CCL.Language.Interpret ("(->> 1 (+ 2))", 256, Outcome);
      Check (Outcome.Status = CCL.Language.Parse_Failed, "an operator is not a stage");
      CCL.Language.Interpret ("(->> [1 2] (each (fn (n) (* n 10))) (join "",""))", 256, Outcome);
      Check (Outcome.Status = CCL.Language.Type_Check_Failed, "stages are type-checked like calls");

      --  The persistent session environment (CCL.Sessions).
      declare
         S : CCL.Sessions.Session;
         R : CCL.Language.Interpretation_Result;
         procedure Step (Source, Expected, Label : String) is
         begin
            CCL.Sessions.Submit (S, Source, CCL.Sessions.Default_Fuel, R);
            Check (CCL.Sessions.Result_Image (R) = Expected, Label);
            if CCL.Sessions.Result_Image (R) /= Expected then
               Ada.Text_IO.Put_Line ("   got: " & CCL.Sessions.Result_Image (R));
            end if;
         end Step;
      begin
         CCL.Sessions.Initialize (S);
         Step ("(define (sq (n Integer)) Integer (* n n))", "String: defined sq", "a Lisp definition is kept");
         Step ("(sq 12)", "Integer: 144", "a later entry calls it");
         Step ("FUNCTION cube(n AS Integer) AS Integer RETURN n * sq(n) END", "String: defined cube",
               "a BASIC definition alone is kept");
         Step ("cube(3)", "Integer: 27", "BASIC calls across entries and dialects");
         Step ("LET words = split("""", ""the quick brown fox"")",
               "List<String>: [""the"", ""quick"", ""brown"", ""fox""]", "LET keeps a list value");
         Step ("length(words)", "Integer: 4", "a kept value is in scope");
         Step ("(define total (sum (each (fn ((w String)) (length w)) words)))", "Integer: 16",
               "a Lisp value binding shows its value");
         Step ("LET total = total * 2", "Integer: 32", "rebinding sees the old value");
         Step ("total", "Integer: 32", "the rebound value is kept");
         Step ("(define (sq (n Integer)) Integer (+ n n))", "String: defined sq", "redefinition replaces");
         Step ("cube(3)", "Integer: 18", "dependents use the new definition");
         Step ("(define (sq (n Boolean)) Boolean n)",
               "Expression does not type-check: Argument type does not match the function parameter",
               "a redefinition that breaks a dependent is refused");
         Step ("cube(3)", "Integer: 18", "and the environment is unchanged");
         Step ("(define big (range 1 100))",
               "Value cannot be kept in the session (too long or not storable); define a function instead",
               "a value too large to keep is reported");
         Step ("big", "Expression does not type-check: Unknown name (keep a value with LET x = ... or (define x ...)) at character 1", "and it is not kept");
         Step (":env", "String: definitions: sq, cube; values: words = [""the"" ""quick"" ""brown"" ""fox""], total = 32",
               ":env lists the environment");
         CCL.Sessions.Submit (S, "LET 1x = 2", CCL.Sessions.Default_Fuel, R);
         Check (R.Status /= CCL.Language.Succeeded and then CCL.Sessions.Kept_Values (S) = 2,
                "a value name must be an identifier");
         Step (":reset", "String: session environment cleared", ":reset");
         Step ("words", "Expression does not type-check: Unknown name (keep a value with LET x = ... or (define x ...)) at character 1", ":reset forgets values");
         Check (CCL.Sessions.Kept_Definitions (S) = 0 and CCL.Sessions.Kept_Values (S) = 0,
                ":reset forgets everything");
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
               Check (False, Label & " (compile: " & CCL.Compiler.Compilation_Status'Image (Compiled.Status) & ")");
               return;
            end if;
            Verify (Compiled.Program, Checked, Error);
            if Error /= Valid then
               Check (False, Label & " (verify: " & Validation_Error'Image (Error) & ")"); return;
            end if;
            Execute (Checked, 256, Outcome);
            if (Interpreted.Status = CCL.Language.Succeeded) /= (Outcome.Status = Completed) then
               Put_Line ("  interpreter " & CCL.Language.Interpretation_Status'Image (Interpreted.Status) &
                         ", compiled " & Execution_Status'Image (Outcome.Status));
            end if;
            if Interpreted.Status /= CCL.Language.Succeeded then
               Check (Outcome.Status /= Completed, Label);
            elsif Outcome.Status /= Completed or else not Outcome.Has_Value then
               Check (False, Label & " (execute)");
            elsif Outcome.Has_Literal or else Interpreted.Has_Literal then
               Check (Outcome.Has_Literal and then Interpreted.Has_Literal and then
                      Outcome.Literal.Data (1 .. Outcome.Literal.Length) =
                        Interpreted.Literal.Data (1 .. Interpreted.Literal.Length), Label);
            elsif Outcome.Result_Value.Kind = Integer_Value then
               Check (Outcome.Result_Value.Integer = Interpreted.Result_Value.Integer, Label);
            elsif Outcome.Result_Value.Kind = Function_Value then
               Check (Interpreted.Has_Function, Label);
            elsif Outcome.Result_Value.Kind = List_Value then
               declare
                  Same_List : Boolean :=
                    Interpreted.Has_List and then
                    Outcome.List_Total = Interpreted.List_Total and then
                    Outcome.List_Length = Interpreted.List_Length and then
                    Outcome.List_Text.Length = Interpreted.List_Text.Length and then
                    Outcome.List_Text.Data (1 .. Outcome.List_Text.Length) =
                      Interpreted.List_Text.Data (1 .. Interpreted.List_Text.Length);
               begin
                  for I in 1 .. Outcome.List_Length loop
                     exit when not Same_List;
                     Same_List :=
                       Outcome.List_Text_Ends (I) = Interpreted.List_Text_Ends (I) and then
                       Outcome.List_Values (I).Kind = Interpreted.List_Values (I).Kind and then
                       Outcome.List_Values (I).Integer = Interpreted.List_Values (I).Integer and then
                       Outcome.List_Values (I).Boolean = Interpreted.List_Values (I).Boolean;
                  end loop;
                  Check (Same_List, Label);
               end;
            elsif Outcome.Result_Value.Kind = Character_Value then
               Check (Interpreted.Has_Character and then
                      Outcome.Result_Value.Integer =
                        Character'Pos (Interpreted.Result_Character), Label);
            elsif Outcome.Result_Value.Kind = Text_Value then
               Check (Interpreted.Has_Text and then Outcome.Has_Result_Text and then
                      Outcome.Result_Text_Value.Data (1 .. Outcome.Result_Text_Value.Length) =
                        Interpreted.Result_Text.Data (1 .. Interpreted.Result_Text.Length), Label);
            else
               Check (Outcome.Result_Value.Kind = Boolean_Value and then
                      Outcome.Result_Value.Boolean = Interpreted.Result_Value.Boolean, Label);
            end if;
         end Same;
      begin
         Same ("(- 50 8)", "CCLB subtraction");
         --  A lambda inside a definition keeps its slot: a later lambda
         --  must not take it (the parser once reset the count after a body).
         Same ("(define (h (x Integer)) Integer (fold (fn ((a Integer) (b Integer)) (+ a b)) x (range 1 2))) " &
               "(each (fn ((i Integer)) (h i)) (range 0 2))", "lambda slots after a definition's lambda");
         Same ("(define (h (x Integer)) Integer (fold (fn ((a Integer) (b Integer)) (+ a x)) 0 (range 1 2))) " &
               "(define (k (y Integer)) Integer (h (+ y 1))) (each (fn ((i Integer)) (k i)) (range 0 2))",
               "captures through nested definitions");
         Same ("(- 5 8)", "CCLB subtraction below zero");
         Same ("(- 0 9223372036854775807)", "CCLB subtraction at the range edge");
         Same ("(- -9223372036854775807 2)", "CCLB subtraction overflow traps");
         Same ("(< 1 2)", "CCLB less");
         --  Text (parity step 2): constants, concat, length, equality.
         Same ("""hello""", "CCLB a text literal");
         Same ("(concat ""ab"" ""cd"")", "CCLB concat");
         Same ("(length (concat ""ab"" ""cde""))", "CCLB length of a concat");
         Same ("(= ""ab"" (concat ""a"" ""b""))", "CCLB text equality");
         Same ("(/= ""ab"" ""abc"")", "CCLB text inequality");
         Same ("(if (= ""x"" ""x"") (concat ""y"" ""es"") ""no"")", "CCLB text in branches");
         Same ("(let ((s ""hi"")) (concat s s))", "CCLB text in a local");
         Same ("(concat """" """")", "CCLB empty text");
         Same ("(concat ""ab"" ""ab"")", "CCLB equal literals share a constant");
         --  The shared string built-ins: one implementation, both engines.
         Same ("(upper ""Hello, World"")", "CCLB upper");
         Same ("(lower ""Hello, World"")", "CCLB lower");
         Same ("(trim ""  padded \t"")", "CCLB trim");
         Same ("(reverse ""stressed"")", "CCLB reverse text");
         Same ("(first 3 ""abcdef"")", "CCLB first on text");
         Same ("(last 2 ""abcdef"")", "CCLB last on text");
         Same ("(skip 4 ""abcdef"")", "CCLB skip on text");
         Same ("(first 99 ""abc"")", "CCLB first past the end");
         Same ("(skip -1 ""abc"")", "CCLB skip below zero");
         Same ("(contains ""lo, W"" ""Hello, World"")", "CCLB contains");
         Same ("(contains ""xyz"" ""Hello"")", "CCLB contains (absent)");
         Same ("(index-of ""World"" ""Hello, World"")", "CCLB index-of");
         Same ("(index-of """" ""abc"")", "CCLB index-of an empty pattern");
         Same ("(starts-with ""He"" ""Hello"")", "CCLB starts-with");
         Same ("(ends-with ""lo"" ""Hello"")", "CCLB ends-with");
         Same ("(ends-with ""Hello!"" ""Hello"")", "CCLB ends-with (longer pattern)");
         Same ("(replace ""a"" ""ooo"" ""banana"")", "CCLB replace");
         Same ("(replace """" ""x"" ""keep"")", "CCLB replace with an empty pattern");
         Same ("(parse-int "" -42 "")", "CCLB parse-int");
         Same ("(parse-int ""12x"")", "CCLB parse-int rejects junk");
         Same ("(parse-int ""99999999999999999999"")", "CCLB parse-int overflow");
         Same ("(length (upper (concat ""ab"" ""cd"")))", "CCLB built-ins compose");
         Same ("(let ((p (concat ""0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"" " &
               """0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef""))) " &
               "(let ((q (concat p p))) (let ((r (concat q q))) (let ((t (concat r r))) " &
               "(let ((u (concat t ""!""))) (contains u ""abc""))))))",
               "CCLB a pattern over 1 KiB fails as in the interpreter");
         --  Characters and to-string.
         Same ("(at ""hello"" 2)", "CCLB at");
         Same ("(at ""hello"" 5)", "CCLB at the last character");
         Same ("(at ""hello"" 0)", "CCLB at index zero fails");
         Same ("(at ""hello"" 6)", "CCLB at past the end fails");
         Same ("(at """" 1)", "CCLB at on empty text fails");
         Same ("(= (at ""abc"" 2) (at ""xbz"" 2))", "CCLB character equality");
         Same ("(/= (at ""abc"" 1) (at ""abc"" 3))", "CCLB character inequality");
         Same ("(let ((c (at ""xyz"" 3))) (= c c))", "CCLB a character in a local");
         Same ("(to-string 42)", "CCLB to-string");
         Same ("(to-string -9223372036854775807)", "CCLB to-string near the low edge");
         Same ("(to-string (- -9223372036854775807 1))", "CCLB to-string overflow fails first");
         Same ("(length (to-string 1000))", "CCLB to-string composes");
         Same ("(concat ""n="" (to-string (* 6 7)))", "CCLB to-string in concat");
         Same ("(type Color (variant (Red) (Green) (Blue))) (= Color.Red Color.Red)",
               "CCLB enumeration equality");
         Same ("(type Color (variant (Red) (Green) (Blue))) (to-string Color.Green)",
               "CCLB to-string on an enumeration");
         Same ("(type Color (variant (Red) (Green) (Blue))) (concat (to-string Color.Red) (to-string Color.Blue))",
               "CCLB to-string on enumerations in concat");
         --  Lists (parity step 3): literals, length, at, locals, results.
         Same ("[1 2 3]", "CCLB an integer list");
         Same ("[true false true]", "CCLB a Boolean list");
         Same ("[""a"" ""bc"" """"]", "CCLB a string list");
         Same ("[(at ""xy"" 2) (at ""xy"" 1)]", "CCLB a character list");
         Same ("(type Color (variant (Red) (Green) (Blue))) [Color.Blue Color.Red]",
               "CCLB an enumeration list");
         Same ("(list-of Integer)", "CCLB an empty list");
         Same ("(length (list-of String))", "CCLB length of an empty list");
         Same ("(length [4 5 6 7])", "CCLB length of a list");
         Same ("(at [10 20 30] 2)", "CCLB at on a list");
         Same ("(at [""p"" ""q""] 2)", "CCLB at on a string list");
         Same ("(type Color (variant (Red) (Green) (Blue))) (to-string (at [Color.Blue Color.Red] 2))",
               "CCLB at on an enumeration list");
         Same ("(at [1 2] 0)", "CCLB list index zero fails");
         Same ("(at [1 2] 3)", "CCLB list index past the end fails");
         Same ("(at (list-of Integer) 1)", "CCLB at on an empty list fails");
         Same ("(let ((xs [1 2 3])) (+ (at xs 3) (length xs)))", "CCLB a list in a local");
         Same ("(if (< 1 2) [1] [2 3])", "CCLB lists in branches");
         Same ("(let ((n 5)) [n (* n n) (+ n 1)])", "CCLB computed elements");
         Same ("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 " &
               "31 32 33 34 35 36 37 38 39 40]", "CCLB a chunked literal");
         Same ("(at [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20] 18)", "CCLB at in a later chunk");
         Same ("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 " &
               "31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 " &
               "59 60 61 62 63 64 65 66 67 68 69 70]", "CCLB a result longer than it carries");
         Same ("(let ((s ""0123456789012345678901234567890123456789012345678901234567890123"")) " &
               "(let ((t (concat s s))) (let ((u (concat t t))) (let ((v (concat u u))) [v v v]))))",
               "CCLB string list results carry what fits");
         --  List built-ins (step 3b): shared algorithms, both engines.
         Same ("(first 2 [1 2 3])", "CCLB first on a list");
         Same ("(last 2 [1 2 3])", "CCLB last on a list");
         Same ("(skip 1 [1 2 3])", "CCLB skip on a list");
         Same ("(first -1 [1 2])", "CCLB first below zero");
         Same ("(skip 99 [1 2])", "CCLB skip past the end");
         Same ("(reverse [1 2 3])", "CCLB reverse a list");
         Same ("(reverse [""a"" ""b""])", "CCLB reverse a string list");
         Same ("(sort [3 1 2 5 4])", "CCLB sort integers");
         Same ("(sort [""b"" ""a"" ""ab"" """"])", "CCLB sort strings");
         Same ("(sort [(at ""zay"" 1) (at ""zay"" 2) (at ""zay"" 3)])", "CCLB sort characters");
         Same ("(type Color (variant (Red) (Green) (Blue))) (sort [Color.Blue Color.Red Color.Green])",
               "CCLB sort an enumeration");
         Same ("(sort (list-of Integer))", "CCLB sort an empty list");
         Same ("(sum [1 2 3])", "CCLB sum");
         Same ("(sum (list-of Integer))", "CCLB sum of nothing");
         Same ("(sum [9223372036854775807 1])", "CCLB sum overflow fails");
         Same ("(min [3 1 2])", "CCLB min");
         Same ("(max [3 1 2])", "CCLB max");
         Same ("(min (list-of Integer))", "CCLB min of nothing fails");
         Same ("(contains 2 [1 2 3])", "CCLB contains an integer");
         Same ("(contains 7 [1 2 3])", "CCLB contains (absent)");
         Same ("(contains ""b"" [""a"" ""b""])", "CCLB contains a string");
         Same ("(contains false [true true])", "CCLB contains a Boolean");
         Same ("(contains (at ""q"" 1) [(at ""pq"" 1) (at ""pq"" 2)])", "CCLB contains a character");
         Same ("(type Color (variant (Red) (Green) (Blue))) (contains Color.Green [Color.Red Color.Green])",
               "CCLB contains an enumeration member");
         Same ("(join "", "" [""a"" ""b"" ""c""])", "CCLB join");
         Same ("(join """" (list-of String))", "CCLB join nothing");
         Same ("(range 1 5)", "CCLB range");
         Same ("(range 5 1)", "CCLB an empty range");
         Same ("(range -2 2)", "CCLB a range across zero");
         Same ("(range 1 5000)", "CCLB a range past the region fails");
         Same ("(range -9223372036854775807 9223372036854775807)", "CCLB a range past any span fails");
         Same ("(split "","" ""a,b,,c"")", "CCLB split on a separator");
         Same ("(split """" ""  hello   world "")", "CCLB split into words");
         Same ("(split "","" """")", "CCLB split empty text");
         Same ("(split "", "" ""a, b, c"")", "CCLB split on a longer separator");
         Same ("(split "","" "","")", "CCLB split a lone separator");
         Same ("(sum (range 1 100))", "CCLB sum of a range");
         Same ("(length (split "" "" ""a b c""))", "CCLB length of a split");
         Same ("(join ""-"" (sort (split "","" ""c,a,b"")))", "CCLB split, sort and join");
         Same ("(->> (range 1 10) reverse (first 3))", "CCLB a list pipeline");
         Same ("(let ((xs (range 1 6))) (+ (min xs) (max xs)))", "CCLB built-ins on a local");
         Same ("(sort (range 1 300))", "CCLB fuel per element runs out in both");
         --  The value arena (step 4): records, payload variants, fields,
         --  matches, lists of records, list fields and recursive types.
         declare
            Shapes : constant String :=
              "(type C (record (a Integer) (s String))) " &
              "(type K (variant (A) (B Integer) (Boxed C))) " &
              "(type R (record (xs (List Integer)) (cs (List C)))) " &
              "(type Color (variant (Red) (Green))) " &
              "(type P (record (c Color) (k K) (on Boolean))) ";
            Trees : constant String :=
              "(type Launch (record (name String) (after (List Launch)))) " &
              "(type Tree (variant (Leaf Integer) (Node (List Tree)))) ";
         begin
            Same (Shapes & "(C 1 ""x"")", "CCLB a record");
            Same (Shapes & "(field (C 7 ""x"") a)", "CCLB a field");
            Same (Shapes & "(field (C 7 ""xyz"") s)", "CCLB a String field");
            Same (Shapes & "(length (field (C 7 ""xyz"") s))", "CCLB a String field is text");
            Same (Shapes & "K.A", "CCLB a payload variant's unit member");
            Same (Shapes & "(K.B 2)", "CCLB a scalar payload in a payload variant");
            Same (Shapes & "(K.Boxed (C 3 ""z""))", "CCLB a record payload");
            Same (Shapes & "(P Color.Green K.A true)", "CCLB nested members");
            Same (Shapes & "(P Color.Red (K.Boxed (C 1 ""q"")) false)", "CCLB nested records");
            Same (Shapes & "(match (K.Boxed (C 3 ""z"")) ((K.A) 0) ((K.B n) n) ((K.Boxed c) (field c a)))",
                  "CCLB match on a record payload");
            Same (Shapes & "(match (K.B 9) ((K.A) 0) ((K.B n) n) ((K.Boxed c) (field c a)))",
                  "CCLB match on a scalar payload");
            Same (Shapes & "(match K.A ((K.A) 5) ((K.B n) n) ((K.Boxed c) (field c a)))",
                  "CCLB match on a unit member");
            Same (Shapes & "[(C 1 ""x"") (C 2 ""y"")]", "CCLB a list of records");
            Same (Shapes & "[K.A (K.B 2) (K.Boxed (C 3 ""z""))]", "CCLB a list of payload variants");
            Same (Shapes & "(R [1 2] [(C 1 ""x"")])", "CCLB list fields");
            Same (Shapes & "(R (list-of Integer) (list-of C))", "CCLB empty list fields");
            Same (Shapes & "(length (field (R [5 6 7] (list-of C)) xs))", "CCLB a list field's length");
            Same (Shapes & "(field (at [(C 1 ""x"") (C 9 ""y"")] 2) a)", "CCLB a field of a list element");
            Same (Shapes & "(reverse [(C 1 ""x"") (C 2 ""y"")])", "CCLB reverse keeps records");
            Same (Shapes & "(first 1 (skip 1 [(C 1 ""x"") (C 9 ""y"")]))", "CCLB first and skip keep records");
            Same (Shapes & "(let ((c (C 4 ""l""))) (+ (field c a) (length (field c s))))", "CCLB a record in a local");
            Same (Shapes & "(if true (C 1 ""t"") (C 2 ""f""))", "CCLB records in branches");
            Same (Trees & "(let ((a (Launch ""a"" (list-of Launch)))) (Launch ""b"" [a a]))",
                  "CCLB a record holding a list of itself");
            Same (Trees & "(Tree.Node [(Tree.Leaf 1) (Tree.Node [(Tree.Leaf 2)])])", "CCLB a recursive variant");
         end;
         --  Range checks (step 5): fields, payloads, arguments and results.
         declare
            Ranges : constant String :=
              "(type Priority (range 1 10)) " &
              "(type L (record (name String) (pri Priority))) " &
              "(type V (variant (Some Priority) (None))) " &
              "(define (bump (p Priority)) Priority (+ p 1)) ";
         begin
            Same (Ranges & "(L ""a"" (+ 5 5))", "CCLB a computed field inside its range");
            Same (Ranges & "(L ""a"" (+ 5 6))", "CCLB a computed field outside its range fails");
            Same (Ranges & "(+ (field (L ""a"" 7) pri) 100)", "CCLB a range field reads as an Integer");
            Same (Ranges & "(V.Some (+ 1 1))", "CCLB a range payload");
            Same (Ranges & "(V.Some (+ 9 2))", "CCLB a range payload outside fails");
            Same (Ranges & "(bump 9)", "CCLB a range parameter and result");
            Same (Ranges & "(bump 10)", "CCLB a range result outside fails");
            Same (Ranges & "(bump (- 1 1))", "CCLB a range argument outside fails");
         end;
         --  Functions over any value a run holds.
         declare
            Shapes : constant String :=
              "(type C (record (a Integer) (s String))) " &
              "(define (greet (s String)) String (concat ""hi "" s)) " &
              "(define (total (xs (List Integer))) Integer (sum xs)) " &
              "(define (label (c C)) String (field c s)) " &
              "(define (pair (n Integer)) C (C n (to-string n))) ";
         begin
            Same (Shapes & "(greet ""bo"")", "CCLB a String parameter and result");
            Same (Shapes & "(total [1 2 3])", "CCLB a list parameter");
            Same (Shapes & "(label (C 1 ""zed""))", "CCLB a record parameter");
            Same (Shapes & "(pair 7)", "CCLB a record result");
            Same (Shapes & "(field (pair 7) s)", "CCLB a field of a function's record");
         end;
         --  Function values and captures (step 6a).
         declare
            Funs : constant String :=
              "(define (square (n Integer)) Integer (* n n)) " &
              "(define (twice (f (Function (Integer) Integer)) (x Integer)) Integer (f (f x))) " &
              "(type Priority (range 1 10)) " &
              "(define (inc (p Priority)) Priority (+ p 1)) " &
              "(define (lift (f (Function (Priority) Priority)) (p Priority)) Priority (f p)) " &
              "(define (raw (f (Function (Priority) Priority)) (n Integer)) Integer (f n)) ";
         begin
            Same (Funs & "(twice square 3)", "CCLB a named function as a value");
            Same (Funs & "(twice (fn ((n Integer)) (+ n 1)) 5)", "CCLB a lambda as an argument");
            Same (Funs & "(let ((k 3)) (twice (fn ((n Integer)) (* n k)) 2))", "CCLB a lambda with a capture");
            Same (Funs & "(let ((a 2)) (let ((b 7)) (twice (fn ((n Integer)) (+ (* n a) b)) 1)))",
                  "CCLB a lambda with two captures");
            Same (Funs & "(let ((s ""ab"")) (length (concat s s)))", "CCLB a string beside function values");
            Same (Funs & "(let ((f square)) (f 9))", "CCLB a function value in a local");
            Same (Funs & "(lift inc 4)", "CCLB a range checked through a value call");
            Same (Funs & "(raw inc 11)", "CCLB a range argument outside fails through a value");
            Same (Funs & "(lift inc 10)", "CCLB a range result outside fails through a value");
            Same (Funs & "square", "CCLB a function value result");
            --  Higher-order built-ins (step 6b): resumable List_Apply.
            Same (Funs & "(each square [1 2 3])", "CCLB each with a named function");
            Same (Funs & "(each (fn ((n Integer)) (+ n 1)) [1 2 3])", "CCLB each with a lambda");
            Same (Funs & "(let ((k 10)) (each (fn ((n Integer)) (* n k)) (range 1 4)))", "CCLB each with a capture");
            Same (Funs & "(each (fn ((n Integer)) (to-string n)) [7 8])", "CCLB each changes the element type");
            Same (Funs & "(where (fn ((n Integer)) (> n 2)) [1 2 3 4])", "CCLB where");
            Same (Funs & "(where (fn ((n Integer)) false) [1 2])", "CCLB where keeps nothing");
            Same (Funs & "(fold (fn ((t Integer) (n Integer)) (+ t n)) 0 [1 2 3 4])", "CCLB fold");
            Same (Funs & "(fold (fn ((t String) (n Integer)) (concat t (to-string n))) """" [1 2 3])",
                  "CCLB fold with a String accumulator");
            Same (Funs & "(any (fn ((n Integer)) (> n 2)) [1 5 2])", "CCLB any");
            Same (Funs & "(any (fn ((n Integer)) (> n 9)) [1 5 2])", "CCLB any (none)");
            Same (Funs & "(all (fn ((n Integer)) (> n 0)) [1 5 2])", "CCLB all");
            Same (Funs & "(all (fn ((n Integer)) (> n 1)) [1 5 2])", "CCLB all (not all)");
            Same (Funs & "(count (fn ((n Integer)) (> n 1)) [1 5 2])", "CCLB count");
            Same (Funs & "(sort-by (fn ((n Integer)) (- 0 n)) [3 1 2])", "CCLB sort-by an Integer key");
            Same (Funs & "(sort-by (fn ((s String)) (reverse s)) [""ba"" ""ab"" ""ca""])", "CCLB sort-by a String key");
            Same (Funs & "(each square (list-of Integer))", "CCLB each over nothing");
            Same (Funs & "(fold (fn ((t Integer) (n Integer)) (+ t n)) 7 (list-of Integer))", "CCLB fold over nothing");
            Same (Funs & "(each (fn ((n Integer)) (sum (each (fn ((m Integer)) (* m n)) [1 2]))) [1 2 3])",
                  "CCLB nested applies");
            Same (Funs & "(sum (each (fn ((n Integer)) (twice square n)) [1 2]))", "CCLB a call inside an applied function");
            Same (Funs & "(each (fn ((n Integer)) n) (range 1 300))", "CCLB fuel runs out while applying");
            Same (Funs & "(each inc [3 11])", "CCLB a range parameter checked per element");
            Same ("(type C (record (a Integer) (s String))) (each (fn ((n Integer)) (C n ""k"")) (range 1 3))",
                  "CCLB each builds records");
            Same ("(type C (record (a Integer) (s String))) " &
                  "(sort-by (fn ((c C)) (field c a)) [(C 3 ""z"") (C 1 ""x"")])", "CCLB sort-by orders records");
         end;
         --  Both fail at the same bounds: a string over 8 KiB, and a text
         --  result over 1 KiB.
         Same ("(let ((a ""0123456789abcdef0123456789abcdef"")) " &
               "(let ((b (concat a a))) (let ((c (concat b b))) (let ((d (concat c c))) " &
               "(let ((e (concat d d))) (let ((f (concat e e))) (let ((g (concat f f))) " &
               "(let ((h (concat g g))) (let ((i (concat h h))) (length i))))))))))",
               "CCLB 8 KiB of text");
         Same ("(let ((a ""0123456789abcdef0123456789abcdef"")) " &
               "(let ((b (concat a a))) (let ((c (concat b b))) (let ((d (concat c c))) " &
               "(let ((e (concat d d))) (let ((f (concat e e))) (let ((g (concat f f))) " &
               "(let ((h (concat g g))) (let ((i (concat h h))) (length (concat i ""!"")))))))))))",
               "CCLB text past 8 KiB fails as in the interpreter");
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
         Same ("(define (double (n Integer)) Integer (* n 2)) (define (inc (n Integer)) Integer (+ n 1)) " &
               "(->> 5 double inc double)", "CCLB a pipeline of functions");
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
         Check (Format_Status = CCL.Format.Format_Valid,
                "version 8 modules carry the function table");
         declare
            Round_Trip : Program;
            Round_Linkage : CCL.Catalog.Linkage_Table;
            Round_Limits : CCL.Format.Resource_Limits;
         begin
            CCL.Format.Decode (Encoded, Encoded_Length, Round_Trip, Round_Linkage, Round_Limits,
                               Format_Status, Encode_Validation);
            Check (Format_Status = CCL.Format.Format_Valid and then
                   Round_Trip.Functions_Length = Compiled.Program.Functions_Length and then
                   Round_Trip.Functions = Compiled.Program.Functions and then
                   Round_Trip.Code = Compiled.Program.Code,
                   "functions round-trip through a version 8 module");
         end;
      end;


      CCL.Language.Analyze ("(concat ""a"" ""b"")", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check
        (Compiled.Status = CCL.Compiler.Compilation_Succeeded,
         "strings compile to CCLB text values");

      --  Text through a version 8 module, and verifier rules for the pool.
      CCL.Language.Analyze ("(let ((s ""ab"")) (concat s ""cd""))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Format_Validation : Validation_Error;
         Decoded : Validated_Program;
         Limits : CCL.Format.Resource_Limits;
         Tampered : Program := Compiled.Program;
      begin
         CCL.Format.Encode (Compiled.Program, (Fuel => 64, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "encode a module with text constants");
         CCL.Format.Decode (Encoded, Encoded_Length, Decoded, Limits, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "decode a module with text constants");
         if Format_Status = CCL.Format.Format_Valid then
            Execute (Decoded, 64, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Result_Value.Kind = Text_Value and then
                   Outcome.Result_Text_Value.Data (1 .. Outcome.Result_Text_Value.Length) = "abcd",
                   "run text decoded from a module");
         end if;
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Tampered.Length;
            if Tampered.Code (PC).Op = Push_Text then
               Tampered.Code (PC).Immediate := Integer_64 (Tampered.Constants_Length);
               exit;
            end if;
         end loop;
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Constant, "reject a text constant outside the pool");
         Tampered := Compiled.Program;
         Tampered.Constants (0).First := MAX_CONSTANT_BYTES;
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Constant, "reject a pool entry outside the pool text");
      end;

      --  Characters, to-string and built-ins: a module round trip, the exact
      --  index status, and tampered programs the verifier must refuse.
      CCL.Language.Analyze
        ("(type Color (variant (Red) (Green))) " &
         "(concat (to-string Color.Green) (concat (to-string (at ""x7"" 2)) (upper ""ok"")))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status /= CCL.Compiler.Compilation_Succeeded,
             "to-string refuses a character, as the type checker does");
      CCL.Language.Analyze
        ("(type Color (variant (Red) (Green))) " &
         "(if (= (at ""x7"" 2) (at ""77"" 1)) (concat (to-string Color.Green) (upper (to-string 7))) ""no"")",
         Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded, "compile characters and to-string");
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Format_Validation : Validation_Error;
         Decoded : Validated_Program;
         Limits : CCL.Format.Resource_Limits;
         Tampered : Program;

         --  Tampered with the first instruction whose Op is From changed.
         procedure Tamper (From : Op_Code; To : Op_Code; Immediate : Integer_64 := 0;
                           Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type) is
         begin
            Tampered := Compiled.Program;
            for PC in Instruction_Index loop
               exit when Program_Length (PC) >= Tampered.Length;
               if Tampered.Code (PC).Op = From then
                  Tampered.Code (PC).Op := To;
                  Tampered.Code (PC).Immediate := Immediate;
                  Tampered.Code (PC).Data_Type := Data_Type;
                  exit;
               end if;
            end loop;
         end Tamper;
      begin
         CCL.Format.Encode (Compiled.Program, (Fuel => 64, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "encode characters and to-string");
         CCL.Format.Decode (Encoded, Encoded_Length, Decoded, Limits, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "decode characters and to-string");
         if Format_Status = CCL.Format.Format_Valid then
            Execute (Decoded, 64, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Result_Value.Kind = Text_Value and then
                   Outcome.Result_Text_Value.Data (1 .. Outcome.Result_Text_Value.Length) = "Green7",
                   "run characters and to-string decoded from a module");
         end if;
         Tamper (Text_Builtin, Text_Builtin, Immediate => 99);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Builtin, "reject a built-in naming no operation");
         Tamper (Variant_To_Text, Variant_To_Text, Data_Type => CCL.Types.Integer_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Data_Type, "reject to-string of a type that is no enumeration");
         Tamper (Integer_To_Text, Variant_To_Text,
                 Data_Type => Compiled.Program.Code (0).Data_Type);
         Verify (Tampered, Checked, Error);
         Check (Error /= Valid, "reject to-string of an integer as an enumeration");
         Tamper (Equal_Character, Equal_Integer);
         Verify (Tampered, Checked, Error);
         Check (Error = Type_Mismatch, "reject characters compared as integers");
         Tamper (Text_At, Length_Text);
         Verify (Tampered, Checked, Error);
         Check (Error /= Valid, "reject at rewritten to length");
      end;
      --  Lists: a module round trip and tampered programs.
      CCL.Language.Analyze ("(let ((xs [""a"" ""b"" ""c""])) (if (= (length xs) 3) [(at xs 3) (at xs 1)] xs))",
                            Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded, "compile lists");
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Format_Validation : Validation_Error;
         Decoded : Validated_Program;
         Limits : CCL.Format.Resource_Limits;
         Tampered : Program;
         Other_List : CCL.Types.Type_Reference;
         Specialized : CCL.Types.List_Result;

         procedure Tamper (From : Op_Code; Immediate : Integer_64; Data_Type : CCL.Types.Type_Reference) is
         begin
            Tampered := Compiled.Program;
            for PC in Instruction_Index loop
               exit when Program_Length (PC) >= Tampered.Length;
               if Tampered.Code (PC).Op = From then
                  Tampered.Code (PC).Immediate := Immediate;
                  Tampered.Code (PC).Data_Type := Data_Type;
                  exit;
               end if;
            end loop;
         end Tamper;
         List_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      begin
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Compiled.Program.Length;
            if Compiled.Program.Code (PC).Op = New_List then
               List_Type := Compiled.Program.Code (PC).Data_Type; exit;
            end if;
         end loop;
         CCL.Format.Encode (Compiled.Program, (Fuel => 64, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "encode lists");
         CCL.Format.Decode (Encoded, Encoded_Length, Decoded, Limits, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "decode lists");
         if Format_Status = CCL.Format.Format_Valid then
            Execute (Decoded, 64, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Result_Value.Kind = List_Value and then
                   Outcome.List_Total = 2 and then Outcome.List_Text.Data (1 .. Outcome.List_Text.Length) = "ca",
                   "run lists decoded from a module");
         end if;
         --  A list of another element type where this one is expected.
         Tampered := Compiled.Program;
         CCL.Types.Specialize_List (Tampered.Data_Types, CCL.Types.Integer_Type, Other_List, Specialized);
         Check (Specialized in CCL.Types.List_Specialized | CCL.Types.List_Already_Specialized,
                "specialize a second list type");
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Tampered.Length;
            if Tampered.Code (PC).Op = Fill_List then
               Tampered.Code (PC).Data_Type := Other_List; exit;
            end if;
         end loop;
         Verify (Tampered, Checked, Error);
         Check (Error = Type_Mismatch, "reject a text element filled into a list of integers");
         Tamper (List_At, 0, CCL.Types.String_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Data_Type, "reject at on a type that is no list");
         Tamper (New_List, Integer_64 (MAX_LIST_ELEMENTS) + 1, List_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Data_Type, "reject a list longer than the region");
         Tamper (Fill_List, 0, List_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Data_Type, "reject filling position zero");
         Tamper (Length_List, 1, List_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Invalid_Data_Type, "reject a length with an immediate");
         --  Past the reserved length: the verifier cannot know the length,
         --  the machine refuses the write.
         Tamper (Fill_List, 4, List_Type);
         Verify (Tampered, Checked, Error);
         Check (Error = Valid, "verify a fill past the reserved length");
         if Error = Valid then
            Execute (Checked, 64, Outcome);
            Check (Outcome.Status = Invalid_Bytecode, "refuse a fill past the reserved length");
         end if;
      end;
      --  List built-ins: an unknown operation, and one on a list it does not fit.
      CCL.Language.Analyze ("(join "","" (sort [""b"" ""a""]))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded, "compile list built-ins");
      declare
         Tampered : Program := Compiled.Program;
         Sort_At : Natural := Natural'Last;
      begin
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Tampered.Length;
            if Tampered.Code (PC).Op = List_Builtin and then Sort_At = Natural'Last then
               Sort_At := Natural (PC);
            end if;
         end loop;
         Check (Sort_At /= Natural'Last, "find the compiled sort");
         if Sort_At /= Natural'Last then
            Tampered.Code (Instruction_Index (Sort_At)).Immediate := 99;
            Verify (Tampered, Checked, Error);
            Check (Error = Invalid_Builtin, "reject a list built-in naming no operation");
            Tampered := Compiled.Program;
            Tampered.Code (Instruction_Index (Sort_At)).Immediate :=
              Integer_64 (CCL.List_Operations.Operation'Enum_Rep (CCL.List_Operations.Sum_Items));
            Verify (Tampered, Checked, Error);
            Check (Error = Invalid_Builtin, "reject sum over a list of strings");
         end if;
      end;
      --  A text result past 1 KiB: the REPL refuses it; compiled code completes
      --  without carrying the characters (a host exports the full text).
      declare
         Program : constant String :=
           "(let ((a ""0123456789abcdef0123456789abcdef"")) " &
           "(let ((b (concat a a))) (let ((c (concat b b))) (let ((d (concat c c))) " &
           "(let ((e (concat d d))) (let ((f (concat e e))) (concat f ""!"")))))))";
         Interpreted : CCL.Language.Interpretation_Result;
      begin
         CCL.Language.Interpret (Program, 256, Interpreted);
         Check (Interpreted.Status = CCL.Language.Evaluation_Text_Storage_Exhausted,
                "the REPL refuses a text result past 1 KiB");
         CCL.Language.Analyze (Program, Analysis);
         CCL.Compiler.Compile (Analysis, Compiled);
         Verify (Compiled.Program, Checked, Error);
         if Error = Valid then
            Execute (Checked, 256, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Result_Value.Kind = Text_Value and then
                   not Outcome.Has_Result_Text, "compiled code completes without carrying a long text");
         else
            Check (False, "verify a long text result");
         end if;
      end;
      --  A record program through the module format.
      CCL.Language.Analyze
        ("(type Priority (range 1 10)) (type L (record (name String) (pri Priority))) " &
         "(type K (variant (A) (Boxed L))) (K.Boxed (L ""m"" (+ 2 3)))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded, "compile records with a range field");
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Format_Validation : Validation_Error;
         Decoded : Validated_Program;
         Limits : CCL.Format.Resource_Limits;
      begin
         CCL.Format.Encode (Compiled.Program, (Fuel => 64, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "encode records");
         CCL.Format.Decode (Encoded, Encoded_Length, Decoded, Limits, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "decode records");
         if Format_Status = CCL.Format.Format_Valid then
            Execute (Decoded, 64, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Has_Literal and then
                   Outcome.Literal.Data (1 .. Outcome.Literal.Length) = "(K.Boxed (L ""m"" 5))",
                   "run records decoded from a module");
         end if;
      end;
      --  Function values and List_Apply through the module format.
      CCL.Language.Analyze
        ("(let ((k 3)) (fold (fn ((t Integer) (n Integer)) (+ t (* n k))) 0 [1 2 3]))", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded, "compile a closure applied by fold");
      declare
         Encoded : CCL.Format.Byte_Array;
         Encoded_Length : CCL.Format.Module_Length;
         Format_Status : CCL.Format.Format_Error;
         Format_Validation : Validation_Error;
         Decoded : Validated_Program;
         Limits : CCL.Format.Resource_Limits;
      begin
         CCL.Format.Encode (Compiled.Program, (Fuel => 256, Memory => 0, In_Flight => 0),
                            Encoded, Encoded_Length, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "encode a closure");
         CCL.Format.Decode (Encoded, Encoded_Length, Decoded, Limits, Format_Status, Format_Validation);
         Check (Format_Status = CCL.Format.Format_Valid, "decode a closure");
         if Format_Status = CCL.Format.Format_Valid then
            Execute (Decoded, 256, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Result_Value = Integer_Constant (18),
                   "run a closure decoded from a module");
         end if;
      end;
      --  A value without a literal: the REPL refuses to show it, compiled code
      --  completes (a host takes such a value through Export_Result).
      declare
         Program : constant String := "(type W (record (c Character))) (W (at ""x"" 1))";
         Interpreted : CCL.Language.Interpretation_Result;
      begin
         CCL.Language.Interpret (Program, 256, Interpreted);
         Check (Interpreted.Status /= CCL.Language.Succeeded, "the REPL has no literal for a Character field");
         CCL.Language.Analyze (Program, Analysis);
         CCL.Compiler.Compile (Analysis, Compiled);
         Verify (Compiled.Program, Checked, Error);
         Check (Error = Valid, "verify a record with a Character field");
         if Error = Valid then
            Execute (Checked, 256, Outcome);
            Check (Outcome.Status = Completed and then Outcome.Has_Value and then not Outcome.Has_Literal,
                   "compiled code completes without a literal");
         end if;
      end;
      CCL.Language.Analyze ("(at [1 2] 3)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Verify (Compiled.Program, Checked, Error);
      if Error = Valid then
         Execute (Checked, 64, Outcome);
         Check (Outcome.Status = Index_Out_Of_Range, "list at past the end reports the index status");
      else
         Check (False, "verify list at past the end");
      end if;
      CCL.Language.Analyze ("(at ""abc"" 4)", Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Verify (Compiled.Program, Checked, Error);
      if Error = Valid then
         Execute (Checked, 64, Outcome);
         Check (Outcome.Status = Index_Out_Of_Range, "at past the end reports the index status");
      else
         Check (False, "verify at past the end");
      end if;

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
        (Error = Format_Valid and then Length > 0,
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

      --  v8 corruptions: each field is found by its one canonical encoding.
      declare
         Push : constant Unsigned_8 := Unsigned_8 (Op_Code'Enum_Rep (Push_Integer));
         Add : constant Unsigned_8 := Unsigned_8 (Op_Code'Enum_Rep (Add_Integer));
         --  [16, 4096, 1]
         Limit_Bytes : constant Module_Patches.Bytes :=
           [16#83#, 16#10#, 16#19#, 16#10#, 16#00#, 16#01#];
         --  [Push_Integer, 0, 0, 0, 0, -5, 0, 0]
         First_Push : constant Module_Patches.Bytes :=
           [16#88#, Push, 0, 0, 0, 0, 16#24#, 0, 0];
         --  [Add_Integer, 0, 0, 0, 0, 0, 0, 0]
         Addition : constant Module_Patches.Bytes := [16#88#, Add, 0, 0, 0, 0, 0, 0, 0];
         procedure Corrupt
           (From, To : Module_Patches.Bytes; Expected : Format_Error; Name : String;
            Extra : Natural := 0)
         is
            Found : Boolean := True;
            Bad : Byte_Array := Data;
            Bad_Length : Module_Length := Length;
         begin
            if From'Length > 0 then
               Module_Patches.Replace (Bad, Bad_Length, From, To, Found);
            end if;
            if Extra > 0 then
               Bad_Length := Bad_Length + Extra;
            end if;
            Decode (Bad, Bad_Length, Decoded, Decoded_Limits, Error, Validation);
            Check (Found and then Error = Expected, Name);
         end Corrupt;
      begin
         Corrupt ([16#44#, 16#43#, 16#43#, 16#4C#, 16#42#],
                  [16#44#, 16#58#, 16#43#, 16#4C#, 16#42#], Bad_Magic, "reject bad module magic");
         Corrupt ([], [], Malformed_Encoding, "reject trailing data after the module", Extra => 1);
         Corrupt (Limit_Bytes, [16#83#, 16#18#, 16#10#, 16#19#, 16#10#, 16#00#, 16#01#],
                  Malformed_Encoding, "reject a non-shortest head");
         Corrupt ([16#8B#, 16#44#], [16#9F#, 16#44#], Malformed_Encoding,
                  "reject an indefinite-length array");
         Corrupt (First_Push, [16#88#, 16#18#, 16#63#, 0, 0, 0, 0, 16#24#, 0, 0],
                  Invalid_Opcode, "reject invalid serialized opcode");
         Corrupt (First_Push, Addition, Bytecode_Invalid, "verify decoded bytecode before execution");
         Check (Validation = Stack_Underflow, "the verifier finds the stack underflow");
         Corrupt (Limit_Bytes, [16#83#, 16#00#, 16#19#, 16#10#, 16#00#, 16#01#],
                  Invalid_Resource_Limit, "reject zero module fuel");
         Corrupt (Addition, [16#88#, Add, 0, 0, 0, 0, 1, 0, 0],
                  Noncanonical_Instruction, "reject noncanonical serialized instruction");
      end;

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
         declare
            Authority : constant Unsigned_8 :=
              Unsigned_8 (Authority_Class'Enum_Rep (Observe_Authority));
            Transfer : constant Unsigned_8 :=
              Unsigned_8 (CCL.Imports.Transfer_Mode'Enum_Rep (Candidate.Imports (0).Transfer));
            --  [argument, result, authority, ownership, local, transfer, ...
            Import_Head : constant Module_Patches.Bytes :=
              [16#93#, 0, 0, Authority, 0, 0, Transfer];
            Found : Boolean;
            Zero_Digest : constant Module_Patches.Bytes (1 .. 34) :=
              [1 => 16#58#, 2 => 16#20#, others => 0];
         begin
            Data_2 := Data;
            Length_2 := Length;
            Module_Patches.Replace
              (Data_2, Length_2, Import_Head,
               [16#93#, 0, 0, Authority, 0, 0, 16#18#, 16#63#], Found);
            Decode
              (Data_2, Length_2, Decoded_Candidate, Decoded_Linkage,
               Decoded_Limits, Error, Validation);
            Check
              (Found and then Error = Invalid_Transfer_Mode,
               "reject invalid portable import transfer mode");

            Data_2 := Data;
            Length_2 := Length;
            Module_Patches.Replace
              (Data_2, Length_2, Module_Patches.Encoded_Digest (TEST_INTERFACE_DIGEST),
               Zero_Digest, Found);
            Decode
              (Data_2, Length_2, Decoded_Candidate, Decoded_Linkage,
               Decoded_Limits, Error, Validation);
            Check
              (Found and then Error = Invalid_Linkage,
               "reject portable import without descriptor identity");
         end;

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

      declare
         Consume_Code : constant Unsigned_8 :=
           Unsigned_8 (CCL.Ownership.Disposition_Effect'Enum_Rep (Consume));
         Found : Boolean;
      begin
         --  Type 2's one disposition [SEND, Consume, 0], then a duplicate.
         Data_2 := Data;
         Length_2 := Length;
         Module_Patches.Replace
           (Data_2, Length_2, [16#81#, 16#83#, SEND, Consume_Code, 0],
            [16#82#, 16#83#, SEND, Consume_Code, 0, 16#83#, SEND, Consume_Code, 0], Found);
         Decode
           (Data_2, Length_2, Decoded, Decoded_Limits, Error, Validation);
         Check
           (Found and then Error = Invalid_Ownership_Metadata,
            "reject duplicate serialized disposition verb");
      end;

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
         "v8 preserves owned import and portable linkage metadata");
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
