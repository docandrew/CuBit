with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.VM; use CCL.VM;
with CCL.Language;
with CCL.Catalog;
with CCL.Compiler;
with CCL.Debug_Maps;
with CCL.Scheduler; use CCL.Scheduler;
with CCL.Format; use CCL.Format;
with CCL.Ownership; use CCL.Ownership;
with CCL.Ownership.Bytecode;
with CCL.Imports;

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
         "lower zero-parameter catalog operation through CCLB v2 sentinel");
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
   begin
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
      Data_2 (0) := 0;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Bad_Magic, "reject bad module magic");

      Data_2 := Data;
      Data_2 (28) := 1;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Bad_Reserved_Field, "reject nonzero module reserved field");

      Data_2 := Data;
      Data_2 (32) := 99;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Invalid_Opcode, "reject invalid serialized opcode");

      Data_2 := Data;
      Data_2 (32) := 3;
      for I in 36 .. 43 loop
         Data_2 (I) := 0;
      end loop;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Bytecode_Invalid and then Validation = Stack_Underflow,
         "verify decoded bytecode before execution");

      Data_2 := Data;
      Data_2 (16) := 0;
      Data_2 (17) := 0;
      Data_2 (18) := 0;
      Data_2 (19) := 0;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check (Error = Invalid_Resource_Limit, "reject zero module fuel");

      Data_2 := Data;
      Data_2 (68) := 1;
      Decode
        (Data_2, Length, Decoded, Decoded_Limits, Error, Validation);
      Check
        (Error = Noncanonical_Instruction,
         "reject noncanonical serialized instruction");

      Candidate := (others => <>);
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) :=
        (Argument => Integer_Value, Result => Integer_Value,
         Authority => Observe_Authority, Binding => 42, others => <>);
      Candidate.Length := 3;
      Candidate.Code (0) := Ins (Push_Integer, 41);
      Candidate.Code (1) := Ins (Invoke_Import, Import => 0);
      Candidate.Code (2) := Ins (Halt);
      Encode (Candidate, Limits, Data, Length, Error, Validation);
      if Error = Format_Valid then
         Decode
           (Data, Length, Decoded, Decoded_Limits, Error, Validation);
      end if;
      if Error = Format_Valid then
         Execute (Decoded, Decoded_Limits.Fuel, Outcome);
      end if;
      Check
        (Error = Format_Valid and then
         Outcome.Status = Waiting_For_Host and then
         Outcome.Requested_Authority = Observe_Authority and then
         Outcome.Requested_Binding = 42,
         "round-trip typed module import");

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
         Authority => Control_Authority, Binding => 77,
         Ownership_Argument => True, Local => 0,
         Transfer => CCL.Imports.Move_Argument,
         Cancellation => CCL.Imports.Not_Cancellable,
         Success_Verb => SEND, Failure_Verb => SEND,
         Cancel_Verb => 0);
      Encode (Candidate, Limits, Data, Length, Error, Validation);
      Check
        (Error = Unsupported_Ownership_Metadata,
         "v2 refuses to erase owned import metadata");
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
         Success_Verb => 1, Failure_Verb => 2, Cancel_Verb => 0);
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
