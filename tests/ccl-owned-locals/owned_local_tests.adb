with Ada.Text_IO;
with Interfaces;
with CCL.VM;
with CCL.Ownership;
with CCL.Imports;
with CCL.Format;

procedure Owned_Local_Tests is
   use CCL.VM;
   use CCL.Ownership;
   use type Interfaces.Integer_64;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Format.Format_Error;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Ada.Text_IO.Put_Line ("FAIL: " & Why);
         raise Program_Error;
      end if;
   end Check;
   Candidate : Program;
   Checked : Validated_Program;
   Error : Validation_Error;
   State : Machine_State;
   Values : Local_Value_Array := [others => (others => <>)];
   Accepted : Boolean;
   Outcome : Execution_Result;
   View : Inspection_Snapshot;
   Bytes : CCL.Format.Byte_Array;
   Size : CCL.Format.Module_Length;
   Limits : CCL.Format.Resource_Limits;
   Format_Error : CCL.Format.Format_Error;

   procedure Transfer_Program (Tag : Type_Id; Mode : Ownership_Mode) is
   begin
      Candidate := (others => <>);
      Candidate.Types_Length := Tag + 1;
      Candidate.Types (Tag).Mode := Mode;
      Candidate.Locals_Length := 2;
      Candidate.Dynamic_Locals_Length := 1;
      Candidate.Local_Types (0) := Tag;
      Candidate.Local_Types (1) := Tag;
      Candidate.Length := 4;
      Candidate.Code (0) := (Op => Move_Local, Local => 0, others => <>);
      Candidate.Code (1) := (Op => Initialize_Local, Local => 1, others => <>);
      Candidate.Code (2) := (Op => Move_Local, Local => 1, others => <>);
      Candidate.Code (3) := (Op => Halt, others => <>);
   end Transfer_Program;

   procedure Expect (Wanted : Validation_Error; Why : String) is
   begin
      Verify (Candidate, Checked, Error);
      Check (Error = Wanted, Why & ": " & Error'Image);
   end Expect;
begin
   -- Ownership tags are not integer values or native service handles. Exercise
   -- every tag, including zero, so no sentinel can mint an owned local.
   for Mode in Ownership_Mode loop
      for Tag in Type_Id loop
         Transfer_Program (Tag, Mode);
         Expect (Valid, "move into dynamic local");
         CCL.Format.Encode (Candidate, (Fuel => 8, others => <>), Bytes, Size, Format_Error, Error);
         Check (Format_Error = CCL.Format.Format_Valid and then Error = Valid,
           "encode dynamic ownership transfer");
         CCL.Format.Decode (Bytes, Size, Checked, Limits, Format_Error, Error);
         Check (Format_Error = CCL.Format.Format_Valid and then Error = Valid and then Limits.Fuel = 8,
           "decode and verify dynamic ownership transfer");
         Values (0) := With_Type (Integer_Constant (42), Tag);
         Initialize_With_Locals (Checked, 8, Values, 1, State, Accepted);
         Check (Accepted, "inject original owner");
         Continue_Execution_For (Checked, State, 2, Outcome);
         Inspect (Checked, State, View);
         Check (Outcome.Status = Paused and then View.Stack_Length = 0 and then
           View.Locals (0).Ownership_State = Moved and then
           View.Locals (1).Ownership_State = Available and then
           View.Locals (1).Value.Integer = 42 and then
           View.Locals (1).Value.Type_Tag = Tag, "one owner after initialization");
         Continue_Execution (Checked, State, Outcome);
         Check (Outcome.Status = Completed and then Outcome.Has_Value and then
           Outcome.Result_Value.Integer = 42 and then
           Outcome.Result_Value.Type_Tag = Tag and then
           not Outcome.Result_Value.Copyable, "return transferred ownership");

         Candidate.Code (2) := (Op => Move_Local, Local => 0, others => <>);
         Expect (Invalid_Ownership, "source cannot be used twice");
         Candidate.Code (2) := (Op => Initialize_Local, Local => 1, others => <>);
         Expect (Stack_Underflow, "initialization consumes operand");

         Transfer_Program (Tag, Mode);
         Candidate.Local_Types (1) := (if Tag = Type_Id'Last then 0 else Tag + 1);
         Candidate.Types_Length := CCL.Ownership.MAX_TYPES;
         Candidate.Types (Candidate.Local_Types (1)).Mode := Mode;
         Expect (Invalid_Ownership, "same mode is not same nominal ownership type");

         if Mode /= Unrestricted then
            Transfer_Program (Tag, Mode);
            Candidate.Code (2) := (Op => Copy_Local, Local => 1, others => <>);
            Expect (Invalid_Ownership, "cannot copy dynamically initialized owner");
            Candidate.Code (2) := (Op => Push_Integer, Immediate => 7, others => <>);
            Expect (Invalid_Ownership, "cannot abandon initialized owner");

            -- A forged primitive has tag zero, but is unrestricted. Even tag
            -- zero ownership types cannot acquire it as an owned resource.
            Candidate := (others => <>);
            Candidate.Types_Length := Tag + 1;
            Candidate.Types (Tag).Mode := Mode;
            Candidate.Locals_Length := 1;
            Candidate.Dynamic_Locals_Length := 1;
            Candidate.Local_Types (0) := Tag;
            Candidate.Length := 4;
            Candidate.Code (0) := (Op => Push_Integer, Immediate => 42, others => <>);
            Candidate.Code (1) := (Op => Initialize_Local, Local => 0, others => <>);
            Candidate.Code (2) := (Op => Move_Local, Local => 0, others => <>);
            Candidate.Code (3) := (Op => Halt, others => <>);
            Expect (Invalid_Ownership, "literal cannot mint ownership");
         end if;

         Transfer_Program (Tag, Mode);
         Candidate.Locals_Length := 1;
         Candidate.Dynamic_Locals_Length := 0;
         Candidate.Length := 3;
         Candidate.Code (1) := (Op => Push_Integer, Immediate => 7, others => <>);
         Candidate.Code (2) := (Op => Halt, others => <>);
         Expect (Invalid_Ownership, "halt cannot lose moved value below result");
      end loop;
   end loop;

   -- Stack-preserving operations must preserve nominal ownership tags, not
   -- silently relabel an unrestricted tagged value as type zero.
   Transfer_Program (2, Unrestricted);
   Candidate.Length := 6;
   Candidate.Code (0) := (Op => Copy_Local, Local => 0, others => <>);
   Candidate.Code (1) := (Op => Copy_Stack, Immediate => 0, others => <>);
   Candidate.Code (2) := (Op => Drop_Under_Top, others => <>);
   Candidate.Code (3) := (Op => Initialize_Local, Local => 1, others => <>);
   Candidate.Code (4) := (Op => Copy_Local, Local => 1, others => <>);
   Candidate.Code (5) := (Op => Halt, others => <>);
   Expect (Valid, "copy and drop-under preserve ownership tag");
   Values (0) := With_Type (Integer_Constant (43), 2);
   Initialize_With_Locals (Checked, 12, Values, 1, State, Accepted);
   Continue_Execution (Checked, State, Outcome);
   Check (Accepted and then Outcome.Status = Completed and then
     Outcome.Result_Value.Type_Tag = 2 and then Outcome.Result_Value.Integer = 43,
     "execute tag-preserving stack operations");
   Candidate.Local_Types (1) := 0;
   Expect (Invalid_Ownership, "stack operations cannot erase ownership tag");

   Transfer_Program (2, Must_Handle);
   Candidate.Length := 6;
   Candidate.Code (0) := (Op => Push_Integer, Immediate => 7, others => <>);
   Candidate.Code (1) := (Op => Move_Local, Local => 0, others => <>);
   Candidate.Code (2) := (Op => Drop_Under_Top, others => <>);
   Candidate.Code (3) := (Op => Initialize_Local, Local => 1, others => <>);
   Candidate.Code (4) := (Op => Move_Local, Local => 1, others => <>);
   Candidate.Code (5) := (Op => Halt, others => <>);
   Expect (Valid, "drop-under preserves moved ownership tag");
   Values (0) := With_Type (Integer_Constant (44), 2);
   Initialize_With_Locals (Checked, 12, Values, 1, State, Accepted);
   Continue_Execution (Checked, State, Outcome);
   Check (Accepted and then Outcome.Status = Completed and then
     Outcome.Result_Value.Type_Tag = 2 and then not Outcome.Result_Value.Copyable,
     "execute moved operand through drop-under");
   Candidate.Code (2) := (Op => Copy_Stack, Immediate => 0, others => <>);
   Expect (Invalid_Ownership, "moved operand cannot be duplicated");

   Transfer_Program (2, Must_Handle);
   Candidate.Length := 5;
   Candidate.Code (0) := (Op => Borrow_Local_RO, Local => 0, others => <>);
   Candidate.Code (1) := (Op => Move_Local, Local => 0, others => <>);
   Candidate.Code (2) := (Op => Initialize_Local, Local => 1, others => <>);
   Candidate.Code (3) := (Op => Move_Local, Local => 1, others => <>);
   Candidate.Code (4) := (Op => Halt, others => <>);
   Expect (Invalid_Ownership, "borrowed source cannot move into new owner");
   Candidate.Code (0).Op := Borrow_Local_RW;
   Expect (Invalid_Ownership, "write-borrowed source cannot move into new owner");

   Transfer_Program (2, Must_Handle);
   Candidate.Length := 6;
   Candidate.Code (2) := (Op => Move_Local, Local => 1, others => <>);
   Candidate.Code (3) := (Op => Initialize_Local, Local => 1, others => <>);
   Candidate.Code (4) := (Op => Move_Local, Local => 1, others => <>);
   Candidate.Code (5) := (Op => Halt, others => <>);
   Expect (Invalid_Ownership, "moved destination cannot be redeclared");

   -- Two paths can transfer the same source into one lexical destination.
   -- The destination then has exactly the same owner state at their join.
   Transfer_Program (2, Must_Handle);
   Candidate.Length := 8;
   Candidate.Code (0) := (Op => Push_Boolean, Immediate => 1, others => <>);
   Candidate.Code (1) := (Op => Jump_If_False, Target => 4, others => <>);
   Candidate.Code (2) := (Op => Move_Local, Local => 0, others => <>);
   Candidate.Code (3) := (Op => Jump, Target => 5, others => <>);
   Candidate.Code (4) := (Op => Move_Local, Local => 0, others => <>);
   Candidate.Code (5) := (Op => Initialize_Local, Local => 1, others => <>);
   Candidate.Code (6) := (Op => Move_Local, Local => 1, others => <>);
   Candidate.Code (7) := (Op => Halt, others => <>);
   for Condition in 0 .. 1 loop
      Candidate.Code (0).Immediate := Interfaces.Integer_64 (Condition);
      Expect (Valid, "join identical owned operand types");
      Initialize_With_Locals (Checked, 12, Values, 1, State, Accepted);
      Continue_Execution (Checked, State, Outcome);
      Check (Accepted and then Outcome.Status = Completed and then
        Outcome.Result_Value.Integer = 44, "execute both ownership branches");
   end loop;

   Candidate := (others => <>);
   Candidate.Types_Length := 3;
   Candidate.Locals_Length := 2;
   Candidate.Local_Types (0) := 1;
   Candidate.Local_Types (1) := 2;
   Candidate.Length := 6;
   Candidate.Code (0) := (Op => Push_Boolean, Immediate => 1, others => <>);
   Candidate.Code (1) := (Op => Jump_If_False, Target => 4, others => <>);
   Candidate.Code (2) := (Op => Copy_Local, Local => 0, others => <>);
   Candidate.Code (3) := (Op => Jump, Target => 5, others => <>);
   Candidate.Code (4) := (Op => Copy_Local, Local => 1, others => <>);
   Candidate.Code (5) := (Op => Halt, others => <>);
   Expect (Inconsistent_Stack, "branch join distinguishes nominal ownership tags");

   -- An import that borrows a local still returns an operand. Verify its
   -- result can initialize a local; do not ignore that result in stack checks.
   for Transfer in CCL.Imports.Transfer_Mode loop
      Candidate := (others => <>);
      Candidate.Types_Length := 2;
      Candidate.Types (1).Mode := (if Transfer = CCL.Imports.Copy_Argument then Unrestricted else Must_Handle);
      Candidate.Types (1).Dispositions_Length := 1;
      Candidate.Types (1).Dispositions (0) := (Verb => 1, Effect => Consume, Next_Type => 0);
      Candidate.Locals_Length := 2;
      Candidate.Dynamic_Locals_Length := 1;
      Candidate.Local_Types (0) := 1;
      Candidate.Imports_Length := 1;
      Candidate.Imports (0) := (Ownership_Argument => True, Local => 0,
        Binding => 99, Transfer => Transfer, Success_Verb => 1, Failure_Verb => 1, others => <>);
      Candidate.Length := 5;
      Candidate.Code (0) := (Op => Invoke_Import, Import => 0, others => <>);
      Candidate.Code (1) := (Op => Initialize_Local, Local => 1, others => <>);
      Candidate.Code (2) := (if Transfer = CCL.Imports.Move_Argument then
        (Op => Push_Integer, Immediate => 0, others => <>) else
        (Op => Apply_Local_Disposition, Local => 0, Verb => 1, others => <>));
      Candidate.Code (3) := (Op => Copy_Local, Local => 1, others => <>);
      Candidate.Code (4) := (Op => Halt, others => <>);
      Expect (Valid, "owned-argument import returns an operand");
      Values (0) := With_Type (Integer_Constant (45), 1);
      Initialize_With_Locals (Checked, 12, Values, 1, State, Accepted);
      Continue_Execution (Checked, State, Outcome);
      Check (Accepted and then Outcome.Status = Waiting_For_Host, "suspend owned import");
      Acknowledge_Host_Submission (Checked, State, True);
      Complete_Host_Call (Checked, State, Integer_Constant (46), True);
      Continue_Execution (Checked, State, Outcome);
      Check (Outcome.Status = Completed and then Outcome.Has_Value and then
        Outcome.Result_Value.Integer = 46, "initialize local from borrowed/moved call completion");

      Candidate.Length := 3;
      Candidate.Code (1) := Candidate.Code (2);
      Candidate.Code (2) := (Op => Halt, others => <>);
      -- Fill the stack before calling: the returned value is not optional.
      for I in 0 .. MAX_STACK_DEPTH - 1 loop
         Candidate.Code (Instruction_Index (I)) := (Op => Push_Integer, Immediate => 0, others => <>);
      end loop;
      Candidate.Length := Program_Length (MAX_STACK_DEPTH) + 3;
      Candidate.Code (Instruction_Index (MAX_STACK_DEPTH)) := (Op => Invoke_Import, others => <>);
      Candidate.Code (Instruction_Index (MAX_STACK_DEPTH + 1)) :=
        (Op => Apply_Local_Disposition, Local => 0, Verb => 1, others => <>);
      Candidate.Code (Instruction_Index (MAX_STACK_DEPTH + 2)) := (Op => Halt, others => <>);
      Expect (Stack_Overflow, "owned import completion needs stack capacity");
   end loop;
   Ada.Text_IO.Put_Line ("Owned local transfers: PASS" & Checks'Image & " checks");
end Owned_Local_Tests;
