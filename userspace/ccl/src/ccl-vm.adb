with CCL.Ownership.Bytecode;
with CCL.Checked_Arithmetic;

package body CCL.VM with
   SPARK_Mode => On
is
   use type CCL.Ownership.Bytecode.Verification_Error;
   use type CCL.Ownership.Ownership_Error;
   use type CCL.Imports.Import_Error;
   use type CCL.Imports.Cancellation_Mode;
   type Abstract_Stack_Index is mod MAX_STACK_DEPTH;
   package Abstract_Stacks is new CCL.Bounded_Stacks
     (Index_Type    => Abstract_Stack_Index,
      Element_Type  => Value_Kind,
      Default_Value => Integer_Value);
   use type Abstract_Stacks.Operation_Result;
   use type Abstract_Stacks.Stack;
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Execution_Budgets.Consume_Result;

   type Abstract_State is record
      Seen  : Boolean := False;
      Values : Abstract_Stacks.Stack;
   end record;

   type State_Table is array (Instruction_Index) of Abstract_State;

   procedure Merge_State
     (States : in out State_Table;
      Target : Instruction_Index;
      Source : Abstract_State;
      Error  : in out Validation_Error)
   is
   begin
      if Error /= Valid then
         null;
      elsif not States (Target).Seen then
         States (Target) := Source;
         States (Target).Seen := True;
      elsif States (Target).Values /= Source.Values then
         Error := Inconsistent_Stack;
      end if;
   end Merge_State;

   procedure Push_Kind
     (State : in out Abstract_State;
      Kind  : Value_Kind;
      Error : in out Validation_Error)
   is
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Push (State.Values, Kind, Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Overflow;
         end if;
      end if;
   end Push_Kind;

   procedure Pop_Kind
     (State    : in out Abstract_State;
      Expected : Value_Kind;
      Error    : in out Validation_Error)
   is
      Actual       : Value_Kind;
      Ignored      : Value_Kind;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Peek_Top
           (State.Values, Actual, Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Underflow;
         elsif Actual /= Expected then
            Error := Type_Mismatch;
         else
            Abstract_Stacks.Pop (State.Values, Ignored, Stack_Result);
            if Stack_Result /= Abstract_Stacks.Stack_Ok then
               Error := Stack_Underflow;
            end if;
         end if;
      end if;
   end Pop_Kind;

   procedure Verify
     (Candidate : Program;
      Result    : out Validated_Program;
      Error     : out Validation_Error)
   is
      States      : State_Table := [others => (others => <>)];
      State       : Abstract_State;
      Instruction : CCL.VM.Instruction;
      Falls_Through : Boolean;
      Ownership_Candidate : CCL.Ownership.Bytecode.Program;
      Ownership_Result : CCL.Ownership.Bytecode.Verification_Result;
      Length : constant Program_Length := Candidate.Length;
   begin
      Result := (Checked => False, Content => Candidate);
      Error := Valid;

      if Length = 0 then
         Error := Empty_Program;
      elsif Candidate.Dynamic_Locals_Length > Candidate.Locals_Length then
         Error := Invalid_Ownership;
      else
         States (0).Seen := True;

      for PC in Instruction_Index loop
         exit when Program_Length (PC) >= Length;
         if Error /= Valid then
            exit;
         elsif not States (PC).Seen then
            Error := Unreachable_Instruction;
            exit;
         end if;

         State := States (PC);
         Instruction := Candidate.Code (PC);
         Falls_Through := True;

         case Instruction.Op is
            when Halt =>
               Falls_Through := False;

            when Push_Integer =>
               Push_Kind (State, Integer_Value, Error);

            when Push_Boolean =>
               Push_Kind (State, Boolean_Value, Error);

            when Add_Integer =>
               Pop_Kind (State, Integer_Value, Error);
               if Error = Valid then
                  Pop_Kind (State, Integer_Value, Error);
               end if;
               if Error = Valid then
                  Push_Kind (State, Integer_Value, Error);
               end if;

            when Equal_Integer =>
               Pop_Kind (State, Integer_Value, Error);
               if Error = Valid then
                  Pop_Kind (State, Integer_Value, Error);
               end if;
               if Error = Valid then
                  Push_Kind (State, Boolean_Value, Error);
               end if;

            when Not_Boolean =>
               Pop_Kind (State, Boolean_Value, Error);
               if Error = Valid then
                  Push_Kind (State, Boolean_Value, Error);
               end if;

            when Drop =>
               declare
                  Ignored : Value_Kind;
                  Stack_Result : Abstract_Stacks.Operation_Result;
               begin
                  Abstract_Stacks.Pop
                    (State.Values, Ignored, Stack_Result);
                  if Stack_Result /= Abstract_Stacks.Stack_Ok then
                     Error := Stack_Underflow;
                  end if;
               end;

            when Jump | Jump_If_False =>
               if Instruction.Op = Jump_If_False then
                  Pop_Kind (State, Boolean_Value, Error);
               end if;

               if Error = Valid then
                  if Program_Length (Instruction.Target) >= Length then
                     Error := Invalid_Jump_Target;
                  elsif Instruction.Target <= PC then
                     Error := Backward_Jump;
                  else
                     Merge_State (States, Instruction.Target, State, Error);
                     if Instruction.Op = Jump then
                        Falls_Through := False;
                     end if;
                  end if;
               end if;

            when Invoke_Import =>
               if Natural (Instruction.Import) >= Candidate.Imports_Length then
                  Error := Invalid_Import;
               elsif Candidate.Imports (Instruction.Import).Ownership_Argument
                 and then
                 (Natural (Candidate.Imports (Instruction.Import).Local) >=
                    Candidate.Locals_Length or else
                  Candidate.Imports (Instruction.Import).Cancellation /=
                    CCL.Imports.Not_Cancellable or else
                  Candidate.Local_Kinds
                    (Candidate.Imports (Instruction.Import).Local) /=
                      Candidate.Imports (Instruction.Import).Argument)
               then
                  Error := Invalid_Import;
               elsif Candidate.Imports (Instruction.Import).Ownership_Argument
               then
                  null;
               else
                  Pop_Kind
                    (State,
                     Candidate.Imports (Instruction.Import).Argument,
                     Error);
                  if Error = Valid then
                     Push_Kind
                       (State,
                        Candidate.Imports (Instruction.Import).Result,
                        Error);
                  end if;
               end if;

            when Initialize_Local =>
               if Natural (Instruction.Local) >= Candidate.Locals_Length then
                  Error := Invalid_Ownership;
               else
                  Pop_Kind
                    (State, Candidate.Local_Kinds (Instruction.Local), Error);
               end if;

            when Copy_Local | Move_Local | Drop_Local |
                 Borrow_Local_RO | Return_Local_RO |
                 Borrow_Local_RW | Return_Local_RW |
                 Apply_Local_Disposition =>
               if Natural (Instruction.Local) >= Candidate.Locals_Length then
                  Error := Invalid_Ownership;
               elsif Instruction.Op in Copy_Local | Move_Local then
                  Push_Kind
                    (State, Candidate.Local_Kinds (Instruction.Local), Error);
               end if;
         end case;

         if Error = Valid and then Falls_Through then
            if Program_Length (PC) + 1 >= Length then
               Error := Missing_Halt;
            else
               Merge_State
                 (States, Instruction_Index'Succ (PC), State, Error);
            end if;
         end if;
      end loop;

      if Error = Valid then
         Ownership_Candidate.Length :=
           CCL.Ownership.Bytecode.Code_Length (Length);
         Ownership_Candidate.Locals_Length := Candidate.Locals_Length;
         Ownership_Candidate.Dynamic_Locals_Length :=
           Candidate.Dynamic_Locals_Length;
         if Candidate.Locals_Length > 0 then
            for Local in 0 .. Candidate.Locals_Length - 1 loop
               Ownership_Candidate.Local_Types (Local) :=
                 Candidate.Local_Types (Local);
            end loop;
         end if;
         Ownership_Candidate.Types := Candidate.Types;
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Length;
            Ownership_Candidate.Code (CCL.Ownership.Bytecode.Code_Index (PC)) :=
              (case Candidate.Code (PC).Op is
                  when Halt => (Op => CCL.Ownership.Bytecode.Halt, others => <>),
                  when Jump =>
                    (Op => CCL.Ownership.Bytecode.Jump,
                     Target => CCL.Ownership.Bytecode.Code_Index
                       (Candidate.Code (PC).Target),
                     others => <>),
                  when Jump_If_False =>
                    (Op => CCL.Ownership.Bytecode.Jump_If,
                     Target => CCL.Ownership.Bytecode.Code_Index
                       (Candidate.Code (PC).Target),
                     others => <>),
                  when Initialize_Local =>
                    (Op => CCL.Ownership.Bytecode.Initialize_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Copy_Local =>
                    (Op => CCL.Ownership.Bytecode.Copy_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Move_Local =>
                    (Op => CCL.Ownership.Bytecode.Move_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Drop_Local =>
                    (Op => CCL.Ownership.Bytecode.Drop_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Borrow_Local_RO =>
                    (Op => CCL.Ownership.Bytecode.Borrow_Local_RO,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Return_Local_RO =>
                    (Op => CCL.Ownership.Bytecode.Return_Local_RO,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Borrow_Local_RW =>
                    (Op => CCL.Ownership.Bytecode.Borrow_Local_RW,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Return_Local_RW =>
                    (Op => CCL.Ownership.Bytecode.Return_Local_RW,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Apply_Local_Disposition =>
                    (Op => CCL.Ownership.Bytecode.Apply_Local_Disposition,
                     Local => Candidate.Code (PC).Local,
                     Verb => Candidate.Code (PC).Verb,
                     others => <>),
                  when Invoke_Import =>
                    (if Candidate.Imports
                       (Candidate.Code (PC).Import).
                         Ownership_Argument
                     then
                       (Op => CCL.Ownership.Bytecode.Import_Local,
                        Local => Candidate.Imports
                          (Candidate.Code (PC).Import).Local,
                        Import_Mode =>
                          (case Candidate.Imports
                             (Candidate.Code (PC).Import).
                               Transfer is
                              when CCL.Imports.Copy_Argument =>
                                CCL.Ownership.Bytecode.Copy_Argument,
                              when CCL.Imports.Move_Argument =>
                                CCL.Ownership.Bytecode.Move_Argument,
                              when CCL.Imports.Borrowed_RO_Argument =>
                                CCL.Ownership.Bytecode.Borrowed_RO_Argument,
                              when CCL.Imports.Borrowed_RW_Argument =>
                                CCL.Ownership.Bytecode.Borrowed_RW_Argument),
                        Success_Verb => Candidate.Imports
                          (Candidate.Code (PC).Import).
                            Success_Verb,
                        Failure_Verb => Candidate.Imports
                          (Candidate.Code (PC).Import).
                            Failure_Verb,
                        others => <>)
                     else
                       (Op => CCL.Ownership.Bytecode.No_Ownership_Op,
                        others => <>)),
                  when others =>
                    (Op => CCL.Ownership.Bytecode.No_Ownership_Op, others => <>));
         end loop;
         CCL.Ownership.Bytecode.Verify
           (Ownership_Candidate, Ownership_Result);
         if Ownership_Result.Error /= CCL.Ownership.Bytecode.Bytecode_Valid then
            Error := Invalid_Ownership;
         else
            Result := (Checked => True, Content => Candidate);
         end if;
      end if;
      end if;
   end Verify;

   procedure Initialize
     (Item  : Validated_Program;
      Fuel  : Natural;
      State : out Machine_State)
   is
      Initial_Locals_Length : constant Local_Count :=
        Item.Content.Locals_Length - Item.Content.Dynamic_Locals_Length;
   begin
      pragma Assert (Is_Valid (Item));
      State := (others => <>);
      CCL.Execution_Budgets.Initialize (State.Execution_Budget, Fuel);
      CCL.Ownership.Initialize (State.Ownership);
      CCL.Imports.Initialize (State.Import_Lifecycle);
      if Initial_Locals_Length > 0 then
         State.Terminal := True;
         State.Terminal_Status := Invalid_Bytecode;
      end if;
   end Initialize;

   procedure Initialize_With_Locals
     (Item     : Validated_Program;
      Fuel     : Natural;
      Values   : Local_Value_Array;
      Count    : Local_Count;
      State    : out Machine_State;
      Accepted : out Boolean)
   is
      Error : CCL.Ownership.Ownership_Error;
      Initial_Locals_Length : constant Local_Count :=
        Item.Content.Locals_Length - Item.Content.Dynamic_Locals_Length;
   begin
      pragma Assert (Is_Valid (Item));
      State := (others => <>);
      CCL.Execution_Budgets.Initialize (State.Execution_Budget, Fuel);
      CCL.Ownership.Initialize (State.Ownership);
      CCL.Imports.Initialize (State.Import_Lifecycle);
      Accepted := Count = Initial_Locals_Length;
      if Accepted and then Count > 0 then
         for Local in 0 .. Count - 1 loop
            if Values (Local).Kind /= Item.Content.Local_Kinds (Local) or else
              Values (Local).Type_Tag /= Item.Content.Local_Types (Local)
            then
               Accepted := False;
               exit;
            end if;
         end loop;
      end if;
      if Accepted then
         State.Locals := Values;
         if Count > 0 then
            for Local in 0 .. Count - 1 loop
               CCL.Ownership.Declare_Binding
                 (State.Ownership, Local, Item.Content.Local_Types (Local), Error);
               if Error /= CCL.Ownership.Ownership_Valid then
                  Accepted := False;
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if not Accepted then
         State.Terminal := True;
         State.Terminal_Status := Invalid_Bytecode;
      end if;
   end Initialize_With_Locals;

   procedure Continue_Execution_For
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Instructions : Natural;
      Result : out Execution_Result)
   is
      Stack : Runtime_Stacks.Stack;
      PC    : Instruction_Index;
      Left  : Integer_64;
      Right : Integer_64;
      Left_Value  : Value;
      Right_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result;
      Waiting : Boolean := State.Waiting;
      Waiting_Owned : Boolean := State.Waiting_Owned;
      Done  : Boolean := State.Terminal or else Waiting;
      Status : Execution_Status := State.Terminal_Status;
      Own_Error : CCL.Ownership.Ownership_Error;
      Import_Error : CCL.Imports.Import_Error;
      Budget_Result : CCL.Execution_Budgets.Consume_Result;
      Addition_Result : Integer_64;
      Addition_Overflowed : Boolean;
      Slice_Remaining : Natural := Instructions;
   begin
      Stack := State.Stack;
      PC := State.PC;
      if Waiting then
         Status := Waiting_For_Host;
      end if;

      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);

      loop
         pragma Loop_Invariant
           (not Waiting or else
            Natural (State.Waiting_Import) < Item.Content.Imports_Length);
         pragma Loop_Invariant (not Waiting or else Done);
         pragma Loop_Invariant
           (not Waiting_Owned or else
            (Waiting and then
             CCL.Imports.Phase (State.Import_Lifecycle) in
               CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted));
         pragma Loop_Invariant
           (Waiting_Owned or else
            CCL.Imports.Phase (State.Import_Lifecycle) not in
              CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);
         pragma Loop_Invariant
           (Fuel_Limit (State) = Fuel_Limit (State'Loop_Entry));
         exit when Done or else
           Slice_Remaining = 0 or else
           not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget);
         CCL.Execution_Budgets.Consume
           (State.Execution_Budget, Budget_Result);
         Slice_Remaining := Slice_Remaining - 1;

         if Budget_Result /= CCL.Execution_Budgets.Consumed or else
           Program_Length (PC) >= Item.Content.Length
         then
            Status := Invalid_Bytecode;
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
            Done := True;
         else
         case Item.Content.Code (PC).Op is
            when Halt =>
               CCL.Ownership.Check_Scope
                 (State.Ownership, Item.Content.Types, Own_Error);
               if Own_Error /= CCL.Ownership.Ownership_Valid then
                  Status := Invalid_Bytecode;
                  State.Terminal_Status := Invalid_Bytecode;
               else
                  Status := Completed;
                  Runtime_Stacks.Peek_Top
                    (Stack, State.Result_Value, Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     State.Has_Value := True;
                  end if;
                  State.Terminal_Status := Completed;
               end if;
               State.Terminal := True;
               Done := True;

            when Push_Integer =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Integer_Constant
                       (Item.Content.Code (PC).Immediate),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Push_Boolean =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Boolean_Constant
                       (Item.Content.Code (PC).Immediate /= 0),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Add_Integer =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Integer_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Left_Value.Kind /= Integer_Value
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     Right := Right_Value.Integer;
                     Left := Left_Value.Integer;
                  CCL.Checked_Arithmetic.Add
                    (Left, Right, Addition_Result, Addition_Overflowed);
                  if Addition_Overflowed then
                     Status := Arithmetic_Overflow;
                     State.Terminal := True;
                     State.Terminal_Status := Arithmetic_Overflow;
                     Done := True;
                  else
                     Runtime_Stacks.Push
                       (Stack, Integer_Constant (Addition_Result), Stack_Result);
                     if Stack_Result = Runtime_Stacks.Stack_Ok then
                        PC := PC + 1;
                     else
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                        Done := True;
                     end if;
                  end if;
                  end if;
               end if;

            when Equal_Integer =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Integer_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Left_Value.Kind /= Integer_Value
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     Runtime_Stacks.Push
                       (Stack,
                        Boolean_Constant
                          (Left_Value.Integer = Right_Value.Integer),
                        Stack_Result);
                     if Stack_Result = Runtime_Stacks.Stack_Ok then
                        PC := PC + 1;
                     else
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                        Done := True;
                     end if;
                  end if;
               end if;

            when Not_Boolean =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Boolean_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Boolean_Constant (not Right_Value.Boolean),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Drop =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  PC := PC + 1;
               end if;

            when Jump =>
               if Item.Content.Code (PC).Target <= PC
                 or else Program_Length
                    (Item.Content.Code (PC).Target) >=
                      Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  PC := Item.Content.Code (PC).Target;
               end if;

            when Jump_If_False =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Boolean_Value or else
                   Item.Content.Code (PC).Target <= PC
                 or else Program_Length
                    (Item.Content.Code (PC).Target) >=
                      Item.Content.Length or else
                  Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  if not Right_Value.Boolean then
                     PC := Item.Content.Code (PC).Target;
                  else
                     PC := PC + 1;
                  end if;
               end if;

            when Invoke_Import =>
               declare
                  Import_Number : constant Import_Index :=
                    Item.Content.Code (PC).Import;
               begin
                  if Natural (Import_Number) >= Item.Content.Imports_Length
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                  elsif Item.Content.Imports (Import_Number).Ownership_Argument
                  then
                     CCL.Imports.Offer
                       (State.Import_Lifecycle,
                        Item.Content.Imports (Import_Number).Local,
                        Item.Content.Imports (Import_Number).Transfer,
                        Item.Content.Imports (Import_Number).Cancellation,
                        Item.Content.Imports (Import_Number).Success_Verb,
                        Item.Content.Imports (Import_Number).Failure_Verb,
                        Item.Content.Imports (Import_Number).Cancel_Verb,
                        Import_Error);
                     if Import_Error /= CCL.Imports.Import_Valid then
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                     else
                        Waiting := True;
                        Waiting_Owned := True;
                        State.Waiting_Import := Import_Number;
                        State.Waiting_Result_Kind :=
                          Item.Content.Imports (Import_Number).Result;
                        State.Waiting_Argument := State.Locals
                          (Item.Content.Imports (Import_Number).Local);
                        Status := Waiting_For_Host;
                     end if;
                  else
                     Runtime_Stacks.Pop
                       (Stack, Right_Value, Stack_Result);
                     if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                       Right_Value.Kind /=
                         Item.Content.Imports (Import_Number).Argument
                     then
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                     else
                        Waiting := True;
                        State.Waiting_Import := Import_Number;
                        State.Waiting_Result_Kind :=
                          Item.Content.Imports (Import_Number).Result;
                        State.Waiting_Argument := Right_Value;
                        Waiting_Owned := False;
                        Status := Waiting_For_Host;
                     end if;
                  end if;
                  Done := True;
               end;

            when Initialize_Local =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Natural (Item.Content.Code (PC).Local) >=
                   Item.Content.Locals_Length or else
                 Right_Value.Kind /= Item.Content.Local_Kinds
                   (Item.Content.Code (PC).Local) or else
                 Right_Value.Type_Tag /= Item.Content.Local_Types
                   (Item.Content.Code (PC).Local) or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  CCL.Ownership.Declare_Binding
                    (State.Ownership, Item.Content.Code (PC).Local,
                     Item.Content.Local_Types (Item.Content.Code (PC).Local),
                     Own_Error);
                  if Own_Error /= CCL.Ownership.Ownership_Valid then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     State.Locals (Item.Content.Code (PC).Local) := Right_Value;
                     PC := PC + 1;
                  end if;
               end if;

            when Copy_Local | Move_Local | Drop_Local |
                 Borrow_Local_RO | Return_Local_RO |
                 Borrow_Local_RW | Return_Local_RW |
                 Apply_Local_Disposition =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  case Item.Content.Code (PC).Op is
                     when Copy_Local =>
                        CCL.Ownership.Copy_Value
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Move_Local =>
                        CCL.Ownership.Move_Value
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Drop_Local =>
                        CCL.Ownership.Drop_Value
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Borrow_Local_RO =>
                        CCL.Ownership.Borrow_RO
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Return_Local_RO =>
                        CCL.Ownership.Return_RO
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Borrow_Local_RW =>
                        CCL.Ownership.Borrow_RW
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Return_Local_RW =>
                        CCL.Ownership.Return_RW
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Apply_Local_Disposition =>
                        CCL.Ownership.Apply_Disposition
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Item.Content.Code (PC).Verb,
                           Own_Error);
                     when others =>
                        Own_Error := CCL.Ownership.Ownership_Valid;
                  end case;
                  if Own_Error /= CCL.Ownership.Ownership_Valid then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     if Item.Content.Code (PC).Op in Copy_Local | Move_Local then
                        Runtime_Stacks.Push
                          (Stack,
                           State.Locals (Item.Content.Code (PC).Local),
                           Stack_Result);
                        if Stack_Result /= Runtime_Stacks.Stack_Ok then
                           Status := Invalid_Bytecode;
                           State.Terminal := True;
                           State.Terminal_Status := Invalid_Bytecode;
                           Done := True;
                        else
                           PC := PC + 1;
                        end if;
                     else
                        PC := PC + 1;
                     end if;
                  end if;
               end if;
         end case;
         end if;
      end loop;

      if not Done and then
        not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget)
      then
         Status := Fuel_Exhausted;
         State.Terminal := True;
         State.Terminal_Status := Fuel_Exhausted;
      elsif not Done then
         Status := Paused;
      end if;

      State.Stack := Stack;
      State.PC := PC;
      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);
      State.Waiting := Waiting;
      State.Waiting_Owned := Waiting_Owned;

      Result :=
        (Status         => Status,
         Has_Value      => State.Has_Value,
         Result_Value   => State.Result_Value,
         Fuel_Remaining =>
           CCL.Execution_Budgets.Remaining (State.Execution_Budget),
         Steps          => CCL.Execution_Budgets.Steps (State.Execution_Budget),
         Requested_Import => State.Waiting_Import,
         Request_Argument => State.Waiting_Argument,
         Requested_Authority =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Authority
            else No_Authority),
         Requested_Binding =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Binding
            else 0));
   end Continue_Execution_For;

   procedure Continue_Execution
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Result : out Execution_Result)
   is
   begin
      Continue_Execution_For
        (Item, State,
         Natural (CCL.Execution_Budgets.Limit (State.Execution_Budget)),
         Result);
   end Continue_Execution;

   function Snapshot (State : Machine_State) return Machine_Snapshot is
     ((Instruction => State.PC,
       Fuel_Remaining =>
         CCL.Execution_Budgets.Remaining (State.Execution_Budget),
       Steps => CCL.Execution_Budgets.Steps (State.Execution_Budget),
       Waiting => State.Waiting,
       Terminal => State.Terminal,
       Status => State.Terminal_Status));

   procedure Inspect
     (Item   : Validated_Program;
      State  : Machine_State;
      Result : out Inspection_Snapshot)
   is
      Stack_Copy : Runtime_Stacks.Stack := State.Stack;
      Stack_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result;
      Local : CCL.Ownership.Binding_Id;
      Type_Tag : CCL.Ownership.Type_Id;
   begin
      Result := (others => <>);
      Result.Machine := Snapshot (State);
      Result.Locals_Length := Item.Content.Locals_Length;
      Result.Waiting_Import := State.Waiting_Import;
      Result.Waiting_Result_Kind := State.Waiting_Result_Kind;
      Result.Waiting_Argument := State.Waiting_Argument;
      Result.Waiting_Owned := State.Waiting_Owned;
      Result.Import_Phase := CCL.Imports.Phase (State.Import_Lifecycle);

      for Position in Stack_Index loop
         Runtime_Stacks.Pop (Stack_Copy, Stack_Value, Stack_Result);
         exit when Stack_Result /= Runtime_Stacks.Stack_Ok;
         Result.Stack (Position) := Stack_Value;
         Result.Stack_Length := Stack_Depth (Natural (Position) + 1);
      end loop;

      if Item.Content.Locals_Length > 0 then
         for Position in 0 .. Item.Content.Locals_Length - 1 loop
            Local := CCL.Ownership.Binding_Id (Position);
            Type_Tag := Item.Content.Local_Types (Local);
            Result.Locals (Local) :=
              (Value => State.Locals (Local),
               Kind => Item.Content.Local_Kinds (Local),
               Type_Tag => Type_Tag,
               Mode => Item.Content.Types (Type_Tag).Mode,
               Ownership_State =>
                 CCL.Ownership.State (State.Ownership, Local),
               Read_Borrows =>
                 CCL.Ownership.Read_Borrows (State.Ownership, Local),
               Write_Borrow =>
                 CCL.Ownership.Has_Write_Borrow (State.Ownership, Local));
         end loop;
      end if;
   end Inspect;

   procedure Stop (State : in out Machine_State) is
   begin
      if not State.Terminal then
         State.Terminal := True;
         State.Terminal_Status := Stopped;
      end if;
   end Stop;

   procedure Complete_Host_Call
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Response : Value;
      Accepted : Boolean)
   is
      Import_Error : CCL.Imports.Import_Error;
      Stack_Result : Runtime_Stacks.Operation_Result;
   begin
      if not State.Waiting or else State.Terminal then
         null;
      elsif not Accepted then
         if State.Waiting_Owned then
            CCL.Imports.Complete
              (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
               CCL.Imports.Import_Failed, Import_Error);
            if Import_Error /= CCL.Imports.Import_Valid then
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
               return;
            end if;
         end if;
         State.Waiting := False;
         State.Waiting_Owned := False;
         State.Terminal := True;
         State.Terminal_Status := Host_Call_Failed;
      else
         if State.Waiting_Owned then
            CCL.Imports.Complete
              (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
               CCL.Imports.Import_Succeeded, Import_Error);
            if Import_Error /= CCL.Imports.Import_Valid then
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
               return;
            end if;
         end if;
         if Response.Kind /= State.Waiting_Result_Kind or else
           Program_Length (State.PC) + 1 >= Item.Content.Length
         then
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
         else
            Runtime_Stacks.Push (State.Stack, Response, Stack_Result);
            if Stack_Result = Runtime_Stacks.Stack_Ok then
               State.PC := State.PC + 1;
            else
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
            end if;
         end if;
         State.Waiting := False;
         State.Waiting_Owned := False;
      end if;
   end Complete_Host_Call;

   procedure Acknowledge_Host_Submission
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Accepted : Boolean)
   is
      Import_Error : CCL.Imports.Import_Error;
   begin
      if not State.Waiting or else not State.Waiting_Owned or else
        State.Terminal
      then
         null;
      elsif Accepted then
         CCL.Imports.Accept_Submission
           (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
            Import_Error);
         if Import_Error /= CCL.Imports.Import_Valid then
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
         end if;
      else
         CCL.Imports.Reject_Submission
           (State.Import_Lifecycle, Import_Error);
         State.Terminal := True;
         if Import_Error = CCL.Imports.Import_Valid then
            State.Waiting := False;
            State.Waiting_Owned := False;
            State.Terminal_Status := Host_Call_Failed;
         else
            State.Terminal_Status := Invalid_Bytecode;
         end if;
      end if;
   end Acknowledge_Host_Submission;

   procedure Execute
     (Item   : Validated_Program;
      Fuel   : Natural;
      Result : out Execution_Result)
   is
      State : Machine_State;
   begin
      Initialize (Item, Fuel, State);
      Continue_Execution (Item, State, Result);
   end Execute;
end CCL.VM;
