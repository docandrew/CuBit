with CCL.VM.Operations;
with CCL.VM.Text_Ops;

package body CCL.VM.Call_Ops with SPARK_Mode => On is
   package L renames CCL.List_Operations;
   use type L.Apply_Operation;
   use type Runtime_Stacks.Operation_Result;
   use type List_Regions.Operation_Result;
   use type CCL.Execution_Budgets.Consume_Result;
   use type CCL.Imports.Import_Phase;

   --  sort-by's last step: the elements in It.Built ordered by their keys
   --  (Integer or String), one fuel per comparison as in sort: counted
   --  against the fuel left, charged after the sort. Exhausted when it ran
   --  out. Only the regions and budget it names change.
   procedure Sort_By_Keys
     (Lists : in out List_Regions.Stack; Texts : Text_Regions.Stack;
      Budget : in out CCL.Execution_Budgets.Budget; Types : CCL.Types.Registry;
      It : in out Iteration; Good : out Boolean; Exhausted : out Boolean)
     with Global => null,
          Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, It.Callee.Data_Type);
      Text_Keys : constant Boolean := D.Count >= 1 and then D.Parts (D.Count).Payload = CCL.Types.String_Type;
      Allowance : constant Natural :=
        Natural (Unsigned_32'Min (CCL.Execution_Budgets.Remaining (Budget), Unsigned_32 (Natural'Last)));
      Compared : Natural := 0;
      procedure Get (List : List_Regions.Array_Value; I : Positive; E : out List_Element; Ok : in out Boolean) is
         Read : List_Regions.Operation_Result;
      begin
         E := Null_List_Element;
         if Ok and then I <= List_Regions.Array_Index'Last then
            List_Regions.Read (Lists, List, List_Regions.Array_Index (I), E, Read);
            Ok := Read = List_Regions.Operation_Ok;
         else
            Ok := False;
         end if;
      end Get;
      procedure Put (List : List_Regions.Array_Value; I : Positive; E : List_Element; Ok : in out Boolean) is
         Written : List_Regions.Operation_Result;
      begin
         if Ok and then I <= List_Regions.Array_Index'Last then
            List_Regions.Write (Lists, List, List_Regions.Array_Index (I), E, Written);
            Ok := Written = List_Regions.Operation_Ok;
         else
            Ok := False;
         end if;
      end Put;
      procedure Less (I, J : Positive; Before : out Boolean; Ok : out Boolean) is
         A, B : List_Element;
      begin
         Before := False;
         Ok := Compared < Allowance;
         if not Ok then
            Exhausted := True;
            return;
         end if;
         Compared := Compared + 1;
         Get (It.Keys, I, A, Ok);
         Get (It.Keys, J, B, Ok);
         Before := Ok and then (if Text_Keys then Text_Ops.Text_Less (Texts, A.Text, B.Text) else A.Integer < B.Integer);
      end Less;
      procedure Swap (I, J : Positive; Ok : out Boolean) is
         A, B, X, Y : List_Element;
      begin
         Ok := True;
         Get (It.Keys, I, A, Ok); Get (It.Keys, J, B, Ok);
         Put (It.Keys, I, B, Ok); Put (It.Keys, J, A, Ok);
         Get (It.Built.Items, I, X, Ok); Get (It.Built.Items, J, Y, Ok);
         Put (It.Built.Items, I, Y, Ok); Put (It.Built.Items, J, X, Ok);
      end Swap;
      procedure Sort is new L.Heap_Sort (Less, Swap);
   begin
      Exhausted := False;
      Sort (List_Regions.Length (It.Built.Items), Good);
      for C in 1 .. Compared loop
         pragma Loop_Invariant
           (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
         declare
            Spent : CCL.Execution_Budgets.Consume_Result;
         begin
            CCL.Execution_Budgets.Consume (Budget, Spent);
            exit when Spent /= CCL.Execution_Budgets.Consumed;
         end;
      end loop;
      Good := Good and then not Exhausted;
   end Sort_By_Keys;

   --  List_Apply at PC: start an iteration, or resume it with the result
   --  of its last call; then call for the next element or finish.
   procedure Run_Apply (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs),
          Post => Same_Frame (State'Old, State);
   procedure Run_Apply (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      Types : constant CCL.Types.Registry := Item.Content.Data_Types;
      Good : Boolean := True;
      Answer : Value;
      Element : List_Element;
      Read : List_Regions.Operation_Result;
      Written : List_Regions.Operation_Result;
      Operation : L.Apply_Operation;
      Known : Boolean;
      Decided : Boolean := False;
      Stack_Result : Runtime_Stacks.Operation_Result;
   begin
      if State.Iteration_Depth > 0 and then
        State.Iterations (State.Iteration_Depth).Active and then
        State.Iterations (State.Iteration_Depth).In_Call and then
        State.Iterations (State.Iteration_Depth).At_PC = Regs.PC and then
        State.Iterations (State.Iteration_Depth).Frame_Level = State.Frame_Count
      then
         --  Resume: the function's result for element Position.
         declare
            --  A copy, written back below (an element whose index can
            --  change cannot be renamed).
            It : Iteration := State.Iterations (State.Iteration_Depth);
         begin
            Runtime_Stacks.Pop (Regs.Stack, Answer, Stack_Result);
            Good := Stack_Result = Runtime_Stacks.Stack_Ok and then It.Position >= 1 and then
              It.Position <= Element_Count (It.Subject.Items);
            if Good then
               It.In_Call := False;
               case It.Operation is
                  when L.Each_Items =>
                     List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (It.Position),
                                         To_Element (Answer), Written);
                     Good := Written = List_Regions.Operation_Ok;
                     It.Kept := It.Position;
                  when L.Where_Items =>
                     if Answer.Boolean then
                        List_Regions.Read (State.Lists, It.Subject.Items, List_Regions.Array_Index (It.Position),
                                           Element, Read);
                        Good := Read = List_Regions.Operation_Ok and then It.Kept < MAX_LIST_ELEMENTS;
                        if Good then
                           It.Kept := It.Kept + 1;
                           List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (It.Kept),
                                               Element, Written);
                           Good := Written = List_Regions.Operation_Ok;
                        end if;
                     end if;
                  when L.Fold_Items => It.Accumulator := Answer;
                  when L.Any_Items | L.All_Items =>
                     if Answer.Boolean = (It.Operation = L.Any_Items) then
                        It.Accumulator := Boolean_Constant (Answer.Boolean);
                        Decided := True;
                     end if;
                  when L.Count_Items =>
                     if Answer.Boolean and then It.Accumulator.Integer < Integer_64'Last then
                        It.Accumulator.Integer := It.Accumulator.Integer + 1;
                     end if;
                  when L.Sort_By_Items =>
                     List_Regions.Write (State.Lists, It.Keys, List_Regions.Array_Index (It.Position),
                                         To_Element (Answer), Written);
                     Good := Written = List_Regions.Operation_Ok;
               end case;
            end if;
            State.Iterations (State.Iteration_Depth) := It;
         end;
      else
         --  Start: the list, fold's initial value, the function value.
         Operations.Find_Apply_Operation (Ins.Immediate, Operation, Known);
         Good := Known and then State.Iteration_Depth < MAX_ITERATIONS;
         declare
            Subject, Initial, Callee : Value;
         begin
            if Good then
               Runtime_Stacks.Pop (Regs.Stack, Subject, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Subject.Kind = List_Value and then
                 Subject.Data_Type = Ins.Data_Type;
            end if;
            if Good and then Operation = L.Fold_Items then
               Runtime_Stacks.Pop (Regs.Stack, Initial, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok;
            end if;
            if Good then
               Runtime_Stacks.Pop (Regs.Stack, Callee, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Callee.Kind = Function_Value and then
                 Operations.Apply_Fits (Types, Operation, Ins.Data_Type, Callee.Data_Type);
            end if;
            if Good then
               State.Iteration_Depth := State.Iteration_Depth + 1;
               State.Iterations (State.Iteration_Depth) :=
                 (Active => True, In_Call => False, At_PC => Regs.PC, Frame_Level => State.Frame_Count,
                  Operation => Operation, List_Type => Ins.Data_Type, Subject => Subject, Callee => Callee,
                  Accumulator =>
                    (case Operation is
                        when L.Fold_Items => Initial,
                        when L.Count_Items => Integer_Constant (0),
                        when L.All_Items => Boolean_Constant (True),
                        when others => Boolean_Constant (False)),
                  Position => 0, Built => (others => <>), Kept => 0, Keys => <>);
               declare
                  It : Iteration := State.Iterations (State.Iteration_Depth);
                  Length : constant Natural := List_Regions.Length (Subject.Items);
                  D : constant CCL.Types.Description := CCL.Types.Describe (Types, Callee.Data_Type);
               begin
                  if Operation in L.Each_Items | L.Where_Items | L.Sort_By_Items then
                     It.Built := (Kind => List_Value, Data_Type =>
                                    (if Operation = L.Each_Items then Operations.List_Of (Types, D.Parts (D.Count).Payload)
                                     else Ins.Data_Type), others => <>);
                     List_Regions.Reserve (State.Lists, Length, It.Built.Items, Written);
                     Good := Written = List_Regions.Operation_Ok;
                  end if;
                  if Good and then Operation = L.Sort_By_Items then
                     List_Regions.Reserve (State.Lists, Length, It.Keys, Written);
                     Good := Written = List_Regions.Operation_Ok;
                     --  The elements, in order, to be sorted with their keys.
                     for I in 1 .. Length loop
                        pragma Loop_Invariant (Fuel_Limit (State) = Fuel_Limit (State'Loop_Entry));
                        pragma Loop_Invariant
                          (CCL.Imports.Phase (State.Import_Lifecycle) =
                             CCL.Imports.Phase (State.Import_Lifecycle'Loop_Entry));
                        exit when not Good;
                        List_Regions.Read (State.Lists, Subject.Items, List_Regions.Array_Index (I), Element, Read);
                        Good := Read = List_Regions.Operation_Ok;
                        if Good then
                           List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (I),
                                               Element, Written);
                           Good := Written = List_Regions.Operation_Ok;
                        end if;
                     end loop;
                  end if;
                  State.Iterations (State.Iteration_Depth) := It;
                  if not Good then
                     Trap (Regs, State, List_Storage_Exhausted);
                     return;
                  end if;
               end;
            end if;
         end;
      end if;
      if not Good or else State.Iteration_Depth = 0 then
         Trap (Regs, State, Invalid_Bytecode);
         return;
      end if;

      declare
         Depth : constant Iteration_Count := State.Iteration_Depth;
         It : Iteration := State.Iterations (Depth);
         D : constant CCL.Types.Description := CCL.Types.Describe (Types, It.Callee.Data_Type);
         Parameter : constant CCL.Types.Type_Reference :=
           D.Parts (if It.Operation = L.Fold_Items then 2 else 1).Payload;
         Arguments : Component_Values := [others => (others => <>)];
         Argument : Value;
         Result : Value;
      begin
         if not Decided and then It.Position < Element_Count (It.Subject.Items) then
            --  The next element, as the function's argument.
            It.Position := It.Position + 1;
            List_Regions.Read (State.Lists, It.Subject.Items, List_Regions.Array_Index (It.Position), Element, Read);
            From_Element (State.Arena, Types, It.List_Type, Element, Argument, Good);
            Good := Good and then Read = List_Regions.Operation_Ok;
            if not Good then
               Trap (Regs, State, Invalid_Bytecode);
            elsif CCL.Types.Is_Range (Types, Parameter) and then
              (Argument.Integer < CCL.Types.Low_Of (Types, Parameter) or else
               Argument.Integer > CCL.Types.High_Of (Types, Parameter))
            then
               Trap (Regs, State, Range_Error);
            else
               if It.Operation = L.Fold_Items then
                  Arguments (1) := It.Accumulator;
                  Arguments (2) := Argument;
               else
                  Arguments (1) := Argument;
               end if;
               It.In_Call := True;
               State.Iterations (Depth) := It;
               if State.Frame_Count = MAX_FUNCTIONS then
                  Trap (Regs, State, Call_Depth_Exhausted);
               else
                  Enter_Call (Item, Regs, State, It.Callee, Arguments, (if It.Operation = L.Fold_Items then 2 else 1), Regs.PC, Good);
                  if not Good then
                     Trap (Regs, State, Invalid_Bytecode);
                  end if;
               end if;
            end if;
         else
            --  Finished: the result, then past the instruction.
            case It.Operation is
               when L.Each_Items | L.Where_Items =>
                  List_Regions.Shrink (State.Lists, It.Built.Items, Count_Of (It.Kept), Written);
                  Good := Written = List_Regions.Operation_Ok;
                  Result := It.Built;
               when L.Sort_By_Items =>
                  declare
                     Exhausted : Boolean;
                  begin
                     Sort_By_Keys (State.Lists, State.Text, State.Execution_Budget, Types, It, Good, Exhausted);
                     if Exhausted then
                        Trap (Regs, State, Fuel_Exhausted);
                     end if;
                  end;
                  Result := It.Built;
               when others =>
                  Result := It.Accumulator;
            end case;
            State.Iterations (Depth) := (others => <>);
            State.Iteration_Depth := Depth - 1;
            if Good then
               Push_Next (Item, Regs, State, Result);
            elsif not Regs.Done then
               Trap (Regs, State, Invalid_Bytecode);
            end if;
         end if;
      end;
   end Run_Apply;

   procedure Run_Call (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      Stack_Result : Runtime_Stacks.Operation_Result;
      List_Status : List_Regions.Operation_Result;
   begin
      case Ins.Op is
         when Make_Closure =>
            --  The captures, in order, into the list region.
            declare
               Captured : List_Element_Array (1 .. MAX_PARAMETERS) := [others => Null_List_Element];
               Closure, Right_Value : Value;
               Good : Boolean := True;
            begin
               if Ins.Immediate not in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 or else
                 not (CCL.Types.Is_Function (Item.Content.Data_Types, Ins.Data_Type) or else
                      (Ins.Data_Type = CCL.Types.Handler_Type and then
                       Item.Content.Functions (Operations.Function_Operand (Ins.Immediate)).Captures = 0))
               then
                  Trap (Regs, State, Invalid_Bytecode);
               else
                  declare
                     Decl : constant Function_Declaration := Item.Content.Functions (Operations.Function_Operand (Ins.Immediate));
                  begin
                     for P in reverse 1 .. Decl.Captures loop
                        Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                        if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Decl.Kinds (P) or else
                          Right_Value.Data_Type /= Decl.Data_Types (P)
                        then
                           Good := False;
                           exit;
                        end if;
                        Captured (Positive (P)) := To_Element (Right_Value);
                     end loop;
                     Closure := (Kind => Function_Value, Data_Type => Ins.Data_Type,
                                 Integer => Ins.Immediate, others => <>);
                     if not Good then
                        Trap (Regs, State, Invalid_Bytecode);
                     elsif Decl.Captures > 0 then
                        List_Regions.Allocate
                          (State.Lists, Captured (1 .. Positive (Decl.Captures)), Closure.Items, List_Status);
                        if List_Status /= List_Regions.Operation_Ok then
                           Trap (Regs, State, List_Storage_Exhausted);
                        else
                           Push_Next (Item, Regs, State, Closure);
                        end if;
                     else
                        Push_Next (Item, Regs, State, Closure);
                     end if;
                  end;
               end if;
            end;
         when Call_Value =>
            declare
               D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
               Arity : constant CCL.Types.Component_Count := (if D.Count >= 1 then D.Count - 1 else 0);
               Arguments : Component_Values := [others => (others => <>)];
               Callee : Value;
               Good : Boolean := D.Count >= 1;
            begin
               for P in reverse 1 .. Arity loop
                  exit when not Good;
                  Runtime_Stacks.Pop (Regs.Stack, Arguments (P), Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok;
               end loop;
               if Good then
                  Runtime_Stacks.Pop (Regs.Stack, Callee, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Callee.Data_Type = Ins.Data_Type and then
                    Program_Length (Regs.PC) + 1 < Item.Content.Length;
               end if;
               if Good and then State.Frame_Count = Call_Depth'Last then
                  Trap (Regs, State, Call_Depth_Exhausted);
               elsif Good then
                  Enter_Call (Item, Regs, State, Callee, Arguments, Arity, Regs.PC + 1, Good);
                  if not Good then
                     Trap (Regs, State, Invalid_Bytecode);
                  end if;
               else
                  Trap (Regs, State, Invalid_Bytecode);
               end if;
            end;
         when List_Apply =>
            Run_Apply (Item, State, Regs);
         when others => null;
      end case;
   end Run_Call;
end CCL.VM.Call_Ops;
