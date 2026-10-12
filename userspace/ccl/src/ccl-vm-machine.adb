with CCL.VM.Operations;

package body CCL.VM.Machine with SPARK_Mode => On is
   use type List_Regions.Operation_Result;

   procedure Trap (Regs : in out Registers; State : in out Machine_State; Code : Execution_Status) is
   begin
      Regs.Status := Code;
      State.Terminal := True;
      State.Terminal_Status := Code;
      Regs.Done := True;
   end Trap;

   procedure Push_Next
     (Item : Validated_Program; Regs : in out Registers; State : in out Machine_State; Value : CCL.VM.Value)
   is
      Pushed : Runtime_Stacks.Operation_Result;
   begin
      if Program_Length (Regs.PC) + 1 >= Item.Content.Length then
         Trap (Regs, State, Invalid_Bytecode);
         return;
      end if;
      Runtime_Stacks.Push (Regs.Stack, Value, Pushed);
      if Pushed = Runtime_Stacks.Stack_Ok then
         Regs.PC := Regs.PC + 1;
      else
         Trap (Regs, State, Invalid_Bytecode);
      end if;
   end Push_Next;

   procedure Pop_Text (Regs : in out Registers; State : in out Machine_State; Value : out CCL.VM.Value) is
      Popped : Runtime_Stacks.Operation_Result;
   begin
      Runtime_Stacks.Pop (Regs.Stack, Value, Popped);
      if Popped /= Runtime_Stacks.Stack_Ok or else Value.Kind /= Text_Value then
         Trap (Regs, State, Invalid_Bytecode);
      end if;
   end Pop_Text;

   procedure Enter_Call
     (Item : Validated_Program; Regs : in out Registers; State : in out Machine_State;
      Callee : Value; Arguments : Component_Values; Arity : CCL.Types.Component_Count;
      Return_PC : Instruction_Index; Good : out Boolean)
   is
      Captured : List_Element;
      Read : List_Regions.Operation_Result;
      Pushed : Runtime_Stacks.Operation_Result;
   begin
      Good := Callee.Kind = Function_Value and then
        Callee.Integer in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 and then
        State.Frame_Count < Call_Depth'Last;
      if not Good then return; end if;
      declare
         F : constant Function_Index := Operations.Function_Operand (Callee.Integer);
         Decl : constant Function_Declaration := Item.Content.Functions (F);
      begin
         Good := Decl.Captures <= Decl.Count and then
           Component_Of (Decl.Count - Decl.Captures) = Arity and then
           List_Regions.Length (Callee.Items) = Natural (Decl.Captures);
         for C in 1 .. Decl.Captures loop
            exit when not Good;
            List_Regions.Read (State.Lists, Callee.Items, List_Regions.Array_Index (C), Captured, Read);
            Good := Read = List_Regions.Operation_Ok;
            if Good then
               Runtime_Stacks.Push
                 (Regs.Stack, (Kind => Decl.Kinds (C), Data_Type => Decl.Data_Types (C),
                               Integer => Captured.Integer, Boolean => Captured.Boolean,
                               Alternative => (if Captured.Alternative in CCL.Types.Component_Index
                                               then Captured.Alternative else 1),
                               Text => Captured.Text, Node => Captured.Node, others => <>),
                  Pushed);
               Good := Pushed = Runtime_Stacks.Stack_Ok;
            end if;
         end loop;
         for P in 1 .. Arity loop
            exit when not Good;
            Runtime_Stacks.Push (Regs.Stack, Arguments (P), Pushed);
            Good := Pushed = Runtime_Stacks.Stack_Ok;
         end loop;
         if Good then
            State.Frames (State.Frame_Count) := (Return_PC => Return_PC, Callee => F);
            State.Frame_Count := State.Frame_Count + 1;
            Regs.PC := Decl.Entry_PC;
         end if;
      end;
   end Enter_Call;
end CCL.VM.Machine;
