with CCL.Checked_Arithmetic;
with CCL.VM.Operations;

package body CCL.VM.Scalar_Ops with SPARK_Mode => On is
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Checked_Arithmetic.Arithmetic_Error;

   procedure Run_Arithmetic (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Op : constant Op_Code := Item.Content.Code (Regs.PC).Op;
      Left_Value, Right_Value : Value;
      Popped, Pushed : Runtime_Stacks.Operation_Result;
      Answer : Integer_64;
      Overflowed : Boolean;
      Error : CCL.Checked_Arithmetic.Arithmetic_Error;
   begin
      Runtime_Stacks.Pop (Regs.Stack, Right_Value, Popped);
      if Popped /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value or else
        Program_Length (Regs.PC) + 1 >= Item.Content.Length
      then
         Trap (Regs, State, Invalid_Bytecode);
         return;
      end if;
      Runtime_Stacks.Pop (Regs.Stack, Left_Value, Popped);
      if Popped /= Runtime_Stacks.Stack_Ok or else Left_Value.Kind /= Integer_Value then
         Trap (Regs, State, Invalid_Bytecode);
         return;
      end if;
      case Op is
         when Add_Integer =>
            CCL.Checked_Arithmetic.Add (Left_Value.Integer, Right_Value.Integer, Answer, Overflowed);
            Error := (if Overflowed then CCL.Checked_Arithmetic.Arithmetic_Overflow
                      else CCL.Checked_Arithmetic.Arithmetic_Ok);
         when Subtract_Integer =>
            CCL.Checked_Arithmetic.Subtract (Left_Value.Integer, Right_Value.Integer, Answer, Overflowed);
            Error := (if Overflowed then CCL.Checked_Arithmetic.Arithmetic_Overflow
                      else CCL.Checked_Arithmetic.Arithmetic_Ok);
         when Multiply_Integer =>
            CCL.Checked_Arithmetic.Multiply (Left_Value.Integer, Right_Value.Integer, Answer, Overflowed);
            Error := (if Overflowed then CCL.Checked_Arithmetic.Arithmetic_Overflow
                      else CCL.Checked_Arithmetic.Arithmetic_Ok);
         when Divide_Integer =>
            CCL.Checked_Arithmetic.Divide (Left_Value.Integer, Right_Value.Integer, Answer, Error);
         when Modulo_Integer =>
            CCL.Checked_Arithmetic.Modulo (Left_Value.Integer, Right_Value.Integer, Answer, Error);
         when others =>
            Answer := 0;
            Error := CCL.Checked_Arithmetic.Arithmetic_Overflow;
      end case;
      if Error = CCL.Checked_Arithmetic.Arithmetic_Overflow then
         Trap (Regs, State, Arithmetic_Overflow);
      elsif Error = CCL.Checked_Arithmetic.Division_By_Zero then
         Trap (Regs, State, Division_By_Zero);
      else
         Runtime_Stacks.Push (Regs.Stack, Integer_Constant (Answer), Pushed);
         if Pushed = Runtime_Stacks.Stack_Ok then
            Regs.PC := Regs.PC + 1;
         else
            Trap (Regs, State, Invalid_Bytecode);
         end if;
      end if;
   end Run_Arithmetic;

   procedure Run_Logic (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Op : constant Op_Code := Item.Content.Code (Regs.PC).Op;
      Left_Value, Right_Value : Value;
      Popped, Pushed : Runtime_Stacks.Operation_Result;
   begin
      Runtime_Stacks.Pop (Regs.Stack, Right_Value, Popped);
      if Popped /= Runtime_Stacks.Stack_Ok or else
        Right_Value.Kind /= (if Op = Not_Boolean then Boolean_Value else Operations.Comparison_Kind (Op)) or else
        Program_Length (Regs.PC) + 1 >= Item.Content.Length
      then
         Trap (Regs, State, Invalid_Bytecode);
         return;
      end if;
      if Op = Not_Boolean then
         Runtime_Stacks.Push (Regs.Stack, Boolean_Constant (not Right_Value.Boolean), Pushed);
      else
         Runtime_Stacks.Pop (Regs.Stack, Left_Value, Popped);
         if Popped /= Runtime_Stacks.Stack_Ok or else Left_Value.Kind /= Right_Value.Kind then
            Trap (Regs, State, Invalid_Bytecode);
            return;
         end if;
         Runtime_Stacks.Push
           (Regs.Stack,
            Boolean_Constant
              (case Op is
                  when Less_Integer => Left_Value.Integer < Right_Value.Integer,
                  when Less_Equal_Integer => Left_Value.Integer <= Right_Value.Integer,
                  when Equal_Boolean => Left_Value.Boolean = Right_Value.Boolean,
                  when others => Left_Value.Integer = Right_Value.Integer),
            Pushed);
      end if;
      if Pushed = Runtime_Stacks.Stack_Ok then
         Regs.PC := Regs.PC + 1;
      else
         Trap (Regs, State, Invalid_Bytecode);
      end if;
   end Run_Logic;
end CCL.VM.Scalar_Ops;
