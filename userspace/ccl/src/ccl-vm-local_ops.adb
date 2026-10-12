package body CCL.VM.Local_Ops with SPARK_Mode => On is
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Ownership.Ownership_Error;
   use type CCL.Ownership.Ownership_Mode;

   procedure Run_Local (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      Right_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result;
      Own_Error : CCL.Ownership.Ownership_Error;
   begin
      case Ins.Op is
      when Initialize_Local =>
         Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
         if Stack_Result /= Runtime_Stacks.Stack_Ok or else
           not Declared_Local (Ins.Local, Item.Content.Locals_Length) or else
           Right_Value.Kind /= Item.Content.Local_Kinds
             (Ins.Local) or else
           Right_Value.Type_Tag /= Item.Content.Local_Types
             (Ins.Local) or else
           Right_Value.Data_Type /= Item.Content.Local_Data_Types
             (Ins.Local) or else
           (Right_Value.Copyable and then Item.Content.Types
             (Item.Content.Local_Types (Ins.Local)).Mode /= CCL.Ownership.Unrestricted) or else
           Program_Length (Regs.PC) + 1 >= Item.Content.Length
         then
            Trap (Regs, State, Invalid_Bytecode);
         else
            CCL.Ownership.Declare_Binding
              (State.Ownership, Ins.Local,
               Item.Content.Local_Types (Ins.Local),
               Own_Error);
            if Own_Error /= CCL.Ownership.Ownership_Valid then
               Trap (Regs, State, Invalid_Bytecode);
            else
               State.Locals (Ins.Local) := Right_Value;
               Regs.PC := Regs.PC + 1;
            end if;
         end if;

      when Copy_Local | Move_Local | Drop_Local |
           Borrow_Local_RO | Return_Local_RO |
           Borrow_Local_RW | Return_Local_RW |
           Apply_Local_Disposition =>
         if Program_Length (Regs.PC) + 1 >= Item.Content.Length then
            Trap (Regs, State, Invalid_Bytecode);
         else
            case Ins.Op is
               when Copy_Local =>
                  CCL.Ownership.Copy_Value
                    (State.Ownership, Item.Content.Types,
                     Ins.Local,
                     Own_Error);
               when Move_Local =>
                  CCL.Ownership.Move_Value
                    (State.Ownership,
                     Ins.Local,
                     Own_Error);
               when Drop_Local =>
                  CCL.Ownership.Drop_Value
                    (State.Ownership, Item.Content.Types,
                     Ins.Local,
                     Own_Error);
               when Borrow_Local_RO =>
                  CCL.Ownership.Borrow_RO
                    (State.Ownership,
                     Ins.Local,
                     Own_Error);
               when Return_Local_RO =>
                  CCL.Ownership.Return_RO
                    (State.Ownership,
                     Ins.Local,
                     Own_Error);
               when Borrow_Local_RW =>
                  CCL.Ownership.Borrow_RW
                    (State.Ownership,
                     Ins.Local,
                     Own_Error);
               when Return_Local_RW =>
                  CCL.Ownership.Return_RW
                    (State.Ownership,
                     Ins.Local,
                     Own_Error);
               when Apply_Local_Disposition =>
                  CCL.Ownership.Apply_Disposition
                    (State.Ownership, Item.Content.Types,
                     Ins.Local,
                     Ins.Verb,
                     Own_Error);
               when others =>
                  Own_Error := CCL.Ownership.Ownership_Valid;
            end case;
            if Own_Error /= CCL.Ownership.Ownership_Valid then
               Trap (Regs, State, Invalid_Bytecode);
            else
               if Ins.Op in Copy_Local | Move_Local then
                  Right_Value := State.Locals (Ins.Local);
                  Right_Value.Copyable := Ins.Op = Copy_Local;
                  Runtime_Stacks.Push
                    (Regs.Stack,
                     Right_Value,
                     Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok then
                     Trap (Regs, State, Invalid_Bytecode);
                  else
                     Regs.PC := Regs.PC + 1;
                  end if;
               else
                  Regs.PC := Regs.PC + 1;
               end if;
            end if;
         end if;
         when others => null;
      end case;
   end Run_Local;
end CCL.VM.Local_Ops;
