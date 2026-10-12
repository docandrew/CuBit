with CCL.VM.Operations;

package body CCL.VM.Data_Ops with SPARK_Mode => On is
   use type Runtime_Stacks.Operation_Result;

   procedure Run_Data (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
      Good : Boolean := True;
      Next_PC : Instruction_Index := Regs.PC + 1;
      Alternative : CCL.Types.Component_Count;
      Native_Value, Left_Value, Right_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result := Runtime_Stacks.Stack_Ok;
      function Matches (V : Value; Ref : CCL.Types.Type_Reference) return Boolean is
        (V.Kind = Kind_For_Type (Item.Content.Data_Types, Ref) and then
         V.Data_Type = Reference_For_Type (Item.Content.Data_Types, Ref) and then
         V.Copyable and then V.Type_Tag = 0 and then Well_Typed (Item.Content.Data_Types, V));
   begin
      case Ins.Op is
         when Project_Field =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            Good := Stack_Result = Runtime_Stacks.Stack_Ok and then
              Matches (Right_Value, Ins.Data_Type) and then D.Form = CCL.Types.Product and then
              Ins.Immediate in 1 .. Integer_64 (D.Count);
            if Good then
               Component (State.Arena, Item.Content.Data_Types, Right_Value,
                          Operations.Component_Operand (Ins.Immediate), Native_Value, Good);
               if Good then
                  Good := Matches (Native_Value, D.Parts (Operations.Component_Operand (Ins.Immediate)).Payload);
               end if;
               if Good then
                  Runtime_Stacks.Push (Regs.Stack, Native_Value, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok;
               end if;
            end if;
         when Make_Variant =>
            Right_Value := Integer_Constant (0);
            if Ins.Alternative not in 1 .. D.Count then Good := False;
            elsif D.Parts (Ins.Alternative).Payload /= CCL.Types.Unit_Type then
               Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable and then
                 Right_Value.Kind = (if D.Parts (Ins.Alternative).Payload = CCL.Types.Integer_Type
                                     then Integer_Value else Boolean_Value);
            end if;
            if Good then
               Right_Value := (Kind => Variant_Value, Data_Type => Ins.Data_Type,
                 Alternative => Ins.Alternative, Integer => Right_Value.Integer,
                 Boolean => Right_Value.Boolean, others => <>);
               Runtime_Stacks.Push (Regs.Stack, Right_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok;
            end if;
         when Equal_Variant =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Kind = Variant_Value and then
              Right_Value.Data_Type = Ins.Data_Type and then Right_Value.Copyable;
            if Good then
               Runtime_Stacks.Pop (Regs.Stack, Left_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Left_Value.Kind = Variant_Value and then
                 Left_Value.Data_Type = Ins.Data_Type and then Left_Value.Copyable;
            end if;
            if Good then
               Runtime_Stacks.Push (Regs.Stack, Boolean_Constant
                 (Left_Value.Alternative = Right_Value.Alternative), Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok;
            end if;
         when Copy_Stack =>
            if Ins.Immediate not in 0 .. MAX_STACK_DEPTH - 1 then Good := False;
            else
               Runtime_Stacks.Peek_At (Regs.Stack, Unsigned_32 (Ins.Immediate), Right_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable;
               if Good then
                  Runtime_Stacks.Push (Regs.Stack, Right_Value, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok;
               end if;
            end if;
         when Drop_Under_Top =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            Good := Stack_Result = Runtime_Stacks.Stack_Ok;
            if Good then
               Runtime_Stacks.Pop (Regs.Stack, Left_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Left_Value.Copyable;
            end if;
            if Good then
               Runtime_Stacks.Push (Regs.Stack, Right_Value, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok;
            end if;
         when Switch_Variant =>
            if Ins.Immediate < 0 or else Ins.Immediate >= Integer_64 (Item.Content.Matches_Length) then
               Good := False;
            else
               declare
                  M : constant Match_Table := Item.Content.Matches (Operations.Match_Operand (Ins.Immediate));
                  Schema : constant CCL.Types.Description :=
                    CCL.Types.Describe (Item.Content.Data_Types, M.Data_Type);
               begin
                  Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable and then
                    Right_Value.Kind = Kind_For_Type (Item.Content.Data_Types, M.Data_Type) and then
                    Right_Value.Data_Type = M.Data_Type and then
                    Well_Typed (Item.Content.Data_Types, Right_Value);
                  if Good and then Right_Value.Kind = Object_Value then
                     Alternative := Right_Value.Alternative;
                     Good := Alternative in 1 .. Schema.Count;
                     if Good then
                        Next_PC := M.Targets (Alternative);
                        if Schema.Parts (Alternative).Payload /= CCL.Types.Unit_Type then
                           Component (State.Arena, Item.Content.Data_Types, Right_Value, 1, Native_Value, Good);
                           Good := Good and then Matches (Native_Value, Schema.Parts (Alternative).Payload);
                           if Good then
                              Runtime_Stacks.Push (Regs.Stack, Native_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           end if;
                        end if;
                     end if;
                  elsif Good then
                     Next_PC := M.Targets (Right_Value.Alternative);
                     case Schema.Parts (Right_Value.Alternative).Payload is
                        when CCL.Types.Integer_Type =>
                           Runtime_Stacks.Push (Regs.Stack, Integer_Constant (Right_Value.Integer), Stack_Result);
                        when CCL.Types.Boolean_Type =>
                           Runtime_Stacks.Push (Regs.Stack, Boolean_Constant (Right_Value.Boolean), Stack_Result);
                        when others => null;
                     end case;
                     Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                  end if;
               end;
            end if;
         when others => null;
      end case;
      if not Good or else Next_PC <= Regs.PC or else Program_Length (Next_PC) >= Item.Content.Length then
         Trap (Regs, State, Invalid_Bytecode);
      else
         Regs.PC := Next_PC;
      end if;
   end Run_Data;
end CCL.VM.Data_Ops;
