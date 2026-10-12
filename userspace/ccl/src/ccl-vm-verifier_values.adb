with CCL.VM.Operations;

package body CCL.VM.Verifier_Values with SPARK_Mode => On is
   use type Abstract_Stacks.Operation_Result;

   procedure Step_Scalar
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Ignored : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      case Ins.Op is
         when Push_Integer =>
            Push_Kind (State, Integer_Value, Error);
         when Push_Boolean =>
            Push_Kind (State, Boolean_Value, Error);
         when Add_Integer | Subtract_Integer | Multiply_Integer | Divide_Integer | Modulo_Integer =>
            Pop_Kind (State, Integer_Value, Error);
            Pop_Kind (State, Integer_Value, Error);
            Push_Kind (State, Integer_Value, Error);
         when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character =>
            Pop_Kind (State, Operations.Comparison_Kind (Ins.Op), Error);
            Pop_Kind (State, Operations.Comparison_Kind (Ins.Op), Error);
            Push_Kind (State, Boolean_Value, Error);
         when Not_Boolean =>
            Pop_Kind (State, Boolean_Value, Error);
            Push_Kind (State, Boolean_Value, Error);
         when Drop =>
            if Error = Valid then
               Abstract_Stacks.Pop (State.Values, Ignored, Stack_Result);
               if Stack_Result /= Abstract_Stacks.Stack_Ok then
                  Error := Stack_Underflow;
               elsif not Ignored.Copyable then
                  Error := Invalid_Ownership;
               end if;
            end if;
         when Check_Range =>
            if Error /= Valid then
               null;
            elsif not CCL.Types.Is_Range (Candidate.Data_Types, Ins.Data_Type) or else
              Ins.Immediate /= 0 or else Ins.Alternative /= 0
            then
               Error := Invalid_Data_Type;
            else
               Pop_Kind (State, Integer_Value, Error);
               Push_Kind (State, Integer_Value, Error);
            end if;
         when others => null;
      end case;
   end Step_Scalar;

   procedure Step_Composite
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Ins.Data_Type);
      Abstract_Value : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         return;
      end if;
      case Ins.Op is
         when Project_Field =>
            if D.Form /= CCL.Types.Product or else
              not Node_Type (Types, Ins.Data_Type) or else
              Ins.Immediate not in 1 .. Integer_64 (D.Count) or else Ins.Alternative /= 0
            then Error := Invalid_Data_Type;
            else
               Pop_Kind (State, Object_Value, Error, Ins.Data_Type);
               declare
                  Ref : constant CCL.Types.Type_Reference :=
                    D.Parts (Operations.Component_Operand (Ins.Immediate)).Payload;
               begin
                  Push_Kind (State, Kind_For_Type (Types, Ref), Error, Reference_For_Type (Types, Ref));
               end;
            end if;
         when Make_Node =>
            if not Node_Type (Types, Ins.Data_Type) or else
              Ins.Immediate /= 0 or else
              (if D.Form = CCL.Types.Product then Ins.Alternative /= 0
               else Ins.Alternative not in 1 .. D.Count)
            then Error := Invalid_Data_Type;
            else
               --  Components in source order; the last is on top.
               if D.Form = CCL.Types.Product then
                  for P in reverse 1 .. D.Count loop
                     Pop_Kind (State, Kind_For_Type (Types, D.Parts (P).Payload), Error,
                               Reference_For_Type (Types, D.Parts (P).Payload));
                  end loop;
               elsif D.Parts (Ins.Alternative).Payload /= CCL.Types.Unit_Type then
                  Pop_Kind (State, Kind_For_Type (Types, D.Parts (Ins.Alternative).Payload),
                            Error, Reference_For_Type (Types, D.Parts (Ins.Alternative).Payload));
               end if;
               Push_Kind (State, Object_Value, Error, Ins.Data_Type);
            end if;
         when Make_Variant =>
            if not CCL.Types.Is_Scalar_Sum (Types, Ins.Data_Type) or else
              Ins.Alternative not in 1 .. D.Count
            then Error := Invalid_Data_Type;
            else
               if D.Parts (Ins.Alternative).Payload /= CCL.Types.Unit_Type then
                  Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
                  if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
                     Error := Invalid_Ownership;
                  end if;
                  Pop_Kind (State, (if D.Parts (Ins.Alternative).Payload = CCL.Types.Integer_Type
                                   then Integer_Value else Boolean_Value), Error);
               end if;
               Push_Kind (State, Variant_Value, Error, Ins.Data_Type);
            end if;
         when Equal_Variant =>
            if not CCL.Types.Is_Enumeration (Types, Ins.Data_Type) or else
              Ins.Alternative /= 0
            then Error := Invalid_Data_Type;
            else
               for Operand in 1 .. 2 loop
                  Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
                  if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
                     Error := Invalid_Ownership;
                  end if;
                  Pop_Kind (State, Variant_Value, Error, Ins.Data_Type);
               end loop;
               Push_Kind (State, Boolean_Value, Error);
            end if;
         when others => null;
      end case;
   end Step_Composite;

   procedure Step_Stack
     (Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Abstract_Value, Discarded : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         return;
      end if;
      case Ins.Op is
         when Copy_Stack =>
            if Ins.Immediate not in 0 .. MAX_STACK_DEPTH - 1 then Error := Stack_Underflow;
            else
               Abstract_Stacks.Peek_At (State.Values, Unsigned_32 (Ins.Immediate), Abstract_Value, Stack_Result);
               if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
               elsif not Abstract_Value.Copyable then Error := Invalid_Ownership;
               else Push_Kind (State, Abstract_Value.Kind, Error, Abstract_Value.Data_Type,
                               Abstract_Value.Copyable, Abstract_Value.Type_Tag);
               end if;
            end if;
         when Drop_Under_Top =>
            Abstract_Stacks.Pop (State.Values, Abstract_Value, Stack_Result);
            if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
            else
               Abstract_Stacks.Pop (State.Values, Discarded, Stack_Result);
               if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
               elsif not Discarded.Copyable then Error := Invalid_Ownership;
               else Push_Kind (State, Abstract_Value.Kind, Error, Abstract_Value.Data_Type,
                               Abstract_Value.Copyable, Abstract_Value.Type_Tag);
               end if;
            end if;
         when others => null;
      end case;
   end Step_Stack;

   procedure Step_Switch
     (Candidate : Program; PC : Instruction_Index; Ins : Instruction; Limit : Program_Length;
      State : in out Abstract_State; States : in out State_Table; Error : in out Validation_Error;
      Switch : out CCL.Ownership.Bytecode.Instruction)
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
      Abstract_Value : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
      Branch : Abstract_State;
   begin
      Switch := (Op => CCL.Ownership.Bytecode.Switch, others => <>);
      if Error /= Valid then
         return;
      end if;
      if Ins.Immediate < 0 or else Ins.Immediate >= Integer_64 (Candidate.Matches_Length) then
         Error := Invalid_Match;
         return;
      end if;
      declare
         M : constant Match_Table := Candidate.Matches (Operations.Match_Operand (Ins.Immediate));
         D : constant CCL.Types.Description := CCL.Types.Describe (Types, M.Data_Type);
      begin
         -- Capture ownership control flow while the table index is
         -- structurally in range; the second pass need not recast an
         -- unchecked signed instruction operand.
         Switch :=
           (Op => CCL.Ownership.Bytecode.Switch,
            Target_Count => D.Count,
            Targets => [for A in CCL.Types.Component_Index => CCL.Ownership.Bytecode.Code_Index (M.Targets (A))],
            others => <>);
         Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
         if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
            Error := Invalid_Ownership;
         end if;
         Pop_Kind (State, Kind_For_Type (Types, M.Data_Type), Error, M.Data_Type);
         for A in 1 .. D.Count loop
            if M.Targets (A) <= PC then Error := Backward_Jump;
            elsif Program_Length (M.Targets (A)) >= Limit then Error := Invalid_Jump_Target;
            else
               Branch := State;
               if D.Parts (A).Payload /= CCL.Types.Unit_Type then
                  Push_Kind (Branch, Kind_For_Type (Types, D.Parts (A).Payload),
                    Error, Reference_For_Type (Types, D.Parts (A).Payload));
               end if;
               Merge_State (States, M.Targets (A), Branch, Error);
            end if;
         end loop;
      end;
   end Step_Switch;
end CCL.VM.Verifier_Values;
