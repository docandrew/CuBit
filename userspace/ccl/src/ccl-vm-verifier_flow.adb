with CCL.VM.Operations;

package body CCL.VM.Verifier_Flow with SPARK_Mode => On is
   package L renames CCL.List_Operations;
   use type L.Apply_Operation;
   use type Abstract_Stacks.Operation_Result;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Ownership.Ownership_Mode;

   procedure Step_Closure
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
   begin
      if Error /= Valid then
         return;
      end if;
      case Ins.Op is
         when Make_Closure =>
            if Ins.Data_Type = CCL.Types.Handler_Type then
               --  A handler: a function with no captures, as an inert
               --  value only a host import takes; Call_Value refuses it.
               if Ins.Alternative /= 0 or else
                 Ins.Immediate not in 0 .. Integer_64 (Candidate.Functions_Length) - 1 or else
                 Candidate.Functions (Operations.Function_Operand (Ins.Immediate)).Captures /= 0
               then Error := Invalid_Function;
               else
                  Push_Kind (State, Function_Value, Error, CCL.Types.Handler_Type);
               end if;
            elsif not CCL.Types.Is_Function (Types, Ins.Data_Type) or else
              Ins.Alternative /= 0 or else
              Ins.Immediate not in 0 .. Integer_64 (Candidate.Functions_Length) - 1
            then Error := Invalid_Function;
            elsif not Matches_Signature (Candidate, Operations.Function_Operand (Ins.Immediate), Ins.Data_Type) then
               Error := Invalid_Function;
            else
               declare
                  Decl : constant Function_Declaration := Candidate.Functions (Operations.Function_Operand (Ins.Immediate));
               begin
                  for P in reverse 1 .. Decl.Captures loop
                     Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
                  end loop;
                  Push_Kind (State, Function_Value, Error, Ins.Data_Type);
               end;
            end if;
         when Call_Value =>
            if not CCL.Types.Is_Function (Types, Ins.Data_Type) or else
              CCL.Types.Describe (Types, Ins.Data_Type).Count = 0 or else
              Ins.Immediate /= 0 or else Ins.Alternative /= 0
            then Error := Invalid_Function;
            else
               declare
                  D : constant CCL.Types.Description := CCL.Types.Describe (Types, Ins.Data_Type);
               begin
                  --  The arguments, then the function value beneath them.
                  for P in reverse 1 .. D.Count - 1 loop
                     Pop_Kind (State, Kind_For_Type (Types, D.Parts (P).Payload), Error,
                               Reference_For_Type (Types, D.Parts (P).Payload));
                  end loop;
                  Pop_Kind (State, Function_Value, Error, Ins.Data_Type);
                  Push_Kind (State, Kind_For_Type (Types, D.Parts (D.Count).Payload), Error,
                             Reference_For_Type (Types, D.Parts (D.Count).Payload));
               end;
            end if;
         when List_Apply =>
            --  Operands: the function value, fold's initial value, the
            --  list on top. The function's type is read from the stack.
            declare
               Operation : L.Apply_Operation;
               Known : Boolean;
               Fn_Slot : Stack_Type;
               Stack_Result : Abstract_Stacks.Operation_Result := Abstract_Stacks.Stack_Ok;
            begin
               Operations.Find_Apply_Operation (Ins.Immediate, Operation, Known);
               if Known then
                  Abstract_Stacks.Peek_At
                    (State.Values, (if Operation = L.Fold_Items then 2 else 1), Fn_Slot, Stack_Result);
               end if;
               if not Known or else Ins.Alternative /= 0 or else
                 Stack_Result /= Abstract_Stacks.Stack_Ok or else Fn_Slot.Kind /= Function_Value or else
                 not Operations.Apply_Fits (Types, Operation, Ins.Data_Type, Fn_Slot.Data_Type)
               then
                  Error := Invalid_Builtin;
               else
                  declare
                     D : constant CCL.Types.Description := CCL.Types.Describe (Types, Fn_Slot.Data_Type);
                     Result : constant CCL.Types.Type_Reference := D.Parts (D.Count).Payload;
                  begin
                     Pop_Kind (State, List_Value, Error, Ins.Data_Type);
                     if Operation = L.Fold_Items then
                        Pop_Kind (State, Kind_For_Type (Types, Result), Error, Reference_For_Type (Types, Result));
                     end if;
                     Pop_Kind (State, Function_Value, Error, Fn_Slot.Data_Type);
                     case Operation is
                        when L.Each_Items =>
                           Push_Kind (State, List_Value, Error, Operations.List_Of (Types, Result));
                        when L.Where_Items | L.Sort_By_Items =>
                           Push_Kind (State, List_Value, Error, Ins.Data_Type);
                        when L.Any_Items | L.All_Items =>
                           Push_Kind (State, Boolean_Value, Error);
                        when L.Count_Items =>
                           Push_Kind (State, Integer_Value, Error);
                        when L.Fold_Items =>
                           Push_Kind (State, Kind_For_Type (Types, Result), Error, Reference_For_Type (Types, Result));
                     end case;
                  end;
               end if;
            end;
         when others => null;
      end case;
   end Step_Closure;

   procedure Step_Call
     (Candidate : Program; Ins : Instruction; Region : Region_Index;
      State : in out Abstract_State; Error : in out Validation_Error;
      Calls : in out Call_Presence; Base_Max : in out Call_Demands; Falls_Through : in out Boolean)
   is
   begin
      if Error /= Valid then
         return;
      end if;
      if Ins.Op = Call_Function then
         --  Only functions declared earlier: the call graph is acyclic.
         if Ins.Immediate < 0 or else
           Ins.Immediate >= Integer_64 (Candidate.Functions_Length) or else
           Ins.Immediate >= Integer_64 (Region)
         then
            Error := Invalid_Function;
         else
            declare
               Callee : constant Function_Index := Operations.Function_Operand (Ins.Immediate);
               Decl : constant Function_Declaration := Candidate.Functions (Callee);
               Base : constant Demand := Depth_Of (State);
            begin
               if Base < Demand_Of (Decl.Count) then
                  Error := Stack_Underflow;
               else
                  Calls (Region, Callee) := True;
                  Base_Max (Region, Callee) :=
                    Demand'Max (Base_Max (Region, Callee), Base - Demand_Of (Decl.Count));
               end if;
               for P in reverse 1 .. Decl.Count loop
                  Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
               end loop;
               Push_Kind (State, Decl.Result, Error, Decl.Result_Data_Type);
            end;
         end if;
      else
         --  Exactly the parameters and one result remain: frames balance.
         if Region = No_Region then
            Error := Invalid_Function;
         else
            declare
               Decl : constant Function_Declaration := Candidate.Functions (Region);
            begin
               Pop_Kind (State, Decl.Result, Error, Decl.Result_Data_Type);
               for P in reverse 1 .. Decl.Count loop
                  Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
               end loop;
               if Error = Valid and then Abstract_Stacks.Depth (State.Values) /= 0 then
                  Error := Inconsistent_Stack;
               end if;
            end;
         end if;
         Falls_Through := False;
      end if;
   end Step_Call;

   procedure Step_Jump
     (PC : Instruction_Index; Ins : Instruction; Limit : Program_Length;
      State : in out Abstract_State; States : in out State_Table; Error : in out Validation_Error;
      Falls_Through : in out Boolean)
   is
      Abstract_Value : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         return;
      end if;
      case Ins.Op is
         when Halt =>
            -- The top value is returned to the host. No other moved
            -- operand may disappear when the machine completes: checking
            -- only the locals would miss ownership moved onto the stack.
            for Depth in 1 .. MAX_STACK_DEPTH - 1 loop
               Abstract_Stacks.Peek_At (State.Values, Unsigned_32 (Depth), Abstract_Value, Stack_Result);
               exit when Stack_Result /= Abstract_Stacks.Stack_Ok;
               if not Abstract_Value.Copyable then
                  Error := Invalid_Ownership;
                  exit;
               end if;
            end loop;
            Falls_Through := False;
         when Jump | Jump_If_False =>
            if Ins.Op = Jump_If_False then
               Pop_Kind (State, Boolean_Value, Error);
            end if;
            if Error = Valid then
               if Program_Length (Ins.Target) >= Limit then
                  Error := Invalid_Jump_Target;
               elsif Ins.Target <= PC then
                  Error := Backward_Jump;
               else
                  Merge_State (States, Ins.Target, State, Error);
                  if Ins.Op = Jump then
                     Falls_Through := False;
                  end if;
               end if;
            end if;
         when others => null;
      end case;
   end Step_Jump;

   procedure Step_Import
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
   begin
      if Error /= Valid then
         return;
      end if;
      if Ins.Import >= Candidate.Imports_Length then
         Error := Invalid_Import;
         return;
      end if;
      declare
         Op : constant Import_Declaration := Candidate.Imports (Ins.Import);
      begin
         if Op.Ownership_Argument and then
           (not Declared_Local (Op.Local, Candidate.Locals_Length) or else
            Op.Cancellation /= CCL.Imports.Not_Cancellable or else
            Candidate.Local_Kinds (Op.Local) /= Local_Argument_Kind (Op) or else
            Candidate.Local_Data_Types (Op.Local) /= Local_Argument_Type (Op))
         then
            Error := Invalid_Import;
         else
            if not Op.Ownership_Argument or else Has_Receiver (Op) then
               Pop_Kind (State, Op.Argument, Error, Op.Argument_Data_Type);
            end if;
            -- A resource receiver comes from its local; a separate data
            -- argument, when declared, is consumed from the operand stack.
            if Error = Valid then
               Push_Kind (State, Op.Result, Error, Op.Result_Data_Type,
                          Op.Result /= Resource_Value, Op.Result_Type_Tag);
            end if;
         end if;
      end;
   end Step_Import;

   procedure Step_Local
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
   is
      Abstract_Value : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         return;
      end if;
      if not Declared_Local (Ins.Local, Candidate.Locals_Length) then
         Error := Invalid_Ownership;
      elsif Ins.Op = Initialize_Local then
         Abstract_Stacks.Pop (State.Values, Abstract_Value, Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Underflow;
         elsif Abstract_Value.Kind /= Candidate.Local_Kinds (Ins.Local) or else
           Abstract_Value.Data_Type /= Candidate.Local_Data_Types (Ins.Local)
         then
            Error := Type_Mismatch;
         elsif Abstract_Value.Type_Tag /= Candidate.Local_Types (Ins.Local) or else
           (Abstract_Value.Copyable and then
            Candidate.Types (Candidate.Local_Types (Ins.Local)).Mode /= CCL.Ownership.Unrestricted)
         then
            Error := Invalid_Ownership;
         end if;
      elsif Ins.Op in Copy_Local | Move_Local then
         Push_Kind
           (State, Candidate.Local_Kinds (Ins.Local), Error,
            Candidate.Local_Data_Types (Ins.Local),
            Ins.Op = Copy_Local, Candidate.Local_Types (Ins.Local));
      end if;
   end Step_Local;
end CCL.VM.Verifier_Flow;
