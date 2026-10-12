with CCL.VM.Verifier_Values;
with CCL.VM.Verifier_Texts;
with CCL.VM.Verifier_Flow;

package body CCL.VM.Verifier_Sweep with SPARK_Mode => On is

   procedure Enter (Candidate : Program; States : in out State_Table; Error : in out Validation_Error) is
      Branch : Abstract_State;
   begin
      States (0).Seen := True;
      --  A function's code starts with its parameters on the stack.
      for F in 1 .. Candidate.Functions_Length loop
         pragma Loop_Invariant (if Error'Loop_Entry /= Valid then Error = Error'Loop_Entry);
         Branch := (others => <>);
         Branch.Seen := True;
         for P in 1 .. Candidate.Functions (F - 1).Count loop
            pragma Loop_Invariant (if Error'Loop_Entry /= Valid then Error = Error'Loop_Entry);
            Push_Kind (Branch, Candidate.Functions (F - 1).Kinds (P), Error,
                       Candidate.Functions (F - 1).Data_Types (P));
         end loop;
         States (Candidate.Functions (F - 1).Entry_PC) := Branch;
      end loop;
   end Enter;

   procedure Step
     (Candidate : Program; PC : Instruction_Index;
      States : in out State_Table; Switches : in out Switch_Table;
      Own_Max : in out Region_Demands; Base_Max : in out Call_Demands; Calls : in out Call_Presence;
      Error : in out Validation_Error)
   is
      State : Abstract_State;
      Ins : Instruction;
      Falls_Through : Boolean;
      Region : Region_Index;
      Limit : Program_Length;
   begin
      State := States (PC);
      Ins := Candidate.Code (PC);
      Falls_Through := True;
      Region := Region_Of (Candidate, PC);
      Limit := Region_End (Candidate, Region);
      Own_Max (Region) := Demand'Max (Own_Max (Region), Depth_Of (State));

      --  Function bodies hold data only: no program locals, no ownership
      --  imports, no halting. The main body never returns.
      if Region /= No_Region and then
        (Ins.Op in Halt | Initialize_Local | Copy_Local | Move_Local | Drop_Local |
           Borrow_Local_RO | Return_Local_RO | Borrow_Local_RW | Return_Local_RW |
           Apply_Local_Disposition or else
         (Ins.Op = Invoke_Import and then
          (Ins.Import >= Candidate.Imports_Length or else
           Candidate.Imports (Ins.Import).Ownership_Argument or else
           Candidate.Imports (Ins.Import).Result = Resource_Value)))
      then
         Error := Invalid_Function; return;
      elsif Region = No_Region and then Ins.Op = Return_Function then
         Error := Invalid_Function; return;
      end if;

      if Ins.Op not in Make_Variant | Equal_Variant | Project_Field | Variant_To_Text |
        New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range |
        Make_Closure | Call_Value | List_Apply | Push_Stream | Stream_View and then
        (Ins.Data_Type /= CCL.Types.Invalid_Type or else Ins.Alternative /= 0)
      then Error := Invalid_Data_Type; return; end if;

      case Ins.Op is
         when Push_Integer | Push_Boolean | Add_Integer | Subtract_Integer | Multiply_Integer |
              Divide_Integer | Modulo_Integer | Equal_Integer | Less_Integer | Less_Equal_Integer |
              Equal_Boolean | Equal_Character | Not_Boolean | Drop | Check_Range =>
            Verifier_Values.Step_Scalar (Candidate, Ins, State, Error);
         when Make_Variant | Equal_Variant | Project_Field | Make_Node =>
            Verifier_Values.Step_Composite (Candidate, Ins, State, Error);
         when Copy_Stack | Drop_Under_Top =>
            Verifier_Values.Step_Stack (Ins, State, Error);
         when Switch_Variant =>
            Falls_Through := False;
            Verifier_Values.Step_Switch (Candidate, PC, Ins, Limit, State, States, Error, Switches (PC));
         when Push_Text | Concat_Text | Equal_Text | Length_Text | Text_At | Integer_To_Text |
              Variant_To_Text | Text_Builtin =>
            Verifier_Texts.Step_Text (Candidate, Ins, State, Error);
         when New_List | Fill_List | Length_List | List_At | List_Builtin =>
            Verifier_Texts.Step_List (Candidate, Ins, State, Error);
         when Push_Stream | Stream_View =>
            Verifier_Texts.Step_Stream (Candidate, Ins, State, Error);
         when Make_Closure | Call_Value | List_Apply =>
            Verifier_Flow.Step_Closure (Candidate, Ins, State, Error);
         when Call_Function | Return_Function =>
            Verifier_Flow.Step_Call (Candidate, Ins, Region, State, Error, Calls, Base_Max, Falls_Through);
         when Halt | Jump | Jump_If_False =>
            Verifier_Flow.Step_Jump (PC, Ins, Limit, State, States, Error, Falls_Through);
         when Invoke_Import =>
            Verifier_Flow.Step_Import (Candidate, Ins, State, Error);
         when Initialize_Local | Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO |
              Return_Local_RO | Borrow_Local_RW | Return_Local_RW | Apply_Local_Disposition =>
            Verifier_Flow.Step_Local (Candidate, Ins, State, Error);
      end case;

      if Error = Valid then
         Own_Max (Region) := Demand'Max (Own_Max (Region), Depth_Of (State));
      end if;
      if Error = Valid and then Falls_Through then
         if Program_Length (PC) + 1 >= Limit then
            Error := (if Region = No_Region then Missing_Halt else Invalid_Function);
         else
            Merge_State (States, Instruction_Index'Succ (PC), State, Error);
         end if;
      end if;
   end Step;
end CCL.VM.Verifier_Sweep;
