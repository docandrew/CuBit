package body CCL.VM.Verifier_State with SPARK_Mode => On is
   use type Abstract_Stacks.Operation_Result;

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
      Error : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Copyable : Boolean := True;
      Type_Tag : CCL.Ownership.Type_Id := 0)
   is
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Push (State.Values, (Kind, Data_Type, Copyable, Type_Tag), Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Overflow;
         end if;
      end if;
   end Push_Kind;

   procedure Pop_Kind
     (State    : in out Abstract_State;
      Expected : Value_Kind;
      Error    : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type)
   is
      Actual       : Stack_Type;
      Ignored      : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Peek_Top (State.Values, Actual, Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Underflow;
         elsif Actual.Kind /= Expected or else Actual.Data_Type /= Data_Type then
            Error := Type_Mismatch;
         elsif not Actual.Copyable then
            -- Moved ownership values may be returned, but cannot be laundered
            -- through arithmetic, scalar imports, or a new unrestricted local.
            Error := Invalid_Ownership;
         else
            Abstract_Stacks.Pop (State.Values, Ignored, Stack_Result);
            if Stack_Result /= Abstract_Stacks.Stack_Ok then
               Error := Stack_Underflow;
            end if;
         end if;
      end if;
   end Pop_Kind;

   function Region_Of (Candidate : Program; PC : Instruction_Index) return Region_Index is
      R : Region_Index := No_Region;
   begin
      for F in 1 .. Candidate.Functions_Length loop
         pragma Loop_Invariant (R = No_Region or else R < F);
         if Candidate.Functions (F - 1).Entry_PC <= PC then
            R := F - 1;
         end if;
      end loop;
      return R;
   end Region_Of;

   --  Parameter P (1-based) of a function type, as a position among a
   --  function's parameters after its Captures.
   function Fits_After (Captures : Parameter_Count; P : CCL.Types.Component_Index) return Boolean is
     (Natural (Captures) + P <= MAX_PARAMETERS);
   function Parameter_At (Captures : Parameter_Count; P : CCL.Types.Component_Index) return Parameter_Index is
     (Parameter_Index (Natural (Captures) + P))
     with Pre => Fits_After (Captures, P);

   function Matches_Signature
     (Candidate : Program; F : Function_Index; T : CCL.Types.Type_Reference) return Boolean
   is
      Types : CCL.Types.Registry renames Candidate.Data_Types;
      Fn : Function_Declaration renames Candidate.Functions (F);
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, T);
   begin
      return CCL.Types.Is_Function (Types, T) and then
        D.Count >= 1 and then
        Fn.Captures <= Fn.Count and then
        Component_Of (Fn.Count - Fn.Captures) = D.Count - 1 and then
        (for all P in 1 .. D.Count - 1 =>
           Fits_After (Fn.Captures, P) and then
           Fn.Kinds (Parameter_At (Fn.Captures, P)) = Kind_For_Type (Types, D.Parts (P).Payload) and then
           Fn.Data_Types (Parameter_At (Fn.Captures, P)) = Reference_For_Type (Types, D.Parts (P).Payload)) and then
        Fn.Result = Kind_For_Type (Types, D.Parts (D.Count).Payload) and then
        Fn.Result_Data_Type = Reference_For_Type (Types, D.Parts (D.Count).Payload);
   end Matches_Signature;
end CCL.VM.Verifier_State;
