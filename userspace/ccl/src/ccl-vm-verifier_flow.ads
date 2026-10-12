with CCL.VM.Verifier_State;

--  The verifier's transfer functions for control flow: function values and
--  calls, List_Apply, halting and jumps, host imports and the program's
--  locals. Each does nothing once Error is set.
private package CCL.VM.Verifier_Flow with SPARK_Mode => On is
   use CCL.VM.Verifier_State;

   --  Make_Closure, Call_Value and List_Apply.
   procedure Step_Closure
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Make_Closure | Call_Value | List_Apply,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Call_Function and Return_Function in Region: a call records the
   --  callee and the stack under its frame (Calls, Base_Max); a return ends
   --  the region's path (Falls_Through).
   procedure Step_Call
     (Candidate : Program; Ins : Instruction; Region : Region_Index;
      State : in out Abstract_State; Error : in out Validation_Error;
      Calls : in out Call_Presence; Base_Max : in out Call_Demands; Falls_Through : in out Boolean)
     with Global => null,
          Pre => Ins.Op in Call_Function | Return_Function,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Halt, Jump and Jump_If_False at PC, in a region ending at Limit.
   procedure Step_Jump
     (PC : Instruction_Index; Ins : Instruction; Limit : Program_Length;
      State : in out Abstract_State; States : in out State_Table; Error : in out Validation_Error;
      Falls_Through : in out Boolean)
     with Global => null,
          Pre => Ins.Op in Halt | Jump | Jump_If_False,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Invoke_Import.
   procedure Step_Import
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op = Invoke_Import,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  The local opcodes (their ownership is checked by the ownership pass).
   procedure Step_Local
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Initialize_Local | Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO |
                   Return_Local_RO | Borrow_Local_RW | Return_Local_RW | Apply_Local_Disposition,
          Post => (if Error'Old /= Valid then Error = Error'Old);
end CCL.VM.Verifier_Flow;
