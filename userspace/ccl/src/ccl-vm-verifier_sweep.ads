with CCL.VM.Verifier_State;

--  The verifier's sweep: the abstract state at each function's entry, and
--  one instruction's step (the checks every instruction gets, its family's
--  transfer function, and the state it passes on). CCL.VM.Verify runs the
--  step once per instruction, in order.
private package CCL.VM.Verifier_Sweep with SPARK_Mode => On is
   use CCL.VM.Verifier_State;

   --  The main body starts empty; a function's code with its parameters on
   --  the stack.
   procedure Enter (Candidate : Program; States : in out State_Table; Error : in out Validation_Error)
     with Global => null,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Instruction PC, reached with States (PC): its checks and transfer
   --  function; the state it falls through to or jumps to is merged into
   --  States, its depth into Own_Max, its calls into Calls and Base_Max.
   procedure Step
     (Candidate : Program; PC : Instruction_Index;
      States : in out State_Table; Switches : in out Switch_Table;
      Own_Max : in out Region_Demands; Base_Max : in out Call_Demands; Calls : in out Call_Presence;
      Error : in out Validation_Error)
     with Global => null,
          Pre => Error = Valid and then Program_Length (PC) < Candidate.Length;
end CCL.VM.Verifier_Sweep;
