with CCL.Ownership.Bytecode;
with CCL.VM.Verifier_State;

--  The verifier's transfer functions for values: scalars and arithmetic,
--  variants, records, range checks, stack shuffling and the variant switch.
--  Each pops the operand kinds its opcode takes and pushes its result's,
--  setting Error on the first mismatch (and doing nothing once it is set).
private package CCL.VM.Verifier_Values with SPARK_Mode => On is
   use CCL.VM.Verifier_State;

   --  Push_Integer, Push_Boolean, integer arithmetic, comparisons, Not_Boolean,
   --  Drop and Check_Range.
   procedure Step_Scalar
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Push_Integer | Push_Boolean | Add_Integer | Subtract_Integer | Multiply_Integer |
                   Divide_Integer | Modulo_Integer | Equal_Integer | Less_Integer | Less_Equal_Integer |
                   Equal_Boolean | Equal_Character | Not_Boolean | Drop | Check_Range,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Make_Variant, Equal_Variant, Project_Field and Make_Node.
   procedure Step_Composite
     (Candidate : Program; Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Make_Variant | Equal_Variant | Project_Field | Make_Node,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Copy_Stack and Drop_Under_Top.
   procedure Step_Stack
     (Ins : Instruction; State : in out Abstract_State; Error : in out Validation_Error)
     with Global => null,
          Pre => Ins.Op in Copy_Stack | Drop_Under_Top,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  Switch_Variant at PC, inside a region ending at Limit: each member's
   --  target gets the state with its payload pushed. Switch is the
   --  instruction's control flow for the ownership pass.
   procedure Step_Switch
     (Candidate : Program; PC : Instruction_Index; Ins : Instruction; Limit : Program_Length;
      State : in out Abstract_State; States : in out State_Table; Error : in out Validation_Error;
      Switch : out CCL.Ownership.Bytecode.Instruction)
     with Global => null,
          Pre => Ins.Op = Switch_Variant,
          Post => (if Error'Old /= Valid then Error = Error'Old);
end CCL.VM.Verifier_Values;
