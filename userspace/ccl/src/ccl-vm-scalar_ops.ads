with CCL.VM.Machine;

--  Integer arithmetic, comparisons and Boolean negation. They change the
--  operand stack, the PC and the terminal status, never the fuel limit or
--  the import lifecycle.
private package CCL.VM.Scalar_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   procedure Run_Arithmetic (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   Add_Integer | Subtract_Integer | Multiply_Integer | Divide_Integer | Modulo_Integer,
          Post => Same_Frame (State'Old, State);

   procedure Run_Logic (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character |
                   Not_Boolean,
          Post => Same_Frame (State'Old, State);
   --  Inlined into the dispatch loop (as the nested steps were): one
   --  call per instruction would cost the hot path.
   pragma Inline_Always (Run_Arithmetic);
   pragma Inline_Always (Run_Logic);
end CCL.VM.Scalar_Ops;
