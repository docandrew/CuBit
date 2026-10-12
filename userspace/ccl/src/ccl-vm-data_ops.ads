with CCL.VM.Machine;

--  Variants, record fields and stack shuffling: they change the operand
--  stack, the PC and the terminal status, never the fuel limit or the
--  import lifecycle.
private package CCL.VM.Data_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   procedure Run_Data (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   Make_Variant | Equal_Variant | Switch_Variant | Copy_Stack | Drop_Under_Top | Project_Field,
          Post => Same_Frame (State'Old, State);
   --  Inlined into the dispatch loop (as the nested steps were): one
   --  call per instruction would cost the hot path.
   pragma Inline_Always (Run_Data);
end CCL.VM.Data_Ops;
