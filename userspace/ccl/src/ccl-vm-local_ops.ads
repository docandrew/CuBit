with CCL.VM.Machine;

--  The program's locals and their ownership (CCL.Ownership): initialize,
--  copy, move, drop, borrow, return and dispose. They change the operand
--  stack, the PC, the locals and their ownership state, and the terminal
--  status; never the fuel limit or the import lifecycle.
private package CCL.VM.Local_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   procedure Run_Local (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   Initialize_Local | Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO |
                   Return_Local_RO | Borrow_Local_RW | Return_Local_RW | Apply_Local_Disposition,
          Post => Same_Frame (State'Old, State);
   --  Inlined into the dispatch loop (as the nested steps were): one
   --  call per instruction would cost the hot path.
   pragma Inline_Always (Run_Local);
end CCL.VM.Local_Ops;
