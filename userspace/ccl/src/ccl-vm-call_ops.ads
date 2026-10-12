with CCL.VM.Machine;

--  Function values: Make_Closure, Call_Value, and List_Apply (each, where,
--  fold, any, all, count and sort-by), whose every element is an ordinary
--  call that comes back to the instruction. They change the operand stack,
--  the PC, the call frames, the iterations and the list region; they spend
--  fuel (sort-by's comparisons) but never change its limit, nor the import
--  lifecycle.
private package CCL.VM.Call_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   procedure Run_Call (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in Make_Closure | Call_Value | List_Apply,
          Post => Same_Frame (State'Old, State);
   --  Inlined into the dispatch loop (as the nested steps were): one
   --  call per instruction would cost the hot path.
   pragma Inline_Always (Run_Call);
end CCL.VM.Call_Ops;
