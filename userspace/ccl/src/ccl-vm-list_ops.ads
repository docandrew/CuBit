with CCL.VM.Machine;

--  Lists, records and range checks: the list opcodes and built-ins
--  (CCL.List_Operations, with the interpreter's fuel), Make_Node and
--  Check_Range. They change the operand stack, the list and text regions,
--  the value arena and the terminal status; they spend fuel but never change
--  its limit, nor the import lifecycle.
private package CCL.VM.List_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   --  A list built-in, with the interpreter's fuel: one per element read and
   --  per sort comparison or joined piece. Subject is the list (the text,
   --  for split); A and B are the operands before it in source.
   procedure Run_List_Builtin
     (Lists  : in out List_Regions.Stack;
      Texts  : in out Text_Regions.Stack;
      Budget : in out CCL.Execution_Budgets.Budget;
      Arena  : Value_Arena;
      Types  : CCL.Types.Registry;
      Item   : CCL.List_Operations.Operation;
      List_Type : CCL.Types.Type_Reference;
      Subject, A, B : Value;
      Result : out Value;
      Status : out Execution_Status)
     with Global => null,
          Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old);

   procedure Run_List (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range,
          Post => Same_Frame (State'Old, State);
end CCL.VM.List_Ops;
