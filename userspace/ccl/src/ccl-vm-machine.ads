--  The executor's registers and the small steps every opcode family shares.
--  Each opcode family (CCL.VM.Scalar_Ops, Text_Ops, Data_Ops, List_Ops,
--  Call_Ops, Local_Ops) is a separate unit whose step procedures take the
--  program, the machine state and these registers, with Pre/Post/Global
--  contracts, so no proof context holds the whole interpreter loop
--  (CODE-002, docs/development-backlog.md).
private package CCL.VM.Machine with SPARK_Mode => On is
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Imports.Import_Phase;

   --  A run's registers within one slice: the operand stack, the next
   --  instruction, and whether (and with what status) the slice ended.
   type Registers is record
      Stack  : Runtime_Stacks.Stack;
      PC     : Instruction_Index := 0;
      Done   : Boolean := False;
      Status : Execution_Status := No_Result;
   end record;

   --  What every opcode step keeps: the fuel limit and the import
   --  lifecycle's phase (the executor's loop invariants rest on these).
   --  A step's contract compares Frame_Of before and after, never a copy of
   --  the whole machine (which may be large).
   type Frame_Mark is record
      Fuel : Unsigned_32 := 0;
      Phase : CCL.Imports.Import_Phase := CCL.Imports.Import_Idle;
   end record;
   function Frame_Of (State : Sized_Machine_State) return Frame_Mark is
     ((Fuel => Fuel_Limit (State), Phase => CCL.Imports.Phase (State.Import_Lifecycle)));

   --  Where a step may start: a valid program, an instruction inside it,
   --  and a slice still running.
   function Ready (Item : Validated_Program; Regs : Registers) return Boolean is
     (Is_Valid (Item) and then Program_Length (Regs.PC) < Item.Content.Length and then not Regs.Done);

   --  Stop the run with Code. Only the terminal status changes.
   procedure Trap (Regs : in out Registers; State : in out Sized_Machine_State; Code : Execution_Status)
     with Global => null,
          Post => Frame_Of (State) = Frame_Of (State)'Old and then Regs.Done and then Regs.Status = Code and then
                  State.Terminal and then State.Terminal_Status = Code;

   --  Push Value and step to the next instruction, or trap.
   procedure Push_Next
     (Item : Validated_Program; Regs : in out Registers; State : in out Sized_Machine_State; Value : CCL.VM.Value)
     with Global => null, Post => Frame_Of (State) = Frame_Of (State)'Old;

   --  Pop a text operand, or trap.
   procedure Pop_Text (Regs : in out Registers; State : in out Sized_Machine_State; Value : out CCL.VM.Value)
     with Global => null, Post => Frame_Of (State) = Frame_Of (State)'Old;

   --  Enter the function value Callee: its captures, then Arity arguments,
   --  as the callee's frame; Return_PC is where its result comes back to.
   procedure Enter_Call
     (Item : Validated_Program; Regs : in out Registers; State : in out Sized_Machine_State;
      Callee : Value; Arguments : Component_Values; Arity : CCL.Types.Component_Count;
      Return_PC : Instruction_Index; Good : out Boolean)
     with Global => null, Post => Frame_Of (State) = Frame_Of (State)'Old;
   --  Small enough to inline into every step.
   pragma Inline_Always (Trap);
   pragma Inline_Always (Push_Next);
   pragma Inline_Always (Pop_Text);
end CCL.VM.Machine;
