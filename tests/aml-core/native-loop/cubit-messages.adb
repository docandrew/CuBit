with CuBit.Memory_Grants;
package body CuBit.Messages is
   procedure Poll_Any_Ipc (From : out Process_ID; Msg : out Message; Found : out Boolean) is
   begin
      Polls := Polls + 1;
      Found := Next < Used;
      From := 0; Msg := NULL_MESSAGE;
      if Found then Next := Next + 1; From := 77; Msg := Incoming (Next); end if;
   end Poll_Any_Ipc;
   function Poll_Completion (Address : System.Address) return Unsigned_64 is
      pragma Unreferenced (Address);
   begin
      return (if Completion_Ready then 1 else 0);
   end Poll_Completion;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      Waits := Waits + 1; Last_Deadline := Deadline;
      if Repair_On_Wait and then Waits = 1 then
         CuBit.Memory_Grants.Return_OK := True;
         Now := Deadline;
         return Deadline_Reached;
      end if;
      return Unavailable;
   end Wait_For_Activity_Until;
   function syscall (Call : Unsigned_64) return Unsigned_64 is
   begin
      if Call /= SYSCALL_GETTIME then raise Program_Error; end if;
      return Now;
   end syscall;
   function reply (Target : Process_ID; Msg : Message) return Unsigned_64 is
   begin
      if Target /= 77 then raise Program_Error; end if;
      Sent := Sent + 1; Replies (Sent) := Msg;
      return (if Fail_Reply then Unsigned_64'Last else 0);
   end reply;
end CuBit.Messages;
