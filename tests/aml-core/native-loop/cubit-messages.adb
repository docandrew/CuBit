with CuBit.Memory_Grants;
package body CuBit.Messages is
   procedure Poll_Any_Ipc (From : out Process_ID; Msg : out Message; Found : out Boolean) is
   begin
      Polls := Polls + 1;
      Found := Next < Used;
      From := NO_PROCESS; Msg := NULL_MESSAGE;
      if Found then Next := Next + 1; From := Senders (Next); Msg := Incoming (Next); end if;
   end Poll_Any_Ipc;
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      pragma Unreferenced (Result);
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
   function syscall
     (Call : Unsigned_64; Arg0 : Unsigned_64 := 0; Arg1 : Unsigned_64 := 0;
      Arg2 : Unsigned_64 := 0; Arg3 : Unsigned_64 := 0;
      Arg4 : Unsigned_64 := 0; Arg5 : Unsigned_64 := 0) return Unsigned_64 is
   begin
      if Call /= SYSCALL_GETTIME or else Arg0 /= 0 or else Arg1 /= 0 or else
        Arg2 /= 0 or else Arg3 /= 0 or else Arg4 /= 0 or else Arg5 /= 0 then raise Program_Error; end if;
      return Now;
   end syscall;
   function reply (ReplyTo : Process_ID; Msg : Message) return Unsigned_64 is
   begin
      if ReplyTo /= Senders (Next) then raise Program_Error with "opaque reply target mismatch"; end if;
      Sent := Sent + 1; Replies (Sent) := Msg;
      return (if Fail_Reply then Unsigned_64'Last else 0);
   end reply;
end CuBit.Messages;
