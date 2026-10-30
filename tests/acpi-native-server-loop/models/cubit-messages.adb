pragma Ada_2022;
package body CuBit.Messages is
   procedure debugPrint (str : String) is
      pragma Unreferenced (str);
   begin Test.Debug_Count := Test.Debug_Count + 1; end debugPrint;
   procedure Reset is begin Test := (others => <>); end Reset;
   procedure Enqueue (From : Process_ID; Item : Message) is
   begin
      Test.Queued := Test.Queued + 1;
      Test.Queue (Test.Queued) := Item; Test.Senders (Test.Queued) := From;
   end Enqueue;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      Test.Wait_Count := Test.Wait_Count + 1; Test.Deadlines (Test.Wait_Count) := Deadline;
      if Test.Wake_Credits = 0 then return Unavailable; end if;
      Test.Wake_Credits := Test.Wake_Credits - 1;
      if Deadline /= Wait_Forever then Test.Now := Deadline; return Deadline_Reached; end if;
      return Work_Available;
   end Wait_For_Activity_Until;
   function Poll_Completion (result : System.Address) return Unsigned_64 is
      pragma Unreferenced (result);
   begin Test.Completion_Count := Test.Completion_Count + 1; return Test.Completion; end Poll_Completion;
   procedure Poll_Any_Ipc (from : out Process_ID; msg : out Message; found : out Boolean) is
   begin
      Test.Poll_Count := Test.Poll_Count + 1;
      found := Test.Taken < Test.Queued; from := No_Process; msg := NULL_MESSAGE;
      if found then
         Test.Taken := Test.Taken + 1; from := Test.Senders (Test.Taken); msg := Test.Queue (Test.Taken);
      end if;
   end Poll_Any_Ipc;
   function syscall (call : Unsigned_64; arg0 : Unsigned_64 := 0; arg1 : Unsigned_64 := 0;
      arg2 : Unsigned_64 := 0; arg3 : Unsigned_64 := 0; arg4 : Unsigned_64 := 0; arg5 : Unsigned_64 := 0) return Unsigned_64 is
      pragma Unreferenced (arg0, arg1, arg2, arg3, arg4, arg5);
   begin
      if call /= SYSCALL_GETTIME then raise Program_Error with "unexpected modeled syscall"; end if;
      Test.Clock_Count := Test.Clock_Count + 1; return Test.Now;
   end syscall;
   function reply (replyTo : Process_ID; msg : Message) return Unsigned_64 is
   begin
      Test.Reply_Count := Test.Reply_Count + 1;
      Test.Reply_To (Test.Reply_Count) := replyTo; Test.Replies (Test.Reply_Count) := msg;
      return Test.Reply_Result;
   end reply;
end CuBit.Messages;
