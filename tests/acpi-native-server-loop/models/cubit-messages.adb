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
   procedure Enqueue_Event (Publisher : Process_ID; Item : Message) is
   begin
      Test.Events_Queued := Test.Events_Queued + 1;
      Test.Events (Test.Events_Queued) := Item; Test.Publishers (Test.Events_Queued) := Publisher;
   end Enqueue_Event;
   function Poll_Event (msg : out Message; publisher : out Process_ID) return Boolean is
   begin
      Test.Event_Polls := Test.Event_Polls + 1; msg := NULL_MESSAGE; publisher := No_Process;
      if Test.Events_Taken = Test.Events_Queued then return False; end if;
      Test.Events_Taken := Test.Events_Taken + 1;
      msg := Test.Events (Test.Events_Taken); publisher := Test.Publishers (Test.Events_Taken);
      return True;
   end Poll_Event;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin
      Test.Wait_Count := Test.Wait_Count + 1; Test.Deadlines (Test.Wait_Count) := Deadline;
      if Test.Events_Taken < Test.Events_Queued then return Work_Available; end if;
      if Test.Wake_Credits = 0 then return Unavailable; end if;
      Test.Wake_Credits := Test.Wake_Credits - 1;
      if Deadline /= Wait_Forever then Test.Now := Deadline; return Deadline_Reached; end if;
      return Work_Available;
   end Wait_For_Activity_Until;
   function Poll_Completion (result : System.Address) return Unsigned_64 is
      pragma Unreferenced (result);
   begin Test.Completion_Count := Test.Completion_Count + 1; return Test.Completion; end Poll_Completion;
   procedure Poll_Service_Request (from : out Process_ID; msg : out Message; found : out Boolean) is
   begin
      if Test.Poll_Count = 0 then Test.First_IPC_Event_Count := Test.Events_Taken; end if;
      Test.Poll_Count := Test.Poll_Count + 1;
      found := Test.Taken < Test.Queued; from := No_Process; msg := NULL_MESSAGE;
      if found then
         Test.Taken := Test.Taken + 1; from := Test.Senders (Test.Taken); msg := Test.Queue (Test.Taken);
      end if;
   end Poll_Service_Request;
   procedure Poll_Any_Ipc (from : out Process_ID; msg : out Message; found : out Boolean) is
   begin
      Test.Mixed_Polls := Test.Mixed_Polls + 1;
      if Test.Events_Taken < Test.Events_Queued then
         if Test.Poll_Count = 0 then Test.First_IPC_Event_Count := Test.Events_Taken; end if;
         Test.Poll_Count := Test.Poll_Count + 1;
         found := Poll_Event (msg, from);
      else Poll_Service_Request (from, msg, found);
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
