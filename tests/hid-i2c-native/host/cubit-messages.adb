package body CuBit.Messages is
   function syscall (Number : Unsigned_64; Argument : Unsigned_64 := 0) return Unsigned_64 is
   begin
      case Number is
         when SYSCALL_GETTIME => return Clock_Value;
         when SYSCALL_SLEEP => Sleep_Calls := Sleep_Calls + 1;
            Sleep_Argument := Argument; return Sleep_Status;
         when others => raise Program_Error;
      end case;
   end syscall;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin Wait_Calls := Wait_Calls + 1; Wait_Argument := Deadline; return Wait_Result; end Wait_For_Activity_Until;
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      pragma Unreferenced (Result);
   begin return Completion_Count; end Poll_Completion;
   function Poll_Event (Item : out Message; Publisher : out Process_ID) return Boolean is
      Found : constant Boolean := Event_Present;
   begin
      Item := (if Found then Queued_Message else NULL_MESSAGE);
      Publisher := (if Found then Queued_Peer else No_Process);
      Event_Present := False; return Found;
   end Poll_Event;
   procedure Poll_Service_Request (From : out Process_ID; Msg : out Message; Found : out Boolean) is
   begin
      Found := Request_Present; Request_Present := False;
      From := (if Found then Queued_Peer else No_Process);
      Msg := (if Found then Queued_Message else NULL_MESSAGE);
   end Poll_Service_Request;
   function reply (ReplyTo : Process_ID; Msg : Message) return Unsigned_64 is
   begin
      Reply_Calls := Reply_Calls + 1; Reply_Peer := ReplyTo;
      Reply_Message := Msg; return Reply_Status;
   end reply;
   function capSend (Target : CuBit.Handles.Endpoint; Msg : Message;
      Deadline : Unsigned_64) return MessageTag is
      pragma Unreferenced (Target);
   begin Send_Calls := Send_Calls + 1; Sent_Message := Msg; Sent_Deadline := Deadline; return Send_Result; end capSend;
   function capSubmit (Target : CuBit.Handles.Endpoint; Msg : Message;
      Token : Unsigned_64) return Boolean is
      pragma Unreferenced (Target);
   begin Submit_Calls := Submit_Calls + 1; Sent_Message := Msg; Submitted_Token := Token; return Submit_Result; end capSubmit;
end CuBit.Messages;
