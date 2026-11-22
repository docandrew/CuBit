with Launch_Configuration;
with Fixture; with CuBit.Control_Events; with CuBit.Driver_Readiness;
package body CuBit.Messages is
   use type Launch_Configuration.Interrupt_Delivery;
   function syscall (Number : Unsigned_64; Argument : Unsigned_64 := 0) return Unsigned_64 is
   begin
      case Number is
         when SYSCALL_GETTIME => return Fixture.Now;
         when SYSCALL_SLEEP => Sleep_Calls := Sleep_Calls + 1;
            Sleep_Argument := Argument;
            if Argument = 1 then Fixture.Poll_Sleeps := Fixture.Poll_Sleeps + 1;
            elsif Argument = 5000 then Fixture.Health_Sleeps := Fixture.Health_Sleeps + 1; raise Fixture.Done;
            else raise Program_Error with "unexpected sleep interval"; end if;
            return Sleep_Status;
         when others => raise Program_Error;
      end case;
   end syscall;
   function Wait_For_Activity_Until (Deadline : Unsigned_64) return Activity_Result is
   begin Wait_Calls := Wait_Calls + 1; Wait_Argument := Deadline; return Wait_Result; end Wait_For_Activity_Until;
   function Poll_Completion (Result : System.Address) return Unsigned_64 is
      pragma Unreferenced (Result);
   begin return Completion_Count; end Poll_Completion;
   function Poll_Event (Item : out Message; Publisher : out Process_ID) return Boolean is
   begin
      Item := NULL_MESSAGE; Publisher := No_Process;
      if Fixture.Scenario = 17 and not Fixture.Peer_Event_Sent then
         Fixture.Peer_Event_Sent := True;
         Publisher := CuBit.Process_IDs.From_Word (16#F123_4567_89AB_CDEF#);
         Item.tag.label := CuBit.Control_Events.Control_Label;
         Item.tag.length := CuBit.Control_Events.Control_Words;
         Item.words (0) := CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop);
         return True;
      end if;
      if Fixture.Scenario = 19 and Fixture.Polled < 20 then
         Fixture.Polled := Fixture.Polled + 1;
         Item.tag.label := 1; Item.tag.length := 2; Item.words (0) := 1;
         return True;
      elsif Fixture.Scenario in 20 .. 23 and not Fixture.Peer_Event_Sent then
         Fixture.Peer_Event_Sent := True;
         Item.tag.label := CuBit.Control_Events.Control_Label;
         Item.tag.length := CuBit.Control_Events.Control_Words;
         Item.words (0) := CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop);
         case Fixture.Scenario is
            when 20 => Item.tag.protocol := 1;
            when 21 => Item.tag.reserved := 1;
            when 22 => Item.tag.ipc.Accepts_Capability := True;
            when 23 => Item.authorityTag := 1;
            when others => null;
         end case;
         return True;
      end if;
      return False;
   end Poll_Event;
   procedure Poll_Service_Request (From : out Process_ID; Msg : out Message; Found : out Boolean) is
   begin
      From := CuBit.Process_IDs.From_Word (16#F123_4567_89AB_CDEF#);
      Msg := NULL_MESSAGE; Msg.tag.label := 42;
      Found := Fixture.Scenario in 3 | 18 and then Fixture.Waits > 0 and then not Fixture.Supplied;
      if Found then Fixture.Supplied := True; end if;
   end Poll_Service_Request;
   function reply (ReplyTo : Process_ID; Msg : Message) return Unsigned_64 is
   begin
      Fixture.Reply_Calls := Fixture.Reply_Calls + 1; Fixture.Reply_Label := Unsigned_64 (Msg.tag.label);
      Reply_Calls := Reply_Calls + 1; Reply_Peer := ReplyTo;
      Reply_Message := Msg; return Reply_Status;
   end reply;
   function capSend (Target : CuBit.Handles.Endpoint; Msg : Message;
      Deadline : Unsigned_64) return MessageTag is
      pragma Unreferenced (Target);
   begin Fixture.Ready_Calls := Fixture.Ready_Calls + 1; Fixture.Ready_Label := Msg.tag.label;
      if Deadline /= Wait_Forever then raise Program_Error with "readiness deadline"; end if;
      if Msg.tag.label = CuBit.Driver_Readiness.Ready_Label and then Fixture.Delivery = Launch_Configuration.MSI and then not Fixture.Masked then
         raise Program_Error with "Ready before source mask";
      end if;
      Send_Calls := Send_Calls + 1; Sent_Message := Msg; Sent_Deadline := Deadline; return Send_Result; end capSend;
   function capSubmit (Target : CuBit.Handles.Endpoint; Msg : Message;
      Token : Unsigned_64) return Boolean is
      pragma Unreferenced (Target);
   begin Submit_Calls := Submit_Calls + 1; Sent_Message := Msg; Submitted_Token := Token; return Submit_Result; end capSubmit;
   function registerDriver (Role : Unsigned_64; Authority : CuBit.Handles.Endpoint) return Unsigned_64 is
      pragma Unreferenced (Role, Authority);
   begin return CuBit.Process_IDs.To_Word (Own_Process); end registerDriver;
   procedure Wait_For_Activity_Or_Event (Deadline : Unsigned_64; Event : aliased out Message; Result : out Wake_Result) is
   begin
      Fixture.Waits := Fixture.Waits + 1; Fixture.Deadline_Seen := Deadline;
      if Deadline /= 1000 or else Fixture.Masked or else not Fixture.Payload then
         raise Program_Error with "wait before arm or wrong deadline";
      end if;
      Event := NULL_MESSAGE;
      if Fixture.Scenario = 4 then Result := Unavailable;
      elsif Fixture.Scenario = 2 then Fixture.Now := Deadline; Result := Deadline_Reached;
      elsif Fixture.Scenario in 3 | 18 then Result := Work_Available;
      else
         Result := Event_Taken;
         if Fixture.Scenario = 5 then
            Event.tag.label := CuBit.Control_Events.Control_Label;
            Event.tag.length := CuBit.Control_Events.Control_Words;
            Event.words (0) := CuBit.Control_Events.Control_Kind'Enum_Rep (CuBit.Control_Events.Stop);
         else
            Event.tag.label := 1; Event.tag.length := 2; Event.words (0) := 1;
            if Fixture.Scenario = 6 then Event.authorityTag := 1; end if;
         end if;
      end if;
   end Wait_For_Activity_Or_Event;
end CuBit.Messages;
