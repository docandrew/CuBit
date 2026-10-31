with Main_Model;
with CuBit.Driver_Readiness;
package body CuBit.Messages is
   package B renames Main_Model;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, B.Scenario;
   use type CuBit.Handles.Endpoint, CuBit.Handles.Notification;
   function Own_Process return CuBit.Process_IDs.Process_ID is (B.Driver_ID);
   procedure Waited is
   begin
      B.Waits := B.Waits + 1; B.Check (B.Waits <= 2048, "bounded model waits before shutdown");
      if B.Decline_Reports > 0 then
         B.Post_Decline_Waits := B.Post_Decline_Waits + 1;
         if B.Post_Decline_Waits = 3 then raise B.Test_Complete; end if;
      end if;
   end Waited;
   function syscall (Number : Interfaces.Unsigned_64; Value : Interfaces.Unsigned_64 := 0) return Interfaces.Unsigned_64 is
   begin
      if Number = SYSCALL_GETTIME then return (if B.Current = B.Clock_Loss then Interfaces.Unsigned_64'Last else B.Clock);
      elsif Number = SYSCALL_SLEEP then
         Waited; if B.Current = B.Wait_Failure then return Interfaces.Unsigned_64'Last; end if;
         B.Check (Value > 0 and then Value < Wait_Forever, "finite positive relative delay"); B.Clock := B.Clock + Value; return 0;
      elsif Number = SYSCALL_YIELD then B.Yields := B.Yields + 1; return 0;
      else raise Program_Error with "unexpected modeled syscall"; end if;
   end syscall;
   function Wait_For_Activity_Until (Deadline : Interfaces.Unsigned_64) return Activity_Result is
   begin
      Waited;
      if Deadline /= Wait_Forever and then Deadline > B.Clock then B.Clock := Deadline;
      else B.Clock := B.Clock + 1; end if;
      return Work_Available;
   end Wait_For_Activity_Until;
   procedure Note (Msg : Message) is
   begin
      if Msg.tag.label = CuBit.Driver_Readiness.Ready_Label then B.Ready_Reports := B.Ready_Reports + 1;
      elsif Msg.tag.label = CuBit.Driver_Readiness.Not_Present_Label then
         B.Decline_Reports := B.Decline_Reports + 1; B.Writes_At_Decline := B.Register_Writes; B.Reads_At_Decline := B.Register_Reads;
      else raise Program_Error with "unknown readiness label"; end if;
      B.Check (B.Ready_Reports + B.Decline_Reports = 1, "Main sends readiness exactly once");
   end Note;
   function capSend (Target : CuBit.Handles.Endpoint; Msg : Message; Deadline : Interfaces.Unsigned_64) return MessageTag is
   begin
      B.Check (Target = 1 and then Deadline > B.Clock and then Deadline < Wait_Forever, "readiness uses expected endpoint and finite deadline"); Note (Msg);
      if B.Current = B.Readiness_Refused then return (label => 1); end if;
      B.Ready_ACK := Msg.tag.label = CuBit.Driver_Readiness.Ready_Label; return NULL_TAG;
   end capSend;
   function capSubmit (Target : CuBit.Handles.Endpoint; Msg : Message; Token : Interfaces.Unsigned_64) return Boolean is
   begin
      B.Check (Target = 1 and then Token = NO_COMPLETION_TOKEN and then Msg.tag.label = CuBit.Driver_Readiness.Not_Present_Label,
         "clockless report is one-way decline only"); Note (Msg); return True;
   end capSubmit;
   function registerDriver (Role : Interfaces.Unsigned_64; Authority : CuBit.Handles.Notification) return Interfaces.Unsigned_64 is
   begin
      B.Check (Role = 34 and then Authority = 1 and then B.Register_Writes > 0, "GPIO role registration follows actual community parking");
      B.Role_Attempts := B.Role_Attempts + 1;
      return (if B.Current = B.Role_Refused then Interfaces.Unsigned_64'Last else B.P.To_Word (B.Driver_ID));
   end registerDriver;
   procedure debugPrint (Text : String) is
   begin B.U.Append (B.Logs, Text); end debugPrint;
end CuBit.Messages;
