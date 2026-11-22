pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles; with CuBit.Handles.Testing; with CuBit.Handles.IPC;
with ACPI_EC_Types; with ACPI_EC_Transactions; with ACPI_EC_Registers;
with ACPI_EC_Endpoint; with ACPI_EC_Native_Calls;
with ACPI_EC_Event_Types; with ACPI_EC_Event_Publisher;
with ACPI_EC_Backend_Progress; with ACPI_EC_Backend_Waits;
with ACPI_EC_Backend_Controller; with ACPI_EC_Backend_Controller.With_Receive;
with ACPI_EC_Clock_Policy; with ACPI_Region_Protocol;
procedure Receive_Tests is
   package T renames ACPI_EC_Types; package M renames CuBit.Messages;
   package H renames CuBit.Handles; package W renames ACPI_Region_Protocol;
   package ET renames ACPI_EC_Event_Types;
   use type T.Byte, T.Read_Register, T.Write_Register, T.Work_Budget;
   use type T.Microseconds, ET.Publication, M.Process_ID, H.Reply;
   use type ET.Controller_ID, ET.Epoch_ID, ET.Event_Number;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Checks, IOs, Saves, Answers, Currents, Event_Calls, Control_Calls, Publications : Natural := 0;
   Now : T.Microseconds := 1;
   Addressed, Consumed, Querying, Saved : Boolean := False;
   Sender : constant M.Process_ID := M.From_Word (16#ABCD_1234_0000_0001#);
   Last_Reply : M.Message := M.NULL_MESSAGE;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Read (Register : T.Read_Register; Value : out T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed;
      if Register = T.Status_Register then
         Value := (if (Addressed or Querying) and not Consumed then T.Output_Full else 0);
         if Mode in "query" | "publication-unknown" and then not Consumed then
            Value := Value or T.Event_Pending;
         end if;
      else Check (Addressed or Querying, "data follows address/query"); Consumed := True;
         Value := (if Querying then 1 else 16#A5#);
      end if;
   end Read;
   procedure Write (Register : T.Write_Register; Value : T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed;
      if Register = T.Command_Register then
         Check (Value in T.Read_Command | T.Query_Command, "known EC command");
         Querying := Value = T.Query_Command;
         if Mode = "unknown" then Result := T.Indeterminate; end if;
      else Check (Value = 16#10#, "admitted cell"); Addressed := True;
      end if;
   end Write;
   procedure Clock (Value : out T.Microseconds; Available : out Boolean) is
   begin Value := Now; Available := Mode /= "clock"; end Clock;
   function Owned return Boolean is (True);
   function Save return H.IPC.Saved_Reply is
   begin Check (not Saved, "one saved reply"); Saved := True; Saves := Saves + 1;
      return (True, H.Testing.Reply_For_Test (16#1_0000_0001#));
   end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin Check (Saved and H.Valid (Item), "matching retained reply");
      Saved := False; Answers := Answers + 1; Last_Reply := Msg; return True;
   end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Unsigned_64 is
   begin Check (From = Sender, "opaque sender retained");
      Currents := Currents + 1; Last_Reply := Msg; return 1;
   end Answer_Current;
   procedure Publish (Item : ET.Notice; Result : out ET.Publication) is
   begin Check (Item.Controller = 42 and Item.Epoch = 1 and Item.Number = 1, "notice identity");
      Publications := Publications + 1;
      Result := (if Mode = "publication-unknown" then ET.Indeterminate else ET.Enqueued);
   end Publish;
   procedure Pause (Target : T.Microseconds; Remaining : in out T.Work_Budget;
                    Result : out ACPI_EC_Clock_Policy.Pause_Report) is
   begin
      if Remaining = 0 then Result := (Status => ACPI_EC_Clock_Policy.Budget_Exhausted, others => <>);
      else Remaining := Remaining - 1; Now := Target;
         Result := (ACPI_EC_Clock_Policy.Pause_Returned, Target);
      end if;
   end Pause;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package R is new ACPI_EC_Registers (Engine);
   package Endpoint is new ACPI_EC_Endpoint (R);
   package Calls is new ACPI_EC_Native_Calls (R, Endpoint, Save, Answer_Saved, Answer_Current);
   package Events is new ACPI_EC_Event_Publisher (R, Publish);
   package Progress is new ACPI_EC_Backend_Progress (R, Calls, Events);
   package Waits is new ACPI_EC_Backend_Waits (R, Progress, Pause);
   package Controller is new ACPI_EC_Backend_Controller (R, Calls, Events, Progress, Waits, Clock, 1000, 100, 8, Answer_Current);
   use type Controller.Admission_Status, R.Submission;
   procedure Reenter;
   procedure Handle_Event (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      Status : Controller.Admission_Status; Submission : R.Submission;
   begin
      Event_Calls := Event_Calls + 1; Stop := False; Handled := False;
      if Mode = "exception" then raise Program_Error with "modeled hook failure"; end if;
      if Mode = "reentry" then Reenter; return; end if;
      if From = M.No_Process and Item.tag.label = 16#100# then Stop := True; Handled := True;
      elsif From = Sender and Item.tag.label = 16#101# then
         Controller.Start_Query (2, Status, Submission);
         Check (Status = Controller.Admitted and Submission = R.Started, "trusted modeled source starts query");
         Handled := True;
      end if;
   end Handle_Event;
   procedure Handle_Control (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      Ignored : Unsigned_64;
   begin
      Control_Calls := Control_Calls + 1; Stop := False; Handled := False;
      if Item.tag.label = 16#102# and From = Sender then
         Handled := True;
         if Mode = "control-stop" then Stop := True;
         else Ignored := M.reply (From, M.NULL_MESSAGE); end if;
      end if;
   end Handle_Control;
   package Loop_Owner is new Controller.With_Receive (Handle_Event, Handle_Control);
   use type Loop_Owner.Stop_Reason;
   procedure Reenter is
      Nested : Loop_Owner.Report;
   begin Loop_Owner.Turn (Nested); Check (Nested.Reason = Loop_Owner.Reentered, "nested entry refuses"); end Reenter;
   Report, Again : Loop_Owner.Report;
   Request, Event : M.Message := M.NULL_MESSAGE;
   Accepted : Boolean;
   Before_IO, Before_Polls, Before_Events, Before_Waits, Before_Current : Natural;
   Conflict : R.Submission; Token : R.Operation_ID;
begin
   M.Reset;
   R.Install ((Register => 17, Epoch => 1, Owner => R.Owner_ID (M.To_Word (Sender)),
      Cell => 16#10#, Readable => True, others => <>), Accepted); Check (Accepted, "catalog installed");
   Events.Configure (42, 1, Accepted); Check (Accepted, "modeled source configured");
   Request.tag.label := W.Read_Operation; Request.tag.length := 4; Request.words := [1, 17, 0, 0];
   Event.tag.label := 16#120#;
   if Mode in "field" | "serve" | "events" | "unknown" | "clock" | "live-stop" then M.Enqueue (Sender, Request); end if;
   if Mode = "events" then for I in 1 .. 4 loop M.Enqueue_Event (Sender, Event); end loop; end if;
   if Mode in "stop" | "exception" | "reentry" then
      Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event); M.Enqueue (Sender, Request);
   end if;
   if Mode in "query" | "publication-unknown" then Event.tag.label := 16#101#; M.Enqueue_Event (Sender, Event); end if;
   if Mode in "control" | "control-stop" then Request.tag.label := 16#102#; M.Enqueue (Sender, Request); end if;
   if Mode = "completion" then M.Test.Completion := 1; M.Enqueue_Event (Sender, Event); M.Enqueue (Sender, Request); end if;
   if Mode = "spurious" then M.Test.Wake_Credits := 1; end if;
   if Mode = "conflict" then
      R.Submit (R.Owner_ID (M.To_Word (Sender)), 17, 1, False, 0, 1000, 100, Conflict, Token);
      Check (Conflict = R.Started, "deliberate owner conflict fixture");
   end if;
   if Mode = "foreign-query" then Event.tag.label := 16#101#; M.Enqueue_Event (M.From_Word (M.To_Word (Sender) + 1), Event); end if;
   if Mode = "serve" then Loop_Owner.Serve (Report);
   else
   for Turn in 1 .. 64 loop
      Before_IO := IOs;
      Loop_Owner.Turn (Report);
      Check (IOs <= Before_IO + 1, "at most one hardware access per turn");
      exit when Report.Reason /= Loop_Owner.Serving;
      if Mode = "live-stop" and Turn = 1 then
         Check (Saved and IOs = 0, "submission before first hardware access");
         Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event);
      end if;
      if Report.Field_Reply_Pending then Check (M.Test.Wait_Count = 0, "no idle wait over active Field"); end if;
   end loop;
   end if;
   Check (Report.Reason /= Loop_Owner.Serving, "bounded test reaches terminal serving outcome");
   Check (M.Test.Mixed_Polls = 0, "only typed service queue used");
   if Mode in "stop" | "live-stop" then
      Check (Report.Reason = Loop_Owner.Event_Stop and IOs = 0, "event stop precedes IO");
      Check (M.Test.Taken = (if Mode = "stop" then 0 else 1), "stop prevents next request");
      Check (Report.Field_Reply_Pending = (Mode = "live-stop"), "serving stop retains saved reply");
   elsif Mode = "completion" then Check (Report.Reason = Loop_Owner.Unexpected_Reply and M.Test.Event_Polls = 0 and M.Test.Poll_Count = 0 and IOs = 0, "completion wins before queues");
   elsif Mode = "exception" then Check (Report.Reason = Loop_Owner.Adapter_Fault and IOs = 0 and M.Test.Taken = 0, "hook exception stops without new work");
   elsif Mode = "reentry" then Check (Report.Reason = Loop_Owner.Reentered and IOs = 0 and M.Test.Taken = 0, "reentry latches before progress");
   elsif Mode = "conflict" then Check (Report.Reason = Loop_Owner.Inconsistent_Owners and IOs = 0 and M.Test.Poll_Count = 0 and R.Active, "unowned transaction quarantines before receive");
   elsif Mode = "control-stop" then Check (Report.Reason = Loop_Owner.Control_Stop and Saves = 0 and IOs = 0, "control stop performs no Field dispatch");
   else
      Check (Report.Reason = Loop_Owner.Wait_Unavailable, "idle wait refusal is explicit: " & Report.Reason'Image);
      Check (M.Test.Deadlines (M.Test.Wait_Count) = M.Wait_Forever, "idle uses explicit no-deadline sentinel");
      if Mode in "field" | "serve" | "events" then
         Check (Saves = 1 and Answers = 1 and Currents = 0 and not Saved, "exactly one deferred answer");
         Check (Last_Reply.words (0) = W.Outcome'Pos (W.Done) and Last_Reply.words (1) = 16#A5#, "Field data delivered");
         Check (not Report.Hardware_Recovery and not Report.Field_Reply_Pending, "normal Field retires");
         if Mode = "events" then Check (Event_Calls = 4 and M.Test.Events_Taken = 4 and M.Test.First_IPC_Event_Count = 1, "events and service get bounded opportunities"); end if;
      elsif Mode = "unknown" then Check (Answers = 1 and Report.Hardware_Recovery and not Saved and IOs = 2, "recovery retained independently of saved reply");
      elsif Mode = "clock" then Check (Saves = 0 and Currents = 1 and IOs = 0 and Last_Reply.words (0) = W.Outcome'Pos (W.Denied), "clock refusal before save/hardware");
      elsif Mode in "query" | "publication-unknown" then
         Check (Publications = 1 and Saves = 0 and Answers = 0, "query publishes without Field reply");
         Check ((Report.Notice_Retained = (Mode = "publication-unknown")) and not Report.Hardware_Recovery, "unknown delivery stays separate from hardware recovery");
      elsif Mode = "control" then Check (M.Test.Reply_Count = 1 and Saves = 0 and IOs = 0 and Control_Calls = 1, "trusted control consumes exactly its request");
      elsif Mode = "foreign-query" then Check (Event_Calls = 1 and IOs = 0 and Saves = 0 and Publications = 0, "event reception alone does not admit a source");
      elsif Mode = "spurious" then Check (M.Test.Wait_Count = 2, "spurious wake repolls queues");
      end if;
   end if;
   Before_IO := IOs; Before_Polls := M.Test.Poll_Count; Before_Events := M.Test.Event_Polls;
   Before_Waits := M.Test.Wait_Count; Before_Current := Currents;
   Loop_Owner.Turn (Again);
   Check (Again.Reason = Report.Reason and IOs = Before_IO and M.Test.Poll_Count = Before_Polls
     and M.Test.Event_Polls = Before_Events and M.Test.Wait_Count = Before_Waits and Currents = Before_Current,
     "terminal turn does no new work");
   Check (Again.Hardware_Recovery = Report.Hardware_Recovery and Again.Notice_Retained = Report.Notice_Retained
     and Again.Field_Reply_Pending = Report.Field_Reply_Pending, "terminal observations retain obligations");
   Ada.Text_IO.Put_Line ("EC receive checks" & Checks'Image);
end Receive_Tests;
