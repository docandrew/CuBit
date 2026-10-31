pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; with CuBit.Handles; with CuBit.Handles.Testing; with CuBit.Handles.IPC;
with ACPI_EC_Types; with ACPI_EC_Transactions; with ACPI_EC_Registers; with ACPI_EC_Endpoint;
with ACPI_EC_Native_Calls; with ACPI_EC_Event_Types; with ACPI_EC_Event_Publisher;
with ACPI_EC_Backend_Progress; with ACPI_EC_Backend_Waits; with ACPI_EC_Backend_Controller;
with ACPI_EC_Backend_Controller.With_Receive; with ACPI_EC_Clock_Policy; with ACPI_Region_Protocol;
procedure Drain_Tests is
   package T renames ACPI_EC_Types; package M renames CuBit.Messages;
   package H renames CuBit.Handles; package W renames ACPI_Region_Protocol;
   package ET renames ACPI_EC_Event_Types;
   use type T.Byte, T.Read_Register, T.Write_Register, T.Work_Budget, T.Microseconds;
   use type M.Process_ID, ET.Controller_ID, ET.Epoch_ID, ET.Event_Number;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Writing_Mode : constant Boolean := Mode = "write";
   Query_Mode : constant Boolean := Mode in "query" | "query-unknown";
   Cut : constant Natural := Natural'Value (Ada.Command_Line.Argument (2));
   Checks, IOs, Saves, Answers, Requests, Hooks, Publications : Natural := 0;
   Now : T.Microseconds := 1;
   Addressed, Consumed, Saved, In_Drain, Fault_Fired : Boolean := False;
   Sender : constant M.Process_ID := M.From_Word (16#ABCD_1234_0000_0001#);
   Last_Reply : M.Message := M.NULL_MESSAGE;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Probe;
   procedure Read (Register : T.Read_Register; Value : out T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Probe; Result := T.Completed;
      if Register = T.Status_Register then
         Value := (if Addressed and not Consumed and not Writing_Mode then T.Output_Full else 0);
         if Query_Mode and not Consumed then Value := Value or T.Event_Pending; end if;
      else Check (Addressed and not Consumed, "data consumed once after address"); Consumed := True; Value := (if Query_Mode then 1 else 16#A5#); end if;
   end Read;
   procedure Write (Register : T.Write_Register; Value : T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Probe; Result := T.Completed;
      if Register = T.Command_Register then Check (Value = (if Query_Mode then T.Query_Command elsif Writing_Mode then T.Write_Command else T.Read_Command), "only original command");
         if Query_Mode then Addressed := True; end if;
      elsif Addressed and Writing_Mode then
         Check (Value = 16#A6# and not Consumed, "one original write value"); Consumed := True;
      else Check (Value = 16#10# and not Addressed, "one original address write"); Addressed := True;
         if In_Drain and Mode = "unknown" then Result := T.Indeterminate; end if;
      end if;
   end Write;
   procedure Clock (Value : out T.Microseconds; Available : out Boolean) is
   begin Value := Now; Available := not (In_Drain and Mode = "clock"); end Clock;
   function Owned return Boolean is (not (In_Drain and Mode = "ownership"));
   function Save return H.IPC.Saved_Reply is
   begin Check (not Saved, "single saved authority"); Saved := True; Saves := Saves + 1;
      return (True, H.Testing.Reply_For_Test (16#1_0000_0001#)); end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin Check (Saved and H.Valid (Item), "answer exact retained authority"); Saved := False;
      Answers := Answers + 1; Last_Reply := Msg; return Mode /= "answer-lost"; end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Unsigned_64 is
   begin Check (From = Sender, "full sender"); Last_Reply := Msg; Requests := Requests + 1; return 1; end Answer_Current;
   procedure Publish (Item : ET.Notice; Result : out ET.Publication) is
   begin Check (Item.Controller = 42 and Item.Epoch = 1 and Item.Number = 1, "notice");
      Publications := Publications + 1;
      Result := (if Mode = "query-unknown" then ET.Indeterminate else ET.Enqueued); end Publish;
   procedure Pause (Target : T.Microseconds; Remaining : in out T.Work_Budget;
                    Result : out ACPI_EC_Clock_Policy.Pause_Report) is
   begin
      if Remaining = 0 then Result := (Status => ACPI_EC_Clock_Policy.Budget_Exhausted, others => <>);
      else Remaining := Remaining - 1; Now := Target; Result := (ACPI_EC_Clock_Policy.Pause_Returned, Target); end if;
   end Pause;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package R is new ACPI_EC_Registers (Engine);
   package Endpoint is new ACPI_EC_Endpoint (R);
   package Calls is new ACPI_EC_Native_Calls (R, Endpoint, Save, Answer_Saved, Answer_Current);
   package Events is new ACPI_EC_Event_Publisher (R, Publish);
   package Progress is new ACPI_EC_Backend_Progress (R, Calls, Events);
   package Waits is new ACPI_EC_Backend_Waits (R, Progress, Pause);
   package Controller is new ACPI_EC_Backend_Controller (R, Calls, Events, Progress, Waits, Clock, 1000, 100, 8, Answer_Current);
   procedure Reenter_Turn;
   procedure Handle_Event (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
   begin
      if Mode = "serving-exception" then raise Program_Error with "serving hook fault"; end if;
      if Mode = "serving-reentry" then Reenter_Turn; end if;
      Hooks := Hooks + 1; Stop := From = M.No_Process and Item.tag.label = 16#100#; Handled := Stop; end Handle_Event;
   package Loop_Owner is new Controller.With_Receive (Handle_Event => Handle_Event);
   use type Loop_Owner.Stop_Reason, Loop_Owner.Drain_Status, R.Submission, Events.Phase, Controller.Admission_Status;
   procedure Reenter_Turn is
      Nested : Loop_Owner.Report;
   begin Loop_Owner.Turn (Nested); Check (Nested.Reason = Loop_Owner.Reentered, "serving reentry quarantines"); end Reenter_Turn;
   procedure Probe is
      Nested : Loop_Owner.Drain_Report;
   begin
      if In_Drain and not Fault_Fired and Mode in "exception" | "reentry" then
         Fault_Fired := True;
         if Mode = "exception" then raise Program_Error with "modeled adapter fault";
         else Loop_Owner.Drain (Nested); Check (Nested.Status = Loop_Owner.Quarantined, "nested drain quarantines"); end if;
      end if;
   end Probe;
   Report, Again : Loop_Owner.Report;
   Drained : Loop_Owner.Drain_Report;
   Request, Event : M.Message := M.NULL_MESSAGE;
   Accepted : Boolean;
   Before_IO, Polls, Event_Polls, Wait_Count, Hook_Count, Answer_Count, Publication_Count : Natural;
   Admission : Controller.Admission_Status;
   Submission : R.Submission; Token : R.Operation_ID;
begin
   M.Reset;
   R.Install ((Register => 17, Epoch => 1, Owner => R.Owner_ID (M.To_Word (Sender)), Cell => 16#10#, Readable => True, Writable => True, Exact_Write_Safe => True, Write_Mask => 16#FF#), Accepted);
   Check (Accepted, "admission");
   Events.Configure (42, 1, Accepted); Check (Accepted, "publisher");
   Request.tag.label := (if Writing_Mode then W.Write_Operation else W.Read_Operation);
   Request.tag.length := 4; Request.words := [1, 17, (if Writing_Mode then 16#A6# else 0), 0];
   Loop_Owner.Drain (Drained);
   Check (Drained.Status = Loop_Owner.Still_Serving and Drained.State.Reason = Loop_Owner.Serving and IOs = 0
      and M.Test.Poll_Count = 0 and M.Test.Event_Polls = 0 and M.Test.Wait_Count = 0, "drain cannot stop or consume serving owner");
   if Mode = "conflict" then
      R.Submit (R.Owner_ID (M.To_Word (Sender)), 17, 1, False, 0, 1000, 100, Submission, Token);
      Check (Submission = R.Started, "unowned test transaction");
      Loop_Owner.Turn (Report); Check (Report.Reason = Loop_Owner.Inconsistent_Owners, "conflict latches");
   elsif Mode = "idle" then
      Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event);
      Loop_Owner.Turn (Report); Check (Report.Reason = Loop_Owner.Event_Stop, "idle stop");
   else
      if Query_Mode then
         Controller.Start_Query (2, Admission, Submission);
         Check (Admission = Controller.Admitted and Submission = R.Started and IOs = 0, "admitted original query");
      else
      M.Enqueue (Sender, Request); Loop_Owner.Turn (Report);
      Check (Saved and Saves = 1 and IOs = 0, "accepted original call");
      end if;
      for Step in 1 .. 64 loop
         exit when IOs >= Cut;
         Loop_Owner.Turn (Report);
         Check (Report.Reason = Loop_Owner.Serving and (Saved or Query_Mode), "cut before completion");
      end loop;
      Check (IOs = Cut, "exact stopped hardware prefix");
      Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event);
      Loop_Owner.Turn (Report);
      Check (Report.Reason = (if Mode = "serving-exception" then Loop_Owner.Adapter_Fault
         elsif Mode = "serving-reentry" then Loop_Owner.Reentered else Loop_Owner.Event_Stop) and (Saved or Query_Mode) and IOs = Cut, "stop preserves original call and prefix");
   end if;
   M.Enqueue (Sender, Request); Event.tag.label := 16#120#; M.Enqueue_Event (Sender, Event);
   Polls := M.Test.Poll_Count; Event_Polls := M.Test.Event_Polls; Wait_Count := M.Test.Wait_Count;
   Hook_Count := Hooks; In_Drain := True;
   for Step in 1 .. 64 loop
      Before_IO := IOs; Loop_Owner.Drain (Drained);
      Check (IOs <= Before_IO + 1, "bounded drain IO");
      Check (Drained.State.Reason = Report.Reason, "original stop remains latched");
      Check (M.Test.Poll_Count = Polls and M.Test.Event_Polls = Event_Polls and M.Test.Wait_Count = Wait_Count
        and Hooks = Hook_Count and Requests = 0 and (Publications = 0 or Query_Mode) and M.Test.Mixed_Polls = 0, "drain admits no queue/hook/wait work");
      exit when Drained.Status = Loop_Owner.Quarantined or (not Drained.State.Field_Reply_Pending and Events.Observe.Stage not in Events.Querying | Events.Publication_Pending);
   end loop;
   if Mode in "exception" | "reentry" | "conflict" | "serving-exception" | "serving-reentry" then
      Check (Drained.Status = Loop_Owner.Quarantined, "unsafe adapter state quarantined");
      Check (R.Active and (Saved or Mode = "conflict"), "quarantine retains backing and pending owner");
   elsif Query_Mode then
      Check (Drained.Status = Loop_Owner.Advanced and not R.Active and Answers = 0 and Saves = 0, "query drains independently of Field replies");
      Check (Publications = (if Cut < 2 then 0 else 1), "one postwrite query publication only");
      Check (Drained.State.Notice_Retained = (Mode = "query-unknown" and Cut >= 2), "unknown notice remains retained");
   elsif Mode = "idle" then
      Check (Drained.Status = Loop_Owner.Advanced and IOs = 0 and not R.Active and Answers = 0, "empty drain performs no hardware or reply");
   else
      Check (Drained.Status = Loop_Owner.Advanced and Answers = 1 and not Saved and not Drained.State.Field_Reply_Pending,
         "original call answered once after stop");
      if Mode in "unknown" | "clock" | "ownership" then
         Check (Drained.State.Hardware_Recovery and R.Retained_For_Recovery, "postwrite fault retains hardware recovery");
      else Check (not Drained.State.Hardware_Recovery and not R.Active, "clean exchange retires");
         Check (Last_Reply.words (0) = W.Outcome'Pos ((if Cut < 2 then W.Denied else W.Done)), "precommand stop or postwrite drain outcome");
      end if;
   end if;
   Before_IO := IOs; Answer_Count := Answers; Publication_Count := Publications;
   Check (Publications <= 1, "bounded publication");
   for Step in 1 .. 3 loop
      Loop_Owner.Drain (Drained); Loop_Owner.Turn (Again);
      Check (IOs = Before_IO and Answers = Answer_Count and Publications = Publication_Count and Again.Reason = Report.Reason, "no replay or serving restart");
      Check (M.Test.Poll_Count = Polls and M.Test.Event_Polls = Event_Polls and M.Test.Wait_Count = Wait_Count, "later queues remain untouched");
   end loop;
   Ada.Text_IO.Put_Line ("EC drain checks" & Checks'Image);
end Drain_Tests;
