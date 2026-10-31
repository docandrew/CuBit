pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; with CuBit.Handles;
with CuBit.Handles.Testing; with CuBit.Handles.IPC;
with ACPI_EC_Types; with ACPI_EC_Event_Types; with ACPI_EC_Clock_Policy;
with ACPI_EC_Backend_Owner; with ACPI_EC_Backend_Owner.With_Receive;
with ACPI_Region_Protocol;
with CuBit.Control_Events; with CuBit.Grant_References; with Channel_Wake_ABI;
procedure Owner_Tests is
   package T renames ACPI_EC_Types; package M renames CuBit.Messages;
   package H renames CuBit.Handles; package W renames ACPI_Region_Protocol;
   package ET renames ACPI_EC_Event_Types;
   use type T.Byte, T.Read_Register, T.Write_Register, T.Work_Budget, T.Microseconds;
   use type M.Process_ID, ET.Controller_ID, ET.Epoch_ID, ET.Event_Number, ET.Notice;
   Mode : constant String := Ada.Command_Line.Argument (1);
   System_Mode : constant Boolean := Mode'Length >= 7 and then Mode (Mode'First .. Mode'First + 6) = "system-";
   Hook_Count : Natural := 0;
   Query_Commands : Natural := 0;
   Register_Word : constant Unsigned_64 := 16#FEDC_9876_0000_0011#;
   Epoch_Word : constant Unsigned_64 := 16#ABCD_ABCD_0000_0001#;
   use type T.Outcome;
   Checks, IOs, Clocks, Pauses, Saves, Answers, Currents, Publications : Natural := 0;
   Now : T.Microseconds := 1;
   Addressed, Consumed, Querying, Saved : Boolean := False;
   Sender : constant M.Process_ID := M.From_Word (16#ABCD_1234_0000_0001#);
   Consumer : constant M.Process_ID := M.From_Word (16#DCBA_4321_0000_0002#);
   Last_Reply : M.Message := M.NULL_MESSAGE;
   Last_Notice : ET.Notice;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Mode & ": " & Why; end if; end Check;
   procedure Read (Register : T.Read_Register; Value : out T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed;
      if Register = T.Status_Register then
         Value := (if (Addressed or Querying) and not Consumed then T.Output_Full else 0);
         if Mode in "query" | "publication-unknown" | "publication-stalled" | "query-only" | "direct-query" | "no-consumer" |
           "observe-zero" | "observe-event" | "observe-reset" | "observe-refused" then
            if not Consumed then Value := Value or T.Event_Pending; end if;
         end if;
      else Check (Addressed or Querying, "address/query before data"); Consumed := True;
         Value := (if Querying then (if Mode = "observe-zero" then 0 else 1) else 16#A5#);
      end if;
   end Read;
   procedure Write (Register : T.Write_Register; Value : T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed;
      if Register = T.Command_Register then
         Check (Value in T.Read_Command | T.Query_Command, "known command"); Querying := Value = T.Query_Command;
         if Querying then Query_Commands := Query_Commands + 1; end if;
         if Mode in "unknown" | "direct-unknown" then Result := T.Indeterminate; end if;
      else Check (Value = 16#10#, "independently admitted cell"); Addressed := True; end if;
   end Write;
   function Owned return Boolean is (Mode /= "not-owned");
   procedure Clock (Value : out T.Microseconds; Available : out Boolean) is
   begin Clocks := Clocks + 1; Value := Now; Available := Mode not in "clock" | "direct-clock"; end Clock;
   procedure Pause (Target : T.Microseconds; Remaining : in out T.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report) is
   begin
      Pauses := Pauses + 1;
      if Remaining = 0 then Result := (Status => ACPI_EC_Clock_Policy.Budget_Exhausted, others => <>);
      else Remaining := Remaining - 1; Now := Target; Result := (ACPI_EC_Clock_Policy.Pause_Returned, Target); end if;
   end Pause;
   function Save return H.IPC.Saved_Reply is
   begin
      Saves := Saves + 1;
      if Mode = "save-refused" then return (False, H.ABI.Invalid_Argument); end if;
      Check (not Saved, "one retained reply"); Saved := True;
      return (True, H.Testing.Reply_For_Test (16#1_0000_0001#));
   end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin Check (Saved and H.Valid (Item), "retained authority answers once"); Saved := False;
      Answers := Answers + 1; Last_Reply := Msg; return Mode /= "answer-lost";
   end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Unsigned_64 is
   begin Check (From = Sender, "full current sender"); Currents := Currents + 1; Last_Reply := Msg; return 1; end Answer_Current;
   procedure Publish (Item : ET.Notice; Result : out ET.Publication) is
   begin
      Check (Item.Controller = 42 and Item.Epoch = 1 and Item.Number = 1, "query identity");
      Publications := Publications + 1; Last_Notice := Item;
      Result := (if Mode in "publication-unknown" | "no-consumer" then ET.Indeterminate
         elsif Mode = "publication-stalled" and Publications = 1 then ET.Backpressure else ET.Enqueued);
   end Publish;
   package Owner is new ACPI_EC_Backend_Owner
     (Read, Write, Owned, Publish, (if Mode = "invalid-policy" then 0 else 1000), 100, 8,
      Maximum_Cells => (if Mode = "too-many" then 1 else 2), Expected_Consumer => (if Mode = "no-consumer" then M.No_Process else Consumer), Clock => Clock, Pause => Pause,
      Save => Save, Answer_Saved => Answer_Saved, Answer_Current => Answer_Current);
   use type Owner.Setup_Status, Owner.Setup_Result, Owner.Call_Status, Owner.Query_Status, Owner.Event_State, Owner.Reply_State;
   use type Owner.Query_Response;
   procedure Reenter;
   procedure Handle_Event (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      Query : Owner.Query_Status;
      Request : Owner.Query_Request_Result;
      use type Owner.Query_Request_Result;
   begin
      Hook_Count := Hook_Count + 1;
      if System_Mode then
         Check (Mode not in "system-stop" | "system-stop-drain", "system stop bypasses callback");
         Stop := False; Handled := True;
         Owner.Request_Query (1, Request);
         if Mode = "system-source" then
            Check (From = Sender and Item.tag.label = 16#101# and Request = Owner.Request_Queued,
               "non-system source retains explicit admission window");
         else
            Check (Request = Owner.Request_Outside_Event, "system hints cannot queue EC queries");
            Owner.Start_Query (1, Query);
            Check (Query = Owner.Query_Receive_Owned, "system hints cannot start EC queries");
         end if;
         return;
      end if;
      if Mode = "hook-exception" then raise Program_Error with "modeled event callback fault"; end if;
      if Mode = "hook-reentry" then Reenter; end if;
      Stop := From = M.No_Process and Item.tag.label = 16#100#; Handled := Stop;
      if From = Sender and Item.tag.label = 16#101# then
         Owner.Start_Query (1, Query); Check (Query = Owner.Query_Started, "admitted source through same owner"); Handled := True;
      end if;
   end Handle_Event;
   package Receiver is new Owner.With_Receive (Handle_Event => Handle_Event);
   package Other_Receiver is new Owner.With_Receive;
   use type Receiver.Stop_Reason, Receiver.Drain_Status, Other_Receiver.Stop_Reason;
   Other : Other_Receiver.Report;
   procedure Reenter is
      Nested : Receiver.Report;
   begin Receiver.Turn (Nested); Check (Nested.Reason = Receiver.Reentered, "nested entry stops same owner"); end Reenter;
   Setup : Owner.Setup_Result; Accepted : Boolean;
   Call : Owner.Call_Report; Query : Owner.Query_Status; Work : Owner.Progress_Report;
   Report, Again : Receiver.Report; Drained : Receiver.Drain_Report;
   Initial : Owner.Cell_List (5 .. 5) :=
     [5 => (Register => Owner.Register_ID (Register_Word), Epoch => Owner.Epoch_ID (Epoch_Word), Owner => Sender, Cell => 16#10#,
            Readable => True, others => <>)];
   Request, Event : M.Message := M.NULL_MESSAGE;
   Before_IO, Before_Clocks, Before_Polls, Before_Events, Before_Answers : Natural;
   Before_Saves : Natural;
   procedure Closed_Routes is
   begin
      Before_IO := IOs; Before_Clocks := Clocks; Before_Polls := M.Test.Poll_Count; Before_Events := M.Test.Event_Polls;
      Owner.Begin_Call (Sender, Request, Call); Check (Call.Status = Owner.Not_Ready, "closed direct call");
      Owner.Start_Query (1, Query); Check (Query = Owner.Query_Not_Ready, "closed query");
      Owner.Advance (Work); Receiver.Turn (Report); Receiver.Serve (Report); Receiver.Drain (Drained);
      Owner.Install (Initial (5), Accepted); Check (not Accepted, "closed install");
      Owner.Revoke (Owner.Register_ID (Register_Word), Owner.Epoch_ID (Epoch_Word), Accepted); Check (not Accepted, "closed revoke");
      Check (IOs = Before_IO and Clocks = Before_Clocks and Pauses = 0 and Saves = 0 and Answers = 0
         and Currents = 0 and M.Test.Poll_Count = Before_Polls and M.Test.Event_Polls = Before_Events
         and M.Test.Wait_Count = 0 and Report.Reason = Receiver.Not_Started
         and Drained.Status = Receiver.Drain_Not_Ready, "unconfigured routes invoke no callbacks or queues");
   end Closed_Routes;
   procedure Run_System is
      package CV renames CuBit.Control_Events;
      Publisher : M.Process_ID := M.No_Process;
      Before_Stop_IO : Natural := 0;
      Expected_Hooks : Natural := 0;
   begin
      Event.tag.label := CV.Control_Label; Event.tag.length := CV.Control_Words;
      Event.words := [CV.Control_Kind'Enum_Rep (CV.Stop), M.To_Word (Sender), 0, 0];
      if Mode = "system-stop-drain" then
         M.Enqueue (Sender, Request);
         for Step in 1 .. 3 loop Receiver.Turn (Report); end loop;
         Check (Saved and IOs = 2, "real stop after accepted command prefix");
         Before_Stop_IO := IOs;
      elsif Mode = "system-forged" then Publisher := Sender;
      elsif Mode = "system-flags" then Event.tag.ipc.Accepts_Capability := True;
      elsif Mode = "system-bad-length" then Event.tag.length := 1;
      elsif Mode = "system-interrupt" then Event.words (0) := CV.Control_Kind'Enum_Rep (CV.Interrupt); Expected_Hooks := 1;
      elsif Mode = "system-reload" then Event.words (0) := CV.Control_Kind'Enum_Rep (CV.Reload); Expected_Hooks := 1;
      elsif Mode in "system-wake" | "system-bad-wake" then
         Event.tag.label := Channel_Wake_ABI.Wake_Label; Event.tag.length := 1;
         Event.words := [CuBit.Grant_References.Encode ((slot => 7, generation => 9)), 0, 0, 0];
         if Mode = "system-bad-wake" then Event.words (0) := 0; else Expected_Hooks := 1; end if;
      elsif Mode in "system-grant" | "system-bad-grant" then
         Event.tag.label := CV.Grant_Revoked_Label; Event.tag.length := CV.Grant_Event_Words;
         Event.words := [7, (if Mode = "system-bad-grant" then 0 else 9), M.To_Word (Sender), 0];
         if Mode = "system-grant" then Expected_Hooks := 1; end if;
      elsif Mode = "system-source" then
         Publisher := Sender; Event.tag.label := 16#101#; Event.tag.length := 0; Expected_Hooks := 1;
      end if;
      if Mode not in "system-stop-drain" | "system-source" then M.Enqueue (Sender, Request); end if;
      M.Enqueue_Event (Publisher, Event); Receiver.Turn (Report);
      Check (Report.Event_Seen and Report.Event_Handled and Hook_Count = Expected_Hooks,
         "reserved system routing and source hook count");
      if Mode in "system-stop" | "system-stop-drain" then
         Check (Report.Reason = Receiver.Event_Stop and not Report.Request_Seen and IOs = Before_Stop_IO,
            "authentic kernel stop precedes all new progress/request receive");
         Before_Polls := M.Test.Poll_Count; Before_Events := M.Test.Event_Polls;
         if Mode = "system-stop-drain" then
            for Step in 1 .. 64 loop Receiver.Drain (Drained); exit when not Drained.State.State.Field_Reply_Pending; end loop;
            Check (Drained.Status = Receiver.Advanced and Answers = 1 and not Saved and not Owner.Observe.Hardware_Recovery,
               "kernel stop retains and drains same accepted exchange");
         else Check (Saves = 0 and Clocks = 0, "kernel stop before submission has no clock/save callback");
         end if;
         Before_IO := IOs; Before_Answers := Answers;
         Receiver.Turn (Again);
         Check (Again.Reason = Receiver.Event_Stop and IOs = Before_IO and Answers = Before_Answers
            and M.Test.Poll_Count = Before_Polls and M.Test.Event_Polls = Before_Events,
            "kernel stop/drain never resumes queues or replays replies");
      elsif Mode = "system-source" then
         Check (Report.Reason = Receiver.Serving and Owner.Observe.Last_Queued_Query_Status = Owner.Query_Started
            and not Report.Request_Seen and IOs = 0, "admitted source starts bounded queued query only");
      else
         Check (Report.Reason = Receiver.Serving and Report.Field_Attempted and Saves = 1 and IOs = 0,
            "non-stop or rejected system event permits ordinary request admission");
         Check (Owner.Observe.Event_Phase = Owner.Event_Ready and not Owner.Observe.Notice.Present,
            "system event never submits hardware query");
      end if;
   end Run_System;
   procedure Run_Observation is
      Before_Clocks, Before_IO : Natural;
   begin
      Check (Owner.Observe.Last_Query_Response = Owner.No_Query_Response and Owner.Observe.Last_Query_Event = 0,
         "no invented query result at setup");
      Owner.Start_Query (1, Query); Check (Query = Owner.Query_Started, "accepted observation query");
      Check (Owner.Observe.Last_Query_Response = Owner.No_Query_Response and Owner.Observe.Last_Query_Event = 0,
         "in-flight query has no response");
      for Step in 1 .. 64 loop
         Owner.Advance (Work);
         exit when Work.State.Event_Phase not in Owner.Event_Querying | Owner.Event_Publication_Pending;
      end loop;
      Check (Owner.Observe.Event_Phase = Owner.Event_Ready and not Owner.Observe.Notice.Present,
         "ready without retention has several meanings");
      if Mode = "observe-empty" then
         Check (Owner.Observe.Last_Query_Response = Owner.No_Pending_Event
            and Owner.Observe.Last_Query_Outcome = T.No_Event and Owner.Observe.Last_Query_Event = 0
            and Query_Commands = 0 and Publications = 0, "clear SCI is not a QR_EC response");
      elsif Mode = "observe-zero" then
         Check (Owner.Observe.Last_Query_Response = Owner.Zero_Query_Response
            and Owner.Observe.Last_Query_Outcome = T.Succeeded and Owner.Observe.Last_Query_Event = 0
            and Query_Commands = 1 and Publications = 0, "zero query response preserves successful hardware status");
      else
         Check (Owner.Observe.Last_Query_Response = Owner.Event_Response
            and Owner.Observe.Last_Query_Outcome = T.Succeeded and Owner.Observe.Last_Query_Event = 1
            and Query_Commands = 1 and Publications = 1, "event response survives successful enqueue");
      end if;
      Before_Clocks := Clocks; Before_IO := IOs;
      for Step in 1 .. 4 loop
         Check (Owner.Observe.Event_Phase = Owner.Event_Ready, "read-only query observation");
      end loop;
      Check (Clocks = Before_Clocks and IOs = Before_IO, "observation never samples hardware or time");
      if Mode = "observe-refused" then
         Owner.Begin_Call (Sender, Request, Call); Check (Call.Status = Owner.Admitted, "Field occupies catalog after published query");
         Before_IO := IOs;
         Owner.Start_Query (1, Query); Check (Query = Owner.Query_Busy, "busy catalog refuses new query");
         Check (Owner.Observe.Last_Query_Response = Owner.Event_Response and Owner.Observe.Last_Query_Event = 1
            and Owner.Observe.Last_Query_Outcome = T.Succeeded and IOs = Before_IO,
            "refused query cannot erase completed response history");
      end if;
      if Mode = "observe-reset" then
         Owner.Start_Query (1, Query); Check (Query = Owner.Query_Started, "new accepted query");
         Check (Owner.Observe.Last_Query_Response = Owner.No_Query_Response and Owner.Observe.Last_Query_Event = 0,
            "new acceptance clears old response before hardware");
         Before_IO := IOs;
         Owner.Start_Query (1, Query); Check (Query = Owner.Query_Busy, "busy query refuses replacement");
         Check (Owner.Observe.Last_Query_Response = Owner.No_Query_Response and Owner.Observe.Last_Query_Event = 0
            and IOs = Before_IO, "refused query preserves accepted in-flight result state");
      end if;
   end Run_Observation;
begin
   M.Reset; Request.tag.label := W.Read_Operation; Request.tag.length := 4; Request.words := [Epoch_Word,Register_Word,0,0];
   Closed_Routes;
   if Mode in "direct-overflow" then Now := T.Microseconds'Last - 10; end if;
   if Mode = "bad-cell" then Initial (5).Owner := M.No_Process; end if;
   if Mode in "partial-setup" | "too-many" then
      declare Cells : constant Owner.Cell_List (7 .. 8) := [7 => Initial (5), 8 => Initial (5)]; begin
         Owner.Configure (Cells, 42, 1, Setup);
      end;
   elsif Mode = "query-only" then Owner.Configure (Owner.Cell_List'(1 .. 0 => <>), 42, 1, Setup);
   else Owner.Configure (Initial, (if Mode = "invalid-route" then 0 else 42), 1, Setup); end if;
   if Mode in "invalid-policy" | "invalid-route" | "bad-cell" | "partial-setup" | "too-many" then
      Check (Owner.Configuration = Owner.Setup_Refused and Setup /= Owner.Configured, "failed setup latched");
      Closed_Routes;
      Owner.Configure (Initial, 42, 1, Setup); Check (Setup = Owner.Already_Attempted, "failed setup cannot reset");
   else
      Check (Setup = Owner.Configured and Owner.Configuration = Owner.Ready and IOs = 0 and Clocks = 0, "configured without hardware/clock");
      Owner.Configure (Owner.Cell_List'(1 .. 0 => <>), 0, 0, Setup);
      Check (Setup = Owner.Already_Attempted and Owner.Configuration = Owner.Ready, "reconfigure cannot replace state");
      if System_Mode then
         Run_System; Ada.Text_IO.Put_Line ("EC owner checks" & Checks'Image); return;
      end if;
      if Mode in "observe-empty" | "observe-zero" | "observe-event" | "observe-reset" | "observe-refused" then
         Run_Observation; Ada.Text_IO.Put_Line ("EC owner checks" & Checks'Image); return;
      end if;
      if Mode in "direct" | "direct-query" | "direct-unknown" | "direct-clock" | "direct-overflow" then
         if Mode = "direct-query" then
            Owner.Start_Query (1, Query); Check (Query = Owner.Query_Started, "standalone query starts");
            Owner.Start_Query (1, Query); Check (Query = Owner.Query_Busy, "busy query does not replace accepted one");
         else
            Owner.Begin_Call (Sender, Request, Call);
            if Mode in "direct-clock" | "direct-overflow" then
               Check (Call.Status = (if Mode = "direct-clock" then Owner.Clock_Unavailable else Owner.Deadline_Overflow)
                  and Currents = 1 and IOs = 0 and Saves = 0, "standalone typed clock/deadline refusal");
            else
               Check (Call.Status = Owner.Admitted and Saved, "standalone Field accepted");
               Before_Saves := Saves;
               Owner.Begin_Call (Sender, Request, Call);
               Check (Call.Status = Owner.Refused and Saves = Before_Saves and Saved and Currents = 1, "busy standalone caller preserves saved owner");
               Owner.Revoke (Owner.Register_ID (Register_Word), Owner.Epoch_ID (Epoch_Word), Accepted); Check (not Accepted, "active standalone record retained");
            end if;
         end if;
         if Mode not in "direct-clock" | "direct-overflow" then
            for Step in 1 .. 64 loop
               Before_IO := IOs; Owner.Advance (Work);
               Check (Work.Advanced and IOs <= Before_IO + 1, "bounded standalone owner progress");
               exit when not Work.State.Field_Reply_Pending and not Work.State.Catalog_Active
                  and Work.State.Event_Phase not in Owner.Event_Querying | Owner.Event_Publication_Pending;
               exit when Work.State.Hardware_Recovery and not Work.State.Field_Reply_Pending;
            end loop;
            if Mode = "direct-query" then Check (Publications = 1 and Answers = 0 and not Owner.Observe.Notice.Present, "standalone query completes");
            else Check (Answers = 1 and not Saved, "standalone reply once");
               if Mode = "direct-unknown" then
                  Check (Owner.Observe.Hardware_Recovery, "standalone recovery retained");
                  Owner.Revoke (Owner.Register_ID (Register_Word), Owner.Epoch_ID (Epoch_Word), Accepted); Check (not Accepted, "retained standalone recovery cannot revoke");
                  Initial (5).Epoch := Owner.Epoch_ID (Epoch_Word + 1); Owner.Install (Initial (5), Accepted); Check (not Accepted, "retained standalone recovery cannot replace");
               else
                  Owner.Revoke (Owner.Register_ID (Register_Word), Owner.Epoch_ID (Epoch_Word), Accepted); Check (Accepted, "completed standalone record can revoke");
                  Owner.Install (Initial (5), Accepted); Check (not Accepted, "same retired epoch cannot return");
                  Initial (5).Epoch := Owner.Epoch_ID (Epoch_Word + 1); Owner.Install (Initial (5), Accepted); Check (Accepted, "new admitted epoch installs");
                  Before_IO := IOs; Owner.Begin_Call (Sender, Request, Call);
                  Check (Call.Status = Owner.Refused and Last_Reply.words (0) = W.Outcome'Pos (W.Denied)
                     and Answers = 2 and IOs = Before_IO, "old epoch denied after re-admission");
               end if;
            end if;
         end if;
         Check (not Owner.Observe.Receive_Owned and M.Test.Poll_Count = 0 and M.Test.Event_Polls = 0 and M.Test.Wait_Count = 0, "standalone owner never receives queues");
      else
      if Mode = "handoff" then
         Owner.Begin_Call (Sender, Request, Call); Check (Call.Status = Owner.Admitted and Saved and IOs = 0, "accepted direct call handed to receiver");
      elsif Mode in "query" | "publication-unknown" | "publication-stalled" | "query-only" | "no-consumer" then
         Event.tag.label := 16#101#; M.Enqueue_Event (Sender, Event);
      else
         if Mode = "wrong-owner" then
            M.Enqueue (M.From_Word (16#1234_1234_0000_0001#), Request);
         else
            if Mode = "stale" then Request.words (0) := Epoch_Word + 1; end if;
            M.Enqueue (Sender, Request);
         end if;
      end if;
      if Mode in "hook-exception" | "hook-reentry" then
         Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event);
      end if;
      if Mode = "serve" then Receiver.Serve (Report); else
      for Turn in 1 .. 64 loop
         Before_IO := IOs; Receiver.Turn (Report); Check (IOs <= Before_IO + 1, "one hardware access per turn");
         exit when Report.Reason /= Receiver.Serving;
         if Turn = 1 then
            Before_IO := IOs; Before_Clocks := Clocks; Before_Polls := M.Test.Poll_Count;
            Before_Events := M.Test.Event_Polls; Before_Saves := Saves;
            Other_Receiver.Turn (Other);
            Check (Other.Reason = Other_Receiver.Receive_Already_Owned, "second receiver cannot acquire same parent");
            Owner.Begin_Call (Sender, Request, Call); Owner.Advance (Work); Owner.Start_Query (1, Query);
            Check (Call.Status = Owner.Receive_Owned and not Work.Advanced and Query = Owner.Query_Receive_Owned,
               "direct submit/progress cannot bypass claimed receiver");
            Check (IOs = Before_IO and Clocks = Before_Clocks and Saves = Before_Saves
               and M.Test.Poll_Count = Before_Polls and M.Test.Event_Polls = Before_Events,
               "refused competing routes invoke no callbacks/queues");
         end if;
         if Mode = "stop-drain" and Turn = 3 then
            Check (IOs = 2 and Saved, "stop after command prefix");
            Event.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Event);
         end if;
      end loop;
      end if;
      if Mode in "hook-exception" | "hook-reentry" then
         Check (Report.Reason = (if Mode = "hook-exception" then Receiver.Adapter_Fault else Receiver.Reentered)
            and IOs = 0 and Clocks = 0 and Saves = 0, "hook failure retains closed owner before work");
      elsif Mode = "stop-drain" then
         Check (Report.Reason = Receiver.Event_Stop and Saved, "stopped same retained owner");
         Before_Polls := M.Test.Poll_Count; Before_Events := M.Test.Event_Polls;
         for Step in 1 .. 64 loop
            Receiver.Drain (Drained); exit when not Drained.State.State.Field_Reply_Pending;
         end loop;
         Check (Drained.Status = Receiver.Advanced and Answers = 1 and not Saved and not Drained.State.State.Hardware_Recovery
            and M.Test.Poll_Count = Before_Polls and M.Test.Event_Polls = Before_Events, "same private pair drains without queues");
      else
         Check (Report.Reason = Receiver.Wait_Unavailable, "eligible idle wait ends hosted run");
         if Mode in "query" | "publication-unknown" | "publication-stalled" | "query-only" | "no-consumer" then
            Check (Publications = 1 and Saves = 0 and not Owner.Observe.Catalog_Active, "query completed outside Field replies");
            if Mode = "no-consumer" then
               Owner.Confirm_Claim (Consumer, Last_Notice, Accepted);
               Check (not Accepted and Owner.Observe.Notice.Present, "no consumer never accepts a fabricated claim");
               Owner.Resume_Publication (1, Accepted); Check (not Accepted, "unknown delivery cannot be retried");
            elsif Mode = "publication-unknown" then
               Check (Owner.Observe.Notice.Present and Owner.Observe.Event_Phase = Owner.Event_Delivery_Unknown, "uncertain notice retained");
               Owner.Confirm_Claim (Sender, Last_Notice, Accepted); Check (not Accepted, "wrong consumer rejected");
               Owner.Confirm_Claim (Consumer, Last_Notice, Accepted); Check (Accepted and not Owner.Observe.Notice.Present, "exact claim clears only notice");
               Check (Owner.Observe.Last_Query_Response = Owner.Event_Response and Owner.Observe.Last_Query_Event = 1,
                  "exact claim does not erase query response history");
            elsif Mode = "publication-stalled" then
               Owner.Resume_Publication (1, Accepted); Check (Accepted, "trusted bounded resume");
               Before_IO := IOs; Receiver.Drain (Drained);
               Check (Publications = 2 and IOs = Before_IO and not Drained.State.State.Notice.Present, "publication retry performs no new query");
            end if;
         elsif Mode in "wrong-owner" | "stale" then
            Check (IOs = 0 and Saves = 1 and Answers = 1 and Last_Reply.words (0) = W.Outcome'Pos (W.Denied), "owner/epoch denied before hardware");
         elsif Mode in "clock" | "save-refused" then
            Check (IOs = 0 and Answers = 0 and Currents = 1 and Last_Reply.words (0) = W.Outcome'Pos (W.Denied), "admission refusal no hardware");
         elsif Mode = "not-owned" then
            Check (Answers = 1 and IOs = 0 and Last_Reply.words (0) = W.Outcome'Pos (W.Denied), "ownership loss before hardware");
         elsif Mode = "unknown" then
            Check (Owner.Observe.Hardware_Recovery and Answers = 1 and not Saved, "uncertain exchange retained independently");
            Owner.Revoke (Owner.Register_ID (Register_Word), Owner.Epoch_ID (Epoch_Word), Accepted); Check (not Accepted, "recovery entry remains owned");
            Initial (5).Epoch := Owner.Epoch_ID (Epoch_Word + 1); Owner.Install (Initial (5), Accepted); Check (not Accepted, "recovery record cannot be replaced");
         else Check (Answers = 1 and not Saved and Last_Reply.words (0) = W.Outcome'Pos (W.Done)
            and Last_Reply.words (1) = 16#A5#, "Field completion through private same-instance composition"); end if;
      end if;
      Before_IO := IOs; Before_Answers := Answers; Before_Clocks := Clocks; Before_Saves := Saves;
      Owner.Begin_Call (Sender, Request, Call); Owner.Start_Query (1, Query); Owner.Advance (Work);
      Check (Call.Status = Owner.Serving_Stopped and Query = Owner.Query_Serving_Stopped and not Work.Advanced
         and IOs = Before_IO and Clocks = Before_Clocks and Saves = Before_Saves,
         "public parent cannot restart stopped serving");
      if Mode in "normal" | "serve" | "handoff" | "stop-drain" | "answer-lost" | "not-owned" | "unknown" then
         Check (Owner.Observe.Last_Field_Completion.Present
            and Owner.Observe.Last_Field_Completion.Hardware = (if Mode = "not-owned" then T.Ownership_Lost
               elsif Mode = "unknown" then T.Access_Unknown else T.Succeeded),
            "attributable completion diagnostics survive engine retirement and serving stop");
         Check (Owner.Observe.Last_Field_Completion.Delivered = (Mode /= "answer-lost"), "lost reply fact remains visible");
      end if;
      if Mode = "clock" then
         Check (Owner.Observe.Last_Field_Admission.Status = Owner.Clock_Unavailable
            and Owner.Observe.Last_Field_Admission.Replied, "clock admission diagnosis survives idle stop and competing direct call");
      elsif Mode = "save-refused" then
         Check (Owner.Observe.Last_Field_Admission.Phase = Owner.Save_Refused
            and Owner.Observe.Last_Field_Admission.Replied, "save refusal diagnosis retained");
      end if;
      Receiver.Turn (Again);
      Check (IOs = Before_IO and Answers = Before_Answers and Again.Reason = Report.Reason, "serving never reopens or repeats replies");
      end if;
   end if;
   Ada.Text_IO.Put_Line ("EC owner checks" & Checks'Image);
end Owner_Tests;
