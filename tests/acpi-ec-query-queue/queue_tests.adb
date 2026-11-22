pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; with CuBit.Handles; with CuBit.Handles.Testing; with CuBit.Handles.IPC;
with ACPI_EC_Types; with ACPI_EC_Event_Types; with ACPI_EC_Clock_Policy;
with ACPI_EC_Backend_Owner; with ACPI_EC_Backend_Owner.With_Receive; with ACPI_Region_Protocol;
with ACPI_GPE_Lifecycle;
procedure Queue_Tests is
   package T renames ACPI_EC_Types; package ET renames ACPI_EC_Event_Types;
   package M renames CuBit.Messages; package H renames CuBit.Handles; package W renames ACPI_Region_Protocol;
   use type T.Byte, T.Read_Register, T.Write_Register, T.Microseconds, T.Work_Budget;
   use type M.Process_ID, ET.Controller_ID, ET.Epoch_ID, ET.Event_Number, ET.Token_ID;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Scan_Mode : constant Boolean := Mode'Length >= 5 and then Mode (Mode'First .. Mode'First + 4) = "scan-";
   GPE_Mode : constant Boolean := Mode'Length >= 9 and then Mode (Mode'First .. Mode'First + 8) = "scan-gpe-";
   GPE_Masked : Boolean := False;
   Sender : constant M.Process_ID := M.From_Word (16#ABCD_1234_0000_0001#);
   Consumer : constant M.Process_ID := M.From_Word (16#DCBA_4321_0000_0002#);
   Register_Word : constant Unsigned_64 := 16#FEDC_9876_0000_0011#;
   Checks, IOs, Commands, Query_Commands, Saves, Answers, Currents, Waits, Publications : Natural := 0;
   Available_Events, Returned_Events : Natural := 0;
   Now : T.Microseconds := 1; Last_Write : T.Microseconds := 0;
   Clock_OK, Input_Pending, Output_Ready, Saved : Boolean := False;
   Command : T.Byte := 0; Stage : Natural := 0;
   Trace : array (1 .. 8) of T.Byte := [others => 0];
   Published : array (1 .. 8) of ET.Notice;
   Enqueue_Publication : Boolean := False;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Mode & ": " & Why; end if; end Check;
   procedure Clock (Value : out T.Microseconds; Available : out Boolean) is
   begin Value := Now; Available := Clock_OK; end Clock;
   function Owned return Boolean is (Mode not in "ownership" | "scan-ownership");
   procedure Read (Register : T.Read_Register; Value : out T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed; Value := 0;
      if Register = T.Status_Register then
         if Input_Pending then
            Check (Now >= Last_Write + T.Input_Assertion_Delay, "fresh postwrite delay"); Input_Pending := False;
            if (Command = T.Query_Command and Stage = 1) or else Stage = 2 then Output_Ready := True; end if;
         end if;
         if Output_Ready then Value := Value or T.Output_Full; end if;
         if Available_Events > 0 and Mode /= "spurious" then Value := Value or T.Event_Pending; end if;
      else
         Check (Output_Ready, "output ready before read"); Output_Ready := False; Stage := 0;
         if Command = T.Query_Command then
            Check (Available_Events > 0, "query result corresponds to pending source");
            Available_Events := Available_Events - 1; Returned_Events := Returned_Events + 1;
            Value := (if Mode = "scan-zero" then 0 else T.Byte (Returned_Events));
         else Value := 16#A5#; end if;
      end if;
   end Read;
   procedure Write (Register : T.Write_Register; Value : T.Byte; Result : out T.Access_Result) is
   begin
      IOs := IOs + 1; Result := T.Completed;
      Check (not Input_Pending and not Output_Ready, "serialized EC exchange");
      Input_Pending := True; Last_Write := Now;
      if Register = T.Command_Register then
         Check (Stage = 0 and Value in T.Read_Command | T.Query_Command, "new command starts idle exchange");
         Command := Value; Stage := 1; Commands := Commands + 1; Trace (Commands) := Value;
         if Value = T.Query_Command then
            Query_Commands := Query_Commands + 1;
            if Mode in "unknown" | "scan-unknown" | "scan-gpe-unknown" then Result := T.Indeterminate; end if;
         end if;
      else Check (Command = T.Read_Command and Stage = 1 and Value = 16#10#, "admitted Field address"); Stage := 2; end if;
   end Write;
   procedure Pause (Target : T.Microseconds; Remaining : in out T.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report) is
   begin
      if Remaining = 0 then Result := (Status => ACPI_EC_Clock_Policy.Budget_Exhausted, others => <>);
      else Remaining := Remaining - 1; Now := Target; Result := (ACPI_EC_Clock_Policy.Pause_Returned, Target); end if;
   end Pause;
   function Save return H.IPC.Saved_Reply is
   begin Check (not Saved, "one saved Field reply"); Saved := True; Saves := Saves + 1;
      return (True, H.Testing.Reply_For_Test (2 ** 32 + Unsigned_64 (Saves))); end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin Check (Saved and H.Valid (Item) and Msg.words (0) = W.Outcome'Pos (W.Done)
      and Msg.words (1) = 16#A5#, "Field reply completes without busy denial");
      Saved := False; Answers := Answers + 1; return True; end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Unsigned_64 is
   begin Check (From = Sender and Msg.tag.label = W.Operation_Reply, "current denial attribution"); Currents := Currents + 1; return 1; end Answer_Current;
   procedure Publish (Item : ET.Notice; Result : out ET.Publication) is
   begin
      Publications := Publications + 1; Published (Publications) := Item;
      Check (Item.Controller = 42 and Item.Epoch = 1, "notice routing");
      Result := (if Mode in "publication-unknown" | "scan-unknown-publication" | "scan-gpe-publication" and Publications = 1 then ET.Indeterminate
         elsif Mode in "budgets" | "scan-budgets" and not Enqueue_Publication then ET.Backpressure else ET.Enqueued);
   end Publish;
   package Owner is new ACPI_EC_Backend_Owner
     (Read, Write, Owned, Publish, 1000, 100, 16,
      Maximum_Cells => 1, Maximum_Query_Requests => 2, Expected_Consumer => Consumer,
      Clock => Clock, Pause => Pause, Save => Save, Answer_Saved => Answer_Saved, Answer_Current => Answer_Current);
   use type Owner.Setup_Status, Owner.Query_Request_Result, Owner.Query_Request_Count, Owner.Query_Status, Owner.Event_State;
   use type Owner.Query_Scan_Phase, Owner.Query_Response;
   package G renames ACPI_GPE_Lifecycle;
   use type G.Action_Result, G.Phase, G.Source_State;
   type GPE_ID is new Unsigned_64;
   GPE_Source : constant GPE_ID := 16#FACE_5678_0000_0001#;
   GPE_Masks, GPE_Clears, GPE_Enables, GPE_Invocations : Natural := 0;
   procedure Mask_Source (Source : GPE_ID; Result : out G.IO_Outcome) is
   begin Check (Source = GPE_Source, "independently admitted GPE source"); GPE_Masked := True;
      GPE_Masks := GPE_Masks + 1; Result := G.Completed; end Mask_Source;
   procedure Clear_Source (Source : GPE_ID; Result : out G.IO_Outcome) is
   begin Check (Source = GPE_Source and GPE_Masked, "clear exact still-masked source");
      GPE_Clears := GPE_Clears + 1; Result := G.Completed; end Clear_Source;
   procedure Enable_Source (Source : GPE_ID; Result : out G.IO_Outcome) is
   begin
      Check (Source = GPE_Source and GPE_Masked and Owner.Observe.Queued_Queries = 0
         and Owner.Observe.Scan.Last_Empty_Response = Owner.No_Pending_Event
         and not Owner.Observe.Notice.Present and not Owner.Observe.Hardware_Recovery,
         "source rearm follows attributable scan completion only");
      GPE_Masked := False; GPE_Enables := GPE_Enables + 1; Result := G.Completed;
   end Enable_Source;
   procedure Synchronous_Handler (Source : GPE_ID; Budget : Natural; Result : out G.Handler_Outcome; Charged : out Natural) is
      pragma Unreferenced (Source, Budget);
   begin GPE_Invocations := GPE_Invocations + 1; Result := G.Handler_Failed; Charged := 0; end Synchronous_Handler;
   package GPE is new G.Driver (GPE_ID, 0, Mask_Source, Clear_Source, Enable_Source, Synchronous_Handler);
   procedure Event (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      Queued : Owner.Query_Request_Result;
      Direct : Owner.Query_Status;
   begin
      Stop := From = M.No_Process and Item.tag.label = 16#100#; Handled := Stop;
      if From = Sender and Item.tag.label = 16#101# then
         if Scan_Mode then
            Owner.Request_Query_Scan ((if Item.words (0) = 2 then 2 else 1), Owner.Query_Scan_Budget (Item.words (1)), Queued);
         else Owner.Request_Query ((if Item.words (0) = 2 then 2 else 1), Queued); end if;
         Check (Queued = (if Mode = "overflow" and Item.words (1) = 3 then Owner.Request_Overflow else Owner.Request_Queued), "trusted trigger retained or explicit overflow");
         Handled := True;
         if Mode in "clock" | "scan-clock" then Clock_OK := False;
         elsif Mode = "overflow-time" then Now := T.Microseconds'Last - 1; end if;
      end if;
      if From = Sender and Item.tag.label = 16#104# then
         Owner.Start_Query (1, Direct); Check (Direct = Owner.Query_Busy, "direct event query cannot bypass head scan");
         Handled := True;
      end if;
   end Event;
   procedure Control (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
   begin
      Stop := False; Handled := From = Sender and Item.tag.label = 16#102#;
      if Handled then Check (M.reply (From, M.NULL_MESSAGE) /= 0, "trusted control remains replyable"); end if;
   end Control;
   function Idle return Boolean is
   begin Waits := Waits + 1; return True; end Idle;
   package Receiver is new Owner.With_Receive (Handle_Event => Event, Handle_Control => Control, Wait_Idle => Idle);
   use type Receiver.Stop_Reason, Receiver.Drain_Status;
   Report : Receiver.Report; Drain : Receiver.Drain_Report;
   Setup : Owner.Setup_Result; Accepted : Boolean; Requested : Owner.Query_Request_Result;
   Request : M.Message := M.NULL_MESSAGE;
   procedure Trigger (Number : Positive; Attempts : Positive := 1) is
      Item : M.Message := M.NULL_MESSAGE;
   begin
      Available_Events := Available_Events + 1;
      Item.tag.label := 16#101#; Item.words (0) := Unsigned_64 (Attempts); Item.words (1) := Unsigned_64 (Number);
      M.Enqueue_Event (Sender, Item);
   end Trigger;
   procedure Turn is
      Before : constant Natural := IOs;
   begin Receiver.Turn (Report); Check (IOs <= Before + 1, "one IO per receiver turn"); end Turn;
   procedure Run_Scan is
      Item : M.Message := M.NULL_MESSAGE;
      Limit : Owner.Query_Scan_Budget := 4;
      Before, Before_Wait : Natural;
      Source_Run : GPE.Session;
      Source_Action : G.Action_Result;
      procedure Finish is
      begin
         for Step in 1 .. 256 loop Turn; exit when Owner.Observe.Queued_Queries = 0; end loop;
         Check (Owner.Observe.Queued_Queries = 0 and Owner.Observe.Scan.Phase = Owner.No_Head_Scan,
            "scan retires only after empty observation");
      end Finish;
   begin
      if GPE_Mode then
         GPE.Start (Source_Run, GPE_Source, (if Mode = "scan-gpe-edge" then G.Edge_Triggered else G.Level_Triggered), Source_Action);
         Check (Source_Action = G.Accepted and GPE_Masked and GPE_Masks = 1
            and GPE_Clears = (if Mode = "scan-gpe-edge" then 1 else 0), "mask/edge-clear before deferred scan");
         GPE.Begin_Deferred (Source_Run, 4, Source_Action);
         Check (Source_Action = G.Accepted and GPE.Observe (Source_Run).Deferred and GPE_Invocations = 0,
            "deferred source performs no upward or synchronous handler callback");
      end if;
      Owner.Resume_Query_Scan (1, Accepted); Check (not Accepted, "no scan continuation without exhausted head");
      Available_Events := (if Mode = "scan-empty" then 0 elsif Mode in "scan-zero" | "scan-limit" then 1 else 3);
      if Mode in "scan-limit" | "scan-resume" | "scan-followers" | "scan-gpe-exhaust" then Limit := 1;
      elsif Mode = "scan-budgets" then Limit := 3; Available_Events := 2; end if;
      Item.tag.label := 16#101#; Item.words (0) := (if Mode = "scan-budgets" then 2 else 1);
      Item.words (1) := Unsigned_64 (Limit); M.Enqueue_Event (Sender, Item); Turn;
      if GPE_Mode then
         GPE.Finish (Source_Run, Source_Action);
         Check (Source_Action = G.Wrong_Phase and GPE_Masked and GPE_Enables = 0,
            "pending scan cannot rearm source");
      end if;
      if Mode = "scan-clock" then
         Check (Report.Reason = Receiver.Local_Work_Stop and IOs = 0 and Owner.Observe.Scan.Phase = Owner.Scan_Failed,
            "failed scan admission retains entry and stops without retry");
      elsif Mode in "scan-unknown" | "scan-ownership" | "scan-gpe-unknown" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Scan.Phase = Owner.Scan_Failed; end loop;
         Check (Owner.Observe.Scan.Phase = Owner.Scan_Failed and Owner.Observe.Queued_Queries = 1,
            "failed accepted scan iteration stays retained");
         Before := IOs; for Step in 1 .. 4 loop Turn; end loop;
         Check (IOs = Before and Publications = 0 and Query_Commands = (if Mode in "scan-unknown" | "scan-gpe-unknown" then 1 else 0),
            "failed scan never repeats query");
         Owner.Resume_Query_Scan (4, Accepted); Check (not Accepted, "failed scan has no budget reset");
         Owner.Resume_Queries (Accepted); Check (not Accepted, "generic query resume cannot bypass failed scan");
      elsif Mode in "scan-stop" | "scan-gpe-stop" then
         for Step in 1 .. 64 loop Turn; exit when Query_Commands = 1; end loop;
         Item := M.NULL_MESSAGE; Item.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Item); Turn;
         Check (Report.Reason = Receiver.Event_Stop, "stop during accepted scan query");
         for Step in 1 .. 128 loop Receiver.Drain (Drain); exit when Drain.State.State.Event_Phase = Owner.Event_Ready; end loop;
         Check (Drain.Status = Receiver.Advanced and Query_Commands = 1 and Publications = 1
            and Owner.Observe.Queued_Queries = 1 and Owner.Observe.Scan.Remaining = 3,
            "stopped drain settles accepted iteration without new scan query");
         Owner.Resume_Query_Scan (1, Accepted); Check (not Accepted, "stopped scan cannot resume");
      elsif Mode in "scan-limit" | "scan-resume" | "scan-followers" | "scan-gpe-exhaust" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Scan.Phase = Owner.Scan_Exhausted; end loop;
         Check (Owner.Observe.Scan.Phase = Owner.Scan_Exhausted and Owner.Observe.Queued_Queries = 1
            and Publications = 1 and Query_Commands = 1, "budget exhaustion retains unfinished scan");
         Before := IOs; Before_Wait := Waits; for Step in 1 .. 4 loop Turn; end loop;
         Check (IOs = Before and Waits > Before_Wait, "exhausted scan idles without polling hardware");
         Item := M.NULL_MESSAGE; Item.tag.label := 16#104#; M.Enqueue_Event (Sender, Item); Turn;
         Check (IOs = Before, "head scan blocks direct query bypass");
         if Mode = "scan-followers" then
            Item.tag.label := 16#101#; Item.words (1) := 2; M.Enqueue_Event (Sender, Item); Turn;
            Check (Owner.Observe.Queued_Queries = 2 and Query_Commands = 1, "later scan remains behind exhausted head");
         end if;
         Item.tag.label := 16#102#; M.Enqueue (Sender, Item); Turn;
         Check (M.Test.Reply_Count = 1 and IOs = Before, "exhaustion keeps control reachable");
         M.Enqueue (Sender, Request);
         for Step in 1 .. 128 loop Turn; exit when Answers = 1; end loop;
         Check (Answers = 1 and Query_Commands = 1 and Currents = 0, "exhaustion permits ordinary Field completion");
         if GPE_Mode then
            GPE.Finish (Source_Run, Source_Action);
            Check (Source_Action = G.Wrong_Phase and GPE_Masked and GPE_Enables = 0
               and GPE.Observe (Source_Run).Handler_Budget = 4, "exhaustion retains original masked-source handler budget");
         end if;
         Owner.Resume_Query_Scan ((if Mode = "scan-gpe-exhaust" then 3 else 4), Accepted); Check (Accepted, "explicit new scan allowance");
         Owner.Resume_Query_Scan (4, Accepted); Check (not Accepted, "allowance cannot reset unspent episode");
         Finish;
         Check (Query_Commands = (if Mode = "scan-limit" then 1 else 3) and Owner.Observe.Scan.Last_Empty_Response = Owner.No_Pending_Event,
            "continuation queries distinct events then observes clear SCI");
      elsif Mode in "scan-unknown-publication" | "scan-gpe-publication" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Delivery_Unknown; end loop;
         Check (Owner.Observe.Scan.Phase = Owner.Scan_Waiting and Owner.Observe.Notice.Present,
            "scan retains unknown publication");
         Before := IOs; for Step in 1 .. 4 loop Turn; end loop;
         Check (IOs = Before and Query_Commands = 1 and Publications = 1, "unknown publication prevents automatic next query");
         if GPE_Mode then Check (GPE_Masked and GPE_Enables = 0 and GPE.Observe (Source_Run).Deferred, "unknown publication retains source session"); end if;
         Owner.Resume_Query_Scan (4, Accepted); Check (not Accepted, "publication retention cannot be reset as scan allowance");
         Owner.Confirm_Claim (Sender, Published (1), Accepted); Check (not Accepted, "foreign scan claim refused");
         Owner.Confirm_Claim (Consumer, Published (1), Accepted); Check (Accepted, "exact scan notice claimed");
         Finish; Check (Query_Commands = 3 and Publications = 3, "claim enables distinct subsequent scan queries");
      elsif Mode = "scan-budgets" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Delivery_Stalled; end loop;
         Check (Publications = 2 and Query_Commands = 1, "first iteration original publication budget");
         Enqueue_Publication := True; Owner.Resume_Publication (1, Accepted); Check (Accepted, "explicit first notice retry episode");
         for Step in 1 .. 128 loop Turn; exit when Publications = 3; end loop;
         Enqueue_Publication := False;
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Delivery_Stalled; end loop;
         Check (Publications = 5 and Query_Commands = 2 and Published (1).Token = Published (3).Token
            and Published (4).Token = Published (5).Token and Published (1).Token /= Published (4).Token,
            "new iteration retains original budget independently of retry episode");
         Enqueue_Publication := True; Owner.Resume_Publication (1, Accepted); Check (Accepted, "explicit second notice retry");
         Finish; Check (Publications = 6 and Query_Commands = 2, "publication retry never reissues query");
      else
         if Mode in "scan-fair" | "scan-gpe-level" then
            for Step in 1 .. 64 loop Turn; exit when Query_Commands = 1; end loop;
            M.Enqueue (Sender, Request);
         end if;
         Finish;
         Check (Query_Commands = (if Mode = "scan-empty" then 0 elsif Mode = "scan-zero" then 1 else 3)
            and Publications = (if Mode in "scan-empty" | "scan-zero" then 0 else 3), "scan drains available event responses only");
         Check (Owner.Observe.Scan.Last_Empty_Response = (if Mode = "scan-zero" then Owner.Zero_Query_Response else Owner.No_Pending_Event),
            "empty scan evidence preserves response kind");
         if Mode in "scan-fair" | "scan-gpe-level" then
            Check (Answers = 1 and Currents = 0 and Trace (1 .. 4) = [T.Query_Command,T.Read_Command,T.Query_Command,T.Query_Command],
               "Field gets bounded opportunity between scan queries");
         end if;
         if Mode = "scan-wrap" then
            for Round in 1 .. 2 loop
               Available_Events := 1; Item.words (1) := 2; M.Enqueue_Event (Sender, Item); Turn; Finish;
            end loop;
            Check (Query_Commands = 5 and Publications = 5 and Published (5).Number = 5,
               "scan FIFO slots wrap with fresh in-flight and budget state");
         end if;
      end if;
      if GPE_Mode then
         if Mode in "scan-gpe-unknown" | "scan-gpe-stop" then
            GPE.Complete_Deferred (Source_Run, G.Handler_Completion_Unknown, 1, Source_Action);
            Check (Source_Action = G.Accepted and GPE.Observe (Source_Run).Stage = G.Failed
               and GPE.Observe (Source_Run).Source = G.Known_Masked, "uncertain scan never authorizes source rearm");
            GPE.Finish (Source_Run, Source_Action); Check (Source_Action = G.Wrong_Phase and GPE_Enables = 0 and GPE_Masked,
               "stopped/uncertain scan keeps source masked");
         else
            GPE.Complete_Deferred (Source_Run, G.Handler_Succeeded, 4, Source_Action);
            Check (Source_Action = G.Accepted and GPE.Observe (Source_Run).Stage = G.Handler_Done and GPE_Masked,
               "scan completion is recorded before source finish");
            GPE.Finish (Source_Run, Source_Action);
            Check (Source_Action = G.Accepted and GPE.Observe (Source_Run).Stage = G.Finished
               and GPE_Clears = 1 and GPE_Enables = 1 and not GPE_Masked, "deferred edge/level completion clears and enables once");
         end if;
         Check (GPE_Invocations = 0, "EC source never uses synchronous upward handler");
      end if;
   end Run_Scan;
   Before_IO, Before_Waits, Before_Commands : Natural;
begin
   M.Reset; Clock_OK := True;
   Owner.Request_Query (1, Requested); Check (Requested = Owner.Request_Not_Ready, "no queue before setup");
   Owner.Request_Query_Scan (1, 1, Requested); Check (Requested = Owner.Request_Not_Ready, "no scan before setup");
   Owner.Resume_Query_Scan (1, Accepted); Check (not Accepted, "no scan allowance before setup");
   Owner.Configure (Owner.Cell_List'[1 => (Register => Owner.Register_ID (Register_Word), Epoch => 1,
      Owner => Sender, Cell => 16#10#, Readable => True, others => <>)], 42, 1, Setup);
   Check (Owner.Configuration = Owner.Ready, "setup");
   Owner.Request_Query (1, Requested);
   Check (Requested = Owner.Request_Outside_Event and Owner.Observe.Queued_Queries = 0, "no queue outside admitted event window");
   Owner.Request_Query_Scan (1, 1, Requested); Check (Requested = Owner.Request_Outside_Event, "no scan outside source admission window");
   Request.tag.label := W.Read_Operation; Request.tag.length := 4; Request.words := [1,Register_Word,0,0];
   if Scan_Mode then Run_Scan;
   elsif Mode = "foreign" then
      declare Item : M.Message := M.NULL_MESSAGE; begin Item.tag.label := 16#101#;
         M.Enqueue_Event (M.From_Word (16#1234_1234_0000_0001#), Item);
      end;
      Turn; Check (IOs = 0 and Owner.Observe.Queued_Queries = 0, "foreign source cannot queue work");
   elsif Mode in "field" | "overflow" | "stop" then
      M.Enqueue (Sender, Request); Turn; Check (Saved and IOs = 0, "Field accepted first");
      Trigger (1); Turn; Trigger (2); Turn;
      Check (Owner.Observe.Queued_Queries = 2 and Query_Commands = 0, "busy Field retains both query triggers");
      if Mode = "overflow" then Trigger (3); Turn;
         Check (Report.Reason = Receiver.Local_Work_Stop and Owner.Observe.Query_Request_Overflow
            and Owner.Observe.Queued_Queries = 2 and Saved, "overflow stops without discarding earlier triggers");
      elsif Mode = "stop" then
         declare Item : M.Message := M.NULL_MESSAGE; begin Item.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Item); end;
         Turn; Check (Report.Reason = Receiver.Event_Stop and Saved, "stop retains pending work");
      end if;
      if Mode in "overflow" | "stop" then
         for Step in 1 .. 64 loop Receiver.Drain (Drain); exit when not Drain.State.State.Field_Reply_Pending; end loop;
         Check (Drain.Status = Receiver.Advanced and Answers = 1 and Query_Commands = 0 and Drain.State.State.Queued_Queries = 2
            and not Drain.State.State.Hardware_Recovery, "drain completes only accepted Field and retains queued requests");
      else
         for Step in 1 .. 128 loop Turn; exit when Query_Commands = 1; end loop;
         M.Enqueue (Sender, Request);
         for Step in 1 .. 128 loop Turn; exit when Answers = 2 and Publications = 2; end loop;
         Check (Answers = 2 and Saves = 2 and Currents = 0 and Query_Commands = 2
            and Trace (1 .. 4) = [T.Read_Command,T.Query_Command,T.Read_Command,T.Query_Command], "Field request waits behind query and gets next bounded opportunity");
         Check (Published (1).Token = 2 and Published (2).Token = 4 and Owner.Observe.Queued_Queries = 0, "shared tokens preserve serial Field/query ordering");
      end if;
   else
      Trigger (1); Turn;
      if Mode not in "clock" | "overflow-time" then Trigger (2, (if Mode = "budgets" then 2 else 1)); Turn; end if;
      if Mode in "clock" | "overflow-time" then
         Check (Report.Reason = Receiver.Local_Work_Stop and IOs = 0 and Owner.Observe.Queued_Queries = 1
            and Owner.Observe.Last_Queued_Query_Status = (if Mode = "clock" then Owner.Query_Clock_Unavailable else Owner.Query_Deadline_Overflow), "failed query admission retains queued request without spinning");
      elsif Mode in "unknown" | "ownership" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Query_Failed; end loop;
         Check (Owner.Observe.Queued_Queries = 1 and Query_Commands = (if Mode = "unknown" then 1 else 0), "failed accepted query not replayed");
         Before_IO := IOs; Before_Waits := Waits; for Step in 1 .. 3 loop Turn; end loop;
         Check (IOs = Before_IO and Waits > Before_Waits and Owner.Observe.Queued_Queries = 1, "blocked queue uses eligible idle wait");
         if Mode = "unknown" then
            declare Item : M.Message := M.NULL_MESSAGE; begin Item.tag.label := 16#102#; M.Enqueue (Sender, Item); end;
            Turn; Check (M.Test.Reply_Count = 1 and IOs = Before_IO, "recovery allows control receive without hardware");
         end if;
         Owner.Resume_Queries (Accepted); Check (Accepted = (Mode = "ownership"), "recovery cannot be reset by resuming queries");
      elsif Mode = "publication-unknown" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Notice.Present; end loop;
         Before_IO := IOs; Before_Waits := Waits; for Step in 1 .. 3 loop Turn; end loop;
         Check (Publications = 1 and Query_Commands = 1 and IOs = Before_IO and Waits > Before_Waits
            and Owner.Observe.Queued_Queries = 1, "unknown publication blocks queue without replay or spin");
         Owner.Confirm_Claim (Consumer, Published (1), Accepted); Check (Accepted, "exact retained claim");
         for Step in 1 .. 128 loop Turn; exit when Publications = 2; end loop;
         Check (Query_Commands = 2 and Published (1).Token /= Published (2).Token, "claim enables next distinct queued query");
      elsif Mode = "budgets" then
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Delivery_Stalled; end loop;
         Check (Publications = 1 and Owner.Observe.Queued_Queries = 1, "first trigger retains one-attempt budget");
         Enqueue_Publication := True; Owner.Resume_Publication (1, Accepted); Check (Accepted, "bounded first notice resume");
         for Step in 1 .. 128 loop Turn; exit when Publications = 2; end loop;
         Enqueue_Publication := False;
         for Step in 1 .. 128 loop Turn; exit when Owner.Observe.Event_Phase = Owner.Event_Delivery_Stalled; end loop;
         Check (Publications = 4 and Query_Commands = 2 and Published (3).Token = Published (4).Token
            and Owner.Observe.Queued_Queries = 0, "second queued trigger keeps its independent two-attempt budget");
      else
         for Step in 1 .. 128 loop Turn; exit when (if Mode = "spurious" then Owner.Observe.Queued_Queries = 0
            and not Owner.Observe.Catalog_Active else Publications = 2); end loop;
         Check (Owner.Observe.Queued_Queries = 0 and Query_Commands = (if Mode = "spurious" then 0 else 2), "each retained trigger consumed once");
         if Mode = "wrap" then
            Trigger (3); for Step in 1 .. 128 loop Turn; exit when Publications = 3; end loop;
            Check (Query_Commands = 3 and Published (3).Number = 3 and Owner.Observe.Queued_Queries = 0,
               "ring slots wrap and remain reusable after complete dequeue");
         end if;
         if Mode = "ordinary" then Check (Published (1).Number = 1 and Published (2).Number = 2, "query result source order"); end if;
      end if;
   end if;
   Before_Commands := Query_Commands;
   if Report.Reason /= Receiver.Serving then for Step in 1 .. 3 loop Turn; end loop;
      Check (Query_Commands = Before_Commands, "stopped queue cannot restart"); end if;
   Check (M.Test.Mixed_Polls = 0, "typed request receive only");
   Ada.Text_IO.Put_Line ("EC queue checks" & Checks'Image);
end Queue_Tests;
