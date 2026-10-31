pragma Ada_2022;
with Ada.Text_IO;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Service_Loop;
with GPIO_Admin_Owner;
with GPIO_Service_Pump;
with GPIO_Startup;
with GPIO_Polling_Loop;
with GPIO_Loop_Time;
with GPIO_Controller_Resources;
with GPIO_Admin_Wire;
with GPIO_Channel_Badges;
with GPIO_Control_Wire;
with Ada.Command_Line;
with GPIO_Lease_Control;
with CuBit.Process_IDs;
with System.Storage_Elements;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with GPIO_Community_Window;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
procedure Admin_Tests is
   package P renames GPIO_Mapping_Policy;
   package T renames GPIO_Community_Types;
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type P.Address, GPIO_Controller_Resources.Pin_Count;
   use type G.Outcome;
   use type G.Plan_Result;
   use type T.Map_Outcome;
   use type T.Header_Outcome;
   use type T.Pin_Admission;
   use type I.Word, I.Tick;
   use type I.Access_Status;
   use type I.Write_ID;
   type Words is array (Natural range 0 .. 2047) of I.Word;
   Memory : aliased Words := [others => 0] with Alignment => 4096;
   Physical : constant P.Address := 16#1234_0000#;
   Virtual : constant P.Address := P.Address (System.Storage_Elements.To_Integer (Memory'Address));
   Case_Name : constant String := Ada.Command_Line.Argument (1);
   Checks, Inspects, Maps, Clears, Accesses : Natural := 0;
   Startup_Held_Only : Boolean := Case_Name in "parked-start" | "parked-uncertain" | "startup-owner" | "startup-uncertain";
   Clock_Available : Boolean := True;
   Clock_Sample : I.Tick := 1; Fault_Pause : Boolean := False;
   procedure Check (OK : Boolean; Why : String) is
   begin if not OK then raise Program_Error with Why; end if; Checks := Checks + 1; end Check;
   procedure Inspect (Authority : Natural; Result : out P.Description) is
   begin
      Check (Authority = 1, "same admitted authority"); Inspects := Inspects + 1;
      Result := (True, True, True, True, Physical, 8192);
   end Inspect;
   procedure Map (Authority : Natural; First, Destination, Pages : P.Address; Mapped : out Boolean) is
   begin
      Check (Authority = 1 and First = Physical and Destination = Virtual and Pages = 2,
        "exact admitted whole-page region"); Maps := Maps + 1; Mapped := True;
   end Map;
   package W is new GPIO_Community_Window (Natural, Inspect, Map);
   procedure Clock (Now : out I.Tick; Available : out Boolean) is
   begin Now := Clock_Sample; Available := Clock_Available; end Clock;
   procedure Read_Register (Plan : G.Plan_Result; ID : I.Read_ID; Value : out I.Word; Status : out I.Access_Status) is
   begin Accesses := Accesses + 1; W.Read (Plan, ID, Value, Status); end Read_Register;
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      Index : constant Natural := (if Plan.Status = G.Ready then Natural (Plan.Selected.Status_Register) / 4 else 0);
      Before : constant I.Word := Memory (Index);
   begin
      if Startup_Held_Only and then ID = I.Write_Enable then
         Check ((Value and Plan.Selected.Interrupt_Mask) = 0, "parked startup never writes selected enable bit");
      end if;
      Accesses := Accesses + 1; W.Write (Plan, ID, Value, Status);
      if Status = I.Access_OK and ID = I.Clear_Status then
         -- The adapter emits actual volatile stores; model ONLY chip W1C here.
         Clears := Clears + 1; Memory (Index) := Before and not Value;
      end if;
      if Status = I.Access_OK and then ID = I.Write_Enable and then Fault_Pause then Status := I.Unknown_Effect; end if;
      if Startup_Held_Only and then Case_Name in "parked-uncertain" | "startup-uncertain" and then ID = I.Clear_Status
      then Status := I.Unknown_Effect; end if;
   end Write;
   package C is new Intel_GPIO_Controller (2, Clock, Read_Register, Write);
   use type C.Pin_ID;
   use type C.Admission;
   use type C.Engine.Admission;
   Plan_A, Plan_B : G.Plan_Result;
   Bind_Report : T.Map_Report;
   Header : T.Header_Report;
   Admission : T.Pin_Admission;
   Registry_Admission : C.Admission;
   Owner_Admission : C.Engine.Admission;
   Pin_A, Pin_B : C.Pin_ID;
   function Index (Offset : G.Register_Offset) return Natural is (Natural (Offset) / 4);
   procedure Finish is
      Steps : Natural := 0;
      Worked : Boolean;
   begin
      while C.Observe.Active /= C.No_Pin loop
         Steps := Steps + 1; Check (Steps <= 32, "bounded integrated controller operation");
         C.Advance (Worked); Check (Worked, "active operation advances");
      end loop;
   end Finish;
   package PID renames CuBit.Process_IDs;
   package L renames GPIO_Lease_Control;
   Lease_Counter : L.Lease_ID := 16#1000_0000_0000_0000#;
   procedure Next_Lease (Lease : out L.Lease_ID; Available : out Boolean) is
      use type L.Lease_ID;
   begin Lease_Counter := Lease_Counter + 1; Lease := Lease_Counter; Available := True; end Next_Lease;
   package Leases is new GPIO_Pin_Leases (C, Clock, Next_Lease);
   package Wire renames GPIO_Control_Wire;
   use type Wire.Completion_Status, Wire.Word, Wire.Control_Kind;
   Reply_Count, Event_Attempts, Events_Accepted : Natural := 0;
   Allow_Answer : constant Boolean := True;
   Allow_Event : Boolean := True;
   Last_Reply : Wire.Completion := (Wire.Arm, Wire.Refused, 0, 0);
   Last_Event : Wire.Event := (1, 1);
   Last_Authority : Natural := 0;
   Sink_Closed : array (Natural range 17 .. 19) of Boolean := [others => False];
   Close_Attempts : array (Natural range 17 .. 19) of Natural := [others => 0];
   Fail_Close : Natural := 0;
   Replies : array (Natural range 0 .. 255) of Wire.Completion := [others => (Wire.Arm, Wire.Refused, 0, 0)];
   Events : array (Natural range 17 .. 19) of Wire.Event := [others => (1, 1)];
   procedure Answer (Authority : Natural; Value : Wire.Completion; Delivered : out Boolean) is
   begin
      Check (Wire.Valid (Value), "valid completion before send");
      Check (Authority in Replies'Range, "known saved reply authority"); Replies (Authority) := Value;
      Last_Authority := Authority; Last_Reply := Value; Reply_Count := Reply_Count + 1; Delivered := Allow_Answer;
   end Answer;
   procedure Try_Event (Sink : Natural; Value : Wire.Event; Accepted : out Boolean) is
   begin
      Check (Sink in Events'Range and then not Sink_Closed (Sink), "only live launcher-installed sink"); Events (Sink) := Value;
      Event_Attempts := Event_Attempts + 1; Last_Event := Value; Accepted := Allow_Event;
      if Accepted then Events_Accepted := Events_Accepted + 1; end if;
   end Try_Event;
   package Channels is new GPIO_Channel_Owner (Leases, 32, Natural, Natural, Answer, Try_Event);
   use type C.Engine.Phase;
   procedure Close_Sink (Sink : Natural; Closed : out Boolean) is
      Pin : constant C.Pin_ID := (if Sink = 18 then 2 else 1);
   begin
      Check (Sink in Sink_Closed'Range and then not Sink_Closed (Sink), "close owned endpoint once");
      Check (C.Observe_Pin (Pin).Report.At_Phase = C.Engine.Held_Masked and then not C.Observe_Pin (Pin).Report.Pin_May_Be_Enabled, "close only after confirmed pin mask");
      Close_Attempts (Sink) := Close_Attempts (Sink) + 1; Closed := Sink /= Fail_Close;
      if Closed then Sink_Closed (Sink) := True; end if;
   end Close_Sink;
   package Service is new GPIO_Service_Loop (Natural, Natural, Channels, 2, Close_Sink);
   package B renames GPIO_Channel_Badges;
   package A renames GPIO_Admin_Wire;
   use type Service.Channel_ID, Service.Phase, PID.Process_ID;
   Launcher : constant PID.Process_ID := PID.From_Word (16#F123_8888_0000_3333#);
   Wrong_Launcher : constant PID.Process_ID := PID.From_Word (16#F123_8889_0000_3333#);
   Owner : constant PID.Process_ID := PID.From_Word (16#F123_4567_0000_1234#);
   Replacement : constant PID.Process_ID := PID.From_Word (16#F123_4569_0000_1234#);
   Badge_A : constant Wire.Word := B.Canonical ((160, B.Level_Triggered, B.Active_Low));
   Take_Calls, Inspect_Calls, Discard_Calls, Saves, Saved_Answers, Current_Answers : Natural := 0;
   Offered : Natural := 17;
   Offered_Owner : PID.Process_ID := Owner;
   Offered_Badge : Wire.Word := Badge_A;
   Take_OK, Inspect_OK, Discard_OK, Save_OK, Deliver_OK : Boolean := True;
   Issued : array (Natural range 17 .. 19) of Boolean := [others => False];
   Consumed : array (Natural range 1001 .. 1064) of Boolean := [others => False];
   Last_Admin_Status : Wire.Completion_Status := Wire.Refused;
   Last_Admin_Label : Wire.Operation := 0;
   function Authorized (Caller : PID.Process_ID) return Boolean is (Caller = Launcher);
   procedure Take_Offer (Caller : PID.Process_ID; Receipt : out Natural; Owned : out Boolean) is
   begin
      Check (Caller = Launcher, "only authenticated launcher may transfer"); Take_Calls := Take_Calls + 1;
      Receipt := Offered; Owned := Take_OK;
      if Owned then Check (not Issued (Receipt), "fresh offered handle"); Issued (Receipt) := True; end if;
   end Take_Offer;
   procedure Inspect_Offer (Receipt : Natural; Sink : out Natural; Sender : out PID.Process_ID; Badge : out Wire.Word; Valid : out Boolean) is
   begin
      Check (Issued (Receipt) and then not Sink_Closed (Receipt), "inspect owned receipt");
      Inspect_Calls := Inspect_Calls + 1; Sink := Receipt; Sender := Offered_Owner; Badge := Offered_Badge; Valid := Inspect_OK;
   end Inspect_Offer;
   procedure Discard (Receipt : Natural; Closed : out Boolean) is
   begin
      Check (Issued (Receipt) and then not Sink_Closed (Receipt), "discard unused owned receipt once");
      Discard_Calls := Discard_Calls + 1; Closed := Discard_OK;
      if Closed then Sink_Closed (Receipt) := True; end if;
   end Discard;
   procedure Save (Reply : out Natural; Saved : out Boolean) is
   begin Saves := Saves + 1; Reply := 1000 + Saves; Saved := Save_OK; end Save;
   procedure Answer_Saved (Reply : Natural; Label : Wire.Operation; Status : Wire.Completion_Status; Delivered : out Boolean) is
   begin
      Check (Reply in Consumed'Range and then not Consumed (Reply), "saved admin authority consumed once even if delivery fails");
      Consumed (Reply) := True; Saved_Answers := Saved_Answers + 1; Last_Admin_Status := Status; Last_Admin_Label := Label; Delivered := Deliver_OK;
   end Answer_Saved;
   procedure Answer_Current (Caller : PID.Process_ID; Label : Wire.Operation; Status : Wire.Completion_Status; Delivered : out Boolean) is
   begin
      Check (PID.Is_Process (Caller), "current inbox caller remains full-width identity");
      Current_Answers := Current_Answers + 1; Last_Admin_Status := Status; Last_Admin_Label := Label; Delivered := Deliver_OK;
   end Answer_Current;
   package Admin is new GPIO_Admin_Owner (Natural, Natural, Natural, Service, Authorized,
      Take_Offer, Inspect_Offer, Discard, Save, Answer_Saved, Answer_Current);
   use type Admin.Phase;
   S : Admin.State;
   OK, Taken : Boolean;
   Event_Queue, Request_Queue : Natural := 0;
   Stop_Event : Boolean := False;
   procedure Poll_Event (State : in out Admin.State; Found : out Boolean) is
   begin
      Found := Event_Queue > 0;
      if Found then
         Event_Queue := Event_Queue - 1;
         if Stop_Event then Service.Request_Stop (1000); Admin.Request_Stop (State); Stop_Event := False; end if;
      end if;
   end Poll_Event;
   procedure Poll_Request (State : in out Admin.State; Found : out Boolean) is
      pragma Unreferenced (State);
   begin
      Found := Request_Queue > 0;
      if Found then Request_Queue := Request_Queue - 1; end if;
   end Poll_Request;
   package Boot is new GPIO_Startup (Service, W.Admit_Pin);
   use type Boot.Phase, Boot.Failure;
   package Pump is new GPIO_Service_Pump (Admin, 3, 2, 1, Poll_Event, Poll_Request);
   Turn : Pump.Turn_Report;
   package Scheduled is new GPIO_Polling_Loop (Pump, Clock, 10, 100, 5);
   use type Scheduled.Clock_Condition, GPIO_Loop_Time.Wait_Kind;
   Schedule : Scheduled.Turn_Report;
   procedure Drive is
   begin
      for N in 1 .. 160 loop
         declare Before : constant Natural := Accesses; begin
            Pump.Turn (S, Turn);
            Check (Accesses <= Before + 1, "admin observation never advances controller a second time");
         end;
      end loop;
   end Drive;
   procedure Admit (Caller : PID.Process_ID := Launcher; Frame : Boolean := True) is
   begin Admin.Handle (S, Caller, A.OP_GPIO_ADMIT_SINK, 1, [], Frame, True, 1000, 1000); end Admit;
   procedure Retire (Who : PID.Process_ID) is
   begin Admin.Handle (S, Launcher, A.OP_GPIO_RETIRE_SINK, 1,
      A.Encode ((A.Retire_Sink, Badge_A, Who)), True, False, 1000, 1000); end Retire;
begin
   Check (Case_Name in "happy" | "save-failed" | "invalid-offer" | "retained-offer" |
      "take-refused" | "bad-mode" | "lost-admit" | "lost-retire" | "close-failed" |
      "uncertain" | "stop-admit" | "retire-save-failed" | "layout-change" | "parked-start" | "parked-uncertain" | "startup-owner" | "startup-duplicate" | "startup-invalid" | "startup-uncertain" | "startup-empty" | "startup-capacity" | "startup-missing" | "startup-expired" | "polled-loop" | "polled-clock-loss" | "polled-clock-reverse" | "polled-clock-exhausted" | "polled-mmio-uncertain", "known case");
   Event_Queue := 10; Request_Queue := 10;
   Pump.Turn (S, Turn);
   Check (Turn.Events = 3 and Turn.Requests = 2 and Turn.Steps = 0 and not Turn.Progress_Needed,
      "idle turn bounds flooded lanes independently without controller work");
   Check (Event_Queue = 7 and Request_Queue = 8, "unconsumed flooded entries remain queued");
   Event_Queue := 0; Request_Queue := 0;
   Memory (0) := 16#0094_0000#; Memory (3) := 16#1000#;
   W.Bind (1, 1, Physical, 8192, Virtual, 2, Bind_Report); Check (Bind_Report.Status = T.Bound, "admitted mapping established");
   W.Probe (G.Target_HID, Header); Check (Header.Status = T.Probed, "same mapped resource headers checked");
   W.Admit_Pin (160, Admission, Plan_A); Check (Admission = T.Admitted, "canonical A pin admitted");
   W.Admit_Pin (161, Admission, Plan_B); Check (Admission = T.Admitted, "canonical B pin admitted");
   Memory (Index (Plan_A.Selected.Host_Register)) := 7;
   if Case_Name in "startup-empty" | "startup-capacity" then
      declare
         Empty : Boot.Inventory (1 .. 0);
         Too_Many : constant Boot.Inventory := [(160, B.Level_Triggered, B.Active_Low), (161, B.Edge_Triggered, B.Active_High), (162, B.Edge_Triggered, B.Active_Low)];
         Before : constant Natural := Accesses;
      begin
         if Case_Name = "startup-empty" then Boot.Start (Empty, 1000, 32, OK);
         else Boot.Start (Too_Many, 1000, 32, OK); end if;
         Check (not OK and then Boot.Observe.Reason =
            (if Case_Name = "startup-empty" then Boot.Empty_Inventory else Boot.Capacity_Refused),
            "startup inventory size refusal is explicit");
         Check (C.Observe.Registered = 0 and then Accesses = Before, "invalid count never initializes a partial pin set");
         Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
      end;
   end if;
   if Case_Name in "startup-owner" | "startup-duplicate" | "startup-invalid" | "startup-uncertain" | "startup-missing" | "startup-expired" then
      declare
         Pins : Boot.Inventory := [(160, B.Level_Triggered, B.Active_Low), (161, B.Edge_Triggered, B.Active_High)];
         Before : constant Natural := Accesses;
      begin
         if Case_Name = "startup-duplicate" then Pins (2) := Pins (1);
         elsif Case_Name = "startup-invalid" then Pins (2) := (161, B.Level_Triggered, B.Active_Both);
         elsif Case_Name = "startup-missing" then Pins (2) := (65_534, B.Edge_Triggered, B.Active_High); end if;
         Boot.Start (Pins, (if Case_Name = "startup-expired" then 0 else 1000), 32, OK);
         if Case_Name in "startup-duplicate" | "startup-invalid" | "startup-missing" then
            Check (not OK and then Boot.Observe.At_Phase = Boot.Failed and then
               Boot.Observe.Reason = (if Case_Name = "startup-duplicate" then Boot.Duplicate_Pin elsif Case_Name = "startup-missing" then Boot.Unavailable_Pin else Boot.Invalid_Input),
               "complete inventory preflight rejects duplicate or invalid pin mode");
            Check (Accesses = Before and then C.Observe.Registered = 0, "invalid inventory never starts controller MMIO");
            Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
         end if;
         Check (OK, "trusted inventory starts bounded startup owner");
         for N in 1 .. 100 loop
            declare Previous : constant Natural := Accesses; begin
               Boot.Step; Check (Accesses <= Previous + 1, "one startup step performs at most one register access");
            end;
            exit when Boot.Observe.At_Phase in Boot.Ready | Boot.Failed;
         end loop;
         if Case_Name = "startup-expired" then
            Check (Boot.Observe.At_Phase = Boot.Failed and then Boot.Observe.Reason = Boot.Registration_Refused,
               "expired startup deadline rejects registration");
            Check (C.Observe.Registered = 0 and then Accesses = Before, "expired startup performs no pin register access");
            Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
         end if;
         if Case_Name = "startup-uncertain" then
            Check (Boot.Observe.At_Phase = Boot.Failed and then Boot.Observe.Reason = Boot.Controller_Faulted,
               "uncertain initialization never reaches ready");
            Check (C.Observe.Registered = 1 and then not Service.Observe.Sealed,
               "failed first pin prevents initializing later inventory pins or sealing service");
            declare Previous : constant Natural := Accesses; begin
               Boot.Step; Check (Accesses = Previous, "failed startup never retries MMIO");
            end;
            Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
         end if;
         Check (Boot.Observe.At_Phase = Boot.Ready and then Service.Quiescent and then Service.Observe.Sealed,
            "startup readiness requires all pins masked and sealed registry quiet");
         Boot.Start (Pins, 2000, 32, OK);
         Check (not OK and then Boot.Observe.At_Phase = Boot.Ready, "repeat startup cannot change immutable pin inventory");
      end;
   else
   if Startup_Held_Only then
      C.Register_Parked (Plan_A, I.Level_Low, 1000, 32, Pin_A, Registry_Admission, Owner_Admission); Finish;
      if Case_Name = "parked-uncertain" then
         Check (C.Observe.Faulted and then C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Faulted,
            "unknown startup status-clear effect freezes the controller");
         Check (not C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled and then not Service.Observe.Operational,
            "confirmed startup mask remains known but uncertain controller never becomes ready");
         C.Register_Parked (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission);
         Check (Registry_Admission = C.Controller_Faulted, "startup uncertainty prevents further pin admission");
         Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
      end if;
      Check (C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Held_Masked and then
         not C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled, "first registered pin completes masked");
   else C.Register_Pin (Plan_A, I.Level_Low, 1000, 32, Pin_A, Registry_Admission, Owner_Admission); Finish; end if;
   if Case_Name /= "layout-change" then
      if Startup_Held_Only then C.Register_Parked (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission);
      else C.Register_Pin (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission); end if; Finish;
   end if;
   Check (not Service.Needs_Step, "unsealed service waits without work");
   Service.Seal (1000, 32, OK); Check (OK, "immutable pin registry starts parking");
   Check (Service.Needs_Step, "initial parking requires bounded progress"); Drive;
   end if;
   Check (not Service.Needs_Step, "parked empty registry waits without spinning");
   if Startup_Held_Only then
      Check (C.Observe_Pin (2).Report.At_Phase = C.Engine.Held_Masked and then
         not C.Observe_Pin (2).Report.Pin_May_Be_Enabled, "all startup pins remain masked through service seal");
      Startup_Held_Only := False;
   end if;
   Check (Service.Observe.Operational and then Service.Quiescent, "no client needed for initial parking");
   if Case_Name in "polled-loop" | "polled-clock-loss" | "polled-clock-reverse" | "polled-clock-exhausted" then
      Scheduled.Turn (S, Schedule);
      Check (not Schedule.Poll_Started and then Schedule.Wait_For.Kind = GPIO_Loop_Time.Activity,
         "empty parked registry waits for activity without periodic hardware polling");
   end if;
   Admit (Wrong_Launcher); Check (Last_Admin_Status = Wire.Unauthorized and then Take_Calls = 0 and then Saves = 0, "wrong launcher incarnation cannot transfer");
   Admit (Frame => False); Check (Last_Admin_Status = Wire.Invalid_Frame and then Take_Calls = 0, "malformed frame never accepts capability");
   Current_Answers := 0;
   if Case_Name = "save-failed" then Save_OK := False;
   elsif Case_Name in "invalid-offer" | "retained-offer" then Inspect_OK := False; Discard_OK := Case_Name /= "retained-offer";
   elsif Case_Name = "take-refused" then Take_OK := False;
   elsif Case_Name = "bad-mode" then Offered_Badge := B.Canonical ((160, B.Level_Triggered, B.Active_High));
   elsif Case_Name = "lost-admit" then Deliver_OK := False;
   end if;
   Admit;
   if Case_Name in "save-failed" | "invalid-offer" | "retained-offer" | "take-refused" | "bad-mode" then
      Check (Service.Observe.Bound = 0 and then Service.Quiescent, "failed acceptance/save never admits hardware work");
      if Case_Name = "retained-offer" then
         Check (Admin.Observe (S).At_Phase = Admin.Quarantined and then Admin.Observe (S).Receipt_Owned and then not Admin.Quiescent (S), "failed discard retains opaque receipt");
         Offered := 19; Admit; Check (Take_Calls = 1 and then Discard_Calls = 1, "quarantine bounds held transfers without retries");
      else
         Check (Admin.Quiescent (S) and then not Admin.Observe (S).Receipt_Owned, "definite rejection disposes all local ownership");
         if Case_Name = "take-refused" then Check (Inspect_Calls = 0 and then Saves = 0 and then Discard_Calls = 0, "failed take has nothing to dispose");
         elsif Case_Name = "invalid-offer" then Check (Saves = 0 and then Discard_Calls = 1, "invalid sink disposed before saving reply");
         elsif Case_Name = "save-failed" then Check (Saves = 1 and then Saved_Answers = 0 and then Sink_Closed (17), "save failure closes unused offer and answers current request only");
         elsif Case_Name = "bad-mode" then Check (Saved_Answers = 1 and then Last_Admin_Status = Wire.Invalid_Channel and then Sink_Closed (17), "mode mismatch disposes accepted unused sink after saved rejection"); end if;
      end if;
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   Check (Admin.Observe (S).At_Phase = Admin.Await_Admission and then Admin.Observe (S).Reply_Owned and then not Admin.Observe (S).Receipt_Owned, "binding owns sink, admin retains one saved reply");
   Check (Service.Needs_Step, "pending binding requires progress before admin completion");
   if Case_Name = "layout-change" then
      C.Register_Pin (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission); Finish;
      Drive;
      Check (Service.Observe.Layout_Changed and then Admin.Observe (S).At_Phase = Admin.Quarantined,
         "changed sealed registry quarantines awaiting admission");
      Check (Last_Admin_Status = Wire.Uncertain and then Saved_Answers = 1 and then not Admin.Observe (S).Reply_Owned,
         "frozen registry answers saved admin caller once without awaiting impossible progress");
      Check (not Service.Needs_Step and then not Turn.Progress_Needed and then not Turn.Quiescent and then Close_Attempts (17) = 0,
         "frozen layout retains endpoint, stops polling and never claims quiet");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   Offered := 18; Admit; Check (Take_Calls = 1 and then Saves = 1 and then Last_Admin_Status = Wire.Busy, "busy admin request never takes another offer or overwrites saved reply");
   if Case_Name = "stop-admit" then Event_Queue := 1; Stop_Event := True; end if;
   Drive;
   if Case_Name = "lost-admit" then
      Check (Saved_Answers = 1 and then Consumed (1001) and then Sink_Closed (17) and then Close_Attempts (17) = 1, "lost successful admission reply triggers one bounded retirement");
      Check (Admin.Quiescent (S) and then Service.Observe_Channel (1).At_Phase = Service.Free, "lost-admission cleanup consumes confirmed retirement");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   elsif Case_Name = "stop-admit" then
      Check (Saved_Answers = 1 and then Last_Admin_Status = Wire.Refused and then Sink_Closed (17), "stop during pending admission replies only after cleanup");
      Check (Admin.Quiescent (S) and then Service.Terminal and then Service.Quiescent, "pending admission stop retains no admin authority");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   Check (Admin.Quiescent (S) and then Saved_Answers = 1 and then Last_Admin_Status = Wire.Completed and then Service.Authorized (Owner, Badge_A), "admission completion confirms binding");
   Check (not Service.Needs_Step, "parked live binding waits for client input");
   if Case_Name = "retire-save-failed" then
      Save_OK := False; Retire (Owner);
      Check (Last_Admin_Status = Wire.Busy and then Service.Authorized (Owner, Badge_A) and then Close_Attempts (17) = 0, "one-way/unsaved retirement never changes live binding");
      Check (Admin.Quiescent (S) and then Saved_Answers = 1, "failed retirement save retains no authority");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   if Case_Name = "uncertain" then Fault_Pause := True; end if;
   Service.Offer (Owner, Badge_A, (Wire.Arm, (Wire.Within, 1000)), 100, Taken); Check (Taken, "actual client arm queued");
   Check (Service.Needs_Step, "saved client arm requires progress"); Drive;
   Check (not Service.Needs_Step, "settled arm waits for IRQ or control input");
   Check (Reply_Count = 1 and then Last_Authority = 100, "actual client control completion delivered separately");
   if Case_Name in "polled-clock-loss" | "polled-clock-reverse" | "polled-clock-exhausted" then
      declare Before : constant Natural := Accesses; begin
         if Case_Name = "polled-clock-loss" then Clock_Available := False;
         elsif Case_Name = "polled-clock-reverse" then Clock_Sample := 0;
         else Clock_Sample := GPIO_Loop_Time.Finite_Time'Last; end if;
         Scheduled.Turn (S, Schedule);
         Check (Schedule.Clock_Status = (if Case_Name = "polled-clock-loss" then Scheduled.Unavailable
            elsif Case_Name = "polled-clock-reverse" then Scheduled.Reversed else Scheduled.Exhausted),
            "bad loop clock is diagnosed and latched");
         Check (Service.Observe.Stopping and then Schedule.Wait_For.Kind = GPIO_Loop_Time.Relative_Delay
            and then Schedule.Wait_For.Backoff = 5, "bad clocks stop admission and request explicit finite backoff");
         Clock_Available := True; Clock_Sample := 2;
         for N in 1 .. 32 loop Scheduled.Turn (S, Schedule); end loop;
         Check (Schedule.Clock_Status /= Scheduled.Sampled and then not Schedule.Work.Quiescent and then
            Accesses = Before and then Close_Attempts (17) = 0, "clock recovery cannot silently resume, retry MMIO or free uncertain endpoint");
      end;
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   if Case_Name = "polled-mmio-uncertain" then
      Fault_Pause := True;
      Memory (Index (Plan_A.Selected.Status_Register)) := Plan_A.Selected.Interrupt_Mask;
      for N in 1 .. 64 loop Scheduled.Turn (S, Schedule); exit when C.Observe.Faulted; end loop;
      Check (C.Observe.Faulted and then Schedule.Supervision_Required and then
         Schedule.Wait_For.Kind = GPIO_Loop_Time.Relative_Delay,
         "uncertain poll mask write is visible to caller instead of idle readiness");
      declare Before : constant Natural := Accesses; begin
         for N in 1 .. 16 loop Scheduled.Turn (S, Schedule); end loop;
         Check (Accesses = Before and then Event_Attempts = 0 and then Close_Attempts (17) = 0,
            "uncertain polled hardware freezes IO and retains endpoint without inventing an event");
      end;
      Service.Request_Stop (1000); Admin.Request_Stop (S);
      for N in 1 .. 32 loop Scheduled.Turn (S, Schedule); end loop;
      Check (Schedule.Work.Terminal and then not Schedule.Work.Quiescent and then
         Schedule.Supervision_Required and then Close_Attempts (17) = 0,
         "terminal controller failure cannot be mistaken for clean shutdown");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   if Case_Name = "polled-loop" then
      Check (Service.Polling_Needed, "armed admitted input needs timed polling");
      Scheduled.Turn (S, Schedule); Check (Schedule.Poll_Started, "first armed poll starts without parent IRQ");
      for N in 1 .. 32 loop Scheduled.Turn (S, Schedule); end loop;
      Check (Event_Attempts = 0 and then Schedule.Wait_For.Kind = GPIO_Loop_Time.Until_Time and then
         Schedule.Wait_For.Deadline = 11, "idle armed input waits for next finite polling time");
      declare Before : constant Natural := Accesses; begin
         Scheduled.Turn (S, Schedule);
         Check (not Schedule.Poll_Started and then Accesses = Before, "early turn does not poll hardware before interval");
      end;
      Clock_Sample := 11; Allow_Event := False;
      Memory (Index (Plan_A.Selected.Status_Register)) := Plan_A.Selected.Interrupt_Mask;
      for N in 1 .. 64 loop Scheduled.Turn (S, Schedule); exit when Event_Attempts > 0; end loop;
      Check (Event_Attempts > 0 and then Events_Accepted = 0 and then not Service.Polling_Needed and then
         not C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled, "observed input stays masked while sink is backpressured");
      Check (Schedule.Wait_For.Kind = GPIO_Loop_Time.Until_Time and then Schedule.Wait_For.Deadline = 16,
         "retained event gets a finite progress wake");
      declare Lease : constant Wire.Word := Last_Event.Lease; Generation : constant Wire.Word := Last_Event.Generation; begin
         Scheduled.Turn (S, Schedule);
         Check (Last_Event.Lease = Lease and then Last_Event.Generation = Generation,
            "backpressure retains exact event identity");
         Allow_Event := True; Scheduled.Turn (S, Schedule);
         Check (Events_Accepted = 1 and then Schedule.Wait_For.Kind = GPIO_Loop_Time.Activity,
            "delivered masked event waits for client ACK rather than polling");
         Service.Offer (Owner, Badge_A, (Wire.Acknowledge, (Wire.Within, 1000), Lease, Generation + 1), 151, Taken);
         Check (Taken, "stale ACK control queued for actual owner");
         for N in 1 .. 32 loop Scheduled.Turn (S, Schedule); exit when Reply_Count = 2; end loop;
         Check (Last_Reply.Status = Wire.Refused and then not C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled,
            "stale generation ACK cannot rearm polled input");
         Memory (Index (Plan_A.Selected.Status_Register)) := Plan_A.Selected.Interrupt_Mask;
         Service.Offer (Owner, Badge_A, (Wire.Acknowledge, (Wire.Within, 1000), Lease, Generation), 152, Taken);
         Check (Taken, "exact ACK control queued");
         for N in 1 .. 32 loop Scheduled.Turn (S, Schedule); exit when Reply_Count = 3; end loop;
         Check (Last_Reply.Status = Wire.Completed and then Service.Polling_Needed,
            "exact ACK rearms and schedules admitted input");
         Clock_Sample := 21;
         for N in 1 .. 64 loop Scheduled.Turn (S, Schedule); exit when Events_Accepted = 2; end loop;
         Check (Events_Accepted = 2 and then Last_Event.Lease = Lease and then Last_Event.Generation > Generation,
            "edge latched during mask survives ACK and yields a fresh event generation");
      end;
      Service.Request_Stop (1000); Admin.Request_Stop (S);
      for N in 1 .. 64 loop Scheduled.Turn (S, Schedule); exit when Schedule.Work.Terminal; end loop;
      Check (Schedule.Work.Terminal and then Schedule.Work.Quiescent and then Close_Attempts (17) = 1,
         "polled service stop drains and closes sink after confirmed mask");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   if Case_Name = "close-failed" then Fail_Close := 17;
   elsif Case_Name = "lost-retire" then Deliver_OK := False; end if;
   Retire (Owner); Check (Admin.Observe (S).At_Phase = Admin.Await_Retirement, "retirement retains admin reply until drained");
   Check (Service.Needs_Step, "retirement progresses before close and reply"); Drive;
   Check (not Service.Needs_Step, "settled retirement never spins on retained failure");
   if Case_Name in "close-failed" | "uncertain" then
      Check (Admin.Observe (S).At_Phase = Admin.Quarantined and then Last_Admin_Status = Wire.Uncertain and then not Admin.Observe (S).Reply_Owned, "unresolved retirement answers uncertain exactly once");
      Check (Saved_Answers = 2 and then not Service.Quiescent, "failed cleanup never acknowledges/free quiet state");
      if Case_Name = "close-failed" then Check (Close_Attempts (17) = 1 and then Service.Observe_Channel (1).At_Phase = Service.Close_Failed, "sink close failure retained");
      else Check (Close_Attempts (17) = 0, "uncertain hardware retains endpoint"); end if;
      Offered := 19; Admit; Check (Take_Calls = 1, "unresolved retirement prevents new offer ownership");
      Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image); return;
   end if;
   Check (Saved_Answers = 2 and then Consumed (1002) and then Last_Admin_Status = Wire.Completed and then Sink_Closed (17), "confirmed retirement saved reply consumed once");
   Check (Admin.Quiescent (S) and then Service.Observe_Channel (1).At_Phase = Service.Free, "admin acknowledgement frees retired slot after reply attempt");
   if Case_Name = "lost-retire" then Check (not Admin.Observe (S).Reply_Delivered and then Close_Attempts (17) = 1, "lost retirement reply does not replay cleanup");
   else
      Offered := 19; Offered_Owner := Replacement; Admit; Drive;
      Check (Last_Admin_Status = Wire.Completed and then Service.Authorized (Replacement, Badge_A), "runtime replacement admission succeeds");
      Retire (Owner); Check (Last_Admin_Status = Wire.Invalid_Channel and then Close_Attempts (19) = 0 and then Service.Authorized (Replacement, Badge_A), "stale retirement cannot affect replacement");
      Service.Offer (Replacement, Badge_A, (Wire.Arm, (Wire.Within, 1000)), 150, Taken); Check (Taken, "replacement control admitted"); Drive;
      Check (Last_Reply.Lease > Replies (100).Lease and then Last_Reply.Status = Wire.Completed, "replacement lease epoch remains fresh");
      Service.Request_Stop (1000); Admin.Request_Stop (S); Drive;
      Check (Admin.Quiescent (S) and then Service.Quiescent and then Service.Terminal and then Sink_Closed (19), "global shutdown drains registry and admin ownership");
   end if;
   Check (Event_Attempts = 0 and then Last_Event.Lease = 1 and then Last_Admin_Label = A.OP_GPIO_RETIRE_SINK, "no invented events or changed admin reply label");
   Ada.Text_IO.Put_Line ("GPIO admin owner checks" & Checks'Image);
end Admin_Tests;
