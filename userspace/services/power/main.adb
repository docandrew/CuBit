------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  power.svc, the power manager (PWR-003 slice 1, docs/power-states.md).
--
--  @description
--  It takes shutdown requests from its requesters (CuBit.Power_Requests:
--  Desktop's and the console's badges) and power-button presses from
--  acpi.svc (CuBit.Power_Event_Exchange: a watch it submits and acpi.svc
--  answers at the next press; acpi.svc reports, power.svc decides by its
--  typed Button_Policy). An accepted stop, for Power_Off with acpi.svc's
--  power endpoint (ACPI 6.6, 16.1.7):
--    1. procmgr stops the apps (CuBit.System_Stops.Stop_Apps_Operation);
--    2. acpi.svc prepares to sleep (Prepare_To_Sleep S5: \_TTS, \_PTS)
--       while every service and driver AML may use still runs;
--    3. procmgr stops the rest, keeping acpi.svc and what it depends on
--       (Stop_Operation; filesystem commit, drivers);
--    4. acpi.svc enters S5 (Enter_Sleep_State); on success the machine is
--       off.
--  For Halt, or without acpi.svc's endpoint, steps 2 and 4 are left out;
--  if 4 fails, power.svc halts: everything else is stopped and the storage
--  committed, and it says so. procmgr's stops are submitted
--  asynchronously: power.svc keeps answering (Busy) while they run. Calls go down only (procmgr,
--  acpi.svc); nothing calls power.svc but its requesters.
--  Every limit is a typed startup parameter or a named constant.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Console_Route;
with Interfaces; use Interfaces;
with CuBit.Log;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Handles.Services;
with CuBit.Power_Requests;
with CuBit.Power_Event_Exchange;
with CuBit.System_Stops;
with Power_Decisions;
with Parameters;
with Startup;
with CuBit.Monotonic;

procedure Main is
   package PR renames CuBit.Power_Requests;
   package PE renames CuBit.Power_Event_Exchange;
   package STP renames CuBit.System_Stops;
   package PD renames Power_Decisions;
   use type PD.Phase, PD.Button_Policy, PR.Answer, PR.Action, PR.Requester, PE.Watch_Status;

   P : constant Parameters.Power_Parameters_Value := Parameters.Value;

   Policy : constant PD.Button_Policy :=
     (case P.button is
        when Parameters.Power_Off => PD.Power_Off,
        when Parameters.Halt      => PD.Halt,
        when Parameters.Ignore    => PD.Ignore);

   --  Completion tokens of power.svc's own submits.
   Watch_Token     : constant Unsigned_64 := 1;
   Stop_Apps_Token : constant Unsigned_64 := 2;
   Stop_Token      : constant Unsigned_64 := 3;
   --  Request ids are per kind, from 1.
   Next_Watch_Id : Unsigned_64 := 1;
   Stop_Apps_Id  : constant Unsigned_64 := 1;
   Stop_Id       : constant Unsigned_64 := 2;
   Prepare_Id    : constant Unsigned_64 := 1;
   Enter_Id      : constant Unsigned_64 := 1;

   Process_Manager : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Services.Process_Manager.Handle (Startup.Process_Manager);
   Firmware : constant CuBit.Handles.Startup.Optional_Endpoint := Startup.Acpi;

   State : PD.Phase := PD.Idle;
   Registered : Boolean := False;
   Watching : Boolean := False;
   Seen : Unsigned_64 := 0;
   --  When a watch answered Watch_Unavailable is submitted again (0: none);
   --  how many times in a row it was.
   Watch_Retry_At : Unsigned_64 := 0;
   Watch_Retries : Unsigned_64 := 0;
   Asked : PR.Action := PR.Power_Off;
   Asked_By : PR.Requester := PR.Unknown;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Unsigned_64'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   procedure Halt (Why : String) is
   begin
      State := PD.Halted;
      CuBit.Log.Warning ("power: halted (" & Why & "): every program is stopped and the storage is " &
                         "committed; it is safe to switch the machine off");
   end Halt;

   procedure Watch is
      Request : Message := NULL_MESSAGE;
   begin
      if not Firmware.Present or else Watching or else State /= PD.Idle then
         return;
      end if;
      Request.tag := (label => PE.Button_Watch, length => PE.Watch_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (PE.Request_Word) := Next_Watch_Id;
      Request.words (PE.Seen_Word) := Seen;
      Next_Watch_Id := Next_Watch_Id + 1;
      Watching := capSubmit (Firmware.Value, Request, Watch_Token);
      if not Watching then
         CuBit.Log.Warning ("power: the power-button watch could not be submitted to acpi.svc");
      end if;
   end Watch;

   --  The root stop, asked of procmgr without waiting (its answer is a
   --  completion): power.svc keeps answering requests meanwhile.
   --  One of procmgr's stops (Label: Stop_Apps_Operation or Stop_Operation),
   --  submitted; False: it could not be.
   function Submit_Stop (Label : Unsigned_32; Id, Token : Unsigned_64) return Boolean is
      Request : Message := NULL_MESSAGE;
   begin
      Request.tag := (label => Label, length => STP.Stop_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (STP.Request_Word) := Id;
      Request.words (STP.Final_Word) := STP.Final_Step'Enum_Rep
        (if Asked = PR.Halt then STP.Halt else STP.Power_Off);
      Request.words (STP.Grace_Word) := STP.Grace_Word_Of
        (STP.Grace_Ms (P.undeclared_grace_ms), STP.Grace_Ms (P.storage_deadline_ms));
      Request.words (STP.Driver_Word) := Unsigned_64 (P.driver_grace_ms);
      return capSubmit (Process_Manager, Request, Token);
   end Submit_Stop;

   procedure Begin_Stop (Action : PR.Action; By : PR.Requester) is
   begin
      Asked := Action;
      Asked_By := By;
      State := PD.Stopping;
      CuBit.Log.Info ("power: " & PR.Name (Action) & " requested by " & PR.Name (By) &
                      "; asking procmgr to stop the system (undeclared grace" &
                      Unsigned_64'Image (Unsigned_64 (P.undeclared_grace_ms)) & " ms, storage" &
                      Unsigned_64'Image (Unsigned_64 (P.storage_deadline_ms)) & " ms, drivers" &
                      Unsigned_64'Image (Unsigned_64 (P.driver_grace_ms)) & " ms)");
      --  The apps first: acpi.svc prepares to sleep before any service or
      --  driver its AML may use is stopped.
      if not Submit_Stop (STP.Stop_Apps_Operation, Stop_Apps_Id, Stop_Apps_Token) then
         --  Nothing was stopped: power.svc cannot do the stop itself.
         State := PD.Idle;
         CuBit.Log.Error ("power: the stop could not be submitted to procmgr; nothing was stopped");
      end if;
   end Begin_Stop;

   --  After the apps stopped: \_TTS / \_PTS while ACPI's providers run
   --  (bounded; a failure is logged and the stop goes on, as Linux does).
   procedure Prepare is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if not PD.Needs_Firmware (Asked, Firmware.Present) then
         return;
      end if;
      CuBit.Log.Info ("power: asking acpi.svc to prepare to sleep (S5)");
      Request.tag := (label => PE.Prepare_To_Sleep, length => PE.Sleep_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (PE.Request_Word) := Prepare_Id;
      Request.words (PE.State_Word) := PE.Sleep_State'Enum_Rep (PE.S5);
      Request.words (PE.Confirm_Word) := PE.Sleep_Confirmation;
      Tag := capCall (Firmware.Value, Request, Deadline_After (PE.Prepare_Budget_Milliseconds));
      if Ending_Of (Tag) = Call_Answered and then Tag.label = PE.Prepare_To_Sleep
        and then PE.Valid_Prepare_Status (Request.words (PE.Prepare_Status_Word))
      then
         CuBit.Log.Info ("power: prepare to sleep " &
                         PE.Name (PE.Prepare_Status'Enum_Val (Request.words (PE.Prepare_Status_Word))) &
                         " (detail " & Image (Request.words (PE.Detail_Word)) & ", SLP_TYPa " &
                         Image (PE.Type_A_Of (Request.words (PE.Sleep_Types_Word))) & ", SLP_TYPb " &
                         Image (PE.Type_B_Of (Request.words (PE.Sleep_Types_Word))) & ")");
      else
         CuBit.Log.Warning ("power: acpi.svc did not answer Prepare_To_Sleep (" &
                            (case Ending_Of (Tag) is
                               when Call_Answered => "label " & Image (Unsigned_64 (Tag.label)),
                               when Call_Ended => "ended",
                               when Call_Timed_Out => "timed out",
                               when Call_Not_Delivered => "not delivered") & "); stopping anyway");
      end if;
   end Prepare;

   --  After procmgr's stop: acpi.svc enters S5, or power.svc halts.
   procedure Final_Step is
      Request : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      if not PD.Needs_Firmware (Asked, Firmware.Present) then
         Halt (if Asked = PR.Halt then "halt was asked for" else "no acpi-power endpoint");
         return;
      end if;
      State := PD.Final;
      CuBit.Log.Info ("power: asking acpi.svc to enter S5");
      --  Routed lines reach serial through logstore (ABI-002): hand them
      --  over before the machine goes off (the kernel drains only its own
      --  console before S5, LOG-009).
      CuBit.Log.Flush;
      Request.tag := (label => PE.Enter_Sleep_State, length => PE.Sleep_Request_Words,
                      protocol => 0, ipc => <>, reserved => 0);
      Request.words (PE.Request_Word) := Enter_Id;
      Request.words (PE.State_Word) := PE.Sleep_State'Enum_Rep (PE.S5);
      Request.words (PE.Confirm_Word) := PE.Sleep_Confirmation;
      Tag := capCall (Firmware.Value, Request, Deadline_After (PE.Enter_Budget_Milliseconds));
      --  Reaching here means the machine did not go off.
      if Ending_Of (Tag) = Call_Answered and then Tag.label = PE.Enter_Sleep_State
        and then PE.Valid_Enter_Status (Request.words (PE.Enter_Status_Word))
      then
         Halt ("acpi.svc could not enter S5: " &
               PE.Name (PE.Enter_Status'Enum_Val (Request.words (PE.Enter_Status_Word))) &
               " detail " & Image (Request.words (PE.Detail_Word)));
      else
         Halt ("acpi.svc did not answer Enter_Sleep_State (" &
               (case Ending_Of (Tag) is
                  when Call_Answered => "label " & Image (Unsigned_64 (Tag.label)),
                  when Call_Ended => "ended",
                  when Call_Timed_Out => "timed out",
                  when Call_Not_Delivered => "not delivered") & ")");
      end if;
   end Final_Step;

   procedure Watch_Answer (Completion : CompletionEntry) is
      Again, Pressed : Boolean;
      Status : PE.Watch_Status;
   begin
      Watching := False;
      if Completion.status /= COMPLETION_OK or else Completion.msg.tag.label /= PE.Button_Watch
        or else not PE.Valid_Watch_Status (Completion.msg.words (PE.Watch_Status_Word))
      then
         --  acpi.svc does not serve the watch (not yet implemented there,
         --  or it ended): no button until it is offered again.
         CuBit.Log.Warning ("power: acpi.svc did not take the power-button watch (label " &
                            Image (Unsigned_64 (Completion.msg.tag.label)) & ", completion status " &
                            Image (Completion.status) & "); the button is not watched");
         return;
      end if;
      Status := PE.To_Watch_Status (Completion.msg.words (PE.Watch_Status_Word));
      if PD.Retry_Later (Status) then
         Watch_Retries := Watch_Retries + 1;
         Watch_Retry_At := CuBit.Monotonic.Milliseconds + PE.Watch_Retry_Milliseconds;
         CuBit.Log.Warning ("power: acpi.svc could not keep the power-button watch (WATCH_UNAVAILABLE," &
                            Unsigned_64'Image (Watch_Retries) & " in a row); watching again in" &
                            Unsigned_64'Image (PE.Watch_Retry_Milliseconds) & " ms");
         return;
      end if;
      Watch_Retries := 0;
      PD.After_Watch (Status, Completion.msg.words (PE.Count_Word), Seen, Again, Pressed);
      if Pressed then
         CuBit.Log.Info
           ("power: power button pressed (" &
            (if PE.Valid_Source (Completion.msg.words (PE.Source_Word))
             then PE.Name (PE.To_Source (Completion.msg.words (PE.Source_Word))) else "UNKNOWN") &
            ", count " & Image (Seen) & ")");
         if PD.Press_Starts (State, Policy) then
            Begin_Stop (PD.Press_Action (Policy), PR.Power_Button);
         elsif Policy = PD.Ignore then
            CuBit.Log.Info ("power: the button policy is Ignore");
         end if;
      elsif Status /= PE.Pressed then
         CuBit.Log.Info ("power: button watch " & PE.Name (Status));
      end if;
      if Again then
         Watch;
      end if;
   end Watch_Answer;

   procedure Stop_Answer (Completion : CompletionEntry; Apps : Boolean) is
      M : Message renames Completion.msg;
      Label : constant Unsigned_32 := (if Apps then STP.Stop_Apps_Operation else STP.Stop_Operation);
   begin
      if Completion.status = COMPLETION_OK and then M.tag.label = Label
        and then M.words (STP.Result_Word) = STP.Stop_Result'Enum_Rep (STP.Stopped)
        and then Apps
      then
         CuBit.Log.Info ("power: procmgr stopped the apps");
         Prepare;
         if not Submit_Stop (STP.Stop_Operation, Stop_Id, Stop_Token) then
            Halt ("the rest of the stop could not be submitted to procmgr");
         end if;
      elsif Completion.status = COMPLETION_OK and then M.tag.label = Label
        and then M.words (STP.Result_Word) = STP.Stop_Result'Enum_Rep (STP.Stopped)
      then
         CuBit.Log.Info
           ("power: procmgr stopped the system:" &
            Natural'Image (STP.Field_Of (M.words (STP.Processes_Word), 0)) & " clean," &
            Natural'Image (STP.Field_Of (M.words (STP.Processes_Word), 1)) & " failing," &
            Natural'Image (STP.Field_Of (M.words (STP.Processes_Word), 2)) & " forced," &
            Natural'Image (STP.Field_Of (M.words (STP.Processes_Word), 3)) & " not ended");
         Final_Step;
      elsif Completion.status = COMPLETION_OK and then M.tag.label = Label and then Apps
        and then M.words (STP.Result_Word) in STP.Stop_Result'Enum_Rep (STP.Not_Approved)
                                            .. STP.Stop_Result'Enum_Rep (STP.Unanswerable)
      then
         --  Refused at once: nothing was stopped.
         State := PD.Idle;
         CuBit.Log.Error ("power: procmgr refused the stop: " &
                          STP.Name (STP.Stop_Result'Enum_Val (M.words (STP.Result_Word))) &
                          "; nothing was stopped");
      else
         --  Ended unanswered: procmgr may have stopped some of the system.
         --  Never power off on an unknown stop.
         Halt ("procmgr did not report the stop (result " & Image (M.words (STP.Result_Word)) &
               ", completion status " & Image (Completion.status) & ")");
      end if;
   end Stop_Answer;

   procedure Serve (From : Process_ID; Request : Message) is
      Answer : Message := NULL_MESSAGE;
      Who : constant PR.Requester := PR.Requester_Of (Request.authorityTag);
      Verdict : PR.Answer;
      Ignore : Unsigned_64;
   begin
      Answer.tag := (label => Request.tag.label, length => PR.Reply_Words, protocol => 0, ipc => <>, reserved => 0);
      Answer.words (PR.Reply_Request_Word) := Request.words (PR.Request_Word);
      if Request.tag.label /= PR.Request_Label then
         Verdict := PR.Malformed;
      else
         Verdict := PD.Answer_For
           (State, Who, Registered, Unsigned_64 (Request.tag.length),
            Request.words (PR.Request_Word), Request.words (PR.Action_Word));
      end if;
      Answer.words (PR.Answer_Word) := PR.Answer'Enum_Rep (Verdict);
      Ignore := reply (From, Answer);
      if Verdict = PR.Accepted then
         Begin_Stop (PR.To_Action (Request.words (PR.Action_Word)), Who);
      else
         CuBit.Log.Warning ("power: request from " & PR.Name (Who) & " " & PR.Name (Verdict));
      end if;
   end Serve;

   Completion : CompletionEntry;
   From : Process_ID;
   Request : Message;
   Found : Boolean;
   Activity : Activity_Result;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   CuBit.Log.Info ("power: starting (button " &
                   (case Policy is when PD.Power_Off => "POWER_OFF", when PD.Halt => "HALT",
                                   when PD.Ignore => "IGNORE") &
                   (if Firmware.Present then ", acpi.svc power endpoint present"
                    else ", no acpi.svc power endpoint: a stop ends halted") & ")");
   Registered := registerDriver (PR.Service_Role, Startup.Registration) /= Unsigned_64'Last;
   if not Registered then
      CuBit.Log.Error ("power: registration failed; procmgr will refuse its stops");
   end if;
   Watch;
   CuBit.Log.Info ("power: ready");
   loop
      Activity := Wait_For_Activity_Until (if Watch_Retry_At = 0 then Wait_Forever else Watch_Retry_At);
      exit when Activity = Unavailable;
      if Watch_Retry_At /= 0 and then CuBit.Monotonic.Milliseconds >= Watch_Retry_At then
         Watch_Retry_At := 0;
         Watch;
      end if;
      while Poll_Completion (Completion'Address) = 1 loop
         if Completion.token = Watch_Token then
            Watch_Answer (Completion);
         elsif Completion.token = Stop_Apps_Token then
            Stop_Answer (Completion, Apps => True);
         elsif Completion.token = Stop_Token then
            Stop_Answer (Completion, Apps => False);
         end if;
      end loop;
      loop
         Poll_Service_Request (From, Request, Found);
         exit when not Found;
         Serve (From, Request);
      end loop;
   end loop;
   CuBit.Log.Error ("power: activity wait unavailable; stopping");
end Main;
