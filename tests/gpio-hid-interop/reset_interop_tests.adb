pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Request_Dispatch;
with GPIO_Client_Controls;
with GPIO_Control_Wire;
with GPIO_Channel_Badges;
with GPIO_Lease_Control;
with HID_GPIO_Reset;
with CuBit.Process_IDs;
procedure Reset_Interop_Tests is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   package W renames GPIO_Control_Wire;
   package P renames CuBit.Process_IDs;
   package L renames GPIO_Lease_Control;
   use type I.Word, I.Tick, I.Write_ID, I.Access_Status;
   use type W.Word, W.Completion_Status, W.Control_Kind, W.Decode_Status;
   use type P.Process_ID, L.Completion_Status, G.Outcome, G.Pad_Number;
   Scenario : constant String := Ada.Command_Line.Argument (1);
   Checks, Accesses, Writes, Unmask_Writes, Answers, Submissions, Event_Attempts : Natural := 0;
   Now : W.Word := 1;
   Clock_OK, In_Submit, Request_Queued, Reply_Queued, Event_Queued : Boolean := False;
   Allow_Event, Allow_Answer : Boolean := True;
   Unknown_Seen : Boolean := False;
   Config : I.Word := 16#8000_0000#;
   Enabled : I.Word := 16#8000_0000#;
   Pending : I.Word := 0;
   Firmware_Mask : constant I.Word := 16#8000_0000#;
   Client : constant P.Process_ID := P.From_Word (16#D123_4567_0000_ABCD#);
   Server : constant P.Process_ID := P.From_Word (16#E123_4567_0000_ABCD#);
   Badge : constant W.Word := GPIO_Channel_Badges.Canonical
     ((160, GPIO_Channel_Badges.Level_Triggered, GPIO_Channel_Badges.Active_Low));
   Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 160, 16#0094_0000#, 16#700#, 4096);
   Queued_Request : W.Request;
   Queued_Token, Current_Reply, Last_Token : W.Word := 0;
   Reply : W.Completion := (W.Arm, W.Refused, 0, 0);
   Event : W.Event := (1, 1);
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   procedure Clock (Value : out I.Tick; Available : out Boolean) is
   begin Value := I.Tick (Now); Available := Clock_OK; end Clock;
   procedure Read_Register (Selected : G.Plan_Result; ID : I.Read_ID;
      Value : out I.Word; Status : out I.Access_Status) is
   begin
      Check (Selected.Status = G.Ready and then Selected.Selected.Pad = Plan.Selected.Pad,
         "server reads only its admitted plan"); Accesses := Accesses + 1; Status := I.Access_OK;
      Value := (case ID is when I.Pad_Owner | I.Configuration_Lock => 0,
         when I.Host_Owner => I.Word'Last, when I.Pad_Configuration => Config,
         when I.Group_Enable => Enabled, when I.Group_Status => Pending);
   end Read_Register;
   procedure Write_Register (Selected : G.Plan_Result; ID : I.Write_ID;
      Value : I.Word; Status : out I.Access_Status) is
   begin
      Check (Selected.Status = G.Ready and then Selected.Selected.Pad = Plan.Selected.Pad,
         "server writes only its admitted plan"); Accesses := Accesses + 1; Writes := Writes + 1; Status := I.Access_OK;
      case ID is
         when I.Write_Configuration => Config := Value;
         when I.Write_Enable =>
            Check ((Value and Firmware_Mask) = Firmware_Mask, "neighbor enable bits preserved");
            Enabled := Value;
            if (Value and Plan.Selected.Interrupt_Mask) /= 0 then
               Unmask_Writes := Unmask_Writes + 1;
               if Scenario = "unknown-arm" then Unknown_Seen := True; Status := I.Unknown_Effect; end if;
            end if;
         when I.Clear_Status =>
            Check (Value = Plan.Selected.Interrupt_Mask, "W1C exact selected pin"); Pending := Pending and not Value;
      end case;
   end Write_Register;
   package Controller is new Intel_GPIO_Controller (1, Clock, Read_Register, Write_Register);
   use type Controller.Pin_ID, Controller.Admission, Controller.Engine.Admission;
   Lease_Counter : L.Lease_ID := 16#1000_0000_0000_0000#;
   procedure Next_Lease (Value : out L.Lease_ID; Available : out Boolean) is
   begin Lease_Counter := Lease_Counter + 1; Value := Lease_Counter; Available := True; end Next_Lease;
   package Leases is new GPIO_Pin_Leases (Controller, Clock, Next_Lease);
   procedure Answer (Authority : W.Word; Value : W.Completion; Delivered : out Boolean) is
   begin
      Check (not In_Submit and then not Reply_Queued and then Authority /= 0,
         "saved reply consumed outside client submit stack"); Answers := Answers + 1;
      Reply := Value; Current_Reply := Authority; Delivered := Allow_Answer; Reply_Queued := Delivered;
   end Answer;
   procedure Try_Event (Sink : Natural; Value : W.Event; Accepted : out Boolean) is
   begin
      Check (not In_Submit and then Sink = 1 and then not Event_Queued,
         "event is queued nonblocking to admitted sink");
      Event_Attempts := Event_Attempts + 1; Accepted := Allow_Event;
      if Accepted then Event := Value; Event_Queued := True; end if;
   end Try_Event;
   package Channels is new GPIO_Channel_Owner (Leases, 100, W.Word, Natural, Answer, Try_Event);
   use type Channels.Binding_Phase, Channels.Admission, Channels.Lifecycle;
   Channel : Channels.State;
   Pin : Controller.Pin_ID;
   function Authorized (Caller : P.Process_ID; Tag : W.Word) return Boolean is
     (Caller = Client and then Tag = Badge);
   procedure Save (Authority : out W.Word; Saved : out Boolean) is
   begin Authority := Queued_Token; Saved := Authority /= 0; end Save;
   procedure Answer_Saved (Authority : W.Word; Label : W.Operation;
      Value : W.Completion; Delivered : out Boolean) is
   begin Check (Label = W.Label_Of (Value.Kind), "reply operation preserved"); Answer (Authority, Value, Delivered); end Answer_Saved;
   procedure Answer_Current (Caller : P.Process_ID; Label : W.Operation;
      Value : W.Completion; Delivered : out Boolean) is
   begin Check (Caller = Client, "current reply bound to requester"); Answer_Saved (Queued_Token, Label, Value, Delivered); end Answer_Current;
   procedure Offer (Caller : P.Process_ID; Tag : W.Word; Item : W.Request;
      Authority : W.Word; Taken : out Boolean) is
      Result : Channels.Admission;
   begin
      Check (Authorized (Caller, Tag), "dispatcher admits exact launcher binding");
      Channels.Offer (Channel, Caller, Item, Authority, Result); Taken := Result = Channels.Accepted;
   end Offer;
   package Dispatch is new GPIO_Request_Dispatch
     (W.Word, Authorized, Save, Answer_Saved, Answer_Current, Offer);
   use type Dispatch.Outcome;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
   begin
      In_Submit := True;
      Check (not Request_Queued and then Token > Last_Token, "one queued request with fresh full-width token");
      Queued_Request := Item; Queued_Token := Token; Last_Token := Token;
      Request_Queued := True; Submissions := Submissions + 1; Accepted := True;
      In_Submit := False;
   end Submit;
   package Controls is new GPIO_Client_Controls (Submit);
   use type Controls.Receive_Result, Controls.Phase;
   Token_Counter : W.Word := 16#2000_0000_0000_0000#;
   procedure Next_Token (Token : out W.Word; Available : out Boolean) is
   begin Token_Counter := Token_Counter + 1; Token := Token_Counter; Available := True; end Next_Token;
   procedure Client_Clock (Value : out W.Word; Available : out Boolean) is
   begin Value := Now; Available := Clock_OK; end Client_Clock;
   package Reset is new HID_GPIO_Reset.Bridge (Controls, Server, 20, Next_Token, Client_Clock);
   procedure Turn is
      Worked : Boolean;
      Before : constant Natural := Accesses;
   begin
      Controller.Advance (Worked); Check (Accesses <= Before + 1, "bounded server register access");
      Channels.Step (Channel);
   end Turn;
   procedure Settle is
   begin
      for N in 1 .. 100 loop
         exit when Controller.Observe.Active = Controller.No_Pin and then not Channels.Needs_Step (Channel);
         Turn;
      end loop;
      Check (Controller.Observe.Active = Controller.No_Pin, "bounded controller completion");
   end Settle;
   procedure Dispatch_Queued is
      Result : Dispatch.Report;
   begin
      Check (Request_Queued and then not In_Submit, "queued transport delivery occurs later");
      Request_Queued := False;
      Dispatch.Handle (Client, Badge, W.Label_Of (Queued_Request.Kind), W.Protocol_Version,
         W.Encode (Queued_Request), True, Result);
      Check (Result.Result = Dispatch.Control_Retained, "real dispatcher transfers saved reply to channel");
      Settle;
   end Dispatch_Queued;
   procedure Deliver_Reply is
      Result : Controls.Receive_Result;
      Before : constant Natural := Submissions;
   begin
      Check (Reply_Queued, "server actually queued completion"); Reply_Queued := False;
      Reset.Receive (Current_Reply, Server, Controls.Delivered, W.Label_Of (Reply.Kind),
         W.Protocol_Version, W.Encode_Completion (Reply), Result);
      Check (Result = (if Reply.Status = W.Uncertain then Controls.Uncertain_Result else Controls.Accepted),
         "actual response accepted with its real uncertainty");
      Check (Submissions = Before, "completion delivery never replays a control");
   end Deliver_Reply;
   procedure Lost_Reply is
      Result : Controls.Receive_Result;
   begin
      Check (not Reply_Queued, "failed server answer queued nothing");
      Reset.Receive (Current_Reply, P.No_Process, Controls.Failed, W.Label_Of (Reply.Kind),
         0, [0, 0, 0], Result);
      Check (Result = Controls.Uncertain_Result, "kernel transport failure retains uncertainty");
   end Lost_Reply;
   Reply_Value : L.Reply;
   Accepted : Boolean;
   Registered : Controller.Admission;
   Owner_Result : Controller.Engine.Admission;
   Arm_Lease : W.Word;
   Before_Writes, Before_Answers, Before_Submissions : Natural;
begin
   Check (Scenario in "happy" | "backpressure" | "late-arm" | "lost-arm" | "lost-release" | "unknown-arm" | "server-expired",
      "known scenario"); Clock_OK := True;
   Controller.Register_Parked (Plan, I.Level_Low, 100, 100, Pin, Registered, Owner_Result);
   Check (Registered = Controller.Accepted and then Owner_Result = Controller.Engine.Accepted, "registered exact profile pin");
   Settle;
   Channels.Configure (Channel, Pin, Client, 1, 100, Accepted); Check (Accepted, "trusted launcher channel configured"); Settle;
   Check (Channels.Observe (Channel).Binding = Channels.Ready and then Channels.Quiescent (Channel), "parked channel ready");
   Before_Writes := Writes;
   Reset.Begin_Arm_Reset (10, Reply_Value);
   Check (Reply_Value.Status = L.Pending and then Request_Queued and then Answers = 0, "HID arm is asynchronous");
   if Scenario = "server-expired" then Now := 11; end if;
   if Scenario = "lost-arm" then Allow_Answer := False; end if;
   Dispatch_Queued;
   if Scenario = "lost-arm" then
      Check (Answers = 1 and then Channels.Observe (Channel).Answer_Lost and then not Channels.Quiescent (Channel),
         "lost arm reply retains real armed obligation");
      Lost_Reply; Reset.Poll_Arm_Reset (Reply_Value);
      Check (Reply_Value.Status = L.Uncertain and then Reset.Observe.At_Phase = Controls.Quarantined,
         "HID does not infer cancellation from delivery loss");
      Channels.Request_Stop (Channel, 100); Settle;
      Check (Channels.Quiescent (Channel) and then Channels.Observe (Channel).Life = Channels.Stopped,
         "supervisor separately confirms server drain");
   elsif Scenario = "unknown-arm" then
      Check (Unknown_Seen and then Unmask_Writes = 1 and then Controller.Observe.Faulted, "unknown write is real and latches fault");
      Deliver_Reply; Reset.Poll_Arm_Reset (Reply_Value);
      Check (Reply_Value.Status = L.Uncertain and then not Channels.Quiescent (Channel), "uncertainty survives client/server join");
      Before_Writes := Writes; Before_Submissions := Submissions;
      for N in 1 .. 4 loop Reset.Poll_Arm_Reset (Reply_Value); Turn; end loop;
      Check (Writes = Before_Writes and then Submissions = Before_Submissions, "unknown effects never replay");
   elsif Scenario = "server-expired" then
      Deliver_Reply; Reset.Poll_Arm_Reset (Reply_Value);
      Check (Reply_Value.Status = L.Refused and then Writes = Before_Writes and then Unmask_Writes = 0,
         "expired server request performs no arm writes");
      Check (Channels.Quiescent (Channel), "definite refusal leaves channel parked");
   else
      Check (Reply.Status = W.Completed and then Reply.Lease /= 0 and then Unmask_Writes = 1,
         "real controller confirms arm before reply"); Arm_Lease := Reply.Lease;
      if Scenario = "late-arm" then
         Now := 11; Reset.Poll_Arm_Reset (Reply_Value);
         Check (Reply_Value.Status = L.Pending and then Reset.Observe.At_Phase = Controls.Quarantined,
            "HID drains late arm instead of assuming cancellation");
      end if;
      Deliver_Reply;
      if Scenario = "late-arm" then Check (Reset.Observe.Late, "late success keeps deadline outcome"); end if;
      Reset.Poll_Arm_Reset (Reply_Value);
      Check (Reply_Value.Status = L.Completed and then Reply_Value.Lease = Arm_Lease, "HID receives actual full-width lease");
      if Scenario in "happy" | "backpressure" then
         if Scenario = "backpressure" then Allow_Event := False; end if;
         Pending := Plan.Selected.Interrupt_Mask; Channels.Poll (Channel, 100, Accepted); Check (Accepted, "real poll admitted"); Settle;
         Check ((Enabled and Plan.Selected.Interrupt_Mask) = 0, "event is masked before delivery");
         if Scenario = "backpressure" then
            Check (not Event_Queued and then Event_Attempts > 0, "backpressure retains event");
            Before_Writes := Writes; Before_Answers := Answers;
            for N in 1 .. 3 loop Turn; end loop;
            Check (Writes = Before_Writes and then Answers = Before_Answers, "event backpressure does not repeat register/control work");
            Allow_Event := True; Turn;
         end if;
         Check (Event_Queued and then Event.Lease = Arm_Lease and then Event.Generation = W.Word (Controller.Observe_Pin (Pin).Report.Event),
            "real GPIO event carries matching lease and generated ticket");
         declare Decoded : constant W.Event_Decoding := W.Decode_Event (W.OP_GPIO_EVENT, W.Protocol_Version, W.Encode_Event (Event)); begin
            Check (Decoded.Status = W.Decoded and then Decoded.Value.Lease = Arm_Lease, "client event codec matches server encoding");
         end;
         Event_Queued := False;
      end if;
      if Scenario = "lost-release" then Allow_Answer := False; end if;
      Reset.Begin_Release_Reset (Arm_Lease, 2, Reply_Value);
      Check (Reply_Value.Status = L.Pending and then Queued_Request.Kind = W.Release_Pin
         and then Queued_Request.Limit.At_Millisecond = Now + 20,
         "release has a fresh cleanup deadline rather than stale HID deadline");
      Dispatch_Queued;
      Check ((Enabled and Plan.Selected.Interrupt_Mask) = 0, "release confirms actual mask");
      if Scenario = "lost-release" then
         Lost_Reply; Reset.Poll_Release_Reset (Reply_Value);
         Check (Reply_Value.Status = L.Uncertain and then Reset.Observe.At_Phase = Controls.Quarantined,
            "masked hardware does not establish client completion after reply loss");
      else
         Deliver_Reply; Reset.Poll_Release_Reset (Reply_Value);
         Check (Reply_Value.Status = L.Completed and then Reply_Value.Lease = Arm_Lease
            and then Channels.Quiescent (Channel), "matching release completes both owners");
      end if;
      Check (Submissions = 2, "one arm and one cleanup submission");
   end if;
   Check (P.To_Word (Client) > 16#FFFF_FFFF# and then P.To_Word (Server) > 16#FFFF_FFFF#,
      "both bound processes preserve opaque full-width identities");
   Check ((Enabled and Firmware_Mask) = Firmware_Mask, "firmware neighbor bits preserved throughout");
   Ada.Text_IO.Put_Line ("GPIO HID reset interop checks" & Checks'Image);
end Reset_Interop_Tests;
