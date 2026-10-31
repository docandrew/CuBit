pragma Ada_2022;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Request_Dispatch;
with GPIO_Channel_Badges;
with GPIO_Lease_Control;
with CuBit.Process_IDs;
package body GPIO_HID_Server_Model is
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   package P renames CuBit.Process_IDs;
   package L renames GPIO_Lease_Control;
   use type I.Word, G.Outcome, G.Pad_Number, W.Word, P.Process_ID;
   use type W.Completion_Status;
   Check_Count, Accesses, Arms, Acks, Releases : Natural := 0;
   Now : W.Word := 1;
   In_Submit, Request_Queued, Reply_Queued, Event_Queued : Boolean := False;
   Config, Enabled : I.Word := 16#8000_0000#;
   Pending : I.Word := 0;
   Neighbor_Mask : constant I.Word := 16#8000_0000#;
   Client : constant P.Process_ID := P.From_Word (16#D123_4567_0000_9876#);
   Badge : constant W.Word := GPIO_Channel_Badges.Canonical
     ((160, GPIO_Channel_Badges.Level_Triggered, GPIO_Channel_Badges.Active_Low));
   Plan : constant G.Plan_Result := G.Plan (G.Target_HID, 160, 16#0094_0000#, 16#700#, 4096);
   Queued_Request : W.Request;
   Queued_Token, Current_Reply, Last_Token : W.Word := 0;
   Reply : W.Completion := (W.Arm, W.Refused, 0, 0);
   Event : W.Event := (1, 1);
   procedure Check (OK : Boolean; Why : String) is
   begin Check_Count := Check_Count + 1; if not OK then raise Program_Error with Why; end if; end Check;
   procedure Clock (Value : out I.Tick; Available : out Boolean) is
   begin Value := I.Tick (Now); Available := True; end Clock;
   procedure Read_Register (Selected : G.Plan_Result; ID : I.Read_ID;
      Value : out I.Word; Status : out I.Access_Status) is
   begin
      Check (Selected.Status = G.Ready and then Selected.Selected.Pad = Plan.Selected.Pad, "admitted read plan");
      Accesses := Accesses + 1; Status := I.Access_OK;
      Value := (case ID is when I.Pad_Owner | I.Configuration_Lock => 0,
         when I.Host_Owner => I.Word'Last, when I.Pad_Configuration => Config,
         when I.Group_Enable => Enabled, when I.Group_Status => Pending);
   end Read_Register;
   procedure Write_Register (Selected : G.Plan_Result; ID : I.Write_ID;
      Value : I.Word; Status : out I.Access_Status) is
   begin
      Check (Selected.Status = G.Ready and then Selected.Selected.Pad = Plan.Selected.Pad, "admitted write plan");
      Accesses := Accesses + 1; Status := I.Access_OK;
      case ID is
         when I.Write_Configuration => Config := Value;
         when I.Write_Enable => Check ((Value and Neighbor_Mask) = Neighbor_Mask, "preserve neighbor bits"); Enabled := Value;
         when I.Clear_Status => Check (Value = Plan.Selected.Interrupt_Mask, "exact W1C"); Pending := Pending and not Value;
      end case;
   end Write_Register;
   package Controller is new Intel_GPIO_Controller (1, Clock, Read_Register, Write_Register);
   use type Controller.Pin_ID, Controller.Admission, Controller.Engine.Admission;
   Lease_Counter : L.Lease_ID := 41;
   procedure Next_Lease (Value : out L.Lease_ID; Available : out Boolean) is
   begin Lease_Counter := Lease_Counter + 1; Value := Lease_Counter; Available := True; end Next_Lease;
   package Leases is new GPIO_Pin_Leases (Controller, Clock, Next_Lease);
   procedure Answer (Authority : W.Word; Value : W.Completion; Delivered : out Boolean) is
   begin
      Check (not In_Submit and then not Reply_Queued and then Authority /= 0, "saved reply queued off client stack");
      Reply := Value; Current_Reply := Authority; Delivered := True; Reply_Queued := True;
      if Value.Status = W.Completed then
         case Value.Kind is when W.Arm => Arms := Arms + 1;
            when W.Release_Pin => Releases := Releases + 1; when W.Acknowledge => Acks := Acks + 1; end case;
      end if;
   end Answer;
   procedure Try_Event (Sink : Natural; Value : W.Event; Accepted : out Boolean) is
   begin
      Check (not In_Submit and then Sink = 1 and then not Event_Queued, "event queued off client stack");
      Accepted := True; Event := Value; Event_Queued := True;
   end Try_Event;
   package Channels is new GPIO_Channel_Owner (Leases, 100, W.Word, Natural, Answer, Try_Event);
   use type Channels.Binding_Phase, Channels.Admission;
   Channel : Channels.State;
   Pin : Controller.Pin_ID;
   function Authorized (Caller : P.Process_ID; Tag : W.Word) return Boolean is
     (Caller = Client and then Tag = Badge);
   procedure Save (Authority : out W.Word; Saved : out Boolean) is
   begin Authority := Queued_Token; Saved := Authority /= 0; end Save;
   procedure Answer_Saved (Authority : W.Word; Label : W.Operation;
      Value : W.Completion; Delivered : out Boolean) is
   begin
      Check (Label = W.Label_Of (Value.Kind), "saved response operation preserved");
      Answer (Authority, Value, Delivered);
   end Answer_Saved;
   procedure Answer_Current (Caller : P.Process_ID; Label : W.Operation;
      Value : W.Completion; Delivered : out Boolean) is
   begin Check (Caller = Client, "current response destination"); Answer_Saved (Queued_Token, Label, Value, Delivered); end Answer_Current;
   procedure Offer (Caller : P.Process_ID; Tag : W.Word; Item : W.Request;
      Authority : W.Word; Taken : out Boolean) is
      Result : Channels.Admission;
   begin
      Check (Authorized (Caller, Tag), "exact admitted channel");
      Channels.Offer (Channel, Caller, Item, Authority, Result); Taken := Result = Channels.Accepted;
   end Offer;
   package Dispatch is new GPIO_Request_Dispatch
     (W.Word, Authorized, Save, Answer_Saved, Answer_Current, Offer);
   use type Dispatch.Outcome;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
   begin
      In_Submit := True; Check (not Request_Queued and then Token > Last_Token, "fresh queued request");
      Queued_Request := Item; Queued_Token := Token; Last_Token := Token;
      Request_Queued := True; Accepted := True; In_Submit := False;
   end Submit;
   procedure Turn is
      Worked : Boolean;
      Before : constant Natural := Accesses;
   begin
      Controller.Advance (Worked); Check (Accesses <= Before + 1, "one register access per controller turn");
      Channels.Step (Channel);
   end Turn;
   procedure Settle is
   begin
      for N in 1 .. 100 loop
         exit when Controller.Observe.Active = Controller.No_Pin and then not Channels.Needs_Step (Channel);
         Turn;
      end loop;
      Check (Controller.Observe.Active = Controller.No_Pin and then not Channels.Needs_Step (Channel), "bounded settle");
   end Settle;
   procedure Drive (Now : W.Word) is
      Result : Dispatch.Report;
   begin
      Check (Now >= GPIO_HID_Server_Model.Now, "server cached clock is monotonic");
      GPIO_HID_Server_Model.Now := Now;
      if Request_Queued then
         Check (not In_Submit, "dispatch is outside Submit"); Request_Queued := False;
         Dispatch.Handle (Client, Badge, W.Label_Of (Queued_Request.Kind), W.Protocol_Version,
            W.Encode (Queued_Request), True, Result);
         Check (Result.Result = Dispatch.Control_Retained, "saved reply transferred");
      end if;
      Settle;
   end Drive;
   procedure Initialize is
      Registered : Controller.Admission;
      Owner_Result : Controller.Engine.Admission;
      Accepted : Boolean;
   begin
      Controller.Register_Parked (Plan, I.Level_Low, 1000, 100, Pin, Registered, Owner_Result);
      Check (Registered = Controller.Accepted and then Owner_Result = Controller.Engine.Accepted, "parked profile pin"); Settle;
      Channels.Configure (Channel, Pin, Client, 1, 1000, Accepted);
      Check (Accepted, "trusted channel configured"); Settle;
      Check (Channels.Observe (Channel).Binding = Channels.Ready and then Channels.Quiescent (Channel), "channel ready");
   end Initialize;
   procedure Trigger_Event (Now : W.Word) is
      Accepted : Boolean;
   begin
      Drive (Now); Check (not Event_Queued, "previous event consumed by transport");
      Pending := Plan.Selected.Interrupt_Mask;
      Channels.Poll (Channel, I.Tick (Now + 100), Accepted); Check (Accepted, "real GPIO poll admitted"); Settle;
      Check (Event_Queued and then Masked, "event masked and queued");
   end Trigger_Event;
   procedure Take_Reply (Token : out W.Word; Value : out W.Completion) is
   begin Check (Reply_Queued, "real reply available"); Token := Current_Reply; Value := Reply; Reply_Queued := False; end Take_Reply;
   procedure Take_Event (Value : out W.Event) is
   begin Check (Event_Queued, "real event available"); Value := Event; Event_Queued := False; end Take_Event;
   function Masked return Boolean is ((Enabled and Plan.Selected.Interrupt_Mask) = 0);
   function Quiescent return Boolean is (Channels.Quiescent (Channel));
   function Checks return Natural is (Check_Count);
   function Ack_Count return Natural is (Acks);
   function Arm_Count return Natural is (Arms);
   function Release_Count return Natural is (Releases);
end GPIO_HID_Server_Model;
