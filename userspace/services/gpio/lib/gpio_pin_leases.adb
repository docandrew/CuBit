pragma Ada_2022;
package body GPIO_Pin_Leases with SPARK_Mode is
   use type I.Tick, I.Ticket, L.Lease_ID, P.Process_ID, C.Pin_ID;
   use type C.Admission, C.Engine.Phase;
   Reserved_Pins : array (C.Pin_ID range 1 .. C.Pin_ID'Last) of Boolean := [others => False];
   Last_Allocated_Lease : L.Lease_ID := L.No_Lease;
   function Quiescent (S : State) return Boolean is
      Pin : constant C.Pin_Observation := C.Observe_Pin (S.Info.Pin);
   begin
      return S.Info.At_Phase = Parked and then S.Info.Lease = L.No_Lease and then
         S.Op = No_Operation and then not S.Reply_Owned and then Pin.Present and then
         Pin.Report.At_Phase = C.Engine.Held_Masked and then not Pin.Report.Pin_May_Be_Enabled;
   end Quiescent;
   procedure Take_Reply (S : in out State; Reply : out L.Reply) is
   begin Reply := S.Reply; S.Reply_Owned := False; S.Reply_Ready := False; end Take_Reply;
   procedure Delivered (S : in out State) is
   begin S.Info.At_Phase := Event_Delivered; end Delivered;
   procedure Begin_Operation (S : in out State; Op : Operation; At_Phase : Phase;
      Deadline : I.Tick; Budget : I.Limit; Reply : Boolean) is
   begin
      S.Before := S.Info.At_Phase; S.Op := Op; S.Info.At_Phase := At_Phase;
      S.Info.Reason := None; S.Deadline := Deadline; S.Budget := Budget; S.Started := False;
      if Reply then S.Reply_Owned := True; S.Reply_Ready := False; S.Reply := (L.Pending, 0); end if;
   end Begin_Operation;
   procedure Bind (S : in out State; Pin : C.Pin_ID; Owner : P.Process_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean) is
      Observation : constant C.Pin_Observation := C.Observe_Pin (Pin);
   begin
      Accepted := False;
      if S.Info.At_Phase /= Unbound or else S.Reply_Owned or else C.Observe.Faulted or else not P.Is_Process (Owner) or else Pin = C.No_Pin or else not Observation.Present then return; end if;
      if Reserved_Pins (Pin) then return; end if;
      if Observation.Report.At_Phase /= C.Engine.Listening and then
        not (Observation.Report.At_Phase = C.Engine.Held_Masked and then
             not Observation.Report.Pin_May_Be_Enabled) then return; end if;
      Reserved_Pins (Pin) := True; S.Owner := Owner; S.Info.Pin := Pin;
      if Observation.Report.At_Phase = C.Engine.Held_Masked then
         S.Info.At_Phase := Parked; S.Info.Reason := None;
         S.Reply := (L.Completed, L.No_Lease); S.Reply_Owned := True; S.Reply_Ready := True;
      else Begin_Operation (S, Bind_Operation, Binding, Deadline, Budget, True);
      end if;
      Accepted := True;
   end Bind;
   procedure Unbind (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := not C.Observe.Faulted and then Quiescent (S);
      if not Accepted then return; end if;
      Reserved_Pins (S.Info.Pin) := False;
      S.Info := (others => <>); S.Owner := P.No_Process; S.Op := No_Operation;
      S.Before := Unbound; S.Started := False; S.Reply_Owned := False; S.Reply_Ready := False;
      S.Reply := (L.Pending, L.No_Lease); S.Deadline := 0;
   end Unbind;
   procedure Arm (S : in out State; Caller : P.Process_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Caller /= S.Owner or else S.Info.At_Phase /= Parked or else S.Reply_Owned then return; end if;
      Begin_Operation (S, Arm_Operation, Arming, Deadline, Budget, True); Accepted := True;
   end Arm;
   procedure Release (S : in out State; Caller : P.Process_ID; Lease : L.Lease_ID;
      Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Caller /= S.Owner or else Lease = L.No_Lease or else Lease /= S.Info.Lease or else
        S.Info.At_Phase not in Armed | Event_Retained | Event_Delivered or else S.Reply_Owned then return; end if;
      S.Info.Had_Event_At_Release := S.Info.At_Phase in Event_Retained | Event_Delivered;
      Begin_Operation (S, Release_Operation, Releasing, Deadline, Budget, True); Accepted := True;
   end Release;
   procedure Acknowledge_Event (S : in out State; Caller : P.Process_ID;
      Lease : L.Lease_ID; Ticket : I.Ticket; Deadline : I.Tick; Budget : I.Limit;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Caller /= S.Owner or else Lease = L.No_Lease or else Lease /= S.Info.Lease or else
        Ticket = I.No_Ticket or else Ticket /= S.Info.Current_Event.Ticket or else
        S.Info.At_Phase /= Event_Delivered or else S.Reply_Owned then return; end if;
      Begin_Operation (S, Ack_Operation, Acknowledging, Deadline, Budget, True); Accepted := True;
   end Acknowledge_Event;
   procedure Poll (S : in out State; Deadline : I.Tick; Budget : I.Limit; Accepted : out Boolean) is
   begin
      Accepted := False; if S.Info.At_Phase /= Armed or else S.Reply_Owned then return; end if;
      Begin_Operation (S, Poll_Operation, Polling, Deadline, Budget, False); Accepted := True;
   end Poll;
   procedure Finish (S : in out State; Status : L.Completion_Status; Why : Failure; New_Phase : Phase) is
   begin
      S.Info.Reason := Why; S.Info.At_Phase := New_Phase; S.Op := No_Operation; S.Started := False;
      if S.Reply_Owned then S.Reply := (Status, S.Info.Lease); S.Reply_Ready := True; end if;
   end Finish;
   procedure Refuse (S : in out State; Why : Failure) is
   begin
      if S.Op = Arm_Operation then S.Info.Lease := L.No_Lease; end if;
      if S.Op = Bind_Operation then Reserved_Pins (S.Info.Pin) := False; S.Info.Pin := C.No_Pin; S.Owner := P.No_Process; end if;
      Finish (S, L.Refused, Why, S.Before);
   end Refuse;
   procedure Step (S : in out State) is
      Pin : C.Pin_Observation;
      Now : I.Tick;
      Available : Boolean;
      Lease : L.Lease_ID;
   begin
      if S.Op = No_Operation then return; end if;
      Pin := C.Observe_Pin (S.Info.Pin);
      if not Pin.Present or else C.Observe.Faulted or else Pin.Report.At_Phase = C.Engine.Faulted then
         Finish (S, (if S.Started then L.Uncertain else L.Refused),
            (if S.Started then Controller_Uncertain else Controller_Refused), Faulted); return;
      end if;
      if not S.Started then
         -- Exact-ticket retirement is already-masked local cleanup, no clock/IO.
         if not (S.Op = Release_Operation and then Pin.Report.At_Phase = C.Engine.Event_Ready) then
            Clock (Now, Available);
            if not Available then Refuse (S, Clock_Unavailable); return; end if;
            if Now < S.Previous then Refuse (S, Clock_Reversed); return; end if;
            S.Previous := Now;
            if Now >= S.Deadline then Refuse (S, Expired); return; end if;
         end if;
         if C.Observe.Active /= C.No_Pin then return; end if;
         if S.Op = Arm_Operation and then S.Info.Lease = L.No_Lease then
            Next_Lease (Lease, Available);
            if not Available or else Lease = L.No_Lease or else Lease <= Last_Allocated_Lease then Refuse (S, Lease_Unavailable); return; end if;
            Last_Allocated_Lease := Lease; S.Info.Lease := Lease;
         end if;
         case S.Op is
            when Bind_Operation => C.Pause (S.Info.Pin, S.Deadline, S.Budget, S.Info.Last_Controller, S.Info.Last_Engine);
            when Arm_Operation => C.Resume (S.Info.Pin, S.Deadline, S.Budget, S.Info.Last_Controller, S.Info.Last_Engine);
            when Poll_Operation => C.Poll (S.Info.Pin, S.Deadline, S.Budget, S.Info.Last_Controller, S.Info.Last_Engine);
            when Ack_Operation => C.Rearm (S.Info.Pin, S.Info.Current_Event.Ticket, S.Deadline, S.Budget, S.Info.Last_Controller, S.Info.Last_Engine);
            when Release_Operation =>
               if Pin.Report.At_Phase = C.Engine.Event_Ready then
                  C.Retire_Event (S.Info.Pin, S.Info.Current_Event.Ticket, S.Info.Last_Controller, S.Info.Last_Engine);
               else C.Pause (S.Info.Pin, S.Deadline, S.Budget, S.Info.Last_Controller, S.Info.Last_Engine);
               end if;
            when No_Operation => return;
         end case;
         if S.Info.Last_Controller = C.Busy then return; end if;
         if S.Info.Last_Controller /= C.Accepted then Refuse (S, Controller_Refused); return; end if;
         S.Started := True; Pin := C.Observe_Pin (S.Info.Pin);
      end if;
      case S.Op is
         when Bind_Operation | Release_Operation =>
            if Pin.Report.At_Phase = C.Engine.Held_Masked then
               Finish (S, L.Completed, None, Parked); S.Info.Lease := L.No_Lease; S.Info.Current_Event := (others => <>);
            end if;
         when Arm_Operation | Ack_Operation =>
            if Pin.Report.At_Phase = C.Engine.Listening then
               S.Info.Current_Event := (others => <>); Finish (S, L.Completed, None, Armed);
            end if;
         when Poll_Operation =>
            if Pin.Report.At_Phase = C.Engine.Listening then Finish (S, L.Completed, None, Armed);
            elsif Pin.Report.At_Phase = C.Engine.Event_Ready then
               if Pin.Report.Event = I.No_Ticket then Finish (S, L.Uncertain, Invalid_Event, Faulted);
               else
                  S.Info.Current_Event := (S.Info.Lease, Pin.Report.Event, S.Info.Pin);
                  Finish (S, L.Completed, None, Event_Retained);
               end if;
            end if;
         when No_Operation => null;
      end case;
   end Step;
end GPIO_Pin_Leases;
