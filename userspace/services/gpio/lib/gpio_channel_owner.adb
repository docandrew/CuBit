pragma Ada_2022;
package body GPIO_Channel_Owner with SPARK_Mode is
   package L renames Leases.L;
   use type P.Process_ID, L.Completion_Status, W.Control_Kind, W.Word, I.Ticket, Leases.Phase;
   function Observe (S : State) return Report is
     ((S.Binding, S.Life, S.Pending, S.Ready_Reply, S.Lost, S.Completion,
       Leases.Observe (S.Pin_State)));
   function Quiescent (S : State) return Boolean is
     (not S.Pending and then not S.Cleanup_Started and then
       ((S.Binding = Unconfigured and then S.Life = Stopped) or else
        (S.Binding = Ready and then Leases.Quiescent (S.Pin_State) and then
          (S.Life = Stopped or else (S.Life = Running and then not S.Lost)))));
   function Needs_Step (S : State) return Boolean is
     (S.Life not in Stopped | Unresolved and then
       (S.Binding = Binding or else S.Life = Draining or else S.Pending or else
        S.Cleanup_Started or else Leases.Has_Event (S.Pin_State) or else
        Leases.Observe (S.Pin_State).At_Phase in Leases.Binding | Leases.Arming |
           Leases.Polling | Leases.Acknowledging | Leases.Releasing));
   function Polling_Needed (S : State) return Boolean is
     (S.Binding = Ready and then S.Life = Running and then not S.Pending and then not S.Lost
        and then Leases.Observe (S.Pin_State).At_Phase = Leases.Armed);
   procedure Configure (S : in out State; Pin : C.Pin_ID; Owner : P.Process_ID;
      Sink : Event_Sink; Deadline : I.Tick; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Binding /= Unconfigured or else S.Life /= Running then return; end if;
      Leases.Bind (S.Pin_State, Pin, Owner, Deadline, Operation_Budget, Accepted);
      if Accepted then S.Owner := Owner; S.Sink := Sink; S.Binding := Binding; end if;
   end Configure;
   function Request_Lease (Item : W.Request) return W.Word is
     (case Item.Kind is when W.Arm => 0, when W.Release_Pin => Item.Lease_To_Release,
        when W.Acknowledge => Item.Lease_To_Ack);
   function Request_Generation (Item : W.Request) return W.Word is
     (if Item.Kind = W.Acknowledge then Item.Generation_To_Ack else 0);
   procedure Offer (S : in out State; Caller : P.Process_ID;
      Item : W.Request; Reply : Reply_Authority; Result : out Admission) is
      Started : Boolean;
      Limit : constant I.Tick := I.Tick (W.Encode_Deadline (Item.Limit));
   begin
      if Caller /= S.Owner or else not P.Is_Process (Caller) then Result := Unauthorized;
      elsif S.Lost then Result := Reply_Lost;
      elsif S.Binding /= Ready or else S.Life /= Running then Result := Not_Ready;
      elsif S.Pending then Result := Busy;
      else
         S.Request := Item; S.Reply := Reply; S.Pending := True; S.Ready_Reply := False;
         S.Completion := (Item.Kind, W.Refused, Request_Lease (Item), Request_Generation (Item));
         case Item.Kind is
            when W.Arm => Leases.Arm (S.Pin_State, Caller, Limit, Operation_Budget, Started);
            when W.Release_Pin => Leases.Release (S.Pin_State, Caller,
               Item.Lease_To_Release, Limit, Operation_Budget, Started);
            when W.Acknowledge => Leases.Acknowledge_Event (S.Pin_State, Caller,
               Item.Lease_To_Ack, I.Ticket (Item.Generation_To_Ack), Limit, Operation_Budget, Started);
         end case;
         S.Ready_Reply := not Started; Result := Accepted;
      end if;
   end Offer;
   procedure Poll (S : in out State; Deadline : I.Tick; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Binding /= Ready or else S.Life /= Running or else S.Pending or else S.Lost then return; end if;
      Leases.Poll (S.Pin_State, Deadline, Operation_Budget, Accepted);
   end Poll;
   procedure Request_Stop (S : in out State; Cleanup_Deadline : I.Tick) is
   begin
      if S.Life /= Running then return; end if;
      S.Life := Draining; S.Cleanup_Deadline := Cleanup_Deadline;
   end Request_Stop;
   procedure Detach (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Binding /= Ready or else S.Life /= Stopped or else not Quiescent (S) then return; end if;
      Leases.Unbind (S.Pin_State, Accepted);
      if not Accepted then return; end if;
      S.Owner := P.No_Process; S.Binding := Unconfigured; S.Life := Running;
      S.Pending := False; S.Ready_Reply := False; S.Lost := False;
      S.Cleanup_Started := False; S.Cleanup_Lease := 0; S.Cleanup_Deadline := 0;
      S.Completion := (W.Arm, W.Refused, 0, 0);
   end Detach;
   procedure Drain (S : in out State) is
      Info : constant Leases.Report := Leases.Observe (S.Pin_State);
      Started : Boolean;
   begin
      if S.Life /= Draining or else S.Pending or else S.Cleanup_Started then return; end if;
      if S.Binding = Unconfigured then S.Life := Stopped;
      elsif S.Binding = Failed or else Info.At_Phase = Leases.Faulted then S.Life := Unresolved;
      elsif S.Binding = Binding then null;
      elsif Leases.Quiescent (S.Pin_State) then S.Life := Stopped;
      elsif Info.At_Phase in Leases.Armed | Leases.Event_Retained | Leases.Event_Delivered then
         S.Cleanup_Lease := Info.Lease;
         Leases.Release (S.Pin_State, S.Owner, Info.Lease, S.Cleanup_Deadline,
            Operation_Budget, Started);
         if Started then S.Cleanup_Started := True; else S.Life := Unresolved; end if;
      elsif Info.At_Phase in Leases.Binding | Leases.Arming | Leases.Polling |
         Leases.Acknowledging | Leases.Releasing then null;
      else S.Life := Unresolved;
      end if;
   end Drain;
   procedure Step (S : in out State) is
      Reply : L.Reply;
      Sent : Boolean;
      Event : Leases.Event;
   begin
      if S.Life in Stopped | Unresolved then return; end if;
      if S.Binding not in Binding | Ready then Drain (S); return; end if;
      Leases.Step (S.Pin_State);
      if Leases.Has_Reply (S.Pin_State) then
         Leases.Take_Reply (S.Pin_State, Reply);
         if S.Binding = Binding then
            S.Binding := (if Reply.Status = L.Completed then Ready else Failed);
         elsif S.Cleanup_Started then
            S.Cleanup_Started := False;
            S.Life := (if Reply.Status = L.Completed and then Reply.Lease = S.Cleanup_Lease
               and then Leases.Quiescent (S.Pin_State) then Stopped else Unresolved);
         elsif S.Pending and then not S.Ready_Reply then
            S.Completion := (S.Request.Kind,
               (case Reply.Status is when L.Completed => W.Completed,
                when L.Refused => W.Refused, when L.Uncertain | L.Pending => W.Uncertain),
               Reply.Lease, Request_Generation (S.Request));
            if not W.Valid (S.Completion) then
               S.Completion := (S.Request.Kind, W.Internal_Mismatch,
                  Reply.Lease, (if Reply.Lease = 0 then 0 else Request_Generation (S.Request)));
               S.Lost := True;
            end if;
            S.Ready_Reply := True;
         else
            -- No saved requester owns this reply: never invent a destination.
            S.Lost := True;
         end if;
      end if;
      if S.Pending and then S.Ready_Reply then
         Answer (S.Reply, S.Completion, Sent);
         S.Pending := False; S.Ready_Reply := False;
         if not Sent then S.Lost := True; end if;
      end if;
      Drain (S);
      if S.Binding = Ready and then S.Life = Running and then not S.Pending and then not S.Lost
         and then Leases.Has_Event (S.Pin_State)
      then
         Event := Leases.Head (S.Pin_State);
         if Event.Lease = L.No_Lease or else Event.Ticket = I.No_Ticket then
            S.Lost := True;
         else
            Try_Event (S.Sink, (Event.Lease, W.Word (Event.Ticket)), Sent);
            if Sent then Leases.Delivered (S.Pin_State); end if;
         end if;
      end if;
   end Step;
end GPIO_Channel_Owner;
