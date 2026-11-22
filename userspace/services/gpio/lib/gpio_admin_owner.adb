pragma Ada_2022;
with GPIO_Channel_Badges;
package body GPIO_Admin_Owner with SPARK_Mode is
   use type A.Decode_Status, A.Request_Kind, Registry.Admission, Registry.Phase;
   use type Registry.Channel_ID, P.Process_ID, W.Word;
   function Observe (S : State) return Report is
     ((S.At_Phase, S.Receipt_Owned, S.Reply_Owned, S.Stopping, S.Delivered, S.Last_Status, S.ID));
   function Quiescent (S : State) return Boolean is
     (S.At_Phase = Idle and then not S.Receipt_Owned and then not S.Reply_Owned);
   procedure Request_Stop (S : in out State) is
   begin S.Stopping := True; end Request_Stop;
   procedure Dispose (S : in out State) is
      Closed : Boolean;
   begin
      if not S.Receipt_Owned then return; end if;
      Discard (S.Receipt, Closed);
      if Closed then S.Receipt_Owned := False; else S.At_Phase := Quarantined; end if;
   end Dispose;
   procedure Finish_Saved (S : in out State; Status : W.Completion_Status) is
   begin
      S.Last_Status := Status;
      if S.Reply_Owned then Answer_Saved (S.Reply, S.Label, Status, S.Delivered); S.Reply_Owned := False; end if;
   end Finish_Saved;
   procedure Reject_Current (S : in out State; Caller : P.Process_ID;
      Label : W.Operation; Status : W.Completion_Status) is
   begin S.Last_Status := Status; Answer_Current (Caller, Label, Status, S.Delivered); end Reject_Current;
   function Refusal (Result : Registry.Admission) return W.Completion_Status is
     (case Result is
      when Registry.Not_Ready | Registry.Already_Bound | Registry.Busy => W.Busy,
      when Registry.Invalid_Owner | Registry.Invalid_Badge | Registry.Unknown_Pin |
        Registry.Trigger_Mismatch | Registry.Wrong_Binding => W.Invalid_Channel,
      when Registry.Accepted | Registry.Bind_Refused => W.Refused);
   procedure Handle (S : in out State; Caller : P.Process_ID;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words;
      Frame_Valid, Has_Offer : Boolean; Setup_Deadline, Cleanup_Deadline : I.Tick) is
      Parsed : A.Decoding;
      Sink : Event_Sink;
      Valid, Saved : Boolean;
      Result : Registry.Admission;
   begin
      if not P.Is_Process (Caller) or else not Authorized (Caller) then Reject_Current (S, Caller, Label, W.Unauthorized); return; end if;
      if S.At_Phase /= Idle or else S.Stopping then Reject_Current (S, Caller, Label, W.Busy); return; end if;
      if Frame_Valid then Parsed := A.Decode (Label, Protocol, Words, Has_Offer); end if;
      if Parsed.Status /= A.Decoded then Reject_Current (S, Caller, Label, W.Invalid_Frame); return; end if;
      S.ID := Registry.No_Channel; S.Cleanup_Deadline := Cleanup_Deadline;
      if Parsed.Value.Kind = A.Admit_Sink then
         Take_Offer (Caller, S.Receipt, S.Receipt_Owned);
         if not S.Receipt_Owned then Reject_Current (S, Caller, Label, W.Refused); return; end if;
         Inspect (S.Receipt, Sink, S.Owner, S.Badge, Valid);
         if not Valid or else not P.Is_Process (S.Owner) or else not GPIO_Channel_Badges.Valid_Word (S.Badge) then
            Dispose (S); Reject_Current (S, Caller, Label, (if S.Receipt_Owned then W.Uncertain else W.Refused)); return;
         end if;
      else S.Owner := Parsed.Value.Owner; S.Badge := Parsed.Value.Badge;
      end if;
      Save (S.Reply, Saved);
      if not Saved then
         Dispose (S); Reject_Current (S, Caller, Label, (if S.Receipt_Owned then W.Uncertain else W.Busy)); return;
      end if;
      S.Reply_Owned := True; S.Label := Label;
      if Parsed.Value.Kind = A.Admit_Sink then
         Registry.Enroll (S.Owner, S.Badge, Sink, Setup_Deadline, S.ID, Result);
         if Result = Registry.Accepted then
            S.Receipt_Owned := False; S.At_Phase := Await_Admission; return;
         end if;
         Dispose (S);
      else
         Registry.Retire (S.Owner, S.Badge, Cleanup_Deadline, S.ID, Result);
         if Result = Registry.Accepted then S.At_Phase := Await_Retirement; return; end if;
      end if;
      Finish_Saved (S, (if S.Receipt_Owned then W.Uncertain else Refusal (Result)));
   end Handle;
   procedure Consume_Retirement (S : in out State) is
      Accepted : Boolean;
   begin
      Registry.Acknowledge_Retirement (S.ID, Accepted);
      S.At_Phase := (if Accepted then Idle else Quarantined);
   end Consume_Retirement;
   procedure Step (S : in out State) is
      Info : Registry.Binding_Report;
      Result : Registry.Admission;
      ID : Registry.Channel_ID;
   begin
      if S.At_Phase not in Await_Admission | Await_Retirement | Drain_Lost_Admission then return; end if;
      if Registry.Observe.Layout_Changed then
         -- The registry freezes if its owned slot layout changes externally.
         -- Stop awaiting progress it cannot make; retain the sink for supervision.
         Finish_Saved (S, W.Uncertain); S.At_Phase := Quarantined; return;
      end if;
      if S.ID = Registry.No_Channel or else S.ID > Registry.Observe.Registered then
         Finish_Saved (S, W.Internal_Mismatch); S.At_Phase := Quarantined; return;
      end if;
      Info := Registry.Observe_Channel (S.ID);
      if Info.Owner /= S.Owner or else Info.Badge /= S.Badge then
         Finish_Saved (S, W.Internal_Mismatch); S.At_Phase := Quarantined; return;
      end if;
      if Info.At_Phase in Registry.Close_Failed | Registry.Unresolved then
         Finish_Saved (S, W.Uncertain); S.At_Phase := Quarantined;
      elsif Info.At_Phase = Registry.Retired then
         if S.At_Phase = Await_Admission then Finish_Saved (S, W.Refused);
         elsif S.At_Phase = Await_Retirement then Finish_Saved (S, W.Completed); end if;
         Consume_Retirement (S);
      elsif S.At_Phase = Await_Admission and then Info.At_Phase = Registry.Live then
         Finish_Saved (S, W.Completed);
         if S.Delivered then S.At_Phase := Idle;
         else
            Registry.Retire (S.Owner, S.Badge, S.Cleanup_Deadline, ID, Result);
            if Result = Registry.Accepted and then ID = S.ID then S.At_Phase := Drain_Lost_Admission;
            else S.At_Phase := Quarantined; end if;
         end if;
      elsif Info.At_Phase not in Registry.Registering | Registry.Binding | Registry.Retiring then
         Finish_Saved (S, W.Internal_Mismatch); S.At_Phase := Quarantined;
      end if;
   end Step;
end GPIO_Admin_Owner;
