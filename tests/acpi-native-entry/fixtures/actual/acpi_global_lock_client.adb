pragma Ada_2022;
package body ACPI_Global_Lock_Client with SPARK_Mode is
   use type Interfaces.Unsigned_64, W.Time_Instant, W.Delivery, P.Reply_Kind, P.Lock_Outcome;
   function Consistent (State : Context) return Boolean is
     ((if State.Usable then State.Stage = Held)
      and then (if State.Stage in Held | Uncertain then P.Valid (State.Held_Ticket)));
   function Observe (State : Context) return Statistics is
     ((State.Stage, State.Working, State.Usable, State.Stage in Held | Uncertain));
   function Ticket (State : Context) return P.Ticket is (State.Held_Ticket);
   function Bound_Peer (State : Context) return Peer is (State.Destination);
   procedure Stop_Using (State : in out Context) is
   begin if not State.Working then State.Usable := False; end if; end Stop_Using;
   function Finite (Deadline : W.Time_Instant) return Boolean is
     (Deadline /= 0 and then Deadline /= W.No_Finite_Deadline);
   procedure Initialize (State : in out Context; Destination : Peer; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif State.Stage /= Fresh then Result := Already_Initialized;
      elsif Destination = No_Peer then Result := Invalid_Peer;
      else State.Destination := Destination; State.Stage := Ready; Result := Initialized;
      end if;
   end Initialize;
   procedure Sample
     (State : in out Context; IPC : W.Time_Instant; Check_IPC : Boolean; Result : out Outcome) is
      Now : Interfaces.Unsigned_64;
      Available : Boolean;
      IPC_Now : W.Time_Instant;
   begin
      if State.Clock_Broken then Result := Clock_Reversed; return; end if;
      Clock (Now, Available);
      if not Available then Result := Clock_Unavailable; return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then
         State.Clock_Broken := True; Result := Clock_Reversed; return;
      end if;
      State.Seen_Clock := True; State.Last_Clock := Now;
      if Now >= State.Data_Deadline then Result := Expired; return; end if;
      if Check_IPC then
         IPC_Clock (IPC_Now, Available);
         if not Available then Result := Clock_Unavailable; return; end if;
         if State.Seen_IPC_Clock and then IPC_Now < State.Last_IPC_Clock then
            State.Clock_Broken := True; Result := Clock_Reversed; return;
         end if;
         State.Seen_IPC_Clock := True; State.Last_IPC_Clock := IPC_Now;
         if IPC_Now >= IPC then Result := Expired; return; end if;
      end if;
      Result := Acquired;
   end Sample;
   procedure Clear (State : in out Context) is
   begin
      State.Stage := Ready; State.Usable := False; State.Held_Ticket := P.No_Ticket;
      State.Release_Attempted := False;
   end Clear;
   function Owns (Item : P.Lock_Outcome) return Boolean is
     (Item in P.Acquired | P.Acquired_After_Deadline | P.Acquired_Clock_Failed);
   function Did_Not_Acquire (Item : P.Lock_Outcome) return Boolean is
     (Item in P.Waiting_For_Firmware | P.Contended | P.Expired | P.Clock_Failed |
       P.Clock_Reversed | P.Invalid_Deadline | P.Unauthorized | P.Not_Ready |
       P.Lease_Exhausted | P.Read_Unavailable | P.Atomic_Refused | P.Reentered);
   procedure Acquire
     (State : in out Context; Channel : in out Transport;
      Lease_Deadline_Microseconds : Interfaces.Unsigned_64;
      IPC_Deadline_Milliseconds : W.Time_Instant; Budget : P.Attempt_Budget; Result : out Outcome) is
      Reply : W.Packet;
      Delivery : W.Delivery;
      Decoded : P.Reply;
      procedure Execute is
      begin
         State.Data_Deadline := Lease_Deadline_Microseconds;
         Sample (State, IPC_Deadline_Milliseconds, True, Result); if Result /= Acquired then return; end if;
         State.Nonce := State.Nonce + 1;
         Exchange (Channel, State.Destination, IPC_Deadline_Milliseconds,
           P.Prepare_Request (State.Nonce, State.Data_Deadline, Budget), Reply, Delivery);
         -- Prepare is metadata-only, even if its reply is lost. No acquire
         -- frame is sent until a valid ticket has actually been received.
         if Delivery /= W.Reply_Received then
            Result := (if Delivery = W.Not_Dispatched then Not_Sent else Completion_Unknown); return;
         end if;
         Decoded := P.Decode_Reply (Reply, P.Prepare);
         if not Decoded.Valid then Result := Bad_Reply; return;
         elsif Decoded.Kind /= P.Ready then Result := Backend_Refused; return; end if;
         State.Held_Ticket := Decoded.Token;
         Sample (State, IPC_Deadline_Milliseconds, True, Result); if Result /= Acquired then return; end if;
         State.Stage := Uncertain;
         Exchange (Channel, State.Destination, IPC_Deadline_Milliseconds,
           P.Ticket_Request (P.Acquire, State.Held_Ticket), Reply, Delivery);
         if Delivery = W.Not_Dispatched then Clear (State); Result := Not_Sent; return;
         elsif Delivery /= W.Reply_Received then Result := Completion_Unknown; return; end if;
         Decoded := P.Decode_Reply (Reply, P.Acquire);
         if not Decoded.Valid then Result := Bad_Reply; return; end if;
         if Decoded.Kind not in P.Recorded | P.Cached then Result := No_Evidence; return; end if;
         if Owns (Decoded.Outcome) then
            State.Stage := Held;
            Sample (State, IPC_Deadline_Milliseconds, True, Result);
            State.Usable := Decoded.Outcome = P.Acquired and then Result = Acquired;
            Result := (if State.Usable then Acquired else Needs_Cleanup);
         elsif Did_Not_Acquire (Decoded.Outcome) then Clear (State); Result := Backend_Refused;
         else Result := Completion_Unknown;
         end if;
      end Execute;
   begin
      Result := Not_Ready;
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Ready then return; end if;
      if not Finite (IPC_Deadline_Milliseconds) or else Lease_Deadline_Microseconds in 0 | Interfaces.Unsigned_64'Last
      then Result := Invalid_Deadline; return; end if;
      if State.Nonce = Interfaces.Unsigned_64'Last then Result := Exhausted; return; end if;
      State.Usable := False; State.Release_Attempted := False; State.Held_Ticket := P.No_Ticket;
      State.Working := True; Execute; State.Working := False;
   end Acquire;
   procedure Check_Usable (State : in out Context; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif State.Stage /= Held or else not State.Usable then Result := Not_Ready;
      else
         State.Working := True; Sample (State, 0, False, Result); State.Working := False;
         if Result /= Acquired then State.Usable := False; end if;
      end if;
   end Check_Usable;
   procedure Release
     (State : in out Context; Channel : in out Transport;
      IPC_Deadline_Milliseconds : W.Time_Instant; Result : out Outcome) is
      Reply : W.Packet;
      Delivery : W.Delivery;
      Decoded : P.Reply;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage not in Held | Uncertain then Result := Not_Ready; return; end if;
      State.Usable := False;
      if not Finite (IPC_Deadline_Milliseconds) then Result := Invalid_Deadline; return; end if;
      if State.Release_Attempted then Result := Waiting; return; end if;
      State.Working := True; State.Release_Attempted := True;
      Exchange (Channel, State.Destination, IPC_Deadline_Milliseconds,
        P.Ticket_Request (P.Release, State.Held_Ticket), Reply, Delivery);
      if Delivery = W.Not_Dispatched then State.Release_Attempted := False; Result := Not_Sent;
      elsif Delivery /= W.Reply_Received then State.Stage := Uncertain; Result := Completion_Unknown;
      else
         Decoded := P.Decode_Reply (Reply, P.Release);
         if not Decoded.Valid then State.Stage := Uncertain; Result := Bad_Reply;
         elsif Decoded.Kind in P.Recorded | P.Cached and then Decoded.Outcome = P.Released then
            Clear (State); Result := Released;
         else Result := Waiting;
         end if;
      end if;
      State.Working := False;
   end Release;
   procedure Recover
     (State : in out Context; Channel : in out Transport;
      IPC_Deadline_Milliseconds : W.Time_Instant; Result : out Outcome) is
      Reply : W.Packet;
      Delivery : W.Delivery;
      Decoded : P.Reply;
   begin
      if State.Working then Result := Reentered; return; end if;
      if State.Stage not in Held | Uncertain then Result := Not_Ready; return; end if;
      State.Usable := False;
      if not Finite (IPC_Deadline_Milliseconds) then Result := Invalid_Deadline; return; end if;
      State.Working := True; State.Usable := False; Result := No_Evidence;
      Exchange (Channel, State.Destination, IPC_Deadline_Milliseconds,
        P.Ticket_Request (P.Query_Acquisition, State.Held_Ticket), Reply, Delivery);
      if Delivery = W.Reply_Received then
         Decoded := P.Decode_Reply (Reply, P.Query_Acquisition);
         if Decoded.Valid and then Decoded.Kind = P.Cached then
            if Did_Not_Acquire (Decoded.Outcome) then Clear (State); Result := Backend_Refused;
            elsif Owns (Decoded.Outcome) then State.Stage := Held; Result := Needs_Cleanup;
            end if;
         end if;
      end if;
      if State.Stage in Held | Uncertain then
         Exchange (Channel, State.Destination, IPC_Deadline_Milliseconds,
           P.Ticket_Request (P.Query_Termination, State.Held_Ticket), Reply, Delivery);
         if Delivery = W.Reply_Received then
            Decoded := P.Decode_Reply (Reply, P.Query_Termination);
            if Decoded.Valid and then Decoded.Kind = P.Cached and then Decoded.Outcome = P.Released then
               Clear (State); Result := Released;
            end if;
         end if;
      end if;
      State.Working := False;
   end Recover;
end ACPI_Global_Lock_Client;
