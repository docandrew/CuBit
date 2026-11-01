pragma Ada_2022;
package body ACPI_Locked_NVS_Client with SPARK_Mode is
   use type W.Time_Instant, W.Delivery, P.Reply_Kind, P.Read_Outcome, P.Record_ID,
     P.Descriptor_ID, P.Read_Sequence, P.Geometry, P.Lease_Ticket, Lease.Outcome, Lease.Phase;
   function Consistent (State : Context) return Boolean is
     (if State.Pending_Read then State.Stage = Uncertain and then State.Bound
       and then Lease.P.Valid (State.Ticket)
       and then State.Descriptor /= P.No_Descriptor and then State.Sequence /= P.No_Read);
   function Observe (State : Context) return Statistics is
     ((State.Stage, State.Working, State.Pending_Read, State.Remote));
   function Finite (Deadline : W.Time_Instant) return Boolean is
     (Deadline /= 0 and then Deadline /= W.No_Finite_Deadline);
   procedure Time_Remains (State : in out Context; Deadline : W.Time_Instant; Result : out Outcome) is
      Now : W.Time_Instant;
      Available : Boolean;
   begin
      if State.Clock_Broken then Result := Clock_Reversed; return; end if;
      IPC_Clock (Now, Available);
      if not Available then Result := Clock_Unavailable; return; end if;
      if State.Seen_Clock and then Now < State.Last_Clock then
         State.Clock_Broken := True; Result := Clock_Reversed; return;
      end if;
      State.Seen_Clock := True; State.Last_Clock := Now;
      Result := (if Now >= Deadline then Expired else Read_Complete);
   end Time_Remains;
   procedure Read
     (State : in out Context; Held : in out Lease.Context; Channel : in out Transport;
      ID : P.Record_ID; Datum : P.Geometry; IPC_Deadline : W.Time_Instant;
      Value : out Interfaces.Unsigned_64; Result : out Outcome) is
      Wire_Reply : W.Packet;
      Delivery : W.Delivery;
      Reply : P.Reply;
      Access_Status : Lease.Outcome;
      Token : constant P.Lease_Ticket := Lease.Ticket (Held);
      procedure Execute is
      begin
         Lease.Check_Usable (Held, Access_Status);
         if Access_Status /= Lease.Acquired then Result := Not_Ready; return; end if;
         if State.Bound and then State.Destination /= Lease.Bound_Peer (Held) then Result := Peer_Changed; return; end if;
         if not State.Bound then State.Destination := Lease.Bound_Peer (Held); State.Bound := True; end if;
         if State.Ticket /= Token then
            State.Ticket := Token; State.Descriptor := P.No_Descriptor; State.Sequence := P.No_Read;
         end if;
         Time_Remains (State, IPC_Deadline, Result); if Result /= Read_Complete then return; end if;
         Exchange (Channel, State.Destination, IPC_Deadline, P.Select_Request (Token, ID, Datum), Wire_Reply, Delivery);
         if Delivery /= W.Reply_Received then
            Result := (if Delivery = W.Not_Dispatched then Not_Sent else Completion_Unknown); return;
         end if;
         Reply := P.Decode_Reply (P.Select_Datum, Wire_Reply);
         if not Reply.Valid then Result := Bad_Reply; return; end if;
         if Reply.Kind /= P.Prepared then Result := Backend_Refused; return; end if;
         if not P.Matches_Selection (Reply, ID, Datum) then Result := Bad_Reply; return; end if;
         if Reply.Descriptor = State.Descriptor then
            if State.ID /= ID or else State.Datum /= Datum then Result := Bad_Reply; return; end if;
         else State.Descriptor := Reply.Descriptor; State.Sequence := P.No_Read;
         end if;
         State.ID := ID; State.Datum := Datum;
         Lease.Check_Usable (Held, Access_Status);
         if Access_Status /= Lease.Acquired then Result := Not_Ready; return; end if;
         Time_Remains (State, IPC_Deadline, Result); if Result /= Read_Complete then return; end if;
         if State.Sequence = P.Read_Sequence'Last then Result := Exhausted; return; end if;
         State.Sequence := State.Sequence + 1; State.Pending_Read := True; State.Remote := P.No_Data;
         Exchange (Channel, State.Destination, IPC_Deadline,
           P.Read_Request (P.Read_Datum, Token, State.Descriptor, State.Sequence), Wire_Reply, Delivery);
         if Delivery = W.Not_Dispatched then State.Pending_Read := False; Result := Not_Sent; return;
         elsif Delivery /= W.Reply_Received then State.Stage := Uncertain; Result := Completion_Unknown; return; end if;
         Reply := P.Decode_Reply (P.Read_Datum, Wire_Reply, Datum.Width);
         if not Reply.Valid or else not P.Matches_Read (Reply, State.Descriptor, State.Sequence) then
            State.Stage := Uncertain; Result := Bad_Reply; return;
         end if;
         State.Pending_Read := False; State.Remote := Reply.Outcome;
         if Reply.Outcome /= P.Done then Result := Backend_Refused; return; end if;
         Lease.Check_Usable (Held, Access_Status);
         Time_Remains (State, IPC_Deadline, Result);
         if Access_Status /= Lease.Acquired or else Result /= Read_Complete then Result := Late_Completed; return; end if;
         Value := Reply.Value; Result := Read_Complete;
      end Execute;
   begin
      Value := 0; Result := Not_Ready;
      if State.Working then Result := Reentered; return; end if;
      if State.Stage /= Ready then Lease.Stop_Using (Held); return; end if;
      if not Finite (IPC_Deadline) then Result := Invalid_Deadline;
      elsif ID = P.No_Record or else not P.Valid (Datum) then Result := Invalid_Request;
      else
         State.Working := True; Execute; State.Working := False;
         if Result = Read_Complete then return; end if;
      end if;
      if State.Stage /= Uncertain then State.Stage := Failed; end if;
      Lease.Stop_Using (Held);
   end Read;
   procedure Query_Evidence
     (State : in out Context; Held : in out Lease.Context; Channel : in out Transport;
      IPC_Deadline : W.Time_Instant; Result : out Outcome) is
      Wire_Reply : W.Packet;
      Delivery : W.Delivery;
      Reply : P.Reply;
   begin
      if State.Working then Result := Reentered; return; end if;
      if not State.Pending_Read or else not State.Bound then Result := No_Evidence; return; end if;
      if State.Destination /= Lease.Bound_Peer (Held) then Result := Peer_Changed; return; end if;
      if not Finite (IPC_Deadline) then Result := Invalid_Deadline; return; end if;
      State.Working := True; Result := No_Evidence; Lease.Stop_Using (Held);
      Exchange (Channel, State.Destination, IPC_Deadline,
        P.Read_Request (P.Query_Read, State.Ticket, State.Descriptor, State.Sequence), Wire_Reply, Delivery);
      if Delivery = W.Reply_Received then
         Reply := P.Decode_Reply (P.Query_Read, Wire_Reply, State.Datum.Width);
         if Reply.Valid and then P.Matches_Read (Reply, State.Descriptor, State.Sequence) then
            State.Pending_Read := False; State.Remote := Reply.Outcome; State.Stage := Failed; Result := Evidence_Received;
         end if;
      end if;
      State.Working := False;
   end Query_Evidence;
   procedure Retire (State : in out Context; Held : Lease.Context; Result : out Outcome) is
   begin
      if State.Working then Result := Reentered;
      elsif Lease.Observe (Held).Working or else Lease.Observe (Held).Stage /= Lease.Ready
        or else Lease.Observe (Held).Cleanup_Required then Result := Not_Ready;
      elsif State.Bound and then State.Destination /= Lease.Bound_Peer (Held) then Result := Peer_Changed;
      elsif State.Clock_Broken then Result := Clock_Reversed;
      else
         State.Stage := Ready; State.Pending_Read := False; State.Remote := P.No_Data;
         State.Ticket := (0, 0); State.Descriptor := P.No_Descriptor; State.Sequence := P.No_Read; Result := Retired;
      end if;
   end Retire;
end ACPI_Locked_NVS_Client;
