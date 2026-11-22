pragma Ada_2022;
with ACPI_Inventory_Protocol;
with ACPI_Global_Lock_Protocol;
package body ACPI_Locked_NVS with SPARK_Mode is
   use type Interfaces.Unsigned_64, P.Lease_Ticket, P.Record_ID, P.Descriptor_ID,
     P.Read_Sequence, P.Geometry, P.Read_Outcome, P.Reply_Kind, Owner.Outcome, Requests.Reply_Kind;
   function Empty (Kind : P.Reply_Kind) return P.Reply is ((Valid => True, Kind => Kind, others => <>));
   function Selection
     (State : Context; Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      Descriptor : P.Descriptor_ID) return P.Reply_Kind is
     (if State.Working then P.Reentered
      elsif Scope = No_Scope or else Caller = No_Principal then P.Denied
      elsif not State.Present or else Lease /= State.Lease or else Descriptor = P.No_Descriptor
        or else Descriptor /= State.Descriptor then P.Stale
      elsif Scope /= State.Scope or else Caller /= State.Caller then P.Denied
      else P.Prepared);
   function Original (State : Context) return P.Reply is
     ((Valid => True, Kind => P.Cached, Descriptor => State.Descriptor,
       Sequence => State.Last_Read, Outcome => State.Outcome, Value => State.Value, others => <>));
   procedure Check_Access
     (Lock : in out Requests.Context; Scope : Scope_Tag; Caller : Principal;
      Lease : P.Lease_Ticket; Accepted : out Boolean) is
      Reply : Requests.Reply;
   begin
      Requests.Check_Access (Lock, Scope, Caller, (Lease.Domain, Lease.Sequence), Reply);
      Accepted := Reply.Kind = Requests.Checked and then Reply.Outcome = Owner.Configured;
   end Check_Access;
   procedure Select_Datum
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      ID : P.Record_ID; Datum : P.Geometry; Result : out P.Reply) is
      Epoch : Interfaces.Unsigned_64;
      Accepted : Boolean;
      procedure Execute is
      begin
         Check_Access (Lock, Scope, Caller, Lease, Accepted);
         if not Accepted then Result := Empty (P.Denied); return; end if;
         if State.Present and then State.Lease = Lease and then State.Lease_Closed then
            Result := Empty (P.Unavailable); return;
         end if;
         if State.Present and then Scope = State.Scope and then Caller = State.Caller
           and then Lease = State.Lease and then ID = State.ID and then Datum = State.Datum
         then Result := (Valid => True, Kind => P.Prepared, Descriptor => State.Descriptor,
           ID => State.ID, Datum => State.Datum, others => <>); return; end if;
         if State.Issued >= Maximum_Descriptors then Result := Empty (P.Exhausted); return; end if;
         Describe_Datum (Scope, ID, Datum, Epoch, Accepted);
         if not Accepted or else Epoch = 0 then Result := Empty (P.Denied); return; end if;
         Check_Access (Lock, Scope, Caller, Lease, Accepted);
         if not Accepted then Result := Empty (P.Denied); return; end if;
         State.Issued := State.Issued + 1; State.Present := True;
         State.Caller := Caller; State.Scope := Scope; State.Lease := Lease;
         State.ID := ID; State.Datum := Datum; State.Epoch := Epoch;
         State.Descriptor := P.Descriptor_ID (State.Issued);
         State.Last_Read := P.No_Read; State.Saved := False; State.Lease_Closed := False;
         State.Outcome := P.No_Data; State.Value := 0;
         Result := (Valid => True, Kind => P.Prepared, Descriptor => State.Descriptor,
           ID => State.ID, Datum => State.Datum, others => <>);
      end Execute;
   begin
      Result := Empty (P.Denied);
      if State.Working then Result := Empty (P.Reentered); return; end if;
      if State.Stopped then Result := Empty (P.Unavailable); return; end if;
      if ID = P.No_Record or else not P.Valid (Datum) or else not ACPI_Global_Lock_Protocol.Valid (Lease) then
         Result := Empty (P.Invalid_Request); return;
      end if;
      State.Working := True; Execute; State.Working := False;
   end Select_Datum;
   procedure Read
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      Descriptor : P.Descriptor_ID; Sequence : P.Read_Sequence; Result : out P.Reply) is
      Kind : constant P.Reply_Kind := Selection (State, Scope, Caller, Lease, Descriptor);
      Accepted, Completed : Boolean;
      Value : Interfaces.Unsigned_64;
   begin
      Result := Empty (Kind); if Kind /= P.Prepared then return; end if;
      if Sequence = P.No_Read then Result := Empty (P.Invalid_Request); return; end if;
      if State.Saved and then Sequence = State.Last_Read then Result := Original (State); return; end if;
      if Sequence <= State.Last_Read then Result := Empty (P.Stale); return; end if;
      if State.Stopped or else State.Lease_Closed then Result := Empty (P.Unavailable); return; end if;
      if State.Last_Read = P.Read_Sequence'Last then Result := Empty (P.Exhausted); return; end if;
      if Sequence /= State.Last_Read + 1 then Result := Empty (P.Invalid_Request); return; end if;
      State.Working := True;
      State.Last_Read := Sequence; State.Saved := True; State.Outcome := P.Access_Refused; State.Value := 0;
      Check_Access (Lock, Scope, Caller, Lease, Accepted);
      if Accepted then
         Read_Datum (Scope, State.Epoch, State.ID, State.Datum, Value, Completed);
         if not Completed or else Value > ACPI_Inventory_Protocol.Width_Mask (State.Datum.Width) then
            State.Stopped := True; State.Outcome := P.Read_Failed;
         else
            Check_Access (Lock, Scope, Caller, Lease, Accepted);
            if Accepted then State.Outcome := P.Done; State.Value := Value;
            else State.Outcome := P.Late_Completed;
            end if;
         end if;
      end if;
      if State.Outcome /= P.Done then State.Lease_Closed := True; end if;
      State.Working := False; Result := Original (State); Result.Kind := P.Recorded;
   end Read;
   procedure Query
     (State : Context; Scope : Scope_Tag; Caller : Principal; Lease : P.Lease_Ticket;
      Descriptor : P.Descriptor_ID; Sequence : P.Read_Sequence; Result : out P.Reply) is
      Kind : constant P.Reply_Kind := Selection (State, Scope, Caller, Lease, Descriptor);
   begin
      Result := Empty (Kind); if Kind /= P.Prepared then return; end if;
      if Sequence = P.No_Read then Result := Empty (P.Invalid_Request);
      elsif State.Saved and then Sequence = State.Last_Read then Result := Original (State);
      elsif Sequence <= State.Last_Read then Result := Empty (P.Stale);
      else Result := Empty (P.No_Result);
      end if;
   end Query;
   procedure Dispatch
     (State : in out Context; Lock : in out Requests.Context;
      Scope : Scope_Tag; Caller : Principal; Request : P.Packet;
      Response : out P.Packet; Handled : out Boolean) is
      Input : constant P.Request := P.Decode_Request (Request);
      Kind : P.Operation;
      Result : P.Reply;
      use type Interfaces.Unsigned_32;
   begin
      Response := (others => <>); Handled := P.Owned_Label (Request.Label); if not Handled then return; end if;
      Kind := P.Operation'Val (Request.Label - P.First_Operation);
      if not Input.Valid then Result := Empty (P.Malformed);
      else
         case Kind is
            when P.Select_Datum => Select_Datum (State, Lock, Scope, Caller, Input.Lease, Input.ID, Input.Datum, Result);
            when P.Read_Datum => Read (State, Lock, Scope, Caller, Input.Lease, Input.Descriptor, Input.Sequence, Result);
            when P.Query_Read => Query (State, Scope, Caller, Input.Lease, Input.Descriptor, Input.Sequence, Result);
         end case;
      end if;
      Response := P.Encode_Reply (Kind, Result);
   end Dispatch;
end ACPI_Locked_NVS;
