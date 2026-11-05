pragma Ada_2022;
package body ACPI_Field_Lease_Scope with SPARK_Mode is
   use type Lease.Outcome, Lease.Phase, NVS.Outcome, W.Time_Instant;
   package D renames ACPI_Field_Diagnostics;
   function Lease_Reason (Result : Lease.Outcome) return D.Cause is
     (case Result is
       when Lease.Expired => D.Lease_Deadline,
       when Lease.Clock_Unavailable => D.Lease_Clock_Unavailable,
       when Lease.Clock_Reversed => D.Lease_Clock_Reversed,
       when Lease.Not_Sent => D.Transport_Not_Sent,
       when Lease.Completion_Unknown | Lease.Waiting | Lease.No_Evidence => D.Transport_Unknown,
       when Lease.Bad_Reply => D.Malformed_Reply,
       when Lease.Backend_Refused => D.Backend_Refused,
       when Lease.Invalid_Deadline => D.Invalid_Deadline,
       when Lease.Exhausted => D.Capacity_Exhausted,
       when Lease.Reentered => D.Reentered,
       when others => D.Lease_Unavailable);
   function Read_Diagnostic (State : Context) return D.Report is
      F : constant NVS.Diagnostic := NVS.First_Failure (State.Data);
      R : D.Report;
   begin
      if not F.Present then return R; end if;
      R := (From_Read => True, Read_Pending => F.Pending_Read, Remote => F.Remote,
        Delivery => F.Delivery, Record_Number => D.Word (F.ID), Byte_Offset => F.Datum.Offset,
        IPC_Until => D.Word (F.IPC_Deadline), Last_Known_IPC_Time => D.Word (F.Last_Known_IPC_Time),
        Seen_IPC_Clock => F.Seen_IPC_Clock, others => <>);
      R.At_Point := (case F.At_Point is
        when NVS.Validate_Request => D.Validate_Request,
        when NVS.Before_Select_Lease => D.Before_Select_Lease,
        when NVS.Before_Select_Clock => D.Before_Select_Clock,
        when NVS.Select_Exchange => D.Select_Exchange, when NVS.Select_Reply => D.Select_Reply,
        when NVS.Before_Read_Lease => D.Before_Read_Lease,
        when NVS.Before_Read_Clock => D.Before_Read_Clock,
        when NVS.Read_Exchange => D.Read_Exchange, when NVS.Read_Reply => D.Read_Reply,
        when NVS.After_Read => D.After_Read);
      R.Reason := (case F.Result is
        when NVS.Expired => D.IPC_Deadline,
        when NVS.Clock_Unavailable => D.IPC_Clock_Unavailable,
        when NVS.Clock_Reversed => D.IPC_Clock_Reversed,
        when NVS.Not_Ready => Lease_Reason (F.Lease_Result),
        when NVS.Late_Completed =>
          (if F.Lease_Result /= Lease.Acquired then Lease_Reason (F.Lease_Result)
           elsif F.Seen_IPC_Clock and then F.Last_Known_IPC_Time >= F.IPC_Deadline
           then D.IPC_Deadline else D.IPC_Clock_Unavailable),
        when NVS.Not_Sent => D.Transport_Not_Sent,
        when NVS.Completion_Unknown => D.Transport_Unknown,
        when NVS.Bad_Reply => D.Malformed_Reply,
        when NVS.Backend_Refused =>
          (case F.Remote is when P.Late_Completed => D.Backend_Late,
           when P.Read_Failed => D.Backend_Read_Failed, when others => D.Backend_Refused),
        when NVS.Invalid_Request => D.Invalid_Request, when NVS.Invalid_Deadline => D.Invalid_Deadline,
        when NVS.Peer_Changed => D.Peer_Changed, when NVS.Exhausted => D.Capacity_Exhausted,
        when NVS.Reentered => D.Reentered, when others => D.Scope_Unavailable);
      return R;
   end Read_Diagnostic;
   function First_Failure (State : Context) return D.Report is
     (if D.Present (State.First) then State.First else Read_Diagnostic (State));
   function Observe (State : Context) return Statistics is
     ((State.Depth, State.Initialized, State.Working, State.Faulted,
       Lease.Observe (State.Held).Cleanup_Required, Lease.Observe (State.Held).Usable));
   function Pending (State : Context) return Boolean is
     (State.Faulted or else (State.Depth = 0 and then Lease.Observe (State.Held).Cleanup_Required));
   function Current_Ticket (State : Context) return Lease.P.Ticket is
     (if State.Initialized and then not State.Working and then not State.Faulted
       and then State.Depth /= 0 and then Lease.Observe (State.Held).Usable
      then Lease.Ticket (State.Held) else Lease.P.No_Ticket);
   procedure Fail (State : in out Context; At_Point : D.Point := D.Scope_State;
      Reason : D.Cause := D.Scope_Unavailable) is
      Saved : constant D.Report := Read_Diagnostic (State);
   begin
      if D.Present (Saved) then D.Remember (State.First, Saved);
      else D.Remember (State.First, (Reason => Reason, At_Point => At_Point, others => <>)); end if;
      State.Faulted := True; Lease.Stop_Using (State.Held);
   end Fail;
   procedure Initialize (State : in out Context; Destination : Peer; Accepted : out Boolean) is
      Result : Lease.Outcome;
   begin
      Accepted := False;
      if State.Working or else State.Initialized then return; end if;
      Lease.Initialize (State.Held, Destination, Result);
      State.Initialized := Result = Lease.Initialized;
      Accepted := State.Initialized;
   end Initialize;
   procedure Begin_Field
     (State : in out Context; Channel : in out Transport;
      Requires_Lock, For_Write : Boolean;
      Lease_Deadline : Interfaces.Unsigned_64; IPC_Deadline : W.Time_Instant;
      Budget : Lease.P.Attempt_Budget; Accepted : out Boolean) is
      pragma Unreferenced (For_Write);
      Result : Lease.Outcome;
      Owns : Boolean := False;
   begin
      Accepted := False;
      if State.Working or else not State.Initialized or else State.Faulted then return; end if;
      if State.Depth = Depth_Count'Last then Fail (State); return; end if;
      State.Working := True;
      if Lease.Observe (State.Held).Cleanup_Required then
         Lease.Check_Usable (State.Held, Result);
         if Result /= Lease.Acquired then Fail (State, D.Acquire_Lease, Lease_Reason (Result)); State.Working := False; return; end if;
      elsif Requires_Lock then
         Lease.Acquire (State.Held, Channel, Lease_Deadline, IPC_Deadline, Budget, Result);
         if Result /= Lease.Acquired then Fail (State, D.Acquire_Lease, Lease_Reason (Result)); State.Working := False; return; end if;
         Owns := True;
      end if;
      State.Depth := State.Depth + 1; State.Owners (State.Depth) := Owns;
      State.Working := False; Accepted := True;
   end Begin_Field;
   procedure Release_Or_Query
     (State : in out Context; Channel : in out Transport;
      Deadline : W.Time_Instant; Result : out Lease.Outcome) is
   begin
      Lease.Release (State.Held, Channel, Deadline, Result);
      if Result = Lease.Waiting then
         -- The client never resends a possibly delivered release. Query only.
         Lease.Recover (State.Held, Channel, Deadline, Result);
      end if;
   end Release_Or_Query;
   procedure End_Field
     (State : in out Context; Channel : in out Transport;
      Cleanup_Deadline : W.Time_Instant; Accepted : out Boolean) is
      Owned : Boolean;
      Result : Lease.Outcome;
   begin
      Accepted := False;
      if State.Working or else not State.Initialized then return; end if;
      if State.Depth = 0 then Fail (State); return; end if;
      State.Working := True;
      Owned := State.Owners (State.Depth); State.Owners (State.Depth) := False;
      State.Depth := State.Depth - 1;
      if Owned or else (State.Depth = 0 and then Lease.Observe (State.Held).Cleanup_Required) then
         if Owned and then not State.Faulted then
            -- Completion must still fit the original data lease. Cleanup is
            -- mandatory even when this check fails or its clock is lost.
            Lease.Check_Usable (State.Held, Result);
            if Result /= Lease.Acquired then Fail (State, D.End_Lease, Lease_Reason (Result)); end if;
         end if;
         Release_Or_Query (State, Channel, Cleanup_Deadline, Result);
         if Result /= Lease.Released then Fail (State, D.Release_Lease, Lease_Reason (Result)); end if;
      elsif not State.Faulted and then Lease.Observe (State.Held).Cleanup_Required then
         Lease.Check_Usable (State.Held, Result);
         if Result /= Lease.Acquired then Fail (State, D.End_Lease, Lease_Reason (Result)); end if;
      end if;
      State.Working := False; Accepted := not State.Faulted;
   end End_Field;
   procedure Read
     (State : in out Context; Channel : in out Transport;
      ID : P.Record_ID; Datum : P.Geometry; IPC_Deadline : W.Time_Instant;
      Value : out Interfaces.Unsigned_64; Accepted : out Boolean) is
      Result : NVS.Outcome;
   begin
      Value := 0; Accepted := False;
      if State.Working or else not State.Initialized or else State.Faulted then return; end if;
      if State.Depth = 0 or else not Lease.Observe (State.Held).Usable then Fail (State); return; end if;
      State.Working := True;
      NVS.Read (State.Data, State.Held, Channel, ID, Datum, IPC_Deadline, Value, Result);
      Accepted := Result = NVS.Read_Complete;
      if not Accepted then Fail (State); end if;
      State.Working := False;
   end Read;
   procedure Cleanup
     (State : in out Context; Channel : in out Transport;
      Deadline : W.Time_Instant; Result : out Lease.Outcome) is
   begin
      Result := Lease.Not_Ready;
      if State.Working or else not State.Initialized or else State.Depth /= 0
        or else not Lease.Observe (State.Held).Cleanup_Required then return; end if;
      State.Working := True;
      Release_Or_Query (State, Channel, Deadline, Result);
      State.Working := False;
   end Cleanup;
   procedure Retire (State : in out Context; Accepted : out Boolean) is
      Result : NVS.Outcome;
   begin
      Accepted := False;
      if State.Working or else not State.Initialized or else State.Depth /= 0
        or else Lease.Observe (State.Held).Stage /= Lease.Ready
        or else Lease.Observe (State.Held).Cleanup_Required then return; end if;
      State.Working := True;
      NVS.Retire (State.Data, State.Held, Result);
      if Result = NVS.Retired then State.Faulted := False; Accepted := True; end if;
      State.Working := False;
   end Retire;
end ACPI_Field_Lease_Scope;
