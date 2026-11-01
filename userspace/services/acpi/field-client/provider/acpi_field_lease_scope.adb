pragma Ada_2022;
package body ACPI_Field_Lease_Scope with SPARK_Mode is
   use type Lease.Outcome, Lease.Phase, NVS.Outcome;
   function Observe (State : Context) return Statistics is
     ((State.Depth, State.Initialized, State.Working, State.Faulted,
       Lease.Observe (State.Held).Cleanup_Required, Lease.Observe (State.Held).Usable));
   function Pending (State : Context) return Boolean is
     (State.Faulted or else (State.Depth = 0 and then Lease.Observe (State.Held).Cleanup_Required));
   procedure Fail (State : in out Context) is
   begin State.Faulted := True; Lease.Stop_Using (State.Held); end Fail;
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
         if Result /= Lease.Acquired then Fail (State); State.Working := False; return; end if;
      elsif Requires_Lock then
         Lease.Acquire (State.Held, Channel, Lease_Deadline, IPC_Deadline, Budget, Result);
         if Result /= Lease.Acquired then Fail (State); State.Working := False; return; end if;
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
            if Result /= Lease.Acquired then Fail (State); end if;
         end if;
         Release_Or_Query (State, Channel, Cleanup_Deadline, Result);
         if Result /= Lease.Released then Fail (State); end if;
      elsif not State.Faulted and then Lease.Observe (State.Held).Cleanup_Required then
         Lease.Check_Usable (State.Held, Result);
         if Result /= Lease.Acquired then Fail (State); end if;
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
