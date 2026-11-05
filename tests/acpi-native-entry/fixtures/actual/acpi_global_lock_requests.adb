pragma Ada_2022;
package body ACPI_Global_Lock_Requests with SPARK_Mode is
   use type Counter, Owner.Phase, Sessions.Token;
   function Observe (State : Context) return Sessions.Statistics is
     (Sessions.Observe (State.Engine));
   function Closing (State : Context) return Boolean is (State.Stopping);
   function May_Exit (State : Context) return Boolean is
     (not State.Working and then not Observe (State).Working
      and then Observe (State).Current = Sessions.No_Token
      and then Observe (State).Engine.Stage in Owner.Fresh | Owner.Ready);
   procedure Begin_Stop (State : in out Context; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      else State.Stopping := True; Result := Owner.Configured; end if;
   end Begin_Stop;
   procedure Cleanup (State : in out Context; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif not State.Stopping or else not State.Configured then Result := Owner.Not_Ready;
      elsif State.Current.Terminal.Present and then State.Current.Terminal.Outcome = Owner.Released then
         Result := Owner.Released;
      elsif State.Current.Held = Sessions.No_Token then Result := Owner.Not_Ready;
      else
         State.Working := True;
         Sessions.Release (State.Engine, State.Scope, State.Current.Caller,
           State.Current.Held, State.Current.Budget, Result);
         if Result /= Owner.Not_Ready and then
           (not State.Current.Terminal.Present or else State.Current.Terminal.Outcome /= Owner.Released)
         then State.Current.Terminal := (True, Result); end if;
         State.Working := False;
      end if;
   end Cleanup;
   function Admission (State : Context; Scope : Scope_Tag; Caller : Principal) return Reply_Kind is
     (if State.Working then Reentered elsif not State.Configured then Unavailable
      elsif Scope = No_Scope or else Scope /= State.Scope or else Caller = No_Principal then Denied else Ready);
   function Selection
     (State : Context; Scope : Scope_Tag; Caller : Principal; Token : Ticket) return Reply_Kind is
     (if Admission (State, Scope, Caller) /= Ready then Admission (State, Scope, Caller)
      elsif not State.Current.Present or else Token = No_Ticket or else Token /= State.Current.Token then Stale_Ticket
      elsif Caller /= State.Current.Caller then Denied else Ready);
   function Owns_Lease (Item : Saved_Result) return Boolean is
     (Item.Present and then Item.Outcome in Owner.Acquired | Owner.Acquired_After_Deadline | Owner.Acquired_Clock_Failed);
   function Saved (Item : Saved_Result) return Reply is
     (if Item.Present then (Cached, Item.Outcome) else (No_Result, Owner.Not_Ready));
   procedure Configure
     (State : in out Context; Scope : Scope_Tag; Domain : Counter; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif State.Stopping then Result := Owner.Not_Ready;
      elsif State.Configured then Result := Owner.Already_Configured;
      else
         State.Working := True; Sessions.Configure (State.Engine, Scope, Domain, Result); State.Working := False;
         if Result = Owner.Configured then State.Configured := True; State.Scope := Scope; State.Domain := Domain; end if;
      end if;
   end Configure;
   procedure Prepare
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Nonce : Counter;
      Now : Counter; Clock_Available : Boolean; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Token : out Ticket; Result : out Reply_Kind) is
      Status : constant Sessions.Statistics := Sessions.Observe (State.Engine);
   begin
      Token := No_Ticket; Result := Admission (State, Scope, Caller); if Result /= Ready then return; end if;
      if State.Stopping then Result := Unavailable; return; end if;
      if Nonce = 0 then Result := Invalid_Request; return; end if;
      if State.Current.Present and then Caller = State.Current.Caller and then Nonce = State.Current.Nonce then
         if Deadline /= State.Current.Deadline or else Budget /= State.Current.Budget then Result := Conflict;
         else Token := State.Current.Token; end if;
         return;
      end if;
      if not Clock_Available then Result := Unavailable; return; end if;
      if Deadline = Owner.No_Finite_Deadline or else Deadline <= Now
        or else Deadline - Now > Counter (Maximum_Lease_Microseconds)
      then Result := Invalid_Request; return; end if;
      if Status.Current /= Sessions.No_Token or else Status.Engine.Stage /= Owner.Ready then Result := Busy; return; end if;
      if State.Current.Present and then not State.Current.Acquired.Present and then Now < State.Current.Deadline
      then Result := Busy; return; end if;
      if State.Issued >= Maximum_Tickets then Result := Exhausted; return; end if;
      State.Issued := State.Issued + 1;
      State.Current := (Present => True, Caller => Caller, Nonce => Nonce, Deadline => Deadline,
        Budget => Budget, Token => (State.Domain, State.Issued), others => <>);
      Token := State.Current.Token;
   end Prepare;
   procedure Acquire
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Token : Ticket; Result : out Reply) is
      Status : Reply_Kind;
      Outcome : Owner.Outcome;
      Held : Sessions.Token;
   begin
      Status := Selection (State, Scope, Caller, Token); Result := (Status, Owner.Not_Ready);
      if Status /= Ready then return; end if;
      if State.Stopping then Result.Kind := Unavailable; return; end if;
      if State.Current.Acquired.Present then Result := Saved (State.Current.Acquired); return; end if;
      State.Working := True;
      Sessions.Acquire (State.Engine, Scope, Caller, State.Current.Deadline, State.Current.Budget, Held, Outcome);
      State.Current.Held := Held; State.Current.Acquired := (True, Outcome);
      State.Working := False; Result := (Recorded, Outcome);
   end Acquire;
   procedure Check_Access
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Token : Ticket; Result : out Reply) is
      Status : Reply_Kind;
      Outcome : Owner.Outcome;
   begin
      Status := Selection (State, Scope, Caller, Token); Result := (Status, Owner.Not_Ready);
      if Status /= Ready then return; end if;
      if State.Stopping or else not Owns_Lease (State.Current.Acquired)
        or else State.Current.Released.Present or else State.Current.Terminal.Present
      then Result.Kind := No_Result; return; end if;
      State.Working := True;
      Sessions.Check_Access (State.Engine, Scope, Caller, State.Current.Held, Outcome);
      State.Working := False; Result := (Checked, Outcome);
   end Check_Access;
   procedure Release
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Token : Ticket; Result : out Reply) is
      Status : Reply_Kind;
      Outcome : Owner.Outcome;
   begin
      Status := Selection (State, Scope, Caller, Token); Result := (Status, Owner.Not_Ready);
      if Status /= Ready then return; end if;
      if State.Current.Released.Present then Result := Saved (State.Current.Released); return; end if;
      if State.Current.Terminal.Present and then State.Current.Terminal.Outcome = Owner.Released then
         Result := Saved (State.Current.Terminal); return;
      end if;
      if not Owns_Lease (State.Current.Acquired) then Result.Kind := No_Result; return; end if;
      State.Working := True;
      Sessions.Release (State.Engine, Scope, Caller, State.Current.Held, State.Current.Budget, Outcome);
      State.Current.Released := (True, Outcome);
      if Outcome = Owner.Released then State.Current.Terminal := (True, Outcome); end if;
      State.Working := False; Result := (Recorded, Outcome);
   end Release;
   procedure Query
     (State : Context; Scope : Scope_Tag; Caller : Principal; Token : Ticket; Item : Action; Result : out Reply) is
      Status : constant Reply_Kind := Selection (State, Scope, Caller, Token);
   begin
      Result := (Status, Owner.Not_Ready); if Status /= Ready then return; end if;
      case Item is
         when Acquisition => Result := Saved (State.Current.Acquired);
         when Release_Request => Result := Saved (State.Current.Released);
         when Termination => Result := Saved (State.Current.Terminal);
      end case;
   end Query;
   procedure Expire
     (State : in out Context; Now : Counter; Clock_Available : Boolean; Result : out Owner.Outcome) is
      Budget : constant Owner.Attempt_Budget :=
        (if State.Current.Present then State.Current.Budget else Owner.Attempt_Budget'First);
   begin
      if State.Working then Result := Owner.Reentered;
      elsif not State.Configured then Result := Owner.Not_Ready;
      else
         State.Working := True;
         Sessions.Expire (State.Engine, Now, Clock_Available, Budget, Result);
         if State.Current.Present and then Owns_Lease (State.Current.Acquired)
           and then Result not in Owner.Configured | Owner.Not_Ready
           and then (not State.Current.Terminal.Present or else State.Current.Terminal.Outcome /= Owner.Released)
         then State.Current.Terminal := (True, Result); end if;
         State.Working := False;
      end if;
   end Expire;
end ACPI_Global_Lock_Requests;
