pragma Ada_2022;
package body ACPI_Global_Lock_Scoped_Sessions with SPARK_Mode is
   function Observe (State : Context) return Statistics is
     ((State.Configured, State.Working, State.Holder, State.Uncertain_Caller,
       State.Current, State.Deadline, Owner.Observe (State.Engine)));
   procedure Configure
     (State : in out Context; Scope : Scope_Tag; Domain : Counter; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif State.Configured then Result := Owner.Already_Configured;
      elsif Scope = No_Scope then Result := Owner.Invalid_Configuration;
      else
         State.Working := True; Owner.Configure (State.Engine, Scope, Domain, Result); State.Working := False;
         if Result = Owner.Configured then State.Configured := True; State.Scope := Scope; State.Domain := Domain; end if;
      end if;
   end Configure;
   procedure Admit
     (State : Context; Scope : Scope_Tag; Caller : Principal; Result : out Owner.Outcome)
     with Post => Result = (if State.Working then Owner.Reentered
       elsif not State.Configured then Owner.Not_Ready
       elsif Scope = No_Scope or else Scope /= State.Scope or else Caller = No_Principal then Owner.Unauthorized
       else Owner.Configured)
   is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif not State.Configured then Result := Owner.Not_Ready;
      elsif Scope = No_Scope or else Scope /= State.Scope or else Caller = No_Principal then Result := Owner.Unauthorized;
      else Result := Owner.Configured;
      end if;
   end Admit;
   procedure Acquire
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Held : out Token; Result : out Owner.Outcome) is
      Lease : Owner.Lease;
   begin
      Held := No_Token; Admit (State, Scope, Caller, Result); if Result /= Owner.Configured then return; end if;
      if State.Current /= No_Token then Result := Owner.Already_Held; return; end if;
      State.Working := True;
      Owner.Acquire (State.Engine, State.Scope, Deadline, Budget, Lease, Result);
      if Result in Owner.Acquired | Owner.Acquired_After_Deadline | Owner.Acquired_Clock_Failed then
         State.Holder := Caller; State.Lease := Lease; State.Deadline := Deadline;
         State.Current := (State.Domain, Owner.Observe (State.Engine).Issued); Held := State.Current;
      elsif Result = Owner.Atomic_Indeterminate then State.Uncertain_Caller := Caller;
      end if;
      State.Working := False;
   end Acquire;
   procedure Check_Token
     (State : Context; Scope : Scope_Tag; Caller : Principal; Held : Token; Result : out Owner.Outcome)
     with Post => (Result in Owner.Configured | Owner.Reentered | Owner.Not_Ready | Owner.Unauthorized | Owner.Stale_Lease)
       and then (if Result = Owner.Configured then
       not State.Working and then State.Configured and then Scope /= No_Scope and then Scope = State.Scope
       and then Caller /= No_Principal and then Caller = State.Holder
       and then Held /= No_Token and then Held = State.Current)
   is
   begin
      Admit (State, Scope, Caller, Result); if Result /= Owner.Configured then return; end if;
      if Caller /= State.Holder then Result := Owner.Unauthorized;
      elsif Held = No_Token or else Held /= State.Current then Result := Owner.Stale_Lease;
      end if;
   end Check_Token;
   procedure Check_Access
     (State : in out Context; Scope : Scope_Tag; Caller : Principal;
      Held : Token; Result : out Owner.Outcome) is
   begin
      Check_Token (State, Scope, Caller, Held, Result); if Result /= Owner.Configured then return; end if;
      State.Working := True;
      Owner.Check_Access (State.Engine, State.Scope, State.Lease, Result);
      State.Working := False;
   end Check_Access;
   procedure Finish (State : in out Context; Budget : Owner.Attempt_Budget; Result : out Owner.Outcome)
     with Pre => not State.Working,
       Post => not State.Working and then (if Result = Owner.Released then
         State.Current = No_Token and then State.Holder = No_Principal)
   is
   begin
      State.Working := True;
      Owner.Release (State.Engine, State.Scope, State.Lease, Budget, Result);
      if Result = Owner.Released then
         State.Holder := No_Principal; State.Current := No_Token; State.Lease := Owner.No_Lease; State.Deadline := 0;
      end if;
      State.Working := False;
   end Finish;
   procedure Release
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Held : Token;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome) is
   begin
      Check_Token (State, Scope, Caller, Held, Result); if Result /= Owner.Configured then return; end if;
      Finish (State, Budget, Result);
   end Release;
   procedure Expire
     (State : in out Context; Now : Counter; Clock_Available : Boolean;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif not State.Configured or else State.Current = No_Token then Result := Owner.Not_Ready;
      elsif Clock_Available and then Now < State.Deadline then Result := Owner.Configured;
      else Finish (State, Budget, Result);
      end if;
   end Expire;
   procedure Peer_Exited
     (State : in out Context; Peer : Principal;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome) is
   begin
      if State.Working then Result := Owner.Reentered;
      elsif not State.Configured or else State.Current = No_Token then Result := Owner.Not_Ready;
      elsif Peer = No_Principal or else Peer /= State.Holder then Result := Owner.Unauthorized;
      else Finish (State, Budget, Result);
      end if;
   end Peer_Exited;
end ACPI_Global_Lock_Scoped_Sessions;
