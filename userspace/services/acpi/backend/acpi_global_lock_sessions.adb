pragma Ada_2022;
package body ACPI_Global_Lock_Sessions with SPARK_Mode is
   use type Owner.Outcome;
   function Observe (State : Context) return Owner.Statistics is (Owner.Observe (State.Engine));
   procedure Configure
     (State : in out Context; Authorized : Principal; Domain : Counter;
      Result : out Owner.Outcome) is
   begin
      Owner.Configure (State.Engine, Authorized, Domain, Result);
      if Result = Owner.Configured then
         State.Authorized := Authorized; State.Domain := Domain;
      end if;
   end Configure;
   procedure Acquire
     (State : in out Context; Caller : Principal; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Held : out Token; Result : out Owner.Outcome)
   is
      Lease : Owner.Lease;
   begin
      Held := No_Token;
      Owner.Acquire (State.Engine, Caller, Deadline, Budget, Lease, Result);
      if Result in Owner.Acquired | Owner.Acquired_After_Deadline | Owner.Acquired_Clock_Failed then
         State.Lease := Lease;
         State.Current := (State.Domain, Owner.Observe (State.Engine).Issued);
         Held := State.Current;
      end if;
   end Acquire;
   function Admits (State : Context; Caller : Principal; Held : Token) return Boolean is
     (Caller /= No_Principal and then Caller = State.Authorized and then
      Held /= No_Token and then Held = State.Current);
   procedure Check_Access
     (State : in out Context; Caller : Principal; Held : Token; Result : out Owner.Outcome) is
   begin
      if Caller = No_Principal or else Caller /= State.Authorized then
         Result := Owner.Unauthorized;
      elsif not Admits (State, Caller, Held) then Result := Owner.Stale_Lease;
      else Owner.Check_Access (State.Engine, Caller, State.Lease, Result);
      end if;
   end Check_Access;
   procedure Release
     (State : in out Context; Caller : Principal; Held : Token;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome) is
   begin
      if Caller = No_Principal or else Caller /= State.Authorized then
         Result := Owner.Unauthorized;
      elsif not Admits (State, Caller, Held) then Result := Owner.Stale_Lease;
      else
         Owner.Release (State.Engine, Caller, State.Lease, Budget, Result);
         if Result = Owner.Released then
            State.Current := No_Token; State.Lease := Owner.No_Lease;
         end if;
      end if;
   end Release;
end ACPI_Global_Lock_Sessions;
