pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
-- One physical-domain owner, scope authorization and a full caller identity
-- per lease. Configure is trusted startup only. Scope and Caller on request
-- entry points come from authenticated receive metadata, never payloads.
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
package ACPI_Global_Lock_Scoped_Sessions with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Counter is Interfaces.Unsigned_64;
   use type Counter;
   use type Owner.Outcome;
   type Token is record
      Domain, Sequence : Counter := 0;
   end record;
   No_Token : constant Token := (0, 0);
   type Context is limited private;
   type Statistics is record
      Configured, Working : Boolean;
      Holder, Uncertain_Caller : Principal;
      Current : Token;
      Deadline : Counter;
      Engine : Owner.Statistics;
   end record;
   function Observe (State : Context) return Statistics;
   procedure Configure
     (State : in out Context; Scope : Scope_Tag; Domain : Counter; Result : out Owner.Outcome);
   procedure Acquire
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Held : out Token; Result : out Owner.Outcome)
     with Post => Observe (State).Working = Observe (State)'Old.Working
       and then (if Result in Owner.Acquired | Owner.Acquired_After_Deadline | Owner.Acquired_Clock_Failed
         then Observe (State).Holder = Caller and then Observe (State).Current = Held
           and then Observe (State).Deadline = Deadline
         else Held = No_Token);
   procedure Check_Access
     (State : in out Context; Scope : Scope_Tag; Caller : Principal;
      Held : Token; Result : out Owner.Outcome)
     with Post => Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Owner.Configured then
         Observe (State).Holder = Caller and then Observe (State).Current = Held);
   procedure Release
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Held : Token;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome)
     with Post => Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Owner.Released then
         Observe (State).Current = No_Token and then Observe (State).Holder = No_Principal);
   -- Trusted maintenance only, between completed backend requests. The
   -- sample is the backend's local monotonic MICROSECOND clock. Unavailable
   -- clocks stop access and request cleanup; expiry never authorizes data.
   -- This is not a request-selected time or a force-release primitive.
   procedure Expire
     (State : in out Context; Now : Counter; Clock_Available : Boolean;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome)
     with Post => Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Owner.Released then
         Observe (State).Current = No_Token and then Observe (State).Holder = No_Principal);
   -- Only a verified kernel channel-close/exit adapter may call this. It
   -- supplies the full departed PID; no user-message opcode maps here.
   procedure Peer_Exited
     (State : in out Context; Peer : Principal;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome)
     with Post => Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Owner.Released then
         Observe (State).Current = No_Token and then Observe (State).Holder = No_Principal);
   -- Both cleanup paths use the original private lease. Unknown atomic or
   -- signalling completion remains stopped in Owner; it is never replayed,
   -- treated as Released or handed to another principal.
private
   type Context is limited record
      Engine : Owner.Context;
      Configured, Working : Boolean := False;
      Scope : Scope_Tag := No_Scope;
      Domain, Deadline : Counter := 0;
      Holder, Uncertain_Caller : Principal := No_Principal;
      Current : Token := No_Token;
      Lease : Owner.Lease := Owner.No_Lease;
   end record;
end ACPI_Global_Lock_Scoped_Sessions;
