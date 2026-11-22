pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
-- One bounded result slot and nonreused server tickets for one physical lock.
-- Prepare has no hardware effect. An acquire/release frame names its ticket;
-- duplicate frames return saved outcomes, never repeat an owner operation.
-- Retiring the slot makes old queries Stale, not evidence of completion.
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   Maximum_Lease_Microseconds : Positive := 100_000;
   Maximum_Tickets : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package ACPI_Global_Lock_Requests with SPARK_Mode is
   use type Owner.Outcome;
   subtype Counter is Interfaces.Unsigned_64;
   type Ticket is record
      Domain, Sequence : Counter := 0;
   end record;
   No_Ticket : constant Ticket := (0, 0);
   type Reply_Kind is (Ready, Recorded, Cached, Checked, No_Result, Denied,
     Busy, Stale_Ticket, Conflict, Invalid_Request, Unavailable, Exhausted, Reentered);
   type Reply is record
      Kind : Reply_Kind := Denied;
      Outcome : Owner.Outcome := Owner.Not_Ready;
   end record;
   type Action is (Acquisition, Release_Request, Termination);
   type Context is limited private;
   -- Trusted loop scheduling/diagnostics only; this snapshot is not access
   -- permission. Every data effect must call Check_Access afresh.
   function Observe (State : Context) return Sessions.Statistics;
   function Closing (State : Context) return Boolean;
   function May_Exit (State : Context) return Boolean;
   -- Trusted process-stop policy, never an IPC-selected exit/force-release
   -- operation. Close admission first, then let serialized maintenance use
   -- the original private lease. An uncertain physical effect forbids exit.
   procedure Begin_Stop (State : in out Context; Result : out Owner.Outcome)
     with Post => (if Result = Owner.Configured then Closing (State));
   procedure Cleanup (State : in out Context; Result : out Owner.Outcome)
     with Post => (if Result = Owner.Released then May_Exit (State));
   procedure Configure
     (State : in out Context; Scope : Scope_Tag; Domain : Counter; Result : out Owner.Outcome);
   -- Scope/Caller are kernel receive metadata. Now/Clock_Available are a
   -- trusted backend microsecond clock sample, never request-selected time.
   -- The nonce recovers this slot's Prepare reply, not global history. Only
   -- the minted ticket identifies a physical request. Deadline/budget are
   -- immutable once prepared; changed duplicate parameters are refused.
   procedure Prepare
     (State : in out Context; Scope : Scope_Tag; Caller : Principal; Nonce : Counter;
      Now : Counter; Clock_Available : Boolean; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Token : out Ticket; Result : out Reply_Kind);
   procedure Acquire
     (State : in out Context; Scope : Scope_Tag; Caller : Principal;
      Token : Ticket; Result : out Reply);
   procedure Check_Access
     (State : in out Context; Scope : Scope_Tag; Caller : Principal;
      Token : Ticket; Result : out Reply)
     with Post => (if Closing (State) then Result.Outcome /= Owner.Configured);
   -- Once release or maintenance cleanup has produced a result, data access
   -- stays closed even if that cleanup had no effect and the clock recovers.
   -- The private held lease remains available solely for cleanup.
   procedure Release
     (State : in out Context; Scope : Scope_Tag; Caller : Principal;
      Token : Ticket; Result : out Reply);
   procedure Query
     (State : Context; Scope : Scope_Tag; Caller : Principal;
      Token : Ticket; Item : Action; Result : out Reply);
   -- Trusted serialized maintenance, between completed data operations.
   -- Terminal evidence is saved separately from the original RPC outcome.
   -- A saved Atomic_Indeterminate is knowledge of uncertainty, not proof
   -- that the hardware operation completed or that cleanup is safe.
   procedure Expire
     (State : in out Context; Now : Counter; Clock_Available : Boolean; Result : out Owner.Outcome);
private
   type Saved_Result is record
      Present : Boolean := False;
      Outcome : Owner.Outcome := Owner.Not_Ready;
   end record;
   type Reservation is record
      Present : Boolean := False;
      Caller : Principal := No_Principal;
      Nonce, Deadline : Counter := 0;
      Budget : Owner.Attempt_Budget := Owner.Attempt_Budget'First;
      Token : Ticket := No_Ticket;
      Held : Sessions.Token := Sessions.No_Token;
      Acquired, Released, Terminal : Saved_Result;
   end record;
   type Context is limited record
      Engine : Sessions.Context;
      Configured, Working, Stopping : Boolean := False;
      Scope : Scope_Tag := No_Scope;
      Domain, Issued : Counter := 0;
      Current : Reservation;
   end record;
end ACPI_Global_Lock_Requests;
