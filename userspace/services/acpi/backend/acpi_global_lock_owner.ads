pragma Ada_2022;
with Interfaces;
generic
   type Principal is private;
   No_Principal : Principal;
   -- Callbacks close over one independently admitted, retained platform lock.
   -- They never block, retarget, raise, or reenter the owner. Unknown effects
   -- must be reported as unknown. Clock units are local monotonic microseconds.
   with procedure Read_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure Read_Word (Word : out Interfaces.Unsigned_32; Available : out Boolean);
   type Atomic_Outcome is (<>);
   Exchanged, Not_Exchanged, Not_Attempted, Atomic_Unknown : Atomic_Outcome;
   with procedure Compare_Exchange
     (Expected, Replacement : Interfaces.Unsigned_32;
      Observed : out Interfaces.Unsigned_32; Result : out Atomic_Outcome);
   type Signal_Outcome is (<>);
   Signal_Complete, Signal_Not_Sent, Signal_Unknown : Signal_Outcome;
   -- Not_Sent means no signaling write occurred. Partial multi-bank writes
   -- and lost completion are unknown, never permission to replay the signal.
   with procedure Signal_Firmware (Result : out Signal_Outcome);
   Maximum_Attempts : Positive := 16;
   Maximum_Leases : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
package ACPI_Global_Lock_Owner with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   subtype Counter is Interfaces.Unsigned_64;
   use type Counter;
   No_Domain : constant Counter := 0;
   No_Finite_Deadline : constant Counter := Counter'Last;
   type Lease is private;
   No_Lease : constant Lease;
   type Phase is (Fresh, Ready, Held, Signal_Pending,
     Acquire_Uncertain, Release_Uncertain, Signal_Uncertain);
   type Outcome is (Configured, Invalid_Configuration, Already_Configured,
     Acquired, Acquired_After_Deadline, Acquired_Clock_Failed,
     Released, Waiting_For_Firmware, Contended, Expired, Clock_Failed,
     Clock_Reversed, Invalid_Deadline, Unauthorized, Not_Ready, Already_Held,
     Stale_Lease, Lease_Exhausted, Read_Unavailable, Atomic_Refused,
     Atomic_Indeterminate, Signal_Deferred, Signal_Indeterminate, Reentered);
   subtype Attempt_Count is Natural range 0 .. Maximum_Attempts;
   subtype Attempt_Budget is Attempt_Count range 1 .. Attempt_Count'Last;
   type Context is limited private;
   type Statistics is record
      Stage : Phase;
      Issued : Counter;
      Working : Boolean;
   end record;
   function Observe (State : Context) return Statistics;
   -- Trusted startup only. Principal is the full authenticated opaque sender
   -- identity; Domain is a unique nonzero controller/generation identity.
   -- Configuration never resets a context or accepts a wire-supplied address.
   procedure Configure
     (State : in out Context; Authorized : Principal; Domain : Counter; Result : out Outcome)
     with Post =>
       (if Result = Configured then Observe (State).Stage = Ready
        else Observe (State) = Observe (State)'Old);
   function Holds (State : Context; Caller : Principal; Token : Lease) return Boolean
     with Post => (if Holds'Result then Caller /= No_Principal and then
       Token /= No_Lease and then Observe (State).Stage = Held and then
       not Observe (State).Working);
   procedure Acquire
     (State : in out Context; Caller : Principal; Deadline : Counter;
      Budget : Attempt_Budget; Token : out Lease; Result : out Outcome)
     with Post =>
       (if Result in Acquired | Acquired_After_Deadline | Acquired_Clock_Failed
        then Holds (State, Caller, Token) else Token = No_Lease)
       and then Observe (State).Issued >= Observe (State)'Old.Issued
       and then Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Atomic_Indeterminate then
         Observe (State).Stage = Acquire_Uncertain)
       and then (if Observe (State)'Old.Stage in
         Acquire_Uncertain | Release_Uncertain | Signal_Uncertain then
         Observe (State).Stage = Observe (State)'Old.Stage);
   -- Ownership observation alone does not authorize a timed hardware effect.

   procedure Check_Access
     (State : in out Context; Caller : Principal; Token : Lease; Result : out Outcome)
     with Post =>
       (if Result = Configured then Holds (State, Caller, Token))
       and then Observe (State).Stage = Observe (State)'Old.Stage
       and then Observe (State).Issued = Observe (State)'Old.Issued
       and then Observe (State).Working = Observe (State)'Old.Working;
   -- Bounded mandatory cleanup: no expired acquisition deadline may suppress
   -- release/signaling. The caller's IPC still needs its explicit deadline.
   -- Signal_Not_Sent retries only the signal; unknown completion cannot retry.
   procedure Release
     (State : in out Context; Caller : Principal; Token : Lease;
      Budget : Attempt_Budget; Result : out Outcome)
     with Post =>
       Observe (State).Issued = Observe (State)'Old.Issued
       and then Observe (State).Working = Observe (State)'Old.Working
       and then (if Result = Released then Observe (State).Stage = Ready)
       and then (if Result = Signal_Deferred then Observe (State).Stage = Signal_Pending)
       and then (if Result = Signal_Indeterminate then Observe (State).Stage = Signal_Uncertain)
       and then (if Result = Atomic_Indeterminate then Observe (State).Stage = Release_Uncertain)
       and then (if Observe (State)'Old.Stage in
         Acquire_Uncertain | Release_Uncertain | Signal_Uncertain then
         Observe (State).Stage = Observe (State)'Old.Stage);
private
   type Lease is record
      Domain, Sequence : Counter := 0;
   end record;
   No_Lease : constant Lease := (0, 0);
   type Context is limited record
      Stage : Phase := Fresh;
      Authorized : Principal := No_Principal;
      Domain, Issued, Last_Clock, Deadline : Counter := 0;
      Current : Lease := No_Lease;
      Seen_Clock, Clock_Broken, Working, Access_Admitted : Boolean := False;
   end record;
end ACPI_Global_Lock_Owner;
