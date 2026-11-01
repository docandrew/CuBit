pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Protocol;
with ACPI_Region_Protocol;
generic
   type Transport is limited private;
   type Peer is private;
   No_Peer : Peer;
   with procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   with procedure IPC_Clock (Now : out ACPI_Region_Protocol.Time_Instant; Available : out Boolean);
   with procedure Exchange
     (Channel : in out Transport; Destination : Peer;
      Deadline : ACPI_Region_Protocol.Time_Instant;
      Request : ACPI_Region_Protocol.Packet; Reply : out ACPI_Region_Protocol.Packet;
      Delivered : out ACPI_Region_Protocol.Delivery);
package ACPI_Global_Lock_Client with SPARK_Mode is
   package P renames ACPI_Global_Lock_Protocol;
   package W renames ACPI_Region_Protocol;
   use type P.Ticket;
   type Phase is (Fresh, Ready, Held, Uncertain);
   type Outcome is (Initialized, Acquired, Needs_Cleanup, Released, Backend_Refused,
     Not_Ready, Invalid_Peer, Invalid_Deadline, Already_Initialized,
     Expired, Clock_Unavailable, Clock_Reversed, Not_Sent,
     Completion_Unknown, Bad_Reply, Waiting, No_Evidence, Exhausted, Reentered);
   type Context is limited private;
   type Statistics is record
      Stage : Phase;
      Working, Usable, Cleanup_Required : Boolean;
   end record;
   function Observe (State : Context) return Statistics;
   -- One persistent context per peer in this frontend. Its typed endpoint is
   -- borrowed from retained startup authority, never looked up or replaced.
   procedure Initialize (State : in out Context; Destination : Peer; Result : out Outcome);
   procedure Acquire
     (State : in out Context; Channel : in out Transport;
      Lease_Deadline_Microseconds : Interfaces.Unsigned_64;
      IPC_Deadline_Milliseconds : W.Time_Instant; Budget : P.Attempt_Budget; Result : out Outcome)
     with Post => (if Result = Acquired then Observe (State).Stage = Held
       and then Observe (State).Usable and then Observe (State).Cleanup_Required);
   -- Metadata only, including for uncertain cleanup. Not access authority.
   function Ticket (State : Context) return P.Ticket
     with Post => (if Observe (State).Cleanup_Required then P.Valid (Ticket'Result));
   -- Borrowed peer metadata only; the caller must not close the retained
   -- endpoint. Stop_Using revokes local data admission without losing duty.
   function Bound_Peer (State : Context) return Peer;
   procedure Stop_Using (State : in out Context);
   procedure Check_Usable (State : in out Context; Result : out Outcome)
     with Post => Ticket (State) = Ticket (State)'Old
       and then (if Result = Acquired then Observe (State).Stage = Held
         and then Observe (State).Usable and then Observe (State).Cleanup_Required);
   -- Cleanup/recovery have their own explicit finite IPC deadline and never
   -- use the expired data deadline to suppress cleanup. No release is resent
   -- after possible delivery; Recover queries the saved original outcomes.
   procedure Release
     (State : in out Context; Channel : in out Transport;
      IPC_Deadline_Milliseconds : W.Time_Instant; Result : out Outcome)
     with Post => (if Result /= Reentered then not Observe (State).Usable)
       and then (if Result = Released then not Observe (State).Cleanup_Required);
   procedure Recover
     (State : in out Context; Channel : in out Transport;
      IPC_Deadline_Milliseconds : W.Time_Instant; Result : out Outcome)
     with Post => (if Result /= Reentered then not Observe (State).Usable)
       and then (if Result = Released then not Observe (State).Cleanup_Required);
private
   function Consistent (State : Context) return Boolean;
   type Context is limited record
      Stage : Phase := Fresh;
      Destination : Peer := No_Peer;
      Working, Usable, Seen_Clock, Seen_IPC_Clock, Clock_Broken, Release_Attempted : Boolean := False;
      Last_IPC_Clock : W.Time_Instant := 0;
      Last_Clock, Nonce, Data_Deadline : Interfaces.Unsigned_64 := 0;
      Held_Ticket : P.Ticket := P.No_Ticket;
   end record
     with Type_Invariant => Consistent (Context);
end ACPI_Global_Lock_Client;
