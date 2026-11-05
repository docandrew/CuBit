pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
with ACPI_Global_Lock_Scoped_Sessions;
with ACPI_Global_Lock_Requests;
generic
   type Principal is private;
   No_Principal : Principal;
   type Scope_Tag is private;
   No_Scope : Scope_Tag;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Scope_Tag, No_Principal => No_Scope, others => <>);
   with package Sessions is new ACPI_Global_Lock_Scoped_Sessions
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner);
   with package Requests is new ACPI_Global_Lock_Requests
     (Principal, No_Principal, Scope_Tag, No_Scope, Owner, Sessions, others => <>);
   with procedure Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean);
   Retry_Microseconds : Positive := 1_000;
package ACPI_Global_Lock_Maintenance with SPARK_Mode is
   type Outcome is (Idle, Scheduled, Released, Cleanup_Deferred, Uncertain, Clock_Stopped, Reentered);
   type Report is record
      Status : Outcome := Idle;
      -- Absolute MONOTONIC MILLISECONDS for the sole server loop; infinity
      -- is explicit. Access keeps checking the original microsecond lease.
      Deadline : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
      Cleanup : Owner.Outcome := Owner.Not_Ready;
   end record;
   type Context is limited private;
   -- Run only between completed backend data operations, before polling.
   -- Never invent an exit notification or retry an unknown physical effect.
   -- Deferred known-no-effect cleanup is paced; unrelated/spurious activity
   -- cannot reset the original lease deadline or cause a tight retry loop.
   procedure Run (State : in out Context; Lock : in out Requests.Context; Result : out Report);
private
   type Context is limited record
      Working, Seen_Clock, Clock_Broken, Failed_Clock_Cleanup, Scheduling_Exhausted : Boolean := False;
      Last_Clock, Next_Retry : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Global_Lock_Maintenance;
