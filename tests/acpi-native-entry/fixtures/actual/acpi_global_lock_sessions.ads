pragma Ada_2022;
with Interfaces;
with ACPI_Global_Lock_Owner;
-- Backend-local request owner; a token is correlation data, not authority.
-- Bind the full authenticated caller and unique controller/generation domain
-- during trusted startup. Neither identity may come from request payloads.
-- One session owns the one software lock context for this physical domain.
-- Serialize calls and retain hardware/backing through all uncertain cleanup.
generic
   type Principal is private;
   No_Principal : Principal;
   with package Owner is new ACPI_Global_Lock_Owner
     (Principal => Principal, No_Principal => No_Principal, others => <>);
package ACPI_Global_Lock_Sessions with SPARK_Mode is
   subtype Counter is Interfaces.Unsigned_64;
   type Token is record
      Domain, Sequence : Counter := 0;
   end record;
   No_Token : constant Token := (0, 0);
   type Context is limited private;
   function Observe (State : Context) return Owner.Statistics;
   procedure Configure
     (State : in out Context; Authorized : Principal; Domain : Counter;
      Result : out Owner.Outcome);
   -- Successful late acquisitions return a token for mandatory cleanup;
   -- Check_Access still refuses effects after their acquisition deadline.
   procedure Acquire
     (State : in out Context; Caller : Principal; Deadline : Counter;
      Budget : Owner.Attempt_Budget; Held : out Token; Result : out Owner.Outcome);
   procedure Check_Access
     (State : in out Context; Caller : Principal; Held : Token; Result : out Owner.Outcome);
   -- Release consumes the token only after positive cleanup completion.
   -- Deferred or uncertain outcomes retain the private lease; no reset/rearm.
   procedure Release
     (State : in out Context; Caller : Principal; Held : Token;
      Budget : Owner.Attempt_Budget; Result : out Owner.Outcome);
private
   type Context is limited record
      Engine : Owner.Context;
      Authorized : Principal := No_Principal;
      Domain : Counter := 0;
      Current : Token := No_Token;
      Lease : Owner.Lease := Owner.No_Lease;
   end record;
end ACPI_Global_Lock_Sessions;
