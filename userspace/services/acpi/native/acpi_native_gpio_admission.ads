pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.GPIO_Bootstrap_Exchange;
with GPIO_Bootstrap_Payload;
generic
   -- Trusted native startup owner: after provider prerequisites and before consumer _INI, startup I/O paused,
   -- physical Field/EC commands quiet, copied observation roots released.
   with function Startup_Quiet return Boolean;
package ACPI_Native_GPIO_Admission is
   type Outcome is (Not_Attempted, Boundary_Refused, Payload_Refused,
      Endpoint_Refused, Clock_Refused, Allocation_Refused, Grant_Refused,
      Not_Dispatched, Completion_Unknown, Invalid_Reply, Reply_Confirmed);
   type Report is record
      State : Outcome := Not_Attempted;
      Attempted, Working, Dispatched, Reply_Known : Boolean := False;
      Grant_Pending, Memory_Owned, Revoke_Accepted : Boolean := False;
      Confirmation_Attempted : Boolean := False;
      Calls : Natural range 0 .. 2 := 0;
      Request, Deadline_Milliseconds : Interfaces.Unsigned_64 := 0;
      Answer : CuBit.GPIO_Bootstrap_Exchange.Disposition := CuBit.GPIO_Bootstrap_Exchange.Malformed;
      Detail, Members : Interfaces.Unsigned_64 := 0;
   end record;
   function Observe return Report;
   -- One initial submission per process-lived instance; the bounded explicit
   -- confirmation below never replays AML. Cleanup grants no new deadline.
   -- The payload is copied to a dedicated zeroed aligned page,
   -- granted readonly to the authenticated firmware-data endpoint.
   procedure Submit (Target : CuBit.Handles.Endpoint;
      Data : GPIO_Bootstrap_Payload.Request; Request : Interfaces.Unsigned_64;
      Result : out Report);
   -- At most one explicit same-id confirmation after an unknown/invalid
   -- reply. Same endpoint, epoch, page and immutable request; no AML replay.
   -- Caller must still be paused and within the ORIGINAL startup deadline.
   -- The second bounded RPC deadline never renews that startup deadline.
   procedure Confirm (Result : out Report);
   -- One bounded cleanup observation. Revoke acceptance alone is not enough
   -- to free/reuse a page; only authoritative grant retirement permits that.
   -- An unknown first call keeps the page/grant live for Confirm. Call Confirm
   -- even if startup expired: it consumes confirmation without dispatch when
   -- Startup_Quiet refuses, then permits grant cleanup while retaining outcome.
   procedure Advance_Cleanup;
   -- No new request: abandon the unused confirmation and retire its grant.
   -- This cannot cancel a request already copied by devmgr; unknown remains.
   procedure Abandon_Confirmation;
   -- An unknown completion remains an obligation even after grant retirement:
   -- the peer may already have copied the request and still be deciding it.
   -- No resume/INI/exit permission until an attributable terminal outcome.
   function Quiet return Boolean;
end ACPI_Native_GPIO_Admission;
