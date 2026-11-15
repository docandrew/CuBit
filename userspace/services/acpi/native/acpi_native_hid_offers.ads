pragma Ada_2022;
with Interfaces;
with System;
with CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Handles;
with CuBit.HID_Offer_Exchange;
with HID_Offer_Payload;
generic
   -- Trusted native startup owner: after ALL startup AML, no more owner AML until handoff settles,
   -- physical Field/EC commands quiet, copied observation roots released.
   with function Handoff_Quiet return Boolean;
package ACPI_Native_HID_Offers is
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
      Answer : CuBit.HID_Offer_Exchange.Disposition := CuBit.HID_Offer_Exchange.Malformed;
      Detail, Queue_Position : Interfaces.Unsigned_64 := 0;
   end record;
   type Session is limited private;
   -- Keep sessions process-lived until Quiet; do not discard an unknown peer
   -- outcome even after its page retires. Limitedness prevents copying owners.
   -- One serialized native owner drives all sessions; no callback reentry.
   function Observe (Run : Session) return Report;
   -- One initial submission per process-lived Session; the bounded explicit
   -- confirmation below never replays AML. Cleanup grants no new deadline.
   -- Deadline is a finite absolute GETTIME millisecond, never Wait_Forever.
   -- Each call is bounded by min(Deadline, now + Offer_Budget_Milliseconds).
   -- Queued is a receipt; it never waits for child launch or readiness.
   -- The payload is copied to a dedicated zeroed aligned page,
   -- granted readonly to the authenticated firmware-data endpoint.
   procedure Submit (Run : in out Session; Target : CuBit.Handles.Endpoint;
      Data : HID_Offer_Payload.Request; Request, Deadline : Interfaces.Unsigned_64;
      Result : out Report);
   -- At most one explicit same-id confirmation after an unknown/invalid
   -- reply. Same endpoint, epoch, page and immutable request; no AML replay.
   -- Caller must still be paused and within the ORIGINAL explicit Deadline.
   -- The second bounded RPC deadline never renews that startup deadline.
   procedure Confirm (Run : in out Session; Result : out Report);
   -- One bounded cleanup observation. Revoke acceptance alone is not enough
   -- to free/reuse a page; only authoritative grant retirement permits that.
   -- An unknown first call keeps the page/grant live for Confirm. Call Confirm
   -- even if startup expired: it consumes confirmation without dispatch when
   -- Handoff_Quiet refuses, then permits grant cleanup while retaining outcome.
   procedure Advance_Cleanup (Run : in out Session);
   -- No new request: abandon the unused confirmation and retire its grant.
   -- This cannot cancel a request already copied by devmgr; unknown remains.
   procedure Abandon_Confirmation (Run : in out Session);
   -- An unknown completion remains an obligation even after grant retirement:
   -- the peer may already have copied the request and still be deciding it.
   -- Do not report handoff settled until an attributable terminal outcome.
   function Quiet (Run : Session) return Boolean;
private
   type Session is limited record
      View : Report;
      Buffer_Address : System.Address := System.Null_Address;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Saved_Target : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
      Original_Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Original_Deadline : Interfaces.Unsigned_64 := 0;
   end record;
end ACPI_Native_HID_Offers;
