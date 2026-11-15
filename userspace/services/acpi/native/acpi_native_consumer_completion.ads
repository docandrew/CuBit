pragma Ada_2022;
with Interfaces;
with CuBit.Handles;
with CuBit.Consumer_Initialization_Exchange;
generic
   -- One serialized native startup owner. ALL startup AML has ended; provider,
   -- retained-result and physical IO owners are quiet. No callback reentry.
   with function Completion_Quiet return Boolean;
package ACPI_Native_Consumer_Completion is
   package Protocol renames CuBit.Consumer_Initialization_Exchange;
   type Outcome is (Not_Attempted, Boundary_Refused, Request_Refused,
      Endpoint_Refused, Clock_Refused, Completion_Unknown, Invalid_Reply, Receipt_Confirmed);
   type Report is record
      State : Outcome := Not_Attempted;
      Attempted, Working, Dispatched, Receipt_Known, Confirmation_Attempted : Boolean := False;
      Calls : Natural range 0 .. 2 := 0;
      Request, Epoch, Deadline_Milliseconds : Interfaces.Unsigned_64 := 0;
      Declared : Protocol.Phase_Status := Protocol.Failed;
      Answer : Protocol.Receipt := Protocol.Malformed;
      -- Meaningful only with Receipt_Known; full-width immutable receipt data.
      Released : Interfaces.Unsigned_64 := 0;
   end record;
   function Observe return Report;
   function Quiet return Boolean;
   function Completed_Received return Boolean;
   -- Exactly one immutable declaration per process-lived generic instance.
   -- The native owner instantiates this once and supplies a unique request ID.
   -- Epoch is the full process-incarnation ID, never narrowed. Deadline is a
   -- finite absolute GETTIME millisecond; each RPC is capped by this original
   -- deadline and Completion_Budget_Milliseconds. No child-health wait.
   procedure Submit (Target : CuBit.Handles.Endpoint;
      Request, Epoch, Deadline : Interfaces.Unsigned_64;
      Status : Protocol.Phase_Status; Result : out Report);
   -- At most one explicit same-frame confirmation of an unknown/invalid reply.
   -- Never changes epoch/status/id/endpoint or renews the original deadline.
   -- A terminal refusal, including Capacity, is not automatically retried.
   procedure Confirm (Result : out Report);
   -- Unknown completion remains unknown even when confirmation is abandoned.
   -- Caller must retain that lifecycle obligation; this sends no cancellation.
   procedure Abandon_Confirmation;
end ACPI_Native_Consumer_Completion;
