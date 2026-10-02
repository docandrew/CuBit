with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Capability_Grants;
with Intel_Render_Admission;

-- Startup-owned adapter. Single dispatcher owner; no waits or private polling
-- loop. The dispatcher must keep both source slots unchanged and destination
-- slots reserved (including driver slots40..55)
-- until this object's pending receipt and remote cleanup have been resolved.
package Intel_Render_Admission_Native is
   type Broker_Request is limited private;
   function State (Item : Broker_Request) return Intel_Render_Admission.Phase;
   procedure Start
     (Item : in out Broker_Request; Target : CuBit.Capability_Grants.Recipient;
      Source, Application_Source, Destination : CuBit.Messages.CapabilitySlot);
   -- Application_Source is a grantable endpoint naming Target's captured
   -- incarnation, not a process cap or a raw PID. The broker also needs CSPACE
   -- authority over both Target and the driver captured from Source.
   -- At most one mutation: submit a control message or delegate an endpoint.
   -- The first delegation also inspects Application_Source read-only.
   -- Caller supplies a globally unique token for an asynchronous submission.
   -- No GRANT bit is installed in the client; only READ|WRITE (3).
   procedure Advance (Item : in out Broker_Request; Token : Unsigned_64);
   -- Receipt must come directly from the kernel completion queue. Never
   -- accept an ordinary incoming IPC message as a completion. Unrelated
   -- tokens are left unconsumed for the central dispatcher's other clients.
   procedure Complete (Item : in out Broker_Request;
     Receipt : CuBit.Messages.CompletionEntry; Consumed : out Boolean);
   procedure Cancel (Item : in out Broker_Request);
private
   type Broker_Request is limited record
      Transaction : Intel_Render_Admission.Transaction;
      Recipient : CuBit.Capability_Grants.Recipient;
      Driver : CuBit.Capability_Grants.Recipient;
      Source, Application_Source, Destination : CuBit.Messages.CapabilitySlot := 0;
      Recipient_Installed : Boolean := False;
      Driver_PID : Unsigned_64 := 0;
   end record;
end Intel_Render_Admission_Native;
