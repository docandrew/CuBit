with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with Intel_Render_Admission;

-- Startup-owned adapter. Single dispatcher owner; no waits or private polling
-- loop. Every request to the driver is an asynchronous submission through
-- Driver (devmgr's intel-gpu endpoint); completions are matched on the
-- driver's kernel-stamped identity Driver_PID. No capability is delegated:
-- the reservation's session tag is the application's authority (CAP-002).
package Intel_Render_Admission_Native is
   type Broker_Request is limited private;
   function State (Item : Broker_Request) return Intel_Render_Admission.Phase;
   function Session (Item : Broker_Request) return Unsigned_64;
   -- Identity is the application's process identity (nonzero).
   procedure Start
     (Item : in out Broker_Request; Identity : Unsigned_64;
      Driver : CuBit.Handles.Endpoint; Driver_PID : Unsigned_64);
   -- At most one mutation: submit one control message.
   -- Caller supplies a globally unique token for an asynchronous submission.
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
      Driver : CuBit.Handles.Endpoint;
      Driver_PID : Unsigned_64 := 0;
   end record;
end Intel_Render_Admission_Native;
