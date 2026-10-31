pragma Ada_2022;
with ACPI_EC_Backend_Controller.With_Receive;
with ACPI_EC_Native_Queues;
-- Instantiate once per parent owner. Use this as its sole receive/progress
-- route; the first ready receive/drain use claims it permanently. A second
-- child is refused before queues/callbacks. Trusted event hooks may call the parent's Start_Query after source
-- admission, as with Controller.With_Receive; no other owner reentry is allowed.
-- Authenticated system Stop is handled before this hook and cannot be vetoed.
-- Malformed/foreign reserved system messages are consumed without a callback.
-- Other valid system hints reach the hook outside the query admission window;
-- it may note wakes/log hints, but must not infer backing retirement from them.
-- Scans share this FIFO/progress owner and yield a Field opportunity between
-- iterations. Exhausted/blocked scans may idle while control remains reachable;
-- their independently admitted source must remain owned/masked as appropriate.
generic
   with procedure Handle_Event (Sender : CuBit.Messages.Process_ID;
      Item : CuBit.Messages.Message; Stop, Handled : out Boolean)
      is ACPI_EC_Native_Queues.Ignore_Message;
   with procedure Handle_Control (Sender : CuBit.Messages.Process_ID;
      Item : CuBit.Messages.Message; Stop, Handled : out Boolean)
      is ACPI_EC_Native_Queues.Ignore_Message;
   with function Unexpected_Completion return Boolean
      is ACPI_EC_Native_Queues.Unexpected_Completion;
   with function Poll_Event (Item : out CuBit.Messages.Message;
      Publisher : out CuBit.Messages.Process_ID) return Boolean
      is ACPI_EC_Native_Queues.Poll_Event;
   with procedure Poll_Request (Sender : out CuBit.Messages.Process_ID;
      Item : out CuBit.Messages.Message; Found : out Boolean)
      is ACPI_EC_Native_Queues.Poll_Request;
   with function Wait_Idle return Boolean is ACPI_EC_Native_Queues.Wait_Idle;
package ACPI_EC_Backend_Owner.With_Receive is
   type Stop_Reason is (Not_Started, Receive_Already_Owned, Serving, Event_Stop, Control_Stop, Local_Work_Stop,
      Unexpected_Reply, Owner_Fault, Inconsistent_Owners, Wait_Unavailable,
      Adapter_Fault, Reentered);
   type Report is record
      Reason : Stop_Reason := Not_Started;
      Event_Seen, Event_Handled, Request_Seen, Control_Handled : Boolean := False;
      Field_Attempted, Waited, Clock_Available : Boolean := False;
      State : Observation;
   end record;
   procedure Turn (Result : out Report);
   procedure Serve (Result : out Report);
   type Drain_Status is (Drain_Not_Ready, Still_Serving, Advanced, Quarantined);
   type Drain_Report is record
      Status : Drain_Status := Drain_Not_Ready;
      State : Report;
   end record;
   procedure Drain (Result : out Drain_Report);
   -- Before successful Configure all routes are observations only: no queue,
   -- reply, clock, pause or hardware callback is invoked. Stopped/draining
   -- state and all obligations belong to the same hidden engine/catalog pair.
   -- Default hooks admit no source/control authority. Native launch must bind
   -- authenticated system-stop handling and admitted EC source/receipt policy.
   -- No return value authorizes process exit or backing/ownership release.
private
   Claimed : Boolean := False;
   procedure Route_Event (Sender : CuBit.Messages.Process_ID;
      Item : CuBit.Messages.Message; Stop, Handled : out Boolean);
   procedure Schedule_Queries (Runnable, Stop, Receive_Request : out Boolean);
   package Receiver is new Controller.With_Receive
     (Route_Event, Handle_Control, Unexpected_Completion,
      Poll_Event, Poll_Request, Wait_Idle, Schedule_Queries);
end ACPI_EC_Backend_Owner.With_Receive;
