with ACPI_EC_Supervisor_Control;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Event_Types;
with ACPI_Requests.EC_Events;
with ACPI_Requests.Notification_Pump;
with ACPI_Service;
with AML_Fatal;
with ACPI_Native_Events;
generic
   with package Receiver is new ACPI_EC_Event_Receiver
     (Core => ACPI_Service, others => <>);
   -- All consumers are installed before serving. Bounded local, idempotent
   -- fanout/invalidation; no AML, IPC, reset, reentry or Server mutation.
   with procedure Route_Local (Server : ACPI_Requests.State; Routed : out Boolean);
   -- One already-attributable queued EC query result, in source order.
   -- Sender is authenticated receive metadata. Enrollment/provider readiness
   -- is independent. This callback performs no AML, blocking IPC, reentry or hardware
   -- acknowledgement. Healthy=False includes overflow or lost attribution.
   -- Empty means no runnable source work now; arrivals must wake the runtime
   -- activity wait. Never hide runnable backlog behind an empty observation.
   with procedure Take_Event
     (Sender : out CuBit.Messages.Process_ID;
      Item : out ACPI_EC_Event_Types.Notice; Found, Healthy : out Boolean);
   -- Bounded local/nonblocking handoff of the exact successful event only
   -- after its retained result is released and notifications have been routed
   -- and acknowledged. No AML, blocking IPC, reentry or hardware acknowledgement.
   -- Confirmed means the handoff is established; False includes uncertainty.
   -- The owner stops without replay on False/exception. A receiver must still
   -- independently attribute this to its admitted source/scan/session; this is
   -- neither a wire authority nor proof of hardware quiescence or safe rearm.
   with procedure Complete_Event
     (Item : ACPI_EC_Event_Types.Notice; Charged : Natural; Confirmed : out Boolean);
   Fuel_Per_Event : Positive := 10_000;
package ACPI_Native_Server.With_EC_Events is
   package Events is new ACPI_Requests.EC_Events (Receiver);
   package Pump is new ACPI_Requests.Notification_Pump (Route_Local);
   Claims_Per_Turn : constant Pump.Batch_Limit := 4;
   type Outcome is (Not_Started, Source_Empty, Event_Processed,
      Notifications_Pending, Notifications_Refused, Source_Unhealthy,
      Event_Refused, Completion_Unconfirmed, Recovery_Required, Stopped_By_Owner);
   type Report is record
      Status : Outcome := Not_Started;
      Notifications : Pump.Report;
      Source_Found : Boolean := False;
      Source_Item : ACPI_EC_Event_Types.Notice_Option;
      Event : ACPI_EC_Event_Types.Notice_Option;
      Completion : ACPI_EC_Event_Types.Notice_Option;
      Completion_Charge : Natural := 0;
      Completion_Attempted, Completion_Confirmed : Boolean := False;
      Fatal : AML_Fatal.Observation;
      Admission : Events.Controller.Admission := Events.Controller.Not_Ready;
      Delivery : Receiver.Report;
   end record;
   -- Pump at most four existing receipts, then report the prior successful
   -- event completion, then consume at most one source event only after an
   -- explicit empty notification observation. A consumed
   -- source opportunity returns Pending so newly generated notifications are
   -- routed before another event. This gives IPC a turn between bounded work.
   -- Pending is runnable local work, never an outstanding hardware wait.
   -- Any refusal/exception is terminal for this owner. It retains uncertain
   -- delivery storage and never automatically retries AML, cleanup or sources.
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome);
   function Observation return Report;
   function Observation_Complete return Boolean;
   function Is_Stopped return Boolean;
   type Stop_Result is (Stopped, Already_Stopped, Operation_Incomplete);
   -- Trusted serialized controller, between turns. Closes new event/AML,
   -- notification and completion handoff work without IO or cancellation.
   -- Reentry/incomplete callback observations refuse this operation without
   -- claiming a permanent stop. Existing terminal stops are preserved exactly.
   -- Retained completion/delivery storage stays owned. No result establishes
   -- quiescence or authorizes process exit,hardware rearm or backing release.
   procedure Request_Stop (Result : out Stop_Result);
   -- Current retained completion, independent of Last.Completion (a copied
   -- observation of this turn's prior event). Claim receipts are not completion.
   function Pending_Completion return ACPI_EC_Event_Types.Notice_Option;
   -- Explicit retained-pin cleanup after stop; never clears the stop latch or
   -- acknowledges hardware. Caller must also stop other AML in this Core.
   procedure Cleanup
     (Server : in out ACPI_Requests.State; Result : out Receiver.Close_Result);
   generic
      with procedure Dispatch_Message
        (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
         Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message);
      -- Reserved messages are classified before this trusted bounded hook.
      -- A valid kernel Stop cannot be vetoed; rejected reserved messages never
      -- reach the hook. No AML,blocking IPC,reset or reentry in this hook.
      with procedure Handle_Event
        (Publisher : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
         Stop, Handled : out Boolean) is ACPI_Native_Events.Handle;
   package With_Dispatch is
      -- Bind the service loop to this owner and its stop admission. A stop
      -- closes the owner before returning Stop_Requested; incomplete stop
      -- admission returns Owner_Work_Refused. Unexpected completion or raised
      -- hook/dispatch also closes admission where possible,retaining storage.
      -- Wait_Unavailable leaves a healthy owner available for a later serve.
      -- All hardware,lower-owner and backing-lifetime obligations remain.
      procedure Serve (Server : in out ACPI_Requests.State; Reason : out Stop_Reason);
   end With_Dispatch;
private
   type Delivery_Access is access Receiver.Delivery;
   Active : Delivery_Access;
   Last : Report;
   Lifecycle : ACPI_EC_Supervisor_Control.State;
end ACPI_Native_Server.With_EC_Events;
