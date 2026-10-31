pragma Ada_2022;
with ACPI_EC_Native_Queues;
generic
   -- Serialized bounded trusted hooks; exact authenticated receive metadata.
   -- Admit EC source/receipt/control authority independently. No blocking peer
   -- calls or reentry; receiving a message alone establishes no authority.
   -- A control hook returning Handled=False must preserve the current reply
   -- authority for Begin_Call; it must not receive or consume that authority.
   with procedure Handle_Event
     (Sender : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
      Stop, Handled : out Boolean) is ACPI_EC_Native_Queues.Ignore_Message;
   with procedure Handle_Control
     (Sender : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
      Stop, Handled : out Boolean) is ACPI_EC_Native_Queues.Ignore_Message;
   with function Unexpected_Completion return Boolean
     is ACPI_EC_Native_Queues.Unexpected_Completion;
   with function Poll_Event
     (Item : out CuBit.Messages.Message; Publisher : out CuBit.Messages.Process_ID)
      return Boolean is ACPI_EC_Native_Queues.Poll_Event;
   with procedure Poll_Request
     (Sender : out CuBit.Messages.Process_ID;
      Item : out CuBit.Messages.Message; Found : out Boolean)
     is ACPI_EC_Native_Queues.Poll_Request;
   with function Wait_Idle return Boolean is ACPI_EC_Native_Queues.Wait_Idle;
   -- One bounded serialized local scheduling opportunity after progress and
   -- before service receive. Runnable suppresses idle wait; Stop requests a
   -- retained serving stop. A false Receive_Request leaves that kernel queue
   -- untouched. No queue receives, peer calls or reentry in this hook.
   with procedure Check_Local_Work (Runnable, Stop, Receive_Request : out Boolean)
      is ACPI_EC_Native_Queues.No_Local_Work;
package ACPI_EC_Backend_Controller.With_Receive is
   type Stop_Reason is (Serving, Event_Stop, Control_Stop, Local_Work_Stop, Unexpected_Reply,
      Owner_Fault, Inconsistent_Owners, Wait_Unavailable, Adapter_Fault, Reentered);
   type Report is record
      Reason : Stop_Reason := Serving;
      Event_Seen, Event_Handled, Request_Seen, Control_Handled : Boolean := False;
      Field_Attempted, Waited, Clock_Available : Boolean := False;
      Field_Reply_Pending, Notice_Retained, Hardware_Recovery : Boolean := False;
      Work : Waits.Report;
      Field : Call_Report;
   end record;
   -- One completion check, at most one event, one owning controller advance,
   -- one typed service request and one idle wait. Active hardware waits use
   -- the controller's bounded pause, not a millisecond clock conversion.
   procedure Turn (Result : out Report);
   procedure Serve (Result : out Report);
   type Drain_Status is (Still_Serving, Advanced, Quarantined);
   type Drain_Report is record
      Status : Drain_Status := Still_Serving;
      State : Report;
   end record;
   -- After serving stops, one bounded owning advance of accepted work only.
   -- No queue receives, hooks, new submissions or idle wait. Requests stop
   -- before advancing; postwrite protocol may drain within its original
   -- deadline/budgets. Fault, owner conflict or reentry quarantines this route.
   -- No pending reply is not proof of quiescence: preserve all backing,
   -- ownership and recovery until a separate trusted lifecycle decision.
   -- Calling Drain while serving performs observation only.
   procedure Drain (Result : out Drain_Report);
   -- A serving stop is latched before another turn can consume anything.
   -- It never resets transactions, releases backing/leases or claims that
   -- pending/recovery/unknown publication obligations have completed. The
   -- caller must retain the controller and arrange trusted drain/recovery;
   -- returning from Serve is not permission to exit/release hardware.
end ACPI_EC_Backend_Controller.With_Receive;
