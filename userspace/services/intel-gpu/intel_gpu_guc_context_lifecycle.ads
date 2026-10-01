with Interfaces; use Interfaces;
package Intel_GPU_GuC_Context_Lifecycle with SPARK_Mode is
   type Phase is (Fresh, Ready, Register_Pending, Registration_Queued,
                  Policy_Pending, Policy_Queued, Enable_Pending, Enabled,
                  Disable_Pending, Disabled, Quarantined);
   type Operation is (Register_Context, Set_Policy, Enable, Disable);
   type Send_Result is (Backpressure, Queued, Uncertain);
   type Context is limited private;
   function State (Object : Context) return Phase;
   function Credits_Held (Object : Context) return Natural;
   function Last_Fence (Object : Context) return Unsigned_16;
   -- One driver-owned context, no ID/fence reuse. Re-enable is permitted only
   -- after acknowledged disable and consumes a fresh fence. Caller serializes
   -- send/dispatch and reserves four initial control fences
   -- globally plus four receive DWORDs (CT + HXG + two event data words).
   -- These are trusted local facts, not authorizations accepted from clients.
   -- Repeated scheduling controls share the monotonic notification-fence pool;
   -- re-enable reserves capacity for a subsequent disable. Backpressure alone
   -- restores the previous phase/fence; queued or uncertain sends spend it.
   -- Disabled is a scheduling acknowledgement, not proof of a flushed engine
   -- or permission to mutate PTEs. The owner supplies those separate gates.
   procedure Initialize (Object : in out Context; ID : Unsigned_32;
                         Fence_Base, Fence_Last : Unsigned_16; Ownership_Ready : Boolean)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   procedure Prepare (Object : in out Context; Action : Operation;
                      Fence : out Unsigned_16; Accepted : out Boolean)
     with Post => (if State (Object)'Old = Quarantined then
                     State (Object) = Quarantined and not Accepted);
   -- Called once after the serialized non-reentrant send. Backpressure means
   -- explicitly no publication; Uncertain includes partial writes/transport
   -- failure. Pending is recorded before send, not after notification.
   procedure Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   -- Repeated single-LRC scheduling notifications while already enabled.
   -- Caller has published a new tail with the required cache ordering. A
   -- notification is NOT a GPU completion and holds no scheduling-done credit.
   -- Fences above the four initial controls are never reused after queued or
   -- uncertain publication; exhaustion rejects without wrapping. The caller
   -- reserves [Fence_Base, Fence_Last] for this context lifetime. Four fences
   -- are required initially; extra capacity serves notifications and resume.
   procedure Prepare_Notification
     (Object : in out Context; Fence : out Unsigned_16; Accepted : out Boolean)
     with Post => (if Accepted then Fence <= Last_Fence (Object) else Fence = 0);
   procedure Notification_Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   -- Dispatcher has already checked HXG origin/type/shape. A failure for any
   -- previously attempted fence quarantines even after later actions queued.
   procedure Failed_Request (Object : in out Context; Fence : Unsigned_16;
                             Matched : out Boolean)
     with Post => (if Matched or State (Object)'Old = Quarantined then
                     State (Object) = Quarantined);
   -- Dispatcher validates wire framing separately. No batch-completion claim.
   procedure Scheduling_Done (Object : in out Context; ID, Runnable : Unsigned_32;
                              Accepted : out Boolean)
     with Post =>
       (if State (Object)'Old = Quarantined then State (Object) = Quarantined) and then
       (if Accepted then State (Object) in Enabled | Disabled and Credits_Held (Object) = 0);
   procedure Fail (Object : in out Context)
     with Post => State (Object) = Quarantined;
private
   type Sent_Set is array (Operation) of Boolean;
   type Context is limited record
      Value : Phase := Fresh;
      ID : Unsigned_32 := 65535;
      Base : Unsigned_16 := 0;
      Last : Unsigned_16 := 0;
      Used : Sent_Set := [others => False];
      Active : Operation := Register_Context;
      Before_Send : Phase := Fresh;
      Previously_Used, Dynamic_Fence : Boolean := False;
      Sending : Boolean := False;
      Credits : Natural range 0 .. 4 := 0;
      Notification_Sending : Boolean := False;
      Next_Notification : Unsigned_32 range 0 .. 65536 := 0;
   end record;
end Intel_GPU_GuC_Context_Lifecycle;
