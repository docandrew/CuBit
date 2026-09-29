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
   -- One driver-owned context, no ID/fence reuse or re-enable in this bring-up
   -- lifetime. Caller serializes send/dispatch and reserves four unique fences
   -- globally plus four receive DWORDs (CT + HXG + two event data words).
   -- These are trusted local facts, not authorizations accepted from clients.
   procedure Initialize (Object : in out Context; ID : Unsigned_32;
                         Fence_Base : Unsigned_16; Ownership_Ready : Boolean)
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
      Used : Sent_Set := [others => False];
      Active : Operation := Register_Context;
      Sending : Boolean := False;
      Credits : Natural range 0 .. 4 := 0;
   end record;
end Intel_GPU_GuC_Context_Lifecycle;
