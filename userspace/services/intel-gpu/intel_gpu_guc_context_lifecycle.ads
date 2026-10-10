with Interfaces; use Interfaces;
package Intel_GPU_GuC_Context_Lifecycle with SPARK_Mode is
   pragma Unevaluated_Use_Of_Old (Allow);
   type Phase is (Fresh, Ready, Register_Pending, Registration_Queued,
                  Policy_Pending, Policy_Queued, Enable_Pending, Enabled,
                  Disable_Pending, Disabled, Deregister_Pending, Deregistered,
                  Quarantined);
   type Operation is (Register_Context, Set_Policy, Enable, Disable);
   type Send_Result is (Backpressure, Queued, Uncertain);
   type Context is limited private;
   function Scheduling_Stopped (Value : Phase) return Boolean is
     (Value in Disabled | Deregistered) with Global => null;
   function State (Object : Context) return Phase;
   function Credits_Held (Object : Context) return Natural;
   -- Publication of a control, SCHED_CONTEXT or deregistration in progress.
   function Control_Pending (Object : Context) return Boolean;
   function Notification_Pending (Object : Context) return Boolean;
   function Deregister_Sending (Object : Context) return Boolean;
   -- Resting submission state: scheduling acknowledged on or off, nothing
   -- in flight. GuC-resident contexts stay Enabled between submissions
   -- (Linux i915/xe: register and enable once, then SCHED_CONTEXT per job).
   function Can_Submit (Object : Context) return Boolean is
     (State (Object) in Enabled | Disabled and then
      not Control_Pending (Object) and then not Notification_Pending (Object)
      and then not Deregister_Sending (Object) and then Credits_Held (Object) = 0);
   -- State-only admission. Transport availability, ownership, and independent
   -- GPU drain/backing retirement remain caller obligations.
   function Can_Run_And_Retire (Object : Context) return Boolean;
   procedure Initialize (Object : in out Context; ID : Unsigned_32;
                         Ownership_Ready : Boolean)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   -- No wire IDs here. The exclusive transport assigns diagnostic IDs and
   -- routes failures globally; context completion is by event payload ID.
   procedure Prepare (Object : in out Context; Action : Operation;
                      Accepted : out Boolean)
     with Post => (if State (Object)'Old = Quarantined then
       State (Object) = Quarantined and not Accepted);
   procedure Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   -- SCHED_CONTEXT is a FAST request without a G2H reply: it reserves no
   -- receive credit and leaves the resting Enabled state unchanged, so a
   -- resident context can repeat it indefinitely (steady-state rendering).
   procedure Prepare_Notification (Object : in out Context; Accepted : out Boolean)
     with Post =>
       (if Can_Submit (Object)'Old and State (Object)'Old = Enabled then Accepted) and then
       (if Accepted then Notification_Pending (Object)) and then
       State (Object) = State (Object)'Old and then
       Credits_Held (Object) = Credits_Held (Object)'Old;
   procedure Notification_Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined)
       and then
       (if State (Object)'Old = Enabled and Notification_Pending (Object)'Old and
           not Control_Pending (Object)'Old and not Deregister_Sending (Object)'Old and
           Credits_Held (Object)'Old = 0 and Result /= Uncertain
        then State (Object) = Enabled and Can_Submit (Object));
   procedure Scheduling_Done (Object : in out Context; ID, Runnable : Unsigned_32;
                              Accepted : out Boolean)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined)
       and then (if Accepted then State (Object) in Enabled | Disabled
                   and Credits_Held (Object) = 0);
   procedure Fail (Object : in out Context)
     with Post => State (Object) = Quarantined;
   -- Deregistration needs no reserved wire ID or budget: it is admitted
   -- from every resting Disabled state, however many submissions preceded.
   procedure Prepare_Deregister (Object : in out Context; Accepted : out Boolean)
     with Post => (if Accepted then State (Object) = Deregister_Pending
                   and Credits_Held (Object) = 3) and then
       (if Can_Submit (Object)'Old and State (Object)'Old = Disabled then Accepted);
   procedure Deregister_Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   procedure Deregistration_Done
     (Object : in out Context; ID : Unsigned_32; Accepted : out Boolean)
     with Post => (if Accepted then State (Object) = Deregistered
                   and Credits_Held (Object) = 0);
private
   type Context is limited record
      Value : Phase := Fresh;
      ID : Unsigned_32 := 65535;
      Active : Operation := Register_Context;
      Before_Send : Phase := Fresh;
      Sending : Boolean := False;
      Credits : Natural range 0 .. 4 := 0;
      Notification_Sending : Boolean := False;
      Deregister_Sending : Boolean := False;
   end record;
   function Control_Pending (Object : Context) return Boolean is (Object.Sending);
   function Notification_Pending (Object : Context) return Boolean is
     (Object.Notification_Sending);
   function Deregister_Sending (Object : Context) return Boolean is
     (Object.Deregister_Sending);
end Intel_GPU_GuC_Context_Lifecycle;
