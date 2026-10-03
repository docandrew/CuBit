with Interfaces; use Interfaces;
package Intel_GPU_GuC_Context_Lifecycle with SPARK_Mode is
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
   procedure Prepare_Notification (Object : in out Context; Accepted : out Boolean);
   procedure Notification_Sent (Object : in out Context; Result : Send_Result)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined);
   procedure Scheduling_Done (Object : in out Context; ID, Runnable : Unsigned_32;
                              Accepted : out Boolean)
     with Post => (if State (Object)'Old = Quarantined then State (Object) = Quarantined)
       and then (if Accepted then State (Object) in Enabled | Disabled
                   and Credits_Held (Object) = 0);
   procedure Fail (Object : in out Context)
     with Post => State (Object) = Quarantined;
   procedure Prepare_Deregister (Object : in out Context; Accepted : out Boolean)
     with Post => (if Accepted then State (Object) = Deregister_Pending
                   and Credits_Held (Object) = 3);
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
end Intel_GPU_GuC_Context_Lifecycle;
