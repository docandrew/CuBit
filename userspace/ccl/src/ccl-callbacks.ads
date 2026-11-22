with CCL.Callback_Queues;
with CCL.Language.Handlers;
with CCL.Catalog;
with CCL.Host_Values;
with Interfaces;

--  Owner/event-loop confined, not a concurrent container. The owner must
--  outlive a draining invocation and its host bindings. No source or input
--  sender can replace the admitted handler or supply dispatch authority.
package CCL.Callbacks with SPARK_Mode is
   package Events is new CCL.Callback_Queues;
   subtype Fuel_Budget is Positive range 1 .. 4096;
   type Registration is limited private;
   type Register_Result is (Registered, Invalid_Handler, Busy, Identity_Exhausted);
   procedure Register
     (Item : in out Registration; Handler : CCL.Language.Handlers.Handler;
      Fuel : Fuel_Budget; Target : out Events.Reference; Result : out Register_Result);
   procedure Enqueue
     (Item : in out Registration; Target : Events.Reference; Result : out Events.Enqueue_Result);
   procedure Close (Item : in out Registration; Discarded : out Events.Pending_Count);
   function State (Item : Registration) return Events.Lifecycle;
   function Pending (Item : Registration) return Events.Pending_Count;

   --  Pump ONE event outside input/paint callbacks. A fresh evaluator, arena
   --  and fuel budget are used each time. Never retry or coalesce clicks.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Dispatch_One
     (Item : in out Registration; Current_Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Ran : out Boolean;
      Outcome : out CCL.Language.Interpretation_Result;
      Discarded : out Events.Pending_Count);
private
   type Registration is limited record
      Queue : Events.Queue;
      Program : CCL.Language.Handlers.Handler;
      Budget : Fuel_Budget := 4096;
   end record;
   function State (Item : Registration) return Events.Lifecycle is (Events.State (Item.Queue));
   function Pending (Item : Registration) return Events.Pending_Count is (Events.Pending (Item.Queue));
end CCL.Callbacks;
