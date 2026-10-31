generic
package ACPI_Battery_Cache.Notifications with SPARK_Mode is
   type Routing_Result is (Ignored, Cache_Invalidated, Binding_Forgotten);
   -- Owner-only demultiplexing of an event dequeued from this same Service.
   -- Apply before any further firmware invocation can change target freshness.
   -- Copied metadata grants no authority; do not route client-supplied packets.
   -- Every notification for the bound current Device conservatively invalidates.
   procedure Apply (Cache : in out State; Service : Core.State;
      Event : Core.Notification_Observation; Result : out Routing_Result)
      with Pre => Core.Valid (Service);
   -- Dequeue and route atomically within the serialized owner. Return the copied
   -- event even when unmatched, so the owner can route it to other bindings.
   -- No queue-draining loop, firmware evaluation, or subscriber delivery here.
   procedure Take (Cache : in out State; Service : in out Core.State;
      Event : out Core.Notification_Observation; Present : out Boolean;
      Status : out Core.Values.Access_Status; Result : out Routing_Result)
      with Pre => Core.Valid (Service), Post => Core.Valid (Service);
end ACPI_Battery_Cache.Notifications;
