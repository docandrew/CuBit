with ACPI_Battery_Cache.Observers;
with ACPI_Battery_Cache.Notifications;
with Interfaces;
generic
   Maximum_Bindings : Positive;
   -- Trusted monotonic provider, called after collection and event routing.
   -- Must not invoke AML, reset/reenter Core, change caches or route events.
   with function Read_Clock return Samples.Clock_Reading;
package ACPI_Battery_Cache.Groups with SPARK_Mode is
   subtype Slot is Positive range 1 .. Maximum_Bindings;
   package Pages is new Observers;
   package Events is new Notifications;
   type Owner is limited private;
   -- Slots are service-owner selections, not public device IDs or authority.
   -- Each cache lives for the Owner's entire lifetime, including slot rebinding.
   procedure Bind (Group : in out Owner; Service : Core.State; Index : Slot;
      Device : Core.Node_Descriptor; Result : out Binding_Result)
      with Pre => Core.Valid (Service);
   procedure Forget (Group : in out Owner; Index : Slot);
   procedure Invalidate_All (Group : in out Owner);
   function Header (Group : Owner; Index : Slot; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Pages.Words;
   function Read_Page (Group : Owner; Index : Slot; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Pages.Words;
   subtype Event_Count is Natural range 0 .. Core.Notification_Count'Last;
   type Event_Array is array (Positive range 1 .. Core.Notification_Count'Last)
      of Core.Notification_Observation;
   type Batch is record
      Healthy : Boolean := False;
      Count : Event_Count := 0;
      Events : Event_Array := [others => (others => <>)];
      Status : Core.Values.Access_Status := Core.Values.Available;
   end record;
   -- Dequeue at most queue capacity events plus one empty probe. Immediately
   -- apply each event to every slot without intervening AML or callbacks.
   -- Every copied event is returned in FIFO order, including unrelated events.
   -- The outer owner must forward this batch to its other consumers before
   -- further AML/reset; Current_Target describes freshness at dequeue only.
   -- A failed drain invalidates every slot and never certifies publication.
   procedure Drain (Group : in out Owner; Service : in out Core.State;
      Result : out Batch) with Pre => Core.Valid (Service), Post => Core.Valid (Service);
   -- One owner operation: collect once, route the entire bounded queue, sample
   -- the clock only after successful routing, then consume the pending result.
   -- Calls never retry AML or deliver to peers. Return Batch to other owners
   -- before any subsequent firmware invocation. Notifications must already be
   -- enabled; initialization, device discovery/readiness and IO are external.
   procedure Refresh (Group : in out Owner; Service : aliased in out Core.State;
      Index : Slot; Budget : Natural; Report : out Refresh_Report;
      Routed : out Batch) with Pre => Core.Valid (Service), Post => Core.Valid (Service);
private
   type Cache_Array is array (Slot) of State;
   type Owner is limited record
      Caches : Cache_Array;
   end record;
end ACPI_Battery_Cache.Groups;
