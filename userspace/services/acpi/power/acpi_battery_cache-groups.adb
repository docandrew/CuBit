package body ACPI_Battery_Cache.Groups with SPARK_Mode is
   use type Core.Values.Access_Status;
   procedure Bind (Group : in out Owner; Service : Core.State; Index : Slot;
      Device : Core.Node_Descriptor; Result : out Binding_Result) is
   begin
      ACPI_Battery_Cache.Bind (Group.Caches (Index), Service, Device, Result);
   end Bind;
   procedure Forget (Group : in out Owner; Index : Slot) is
   begin
      ACPI_Battery_Cache.Forget (Group.Caches (Index));
   end Forget;
   procedure Invalidate_All (Group : in out Owner) is
   begin
      for Index in Slot loop Invalidate (Group.Caches (Index)); end loop;
   end Invalidate_All;
   function Header (Group : Owner; Index : Slot; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Pages.Words is
     (Pages.Header (Group.Caches (Index), Now, Maximum_Age));
   function Read_Page (Group : Owner; Index : Slot; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick; Expected_Revision, Page_Number,
      Offset : Interfaces.Unsigned_64) return Pages.Words is
     (Pages.Read_Page (Group.Caches (Index), Now, Maximum_Age,
        Expected_Revision, Page_Number, Offset));
   procedure Drain (Group : in out Owner; Service : in out Core.State;
      Result : out Batch) is
      Item : Core.Notification_Observation;
      Present : Boolean;
      Routed : Events.Routing_Result;
   begin
      Result := (others => <>);
      for Attempt in 0 .. Core.Notification_Count'Last loop
         Core.Take_Notification (Service, Item, Present, Result.Status);
         if Result.Status /= Core.Values.Available then
            Invalidate_All (Group); return;
         end if;
         if not Present then Result.Healthy := True; return; end if;
         -- No producer can run during this loop. Exhaustion means the owner
         -- assumptions were violated; refuse rather than index past the batch.
         if Result.Count = Event_Count'Last then
            Invalidate_All (Group); return;
         end if;
         Result.Count := Result.Count + 1;
         Result.Events (Result.Count) := Item;
         for Index in Slot loop
            Events.Apply (Group.Caches (Index), Service, Item, Routed);
         end loop;
      end loop;
      Invalidate_All (Group);
   end Drain;
   procedure Refresh (Group : in out Owner; Service : aliased in out Core.State;
      Index : Slot; Budget : Natural; Report : out Refresh_Report;
      Routed : out Batch) is
      Clock : Samples.Clock_Reading;
   begin
      Routed := (others => <>);
      Prepare_Refresh (Group.Caches (Index), Service, Budget, Report);
      if Report.Status /= Collected then return; end if;
      Drain (Group, Service, Routed);
      if Routed.Healthy then Clock := Read_Clock; end if;
      Publish_Refresh (Group.Caches (Index), Service, Clock, Routed.Healthy, Report);
   end Refresh;
end ACPI_Battery_Cache.Groups;
