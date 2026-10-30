package body ACPI_Battery_Cache with SPARK_Mode is
   use type Samples.Invalidation_Result;
   use type Samples.Publication_Result;
   use type Core.Namespace.Object_Kind;
   use type Collector.Outcome;
   use type Core.Node_Descriptor;
   function Current (Service : Core.State; Device : Core.Node_Descriptor) return Boolean is
     (Core.Current_Node (Service, Device)
      and then Core.Values.Node_Kind (Device) = Core.Namespace.Device_Object);
   procedure Bind (Cache : in out State; Service : Core.State;
      Device : Core.Node_Descriptor; Result : out Binding_Result) is
      Cleared : Samples.Invalidation_Result;
   begin
      Cache.Device := Core.Values.No_Node; Cache.Has_Binding := False;
      Samples.Invalidate (Cache.Data, Cleared);
      if Cleared = Samples.Counter_Exhausted then Result := Counter_Exhausted;
      elsif not Current (Service, Device) then Result := Invalid_Device;
      else Cache.Device := Device; Cache.Has_Binding := True; Result := Bound;
      end if;
   end Bind;
   procedure Invalidate (Cache : in out State) is
      Ignored : Samples.Invalidation_Result;
   begin
      Samples.Invalidate (Cache.Data, Ignored);
   end Invalidate;
   procedure Forget (Cache : in out State) is
   begin
      Cache.Device := Core.Values.No_Node; Cache.Has_Binding := False;
      Invalidate (Cache);
   end Forget;
   function Read (Cache : State; Now : Samples.Clock_Reading;
                  Maximum_Age : Samples.Tick) return Samples.View is
     (Samples.Read (Cache.Data, Now, Maximum_Age));
      procedure Prepare_Refresh (Cache : in out State; Service : aliased in out Core.State;
         Budget : Natural; Report : out Refresh_Report) is
         Cleared : Samples.Invalidation_Result;
         Update : Pending_Update renames Cache.Update;
      begin
         Report := (others => <>);
         if Update.Armed then Report.Status := Already_Pending; return; end if;
         Update.Armed := False; Update.Expected := 0;
         Update.Device := Core.Values.No_Node; Update.Value := Collector.Empty_Observation;
         Update.Kind := Samples.Acquisition_Failed;
         if not Cache.Has_Binding then return; end if;
         if not Current (Service, Cache.Device) then
            Forget (Cache); Report.Status := Invalid_Binding; return;
         end if;
         Samples.Invalidate (Cache.Data, Cleared);
         if Cleared = Samples.Counter_Exhausted then Report.Status := Counter_Exhausted; return; end if;
         Update.Expected := Samples.Revision (Cache.Data);
         if not Samples.Can_Publish (Cache.Data) then
            Samples.Publish (Cache.Data, Update.Expected, Samples.Acquisition_Failed,
               Collector.Empty_Observation, (others => <>), Report.Publication);
            Report.Status := Counter_Exhausted; return;
         end if;
         Collector.Query (Service, Cache.Device, Budget, Report.Collection);
         Update.Kind := (if Report.Collection.Status = Collector.Sampled then Samples.Sampled
                  elsif Report.Collection.Status = Collector.Absent then Samples.Known_Absent
                  else Samples.Acquisition_Failed);
         Update.Value := Report.Collection.Value;
         Update.Collection := Report.Collection;
         Update.Device := Cache.Device; Update.Armed := True;
         Report.Status := Collected;
      end Prepare_Refresh;
      procedure Publish_Refresh (Cache : in out State; Service : Core.State;
         At_Time : Samples.Clock_Reading;
         Events_Healthy : Boolean; Report : out Refresh_Report) is
         Update : Pending_Update renames Cache.Update;
      begin
         Report := (Collection => Update.Collection, others => <>);
         Report.Events_Drained := Events_Healthy;
         Report.Publication := Samples.Not_Pending;
         if not Update.Armed then Report.Status := Publication_Refused; return; end if;
         Update.Armed := False;
         if not Events_Healthy then
            Invalidate (Cache); Report.Status := Events_Failed; return;
         end if;
         if not Cache.Has_Binding or else not Current (Service, Cache.Device) then
            Forget (Cache); Report.Status := Invalid_Binding; return;
         end if;
         if Cache.Device /= Update.Device or else not Current (Service, Update.Device) then
            Report.Status := Publication_Refused; return;
         end if;
         Samples.Publish (Cache.Data, Update.Expected, Update.Kind, Update.Value,
                           At_Time, Report.Publication);
         Update.Value := Collector.Empty_Observation; Update.Device := Core.Values.No_Node;
         Report.Status := (if Report.Publication = Samples.Published then Updated
                           elsif Report.Publication = Samples.Counter_Exhausted then Counter_Exhausted
                           else Publication_Refused);
      end Publish_Refresh;
end ACPI_Battery_Cache;
