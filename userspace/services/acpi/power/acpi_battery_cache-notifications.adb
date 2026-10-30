package body ACPI_Battery_Cache.Notifications with SPARK_Mode is
   use type Core.Values.Access_Status;
   use type Core.Namespace.Notification_Target_Kind;
   use type Core.Namespace.Object_Kind;
   procedure Apply (Cache : in out State; Service : Core.State;
      Event : Core.Notification_Observation; Result : out Routing_Result) is
   begin
      Result := Ignored;
      if not Cache.Has_Binding then return; end if;
      if not Core.Current_Node (Service, Cache.Device)
        or else Core.Values.Node_Kind (Cache.Device) /= Core.Namespace.Device_Object
      then
         Forget (Cache); Result := Binding_Forgotten; return;
      end if;
      if not Event.Current_Target or else Event.Target_Kind /= Core.Namespace.Device_Notification
        or else Event.Node /= Core.Values.Position (Cache.Device)
      then return; end if;
      Invalidate (Cache); Result := Cache_Invalidated;
   end Apply;
   procedure Take (Cache : in out State; Service : in out Core.State;
      Event : out Core.Notification_Observation; Present : out Boolean;
      Status : out Core.Values.Access_Status; Result : out Routing_Result) is
   begin
      Core.Take_Notification (Service, Event, Present, Status);
      Result := Ignored;
      if Status = Core.Values.Available and then Present then
         Apply (Cache, Service, Event, Result);
      end if;
   end Take;
end ACPI_Battery_Cache.Notifications;
