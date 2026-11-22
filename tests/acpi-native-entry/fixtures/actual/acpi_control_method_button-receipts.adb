pragma Ada_2022;
package body ACPI_Control_Method_Button.Receipts with SPARK_Mode is
   use type Receipt_Number, Device_Status.Status_Outcome;
   use type Core.Namespace.Notification_Target_Kind, Core.Namespace.Notification_Class;
   Press_Notification : constant Core.Namespace.Notification_Number := 16#80#;
   function Is_Bound (Item : State; Service : Core.State) return Boolean is
     (Item.Has_Binding and then Core.Current_Node (Service, Item.Device));
   procedure Bind (Item : in out State; Service : aliased in out Core.State;
      Device : Core.Node_Descriptor; Budget : Natural; Report : out Binding_Report) is
   begin
      Report := (others => <>);
      if Item.Attempted then Report.Outcome := Already_Attempted;return;end if;
      Item.Attempted := True;
      Identify (Service, Device, Budget, Report.Identity);
      if Report.Identity.Charged > Budget then
         Report.Charged := Budget;Report.Outcome := Provider_Error;return;
      end if;
      Report.Charged := Report.Identity.Charged;
      if Report.Identity.Outcome /= Matched then Report.Outcome := Identity_Refused;return;end if;
      Device_Status.Observe_Status (Service, Device, Budget - Report.Charged, Report.Status);
      if Report.Status.Charged > Budget - Report.Charged then
         Report.Charged := Budget;Report.Outcome := Provider_Error;return;
      end if;
      Report.Charged := Report.Charged + Report.Status.Charged;
      if Report.Status.Outcome /= Device_Status.Status_Available then Report.Outcome := Status_Refused;
      elsif not Device_Status.Usable (Report.Status) then Report.Outcome := Device_Unavailable;
      elsif not Core.Current_Node (Service, Device) then Report.Outcome := Provider_Error;
      else Item.Device := Device;Item.Has_Binding := True;Report.Outcome := Bound;end if;
   end Bind;
   procedure Observe (Item : in out State; Service : Core.State;
      ID : Receipt_Number; Event : Core.Notification_Observation; Result : out Observation) is
   begin
      Result := Rejected;if ID = 0 then return;end if;
      if not Is_Bound (Item, Service) then
         if Item.Has_Binding then Item.Has_Binding := False;Result := Binding_Lost;
         else Result := Ignored;end if;
         return;
      end if;
      Result := Ignored;if ID <= Item.Last_Receipt then return;end if;
      Item.Last_Receipt := ID;
      if Event.Current_Target and then Event.Target_Kind = Core.Namespace.Device_Notification
        and then Event.Class = Core.Namespace.Device_Notify
        and then Event.Node = Core.Values.Position (Item.Device)
        and then Event.Number = Press_Notification then Result := Pressed;end if;
   end Observe;
end ACPI_Control_Method_Button.Receipts;
