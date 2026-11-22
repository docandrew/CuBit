package body ACPI_Bootstrap.Lids.Cached with SPARK_Mode is
   package C renames ACPI_Service;
   use type C.Namespace.Object_Kind;
   use type C.Namespace.Notification_Target_Kind;
   use type Driver.Query_Outcome;
   use type Samples.Publication_Result;
   use type Samples.Cache_Status;
   function Ready (Session : State) return Boolean is
     (Current (Session) = Complete and then Device_Completion (Session).Stage = Devices_Completed
      and then Notifications_Enabled (Session));
   function Current (Data : Cache; Session : State) return Boolean is
     (Data.Bound and then C.Current_Node (Session.Core, Data.Item.Device)
      and then C.Values.Node_Kind (Data.Item.Device) = C.Namespace.Device_Object);
   procedure Invalidate (Data : in out Cache) is
      Ignored : Samples.Invalidation_Result;
   begin Samples.Invalidate (Data.Values, Ignored); end Invalidate;
   procedure Forget (Data : in out Cache) is
   begin
      Invalidate (Data); Data.Item := No_Binding; Data.Bound := False;
      Data.Pending := False; Data.Query := (others => <>); Data.Admission := Not_Ready;
   end Forget;
   procedure Bind (Data : in out Cache; Session : in out State; Budget : Natural;
      Report : out Discovery_Report) is
   begin
      Forget (Data);
      if Samples.Status (Data.Values) = Samples.Exhausted then Report := (Outcome => Failed, others => <>); return; end if;
      Find (Session, Budget, Data.Item, Report);
      Data.Bound := Report.Outcome = Found;
   end Bind;
   function Read (Data : Cache; Now : Samples.Clock_Reading;
      Maximum_Age : Samples.Tick) return Samples.View is
     (Samples.Read (Data.Values, Now, Maximum_Age));
   procedure Prepare (Data : in out Cache; Session : in out State; Budget : Natural;
      Report : out Refresh_Report) is
   begin
      Report := (others => <>);
      if Data.Pending then Report.Status := Busy; return; end if;
      if not Data.Bound then return; end if;
      Invalidate (Data);
      if Samples.Status (Data.Values) = Samples.Exhausted then Report.Status := Counter_Exhausted; return; end if;
      if not Ready (Session) then Report.Status := Not_Ready; return; end if;
      if not Current (Data, Session) then Forget (Data); Report.Status := Invalid_Binding; return; end if;
      Data.Expected := Samples.Revision (Data.Values);
      Query (Session, Data.Item, Budget, Data.Admission, Data.Query);
      Report := (Collected, Data.Admission, Data.Query, Samples.Not_Pending);
      Data.Pending := True;
   end Prepare;
   procedure Observe_Notification (Data : in out Cache; Session : State;
      Event : C.Notification_Observation) is
   begin
      if not Data.Bound then return; end if;
      if not Current (Data, Session) then Forget (Data); return; end if;
      if Event.Current_Target and then Event.Target_Kind = C.Namespace.Device_Notification
        and then Event.Node = C.Values.Position (Data.Item.Device)
      then Invalidate (Data); end if;
   end Observe_Notification;
   procedure Publish (Data : in out Cache; Session : State;
      At_Time : Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Refresh_Report) is
      Kind : Samples.Publication := Samples.Acquisition_Failed;
      Value : Sample := Empty_Sample;
   begin
      Report := (Publication_Refused, Data.Admission, Data.Query, Samples.Not_Pending);
      if not Data.Pending then return; end if;
      Data.Pending := False;
      if not Events_Healthy or else Pending_Notifications (Session) /= 0 then Invalidate (Data); Report.Status := Routing_Failed;
      elsif not Ready (Session) then Invalidate (Data); Report.Status := Not_Ready;
      elsif not Current (Data, Session) then Forget (Data); Report.Status := Invalid_Binding;
      else
         if Data.Admission = Admitted and then Data.Query.Outcome = Driver.Sampled then
            Kind := Samples.Sampled; Value := (Data.Query.State, Data.Query.Raw);
         end if;
         Samples.Publish (Data.Values, Data.Expected, Kind, Value, At_Time, Report.Publication);
         if Report.Publication = Samples.Published then Report.Status := Updated; end if;
      end if;
      Data.Query := (others => <>); Data.Admission := Not_Ready;
   end Publish;
end ACPI_Bootstrap.Lids.Cached;
