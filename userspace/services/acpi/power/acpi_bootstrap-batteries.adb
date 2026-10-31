pragma Ada_2022;
package body ACPI_Bootstrap.Batteries with SPARK_Mode is
   package C renames ACPI_Service;
   package N renames C.Namespace;
   use type N.Object_Kind, Identity.Identification_Outcome;
   function Ready (Session : State) return Boolean is
     (Session.Stage = Complete and then Session.Device_Data.Stage = Devices_Completed
      and then Notifications_Enabled (Session));
   procedure Find
     (Session : in out State; After : N.Node_ID; Budget : Natural;
      Device : out N.Node_ID; Report : out Discovery_Report)
   is
      type Descriptors is array (N.Node_ID) of C.Node_Descriptor;
      Snapshot : Descriptors := [others => C.Values.No_Node];
      Last : N.Node_ID;
      Status : C.Values.Access_Status;
      function Same_Topology return Boolean is
      begin
         if not Ready (Session) or else C.Namespace_Node_Count (Session.Core) /= Last then return False; end if;
         for I in N.Root .. Last loop
            if not C.Current_Node (Session.Core, Snapshot (I)) then return False; end if;
         end loop;
         return True;
      end Same_Topology;
   begin
      Device := N.Root; Report := (others => <>);
      if not Ready (Session) then return; end if;
      Last := C.Namespace_Node_Count (Session.Core);
      if After > Last then Report.Outcome := Invalid_Cursor; return; end if;
      Report.Outcome := Incomplete;
      if Budget = 0 then return; end if;
      for I in N.Root .. Last loop
         C.Describe_Node (Session.Core, I, Snapshot (I), Status);
         if Status /= C.Values.Available then Report.Outcome := Failed; return; end if;
      end loop;
      for I in N.Root .. Last loop
         if I > After then
            if Report.Charged = Budget then return; end if;
            Report.Scanned := Report.Scanned + 1;
            if C.Values.Node_Kind (Snapshot (I)) = N.Device_Object then
               Report.Candidates := Report.Candidates + 1;
               Identity.Identify (Session.Core, Snapshot (I), Budget - Report.Charged, Report.Last_Identity);
               if Report.Last_Identity.Charged > Budget - Report.Charged then Report.Outcome := Failed; return; end if;
               Report.Charged := Report.Charged + Report.Last_Identity.Charged;
               if not Same_Topology then Report.Outcome := Failed; return; end if;
               case Report.Last_Identity.Outcome is
                  when Identity.Matched => Device := I; Report.Outcome := Found; return;
                  when Identity.Not_Battery | Identity.Missing_HID => null;
                  when Identity.Budget_Exhausted => return;
                  when others => Report.Outcome := Failed; return;
               end case;
            end if;
         end if;
      end loop;
      Report.Outcome := No_Match;
   end Find;
   procedure Bind
     (Data : in out Store.State; Session : State;
      Device : ACPI_Service.Namespace.Node_ID; Allowed : out Admission;
      Result : out Store.Binding_Result)
   is
      Item : ACPI_Service.Node_Descriptor;
      Status : ACPI_Service.Values.Access_Status;
   begin
      Allowed := Not_Ready; Result := Store.Invalid_Device;
      if not Ready (Session) then Store.Forget (Data); return; end if;
      Allowed := Admitted;
      ACPI_Service.Describe_Node (Session.Core, Device, Item, Status);
      if Status /= ACPI_Service.Values.Available then Store.Forget (Data); return; end if;
      Store.Bind (Data, Session.Core, Item, Result);
   end Bind;
   procedure Prepare
     (Data : in out Store.State; Session : in out State; Budget : Natural;
      Allowed : out Admission; Report : out Store.Refresh_Report) is
   begin
      Allowed := Not_Ready; Report := (others => <>);
      if not Ready (Session) then Store.Invalidate (Data); return; end if;
      Allowed := Admitted;
      Store.Prepare_Refresh (Data, Session.Core, Budget, Report);
   end Prepare;
   procedure Observe_Notification
     (Data : in out Store.State; Session : State;
      Event : ACPI_Service.Notification_Observation;
      Result : out Notifications.Routing_Result) is
   begin
      Notifications.Apply (Data, Session.Core, Event, Result);
   end Observe_Notification;
   procedure Publish
     (Data : in out Store.State; Session : State;
      At_Time : Store.Samples.Clock_Reading; Events_Healthy : Boolean;
      Report : out Store.Refresh_Report) is
   begin
      Store.Publish_Refresh (Data, Session.Core, At_Time,
        Events_Healthy and then Ready (Session) and then Pending_Notifications (Session) = 0, Report);
   end Publish;
   function Read
     (Data : Store.State; Session : State; Now : Store.Samples.Clock_Reading;
      Maximum_Age : Store.Samples.Tick) return Store.Samples.View
   is
      Result : Store.Samples.View := Store.Read (Data, Now, Maximum_Age);
   begin
      if not Ready (Session) or else Pending_Notifications (Session) /= 0 then
         Result := (Revision => Result.Revision, others => <>);
      end if;
      return Result;
   end Read;
end ACPI_Bootstrap.Batteries;
