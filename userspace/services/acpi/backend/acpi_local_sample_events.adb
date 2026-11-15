pragma Ada_2022;
with ACPI_Requests.With_Retained_Notification;
package body ACPI_Local_Sample_Events is
   package C renames ACPI_Service;
   package Q renames ACPI_Requests;
   package R renames ACPI_Local_Runtime_Status;
   use type C.Context_Key,Q.Receipt_Number;
   use type R.Phase,R.Outcome,Samples.Tick,Samples.Availability;
   function Started return Boolean is
     (State in Ready | Publishing and then Runtime_Current in R.Idle | R.Active and then Events_Healthy);
   function Last_Failure return Status.Failure is (Failure);
   procedure Fail (Why : Status.Failure; Result : out Status.Outcome) is
   begin State:=Stopped;Failure:=Why;Working:=False;Result:=Status.Refused;end Fail;
   procedure Stop is begin State:=Stopped;Failure:=Status.Owner_Stopped;end Stop;
   procedure Same_Context (Service : C.State; Completed : out Boolean) is
   begin Completed:=Anchor/=C.No_Context and then C.Current_Context(Service,Anchor);end Same_Context;
   package Metadata is new Q.With_Metadata_Continuation (Same_Context);
   procedure Start (Server : Q.State; Accepted : out Boolean) is
      procedure Bind (Service : C.State; Completed : out Boolean) is
      begin Anchor:=C.Capture_Context(Service);Completed:=Anchor/=C.No_Context;end Bind;
      package Binding is new Q.With_Metadata_Continuation (Bind);
   begin
      Accepted:=False;if Attempted then return;end if;Attempted:=True;
      if not Events_Healthy or else Runtime_Current/=R.Idle or else not Q.Device_Walk_Finished (Server) then Failure:=Status.Not_Ready;return;end if;
      Binding.Advance(Server,Accepted);if Accepted then State:=Ready;Needed:=True;else Failure:=Status.Not_Ready;end if;
   end Start;
   procedure Route (Server : Q.State; Routed : out Boolean) is
      type Token is limited null record;
      Dummy : Token;
      procedure Observe (S : in out Token; Service : C.State; ID : Q.Receipt_Number;
         Event : C.Notification_Observation; Accepted : out Boolean) is
         pragma Unreferenced(S,Event);
         Observed : Boolean;
      begin
         Accepted:=False;
         if not Started or else Working or else Runtime_Current/=R.Idle or else ID=0 or else
           not C.Current_Context(Service,Anchor) or else ID<Last_Receipt then return;end if;
         if ID=Last_Receipt then Accepted:=True;return;end if;
         Observe_Claimed(Data,Server,Observed);if not Observed then return;end if;
         Invalidate(Data);Last_Receipt:=ID;
         if State=Publishing then
            -- No producer interleaves with Collect. The first captured FIFO
            -- count therefore belongs to that query; later receipts are new
            -- external work and must not be lost when publication finishes.
            if Self_Notifications>0 then Self_Notifications:=Self_Notifications-1;
            else Resample:=True;end if;
         else Needed:=True;end if;
         Accepted:=True;
      end Observe;
      package Bridge is new Q.With_Retained_Notification (Token,Observe);
   begin Bridge.Route(Dummy,Server,Routed);end Route;
   function Read (Server : Q.State; Now : Samples.Clock_Reading; Maximum_Age : Samples.Tick) return Samples.View is
      Same : Boolean;
   begin
      Metadata.Advance(Server,Same);
      if not Same or else not Started or else State/=Ready or else Working or else Needed or else
        Runtime_Current/=R.Idle or else Q.Notifications_Pending(Server) then
         return (Value=>Empty_Sample,others=><>);
      end if;
      return Cached_Read(Data,Server,Now,Maximum_Age);
   end Read;
   procedure Advance (Server : in out Q.State; Result : out Status.Outcome) is
      Same,Available,Healthy,Published : Boolean;
      Now : Samples.Tick;
      Transition : R.Outcome;
      Flags : Interfaces.Unsigned_64 := 0;
      Value : Samples.View := (Value=>Empty_Sample,others=><>);
      Kind : Status.Notice;
      procedure Capture_Notifications (Service : C.State; Completed : out Boolean) is
      begin
         Completed:=C.Current_Context(Service,Anchor);
         if Completed then Self_Notifications:=C.Pending_Notifications(Service);end if;
      end Capture_Notifications;
      package Pending_Count is new Q.With_Metadata_Continuation (Capture_Notifications);
      procedure Finish (Outcome : Status.Outcome) is
      begin Working:=False;Result:=Outcome;end Finish;
   begin
      Result:=Status.Refused;
      if State not in Ready|Publishing or else Working or else Runtime_Current/=R.Idle then return;end if;
      Working:=True;if not Events_Healthy then Fail(Status.Source_Unhealthy,Result);return;end if;Metadata.Advance(Server,Same);
      if not Same then Fail(Status.Wrong_Context,Result);return;end if;
      if Q.Notifications_Pending(Server) then Finish(Status.Pending);return;end if;
      if State=Ready and then not Needed then Finish(Status.Idle);return;end if;
      Read_Clock(Now,Available);
      if not Available then Fail(Status.Clock_Unavailable,Result);return;end if;
      if Have_Clock and then Now<Last_Clock then Fail(Status.Clock_Reversed,Result);return;end if;
      Last_Clock:=Now;Have_Clock:=True;
      if not Events_Healthy then Fail(Status.Source_Unhealthy,Result);return;end if;
      if State=Publishing then
         if Self_Notifications/=0 then Fail(Status.Routing_Lost,Result);return;end if;
         if Collected then
            Publish(Data,Server,Collection_Time,Published,Flags);
            if Published then
               Value:=Cached_Read(Data,Server,(True,Now),Samples.Tick(Window_Microseconds));
               Kind:=(case Value.Status is when Samples.Available=>Status.Sample_Available,
                 when Samples.Missing=>Status.Sample_Missing,when others=>Status.Sample_Failed);
            else Kind:=Status.Publication_Refused;end if;
         else Kind:=Status.No_Device;end if;
         if not Events_Healthy or else Runtime_Current/=R.Idle or else Q.Notifications_Pending(Server) then Fail(Status.Not_Ready,Result);return;end if;
         Report_Publication(Server,Kind,Value,Flags);
         if State=Stopped then Fail(Status.Owner_Stopped,Result);return;end if;
         State:=Ready;Collected:=False;Needed:=Resample;Resample:=False;
         Finish((if Needed then Status.Pending else Status.Idle));return;
      end if;
      if Now>=Samples.Tick'Last-Samples.Tick(Window_Microseconds) then Fail(Status.Deadline_Overflow,Result);return;end if;
      Begin_Event(Server,Interfaces.Unsigned_64(Now+Samples.Tick(Window_Microseconds)),Transition);
      if Transition/=R.Event_Opened then Fail(Status.Window_Refused,Result);return;end if;
      Collected:=False;Resample:=False;Self_Notifications:=0;
      Collect(Data,Server,Fuel_Budget,Healthy,Collected);
      Pending_Count.Advance(Server,Same);Healthy:=Healthy and then Same and then State/=Stopped and then Events_Healthy;
      if Healthy and then Collected then
         Read_Clock(Now,Available);Healthy:=Available and then Now>=Last_Clock;
         Collection_Time:=(Available,Now);if Healthy then Last_Clock:=Now;end if;
      end if;
      Healthy:=Healthy and then Runtime_Current=R.Active;
      End_Event(Server,Healthy,Transition);
      if not Healthy or else Transition/=R.Event_Completed or else Runtime_Current/=R.Idle then
         Fail((if not Events_Healthy then Status.Source_Unhealthy else Status.Acquisition_Failed),Result);return;
      end if;
      State:=Publishing;Finish(Status.Pending);
   end Advance;
end ACPI_Local_Sample_Events;
