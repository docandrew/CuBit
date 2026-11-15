pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects;with HID_I2C_Descriptors;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Local_Runtime;
with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Local_Runtime; with ACPI_Delay_Domains; with Firmware_Tables;
with ACPI_HID_I2C_Candidate;with ACPI_HID_I2C_Discovery;with ACPI_HID_I2C_Resolved;with ACPI_HID_Inventory;
with ACPI_Requests.HID_Devices;with ACPI_Requests.HID_Devices.With_Local_Events;
with ACPI_Requests.With_Notification_Routing;with ACPI_Local_Sample_Status;
procedure HID_Events_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package D renames ACPI_Delay_Domains; package IO renames Ada.Streams.Stream_IO;
   use type C.Context_Key, C.Values.Access_Status, Q.Outcome, Q.Phase, Q.Revision_Number;
   use type ACPI_Bootstrap.Device_Phase, Ada.Streams.Stream_Element_Offset;
   Anchor : C.Context_Key := C.No_Context;
   Claimed, Closed : Boolean := False;
   Enter_Calls, Begin_Calls, End_Calls, Claim_Calls, Close_Calls, Stop_Calls, Checks : Natural := 0;
   Clock : Interfaces.Unsigned_64 := 11;
   Domain : D.State;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Why; end if; end Check;
   procedure Bind (Service : C.State; Completed : out Boolean) is
   begin Anchor:=C.Capture_Context(Service);Completed:=Anchor/=C.No_Context;end Bind;
   package Metadata is new Q.With_Metadata_Continuation (Bind);
   function Ready (Service : C.State) return Boolean is
     (not Closed and then Anchor/=C.No_Context and then C.Current_Context(Service,Anchor));
   function Idle return Boolean is (True);
   procedure Claim (Service : C.State; Accepted : out Boolean) is
   begin Claim_Calls:=Claim_Calls+1;Accepted:=Ready(Service) and then not Claimed;if Accepted then Claimed:=True;end if;end Claim;
   procedure Close_Owner is begin Close_Calls:=Close_Calls+1;Closed:=True;end Close_Owner;
   procedure Enter_Delays (Accepted : out Boolean) is
   begin D.Enter_Runtime(Domain,Clock,True,Accepted);end Enter_Delays;
   procedure Begin_Delays (Deadline : Interfaces.Unsigned_64; Accepted : out Boolean) is
   begin D.Begin_Event(Domain,Clock,Deadline,True,Accepted);end Begin_Delays;
   procedure End_Delays (Accepted : out Boolean) is
   begin D.End_Event(Domain,Clock,True,Accepted);end End_Delays;
   procedure Stop_Delays is begin Stop_Calls:=Stop_Calls+1;D.Stop(Domain);end Stop_Delays;
   package R is new ACPI_Local_Runtime (C,Ready,Idle,Claim,Close_Owner,Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
      C.Evaluate_Named,C.Invoke_Retained,C.Invoke_With_Arguments);
   use type R.Phase, R.Outcome;
   procedure Enter_Owner (Service : C.State; Result : out R.Outcome) is
   begin Enter_Calls:=Enter_Calls+1;R.Enter_Runtime(Service,Result);end Enter_Owner;
   procedure Begin_Owner (Service : C.State; Deadline : Interfaces.Unsigned_64; Result : out R.Outcome) is
   begin Begin_Calls:=Begin_Calls+1;R.Begin_Event(Service,Deadline,Result);end Begin_Owner;
   procedure End_Owner (Service : C.State; Completed : Boolean; Result : out R.Outcome) is
   begin End_Calls:=End_Calls+1;R.End_Event(Service,Completed,Result);end End_Owner;
   package Windows is new Q.With_Local_Runtime (Enter_Owner,Begin_Owner,End_Owner);
   procedure No_Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced(Service,Budget);
   begin Charged:=0;Outcome:=ACPI_Device_Policy.No_Regions_Required;end No_Regions;
   package Driver is new ACPI_Device_Initialization(C,No_Regions);
   package Controller is new Q.With_Driver(Driver);
   Mode:constant String:=Ada.Command_Line.Argument(2);
   Fuel:constant Positive:=(if Mode="budget" then 1 else ACPI_Local_Sample_Status.Default_Fuel);
   Crs_Calls,Dsm_Calls,Reports,Clock_Reads:Natural:=0;
   Source_Healthy:Boolean:=True;
   function Event_Health return Boolean is (Source_Healthy);
   procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status) is
      Ignored:R.Outcome;
   begin
      R.Evaluate_Named(Service,Item,Budget,Result,Status);
      if C.Values.Node_Name(Item)="_CRS" then
         Crs_Calls:=Crs_Calls+1;
         if Mode="stopped" then R.Request_Stop(Ignored);end if;
      end if;
   end Evaluate;
   procedure Invoke(Service:aliased in out C.State;Node:C.Namespace.Node_ID;
      Args:C.Values.Arguments;Argument_Count:C.Values.Argument_Count;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status) is
   begin Dsm_Calls:=Dsm_Calls+1;R.Invoke_With_Arguments(Service,Node,Args,Argument_Count,Budget,Result,Status);end Invoke;
   package Candidates is new ACPI_HID_I2C_Candidate(256);
   package Discovery is new ACPI_HID_I2C_Discovery(C,Invoke,Evaluate);
   package Resolver is new ACPI_HID_I2C_Resolved(C,Candidates);
   package Hids is new Q.HID_Devices(Discovery,Resolver);
   package Inventory is new ACPI_HID_Inventory(Resolver,2);
   use type Inventory.Samples.Tick,Inventory.Samples.Availability;
   use type Candidates.S.Slave_Address,Candidates.G.Pin_Number;
   use type HID_I2C_Descriptors.Register_Number;
   procedure Read_Clock(Now:out Inventory.Samples.Tick;Available:out Boolean) is
   begin Clock_Reads:=Clock_Reads+1;Clock:=19+Interfaces.Unsigned_64(Clock_Reads);Now:=Inventory.Samples.Tick(Clock);Available:=True;end Read_Clock;
   Last_Kind:ACPI_Local_Sample_Status.Notice:=ACPI_Local_Sample_Status.No_Device;
   Last_View:Inventory.Samples.View;
   procedure Report_Publication(Server:Q.State;Kind:ACPI_Local_Sample_Status.Notice;
      Value:Inventory.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Flags);
   begin Check(R.Current=R.Status.Idle and not Q.Notifications_Pending(Server) and Source_Healthy,"quiet copied-fact boundary");
      Reports:=Reports+1;Last_Kind:=Kind;Last_View:=Value;end Report_Publication;
   Resource_Bytes:constant Positive:=(if Mode="small" then 1 else 4096);
   package E is new Hids.With_Local_Events(Inventory,R.Current,Windows.Begin_Event,Windows.End_Event,
      Read_Clock,Report_Publication,Event_Health,Resource_Bytes,Fuel,100);
   use type E.Status.Outcome,E.Status.Notice,E.Collection_Outcome;
   procedure Route_All(Server:Q.State;Routed:out Boolean) is
   begin E.Route(Server,Routed);end Route_All;
   package Routing is new Q.With_Notification_Routing(Route_All);
   procedure Check_Roots(Service:C.State;Completed:out Boolean) is
   begin Completed:=(if Mode="stopped" then C.Retained_Results(Service)>0 else C.Retained_Results(Service)=0);end Check_Roots;
   package Roots is new Q.With_Metadata_Continuation(Check_Roots);
   Server:Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work:Driver.Session;Reply:Q.Response;Accepted,Released:Boolean;Outcome:R.Outcome;Result:E.Status.Outcome;
   Held:Hids.Controller.Probe;
   Held_Device:C.Namespace.Node_ID;
   Held_Search:Hids.Controller.Search_Report;
   Held_Admission:Hids.Controller.Admission;
   Held_Action:Discovery.Action_Result;
   use type Hids.Controller.Admission,Discovery.Action_Result;
   File:IO.File_Type;
begin
   Check(not Q.Retained_Results_Pending(Server),"fresh protocol owner has no retained results");
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"enable notifications");
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"begin snapshot");
   Check(not Q.Retained_Results_Pending(Server),"receiving protocol owner can observe retention");
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size:constant Natural:=Natural(IO.Size(File));Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));Last:Ada.Streams.Stream_Element_Offset;
      Table:Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"complete table");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Q.Import_Block(Server,Q.Snapshot_Provider,Q.Revision(Server),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"actual battery AML imported");
      Controller.Handle(Server,Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Current(Server)=Q.Complete and Q.Device_Completion(Server).Stage=ACPI_Bootstrap.Devices_Completed,"device startup complete");
   end;
   Metadata.Advance(Server,Accepted);Check(Accepted,"bind exact private Core");
   D.Start(Domain,10,1000,True,Accepted);Check(Accepted,"startup clock owner");
   Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Runtime_Entered,"transfer once");

   if Mode="root-query" then
      Check(not Q.Retained_Results_Pending(Server),"complete owner starts clear");
      Windows.Begin_Event(Server,Clock+100,Outcome);Check(Outcome=R.Status.Event_Opened,"retention probe window");
      Hids.Begin_Next_Probe(Held,Server,C.Namespace.Root,Fuel,Held_Device,Held_Search,Held_Admission,Held_Action);
      Check(Held_Admission=Hids.Controller.Admitted and Held_Action=Discovery.Accepted and Hids.Controller.Pending(Held),"actual CRS retained by probe");
      declare Revision:constant Q.Revision_Number:=Q.Revision(Server);begin
         for I in 1..3 loop Check(Q.Retained_Results_Pending(Server),"retention query observes actual held root");end loop;
         Check(Q.Revision(Server)=Revision,"retirement query does not mutate protocol");
      end;
      Hids.Cancel(Held,Server,Held_Action);
      Check(Held_Action=Discovery.Accepted and not Hids.Controller.Pending(Held) and not Q.Retained_Results_Pending(Server),"one cancel clears actual retained root");
      Windows.End_Event(Server,True,Outcome);Check(Outcome=R.Status.Event_Completed,"clear retention permits window close");
      R.Request_Stop(Outcome);Check(Outcome=R.Status.Stopped_By_Owner,"stop clear owner");
      Q.Release_Storage(Server,Released);Check(Released,"clear owner storage can release");
      Ada.Text_IO.Put_Line("HID events "&Mode&" checks"&Checks'Image);return;
   end if;
   E.Start(Server,Accepted);Check(Accepted,"start HID consumer");E.Advance(Server,Result);
   Check(Begin_Calls=1 and End_Calls=1,"one finite event window");
   Check(E.Collection_Report.Charged<=Fuel,"one total discovery/collection budget");
   if Mode in "small"|"limit"|"budget"|"missing"|"stopped" then
      Check(Result=E.Status.Refused,"typed refusal");
      Check(E.Read(Server,(Available=>True,Value=>30),100).Status/=Inventory.Samples.Available,"no partial inventory");
      if Mode="small" then Check(E.Collection_Report.Outcome=E.Capacity_Refused,"bounded copy refusal after one cleanup");
      elsif Mode="limit" then Check(E.Collection_Report.Outcome=E.Inventory_Limit and E.Collection_Report.Count=2,"no third probe");
      elsif Mode="budget" then Check(E.Collection_Report.Outcome=E.Budget_Exhausted,"budget failure");
      elsif Mode="missing" then Check(E.Collection_Report.Outcome=E.Sources_Failed,"missing controller source");
      else Check(E.Collection_Report.Outcome=E.Recovery_Required,"stopped retained owner");end if;
      Check(Reports=0,"no healthy callback on refusal");
   else
      Check(Result=E.Status.Pending and E.Collection_Report.Outcome=E.Complete,"complete staged inventory");
      Check(Reports=0,"not published during collection");
      if Mode="source-lost" then Source_Healthy:=False;end if;
      if Mode="self" then
         Check(Q.Notifications_Pending(Server),"actual CRS-generated Notify");
         while Q.Notifications_Pending(Server) loop
            Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
            Check(Reply.Status=Q.OK,"claim retained receipt");
            Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),Reply.Data(1),0,0],others=><>),Reply);
            Check(Reply.Status=Q.OK,"route before guarded ACK");
         end loop;
      end if;
      E.Advance(Server,Result);
      if Mode="source-lost" then Check(Result=E.Status.Refused and Reports=0,"source loss suppresses inventory");
      elsif Mode="self" then
         Check(Result=E.Status.Idle and Reports=1 and Last_Kind=E.Status.Publication_Refused,"self Notify invalidates staged inventory");
         Check(E.Read(Server,(Available=>True,Value=>30),100).Status/=Inventory.Samples.Available,"invalidated inventory hidden");
      else
         Check(Result=E.Status.Idle and Reports=1 and Last_View.Status=Inventory.Samples.Available,"whole inventory published");
         Check(Last_View.Value.Count=(if Mode="empty" then 0 else 2),"complete count");
         for I in 1..Last_View.Value.Count loop
            declare Item:constant Resolver.Observation:=Last_View.Value.Items(I);Facts:constant Candidates.Candidate:=Resolver.Facts(Item);begin
               Check(C.Values.Node_Name(Resolver.I2C_Controller(Item))="I2C0" and C.Values.Node_Name(Resolver.GPIO_Controller(Item))="GPIO","both resolved controller identities");
               Check(Facts.I2C.Address=16#15# and Facts.Pin=16#2A# and Facts.Descriptor_Register=16#20#,"exact copied touchpad resource facts");
            end;
         end loop;
      end if;
      declare Before:constant Natural:=Crs_Calls+Dsm_Calls;begin
         E.Advance(Server,Result);Check(Crs_Calls+Dsm_Calls=Before and Begin_Calls=1,"no autonomous AML retry");
      end;
   end if;
   Check(Q.Retained_Results_Pending(Server)=(Mode="stopped"),"protocol retirement view matches actual retained ownership");
   Roots.Advance(Server,Accepted);Check(Accepted,"retained CRS preserved on stopped owner, otherwise fully cleaned");
   if Mode="small" then Check(Crs_Calls=1 and Dsm_Calls=0,"capacity failure does not rerun CRS or execute DSM");end if;
   if Mode="stopped" then Check(Crs_Calls=1 and Dsm_Calls=0,"no further AML after retained stop");end if;
   if Mode="limit" then Check(Crs_Calls=2,"third candidate is not probed");end if;
   Check(Enter_Calls=1 and Claim_Calls=1,"same owner transferred once");
   if Mode="stopped" then
      Check(R.Current=R.Status.Stopped,"runtime remains stopped");
   else
      R.Request_Stop(Outcome);Check(Outcome in R.Status.Stopped_By_Owner|R.Status.Already_Stopped,"shared runtime remains stopped");
   end if;
   Check(Close_Calls=1 and Stop_Calls=1,"owner and delay close once");
   if Mode/="stopped" then
      Q.Release_Storage(Server,Released);Check(Released,"no retained CRS after normal/refusal cleanup");
   end if;
   Ada.Text_IO.Put_Line("HID events "&Mode&" checks"&Checks'Image);
end HID_Events_Tests;
