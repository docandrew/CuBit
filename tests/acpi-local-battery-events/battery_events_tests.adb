pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Local_Runtime;
with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Local_Runtime; with ACPI_Delay_Domains; with Firmware_Tables;
with ACPI_Battery;with ACPI_Battery_Information;with ACPI_Battery_Information_Reader;
with ACPI_Battery_Legacy_Information;with ACPI_Battery_Legacy_Information_Reader;with ACPI_Battery_Information_Selection;
with ACPI_Battery_Observation;with ACPI_Battery_Inventory;with ACPI_Requests.Battery_Observations;
with ACPI_Requests.Battery_Observations.With_Local_Events;with ACPI_Requests.With_Notification_Routing;
with ACPI_Local_Sample_Status;
procedure Battery_Events_Tests is
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
   Bix_Calls,Bif_Calls,Bst_Calls,Reports,Clock_Reads:Natural:=0;
   Source_Healthy:Boolean:=True;
   function Event_Health return Boolean is (Source_Healthy);
   procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status) is
      Ignored:R.Outcome;
   begin
      R.Evaluate_Named(Service,Item,Budget,Result,Status);
      if Status=C.Values.Available then
         if C.Values.Node_Name(Item)="_BIX" then
            Bix_Calls:=Bix_Calls+1;
            if Mode="stopped" then R.Request_Stop(Ignored);Check(Ignored=R.Status.Stopped_By_Owner,"stop after static query");end if;
         end if;
         if C.Values.Node_Name(Item)="_BIF" then Bif_Calls:=Bif_Calls+1;end if;
         if C.Values.Node_Name(Item)="_BST" then Bst_Calls:=Bst_Calls+1;end if;
      end if;
   end Evaluate;
   package Format is new ACPI_Battery_Information(16);
   package Legacy_Format is new ACPI_Battery_Legacy_Information(16);
   package Dynamic is new ACPI_Battery(C,Evaluate);
   package Extended_Reader is new ACPI_Battery_Information_Reader(C,Format,Evaluate);
   package Legacy_Reader is new ACPI_Battery_Legacy_Information_Reader(C,Legacy_Format,Evaluate);
   package Selection is new ACPI_Battery_Information_Selection(C,Format,Legacy_Format,Extended_Reader,Legacy_Reader);
   package Collector is new ACPI_Battery_Observation(C,Format,Dynamic,Selection);
   package Batteries is new Q.Battery_Observations(Dynamic,Collector);
   package Inventory is new ACPI_Battery_Inventory(Collector,2);
   use type Inventory.Samples.Tick,Inventory.Samples.Availability,Inventory.Availability;
   use type Collector.Outcome,Selection.Source_Kind;
   procedure Read_Clock(Now:out Inventory.Samples.Tick;Available:out Boolean) is
   begin Clock_Reads:=Clock_Reads+1;Clock:=19+Interfaces.Unsigned_64(Clock_Reads);Now:=Inventory.Samples.Tick(Clock);Available:=True;end Read_Clock;
   Last_Kind:ACPI_Local_Sample_Status.Notice:=ACPI_Local_Sample_Status.No_Device;
   Last_View:Inventory.Samples.View;
   procedure Report_Publication(Server:Q.State;Kind:ACPI_Local_Sample_Status.Notice;
      Value:Inventory.Samples.View;Flags:Interfaces.Unsigned_64) is
      pragma Unreferenced(Flags);
   begin
      Check(R.Current=R.Status.Idle and not Q.Notifications_Pending(Server) and Source_Healthy,"inventory fact sink only after safe publication boundary");
      Reports:=Reports+1;Last_Kind:=Kind;Last_View:=Value;
   end Report_Publication;
   package E is new Batteries.With_Local_Events(Inventory,R.Current,Windows.Begin_Event,Windows.End_Event,
      Read_Clock,Report_Publication,Event_Health,Fuel,100);
   use type E.Status.Outcome,E.Status.Notice,E.Status.Failure,E.Collection_Outcome;
   procedure Route_All(Server:Q.State;Routed:out Boolean) is
   begin E.Route(Server,Routed);end Route_All;
   package Routing is new Q.With_Notification_Routing(Route_All);
   Server:Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work:Driver.Session;Reply:Q.Response;Accepted,Released:Boolean;Outcome:R.Outcome;Result:E.Status.Outcome;
   File:IO.File_Type;
begin
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"enable notifications");
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"begin snapshot");
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
   E.Start(Server,Accepted);Check(Accepted,"start battery consumer");E.Advance(Server,Result);
   if Mode in "limit"|"budget"|"stopped" then
      Check(Result=E.Status.Refused and Reports=0 and R.Current=R.Status.Stopped,"incomplete inventory cannot publish partial data");
      if Mode="limit" then Check(E.Collection_Report.Outcome=E.Inventory_Limit and E.Collection_Report.Count=2 and Bst_Calls=2,"third battery refuses without observing it");
      elsif Mode="budget" then Check(E.Collection_Report.Outcome=E.Budget_Exhausted and Bix_Calls=0 and Bif_Calls=0 and Bst_Calls=0,"one shared inventory budget stops before queries");
      else Check(E.Collection_Report.Outcome=E.Recovery_Required and Bif_Calls=0,"shared runtime stop never triggers legacy replay");end if;
      Check(E.Read(Server,(True,22),100).Status=Inventory.Samples.Not_Ready,"failed complete-inventory observation hidden");
   else
      Check(Result=E.Status.Pending and Reports=0 and R.Current=R.Status.Idle,"entire inventory collected in one closed window");
      Check(E.Read(Server,(True,21),100).Status=Inventory.Samples.Not_Ready,"pending inventory never exposed");
      if Mode="source-lost" then
         Source_Healthy:=False;E.Advance(Server,Result);
         Check(Result=E.Status.Refused and Reports=0 and E.Last_Failure=E.Status.Source_Unhealthy,"unhealthy source blocks inventory publication");
      else
         if Mode="self" then
            E.Advance(Server,Result);Check(Result=E.Status.Pending and Reports=0,"generated notification fences publication");
            Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
            Check(Reply.Status=Q.OK and Reply.Data(1)=1,"actual retained battery event routed");
            Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),Reply.Data(1),0,0],others=><>),Reply);
            Check(Reply.Status=Q.OK and not Q.Notifications_Pending(Server),"actual receipt ACK completes after fanout");
         end if;
         E.Advance(Server,Result);
         if Mode="self" then
            Check(Result=E.Status.Idle and Last_Kind=E.Status.Publication_Refused,"self notice invalidates inventory");
            E.Advance(Server,Result);Check(Result=E.Status.Idle and Bix_Calls=1 and Bif_Calls=1 and Bst_Calls=2,"no repeated battery AML after self invalidation");
         else
            Check(Result=E.Status.Idle and Reports=1 and Last_Kind=E.Status.Sample_Available and Last_View.Status=Inventory.Samples.Available,"complete inventory published");
            if Mode="empty" then Check(Last_View.Value.Count=0 and Bix_Calls=0 and Bif_Calls=0 and Bst_Calls=0,"complete empty inventory is not an acquisition failure");
            else
               Check(Last_View.Value.Count=2,"both battery entries retained");
               Check(Last_View.Value.Items(2).Status=(if Mode="absent" then Inventory.Known_Absent else Inventory.Sampled),"per-battery absence separate from sampled status");
               if Mode="absent" then Check(Bif_Calls=0 and Bst_Calls=1,"absent battery does not query information/status");
               else Check(Last_View.Value.Items(2).Value.Information.Source=Selection.Legacy_Reported and Bif_Calls=1,"canonical missing BIX selects one legacy information query");end if;
               if Mode="malformed-bix" then
                  Check(Last_View.Value.Items(1).Status=Inventory.Acquisition_Failed and Last_View.Value.Items(1).Reason=Collector.Information_Failed and
                    Last_View.Value.Items(1).Value.Information.Source=Selection.None_Reported and Bst_Calls=1,"bad BIX is a failed entry without BIF fallback or BST");
               elsif Mode="presence-changed" then
                  Check(Last_View.Value.Items(1).Status=Inventory.Acquisition_Failed and Last_View.Value.Items(1).Reason=Collector.Presence_Changed and
                    Last_View.Value.Items(1).Value.Information.Source=Selection.None_Reported,"presence change discards the entire per-battery bundle");
               else Check(Last_View.Value.Items(1).Status=Inventory.Sampled and Last_View.Value.Items(1).Value.Information.Source=Selection.Extended_Reported,"extended information provenance retained");end if;
            end if;
         end if;
      end if;
   end if;
   Check(E.Collection_Report.Charged<=Fuel,"inventory diagnostic accepted charge bounded");
   Check(Enter_Calls=1 and Claim_Calls=1 and Begin_Calls=1 and End_Calls=1,"one runtime and one inventory window");
   R.Request_Stop(Outcome);Check(Close_Calls=1 and Stop_Calls=1,"shared owner closes once");
   Q.Release_Storage(Server,Released);Check(Released,"release hosted storage");
   Ada.Text_IO.Put_Line("Battery events "&Mode&" checks"&Checks'Image);
end Battery_Events_Tests;
