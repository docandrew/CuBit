pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Local_Runtime;
with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Local_Runtime; with ACPI_Delay_Domains; with Firmware_Tables;
with ACPI_Lid;with ACPI_Power_Source;with ACPI_Requests.Lids;with ACPI_Requests.Power_Sources;
with ACPI_Local_Sample_Status;
with ACPI_Requests.With_Notification_Routing;with ACPI_Local_Sample_Events;
procedure Sample_Events_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package D renames ACPI_Delay_Domains; package IO renames Ada.Streams.Stream_IO;
   use type C.Context_Key, Q.Outcome, Q.Phase, Q.Revision_Number;
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
   begin
      if Ada.Command_Line.Argument(2)="end-failed" then Accepted:=False;
      else D.End_Event(Domain,Clock,True,Accepted);end if;
   end End_Delays;
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
   package Driver is new ACPI_Device_Initialization (C,No_Regions);
   package Controller is new Q.With_Driver (Driver);
   Mode : constant String:=Ada.Command_Line.Argument(2);
   Member_Calls,Source_Calls,Clock_Reads,Reports : Natural:=0;
   Other_Ready,Source_Healthy : Boolean:=True;
   function Event_Health return Boolean is (Source_Healthy);
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      if C.Values.Node_Name(Item)="_LID" then Member_Calls:=Member_Calls+1;end if;
      if C.Values.Node_Name(Item)="_PSR" then Source_Calls:=Source_Calls+1;end if;
      R.Evaluate_Named(Service,Item,Budget,Result,Status);
   end Evaluate;
   package L is new ACPI_Lid (C,Evaluate);package P is new ACPI_Power_Source (C,Evaluate);
   package Lids is new Q.Lids (L);package Powers is new Q.Power_Sources (P);
   use type Lids.Controller.Discovery_Outcome,Lids.Cached.Refresh_Outcome;
   use type L.Query_Outcome,L.Device_Status.Status_Outcome,Lids.Samples.Availability,Lids.Samples.Tick;
   use type Powers.Controller.Sampling_Outcome;
   procedure Collect (Data : in out Lids.Cache; Server : in out Q.State; Budget : Natural;
      Healthy,Collected : out Boolean) is
      Search : Lids.Controller.Discovery_Report; Query : Lids.Cached.Refresh_Report;
      Remaining : Natural:=Budget;
   begin
      Collected:=False;Lids.Bind(Data,Server,Remaining,Search);
      Healthy:=Search.Charged<=Remaining and then Search.Outcome in Lids.Controller.Found|Lids.Controller.No_Match;
      if not Healthy or else Search.Outcome/=Lids.Controller.Found then return;end if;
      Remaining:=Remaining-Search.Charged;Lids.Prepare(Data,Server,Remaining,Query);
      Collected:=Query.Status=Lids.Cached.Collected;
      Healthy:=Collected and then Query.Charged<=Remaining and then
        Query.Device_Status.Outcome not in L.Device_Status.Cleanup_Failed|L.Device_Status.Provider_Contract_Error and then
        Query.Query.Outcome not in L.Cleanup_Failed|L.Provider_Contract_Error;
      if Mode="collect-failed" then Healthy:=False;end if;
      if Mode="source-during-collect" then Source_Healthy:=False;end if;
   end Collect;
   procedure Publish (Data : in out Lids.Cache; Server : Q.State; At_Time : Lids.Samples.Clock_Reading;
      Published : out Boolean; Flags : out Interfaces.Unsigned_64) is
      Query : Lids.Cached.Refresh_Report;
   begin
      Lids.Publish(Data,Server,At_Time,True,Query);Published:=Query.Status=Lids.Cached.Updated;Flags:=Query.Device_Status.Flags;
   end Publish;
   function Cached_Read (Data : Lids.Cache; Server : Q.State; Now : Lids.Samples.Clock_Reading;
      Maximum_Age : Lids.Samples.Tick) return Lids.Samples.View is (Lids.Read(Data,Server,Now,Maximum_Age));
   procedure Read_Clock (Now : out Lids.Samples.Tick; Available : out Boolean) is
   begin
      Clock_Reads:=Clock_Reads+1;Available:=not(Mode="clock-missing" and Clock_Reads=1);
      Clock:=19+Interfaces.Unsigned_64(Clock_Reads);
      if Mode="clock-backwards" and Clock_Reads=2 then Clock:=19;end if;
      if Mode="publish-backwards" and Clock_Reads=3 then Clock:=20;end if;
      if Mode="overflow" then Clock:=Interfaces.Unsigned_64'Last-10;end if;
      if Mode="delayed" and Clock_Reads>=3 then Clock:=500+Interfaces.Unsigned_64(Clock_Reads);end if;
      Now:=Lids.Samples.Tick(Clock);
   end Read_Clock;
   Last_Kind : ACPI_Local_Sample_Status.Notice:=ACPI_Local_Sample_Status.No_Device;
   Last_View : Lids.Samples.View;
   procedure Report_Publication (Server : Q.State; Kind : ACPI_Local_Sample_Status.Notice;
      Value : Lids.Samples.View; Flags : Interfaces.Unsigned_64) is
      pragma Unreferenced(Flags);
   begin
      Check(R.Current=R.Status.Idle and not Q.Notifications_Pending(Server),"publication/logging hook runs only outside runtime and pending notifications");
      Reports:=Reports+1;Last_Kind:=Kind;Last_View:=Value;
   end Report_Publication;
   package E is new ACPI_Local_Sample_Events (Lids.Cached.Sample,Lids.Cached.Empty_Sample,Lids.Samples,Lids.Cache,
      Lids.Observe_Claimed,Lids.Invalidate,Collect,Publish,Cached_Read,R.Current,Windows.Begin_Event,Windows.End_Event,
      Read_Clock,Report_Publication,Event_Health,1000,100);
   use type E.Status.Outcome,E.Status.Notice,E.Status.Failure;
   procedure Route_All (Server : Q.State; Routed : out Boolean) is
      Done : Boolean;
   begin E.Route(Server,Done);Routed:=Done and then Other_Ready;end Route_All;
   package Routing is new Q.With_Notification_Routing (Route_All);
   Server,Foreign : Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work,Foreign_Work : Driver.Session;
   Reply : Q.Response;Accepted,Released : Boolean;Outcome : R.Outcome;Result : E.Status.Outcome;
   File : IO.File_Type;
   procedure Load (Target : in out Q.State; Run : in out Driver.Session; Table : Firmware_Tables.Bytes) is
   begin
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"configure notifications");
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"start snapshot");
      Q.Import_Block(Target,Q.Snapshot_Provider,Q.Revision(Target),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"stage actual fixture");
      Controller.Handle(Target,Run,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Target),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Current(Target)=Q.Complete and Q.Device_Completion(Target).Stage=ACPI_Bootstrap.Devices_Completed,"actual startup complete");
   end Load;
   procedure Drain is
      ID : Q.Receipt_Number;
   begin
      for Attempt in 1..4 loop
         Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
         Check(Reply.Status=Q.OK,"all local consumers observe claimed receipt");ID:=Reply.Data(1);exit when ID=0;
         Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),ID,0,0],others=><>),Reply);
         Check(Reply.Status=Q.OK,"retire only routed receipt");
      end loop;
      Check(not Q.Notifications_Pending(Server),"bounded fixture queue drained");
   end Drain;
begin
   if Mode="early" then
      E.Start(Server,Accepted);Check(not Accepted,"early consumer start refuses");
      E.Start(Server,Accepted);Check(not Accepted,"early start cannot be replayed");
      Q.Release_Storage(Server,Released);Check(Released,"release fresh test owner");
      Q.Release_Storage(Foreign,Released);Check(Released,"release fresh foreign test owner");
      Ada.Text_IO.Put_Line("Local sample early checks"&Checks'Image);return;
   end if;
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size : constant Natural:=Natural(IO.Size(File));Raw : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));
      Last : Ada.Streams.Stream_Element_Offset;Table : Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole fixture");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Load(Server,Work,Table);Load(Foreign,Foreign_Work,Table);
   end;
   Metadata.Advance(Server,Accepted);Check(Accepted,"same private Core runtime anchor");
   D.Start(Domain,10,1000,True,Accepted);Check(Accepted,"startup delay owner");
   Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Runtime_Entered,"transfer startup once");
   E.Start(Server,Accepted);Check(Accepted and E.Started,"start same-Core event consumer");
   E.Start(Server,Accepted);Check(not Accepted and E.Started,"repeat start leaves existing consumer intact");
   if Mode="foreign" then
      E.Advance(Foreign,Result);Check(Result=E.Status.Refused and E.Last_Failure=E.Status.Wrong_Context and Begin_Calls=0,"foreign request cannot run collection");
   else
      E.Advance(Server,Result);
      if Mode in "clock-missing"|"clock-backwards"|"overflow"|"collect-failed"|"end-failed"|"source-during-collect" then
         Check(Result=E.Status.Refused and Reports=0 and not E.Started,"failed collection/window never publishes or invokes logging hook");
         if Mode="clock-missing" then Check(Begin_Calls=0 and E.Last_Failure=E.Status.Clock_Unavailable,"missing clock before admission");
         elsif Mode="overflow" then Check(Begin_Calls=0 and E.Last_Failure=E.Status.Deadline_Overflow,"finite deadline overflow refuses before admission");
         else Check(R.Current=R.Status.Stopped,"unhealthy acquisition closes shared runtime");end if;
      else
         Check(Result=E.Status.Pending and Reports=0 and R.Current=R.Status.Idle,"one collection turn closes runtime before deferred publication");
         Check(E.Read(Server,(True,Lids.Samples.Tick(Clock)),100).Status=Lids.Samples.Not_Ready,"unpublished collection is unavailable");
         if Mode="source-lost" then
            Source_Healthy:=False;E.Advance(Server,Result);Check(Result=E.Status.Refused and Reports=0 and E.Last_Failure=E.Status.Source_Unhealthy,"source failure fences publication");
            Check(E.Read(Server,(True,22),100).Status=Lids.Samples.Not_Ready,"unhealthy source hides cached observation");
         elsif Mode="stop" then
            E.Stop;E.Advance(Server,Result);Check(Result=E.Status.Refused and Reports=0 and R.Current=R.Status.Idle,"consumer stop retains shared owner and suppresses publication");
         else
            if Mode="external" then
               Windows.Begin_Event(Server,1000,Outcome);Check(Outcome=R.Status.Event_Opened,"independent source window");
               declare Source : Powers.Controller.Sampling_Report;begin
                  Powers.Sample_Ready(Server,1000,Source);Check(Source.Outcome=Powers.Controller.Sampled and Source_Calls=1,"external source emits real Notify after collection");
               end;
               Windows.End_Event(Server,True,Outcome);Check(Outcome=R.Status.Event_Completed,"external source window closes");
            end if;
            if Mode in "self"|"external"|"fanout" then
               E.Advance(Server,Result);Check(Result=E.Status.Pending and Reports=0,"pending query/event receipts prevent publication");
               if Mode="fanout" then
                  Other_Ready:=False;
                  Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
                  Check(Reply.Status=Q.Incomplete and Reply.Data(1)=1,"other consumer holds actual receipt after this consumer routed");
                  E.Advance(Server,Result);Check(Result=E.Status.Pending and Reports=0,"held receipt fences publication even with empty FIFO");
                  Other_Ready:=True;
               end if;
               Drain;
            end if;
            E.Advance(Server,Result);
            if Mode="publish-backwards" then
               Check(Result=E.Status.Refused and Reports=0 and E.Last_Failure=E.Status.Clock_Reversed,"publication clock regression hides sample");
            elsif Mode in "self"|"fanout" then
               Check(Result=E.Status.Idle and Reports=1 and Last_Kind=E.Status.Publication_Refused,"query-generated notice invalidates publication");
               E.Advance(Server,Result);Check(Result=E.Status.Idle and Member_Calls=1,"query-generated notification cannot create resampling storm");
            elsif Mode="external" then
               Check(Result=E.Status.Pending and Reports=1 and Last_Kind=E.Status.Publication_Refused,"later external receipt is not lost with self-generated drain");
               E.Advance(Server,Result);Check(Result=E.Status.Pending and Member_Calls=2,"one coalesced resample for later external work");
               E.Advance(Server,Result);Check(Result=E.Status.Idle and Reports=2 and Last_Kind=E.Status.Sample_Available,"clean second acquisition publishes after new event");
            elsif Mode="delayed" then
               Check(Result=E.Status.Idle and Last_View.Status=Lids.Samples.Too_Old and Last_View.Time.Value=21,
                 "publication does not renew acquisition timestamp");
            elsif Mode="no-device" then
               Check(Result=E.Status.Idle and Reports=1 and Last_Kind=E.Status.No_Device and Member_Calls=0,"no matching family is distinct from observed absence");
            else
               Check(Result=E.Status.Idle and Reports=1 and Last_Kind=E.Status.Sample_Available and Last_View.Time.Value=21,"publish copied sample at collection timestamp");
               Check(E.Read(Server,(True,22),100).Status=Lids.Samples.Available,"cached observer copy becomes available only after publication");
            end if;
         end if;
      end if;
   end if;
   Check(Enter_Calls=1 and Claim_Calls=1,"one interpreter/runtime owner throughout");
   R.Request_Stop(Outcome);Check(Close_Calls=1 and Stop_Calls=1,"shared runtime closes once independently of consumer stop");
   Q.Release_Storage(Server,Released);Check(Released,"release main hosted owner");
   Q.Release_Storage(Foreign,Released);Check(Released,"release foreign hosted owner");
   Ada.Text_IO.Put_Line("Local sample "&Mode&" checks"&Checks'Image);
end Sample_Events_Tests;
