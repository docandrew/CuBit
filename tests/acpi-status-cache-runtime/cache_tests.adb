pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Local_Runtime;
with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Local_Runtime; with ACPI_Delay_Domains; with Firmware_Tables;
with ACPI_Lid; with ACPI_Power_Source; with ACPI_Requests.Lids; with ACPI_Requests.Power_Sources;
procedure Cache_Tests is
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
   begin Clock:=Clock+1;D.Begin_Event(Domain,Clock,Deadline,True,Accepted);end Begin_Delays;
   procedure End_Delays (Accepted : out Boolean) is
   begin Clock:=Clock+1;D.End_Event(Domain,Clock,True,Accepted);end End_Delays;
   procedure Stop_Delays is begin Stop_Calls:=Stop_Calls+1;D.Stop(Domain);end Stop_Delays;
   package R is new ACPI_Local_Runtime (C,Ready,Idle,Claim,Close_Owner,Enter_Delays,Begin_Delays,End_Delays,Stop_Delays,
      C.Evaluate_Named,C.Invoke_Retained,C.Invoke_With_Arguments);
   use type R.Outcome;
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
   Lid_Calls,Power_Calls,Status_Calls : Natural := 0;
   procedure Evaluate (Service : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      if C.Values.Node_Name(Item)="_LID" then Lid_Calls:=Lid_Calls+1;end if;
      if C.Values.Node_Name(Item)="_PSR" then Power_Calls:=Power_Calls+1;end if;
      if C.Values.Node_Name(Item)="_STA" then Status_Calls:=Status_Calls+1;end if;
      R.Evaluate_Named(Service,Item,Budget,Result,Status);
   end Evaluate;
   package L is new ACPI_Lid (C,Evaluate);package P is new ACPI_Power_Source (C,Evaluate);
   package Lids is new Q.Lids (L);package Powers is new Q.Power_Sources (P);
   use type Lids.Controller.Discovery_Outcome,Powers.Controller.Discovery_Outcome;
   use type Lids.Cached.Refresh_Outcome,Powers.Cached.Refresh_Outcome;
   use type Lids.Samples.Availability,Powers.Samples.Availability;
   use type L.Lid_State,P.Source_State;
   Server : Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work : Driver.Session;Reply : Q.Response;Accepted,Released : Boolean;Outcome : R.Outcome;
   LC : Lids.Cache;PC : Powers.Cache;
   LS : Lids.Controller.Discovery_Report;PS : Powers.Controller.Discovery_Report;
   LR : Lids.Cached.Refresh_Report;PR : Powers.Cached.Refresh_Report;
   Mode : constant String:=Ada.Command_Line.Argument(2);
   Budget : constant Natural:=(if Mode="zero" then 0 else 1000);
   File : IO.File_Type;
begin
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"enable cache event tracking");
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"start snapshot");
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size : constant Natural:=Natural(IO.Size(File));Raw : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));Last : Ada.Streams.Stream_Element_Offset;Table : Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole table");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Q.Import_Block(Server,Q.Snapshot_Provider,Q.Revision(Server),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"import real AML");
      Controller.Handle(Server,Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Current(Server)=Q.Complete and Q.Device_Completion(Server).Stage=ACPI_Bootstrap.Devices_Completed,"startup observes initially usable devices");
   end;
   Metadata.Advance(Server,Accepted);Check(Accepted,"same private Core anchor");
   D.Start(Domain,10,1000,True,Accepted);Check(Accepted,"startup delay owner");
   Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Runtime_Entered,"transfer same Core");
   Windows.Begin_Event(Server,100,Outcome);Check(Outcome=R.Status.Event_Opened,"finite refresh window");
   Lids.Bind(LC,Server,1000,LS);Powers.Bind(PC,Server,1000,PS);
   Check(LS.Outcome=Lids.Controller.Found and PS.Outcome=Powers.Controller.Found,"bind both actual families");
   Lids.Prepare(LC,Server,Budget,LR);Powers.Prepare(PC,Server,Budget,PR);
   Check(LR.Status=Lids.Cached.Collected and PR.Status=Powers.Cached.Collected,"explicit collection phase");
   Check(LR.Charged<=Budget and PR.Charged<=Budget,"status and member share each accepted budget");
   Check(LR.Charged=LR.Device_Status.Charged+LR.Query.Charged and PR.Charged=PR.Device_Status.Charged+PR.Query.Charged,"total reports include status and member charges");
   if Mode in "ready" | "default" then
      Check(Lid_Calls=1 and Power_Calls=1,"one member query after usable status");
      Check(LR.Query.State=L.Open and PR.Query.State=P.Online,"real method values collected");
      if Mode="default" then Check(Status_Calls=0 and LR.Device_Status.Defaulted and PR.Device_Status.Defaulted,"missing STA preserves standard default");
      else Check(Status_Calls=2 and LR.Device_Status.Present and PR.Device_Status.Present,"fresh status executed for each cache");end if;
   else
      Check(Lid_Calls=0 and Power_Calls=0,"unusable or failed status suppresses LID and PSR");
      Check(LR.Query.State=L.Unknown and PR.Query.State=P.Unknown,"no invented closed/offline sample");
      Check(Status_Calls=(if Mode="zero" then 0 else 2),"bounded status evaluation count");
   end if;
   Windows.End_Event(Server,True,Outcome);Check(Outcome=R.Status.Event_Completed,"all retained roots released before close");
   Lids.Publish(LC,Server,(True,20),True,LR);Powers.Publish(PC,Server,(True,20),True,PR);
   Check(LR.Status=Lids.Cached.Updated and PR.Status=Powers.Cached.Updated,"publish copied facts after evaluation");
   declare
      LV : constant Lids.Samples.View:=Lids.Read(LC,Server,(True,21),100);
      PV : constant Powers.Samples.View:=Powers.Read(PC,Server,(True,21),100);
   begin
      if Mode in "ready" | "default" then Check(LV.Status=Lids.Samples.Available and PV.Status=Powers.Samples.Available,"usable state publishes available sample");
      elsif Mode="absent" then Check(LV.Status=Lids.Samples.Missing and PV.Status=Powers.Samples.Missing,"known absence is distinct from query failure");
      else Check(LV.Status=Lids.Samples.Failure and PV.Status=Powers.Samples.Failure,"disabled broken malformed and zero-budget observations publish failure");end if;
   end;
   Check(Enter_Calls=1 and Begin_Calls=1 and End_Calls=1 and Claim_Calls=1,"one runtime owner and window");
   R.Request_Stop(Outcome);Check(Close_Calls=1 and Stop_Calls=1,"terminal owner close once");
   Q.Release_Storage(Server,Released);Check(Released,"release hosted test storage");
   Ada.Text_IO.Put_Line("Status cache "&Mode&" checks"&Checks'Image);
end Cache_Tests;
