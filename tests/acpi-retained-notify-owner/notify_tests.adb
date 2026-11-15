pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with AML_Objects; with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Notification_Routing;
with ACPI_Requests.With_Retained_Notification;
with ACPI_Device_Initialization; with ACPI_Device_Policy; with Firmware_Tables;
procedure Notify_Tests is
   package C renames ACPI_Service; package Q renames ACPI_Requests;
   package IO renames Ada.Streams.Stream_IO;
   use type C.Context_Key, Q.Outcome, Q.Revision_Number, Q.Phase;
   use type ACPI_Bootstrap.Device_Phase, Ada.Streams.Stream_Element_Offset;
   Checks,Routes : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
   type Consumer is limited record
      Context : C.Context_Key := C.No_Context;
      Last : Q.Receipt_Number := 0;
      Count,Calls : Natural := 0;
      Blocked : Boolean := False;
   end record;
   A,B : Consumer;
   procedure Bind (Service : C.State; Completed : out Boolean) is
   begin
      A.Context:=C.Capture_Context(Service);B.Context:=A.Context;
      Completed:=A.Context/=C.No_Context;
   end Bind;
   package Metadata is new Q.With_Metadata_Continuation (Bind);
   procedure Observe (S : in out Consumer; Service : C.State; ID : Q.Receipt_Number;
      Event : C.Notification_Observation; Accepted : out Boolean) is
   begin
      S.Calls:=S.Calls+1;Accepted:=False;
      Check(C.Valid(Service),"callback receives actual valid Core");
      if S.Context=C.No_Context or else not C.Current_Context(Service,S.Context) then return;end if;
      Check(ID/=0 and Event.Current_Target and Event.Path_Length=1 and Event.Path(1)="DEV0",
        "retained receipt and copied event belong to matching initialized owner");
      if S.Blocked or else ID<S.Last then return;end if;
      if ID=S.Last then Accepted:=True;return;end if;
      Check(Event.Number=128+Q.Receipt_Number(S.Count),"actual INI notifications preserve order");
      S.Count:=S.Count+1;S.Last:=ID;Accepted:=True;
   end Observe;
   package Bridge is new Q.With_Retained_Notification (Consumer,Observe);
   procedure Route_All (Server : Q.State; Routed : out Boolean) is
      First,Second : Boolean;
   begin
      Routes:=Routes+1;Bridge.Route(A,Server,First);Bridge.Route(B,Server,Second);
      Routed:=First and then Second;
   end Route_All;
   package Routing is new Q.With_Notification_Routing (Route_All);
   procedure No_Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced(Service,Budget);
   begin Charged:=0;Outcome:=ACPI_Device_Policy.No_Regions_Required;end No_Regions;
   package Driver is new ACPI_Device_Initialization (C,No_Regions);
   package Controller is new Q.With_Driver (Driver);
   Server,Foreign : Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work,Foreign_Work : Driver.Session;
   Reply : Q.Response; Done,Released : Boolean;
   ID,Revision : Q.Revision_Number; Before_Calls,Before_Routes : Natural;
   procedure Load (Target : in out Q.State; Run : in out Driver.Session; Table : Firmware_Tables.Bytes) is
   begin
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"enable capture");
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"begin snapshot");
      Q.Import_Block(Target,Q.Snapshot_Provider,Q.Revision(Target),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"actual AML import");
      Controller.Handle(Target,Run,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Target),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Current(Target)=Q.Complete and
        Q.Device_Completion(Target).Stage=ACPI_Bootstrap.Devices_Completed,"real device initialization");
   end Load;
   procedure Claim is
   begin Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);end Claim;
   procedure Ack (Receipt : Q.Receipt_Number) is
   begin Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),Receipt,0,0],others=><>),Reply);end Ack;
   procedure Check_Retained (Receipt,Queued : Q.Revision_Number) is
      Observed : Q.Response;
   begin
      Q.Handle(Server,Q.Observer,(Label=>Q.Read_Notification_State,others=><>),Observed);
      Check(Observed.Status=Q.OK and Observed.Data(3)=Receipt and Observed.Data(2)=Queued,"same actual retained receipt and queued count");
   end Check_Retained;
   File : IO.File_Type;
begin
   Bridge.Route(A,Server,Done);Check(Done and A.Calls=0,"no retained receipt means no callback even before startup");
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size : constant Natural:=Natural(IO.Size(File));Raw : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));
      Last : Ada.Streams.Stream_Element_Offset;Table : Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole fixture read");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Load(Server,Work,Table);Load(Foreign,Foreign_Work,Table);
   end;
   Metadata.Advance(Server,Done);Check(Done,"bind consumers to private owning Core");
   Bridge.Route(A,Server,Done);Check(Done and A.Calls=0 and Q.Notifications_Pending(Server),"queued event is not fabricated as a retained receipt");
   B.Blocked:=True;Claim;ID:=Reply.Data(1);
   Check(Reply.Status=Q.Incomplete and ID=1 and A.Count=1 and B.Count=0,"partial fanout retains receipt");
   Check_Retained(ID,2);Revision:=Q.Revision(Server);
   Claim;Check(Reply.Status=Q.Incomplete and Reply.Data(1)=ID and Q.Revision(Server)=Revision and
     A.Count=1 and B.Count=0,"claim retry preserves prior delivery and exact receipt");
   Ack(ID);Check(Reply.Status=Q.Incomplete and Q.Revision(Server)=Revision and A.Count=1 and B.Count=0,
     "blocked consumer prevents ACK without duplicating first observation");Check_Retained(ID,2);
   Before_Routes:=Routes;
   Routing.Handle(Server,Q.Observer,(Label=>Q.Acknowledge_Notification,Data=>[Revision,ID,0,0],others=><>),Reply);
   Check(Reply.Status=Q.Denied and Routes=Before_Routes,"observer cannot reach local fanout through ACK");
   Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Length=>3,Data=>[Revision,ID,0,0],others=><>),Reply);
   Check(Reply.Status=Q.Malformed and Routes=Before_Routes,"malformed ACK never calls fanout");
   Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Revision-1,ID,0,0],others=><>),Reply);
   Check(Reply.Status=Q.Stale and Routes=Before_Routes,"stale ACK never calls fanout");
   Q.Handle(Foreign,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Foreign),0,0,0],others=><>),Reply);
   Check(Reply.Status=Q.OK and Reply.Data(1)=ID,"foreign owner may have same numeric receipt");
   Before_Calls:=A.Calls;Bridge.Route(A,Foreign,Done);
   Check(not Done and A.Calls=Before_Calls+1 and A.Count=1 and A.Last=ID,"consumer sees true foreign Core and refuses cross-owner receipt");
   B.Blocked:=False;Ack(ID);
   Check(Reply.Status=Q.OK and Q.Revision(Server)=Revision+1 and A.Count=1 and B.Count=1,
     "all consumers observed before receipt retirement");Check_Retained(0,2);
   Before_Routes:=Routes;Ack(ID);Check(Reply.Status=Q.OK and Routes=Before_Routes,"duplicate accepted ACK cannot reroute retired receipt");
   for Expected in 2..3 loop
      Claim;ID:=Reply.Data(1);Check(Reply.Status=Q.OK and ID=Q.Receipt_Number(Expected) and
        A.Count=Expected and B.Count=Expected,"both consumers receive next actual notification");
      Check(Q.Notifications_Pending(Server),"retained final receipt still fences pending observations");
      Ack(ID);Check(Reply.Status=Q.OK,"all-consumer ACK succeeds");
   end loop;
   Check(not Q.Notifications_Pending(Server),"pending clears only after final receipt ACK");
   Before_Calls:=A.Calls;Bridge.Route(A,Server,Done);Check(Done and A.Calls=Before_Calls,"empty bridge does not fabricate a callback");
   Before_Routes:=Routes;Claim;Check(Reply.Status=Q.OK and Reply.Data(1)=0 and Routes=Before_Routes,"empty claim does not fan out");
   Q.Release_Storage(Server,Released);Check(Released,"release main test storage");Q.Release_Storage(Foreign,Released);Check(Released,"release foreign test storage");
   Ada.Text_IO.Put_Line("Retained Notify owner checks"&Checks'Image);
end Notify_Tests;
