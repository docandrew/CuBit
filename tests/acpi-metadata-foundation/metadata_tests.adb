pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
with AML_Objects;with ACPI_Service;with ACPI_Requests;
with ACPI_Requests.With_Driver;with ACPI_Device_Initialization;
with ACPI_Device_Policy;with ACPI_Bootstrap;with Firmware_Tables;
procedure Metadata_Tests is
 package C renames ACPI_Service;package Q renames ACPI_Requests;
 package IO renames Ada.Streams.Stream_IO;
 use type C.Context_Key,C.Values.Access_Status,C.Stage_Status,C.Freeze_Status;
 use type C.Values.Module_Outcome,Q.Outcome,Q.Phase,Q.Revision_Number;
 use type ACPI_Bootstrap.Device_Phase,Ada.Streams.Stream_Element_Offset;
 Checks,Calls:Natural:=0;Accept_Callback:Boolean:=True;
 Seen,Foreign:C.Context_Key:=C.No_Context;
 procedure Check(Good:Boolean;Why:String) is
 begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
 procedure Inspect(Service:C.State;Completed:out Boolean) is
 begin
  Calls:=Calls+1;
  Check(C.Valid(Service) and then C.Retained_Results(Service)=0,"metadata sees valid owning Core without retained AML results");
  Check(C.Capture_Context(Service)/=C.No_Context and then C.Current_Context(Service,C.Capture_Context(Service)),"initialized context has live opaque key");
  if Seen=C.No_Context then Seen:=C.Capture_Context(Service);
  else Check(C.Current_Context(Service,Seen),"same Core context remains current across metadata maintenance");end if;
  if Foreign/=C.No_Context then Check(not C.Current_Context(Service,Foreign),"foreign detached context does not match owning Core");end if;
  Completed:=Accept_Callback;
 end Inspect;
 package Metadata is new Q.With_Metadata_Continuation(Inspect);
 procedure No_Regions(Service:aliased in out C.State;Budget:Natural;Charged:out Natural;Outcome:out ACPI_Device_Policy.Region_Outcome) is
  pragma Unreferenced(Service,Budget);
 begin Charged:=0;Outcome:=ACPI_Device_Policy.No_Regions_Required;end No_Regions;
 package Driver is new ACPI_Device_Initialization(C,No_Regions);
 package Controller is new Q.With_Driver(Driver);
 Server,Other,Partial:Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
 Run,Other_Run:Driver.Session;
 Direct:C.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
 Accepted,Released:Boolean;Reply:Q.Response;
 procedure Meta(Target:Q.State;Expected:Boolean;Expected_Calls:Natural) is
  Revision:constant Q.Revision_Number:=Q.Revision(Target);
  Phase:constant Q.Phase:=Q.Current(Target);
  Open:constant Boolean:=Q.Table_Open(Target);
  Used:constant Natural:=Q.Received(Target);
  Pending:constant Boolean:=Q.Notifications_Pending(Target);
 begin
  Accepted:=True;Metadata.Advance(Target,Accepted);
  Check(Accepted=Expected and then Calls=Expected_Calls,"metadata eligibility and callback outcome");
  Check(Q.Revision(Target)=Revision and then Q.Current(Target)=Phase and then Q.Table_Open(Target)=Open and then Q.Received(Target)=Used and then Q.Notifications_Pending(Target)=Pending,"metadata does not mutate protocol phase/revision/import/notification ownership");
 end Meta;
 procedure Start(Target:in out Q.State) is
 begin
  Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);
  Check(Reply.Status=Q.OK,"request capture before initialization");
  Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);
  Check(Reply.Status=Q.OK and then Q.Current(Target)=Q.Receiving,"start actual snapshot");
 end Start;
 procedure Finish(Target:in out Q.State;Work:in out Driver.Session;Table:Firmware_Tables.Bytes) is
 begin
  Q.Import_Block(Target,Q.Snapshot_Provider,Q.Revision(Target),1,C.DSDT,Table,Reply);
  Check(Reply.Status=Q.OK,"import actual synthetic table");
  Controller.Handle(Target,Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Target),0,0,0],others=><>),Reply);
  Check(Reply.Status=Q.OK and then Q.Current(Target)=Q.Complete and then Q.Device_Completion(Target).Stage=ACPI_Bootstrap.Devices_Completed,"actual device initialization completes");
 end Finish;
 File:IO.File_Type;
begin
 Check(Ada.Command_Line.Argument_Count=1,"fixture argument");
 Check(C.Capture_Context(Direct)=C.No_Context and then not C.Current_Context(Direct,C.No_Context),"fresh Core/default key never grants a current context");
 Check(not Q.Notifications_Pending(Server),"fresh owner has no notification obligation");
 Meta(Server,False,0);Start(Server);Meta(Server,False,0);
 Start(Partial);
 Q.Handle(Partial,Q.Snapshot_Provider,(Label=>Q.Begin_Table,Data=>[Q.Revision(Partial),1,0,64],others=><>),Reply);
 Check(Reply.Status=Q.OK and then Q.Table_Open(Partial),"partial import has open table");Meta(Partial,False,0);
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
 declare
  Size:constant Natural:=Natural(IO.Size(File));Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));Last:Ada.Streams.Stream_Element_Offset;
  Table:Firmware_Tables.Bytes(1..Size);Stage:C.Stage_Status;Frozen:C.Freeze_Status;Execution:C.Execution_Report;Members:C.Namespace.Initialization_Report;Status:C.Values.Access_Status;
  Root_Descriptor:C.Node_Descriptor;
  Root_Status:C.Values.Access_Status;
  Direct_Key,Original:C.Context_Key;Receipt:Q.Receipt_Number;
 begin
  IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole fixture read");
  for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
  C.Stage_Table(Direct,1,C.DSDT,Table,Stage);Check(Stage=C.Staged,"stage direct Core");
  C.Freeze_Catalog(Direct,Frozen);Check(Frozen=C.Catalog_Frozen_OK and then C.Capture_Context(Direct)=C.No_Context,"staging/freeze do not invent initialized context");
  C.Execute_Frozen(Direct,1000,Execution,Status);Check(Status=C.Values.Available and then Execution.Modules.Outcome=C.Values.Modules_Completed,"direct module execution");
  C.Describe_Node(Direct,C.Namespace.Root,Root_Descriptor,Root_Status);
  Check(Root_Status=C.Values.Wrong_Phase,"native startup witness: catalog executed is not metadata Ready");
  Check(Status=C.Values.Available and then Execution.Modules.Outcome=C.Values.Modules_Completed,"metadata refusal cannot overwrite successful module result");
  Direct_Key:=C.Capture_Context(Direct);Check(Direct_Key/=C.No_Context and then C.Current_Context(Direct,Direct_Key),"context available before member metadata");
  Check(not C.Current_Context(Direct,C.No_Context),"default context cannot authorize executed owner");

  C.Initialize_Members(Direct,Members,Status);Check(Status=C.Values.Available and then C.Current_Context(Direct,Direct_Key),"member initialization preserves owner key");
  C.Describe_Node(Direct,C.Namespace.Root,Root_Descriptor,Root_Status);
  Check(Root_Status=C.Values.Available and then C.Current_Binding(Direct,Root_Descriptor),"root metadata only becomes available after member finish");
  Check(C.Current_Context(Direct,Direct_Key),"original early context remains the owner after member finish");
  Finish(Server,Run,Table);Check(Q.Notifications_Pending(Server),"real _INI events require owner progress");
  Meta(Server,True,1);Original:=Seen;Check(not C.Current_Context(Direct,Seen),"request Core key cannot match direct foreign Core");
  Accept_Callback:=False;Meta(Server,False,2);Accept_Callback:=True;
  Start(Other);Finish(Other,Other_Run,Table);Foreign:=Seen;Seen:=C.No_Context;Meta(Other,True,3);
  Check(Seen/=Original,"identical firmware in separate owners has different keys");Foreign:=Seen;Seen:=Original;Meta(Server,True,4);
  for I in 1..3 loop
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
   Receipt:=Reply.Data(1);Check(Reply.Status=Q.OK and then Receipt/=0 and then Q.Notifications_Pending(Server),"claimed notification remains pending");
   if I=3 then Check(Reply.Data(2)=0,"last event moved from queue to retained receipt");Meta(Server,True,5);Check(Q.Notifications_Pending(Server),"empty queue with held receipt still fences publication");end if;
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),Receipt,0,0],others=><>),Reply);
   Check(Reply.Status=Q.OK,"exact receipt acknowledged once");
  end loop;
  Check(not Q.Notifications_Pending(Server),"notification obligation ends only after last ACK");Meta(Server,True,6);
 end;
 Q.Release_Storage(Server,Released);Check(Released,"release main hosted owner");
 Q.Release_Storage(Other,Released);Check(Released,"release foreign hosted owner");
 Q.Release_Storage(Partial,Released);Check(Released,"release incomplete local Core storage only");
 C.Release_Storage(Direct,Released);Check(Released,"release direct Core storage");
 Ada.Text_IO.Put_Line("Metadata/context checks"&Checks'Image);
end Metadata_Tests;
