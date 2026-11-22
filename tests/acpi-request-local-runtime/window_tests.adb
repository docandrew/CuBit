pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects;
with ACPI_Service; with ACPI_Requests; with ACPI_Bootstrap;
with ACPI_Requests.With_Driver; with ACPI_Requests.With_Local_Runtime;
with ACPI_Device_Initialization; with ACPI_Device_Policy;
with ACPI_Local_Runtime; with ACPI_Delay_Domains; with Firmware_Tables;
procedure Window_Tests is
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
   begin Clock:=Clock+1;D.Begin_Event(Domain,Clock,Deadline,True,Accepted);end Begin_Delays;
   procedure End_Delays (Accepted : out Boolean) is
   begin Clock:=Clock+1;D.End_Event(Domain,Clock,True,Accepted);end End_Delays;
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
   procedure Blocked_Regions (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced(Service,Budget);
   begin Charged:=0;Outcome:=ACPI_Device_Policy.Regions_Unavailable;end Blocked_Regions;
   package Driver is new ACPI_Device_Initialization (C,No_Regions);
   package Blocked_Driver is new ACPI_Device_Initialization (C,Blocked_Regions);
   package Controller is new Q.With_Driver (Driver);
   package Blocked_Controller is new Q.With_Driver (Blocked_Driver);
   Server,Other,Blocked : Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work,Other_Work : Driver.Session; Blocked_Work : Blocked_Driver.Session;
   Reply : Q.Response; Accepted,Released : Boolean; Outcome : R.Outcome;
   procedure Start (Target : in out Q.State) is
   begin
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK,"enable real notification capture");
      Q.Handle(Target,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Target),1,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK,"start request snapshot");
   end Start;
   File : IO.File_Type;
begin
   Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Rejected and Enter_Calls=0,"fresh request cannot claim runtime");
   Windows.Begin_Event(Server,100,Outcome);Check(Outcome=R.Status.Rejected and Begin_Calls=0,"fresh request cannot open event");
   Windows.End_Event(Server,False,Outcome);Check(Outcome=R.Status.Rejected and End_Calls=1,"end delegates to original-Core policy without readiness gate");
   Start(Server);Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Rejected and Enter_Calls=0,"receiving request cannot claim runtime");
   Start(Other);Start(Blocked);
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size : constant Natural:=Natural(IO.Size(File));Raw : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));
      Last : Ada.Streams.Stream_Element_Offset; Table : Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole fixture");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Q.Import_Block(Server,Q.Snapshot_Provider,Q.Revision(Server),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"main import");
      Controller.Handle(Server,Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Device_Completion(Server).Stage=ACPI_Bootstrap.Devices_Completed,"device startup complete");
      Q.Import_Block(Other,Q.Snapshot_Provider,Q.Revision(Other),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"foreign import");
      Controller.Handle(Other,Other_Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Other),0,0,0],others=><>),Reply);Check(Reply.Status=Q.OK,"foreign startup");
      Q.Import_Block(Blocked,Q.Snapshot_Provider,Q.Revision(Blocked),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"blocked import");
      Blocked_Controller.Handle(Blocked,Blocked_Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Blocked),0,0,0],others=><>),Reply);
      Check(Q.Device_Completion(Blocked).Stage/=ACPI_Bootstrap.Devices_Completed,"region readiness unavailable");
   end;
   Windows.Enter_Runtime(Blocked,Outcome);Check(Outcome=R.Status.Rejected and Enter_Calls=0,"incomplete device barrier excludes claim callback");
   Windows.Begin_Event(Blocked,100,Outcome);Check(Outcome=R.Status.Rejected and Begin_Calls=0,"incomplete barrier excludes begin callback");
   Metadata.Advance(Server,Accepted);Check(Accepted,"capture exact owning Core key");
   D.Start(Domain,10,1000,True,Accepted);Check(Accepted,"startup delay domain");
   Windows.Enter_Runtime(Other,Outcome);Check(Outcome=R.Status.Rejected and Enter_Calls=1 and Claim_Calls=0,"complete foreign owner cannot transfer this runtime");
   declare
      Revision : constant Q.Revision_Number:=Q.Revision(Server);
      Pending : constant Boolean:=Q.Notifications_Pending(Server);
      Phase : constant Q.Phase:=Q.Current(Server);
   begin
      Check(Pending,"actual INI notifications remain pending");
      Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Runtime_Entered and Claim_Calls=1,"same private Core transfers once");
      Windows.Begin_Event(Other,100,Outcome);Check(Outcome=R.Status.Rejected and R.Current=R.Status.Idle,"foreign event refuses without altering owner");
      Windows.Begin_Event(Server,0,Outcome);Check(Outcome=R.Status.Rejected and R.Current=R.Status.Idle,"invalid finite deadline refuses");
      Windows.Begin_Event(Server,100,Outcome);Check(Outcome=R.Status.Event_Opened,"same private Core opens event");
      Windows.End_Event(Other,True,Outcome);Check(Outcome=R.Status.Rejected and R.Current=R.Status.Active,"foreign end cannot close active event");
      Windows.End_Event(Server,True,Outcome);Check(Outcome=R.Status.Event_Completed,"original owner completes event");
      Check(Q.Valid(Server) and Q.Revision(Server)=Revision and Q.Current(Server)=Phase and
        Q.Notifications_Pending(Server)=Pending and not Q.Table_Open(Server),"lifecycle bridge preserves request state and receipts");
      Windows.Begin_Event(Server,100,Outcome);Check(Outcome=R.Status.Event_Opened,"next event uses same owner");
      Windows.End_Event(Server,False,Outcome);Check(Outcome=R.Status.Evaluation_Failed and Close_Calls=1 and Stop_Calls=1,"failed event stops exactly once");
      Windows.Begin_Event(Server,100,Outcome);Check(Outcome=R.Status.Rejected,"stopped owner never reopens");
      Windows.Enter_Runtime(Server,Outcome);Check(Outcome=R.Status.Rejected and Claim_Calls=1,"stopped owner never transfers again");
   end;
   Q.Release_Storage(Server,Released);Check(Released,"release test-only main storage");
   Q.Release_Storage(Other,Released);Check(Released,"release foreign test storage");
   Q.Release_Storage(Blocked,Released);Check(Released,"release blocked test storage");
   Ada.Text_IO.Put_Line("Request runtime windows checks"&Checks'Image);
end Window_Tests;
