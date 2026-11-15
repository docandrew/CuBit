pragma Ada_2022;
with ACPI_Hosted_Heap;
with ACPI_Snapshot_Startup;with ACPI_Snapshot_Startup.With_Driver;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
with Interfaces;with AML_Execute;with Firmware_Tables;
with ACPI_Service;with ACPI_Requests;
with ACPI_Device_Initialization;with ACPI_Device_Policy;with ACPI_One_Service;
with ACPI_Native_Event_Domain;with ACPI_Native_Server;with ACPI_Native_Log;
with ACPI_Native_EC_Discovery;
with ACPI_SCI_Events;with ACPI_SCI_Registers;with ACPI_SCI_Quiesce;
with ACPI_Requests.With_Local_Maintenance;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Domain_Tests is
   package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;
   package D renames ACPI_Native_Event_Domain;package Log renames ACPI_Native_Log;
   use type Interfaces.Unsigned_64;
   use type L.Outcome,L.State_Access,L.R.S.Table_ID;
   use type Q.Outcome,One.Runtime.Status.Outcome;
   use type D.Start_Outcome,D.Drain_Outcome,ACPI_Native_Server.Owner_Outcome,ACPI_SCI_Events.Disposition;
   use type ACPI_Native_EC_Discovery.Outcome;
   use type C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
   Mode:constant String:=Ada.Command_Line.Argument(2);Checks:Natural:=0;
   procedure Check(Good:Boolean;Why:String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
   procedure No_Regions(Service:aliased in out C.State;Budget:Natural;Charged:out Natural;Outcome:out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced(Service,Budget);
   begin Charged:=0;Outcome:=ACPI_Device_Policy.No_Regions_Required;end No_Regions;
   package Driver is new ACPI_Device_Initialization(C,No_Regions);
   package Metadata is new Q.With_Metadata_Continuation(One.Bind);
   Owner:L.State_Access;
   Work:Driver.Session;Reply:Q.Response;Accepted:Boolean;
   Runtime_Result:One.Runtime.Status.Outcome;Owner_Result:ACPI_Native_Server.Owner_Outcome;
   Start_Result:D.Start_Outcome;Drain_Result:D.Drain_Outcome;Disposition:ACPI_SCI_Events.Disposition;
   package IO renames Ada.Streams.Stream_IO;
   function Read_Table return Firmware_Tables.Bytes is
      File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Final:Ada.Streams.Stream_Element_Offset;
   begin IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Final);IO.Close(File);
      return Data:Firmware_Tables.Bytes(1..Natural(Final)) do
         for I in Data'Range loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      end return;
   end Read_Table;
   Table:constant Firmware_Tables.Bytes:=Read_Table;
   procedure Info(Item:out L.Inventory;Status:out L.R.Copy_Status) is
   begin Item:=(True,1,L.Count(Table'Length),L.Count(Table'Length));Status:=L.R.Copy_OK;end Info;
   procedure Describe(ID:Positive;Item:out L.Description;Status:out L.R.Copy_Status) is
   begin Check(ID=1,"single captured fixture");Item:=("DSDT",Table(9),L.Count(Table'Length));Status:=L.R.Copy_OK;end Describe;
   procedure Copy(ID:L.R.S.Table_ID;Offset:Natural;Into:out L.R.F.Bytes;Copied:out L.R.Byte_Count;Status:out L.R.Copy_Status) is
   begin Check(ID=1,"exact table identity");for I in Into'Range loop Into(I):=Table(Table'First+Offset+I-Into'First);end loop;
      Copied:=L.R.Byte_Count(Into'Length);Status:=L.R.Copy_OK;
   end Copy;
   procedure Clock(Now:out L.R.Tick;Available:out Boolean) is
   begin Now:=L.R.Tick(One.Now);Available:=True;end Clock;
   package Loader is new L.With_Driver(Driver,Info,Describe,4096,Copy,Clock);
   Context:Loader.Session;Loaded:L.Report;
   procedure Run_To_Idle is
   begin
      for Step in 1..256 loop
         D.Advance(Owner.all,Owner_Result);Check(Owner_Result/=ACPI_Native_Server.Owner_Refused,"event owner refused "&D.Observe.Why'Image);
         exit when Owner_Result=ACPI_Native_Server.Owner_Idle;
      end loop;
      D.Advance(Owner.all,Owner_Result);Check(Owner_Result=ACPI_Native_Server.Owner_Idle,"bounded domain reaches idle");
   end Run_To_Idle;
   procedure Drain_Only is
   begin
      for Step in 1..D.Shutdown_Turns loop
         D.Drain(Owner.all,Drain_Result);Check(Drain_Result/=D.Retained,"drain unexpectedly retained");exit when Drain_Result=D.Drained;
      end loop;
      Check(Drain_Result=D.Drained,"bounded stop drain completes");
   end Drain_Only;
   procedure Region_Off(Service:aliased in out C.State;Completed:out Boolean) is
      Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;Result:C.Values.Result;
      Args:constant AML_Execute.Arguments:=[0=>3,1=>0,others=>0];
   begin
      Completed:=False;C.Find_Object(Service,C.Namespace.Root,"EC0_",Found,Status);
      if Status/=C.Values.Available or else Found.Status/=C.Namespace.Found then return;end if;
      C.Find_Object(Service,Found.Node,"_REG",Found,Status);
      if Status/=C.Values.Available or else Found.Status/=C.Namespace.Found then return;end if;
      One.Runtime.Invoke_Retained(Service,Found.Node,Args,2,1000,Result,Status);
      Completed:=Status=C.Values.Available and then Result.Status in AML_Execute.No_Return|AML_Execute.Returned;
   end Region_Off;
   Battery_Method : String(1..4):="BAD0";
   procedure Change_Battery(Service:aliased in out C.State;Completed:out Boolean) is
      Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;Result:C.Values.Result;
      Args:constant AML_Execute.Arguments:=[others=>0];
   begin
      Completed:=False;C.Find_Object(Service,C.Namespace.Root,Battery_Method,Found,Status);
      if Status/=C.Values.Available or else Found.Status/=C.Namespace.Found then return;end if;
      One.Runtime.Invoke_Retained(Service,Found.Node,Args,0,1000,Result,Status);
      Completed:=Status=C.Values.Available and then Result.Status=AML_Execute.No_Return
        and then C.Retained_Results(Service)=0;
   end Change_Battery;
   package Battery_Change is new Q.With_Local_Maintenance(Change_Battery);
   procedure Refresh_Battery(Method_Name:String;Expected:String;Forbidden:String) is
      Admission:Battery_Change.Controller.Admission;
      use type Battery_Change.Controller.Admission;
   begin
      Log.Clear;Battery_Method:=Method_Name;
      One.Runtime_Access.Begin_Event(Owner.all,One.Now+1000,Runtime_Result);
      Check(Runtime_Result=One.Runtime.Status.Event_Opened,"battery mutation finite window");
      Battery_Change.Advance(Owner.all,Admission,Accepted);
      Check(Admission=Battery_Change.Controller.Admitted and Accepted and Q.Notifications_Pending(Owner.all),"actual battery method emits captured Notify with no retained result");
      One.Runtime_Access.End_Event(Owner.all,True,Runtime_Result);
      Check(Runtime_Result=One.Runtime.Status.Event_Completed,"battery mutation window closes");
      Check(Log.Count=0,"no logger during AML mutation");
      Run_To_Idle;
      Check(Log.Saw(Expected),"battery refresh fact "&Expected);
      Check(not Log.Saw(Forbidden),"battery refresh cannot publish stale capacity "&Forbidden);
      Check(Q.Valid(Owner.all),"valid protocol after failed or recovered sample");
   end Refresh_Battery;
   package Disconnect is new Q.With_Local_Maintenance(Region_Off);
   Region_Admission:Disconnect.Controller.Admission;
   use type Disconnect.Controller.Admission;
begin
   Loader.Load(Context,Work,1000,Owner,Loaded);
   Check(Loaded.Status=L.Loaded and then Owner/=null,"actual snapshot-with-driver completion");
   Check(Q.Notifications_Ready(Owner.all),"native loader requested and enabled notification capture");
   Check(Q.Notifications_Pending(Owner.all)=(Mode="startup"),"startup INI queue witness");
   Metadata.Advance(Owner.all,Accepted);Check(Accepted,"same Core");
   One.Runtime_Access.Enter_Runtime(Owner.all,Runtime_Result);Check(Runtime_Result=One.Runtime.Status.Runtime_Entered,"runtime handoff");
   One.Boot_Ready:=Mode not in "no-ec"|"absent"|"identity-failed";ACPI_SCI_Events.Armed_State:=Mode/="no-sci";
   if Mode="disabled" then
      Q.Handle(Owner.all,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Owner.all),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and not Q.Notifications_Ready(Owner.all),"explicit disabled-policy witness");
   end if;
   D.Start(Owner.all,Start_Result);
   if Mode="disabled" then
      Check(Start_Result=D.Refused and ACPI_SCI_Events.Enables=0 and One.Writes=0,"disabled capture refuses source admission");
   elsif Mode="identity-failed" then
      Check(Start_Result=D.Refused,"failed identity refuses domain activation");
      Check(D.Observe.EC=ACPI_Native_EC_Discovery.Recovery_Required,"failed identity observation never certifies no EC");
      Check(ACPI_SCI_Events.Enables=0 and One.Writes=0 and not Log.Saw("acpi: lid"),"failed identity enables no source and publishes no fresh facts");
   elsif Mode in "no-ec"|"no-sci"|"absent"|"absent-with-backing" then
      Check(Start_Result=(if Mode="no-sci" then D.SCI_Unavailable else D.EC_Unavailable),"explicit unavailable event support");
      if Mode in "absent"|"absent-with-backing" then
         Check(D.Observe.EC=ACPI_Native_EC_Discovery.No_EC,"complete successful namespace scan establishes absent EC independently of early backing");
      elsif Mode="no-ec" then
         Check(D.Observe.EC=ACPI_Native_EC_Discovery.Boot_Unavailable,"identified EC without backing remains unavailable, never absent");
      end if;
      Check(ACPI_SCI_Events.Enables=0 and One.Writes=0,"unavailable source never invented");
      D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);Check(Disposition=ACPI_SCI_Events.Handled and Log.Count=0,"fixed callback only copies");
      Run_To_Idle;Check(Log.Saw("power-button") and not Log.Saw("acpi: lid"),"fixed path remains useful without false fresh family claims");
      D.Begin_Stop;D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;D.Close(Owner.all,Drain_Result);Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"unavailable source closes");
   else
      Check(Start_Result=D.Ready and not Q.Notifications_Pending(Owner.all),"domain admits source after startup routing");
      Run_To_Idle;
      Check(Log.Saw("lid OPEN") and Log.Saw("AC ONLINE") and Log.Saw("remaining=20000 full=40000") and Log.Saw("descriptor=32 gpio=42"),"initial actual four-family facts through concrete owner");
      Log.Clear;
      D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);
      Check(Log.Count=0,"no logger inside fixed callback");Run_To_Idle;Check(Log.Saw("power-button presses= 2"),"copied button count logged outside callback");Log.Clear;
      One.Status_Unknown:=Mode="retained";
      D.GPE_Event(16#6E#,Disposition);Check(Disposition=ACPI_SCI_Events.Handled and Log.Count=0,"EC edge callback contains no logging");
      if Mode="retained" then
         for Step in 1..8 loop D.Advance(Owner.all,Owner_Result);exit when Owner_Result=ACPI_Native_Server.Owner_Refused;end loop;
         Check(Owner_Result=ACPI_Native_Server.Owner_Refused and not D.Diagnostics_Quiet(Owner.all) and Log.Count=0,"unknown command retains owner without diagnostics IPC");
         D.Begin_Stop;D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));D.Close(Owner.all,Drain_Result);
         Check(Drain_Result=D.Retained and not D.Quiet(Owner.all) and Log.Count=0,"SCI disarm cannot erase command/Field ownership");
      else
         if Mode="stop-notify" then
            for Step in 1..8 loop D.Advance(Owner.all,Owner_Result);exit when Q.Notifications_Pending(Owner.all);end loop;
            Check(Q.Notifications_Pending(Owner.all) and Log.Count=0,"pending Notify/fact stop witness");
         else
            Run_To_Idle;
            Check(Log.Saw("query Q58") and Log.Saw("query Q59 UNMAPPED"),"unknown queries logged and consumed without domain failure");
            Check(Log.Saw("lid CLOSED") and Log.Saw("AC OFFLINE") and Log.Saw("remaining=15000 full=40000") and Log.Saw("descriptor=33 gpio=42"),"actual query/Notify refreshed all four families");
         end if;
         if Mode="battery-recovery" then
            Refresh_Battery("BAD0","ACQUISITION_FAILED reason=STATUS_FAILED","remaining=15000");
            Refresh_Battery("GOOD","remaining=12345 full=40000","ACQUISITION_FAILED");
            Refresh_Battery("BAD1","ACQUISITION_FAILED reason=INFORMATION_FAILED","remaining=12345");
            Refresh_Battery("GOD1","remaining=13000 full=40000","ACQUISITION_FAILED");
         end if;
         D.Begin_Stop;ACPI_SCI_Events.Armed_State:=False;D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
         declare Before:constant Natural:=One.Writes;begin
            -- The actual _REG method emits Notify during region disconnect.
            -- This harness invokes it on the same Core; region-registry policy
            -- is separately tested, not replaced by this callback.
            One.Runtime_Access.Begin_Event(Owner.all,One.Now+1000,Runtime_Result);Check(Runtime_Result=One.Runtime.Status.Event_Opened,"disconnect finite window");
            Disconnect.Advance(Owner.all,Region_Admission,Accepted);Check(Region_Admission=Disconnect.Controller.Admitted and Accepted and Q.Notifications_Pending(Owner.all),"actual REG0 emitted shutdown Notify");
            One.Runtime_Access.End_Event(Owner.all,True,Runtime_Result);Check(Runtime_Result=One.Runtime.Status.Event_Completed,"disconnect window closes");
            Drain_Only;D.Close(Owner.all,Drain_Result);Check(Drain_Result=D.Drained and D.Quiet(Owner.all) and One.Writes=Before,"route after disarm/disconnect, then close without query replay");
         end;
      end if;
   end if;
   Check(Q.Valid(Owner.all),"valid final protocol/Core");
   Ada.Text_IO.Put_Line("Native event domain "&Mode&" checks"&Checks'Image);
end Domain_Tests;
