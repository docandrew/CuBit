pragma Ada_2022;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
with Interfaces;with AML_Objects;with Firmware_Tables;with ACPI_Hosted_Heap;
with ACPI_Service;with ACPI_Requests;with ACPI_Bootstrap;with ACPI_One_Service;
with ACPI_Requests.With_Driver;with ACPI_Device_Initialization;with ACPI_Device_Policy;
with ACPI_Requests.With_Notification_Routing;with ACPI_Native_Button_Discovery;
with ACPI_Native_Power_Buttons;with ACPI_Native_Diagnostic_Output;with ACPI_Fallible_Heap;with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Binding_Types;with CuBit.Messages;with CuBit.Handles.IPC;with CuBit.Power_Event_Exchange;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Native_Button_Tests is
   package C renames ACPI_Service;package Q renames ACPI_Requests;package IO renames Ada.Streams.Stream_IO;
   package N renames ACPI_Native_Button_Discovery;package W renames ACPI_Native_Power_Buttons;
   package H renames ACPI_Fallible_Heap;package M renames CuBit.Messages;
   package E renames CuBit.Power_Event_Exchange;package IPC renames CuBit.Handles.IPC;
   use type Q.Outcome,Q.Phase,ACPI_Bootstrap.Device_Phase,Ada.Streams.Stream_Element_Offset;
   use type N.Outcome,Interfaces.Unsigned_64;
   Checks:Natural:=0;Mode:constant String:=Ada.Command_Line.Argument(2);
   procedure Check(Good:Boolean;Why:String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
   procedure No_Regions(Service:aliased in out C.State;Budget:Natural;
      Charged:out Natural;Outcome:out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced(Service,Budget);
   begin Charged:=0;Outcome:=ACPI_Device_Policy.No_Regions_Required;end No_Regions;
   package Driver is new ACPI_Device_Initialization(C,No_Regions);
   package Startup is new Q.With_Driver(Driver);
   procedure Route_All(Server:Q.State;Routed:out Boolean) is
   begin N.Route(Server,Routed);end Route_All;
   package Routing is new Q.With_Notification_Routing(Route_All);
   Server:Q.State(1,4096,4096,1,0,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
   Work:Driver.Session;Reply:Q.Response;Accepted,Released,Did_Work:Boolean;File:IO.File_Type;
   Result,Again:N.Outcome;Response:M.Message;Handled,Now:Boolean;Baseline,Before_Free:Natural;
   procedure Watch(Seen:Interfaces.Unsigned_64;Expected:E.Watch_Status;Deferred:Boolean:=False) is
      Request:M.Message:=M.NULL_MESSAGE;
   begin
      Request.tag.label:=E.Button_Watch;Request.tag.length:=E.Watch_Request_Words;Request.authorityTag:=E.Authority_Tag;
      Request.words(E.Request_Word):=16#FACE_CAFE_DEAD_BEEF#;Request.words(E.Seen_Word):=Seen;
      M.Current_Reply_Authority:=True;W.Dispatch(Request,True,Response,Handled,Now);
      Check(Handled and (Now/=Deferred),"actual singleton reply disposition");
      if Now then
         Check(M.Current_Reply_Authority,"immediate watch retained current authority");M.Current_Reply_Authority:=False;
         Check(Response.words(E.Watch_Status_Word)=Interfaces.Unsigned_64(E.Watch_Status'Enum_Rep(Expected)),"exact watch status");
      else Check(not M.Current_Reply_Authority,"saved authority consumed current reply");end if;
   end Watch;
begin
   M.Reset(M.Default_Case);IPC.Reset;
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);
   Check(Reply.Status=Q.OK,"notification capture configured");
   Q.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Start_Snapshot,Data=>[Q.Revision(Server),1,0,0],others=><>),Reply);
   Check(Reply.Status=Q.OK,"snapshot start");
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size:constant Natural:=Natural(IO.Size(File));Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));Last:Ada.Streams.Stream_Element_Offset;
      Table:Firmware_Tables.Bytes(1..Size);
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"complete table");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      Q.Import_Block(Server,Q.Snapshot_Provider,Q.Revision(Server),1,C.DSDT,Table,Reply);Check(Reply.Status=Q.OK,"actual AML import");
      Startup.Handle(Server,Work,Q.Snapshot_Provider,(Label=>Q.Finish_Snapshot,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and Q.Current(Server)=Q.Complete and Q.Device_Completion(Server).Stage=ACPI_Bootstrap.Devices_Completed,"startup complete");
   end;

   ACPI_One_Service.Attach(Server,Accepted);Check(Accepted,"actual local runtime attached to private Core");
   if Mode="not-ready" then ACPI_One_Service.Owner_Allowed:=False;
   elsif Mode="clock-fail" then ACPI_Backend_Kernel_Handles.Clock_Available:=False;
   elsif Mode="clock-overflow" then ACPI_Backend_Kernel_Handles.Clock:=ACPI_Backend_Binding_Types.Tick'Last;
   elsif Mode="alloc-fail" then H.Fail_Next:=True;
   elsif Mode="close-fail" then ACPI_One_Service.Close_Allowed:=False;
   elsif Mode="retired-before" then N.Begin_Stop;W.Begin_Stop;end if;
   Baseline:=H.Reserves;Before_Free:=H.Releases;
   N.Start(Server,Mode="fixed",Result);
   Check(not Q.Retained_Results_Pending(Server),"native discovery retains no AML result");
   Check(ACPI_Native_Diagnostic_Output.Unsafe_Lines=(if Mode in "notify"|"close-fail" then 1 else 0),
     "pending notification or unconfirmed close cannot open diagnostic channel");
   if Mode="fixed" then
      Check(Result=N.Fixed_Selected and H.Reserves=Baseline,"fixed source avoids AML inventory allocation");
      W.Control_Method_Press;Watch(0,E.Watch_Unavailable,True);
      Check(IPC.Answer_Calls=0,"unselected method cannot synthesize fixed press");
      W.Fixed_Press;W.Advance(True,Did_Work);
      Check(Did_Work and IPC.Last_Answer.words(E.Source_Word)=Interfaces.Unsigned_64(E.Press_Source'Enum_Rep(E.Fixed_Event)),"selected fixed provenance only");
   elsif Mode in "not-ready"|"clock-fail"|"clock-overflow"|"alloc-fail"|"retired-before" then
      Check(Result=(if Mode="not-ready" then N.Not_Ready elsif Mode="alloc-fail" then N.Allocation_Refused
        elsif Mode="retired-before" then N.Stopping else N.Clock_Unavailable),"typed early refusal");
      Check(H.Reserves=Baseline+(if Mode="alloc-fail" then 1 else 0),"only allocation failure attempted reserve");
      Watch(0,(if Mode="retired-before" then E.Stopping else E.Watch_Unavailable));
   elsif Mode="no-match" then
      Check(Result=N.Source_Unavailable and N.Quiet,"no matching source remains safely unavailable");Watch(0,E.Watch_Unavailable);
   elsif Mode="close-fail" then
      Check(Result=N.Discovery_Failed and not N.Quiet,"unconfirmed close retained");Watch(0,E.Watch_Unavailable);
   else
      Check(Result=N.Control_Method_Selected and N.Quiet and H.Reserves=Baseline+1 and H.Last_Bytes>0,"dynamic inventory selected after confirmed close");
      W.Fixed_Press;
      if Mode="notify" then
         Check(Q.Notifications_Pending(Server),"actual discovery AML produced notifications");
         for I in 1..3 loop
            Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Claim_Notification,Data=>[Q.Revision(Server),0,0,0],others=><>),Reply);
            Check(Reply.Status=Q.OK,"native source routed actual private receipt");
            declare ID:constant Q.Revision_Number:=Reply.Data(1);begin
               N.Route(Server,Accepted);Check(Accepted,"repeat native route accepted");
               Routing.Handle(Server,Q.Snapshot_Provider,(Label=>Q.Acknowledge_Notification,Data=>[Q.Revision(Server),ID,0,0],others=><>),Reply);
               Check(Reply.Status=Q.OK,"actual common fanout ACK");
            end;
         end loop;
         Watch(0,E.Pressed);Check(Response.words(E.Count_Word)=1 and Response.words(E.Source_Word)=
           Interfaces.Unsigned_64(E.Press_Source'Enum_Rep(E.Control_Method)),"one method press, not wake or unselected fixed event");
         Watch(1,E.Watch_Unavailable,True);
      else Watch(0,E.Watch_Unavailable,True);end if;
   end if;
   N.Start(Server,False,Again);Check(Again=(if Mode="retired-before" then N.Stopping else N.Already_Attempted),"native source cannot restart/reallocate");
   N.Close(Released);Check(Released=(Mode="retired-before"),"release only after source stop, including stop before startup");
   N.Begin_Stop;W.Begin_Stop;W.Advance(True,Did_Work);N.Close(Released);
   if Mode="close-fail" then
      Check(not Released and H.Releases=Before_Free and not N.Quiet,"unknown window never freed");
   else
      Check(Released and N.Quiet and W.Quiet and IPC.Live_Count=0,"source and saved watch drained before release");
      Check(H.Releases=Before_Free+(if Mode in "fixed"|"not-ready"|"clock-fail"|"clock-overflow"|"alloc-fail"|"retired-before" then 0 else 1),"exact inventory release ownership");
   end if;
   N.Start(Server,False,Again);Check(Again=N.Stopping,"closed source cannot resurrect");
   Ada.Text_IO.Put_Line("Native button "&Mode&" checks"&Checks'Image);
end Native_Button_Tests;
