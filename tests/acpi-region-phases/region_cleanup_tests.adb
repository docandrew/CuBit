pragma Ada_2022;
with ACPI_Hosted_Heap;with ACPI_Service;with ACPI_Device_Initialization;with ACPI_Device_Policy;
with ACPI_Region_Initialization;with ACPI_Bootstrap;with ACPI_Bootstrap.With_Region_Cleanup;
with ACPI_Bootstrap.With_Local_Maintenance;with ACPI_Bootstrap.With_Owner_Cleanup;
with AML_Execute;with AML_Objects;with Ada.Command_Line;with Ada.Text_IO;
with Ada.Streams;with Ada.Streams.Stream_IO;with Firmware_Tables;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Region_Cleanup_Tests is
 package C renames ACPI_Service;package B renames ACPI_Bootstrap;package IO renames Ada.Streams.Stream_IO;
 Mode:constant String:=Ada.Command_Line.Argument(2);Checks,Disconnect_Calls,Maintenance_Calls,Mark:Natural:=0;
 use type C.Values.Value_Handle,C.Install_Status,C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
 use type ACPI_Device_Policy.Region_Outcome,B.Phase,B.Device_Phase;
 procedure Check(Good:Boolean;Why:String) is
 begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
 type Context is null record;Owner:Context;
 function Ready(Unused:Context;Region:C.Node_Descriptor;Data:C.Namespace.Operation_Region_Attributes) return Boolean is
 pragma Unreferenced(Unused,Region,Data);begin return True;end;
 function Permanent(Unused:Context;Region:C.Node_Descriptor) return Boolean is
 pragma Unreferenced(Unused,Region);begin return False;end;
 procedure Invoke(S:aliased in out C.State;Unused:in out Context;Node:C.Namespace.Node_ID;
 Args:AML_Execute.Arguments;Argument_Count:Natural;Budget:Natural;Result:out C.Values.Result;Status:out C.Values.Access_Status) is
 pragma Unreferenced(Unused);
 begin C.Invoke_Retained(S,Node,Args,Argument_Count,Budget,Result,Status);end;
 package Regions is new ACPI_Region_Initialization(C,Context,Ready,Permanent,Invoke);
 use type Regions.Phase;Region_Run:Regions.Session;
 procedure Connect(S:aliased in out C.State;Budget:Natural;Charged:out Natural;Outcome:out ACPI_Device_Policy.Region_Outcome) is
 Accepted:Boolean;begin
 if Mode="stagedpartial" then Regions.Prepare(Region_Run,S,Owner,Accepted);
 else Regions.Connect(Region_Run,S,Owner,Budget,Accepted);end if;Check(Accepted,"single region preparation accepted");
 Charged:=Regions.Observe(Region_Run).Charged;Outcome:=(if Mode="stagedpartial" then ACPI_Device_Policy.Regions_Prepared else Regions.Outcome(Region_Run));end;
 procedure After_Initializer(S:aliased in out C.State;Scope:C.Namespace.Node_ID;Initialization_Completed:Boolean;
 Budget:Natural;Charged:in out Natural;Outcome:in out ACPI_Device_Policy.Region_Outcome) is
 Action:Regions.Scope_Action;use type Regions.Scope_Action;
 begin Charged:=0;Outcome:=ACPI_Device_Policy.Regions_Connected;
 if Scope=C.Namespace.Root then
 Check(Initialization_Completed,"root initializer completed before notice");
 Regions.Connect_Scope(Region_Run,S,Owner,Scope,Budget,Charged,Action);
 Check(Action=Regions.Scope_Completed,"root group connected once before unvisited auxiliary group");
 end if;end;
 procedure Finish_Regions(S:aliased in out C.State;Budget:Natural;Charged:in out Natural;
 Outcome:in out ACPI_Device_Policy.Region_Outcome) is
 pragma Unreferenced(Budget);Finished:Boolean;
 begin Charged:=0;Regions.Finish_Connect(Region_Run,S,Owner,Finished);
 Check(not Finished and Regions.Observe(Region_Run).Stage=Regions.Prepared,"unvisited auxiliary group prevents global finish");
 Outcome:=ACPI_Device_Policy.Region_Initialization_Failed;end;
 package Driver is new ACPI_Device_Initialization(C,Connect,After_Initializer=>After_Initializer,Finish_Regions=>Finish_Regions);
 package Startup is new B.With_Driver(Driver);
 procedure Disconnect(S:aliased in out C.State;Completed:out Boolean) is
 Accepted:Boolean;Found:C.Namespace.Lookup_Result;Node:C.Node_Descriptor;Status:C.Values.Access_Status;Value:C.Values.Result;
 begin Disconnect_Calls:=Disconnect_Calls+1;Regions.Disconnect(Region_Run,S,Owner,10000,Accepted);
 Completed:=Accepted and Regions.Observe(Region_Run).Stage=Regions.Disconnected;
 if Completed then
 C.Find_Object(S,C.Namespace.Root,"READ",Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"read method lookup");
 C.Describe_Node(S,Found.Node,Node,Status);Check(Status=C.Values.Available,"read descriptor");
 C.Evaluate_Named(S,Node,1000,Value,Status);Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned,"read mark scalar");
 Mark:=Natural(Value.Number);Check(C.Retained_Results(S)=0,"no result leaks");end if;end;
 Held:C.Values.Value_Handle:=C.Values.No_Value;
 procedure Release_Held(S:aliased in out C.State;Completed:out Boolean) is
 Status:C.Values.Access_Status;
 begin C.Release_Result(S,Held,Status);Completed:=Status=C.Values.Available and Held=C.Values.No_Value;end;
 package Owner_Cleanup is new B.With_Owner_Cleanup(Release_Held);
 procedure Maintenance(S:aliased in out C.State;Completed:out Boolean) is
 Found:C.Namespace.Lookup_Result;Node:C.Node_Descriptor;Status:C.Values.Access_Status;Value:C.Values.Result;
 begin Maintenance_Calls:=Maintenance_Calls+1;Completed:=True;
 if Mode="retained" then
 C.Find_Object(S,C.Namespace.Root,"BUF0",Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"buffer lookup");
 C.Describe_Node(S,Found.Node,Node,Status);Check(Status=C.Values.Available,"buffer descriptor");
 C.Evaluate_Named(S,Node,1000,Value,Status);Check(Status=C.Values.Available and Value.Status=AML_Execute.Object_Returned and Value.Handle/=C.Values.No_Value,"actual compound result retained");Held:=Value.Handle;end if;end;
 package Cleanup is new B.With_Region_Cleanup(Disconnect);
 package Runtime is new B.With_Local_Maintenance(Maintenance);
 use type Cleanup.Admission,Runtime.Admission;
 Session:B.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,
 Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
 Run:Driver.Session;Accepted,Completed:Boolean;Installed:C.Install_Status;Allowed:Cleanup.Admission;Runtime_Allowed:Runtime.Admission;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;
 Data:Firmware_Tables.Bytes(1..4096);
 Item:C.Notification_Observation;Present:Boolean;Status:C.Values.Access_Status;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);
 for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
 B.Start(Session,1,Accepted);Check(Accepted,"bootstrap started");
 B.Set_Notification_Interest(Session,True,Status);Check(Status=C.Values.Available,"notification capture configured");
 B.Import_Table(Session,1,C.DSDT,Data(1..Natural(Last)),Installed);Check(Installed=C.Installed,"table imported");
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Not_Ready and not Completed and Disconnect_Calls=0,"early cleanup blocked");
 Startup.Finish(Session,Run,10000);Check(B.Current(Session)=B.Complete,"table startup complete independently of device status");
 if Mode="failedreg" then
 Check(B.Device_Completion(Session).Region_Status=ACPI_Device_Policy.Region_Initialization_Failed,"failed connection remains failed");
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Admitted and not Completed and Disconnect_Calls=1 and Regions.Observe(Region_Run).Stage=Regions.Failed,"owning failed session refuses disconnect without replay");
 else
 if Mode="stagedpartial" then
 Check(B.Device_Completion(Session).Stage=B.Devices_Failed and Regions.Observe(Region_Run).Stage=Regions.Prepared,"failed device finish preserves known prepared registration successes");
 else
 Check(B.Device_Completion(Session).Region_Status=ACPI_Device_Policy.Regions_Connected and Regions.Observe(Region_Run).Stage=Regions.Connected,"actual regions connected");
 if Mode="fatal" then Check(B.Device_Completion(Session).Stage=B.Devices_Failed,"fatal after connect");
 elsif Mode="error" then Check(B.Device_Completion(Session).Stage=B.Devices_Completed_With_Errors,"initializer error after connect");
 else Check(B.Device_Completion(Session).Stage=B.Devices_Completed,"device startup completed");end if;end if;
 Runtime.Advance(Session,Runtime_Allowed,Completed);
 if Mode in "fatal"|"error"|"stagedpartial" then Check(Runtime_Allowed=Runtime.Not_Ready and not Completed and Maintenance_Calls=0,"runtime admission stays strict");end if;
 if Mode="retained" then
 Check(Runtime_Allowed=Runtime.Admitted and Completed and Maintenance_Calls=1,"runtime owner created result");
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Results_Pending and not Completed and Disconnect_Calls=0,"actual retained result blocks cleanup");
 Owner_Cleanup.Advance(Session,Completed);Check(Completed and Held=C.Values.No_Value,"attributable result released without AML");end if;
 if Mode="notify" then
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Notifications_Pending and not Completed and Disconnect_Calls=0,"queued real AML notification blocks cleanup");
 B.Take_Notification(Session,Item,Present,Status);Check(Present and Status=C.Values.Available,"notification drained explicitly");end if;
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Admitted and Completed and Disconnect_Calls=1 and Mark=17,"actual REG zero runs after successful REG one");
 Cleanup.Advance(Session,Allowed,Completed);Check(Allowed=Cleanup.Admitted and not Completed and Disconnect_Calls=2 and Mark=17,"disconnect cannot replay AML");
 end if;
 B.Release_Storage(Session,Accepted);Check(Accepted,"bootstrap storage released");
 Ada.Text_IO.Put_Line("Real region cleanup checks"&Checks'Image);
end Region_Cleanup_Tests;
