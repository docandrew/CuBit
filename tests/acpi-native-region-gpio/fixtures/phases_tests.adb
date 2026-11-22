pragma Ada_2022;
with ACPI_Native_GPIO_Observation;with Composition_State;with ACPI_Native_Pre_Device;with ACPI_Native_Delays;with Prep_Model;with Sender_Model;with ACPI_Native_Initializer_History;with Interfaces;with ACPI_Hosted_Heap;with ACPI_Service;with AML_Execute;with AML_Names;
with ACPI_One_Service;with ACPI_Native_Regions;with ACPI_Serving_State;with ACPI_Device_Initialization;with ACPI_Device_Policy;
with Firmware_Tables;with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Phases_Tests is
 package C renames ACPI_Service;package IO renames Ada.Streams.Stream_IO;package P renames ACPI_Device_Policy;
 Mode:constant String:=Ada.Command_Line.Argument(2);
 No_EC_Handler:constant Boolean:=Mode in "ec-unavailable"|"lock-unavailable";
 Early_Refusal:constant Boolean:=Mode in "preparation-refused"|"fields-pending"|"handler-lost-prefix"|"reg-connect-failed"|"reg-connect-fatal";
 Unavailable_EC_Regions:constant:=3;
 use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
 use type P.Region_Outcome;use type Interfaces.Unsigned_32;
 Checks:Natural:=0;Connect_Calls:Natural renames Prep_Model.Connect_Calls;Disconnect_Calls:Natural renames Prep_Model.Disconnect_Calls;
 procedure Check(OK:Boolean;Why:String) is begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end;
 package History renames ACPI_Native_Initializer_History;
 History_Count:Natural:=0;
 procedure Note(Service:C.State;Scope:C.Namespace.Node_ID;Method:C.Node_Descriptor;
 Access_Result:C.Values.Access_Status;Execution:AML_Execute.Execution_Status;Reported_Charge:Natural) is
 begin
 History.Note(Service,Scope,Method,Access_Result,Execution,Reported_Charge);
 History_Count:=History_Count+1;
 Check(History.Observe.Path_Complete and History.Observe.Last_Scope=Scope and History.Observe.Last_Method=C.Values.Position(Method),"copied initializer path/identity matches the actual same-Core invocation");
 Check(History.Observe.Complete and History.Observe.Invocations=Interfaces.Unsigned_32(History_Count),"actual native history adapter records each initializer invocation once");
 end;
 Service:C.State renames Composition_State.Service;
 Foreign:C.State renames Composition_State.Foreign;
 package Native renames ACPI_Native_Pre_Device;
 use type Native.Failure,ACPI_Native_GPIO_Observation.Driver.Outcome;
 package Regions renames ACPI_Native_Regions;
 function Child(Parent:C.Namespace.Node_ID;Part:AML_Names.Segment) return C.Namespace.Node_ID is
 Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;
 begin C.Find_Object(Service,Parent,Part,Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"node lookup");return Found.Node;end;
 function Read_At(Parent:C.Namespace.Node_ID;Part:AML_Names.Segment) return Natural is
 Node:C.Node_Descriptor;Status:C.Values.Access_Status;Value:C.Values.Result;
 begin C.Describe_Node(Service,Child(Parent,Part),Node,Status);Check(Status=C.Values.Available,"scalar descriptor");
 C.Evaluate_Named(Service,Node,1000,Value,Status);Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned,"scalar observation");return Natural(Value.Number);end;
 Bridge:C.Namespace.Node_ID:=C.Namespace.Root;
 procedure Prepare(S:aliased in out C.State;Budget:Natural;Charged:out Natural;Outcome:out P.Region_Outcome) is
 Required,Accepted:Boolean:=False;
 begin
 Regions.Prepare(S,Budget,Charged,Outcome);
 if Mode in "preparation-refused"|"fields-pending" then
 Check(Outcome=P.Regions_Unavailable and Regions.Observe.Attempted and not Regions.Observe.Announced and Connect_Calls=0,"native physical preparation refusal stops before REG or classification");
 Regions.Classify_Prerequisite(S,Bridge,Required,Accepted);
 Check(not Accepted and not Required,"failed preparation cannot classify prerequisites");return;end if;
 Check(Outcome=P.Regions_Prepared and Regions.Observe.Attempted and not Regions.Observe.Announced and Connect_Calls=0,"actual native preparation is metadata-only");
 Regions.Classify_Prerequisite(Foreign,Bridge,Required,Accepted);
 Check(not Accepted and not Required,"native prerequisite classifier refuses a different Core");
 Regions.Classify_Prerequisite(S,Bridge,Required,Accepted);
 Check(Accepted and Required,"actual prepared plan selects initializer ancestor");
 end;
 procedure Evaluator(S:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;Result:out C.Values.Result;Status:out C.Values.Access_Status) is
 begin ACPI_One_Service.Root.Evaluation.Evaluate_Named(S,Item,Budget,Result,Status);
 if Mode="evaluator-overcharge" then Result.Charged:=Budget+1;end if;end;
 use type Interfaces.Unsigned_64;
 Consumer_Calls:Natural:=0;
 procedure Consumers(S:aliased in out C.State;Budget:Natural;Charged:in out Natural;Accepted:in out Boolean) is
 Value:C.Values.Result;Status:C.Values.Access_Status;Args:constant AML_Execute.Arguments:=[others=>0];
 Prefix:constant History.Report:=History.Observe;Spent:Natural;
 begin
 Consumer_Calls:=Consumer_Calls+1;
 Check(History_Count=3 and Read_At(C.Namespace.Root,"PINI")=1 and Read_At(C.Namespace.Root,"RINI")=1 and Read_At(C.Namespace.Root,"SINI")=1,"actual prerequisite prefix before native preparation");
 Check(Read_At(C.Namespace.Root,"DINI")=0 and Read_At(C.Namespace.Root,"CINI")=0 and Read_At(C.Namespace.Root,"EINI")=0,"all consumers still deferred at native owner entry");
 Check(Regions.Observe.Connected and Connect_Calls=(if No_EC_Handler then 0 else 2),"actual scoped REG complete before native owner");
 Native.Prepare(S,Budget,Spent,Accepted);Charged:=Spent;
 Check(Spent<=Budget and C.Retained_Results(S)=0,"actual observer/builder shared fuel and released Core roots");
 Check(Native.Observe.Initializer_Invocations=Prefix.Invocations and Native.Observe.Initializer_Ordered_Digest=Prefix.Ordered_Digest and Native.Observe.Initializer_History_Complete,"actual native owner carries same initializer prefix");
 if not Accepted then return;end if;
 -- This fixture marker means preparation completed, never hardware authority.
 C.Invoke_Retained(S,Child(C.Namespace.Root,"ADMT"),Args,0,Budget-Charged,Value,Status);
 Check(Status=C.Values.Available and Value.Status=AML_Execute.No_Return,"phase-completed marker after native preparation");
 Charged:=Charged+Value.Charged;
 Check(History.Observe.Invocations=Prefix.Invocations and History.Observe.Ordered_Digest=Prefix.Ordered_Digest,"observation/admission adds no automatic initializer");
 end;
 package Driver is new ACPI_Device_Initialization(C,Prepare,Evaluator,After_Initializer=>Regions.After_Ancestor,Finish_Regions=>Regions.Finish,Note_Initializer=>Note,Consumer_Preparation_Enabled=>True,Classify_Prerequisite=>Regions.Classify_Prerequisite,Prepare_Consumers=>Consumers);
 use type Driver.Phase, Driver.Error_Count;
 Work:Driver.Session;Accepted,Released:Boolean;Used:Natural;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;Data:Firmware_Tables.Bytes(1..4096);
 procedure Load(S:aliased in out C.State) is
 Stage:C.Stage_Status;Freeze:C.Freeze_Status;Status:C.Values.Access_Status;Execution:C.Execution_Report;Members:C.Namespace.Initialization_Report;
 begin C.Stage_Table(S,1,C.DSDT,Data(1..Natural(Last)),Stage);Check(Stage=C.Staged,"table staged");C.Freeze_Catalog(S,Freeze);Check(Freeze=C.Catalog_Frozen_OK,"table frozen");C.Execute_Frozen(S,10000,Execution,Status);Check(Status=C.Values.Available,"modules executed");C.Initialize_Members(S,Members,Status);Check(Status=C.Values.Available,"members initialized");end;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;Load(Service);Load(Foreign);
 Bridge:=Child(Child(C.Namespace.Root,"_SB_"),(if Mode="renamed" then "BUS0" else "BRG0"));
 Sender_Model.Reset;Prep_Model.Mode:=1;Sender_Model.Mode:=1;
 if Mode="preparation-refused" then Prep_Model.Prepare_OK:=False;
 elsif Mode="fields-pending" then Prep_Model.Fields_Pending:=True;
 elsif Mode="ec-unavailable" then Prep_Model.EC_Ready:=False;
 elsif Mode="lock-unavailable" then Prep_Model.Lock_Ready:=False;
 elsif Mode="handler-lost-prefix" then Prep_Model.Lose_Handler_After_Prefix:=True;
 elsif Mode="confirmed" then Sender_Model.Mode:=33;
 elsif Mode="unknown" then Sender_Model.Mode:=34;
 elsif Mode="grant-pending" then Sender_Model.Confirmed:=False;
 elsif Mode="expired" then Prep_Model.Mode:=18;
 elsif Mode="known-refusal" then Sender_Model.Mode:=6;
 elsif Mode="p2sb" then Sender_Model.Mode:=2;
 end if;
 ACPI_Native_Delays.Test_Start;
 Native.Configure(77,10000,Accepted);Check(Accepted,"native owner configured with original startup deadline");
 Driver.Begin_Initialization(Work,Service,10000,Accepted);Check(Accepted and Driver.Observe(Work).Stage=Driver.Awaiting_Regions and Read_At(C.Namespace.Root,"PINI")=0,"begin is metadata-only");
 Driver.Complete_Initialization(Work,Service,Accepted);Check(Accepted,"one completion attempt");
 if Early_Refusal then
 Check(Driver.Observe(Work).Stage in Driver.Incomplete|Driver.Failed and not Driver.Observe(Work).Consumers_Prepared and Consumer_Calls=0 and Sender_Model.Calls=0,"native prerequisite refusal cannot enter GPIO or consumer phase");
 Check(History_Count=(if Mode in "preparation-refused"|"fields-pending" then 0 else 3),"native refusal retains exact initializer prefix");
 Check(Read_At(C.Namespace.Root,"DINI")=0 and Read_At(C.Namespace.Root,"CINI")=0 and Read_At(C.Namespace.Root,"EINI")=0,"failed native prerequisite leaves every consumer untouched");
 if Mode in "reg-connect-failed"|"reg-connect-fatal" then
 Check(Connect_Calls=1 and Regions.Observe.Announced and not Regions.Quiet,"partial REG announcement remains an obligation");
 Check(Read_At(Child(Child(Bridge,"LPCB"),"EC0_"),"ECAV")=1,"REG side effect before its failure is preserved, not rolled back");
 Regions.Disconnect(Service,1000,Used,Accepted);
 Check(not Accepted and Disconnect_Calls=0 and Regions.Observe.Announced and not Regions.Quiet,"failed REG cannot be force-disconnected");
 else
 Check(Connect_Calls=0 and not Regions.Observe.Announced,"no registration after preparation or handler readiness loss");
 end if;
 Used:=Connect_Calls;Driver.Complete_Initialization(Work,Service,Accepted);
 Check(not Accepted and Connect_Calls=Used and Consumer_Calls=0,"failed native prerequisite cannot replay");
 Regions.Report_Copied_Diagnostics(Regions.Before_Cleanup);
 if Mode="fields-pending" then Check(not Regions.May_Open_Diagnostic_Channel and Prep_Model.Diagnostic_Open_Requests=0,"pending Fields cannot first-open diagnostic IPC");end if;
 Check(C.Retained_Results(Service)=0 and Prep_Model.Physical_Calls=0,"failed prefix leaks no Core result or physical EC command");
 C.Release_Storage(Foreign,Released);Check(Released,"foreign fixture released");
 -- Keep the failed owner and its Core alive for the rest of this process.
 Ada.Text_IO.Put_Line("Registration phase checks"&Checks'Image);return;
 end if;

 if Mode in "normal"|"prefix-error"|"many-node-errors"|"renamed"|"deep-prerequisite"|"confirmed"|"known-refusal"|"p2sb"|"ec-unavailable"|"lock-unavailable"|"handler-lost-cleanup"|"unsupported-inactive"|"suppressed-inactive"|"missing-reg-inactive"|"unsupported-many" then
 Check(Driver.Observe(Work).Stage=(if Mode in "prefix-error"|"many-node-errors" then Driver.Completed_With_Errors else Driver.Completed) and Driver.Observe(Work).Consumers_Prepared,"actual native preparation precedes completed consumer walk");
 Check(History_Count=(if Mode="many-node-errors" then 15 elsif No_EC_Handler then 4 else 6) and Consumer_Calls=1 and Read_At(C.Namespace.Root,"DINI")=1 and Read_At(C.Namespace.Root,"CINI")=(if No_EC_Handler then 0 else 1) and Read_At(C.Namespace.Root,"EINI")=(if No_EC_Handler then 0 else 1),"deferred initializers ran once");
 Check(Sender_Model.Calls=(if Mode="confirmed" then 2 else 1) and Native.Observe.Admission_Known,
 "bounded attributable admission outcome calls="&Sender_Model.Calls'Image&
 " stage="&Native.Observe.Stage'Image&" why="&Native.Observe.Why'Image&
 " discovery="&Native.Observe.Discovery.Status'Image&" answer="&Native.Observe.Answer'Image);
 Check(Native.Observe.Discovery.Status=ACPI_Native_GPIO_Observation.Driver.Described,"actual captured controller observation completed");
 else
 Check(Driver.Observe(Work).Stage=Driver.Incomplete and not Driver.Observe(Work).Consumers_Prepared and History_Count=3,"unknown/grant/clock refusal cannot resume consumers");
 Check(Read_At(C.Namespace.Root,"DINI")=0 and Read_At(C.Namespace.Root,"CINI")=0 and Read_At(C.Namespace.Root,"EINI")=0,"earlier consumer and siblings remain untouched");
 if Mode="unknown" then Check(Native.Observe.Why=Native.Admission_Unknown and not Native.Quiet and Prep_Model.Resumes=0,"unknown peer completion retains owner");
 elsif Mode="grant-pending" then Check(Native.Observe.Why=Native.Grant_Cleanup_Pending and not Native.Quiet and Sender_Model.Frees=0,"readonly grant must retire before freeing");
 else Check(Native.Observe.Why=Native.Resume_Refused and Prep_Model.Resumes=1,"expired original window cannot resume");end if;
 end if;
 if Mode="many-node-errors" then
 Check(Driver.Observe(Work).Errors=9 and Driver.Captured_Errors(Work)=Driver.Error_Count'Last and Driver.Omitted_Errors(Work)=1,"bounded node ledger preserves exact total and omitted count");
 declare E:constant Driver.Error_Report:=Driver.Error_At(Work,Driver.Error_Position'Last);begin
 Check(E.Scope=Child(Child(C.Namespace.Root,"_SB_"),"EE07") and E.Member="_INI" and E.Failure=AML_Execute.Division_By_Zero,"ledger preserves first eight errors in traversal order");
 end;
 end if;
 if Mode="prefix-error" then
 Check(Driver.Observe(Work).Errors=1 and Driver.Captured_Errors(Work)=1 and Driver.Omitted_Errors(Work)=0,"one ordinary prefix error captured without omission");
 declare Error:constant Driver.Error_Report:=Driver.Error_At(Work,1);begin
 Check(Error.Scope=Bridge and Error.Member="_INI" and Error.Method/=C.Namespace.Root and Error.Access_Result=C.Values.Available and Error.Failure=AML_Execute.Division_By_Zero and Error.Evaluation=AML_Execute.Division_By_Zero and Error.Reported_Charge>0,"actual ordinary initializer failure retains method,access,execution,charge");
 end;
 end if;
 declare Facts:ACPI_Serving_State.Eligibility:=(others=>True);begin
 Facts.Consumer_Owner_Quiet:=Native.Quiet;
 Check(ACPI_Serving_State.Eligible(Facts)=Native.Quiet,
   "observer eligibility preserves actual GPIO owner obligation");
 if Mode in "unknown"|"grant-pending" then
 Check(not ACPI_Serving_State.Eligible(Facts),
   "unknown completion or unretired grant cannot become an observer-only exit");
 end if;end;
 Used:=Connect_Calls;Driver.Complete_Initialization(Work,Service,Accepted);
 Check(not Accepted and Connect_Calls=Used and Consumer_Calls=1,"no prefix,REG or native preparation replay");
 if Mode in "unknown"|"grant-pending" then
 Regions.Disconnect(Service,1000,Used,Accepted);
 Check(not Accepted and Disconnect_Calls=0 and not Regions.Quiet,"native disconnect retains unknown admission or unretired grant without REG0");
 end if;
 Sender_Model.Confirmed:=True;Native.Close(Service);
 Check(C.Retained_Results(Service)=0,"actual observer Core roots fully released");
 if Driver.Observe(Work).Stage in Driver.Completed | Driver.Completed_With_Errors then
 Check(Native.Quiet,"successful owner quiet before modeled REG cleanup");

 if Mode="handler-lost-cleanup" then Prep_Model.EC_Ready:=False;end if;
 Regions.Disconnect(Service,1000,Used,Accepted);
 if Mode="handler-lost-cleanup" then
 Check(not Accepted and Disconnect_Calls=0 and Regions.Observe.Announced and not Regions.Quiet,"changed handler cannot clear an existing announcement");
 declare Facts:ACPI_Serving_State.Eligibility:=(others=>True);begin
 Facts.Regions_Unannounced:=not Regions.Observe.Announced;
 Check(not ACPI_Serving_State.Eligible(Facts),"retained announcement excludes observer even when GPIO owner is quiet");end;
 else
 Check(Accepted and Disconnect_Calls=(if No_EC_Handler then 0 else 2) and Regions.Quiet,"actual native wrapper disconnects only previously announced groups");
 if No_EC_Handler then Check(Regions.Observe.Unavailable=Unavailable_EC_Regions and Connect_Calls=0,"unavailable EC groups receive neither connect nor disconnect");end if;
 end if;
 if Mode/="handler-lost-cleanup" then C.Release_Storage(Service,Released);Check(Released,"successful fixture storage released");end if;
 else
 Check(Disconnect_Calls=0,"failed preparation never force-disconnects its registration owner");
 -- Retain Service/Run for the rest of this modeled process lifetime.
 end if;
 C.Release_Storage(Foreign,Released);Check(Released,"foreign fixture released");
 Check(Prep_Model.Prepare_Calls=1 and Prep_Model.Physical_Calls=0,"one modeled physical preparation, no EC hardware calls from copied reporting or scalar AML");
 Regions.Capture_If_Unvisited;
 Regions.Report_Copied_Diagnostics(Regions.Before_Cleanup);
 if Mode="unsupported-inactive" then
 Check(Regions.Observe.Unavailable=1,"unhandled CMOS stays unavailable without blocking walk");
 Check(Prep_Model.Unavailable_Line_Seen and Prep_Model.Unavailable_CMOS_Line and Prep_Model.Unavailable_Path_Seen,
   "copied unavailable diagnostics identify path,space,base,length and handler reason");
 elsif Mode="unsupported-many" then
 Check(Regions.Observe.Unavailable=9,"all unavailable regions remain counted");
 Check(Prep_Model.Unavailable_Lines=8 and Prep_Model.Unavailable_Limited_Summary,
   "unavailable logging is bounded with an exact omission count");
 elsif Mode in "suppressed-inactive"|"missing-reg-inactive" then
 Check(Regions.Observe.Unavailable=0,"notification-free supported groups are distinct from unavailable handlers");
 end if;
 Used:=Prep_Model.Diagnostic_Calls;
 Regions.Report_Copied_Diagnostics(Regions.Before_Cleanup);
 Check(Used>0 and Prep_Model.Diagnostic_Calls=Used,"native copied diagnostics have a one-shot before-cleanup checkpoint");
 Regions.Report_Copied_Diagnostics(Regions.After_Cleanup);
 Used:=Prep_Model.Diagnostic_Calls;
 Regions.Report_Copied_Diagnostics(Regions.After_Cleanup);
 Check(Prep_Model.Diagnostic_Calls=Used and Prep_Model.Physical_Calls=0,"native copied after-cleanup diagnostics cannot repeat or touch EC hardware");
 Ada.Text_IO.Put_Line("Registration phase checks"&Checks'Image);
end Phases_Tests;
