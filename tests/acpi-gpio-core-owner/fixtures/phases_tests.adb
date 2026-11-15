pragma Ada_2022;
with ACPI_Native_GPIO_Observation;with Composition_State;with ACPI_Native_Pre_Device;with ACPI_Native_Delays;with Prep_Model;with Sender_Model;with ACPI_Native_Initializer_History;with Interfaces;with ACPI_Hosted_Heap;with ACPI_Service;with AML_Execute;with AML_Names;
with ACPI_Serving_State;with ACPI_Region_Initialization;with ACPI_Region_Activation_Order;with ACPI_Device_Initialization;with ACPI_Device_Policy;
with AML_Decode;with Firmware_Tables;with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Phases_Tests is
 package C renames ACPI_Service;package IO renames Ada.Streams.Stream_IO;package P renames ACPI_Device_Policy;
 Mode:constant String:=Ada.Command_Line.Argument(2);
 use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
 use type P.Region_Outcome;use type Interfaces.Unsigned_32;
 Checks,Connect_Calls,Disconnect_Calls:Natural:=0;Release_Enabled:Boolean:=True;
 procedure Check(OK:Boolean;Why:String) is begin Checks:=Checks+1;if not OK then raise Program_Error with Why;end if;end;
 package History renames ACPI_Native_Initializer_History;
 History_Count:Natural:=0;
 type Scope_History is array(1..16)of C.Namespace.Node_ID;
 Recorded_Scopes:Scope_History:=[others=>C.Namespace.Root];
 procedure Note(Service:C.State;Scope:C.Namespace.Node_ID;Method:C.Node_Descriptor;
 Access_Result:C.Values.Access_Status;Execution:AML_Execute.Execution_Status;Reported_Charge:Natural) is
 begin
 History.Note(Service,Scope,Method,Access_Result,Execution,Reported_Charge);
 History_Count:=History_Count+1;Recorded_Scopes(History_Count):=Scope;
 Check(History.Observe.Path_Complete and History.Observe.Last_Scope=Scope and History.Observe.Last_Method=C.Values.Position(Method),"copied initializer path/identity matches the actual same-Core invocation");
 Check(History.Observe.Complete and History.Observe.Invocations=Interfaces.Unsigned_32(History_Count),"actual native history adapter records each initializer invocation once");
 end;
 Service:C.State renames Composition_State.Service;
 Foreign:C.State renames Composition_State.Foreign;
 package Native renames ACPI_Native_Pre_Device;
 use type Native.Failure,ACPI_Native_GPIO_Observation.Driver.Outcome;
 type Context is record Ready:Boolean:=True;end record;Owner:Context;
 function Ready(X:Context;Region:C.Node_Descriptor;Data:C.Namespace.Operation_Region_Attributes) return Boolean is
 pragma Unreferenced(Region,Data);begin return X.Ready;end;
 function Permanent(X:Context;Region:C.Node_Descriptor) return Boolean is pragma Unreferenced(X,Region);begin return False;end;
 procedure Invoke(S:aliased in out C.State;X:in out Context;Node:C.Namespace.Node_ID;Args:AML_Execute.Arguments;
 Count:Natural;Budget:Natural;Result:out C.Values.Result;Status:out C.Values.Access_Status) is
 pragma Unreferenced(X);use type AML_Decode.Integer_Value;
 begin if Args(1)=1 then Connect_Calls:=Connect_Calls+1;else Disconnect_Calls:=Disconnect_Calls+1;end if;
 C.Invoke_Retained(S,Node,Args,Count,Budget,Result,Status);end;
 procedure Release_Value(S:in out C.State;Handle:in out C.Values.Value_Handle;Status:out C.Values.Access_Status) is
 begin if Release_Enabled then C.Release_Result(S,Handle,Status);else Status:=C.Values.Busy;end if;end;
 package R is new ACPI_Region_Initialization(C,Context,Ready,Permanent,Invoke,Release_Value,
 Require_Every_Handler=>False,Coalesce_Local_Notifications=>True);
 package Order is new ACPI_Region_Activation_Order(C);
 use type R.Phase,R.Scope_Action;
 Run:R.Session;
 type Ancestors is array(C.Namespace.Node_ID)of C.Namespace.Node_ID;
 type Flags is array(C.Namespace.Node_ID)of Boolean;
 Due:Ancestors:=[others=>C.Namespace.Root];Done:Flags:=[others=>False];
 function Child(Parent:C.Namespace.Node_ID;Part:AML_Names.Segment) return C.Namespace.Node_ID is
 Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;
 begin C.Find_Object(Service,Parent,Part,Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found,"node lookup");return Found.Node;end;
 function Read_At(Parent:C.Namespace.Node_ID;Part:AML_Names.Segment) return Natural is
 Node:C.Node_Descriptor;Status:C.Values.Access_Status;Value:C.Values.Result;
 begin C.Describe_Node(Service,Child(Parent,Part),Node,Status);Check(Status=C.Values.Available,"scalar descriptor");
 C.Evaluate_Named(Service,Node,1000,Value,Status);Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned,"scalar observation");return Natural(Value.Number);end;
 Bridge,EC,Aux:C.Namespace.Node_ID:=C.Namespace.Root;
 procedure Prepare(S:aliased in out C.State;Budget:Natural;Charged:out Natural;Outcome:out P.Region_Outcome) is
 pragma Unreferenced(Budget);Accepted:Boolean;Action:R.Scope_Action;Used:Natural;
 begin Charged:=0;Outcome:=P.Region_Initialization_Failed;R.Prepare(Run,S,Owner,Accepted);
 Check(Accepted and R.Observe(Run).Stage=R.Prepared and Connect_Calls=0,"preparation executes no REG");
 R.Finish_Connect(Run,S,Owner,Accepted);Check(not Accepted and R.Observe(Run).Stage=R.Prepared,"cannot finish unvisited groups");
 for Scope in C.Namespace.Node_ID loop if R.Has_Prepared_Group(Run,Scope) then
 Order.Required_Ancestor(S,Scope,Due(Scope),Accepted);Check(Accepted,"metadata ancestor plan");end if;end loop;
 Check(Due(EC)=Bridge and Due(Aux)=Bridge,"EC and auxiliary scopes await their initializer ancestor");
 if Mode="foreign" then R.Connect_Scope(Run,Foreign,Owner,EC,1000,Used,Action);
 Check(Action=R.Wrong_Context and Used=0 and Connect_Calls=0 and R.Observe(Run).Stage=R.Prepared,"foreign context cannot consume prepared owner");end if;
 if Mode="handler-change" then Owner.Ready:=False;end if;
 Outcome:=P.Regions_Prepared;end;
 procedure After(S:aliased in out C.State;Scope:C.Namespace.Node_ID;Initialization_Completed:Boolean;
 Budget:Natural;Charged:in out Natural;Outcome:in out P.Region_Outcome) is
 Action:R.Scope_Action;Used:Natural;Limit:Natural;
 begin Charged:=0;Outcome:=P.Region_Initialization_Failed;
 for Group in C.Namespace.Node_ID loop if R.Has_Prepared_Group(Run,Group) and then not Done(Group) and then Due(Group)=Scope then
 if not Initialization_Completed then Outcome:=P.Regions_Unavailable;return;end if;
 if not(Mode="partial" and Group=Aux) then
 Done(Group):=True;Limit:=(if Mode="budget" and Group=EC then 0 else Budget-Charged);
 R.Connect_Scope(Run,S,Owner,Group,Limit,Used,Action);Check(Used<=Limit,"per-call budget bound");Charged:=Charged+Used;
 if Action/=R.Scope_Completed then return;end if;
 if Group=EC and Mode/="budget" then
 declare Expected:constant String:="\_SB_."&(if Mode="renamed" then "BUS0" else "BRG0")&"._INI";Text:constant History.Path_Text:=History.Observe.Last_Path;begin
 Check(Text(Text'Last-Expected'Length+1..Text'Last)=Expected,"ancestor initializer is named exactly before EC registration");end;
 Check(History_Count=3 and Recorded_Scopes(1)=C.Namespace.Root and Recorded_Scopes(3)=Bridge,"initializer history records ancestor attempts before registration and excludes STA");
 Check(Read_At(C.Namespace.Root,"PINI")=1,"ancestor initializer ran before EC REG");
 R.Connect_Scope(Run,S,Owner,Group,Budget-Charged,Used,Action);
 Check(Action=R.Already_Completed and Used=0,"scope cannot replay REG");
 end if;
 end if;end if;end loop;
 Outcome:=P.Regions_Connected;end;
 procedure Finish(S:aliased in out C.State;Budget:Natural;Charged:in out Natural;Outcome:in out P.Region_Outcome) is
 pragma Unreferenced(Budget);Accepted:Boolean;
 begin Charged:=0;R.Finish_Connect(Run,S,Owner,Accepted);Outcome:=(if Accepted then R.Outcome(Run) else P.Region_Initialization_Failed);end;
 procedure Evaluator(S:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;Result:out C.Values.Result;Status:out C.Values.Access_Status) is
 begin C.Evaluate_Named(S,Item,Budget,Result,Status);
 if Mode="evaluator-overcharge" then Result.Charged:=Budget+1;end if;end;
 use type Interfaces.Unsigned_64;
 Consumer_Calls:Natural:=0;
 procedure Classify(S:C.State;Scope:C.Namespace.Node_ID;Required,Accepted:in out Boolean) is
 pragma Unreferenced(S);
 begin Accepted:=Mode/="classifier-refused";Required:=Scope=(if Mode="deep-prerequisite" then Aux else Bridge);end;
 procedure Consumers(S:aliased in out C.State;Budget:Natural;Charged:in out Natural;Accepted:in out Boolean) is
 Value:C.Values.Result;Status:C.Values.Access_Status;Args:constant AML_Execute.Arguments:=[others=>0];
 Prefix:constant History.Report:=History.Observe;Spent:Natural;
 begin
 Consumer_Calls:=Consumer_Calls+1;
 Check(History_Count=3 and Read_At(C.Namespace.Root,"PINI")=1 and Read_At(C.Namespace.Root,"RINI")=1 and Read_At(C.Namespace.Root,"SINI")=1,"actual prerequisite prefix before native preparation");
 Check(Read_At(C.Namespace.Root,"DINI")=0 and Read_At(C.Namespace.Root,"CINI")=0 and Read_At(C.Namespace.Root,"EINI")=0,"all consumers still deferred at native owner entry");
 Check(R.Observe(Run).Stage=R.Connected and Connect_Calls=2,"actual scoped REG complete before native owner");
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
 package Driver is new ACPI_Device_Initialization(C,Prepare,Evaluator,After_Initializer=>After,Finish_Regions=>Finish,Note_Initializer=>Note,Consumer_Preparation_Enabled=>True,Classify_Prerequisite=>Classify,Prepare_Consumers=>Consumers);
 use type Driver.Phase;
 Work:Driver.Session;Accepted,Released:Boolean;Used:Natural;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;Data:Firmware_Tables.Bytes(1..4096);
 procedure Load(S:aliased in out C.State) is
 Stage:C.Stage_Status;Freeze:C.Freeze_Status;Status:C.Values.Access_Status;Execution:C.Execution_Report;Members:C.Namespace.Initialization_Report;
 begin C.Stage_Table(S,1,C.DSDT,Data(1..Natural(Last)),Stage);Check(Stage=C.Staged,"table staged");C.Freeze_Catalog(S,Freeze);Check(Freeze=C.Catalog_Frozen_OK,"table frozen");C.Execute_Frozen(S,10000,Execution,Status);Check(Status=C.Values.Available,"modules executed");C.Initialize_Members(S,Members,Status);Check(Status=C.Values.Available,"members initialized");end;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;Load(Service);Load(Foreign);
 Bridge:=Child(Child(C.Namespace.Root,"_SB_"),(if Mode="renamed" then "BUS0" else "BRG0"));EC:=Child(Child(Bridge,"LPCB"),"EC0_");Aux:=Child(Child(Bridge,"LPCB"),"AUX0");
 if Mode="retained" then Release_Enabled:=False;end if;
 Sender_Model.Reset;Prep_Model.Mode:=1;Sender_Model.Mode:=1;
 if Mode="confirmed" then Sender_Model.Mode:=33;
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
 if Mode in "normal"|"renamed"|"deep-prerequisite"|"confirmed"|"known-refusal"|"p2sb" then
 Check(Driver.Observe(Work).Stage=Driver.Completed and Driver.Observe(Work).Consumers_Prepared,"actual native preparation precedes completed consumer walk");
 Check(History_Count=6 and Consumer_Calls=1 and Read_At(C.Namespace.Root,"DINI")=1 and Read_At(C.Namespace.Root,"CINI")=1 and Read_At(C.Namespace.Root,"EINI")=1,"deferred initializers ran once");
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
 Sender_Model.Confirmed:=True;Native.Close(Service);
 Check(C.Retained_Results(Service)=0,"actual observer Core roots fully released");
 if Driver.Observe(Work).Stage=Driver.Completed then
 Check(Native.Quiet,"successful owner quiet before modeled REG cleanup");
 R.Disconnect(Run,Service,Owner,1000,Accepted);Check(Accepted and Disconnect_Calls=2,"known scalar fixture groups disconnect once");
 C.Release_Storage(Service,Released);Check(Released,"successful fixture storage released");
 else
 Check(Disconnect_Calls=0,"failed preparation never force-disconnects its registration owner");
 -- Retain Service/Run for the rest of this modeled process lifetime.
 end if;
 C.Release_Storage(Foreign,Released);Check(Released,"foreign fixture released");
 Ada.Text_IO.Put_Line("Registration phase checks"&Checks'Image);
end Phases_Tests;
