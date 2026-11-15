pragma Ada_2022;
with ACPI_Native_Initializer_History;with Interfaces;with ACPI_Hosted_Heap;with ACPI_Service;with AML_Execute;with AML_Objects;with AML_Names;
with ACPI_Region_Initialization;with ACPI_Region_Activation_Order;with ACPI_Device_Initialization;with ACPI_Device_Policy;
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
 History_Count,Last_Reported_Charge:Natural:=0;
 type Scope_History is array(1..16)of C.Namespace.Node_ID;
 Recorded_Scopes:Scope_History:=[others=>C.Namespace.Root];
 procedure Note(Service:C.State;Scope:C.Namespace.Node_ID;Method:C.Node_Descriptor;
 Access_Result:C.Values.Access_Status;Execution:AML_Execute.Execution_Status;Reported_Charge:Natural) is
 begin
 Last_Reported_Charge:=Reported_Charge;
 History.Note(Service,Scope,Method,Access_Result,Execution,Reported_Charge);
 History_Count:=History_Count+1;Recorded_Scopes(History_Count):=Scope;
 Check(History.Observe.Path_Complete and History.Observe.Last_Scope=Scope and History.Observe.Last_Method=C.Values.Position(Method),"copied initializer path/identity matches the actual same-Core invocation");
 Check(History.Observe.Complete and History.Observe.Invocations=Interfaces.Unsigned_32(History_Count),"actual native history adapter records each initializer invocation once");
 end;
 Service,Foreign:aliased C.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,
 Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
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
 use type R.Phase,R.Scope_Action,R.Cleanup_Action;
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
 package Driver is new ACPI_Device_Initialization(C,Prepare,Evaluator,After_Initializer=>After,Finish_Regions=>Finish,Note_Initializer=>Note);
 use type Driver.Phase;
 Work:Driver.Session;Accepted,Released:Boolean;Used:Natural;Action:R.Cleanup_Action;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;Data:Firmware_Tables.Bytes(1..4096);
 procedure Load(S:aliased in out C.State) is
 Stage:C.Stage_Status;Freeze:C.Freeze_Status;Status:C.Values.Access_Status;Execution:C.Execution_Report;Members:C.Namespace.Initialization_Report;
 begin C.Stage_Table(S,1,C.DSDT,Data(1..Natural(Last)),Stage);Check(Stage=C.Staged,"table staged");C.Freeze_Catalog(S,Freeze);Check(Freeze=C.Catalog_Frozen_OK,"table frozen");C.Execute_Frozen(S,10000,Execution,Status);Check(Status=C.Values.Available,"modules executed");C.Initialize_Members(S,Members,Status);Check(Status=C.Values.Available,"members initialized");end;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;Load(Service);Load(Foreign);
 Bridge:=Child(Child(C.Namespace.Root,"_SB_"),(if Mode="renamed" then "BUS0" else "BRG0"));EC:=Child(Child(Bridge,"LPCB"),"EC0_");Aux:=Child(Child(Bridge,"LPCB"),"AUX0");
 if Mode="retained" then Release_Enabled:=False;end if;
 Driver.Begin_Initialization(Work,Service,10000,Accepted);Check(Accepted and Driver.Observe(Work).Stage=Driver.Awaiting_Regions and Read_At(C.Namespace.Root,"PINI")=0,"begin is metadata-only");
 Driver.Complete_Initialization(Work,Service,Accepted);Check(Accepted,"one completion attempt");
 if Mode in "normal"|"renamed"|"foreign" then
 Check(Driver.Observe(Work).Stage=Driver.Completed and R.Observe(Run).Stage=R.Connected and Connect_Calls=2,"all eligible groups connected once");
 Check(Read_At(C.Namespace.Root,"CINI")=1 and Read_At(C.Namespace.Root,"EINI")=1,"earlier consumer and EC initializer follow EC registration");
 Check(Read_At(EC,"OSTY")=6 and Read_At(C.Namespace.Root,"DFLT")=0,"recognized OSYS and suppressed default-space REG");
 R.Disconnect(Run,Service,Owner,1000,Accepted);Check(Accepted and R.Observe(Run).Stage=R.Disconnected and Disconnect_Calls=2,"disconnect exactly announced groups");
 Check(Read_At(Aux,"CNRG")=1 and Read_At(Aux,"DNRG")=1,"coalesce two local regions per direction");
 elsif Mode="partial" then
 Check(Driver.Observe(Work).Stage=Driver.Failed and R.Observe(Run).Stage=R.Prepared and Connect_Calls=1,"unvisited group prevents global finish");
 R.Disconnect(Run,Service,Owner,1000,Accepted);Check(Accepted and Disconnect_Calls=1 and Read_At(Aux,"DNRG")=0,"known prepared successes disconnect without touching unvisited groups");
 elsif Mode="retained" then
 Check(Driver.Observe(Work).Stage=Driver.Failed and R.Observe(Run).Stage=R.Failed and R.Pending(Run),"refused result release remains owned");
 Release_Enabled:=True;R.Close(Run,Service,Action);Check(Action=R.Attempted and not R.Pending(Run) and R.Observe(Run).Stage=R.Failed,"attributable close releases handle without replay");
 elsif Mode in "parent-error"|"parent-absent" then
 Check(Driver.Observe(Work).Stage=Driver.Incomplete and Connect_Calls=0,"incomplete ancestor refuses REG before effects");
 elsif Mode="parent-fatal" then Check(Driver.Observe(Work).Stage=Driver.Failed and Connect_Calls=0,"Fatal ancestor executes no REG");
 elsif Mode="evaluator-overcharge" then
 Check(Driver.Observe(Work).Stage=Driver.Failed and History_Count=1 and Last_Reported_Charge=10001 and Driver.Observe(Work).Charged=0 and Connect_Calls=0,"overcharged initializer remains recorded before the fuel guard refuses it");
 elsif Mode="reg-failed" then
 Check(Driver.Observe(Work).Stage=Driver.Failed and R.Observe(Run).Stage=R.Failed and R.Observe(Run).Error=AML_Execute.Uninitialized and R.Observe(Run).Reported_Charge=47,"captured uninitialized-local signature remains visible");
 else Check(Driver.Observe(Work).Stage=Driver.Failed,"changed handler or exhausted scope budget refuses");end if;
 if R.Observe(Run).Stage=R.Failed then R.Disconnect(Run,Service,Owner,1000,Accepted);Check(not Accepted and Disconnect_Calls=0,"failed partial/unknown connection never forced disconnected");end if;
 Check(History_Count=(if Mode in "normal"|"renamed"|"foreign"|"partial" then 5 elsif Mode="parent-absent" then 2 elsif Mode in "evaluator-overcharge"|"handler-change" then 1 else 3),"ordered initializer attempts exclude missing, skipped and STA evaluations");
 Used:=Connect_Calls;Driver.Complete_Initialization(Work,Service,Accepted);Check(not Accepted and Connect_Calls=Used,"no initialization or REG replay");
 Check(History.Observe.Invocations=Interfaces.Unsigned_32(History_Count),"rejected replay cannot append history");
 Check(C.Retained_Results(Service)=0,"no leaked results after explicit cleanup");C.Release_Storage(Service,Released);Check(Released,"hosted storage released");C.Release_Storage(Foreign,Released);Check(Released,"foreign storage released");
 Ada.Text_IO.Put_Line("Registration phase checks"&Checks'Image);
end Phases_Tests;
