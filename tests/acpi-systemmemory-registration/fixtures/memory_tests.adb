pragma Ada_2022;
with AML_Decode;with Interfaces;with Platform_Pool_ABI;with ACPI_Platform_Region_Cache;with ACPI_Platform_Field_Provider;with ACPI_Field_Provider_View;with ACPI_Field_Evaluation;with AML_Field_Notes;with AML_Field_Access;with AML_Field_Layout;with ACPI_FADT.Transactions;
with ACPI_Hosted_Heap;with ACPI_Service;with AML_Execute;with AML_Objects;with AML_Names;
with Firmware_Tables;with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Memory_Tests is
 package C renames ACPI_Service;package IO renames Ada.Streams.Stream_IO;
 use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
 Service:aliased C.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,
 Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
 Checks:Natural:=0;procedure Check(OK:Boolean;Why:String:="") is begin Checks:=Checks+1;if not OK then raise Program_Error with Checks'Image&" "&Why;end if;end;

 package A renames AML_Field_Access;package L renames AML_Field_Layout;package T renames ACPI_FADT.Transactions;package ABI renames Platform_Pool_ABI;
 subtype Word is Interfaces.Unsigned_64;
 use type Word,AML_Decode.Byte,C.Namespace.Region_Space_ID,AML_Field_Notes.Boundary,AML_Field_Notes.Node_Position;
 Mode:constant String:=Ada.Command_Line.Argument(2);
 Virtual_First:constant Word:=16#1000_0000#;Virtual_Limit:constant Word:=Virtual_First+16#10000#;
 ERAX_Base:constant Word:=16#FE0B_0400#;ERAX_Page:constant Word:=16#FE0B_0000#;
 Dummy_Base:constant Word:=16#FEDC_0000#;Page_Bytes:constant Word:=4096;
 ErAX_Virtual:Word:=0;Maps_Called,Reads,Writes,Field_Entries:Natural:=0;
 Held:Boolean:=False;Cell,Lid_Cell:Word:=16#A0#;
 Last_Note:AML_Field_Notes.Event;Notes:Natural:=0;
 procedure Note(Item:AML_Field_Notes.Event) is begin Last_Note:=Item;Notes:=Notes+1;end;
 function Inspect(A:Natural) return Boolean is(A=1);
 function Exclusive(First,Limit:Word) return Boolean is(First>=Virtual_First and Limit<=Virtual_Limit and First<Limit);
 procedure Clock(Now:out Word;Available:out Boolean) is begin Now:=1;Available:=True;end;
 function May_Access(For_Write:Boolean) return Boolean is pragma Unreferenced(For_Write);begin return Held;end;
 procedure Map_Chunk(Auth:Natural;Physical,Virtual,Pages:Word;Read_Only:Boolean;Status:out Word;Why:out ABI.Refusal_Record) is
 begin Check(Auth=1 and Pages=1 and not Read_Only and Virtual>=Virtual_First and Virtual+Page_Bytes<=Virtual_Limit,"independent kernel mapping contract");Maps_Called:=Maps_Called+1;Status:=0;Why:=[others=>0];
 Check(Physical in ERAX_Page|Dummy_Base,"only independently modeled chipset members");
 if Physical=ERAX_Page then if Mode="unbacked" then Status:=ABI.Refusal_Word(ABI.Outside_Pool);Why(ABI.Code):=Status;else ErAX_Virtual:=Virtual;end if;end if;end;
 procedure Transfer(Virtual:Word;Width:T.Access_Width;For_Write:Boolean;Input:Word;Output:out Word;Completed:out Boolean) is
 begin Check(Held and ErAX_Virtual/=0 and T."="(Width,T.Byte_Access),"whole-field guard before data");Completed:=True;
 if Virtual=ErAX_Virtual+16#4A3# then Output:=Cell;if For_Write then Cell:=Input;Output:=0;Writes:=Writes+1;else Reads:=Reads+1;end if;
 elsif Virtual=ErAX_Virtual+16#4B8# then Check(not For_Write,"LSTE is read by captured REG");Output:=Lid_Cell;Reads:=Reads+1;
 else raise Program_Error with "unmodeled data transfer";end if;end;
 package Maps is new ACPI_Platform_Region_Cache(Natural,0,Inspect,Exclusive,Map_Chunk,Clock,May_Access,Transfer,8,8);
 Memory:Maps.Context;
 type Declaration is record Data:C.Namespace.Operation_Region_Attributes;Name:AML_Names.Segment;Node:C.Namespace.Node_ID;end record;
 procedure Validate(Item:Declaration;Geometry:L.Register_Geometry;Origin:out Maps.Source;Accepted:out Boolean) is
 begin Accepted:=Item.Data.Space=0 and Item.Data.Address=Geometry.Region.Base and Item.Data.Length=Geometry.Region.Length and Item.Data.Origin.Known;
 Ada.Text_IO.Put_Line("Validate name="&Item.Name&" space="&Item.Data.Space'Image&" base="&Item.Data.Address'Image&" bytes="&Item.Data.Length'Image&" known="&Item.Data.Origin.Known'Image&" valid="&Accepted'Image);
 Origin:=(Name=>Item.Name,Instance=>1,Namespace_Revision=>0,Node=>Word(Item.Node));end;
 function Compatible(First_Page,Pages:Word) return Boolean is
 begin Ada.Text_IO.Put_Line("Envelope first="&First_Page'Image&" pages="&Pages'Image&" expect="&Word'Image(Dummy_Base/Page_Bytes)&" compatible="&Boolean'(Pages=1 and First_Page in ERAX_Page/Page_Bytes|Dummy_Base/Page_Bytes)'Image);return Pages=1 and First_Page in ERAX_Page/Page_Bytes|Dummy_Base/Page_Bytes;end;
 package Provider is new ACPI_Platform_Field_Provider(Maps,Memory,Declaration,Validate,Compatible);
 procedure Begin_Field(IO:in out Provider.Context;Requires_Lock,For_Write:Boolean;Accepted:in out Boolean) is
 pragma Unreferenced(IO,Requires_Lock,For_Write);begin Accepted:=not Held;if Accepted then Held:=True;Field_Entries:=Field_Entries+1;end if;end;
 procedure End_Field(IO:in out Provider.Context;Accepted:in out Boolean) is pragma Unreferenced(IO);begin Accepted:=Held;if Accepted then Held:=False;end if;end;
 procedure Resolve_Local(IO:in out Provider.Context;Geometry:L.Register_Geometry;Target:in out A.Target;Accepted:in out Boolean) is
 begin Provider.Select_Target(IO,Geometry,False,Target,Accepted);end;
 package View is new ACPI_Field_Provider_View(Provider.Context,Provider.Is_Ready,Provider.Identity,Provider.Pending,
 Provider.Register_Count,Provider.Export_Target,Provider.Select_Target,Provider.Describe,Provider.Authorize,Provider.Dispatch,Provider.Reconciled,
 Resolve_Target=>Resolve_Local,Begin_Transaction=>Begin_Field,End_Transaction=>End_Field,Allow_Locked_Fields=>True,Observe_Field=>Note,Field_Notes_Enabled=>True);
 procedure Resolve_Startup(IO:in out Provider.Context;R:C.Namespace.Operation_Region_Attributes;Name:AML_Names.Segment;Node:C.Namespace.Node_ID;
 Geometry:L.Register_Geometry;Target:in out A.Target;Accepted:in out Boolean) is
 begin Provider.Resolve(IO,(R,Name,Node),Geometry,1000000,Target,Accepted);end;
 package Evaluation is new ACPI_Field_Evaluation(C,View,64,Enable_Startup_Platform_Resolution=>True,Resolve_Startup_Platform_Target=>Resolve_Startup);
 Cache:Evaluation.Context(64);Provider_IO:Provider.Context;
 function Find(Leaf:AML_Names.Segment) return C.Namespace.Node_ID is
 type Parts is array(Positive range<>) of AML_Names.Segment;
 Path:constant Parts:=["_SB_","PC00","LPCB","EC0_",Leaf];
 Parent:C.Namespace.Node_ID:=C.Namespace.Root;Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;
 begin for Part of Path loop C.Find_Object(Service,Parent,Part,Found,Status);Check(Status=C.Values.Available and Found.Status=C.Namespace.Found);Parent:=Found.Node;end loop;return Parent;end;
 function Read(Leaf:AML_Names.Segment) return Natural is
 Node:C.Node_Descriptor;Status:C.Values.Access_Status;Value:C.Values.Result;
 begin C.Describe_Node(Service,Find(Leaf),Node,Status);Check(Status=C.Values.Available);
 C.Evaluate_Named(Service,Node,1000,Value,Status);Check(Status=C.Values.Available and Value.Status=AML_Execute.Returned);return Natural(Value.Number);end;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Last:Ada.Streams.Stream_Element_Offset;Data:Firmware_Tables.Bytes(1..4096);
 Staged:C.Stage_Status;Frozen:C.Freeze_Status;Status:C.Values.Access_Status;Execution:C.Execution_Report;Members:C.Namespace.Initialization_Report;
 procedure Initialize_Provider is
 Good:Boolean;Node:C.Node_Descriptor;Data:C.Namespace.Operation_Region_Attributes;Status:C.Values.Access_Status;Target:A.Target;
 Geometry:constant L.Register_Geometry:=((Space=>L.Memory_Space,Base=>Dummy_Base,Length=>Page_Bytes,PCI=>(Known=>False)),0,T.Byte_Access);
 begin Maps.Initialize(Memory,1,Virtual_First,Virtual_Limit,Good);Check(Good,"independent pool owner initialized");Provider.Initialize(Provider_IO,1,Good);Check(Good);
 C.Describe_Node(Service,Find("DREG"),Node,Status);Check(Status=C.Values.Available);C.Read_Region_Descriptor(Service,Node,Data,Status);Check(Status=C.Values.Available);
 Provider.Resolve(Provider_IO,(Data,"DREG",C.Values.Position(Node)),Geometry,1000000,Target,Good);Ada.Text_IO.Put_Line("Seed ready="&Provider.Is_Ready(Provider_IO)'Image&" maps="&Maps_Called'Image&" regions="&Maps.Observe(Memory).Regions'Image&" stage="&Maps.Observe(Memory).Stage'Image);Check(Good,"unrelated admitted member seeds the same provider");
 Evaluation.Prepare(Cache,Service,Provider_IO,Good);Check(Good,"same-Core actual binding cache");
 Check(Evaluation.Observe(Cache).Read_Fields=1 and Evaluation.Observe(Cache).Missing_Fields=2,"ERAX fields remain unbound until independently resolved");end;
 Value:C.Values.Result;Args:constant AML_Execute.Arguments:=[0=>3,1=>1,others=>0];Released:Boolean;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);
 for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
 C.Stage_Table(Service,1,C.DSDT,Data(1..Natural(Last)),Staged);Check(Staged=C.Staged);
 C.Freeze_Catalog(Service,Frozen);Check(Frozen=C.Catalog_Frozen_OK);
 C.Execute_Frozen(Service,10000,Execution,Status);Check(Status=C.Values.Available);
 C.Initialize_Members(Service,Members,Status);Check(Status=C.Values.Available);
 Initialize_Provider;Lid_Cell:=2;
 Evaluation.Invoke_Retained(Cache,Service,Provider_IO,Find("_REG"),Args,2,10000,Value,Status);
 Check(Status=C.Values.Available and Notes>0 and Last_Note.Known_Field and Last_Note.Known_Region and Last_Note.Space=0,"actual SystemMemory operand captured");
 if Mode="unbacked" then
 Check(Value.Status=AML_Execute.Unsupported and Value.Charged=49 and Reads=0 and Writes=0 and Cell=16#A0#,"unbacked ERAX refuses without data IO");
 Check(Last_Note.Field=AML_Field_Notes.Node_Position(Find("OSTY")) and Last_Note.For_Write and Last_Note.At_Point=AML_Field_Notes.Resolution_Finished and not Last_Note.Direct_Binding,"failed OSTY resolution identified exactly");
 declare P:constant Maps.Profile:=Maps.Read_Profile(Memory,2);use type Maps.Outcome;begin Check(P.Available and P.Geometry.Address=ERAX_Base and P.Geometry.Length=255 and P.Result=Maps.Kernel_Refused and P.Raw_Status=ABI.Refusal_Word(ABI.Outside_Pool),"kernel outside-pool evidence retained");end;
 else Check(Value.Status=AML_Execute.No_Return and Cell=16#A6# and Reads=4 and Writes=1 and not Held,"real ERAX OSTY preserve write and LSTE reads completed");end if;
 Ada.Text_IO.Put_Line("Memory mode="&Mode&" maps="&Maps_Called'Image&" reads="&Reads'Image&" writes="&Writes'Image&" cell="&Cell'Image&" notes="&Notes'Image&" boundary="&Last_Note.At_Point'Image&" space="&Last_Note.Space'Image&" bit="&Last_Note.Bit_Offset'Image&" bits="&Last_Note.Bit_Length'Image);
 Ada.Text_IO.Put_Line("REG status="&Value.Status'Image&" charged="&Value.Charged'Image&" access="&Status'Image&" ecav="&Read("AVAL")'Image);
 Check(C.Retained_Results(Service)=0);C.Release_Storage(Service,Released);Check(Released);
end Memory_Tests;
