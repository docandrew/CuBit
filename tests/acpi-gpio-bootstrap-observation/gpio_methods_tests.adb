pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with AML_Objects; with AML_Execute; with AML_Names;
with ACPI_Service; with ACPI_Device_Status; with ACPI_Resource_Reader;
with ACPI_GPIO_Controller_Resources; with Firmware_Tables;
with ACPI_Resource_Data;
with ACPI_GPIO_Bootstrap_Observation;
with ACPI_GPIO_Admission_Payload;
procedure GPIO_Methods_Tests is
   package C renames ACPI_Service;
   package P renames ACPI_GPIO_Controller_Resources;
   package Statuses is new ACPI_Device_Status(C);
   package Reader is new ACPI_Resource_Reader(C);
   package IO renames Ada.Streams.Stream_IO;
   use type C.Stage_Status,C.Freeze_Status,C.Values.Access_Status,C.Values.Module_Outcome;
   use type C.Values.Value_Kind,C.Values.Value_Handle;
   use type AML_Execute.Execution_Status,Ada.Streams.Stream_Element_Offset;
   use type Statuses.Status_Outcome,Reader.Copy_Status,P.Outcome,P.Address;
   use type Interfaces.Unsigned_64,Firmware_Tables.Bytes,Firmware_Tables.Byte,ACPI_Resource_Data.Byte_Count;
   Service:aliased C.State(2,4096,4096,2,Value_Object_Capacity=>AML_Objects.Max_Objects,
      Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
   File:IO.File_Type;
   Checks:Natural:=0;
   Fuel:constant Positive:=10_000;
   GPIO:C.Namespace.Node_ID;
   Device:C.Node_Descriptor;
   Access_Status:C.Values.Access_Status;
   UID_Mode:constant String:=Ada.Command_Line.Argument(7);
   Expected_HID:constant String:=Ada.Command_Line.Argument(2);
   Expected_STA:constant Interfaces.Unsigned_64:=Interfaces.Unsigned_64'Value(Ada.Command_Line.Argument(3));
   Expected_Base:constant P.Address:=P.Address'Value(Ada.Command_Line.Argument(4));
   Expected_IRQ:constant Natural:=Natural'Value(Ada.Command_Line.Argument(5));
   Expected:Firmware_Tables.Bytes(1..59);
   Quiet:Boolean:=True;
   Refuse_Release:Boolean:=False;
   Release_Calls:Natural:=0;
   Calls,CRS_Calls:Natural:=0;
   procedure Evaluate(Service:aliased in out C.State;Item:C.Node_Descriptor;Budget:Natural;
      Result:out C.Values.Result;Status:out C.Values.Access_Status) is
   begin
      Calls:=Calls+1;if C.Values.Node_Name(Item)="_CRS" then CRS_Calls:=CRS_Calls+1;end if;
      C.Evaluate_Named(Service,Item,Budget,Result,Status);
   end Evaluate;
   function Hardware_Quiet(Service:C.State) return Boolean is
      pragma Unreferenced(Service);
   begin return Quiet;end Hardware_Quiet;
   procedure Release_Result(Service:in out C.State;Handle:in out C.Values.Value_Handle;Status:out C.Values.Access_Status) is
   begin
      Release_Calls:=Release_Calls+1;
      if Refuse_Release then Status:=C.Values.Invalid_Value;
      else C.Release_Result(Service,Handle,Status);end if;
   end Release_Result;
   package Observer is new ACPI_GPIO_Bootstrap_Observation(C,Evaluate,Release_Result,Hardware_Quiet);
   use type Observer.UID_Kind;
   package Payload is new ACPI_GPIO_Admission_Payload(C,Observer,Observer.Session,Observer.Description,Observer.Is_Current);
   package Wire renames Payload.P;
   use type Payload.Outcome,Wire.Request,Wire.Decode_Status,Wire.UID_Kind,Interfaces.Unsigned_32;
   Catalog_Epoch:constant Interfaces.Unsigned_64:=16#0000_0001_0000_0007#;
   use type Observer.Action_Result,Observer.Outcome;
   procedure Check(Good:Boolean;Why:String) is
   begin Checks:=Checks+1;if not Good then raise Program_Error with Why;end if;end Check;
   function Member(Name:AML_Names.Segment) return C.Node_Descriptor is
      Node:C.Namespace.Node_ID;Item:C.Node_Descriptor;Status:C.Values.Access_Status;
   begin
      C.Find_Child(Service,GPIO,Name,Node,Status);
      Check(Status=C.Values.Available and then Node/=C.Namespace.Root,"direct GPIO member "&Name);
      C.Describe_Node(Service,Node,Item,Status);Check(Status=C.Values.Available,"member descriptor");return Item;
   end Member;
   procedure Retire(Value:in out C.Values.Result) is
      Handle:C.Values.Value_Handle:=Value.Handle;
   begin
      C.Release_Result(Service,Handle,Access_Status);
      Check(Access_Status=C.Values.Available and then Handle=C.Values.No_Value and then C.Retained_Results(Service)=0,"all result roots retired");
   end Retire;
begin
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));
   declare
      Size:constant Natural:=Natural(IO.Size(File));
      Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));
      Last:Ada.Streams.Stream_Element_Offset;Table:Firmware_Tables.Bytes(1..Size);
      Stage:C.Stage_Status;Frozen:C.Freeze_Status;Execution:C.Execution_Report;
      Members:C.Namespace.Initialization_Report;Bus:C.Namespace.Node_ID;
   begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole compiled capture-method fixture");
      for I in Table'Range loop Table(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
      C.Stage_Table(Service,1,C.DSDT,Table,Stage);Check(Stage=C.Staged,"stage fixture");
      if Ada.Command_Line.Argument_Count=8 then
         declare
            More:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..4096);Final:Ada.Streams.Stream_Element_Offset;
            Data:Firmware_Tables.Bytes(1..4096);
         begin
            IO.Open(More,IO.In_File,Ada.Command_Line.Argument(8));IO.Read(More,Raw,Final);IO.Close(More);
            for I in 1..Natural(Final) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
            C.Stage_Table(Service,2,C.SSDT,Data(1..Natural(Final)),Stage);Check(Stage=C.Staged,"stage independent defining SSDT");
         end;
      end if;
      C.Freeze_Catalog(Service,Frozen);Check(Frozen=C.Catalog_Frozen_OK,"freeze fixture");
      C.Execute_Frozen(Service,Fuel,Execution,Access_Status);
      Check(Access_Status=C.Values.Available and then Execution.Modules.Outcome=C.Values.Modules_Completed,"execute module declarations");
      C.Initialize_Members(Service,Members,Access_Status);Check(Access_Status=C.Values.Available,"initialize namespace members, no device INI");
      C.Find_Child(Service,C.Namespace.Root,"_SB_",Bus,Access_Status);Check(Access_Status=C.Values.Available and then Bus/=C.Namespace.Root,"system bus");
      C.Find_Child(Service,Bus,"GPI0",GPIO,Access_Status);Check(Access_Status=C.Values.Available and then GPIO/=C.Namespace.Root,"actual captured GPIO device");
      C.Describe_Node(Service,GPIO,Device,Access_Status);Check(Access_Status=C.Values.Available,"device description");
   end;
   IO.Open(File,IO.In_File,Ada.Command_Line.Argument(6));
   declare Raw:Ada.Streams.Stream_Element_Array(1..59);Last:Ada.Streams.Stream_Element_Offset;begin
      IO.Read(File,Raw,Last);IO.Close(File);Check(Last=Raw'Last,"whole expected raw template");
      for I in Expected'Range loop Expected(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
   end;
   -- Repeated calls exercise method-local Buffer/CreateDWordField cleanup on
   -- the same Core; no second evaluator or device _INI is used.
   for Iteration in 1..3 loop
      declare
         Value:C.Values.Result;Desc:C.Values.Value_Description;
         Data:Firmware_Tables.Bytes(1..Expected_HID'Length);Copied:Natural;
         STA:Statuses.Status_Report;Copy:Reader.Report;Description:P.Report;
         CRS:Firmware_Tables.Bytes(1..59);
         Offsets:constant array(P.Resource_Slot) of P.Address:=[16#6E_0000#,16#6D_0000#,16#6A_0000#,16#69_0000#];
         use type P.IRQ.Interrupt_Number;
      begin
         Check(C.Retained_Results(Service)=0,"iteration starts without roots");
         C.Evaluate_Named(Service,Member("_HID"),Fuel,Value,Access_Status);
         Check(Access_Status=C.Values.Available and then Value.Status=AML_Execute.Object_Returned,"captured HID method executes");
         C.Describe_Result(Service,Value.Handle,Desc,Access_Status);
         Check(Access_Status=C.Values.Available and then Desc.Kind=C.Values.String_Description and then Desc.Length=Expected_HID'Length,"exact HID type/length");
         C.Read_Result_Bytes(Service,Value.Handle,0,Data,Copied,Access_Status);
         Check(Access_Status=C.Values.Available and then Copied=Data'Length,"copy exact HID");
         for I in Data'Range loop Check(Data(I)=Firmware_Tables.Byte(Character'Pos(Expected_HID(I))),"HID branch bytes");end loop;
         Retire(Value);
         Statuses.Observe_Status(Service,Device,Fuel,STA);
         Check(STA.Outcome=Statuses.Status_Available and then STA.Flags=Expected_STA and then not STA.Defaulted,"fresh actual STA method flags");
         Check(C.Retained_Results(Service)=0,"STA cleanup");
         C.Evaluate_Named(Service,Member("_CRS"),Fuel,Value,Access_Status);
         Check(Access_Status=C.Values.Available and then Value.Status=AML_Execute.Object_Returned,"captured CRS method executes");
         declare Small:Firmware_Tables.Bytes(1..58);Too_Small:Reader.Report;begin
            Reader.Copy_Template(Service,Value.Handle,Small,Too_Small);
            Check(Too_Small.Status=Reader.Insufficient_Capacity and then Too_Small.Required=59 and then Too_Small.Copied=0,"bounded copy refuses truncation without reevaluation");
         end;
         Reader.Copy_Template(Service,Value.Handle,CRS,Copy);
         Check(Copy.Status=Reader.Copied_Template and then CRS=Expected,"all CRS bytes match independent expected construction");
         P.Describe(CRS,Description);Check(Description.Status=P.Described,"typed resource description");
         for I in P.Resource_Slot loop
            Check(Description.Value.Memory(I).Base=Expected_Base+Offsets(I) and then Description.Value.Memory(I).Bytes=65_536,"original community order/exact ranges");
         end loop;
         Check(Description.Value.Interrupt.Number=P.IRQ.Interrupt_Number(Expected_IRQ),"fresh firmware IRQ scalar copied");
         Retire(Value);
      end;
   end loop;
   declare
      Run,Small_Run,Not_Quiet,Limited_Run,Fuel_Run,Cleanup_Run,High_Run:Observer.Session;
      Data:Firmware_Tables.Bytes(1..64);Small:Firmware_Tables.Bytes(1..58);
      Action:Observer.Action_Result;Prior:Natural;Copied:Observer.Metadata;
      Other:C.State(1,4096,4096,1,Value_Object_Capacity=>AML_Objects.Max_Objects,
         Value_Byte_Capacity=>AML_Objects.Max_Bytes,Value_Element_Capacity=>AML_Objects.Max_Elements);
   begin
      Observer.Describe(Run,Service,Device,Fuel,Data,Action);
      Check(Action=Observer.Accepted and Observer.Consumed(Run),"one-shot observation consumed");
      Check(Observer.Observe(Run).Charged<=Fuel,"one shared fuel budget");
      if Expected_STA=8 then
         Check(Observer.Observe(Run).Status=Observer.Device_Unusable and Calls=1 and CRS_Calls=0,"disabled GPIO never evaluates HID or CRS");
         Check(Data=[Data'Range=>0] and not Observer.Pending(Run),"disabled observation exposes no raw resources or roots");
      elsif UID_Mode in "long"|"buffer" then
         Check(Observer.Observe(Run).Status=Observer.Unsupported_UID and CRS_Calls=0,
           "unsupported UID refuses before CRS");
         Check(not Observer.Pending(Run) and C.Retained_Results(Service)=0 and Data=[Data'Range=>0],
           "unsupported UID releases complete result and publishes no facts");
         declare Released:Boolean;begin C.Release_Storage(Service,Released);Check(Released,"UID refusal retires Core");end;
         Ada.Text_IO.Put_Line("GPIO captured methods checks"&Checks'Image);return;
      else
         Check(Observer.Observe(Run).Status=Observer.Described and Calls=(if UID_Mode="absent" then 3 else 4) and CRS_Calls=1,"fresh STA HID UID CRS evaluate at most once");
         Copied:=Observer.Description(Run);
         Check(Copied.Identifier=Expected_HID and Copied.Status_Flags=Expected_STA and not Copied.Status_Defaulted,"fresh copied identity/status tuple");
         if UID_Mode="absent" then Check(Copied.UID.Kind=Observer.No_UID,"UID absence observed, not invented");
         elsif UID_Mode="integer" then Check(Copied.UID.Kind=Observer.Integer_UID and Copied.UID.Number=16#FEDC_BA98#,"UID integer preserves full AML value");
         elsif UID_Mode="integer-zero" then Check(Copied.UID.Kind=Observer.Integer_UID and Copied.UID.Number=0,"zero UID is present integer, not absent");
         else Check(Copied.UID.Kind=Observer.String_UID and Copied.UID.Length=8 and Copied.UID.Text="GPIO-123","complete eight-byte UID string copied");end if;
         declare
            Packet,Decoded:Wire.Request;Result:Payload.Outcome;Before:constant Natural:=Calls;
            Other_Packet:Wire.Request;
            History:Payload.Initializer_Provenance:=(Complete=>True,Invocations=>Interfaces.Unsigned_32'Last,Ordered_Digest=>Interfaces.Unsigned_64'Last);
         begin
            Payload.Build(Service,Run,Catalog_Epoch,History,Packet,Result);
            Check(Result=Payload.Payload_Ready and Wire.Valid(Packet),"build typed admission payload from current copied facts");
            Check(Packet.Initializer_Invocations=History.Invocations and Packet.Initializer_Ordered_Digest=History.Ordered_Digest,"complete initializer provenance copied at full wire widths");
            Check(Packet.Epoch=Catalog_Epoch and Packet.Segments=2 and Packet.Path(1)="_SB_" and Packet.Path(2)="GPI0","full epoch and real namespace path");
            Check(Packet.Table=(if Ada.Command_Line.Argument_Count=8 then "SSDT" else "DSDT") and
               Packet.Table_ID=(if Ada.Command_Line.Argument_Count=8 then "GPIOORIG" else "GPIOMODL"),"actual device defining table, never guessed first/last table");
            Check(Packet.HID=Expected_HID and Packet.STA=Interfaces.Unsigned_32(Expected_STA) and Packet.Spans=4,"fresh identity and exact resource count");
            if UID_Mode="absent" then Check(Packet.UID_Type=Wire.No_UID and Packet.UID=0,"confirmed UID absence");
            elsif UID_Mode="integer" then Check(Packet.UID_Type=Wire.Integer_UID and Packet.UID=16#FEDC_BA98#,"integer UID encoded whole");
            elsif UID_Mode="integer-zero" then Check(Packet.UID_Type=Wire.Integer_UID and Packet.UID=0,"zero UID distinct from no UID");
            else Check(Packet.UID_Type=Wire.String_UID and Packet.UID=16#3332_312D_4F49_5047#,"eight-byte UID packs little endian without truncation");end if;
            for I in P.Resource_Slot loop Check(Packet.Items(I).Base=Interfaces.Unsigned_64(Copied.Resources.Memory(I).Base) and
               Packet.Items(I).Length=Interfaces.Unsigned_64(Copied.Resources.Memory(I).Bytes) and
               Packet.Items(I).Writable=Copied.Resources.Memory(I).Declared_Writeable,"copy exact ordered spans, including unaligned firmware claims");end loop;
            Check(Packet.IRQ.Present and Packet.IRQ.GSI=Interfaces.Unsigned_32(Expected_IRQ),"IRQ remains copied facts");
            declare Bytes:Wire.Byte_Array(0..Wire.Encoded_Length(Packet.Spans)-1);Status:Wire.Decode_Result;begin
               Wire.Encode(Packet,Bytes);Wire.Decode(Bytes,Decoded,Status);Check(Status.Status=Wire.Decoded and Decoded=Packet,"shared codec full round trip");end;
            History.Complete:=False;
            Payload.Build(Service,Run,Catalog_Epoch,History,Other_Packet,Result);
            Check(Result=Payload.Incomplete_Initializer_History and Other_Packet=Payload.Empty_Payload,"incomplete prefix cannot expose a payload");
            History.Complete:=True;
            Payload.Build(Service,Run,0,History,Other_Packet,Result);Check(Result=Payload.Invalid_Epoch and Other_Packet=Payload.Empty_Payload,"missing epoch exposes no partial payload");
            Payload.Build(Other,Run,Catalog_Epoch,History,Other_Packet,Result);Check(Result=Payload.Stale_Observation and Other_Packet=Payload.Empty_Payload,"foreign Core cannot reuse observation provenance");
            Check(Calls=Before and C.Retained_Results(Service)=0,"payload construction runs no AML and retains no roots");
         end;
         Check(Data(1..59)=Expected and Data(60..64)=[60..64=>0],"whole raw CRS copied with zeroed unused suffix");
         Check(not Observer.Pending(Run) and C.Retained_Results(Service)=0 and Observer.Is_Current(Run,Service),"successful observation releases roots and preserves exact identity");
      end if;
      Prior:=Calls;Observer.Describe(Run,Service,Device,Fuel,Data,Action);
      Check(Action=Observer.Already_Consumed and Calls=Prior and Data=[Data'Range=>0],"repeat attempt does no AML");
      Observer.Describe(Small_Run,Service,Device,Fuel,Small,Action);
      if Expected_STA/=8 then
         Check(Observer.Observe(Small_Run).Status=Observer.Capacity_Required and Observer.Observe(Small_Run).Required=59
           and Observer.Pending(Small_Run) and C.Retained_Results(Service)=1 and Small=[Small'Range=>0],"small buffer retains same complete result without truncation");
         Prior:=Release_Calls;Observer.Close(Small_Run,Other,Action);
         Check(Action=Observer.Wrong_Context and Observer.Pending(Small_Run) and Release_Calls=Prior
           and C.Retained_Results(Service)=1,"foreign Core cannot release retained resource result");
         Prior:=Calls;Observer.Copy_Resources(Small_Run,Service,Data,Action);
         Check(Action=Observer.Accepted and Calls=Prior and Observer.Observe(Small_Run).Status=Observer.Described
           and Data(1..59)=Expected and not Observer.Pending(Small_Run) and C.Retained_Results(Service)=0,"larger copy reuses result without AML or renewed fuel");
      end if;
      Quiet:=False;Prior:=Calls;Observer.Describe(Not_Quiet,Service,Device,Fuel,Data,Action);
      Check(Observer.Observe(Not_Quiet).Status=Observer.Hardware_Not_Quiet and Calls=Prior and Data=[Data'Range=>0],"active lower owner refuses before any AML");Quiet:=True;
      Observer.Describe(Limited_Run,Service,Device,0,Data,Action);
      Check(Observer.Observe(Limited_Run).Status=Observer.Budget_Exhausted and Calls=Prior and not Observer.Pending(Limited_Run),"zero budget refuses without AML");
      if Expected_STA/=8 then
         declare High:Firmware_Tables.Bytes(Positive'Last-58..Positive'Last);begin
            Observer.Describe(High_Run,Service,Device,Fuel,High,Action);
            Check(Observer.Observe(High_Run).Status=Observer.Described and High=Expected
              and not Observer.Pending(High_Run),"last valid array index preserves full raw resource copy without intermediate overflow");
         end;
         Check(Observer.Observe(Run).Charged>0,"actual successful AML charge recorded");
         Observer.Describe(Fuel_Run,Service,Device,Observer.Observe(Run).Charged-1,Data,Action);
         Check(Observer.Observe(Fuel_Run).Status=Observer.Budget_Exhausted and not Observer.Pending(Fuel_Run)
           and Data=[Data'Range=>0],"one less total fuel fails instead of renewing budget per method");
         Refuse_Release:=True;Prior:=Release_Calls;
         Observer.Describe(Cleanup_Run,Service,Device,Fuel,Data,Action);
         Check(Observer.Observe(Cleanup_Run).Status=Observer.Cleanup_Failed and Observer.Pending(Cleanup_Run)
           and Release_Calls=Prior+1 and C.Retained_Results(Service)=1 and Data=[Data'Range=>0],"failed release retains root without automatic repeated cleanup or publication");
         Refuse_Release:=False;Observer.Close(Cleanup_Run,Service,Action);
         Check(Action=Observer.Accepted and Observer.Observe(Cleanup_Run).Status=Observer.Closed
           and not Observer.Pending(Cleanup_Run) and C.Retained_Results(Service)=0,"explicit cleanup recovery retires root without AML replay");
      end if;
      declare Released:Boolean;begin C.Release_Storage(Other,Released);Check(Released,"foreign model Core retires independently");end;
   end;
   declare Released:Boolean;begin C.Release_Storage(Service,Released);Check(Released,"same Core storage retires after all roots");end;
   Ada.Text_IO.Put_Line("GPIO captured methods checks"&Checks'Image);
end GPIO_Methods_Tests;
