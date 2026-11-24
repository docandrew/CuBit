pragma Ada_2022;
with ACPI_Native_Power_Buttons;
with ACPI_Hosted_Heap;
with ACPI_Snapshot_Startup;with ACPI_Snapshot_Startup.With_Driver;
with Ada.Strings.Fixed;with Ada.Strings;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
with Interfaces;with AML_Execute;with Firmware_Tables;
with ACPI_Service;with ACPI_Requests;
with ACPI_Device_Initialization;with ACPI_Device_Policy;with ACPI_One_Service;
with ACPI_Native_Diagnostic_Output;with ACPI_Native_EC_Source;
with ACPI_Native_Endpoint;with ACPI_Native_Power_Entry;with ACPI_Endpoint;
with CuBit.Messages;with Endpoint_Test_Boundary;
with ACPI_Cached_Power_Codec;with ACPI_Cached_Power_Dispatch;with ACPI_Protocol;
with ACPI_Cached_Power_Values;with ACPI_Observation_Values;
with ACPI_Cached_Battery_Values;with ACPI_Battery_Status;
with ACPI_Battery_Observation_Protocol;
with ACPI_Native_Event_Domain;with ACPI_Native_Server;with ACPI_Native_Log;
with ACPI_Native_EC_Discovery;with ACPI_Native_GPE_Control;
with ACPI_SCI_Events;with ACPI_SCI_Registers;with ACPI_SCI_Quiesce;
with ACPI_Requests.With_Local_Maintenance;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Domain_Tests is
   use type ACPI_One_Service.Runtime.Status.Phase;
   package C renames ACPI_Service;package Q renames ACPI_Requests;package One renames ACPI_One_Service;
   package L renames ACPI_Snapshot_Startup;
   package D renames ACPI_Native_Event_Domain;package Log renames ACPI_Native_Log;
   package BV renames ACPI_Cached_Battery_Values;
   package CP renames ACPI_Cached_Power_Values;
   package BW renames ACPI_Battery_Observation_Protocol;
   use type BW.Subpage;
   use type BV.Entry_Status, BV.Acquisition_Reason, BV.Sample, BV.Entry_Value, BV.Entries, BV.View;
   use type BV.Information_Source, BV.Unit_Kind;
   use type ACPI_Cached_Power_Values.Availability, ACPI_Cached_Power_Values.Generation;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   use type L.Outcome,L.State_Access,L.R.S.Table_ID;
   use type Q.Outcome,One.Runtime.Status.Outcome;
   use type D.Phase,D.Failure,D.Start_Outcome,D.Drain_Outcome,ACPI_Native_Server.Owner_Outcome,ACPI_SCI_Events.Disposition;
   use type ACPI_Native_EC_Discovery.Outcome;
   use type C.Values.Access_Status,C.Namespace.Lookup_Status,AML_Execute.Execution_Status;
   package EC_Source renames ACPI_Native_EC_Source;
   use type EC_Source.Phase, EC_Source.Failure;
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
   Maximum_Fixture_Bytes : constant Positive := 16_384;
   Snapshot_Chunk_Bytes : constant Positive := 4096;
   Snapshot_Fixture_Deadline : constant L.R.Tick := 1000;
   function Read_Table return Firmware_Tables.Bytes is
      File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Maximum_Fixture_Bytes));Final:Ada.Streams.Stream_Element_Offset;
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
   package Loader is new L.With_Driver(Driver,Info,Describe,Snapshot_Chunk_Bytes,Copy,Clock);
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
   procedure Check_Battery_Wire (Value : BV.View);
   Last_Battery_Generation : ACPI_Cached_Power_Values.Generation := 0;
   procedure Check_Battery (Expected : BV.Entry_Status; Reason : BV.Acquisition_Reason;
      Capacity : Interfaces.Unsigned_32 := 0; Refreshed : Boolean := False) is
      First, Copy : BV.View;
      Calls : constant Natural := One.Eval_Calls;
      Reads : constant Natural := One.Reads;
      Writes : constant Natural := One.Writes;
      Lines : constant Natural := Log.Count;
   begin
      D.Read_Batteries (Owner.all, 100_000, First);
      Check (First.Status = ACPI_Cached_Power_Values.Available and First.Count = 1
        and First.Revision > 0 and First.Time.Known, "copied battery inventory has cache generation and time");
      Check (First.Items (1).Status = Expected and First.Items (1).Reason = Reason,
        "copied battery acquisition status/reason");
      if Refreshed then Check (First.Revision > Last_Battery_Generation, "battery refresh advances inventory generation"); end if;
      Last_Battery_Generation := First.Revision;
      if Expected = BV.Sampled then
         Check (First.Items (1).Value.Source = BV.Extended_Reported
           and First.Items (1).Value.Units = BV.Charge_And_Current,
           "battery information source and native charge/current units preserved");
         Check (First.Items (1).Value.Dynamic.Remaining_Capacity.Known
           and First.Items (1).Value.Dynamic.Remaining_Capacity.Value = Capacity
           and First.Items (1).Value.Last_Full_Capacity.Known
           and First.Items (1).Value.Last_Full_Capacity.Value = 40000,
           "copied current capacity and full capacity agree with actual AML");
      else Check (First.Items (1).Value = BV.Empty_Sample, "failed battery acquisition contains no previous capacity"); end if;
      Check ((for all Index in BV.Slot range 2 .. BV.Slot'Last => First.Items (Index) = BV.Empty_Entry),
        "unused battery inventory slots have canonical empty payloads");
      D.Read_Batteries (Owner.all, 100_000, Copy);
      Check (Copy = First, "battery reads do not consume or mutate cache samples");
      First.Items (1).Value.Dynamic.Remaining_Capacity.Value := 7;
      Check (First.Items (1).Value.Dynamic.Remaining_Capacity.Value = 7, "caller can edit its copied value");
      D.Read_Batteries (Owner.all, 100_000, First);
      Check (First = Copy, "caller edits to a copied battery value cannot alter the owned cache");
      Check (One.Eval_Calls = Calls and One.Reads = Reads and One.Writes = Writes and Log.Count = Lines,
        "cached battery reads perform no AML, register commands or logging");
      if ACPI_Native_Endpoint.Serving_State.Selected then Check_Battery_Wire (Copy); end if;
   end Check_Battery;
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
      if Method_Name = "BAD0" then Check_Battery (BV.Acquisition_Failed, BV.Status_Failed, Refreshed => True);
      elsif Method_Name = "BAD1" then Check_Battery (BV.Acquisition_Failed, BV.Information_Failed, Refreshed => True);
      elsif Method_Name = "GOOD" then Check_Battery (BV.Sampled, BV.No_Failure, 12345, True);
      elsif Method_Name = "GOD1" then Check_Battery (BV.Sampled, BV.No_Failure, 13000, True);
      end if;
   end Refresh_Battery;
   package Disconnect is new Q.With_Local_Maintenance(Region_Off);
   Region_Admission:Disconnect.Controller.Admission;
   use type Disconnect.Controller.Admission;
   subtype Scalar_Name is String(1..4);
   Selected_Scalar : Scalar_Name := "DREG";
   Observed_Scalar : Interfaces.Unsigned_64 := 0;
   procedure Observe_Scalar(Service:aliased in out C.State;Completed:out Boolean) is
      Found:C.Namespace.Lookup_Result;Status:C.Values.Access_Status;
      Value:C.Values.Result;Handle:C.Values.Value_Handle:=C.Values.No_Value;
   begin
      Completed:=False;C.Find_Object(Service,C.Namespace.Root,Selected_Scalar,Found,Status);
      if Status/=C.Values.Available or else Found.Status/=C.Namespace.Found then return;end if;
      C.Observe_Named_Value(Service,Found.Node,Value,Status);
      if Status=C.Values.Available and then Value.Status=AML_Execute.Returned then
         Observed_Scalar:=Value.Number;Completed:=True;
      elsif Value.Status in AML_Execute.Object_Returned|AML_Execute.Reference_Returned|AML_Execute.Metadata_Returned then
         Handle:=Value.Handle;C.Release_Result(Service,Handle,Status);
      end if;
   end Observe_Scalar;
   package Scalar_Read is new Q.With_Local_Maintenance(Observe_Scalar);
   procedure Read_Scalar(Name:Scalar_Name;Value:out Interfaces.Unsigned_64) is
      Admission:Scalar_Read.Controller.Admission;
      use type Scalar_Read.Controller.Admission;
   begin
      Selected_Scalar:=Name;Observed_Scalar:=0;
      One.Runtime_Access.Begin_Event(Owner.all,One.Now+1000,Runtime_Result);
      Check(Runtime_Result=One.Runtime.Status.Event_Opened,"scalar witness finite window");
      Scalar_Read.Advance(Owner.all,Admission,Accepted);
      Check(Admission=Scalar_Read.Controller.Admitted and Accepted,"same-Core scalar witness");
      One.Runtime_Access.End_Event(Owner.all,True,Runtime_Result);
      Check(Runtime_Result=One.Runtime.Status.Event_Completed,"scalar witness window retires");
      Value:=Observed_Scalar;
   end Read_Scalar;
   function Latest_HID(Descriptor:Interfaces.Unsigned_64) return Boolean is
      Marker:constant String:="acpi: HID-I2C candidate 1 ";
      Expected:constant String:="descriptor="&Ada.Strings.Fixed.Trim(Descriptor'Image,Ada.Strings.Both)&" gpio=42";
   begin
      for I in reverse 1..Log.Count loop
         if Ada.Strings.Fixed.Index(Log.Lines(I)(1..Log.Lengths(I)),Marker)>0 then
            return Ada.Strings.Fixed.Index(Log.Lines(I)(1..Log.Lengths(I)),Expected)>0;
         end if;
      end loop;
      return False;
   end Latest_HID;
   package Wire is new ACPI_Cached_Power_Codec (Q.Read_Metrics, 256, 512);
   Read_Calls : Natural := 0;
   Initial_Source_Generation : ACPI_Cached_Power_Values.Generation := 0;
   procedure Read_For_Wire (Server : Q.State; Maximum_Age : ACPI_Cached_Power_Values.Microseconds;
      Value : out ACPI_Cached_Power_Values.Snapshot) is
   begin Read_Calls:=Read_Calls+1;D.Read_Power(Server,Maximum_Age,Value);end Read_For_Wire;
   package Wire_Owner is new ACPI_Cached_Power_Dispatch (Q.State,Wire,Read_For_Wire);
   use type Wire_Owner.Outcome,Wire.Reply_Words;
   procedure Check_Wire_Unavailable (Why : String) is
      Request : Wire.Packet := (Label=>Q.Read_Metrics,Length=>Wire.Request_Length,
         Data=>[0=>256,1=>100_000,others=>0],others=><>);
      Result : Wire_Owner.Reply;
      Before : Natural := Read_Calls;
   begin
      Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.OK and Result.Data=Wire.Empty_Reply and Read_Calls=Before+1,
         "wire lid read returns unavailable without refresh: "&Why);
      Request.Data(0):=512;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.OK and Result.Data=Wire.Empty_Reply and Read_Calls=Before+2,
         "wire AC read returns unavailable without refresh: "&Why);
      Before:=Read_Calls;Wire_Owner.Dispatch(Owner.all,0,Request,Result,True);
      Check(Result.Status=Wire_Owner.Denied and Result.Data=Wire.Empty_Reply and Read_Calls=Before,
         "unclassified received stamp cannot invoke cache callback");
      Request.Length:=3;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.Malformed and Result.Data=Wire.Empty_Reply and Read_Calls=Before,
         "bad framing cannot invoke cache callback");
      Request.Length:=Wire.Request_Length;Request.Data(0):=0;
      Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.Not_Handled and Result.Data=Wire.Empty_Reply and Read_Calls=Before,
         "existing metrics page stays with original endpoint");
   end Check_Wire_Unavailable;
   package NE renames ACPI_Native_Endpoint;
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_16,Interfaces.Unsigned_8,M.Protocol_Flags,M.MessageWords;
   function Native_Request (Page : Interfaces.Unsigned_64; Subpage : Interfaces.Unsigned_64 := 0;
      Generation : Interfaces.Unsigned_64 := 0) return M.Message is
      Item : M.Message := M.NULL_MESSAGE;
   begin
      Item.tag.label:=Q.Read_Metrics;Item.tag.length:=4;
      Item.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
      Item.words:=[Page,100_000,Subpage,Generation];return Item;
   end Native_Request;
   procedure Native_Call (Request : M.Message; Reply : out M.Message; Admission : Boolean := True) is
   begin
      NE.Dispatch_With_Admission(Owner.all,M.From_Word(Interfaces.Unsigned_64'Last),Request,Reply,Admission);
      Check(NE.Reply_Now and Reply.tag.length=4 and Reply.tag.protocol=0
         and M.Flag_Byte(Reply.tag)=0 and Reply.tag.reserved=0,
         "native cached reply has canonical complete synchronous framing");
      Check((for all Word of Reply.words=>Word<=Q.Max_Revision),"native reply words fit CCL signed integers");
   end Native_Call;
   procedure Check_Battery_Wire (Value : BV.View) is
      Generation : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Value.Revision);
      Request : M.Message := Native_Request (NE.Metrics_Battery_Page);
      Reply : M.Message;
      Expected : M.MessageWords;
      Calls : constant Natural := One.Eval_Calls;
      Reads : constant Natural := One.Reads;
      Writes : constant Natural := One.Writes;
      Lines : constant Natural := Log.Count;
      Passes : constant Natural := Endpoint_Test_Boundary.Pass_Through_Calls;
      Warnings : constant Natural := Endpoint_Test_Boundary.Warnings;
      Clocks : Natural;
      function Quantity_Word (Known : Boolean; Raw : Interfaces.Unsigned_32) return Interfaces.Unsigned_64 is
        (if Known then Interfaces.Unsigned_64 (Raw) else BW.Unknown_Quantity);
   begin
      Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_OK and Reply.words =
        [Generation, CP.Availability'Pos (Value.Status), Interfaces.Unsigned_64 (Value.Count), 0],
        "native battery header is the actual copied inventory generation/status/count");
      if Generation > 0 then
         Request.words (3) := Generation;
         for Slot in 0 .. Value.Count loop
            for Page in BW.Subpage loop
               if (Slot = 0 and then Page in BW.Inventory | BW.Sample_Time)
                 or else (Slot > 0 and then Page in BW.Entry_Summary .. BW.Dynamic_Voltage) then
                  Request.words (2) := Interfaces.Unsigned_64 (Slot) * BW.Selector_Stride
                    + Interfaces.Unsigned_64 (BW.Subpage'Pos (Page));
                  Expected := [Generation, CP.Availability'Pos (Value.Status), 0, 0];
                  if Page = BW.Inventory then Expected (2) := Interfaces.Unsigned_64 (Value.Count);
                  elsif Page = BW.Sample_Time then
                     Expected (1) := Boolean'Pos (Value.Time.Known);
                     if Value.Time.Known then
                        Expected (2) := Interfaces.Unsigned_64 (Value.Time.Value) and Interfaces.Unsigned_64 (Interfaces.Unsigned_32'Last);
                        Expected (3) := Interfaces.Shift_Right (Interfaces.Unsigned_64 (Value.Time.Value), Interfaces.Unsigned_32'Size);
                     end if;
                  else
                     declare Item : BV.Entry_Value renames Value.Items (BV.Slot (Slot)); begin
                        if Page = BW.Entry_Summary then
                           Expected (2) := BV.Entry_Status'Pos (Item.Status);
                           Expected (3) := BV.Acquisition_Reason'Pos (Item.Reason);
                        elsif Item.Status /= BV.Sampled then
                           Expected (1) := CP.Availability'Pos (if Item.Status = BV.Known_Absent then CP.Missing else CP.Failure);
                        else
                           case Page is
                              when BW.Information =>
                                 Expected (2) := BV.Information_Source'Pos (Item.Value.Source);
                                 Expected (3) := BV.Unit_Kind'Pos (Item.Value.Units);
                              when BW.Capacities =>
                                 Expected (2) := Quantity_Word (Item.Value.Design_Capacity.Known, Item.Value.Design_Capacity.Value);
                                 Expected (3) := Quantity_Word (Item.Value.Last_Full_Capacity.Known, Item.Value.Last_Full_Capacity.Value);
                              when BW.Design_Voltage =>
                                 Expected (2) := Quantity_Word (Item.Value.Design_Voltage.Known, Item.Value.Design_Voltage.Value);
                              when BW.Dynamic_State =>
                                 Expected (2) := Interfaces.Unsigned_64 (ACPI_Battery_Status.Direction'Pos (Item.Value.Dynamic.Flow));
                                 Expected (3) := (if Item.Value.Dynamic.Critical then 1 else 0)
                                   + (if Item.Value.Dynamic.Charge_Limiting then 2 else 0);
                              when BW.Rate_And_Capacity =>
                                 Expected (2) := Quantity_Word (Item.Value.Dynamic.Rate.Known, Item.Value.Dynamic.Rate.Value);
                                 Expected (3) := Quantity_Word (Item.Value.Dynamic.Remaining_Capacity.Known, Item.Value.Dynamic.Remaining_Capacity.Value);
                              when BW.Dynamic_Voltage =>
                                 Expected (2) := Quantity_Word (Item.Value.Dynamic.Voltage.Known, Item.Value.Dynamic.Voltage.Value);
                                 Expected (3) := Interfaces.Unsigned_64 (Item.Value.Dynamic.Reserved_State_Bits);
                              when BW.Inventory | BW.Sample_Time | BW.Entry_Summary => null;
                           end case;
                        end if;
                     end;
                  end if;
                  Native_Call (Request, Reply);
                  Check (Reply.tag.label = ACPI_Endpoint.Reply_OK and Reply.words = Expected,
                    "native battery subpage matches actual cache slot/source/units/measurement flags");
               end if;
            end loop;
         end loop;
         Request.words (2) := 0; Request.words (3) := Generation + 1;
         Native_Call (Request, Reply);
         Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words = [Q.Outcome'Pos (Q.Stale),0,0,0],
           "native battery mismatched generation rejects mixed inventory pages");
         if Value.Status = CP.Available and Value.Count < BV.Maximum_Batteries then
            Request.words (2) := Interfaces.Unsigned_64 (Value.Count + 1) * BW.Selector_Stride + 2;
            Request.words (3) := Generation; Native_Call (Request, Reply);
            Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words = [Q.Outcome'Pos (Q.Index_Out_Of_Range),0,0,0],
              "native available inventory rejects out-of-count slot");
         end if;
      end if;
      Clocks := One.Clock_Observations;
      Request := Native_Request (NE.Metrics_Battery_Page);
      Request.authorityTag := 0; Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words = [Q.Outcome'Pos (Q.Denied),0,0,0],
        "native battery query requires received observer stamp, not sender identity");
      Request := Native_Request (NE.Metrics_Battery_Page); Request.tag.length := 3; Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words (0) = Q.Outcome'Pos (Q.Malformed),
        "native battery frame length exact");
      Request := Native_Request (NE.Metrics_Battery_Page); Request.tag.protocol := 1; Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words (0) = Q.Outcome'Pos (Q.Malformed),
        "native battery rejects received protocol flags");
      Request := Native_Request (NE.Metrics_Battery_Page); Request.words (2) := 9; Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words (0) = Q.Outcome'Pos (Q.Malformed),
        "native battery rejects noncanonical selector gaps");
      Request := Native_Request (NE.Metrics_Battery_Page); Request.words (3) := Interfaces.Unsigned_64'Last;
      Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_Error and Reply.words (0) = Q.Outcome'Pos (Q.Malformed),
        "native battery never narrows an oversized generation");
      Check (One.Clock_Observations = Clocks, "rejected native battery frames do not read a cache clock");
      Request := Native_Request (NE.Metrics_Battery_Page); Native_Call (Request, Reply, False);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_OK and Reply.words = [0,0,0,0] and One.Clock_Observations = Clocks,
        "closed native scheduler admission cannot read battery cache");
      ACPI_Native_Power_Entry.Blocked_State := True; Native_Call (Request, Reply);
      ACPI_Native_Power_Entry.Blocked_State := False;
      Check (Reply.tag.label = ACPI_Endpoint.Reply_OK and Reply.words = [0,0,0,0] and One.Clock_Observations = Clocks,
        "retained power operation blocks battery cache query without reader call");
      Check (One.Eval_Calls = Calls and One.Reads = Reads and One.Writes = Writes and Log.Count = Lines
        and Endpoint_Test_Boundary.Pass_Through_Calls = Passes and Endpoint_Test_Boundary.Warnings = Warnings,
        "native battery queries execute no AML, IO, logging, power/passive handler or warnings");
   end Check_Battery_Wire;
   procedure Check_Native_Unavailable (Why : String) is
      Request : M.Message:=Native_Request(NE.Metrics_Lid_Page);
      Reply : M.Message;
      Clocks : constant Natural:=One.Clock_Observations;
      Passes : constant Natural:=Endpoint_Test_Boundary.Pass_Through_Calls;
      Warnings : constant Natural:=Endpoint_Test_Boundary.Warnings;
   begin
      Native_Call(Request,Reply);
      Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[0,0,0,0],
         "native lid unavailable through actual endpoint: "&Why);
      Request.words(0):=NE.Metrics_Source_Page;Native_Call(Request,Reply);
      Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[0,0,0,0],
         "native AC unavailable through actual endpoint: "&Why);
      Request.words (0) := NE.Metrics_Battery_Page; Native_Call (Request, Reply);
      Check (Reply.tag.label = ACPI_Endpoint.Reply_OK and Reply.words = [0,0,0,0],
        "native battery unavailable through actual endpoint: " & Why);
      Check(One.Clock_Observations=Clocks and Endpoint_Test_Boundary.Pass_Through_Calls=Passes
         and Endpoint_Test_Boundary.Warnings=Warnings,
         "cached unavailable request opens no clock read, power handler or diagnostic path");
   end Check_Native_Unavailable;
   procedure Check_Cached_Unavailable (Why : String);
   procedure Overlapping_Events is
      Before_Evaluations:constant Natural:=One.Eval_Calls;
      Before_Reads:constant Natural:=One.Reads;
      Before_Writes:constant Natural:=One.Writes;
      Before_Clears:constant Natural:=ACPI_SCI_Events.General_Clears;
      Before_Enables:constant Natural:=ACPI_SCI_Events.General_Enables;
      Count,Descriptor:Interfaces.Unsigned_64;
   begin
      Check(D.Observe.General_GPEs=1 and D.Observe.EC_Armed,"two independently enrolled event sources");
      Log.Clear;
      D.GPE_Event(ACPI_SCI_Events.General_Source,Disposition);
      Check(Disposition=ACPI_SCI_Events.Handled and not ACPI_SCI_Events.General_Enabled and ACPI_SCI_Events.Enabled,
         "general callback masks only its source");
      D.GPE_Event(ACPI_SCI_Events.EC_Source,Disposition);
      Check(Disposition=ACPI_SCI_Events.Handled and not ACPI_SCI_Events.General_Enabled and not ACPI_SCI_Events.Enabled,
         "EC callback preserves the independently masked general source");
      Check(Log.Count=0 and One.Eval_Calls=Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,
         "overlapping callbacks perform no AML, commands or fact logging");
      Run_To_Idle;
      Check(Log.Saw("query Q58") and Log.Saw("query Q59 UNMAPPED"),"EC queue drains alongside general source");
      Check(Log.Saw("lid CLOSED") and Log.Saw("AC OFFLINE") and Log.Saw("remaining=15000 full=40000"),
         "EC Notify observations progress without general-source starvation");
      Check(ACPI_SCI_Events.General_Clears=Before_Clears+1 and ACPI_SCI_Events.General_Enables=Before_Enables+1
         and ACPI_SCI_Events.General_Enabled and ACPI_SCI_Events.Enabled,"both sources rearm after attributable completion");
      Check(D.Diagnostics_Quiet(Owner.all) and not Q.Notifications_Pending(Owner.all)
         and not Q.Retained_Results_Pending(Owner.all),"all original receipts/results retire");
      Read_Scalar("GCNT",Count);Check(Count=1,"actual general AML handler executes exactly once");
      Read_Scalar("DREG",Descriptor);
      Check(Descriptor in 33|34 and Latest_HID(Descriptor),"last HID observation agrees with actual final namespace value");
      D.Begin_Stop;Check_Cached_Unavailable("stopping");ACPI_SCI_Events.Armed_State:=False;
      D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
      D.Close(Owner.all,Drain_Result);
      Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"overlapping event owners retire without replay");
      Read_Scalar("GCNT",Count);Check(Count=1,"shutdown does not replay general AML");
   end Overlapping_Events;
   procedure General_Source_Failure is
      Before_Evaluations:constant Natural:=One.Eval_Calls;
      Before_Reads:constant Natural:=One.Reads;
      Before_Writes:constant Natural:=One.Writes;
      Before_Clears:constant Natural:=ACPI_SCI_Events.General_Clears;
      Before_Enables:constant Natural:=ACPI_SCI_Events.General_Enables;
      Count:Interfaces.Unsigned_64;
   begin
      Log.Clear;
      if Mode="general-mask-failed" then ACPI_SCI_Events.Mask_Refused:=True;end if;
      D.GPE_Event(ACPI_SCI_Events.General_Source,Disposition);
      Check(Disposition=(if Mode="general-mask-failed" then ACPI_SCI_Events.Unbound else ACPI_SCI_Events.Handled),
         "capture acknowledges only a completed source mask");
      Check(Log.Count=0 and One.Eval_Calls=Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,
         "failed-source callback performs no AML, command or logging");
      if Mode="general-revoked" then ACPI_Native_GPE_Control.Exclusive_State:=False;
      elsif Mode="general-clear-failed" then ACPI_SCI_Events.Clear_Refused:=True;
      elsif Mode="general-enable-failed" then ACPI_SCI_Events.Enable_Refused:=True;end if;
      for Step in 1..16 loop
         D.Advance(Owner.all,Owner_Result);
         exit when Owner_Result=ACPI_Native_Server.Owner_Refused;
      end loop;
      Check(Owner_Result=ACPI_Native_Server.Owner_Refused and D.Observe.Stage=D.Failed
         and D.Observe.Why=D.GPE_Work_Refused,"source failure refuses the live event domain");
      Check(ACPI_SCI_Events.General_Enables=Before_Enables+(if Mode="general-enable-failed" then 1 else 0),
         "failed source never retries or advances past its failed rearm step");
      Check(ACPI_SCI_Events.General_Clears=Before_Clears+(if Mode in "general-clear-failed"|"general-enable-failed" then 1 else 0),
         "clear occurs only after a completed handler and receipt retirement");
      if Mode in "general-revoked"|"general-mask-failed" then
         Check(One.Eval_Calls=Before_Evaluations,"lost admission prevents queued AML handler execution");
      else Check(One.Eval_Calls>Before_Evaluations,"post-handler IO failure does not replay completed AML");end if;
      declare Calls:constant Natural:=One.Eval_Calls;Enables:constant Natural:=ACPI_SCI_Events.General_Enables;begin
         D.GPE_Event(ACPI_SCI_Events.General_Source,Disposition);
         D.Advance(Owner.all,Owner_Result);
         Check(Disposition=ACPI_SCI_Events.Unbound and Owner_Result=ACPI_Native_Server.Owner_Refused
            and One.Eval_Calls=Calls and ACPI_SCI_Events.General_Enables=Enables,
            "failed domain latches refusal without accepting callbacks or replaying AML");
      end;
      Check(D.Diagnostics_Quiet(Owner.all) and not Q.Notifications_Pending(Owner.all)
         and not Q.Retained_Results_Pending(Owner.all),"failure leaves no result, window or Notify obligation");
      ACPI_SCI_Events.Mask_Refused:=False;
      D.Begin_Stop;Check_Cached_Unavailable("stopping");ACPI_SCI_Events.Armed_State:=False;
      D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;D.Close(Owner.all,Drain_Result);
      Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"failed source retires after confirmed disarm");
      Read_Scalar("GCNT",Count);
      Check(Count=(if Mode in "general-revoked"|"general-mask-failed" then 0 else 1),
         "actual handler counter proves no shutdown replay or pre-admission execution");
   end General_Source_Failure;
   package OV renames ACPI_Observation_Values;
   use type CP.Lid_View,CP.Source_View,CP.Microseconds;
   use type OV.Lid_State,OV.Power_Source_State,OV.Lid_Sample,OV.Power_Source_Sample;
   procedure Check_Cached_Unavailable (Why : String) is
      Value : CP.Snapshot;
      Batteries : BV.View;
      Calls : constant Natural := One.Eval_Calls;
      Reads : constant Natural := One.Reads;
      Writes : constant Natural := One.Writes;
      Lines : constant Natural := Log.Count;
   begin
      Check_Native_Unavailable(Why);
      Check_Wire_Unavailable(Why);
      D.Read_Power (Owner.all, 100_000, Value);
      D.Read_Batteries (Owner.all, 100_000, Batteries);
      Check (Batteries.Status = CP.Not_Ready and Batteries.Count = 0 and Batteries.Items = BV.Empty_Entries,
        "unsettled/closed battery inventory cannot expose stale payloads: " & Why);
      Check (Value.Lid.Status=CP.Not_Ready and Value.Source.Status=CP.Not_Ready
        and Value.Lid.Value=OV.Empty_Lid_Sample and Value.Source.Value=OV.Empty_Power_Source_Sample,
        "unavailable cached read clears payload: "&Why);
      Check (One.Eval_Calls=Calls and One.Reads=Reads and One.Writes=Writes and Log.Count=Lines,
        "cached refusal performs no AML, register IO or logging: "&Why);
   end Check_Cached_Unavailable;
   procedure Check_Cached_Initial is
      First,Copy,Value : CP.Snapshot;
      Batteries : BV.View;
      Saved : constant Interfaces.Unsigned_64 := One.Now;
      Calls : constant Natural := One.Eval_Calls;
      Reads : constant Natural := One.Reads;
      Writes : constant Natural := One.Writes;
      Lines : constant Natural := Log.Count;
   begin
      Check_Battery (BV.Sampled, BV.No_Failure, 20000);
      D.Read_Power (Owner.all, 100_000, First);
      Initial_Source_Generation:=First.Source.Revision;
      Check (First.Lid.Status=CP.Available and First.Source.Status=CP.Available
        and First.Lid.Value.State=OV.Open and First.Lid.Value.Raw=1
        and First.Source.Value.State=OV.Online and First.Source.Value.Raw=1,
        "typed cached lid and AC samples from the actual live consumer instances");
      declare
         Request : Wire.Packet := (Label=>Q.Read_Metrics,Length=>Wire.Request_Length,
            Data=>[0=>256,1=>100_000,others=>0],others=><>);
         Result : Wire_Owner.Reply;
         Before : constant Natural := Read_Calls;
      begin
         Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.OK and Result.Data=[Interfaces.Unsigned_64(First.Lid.Revision),1,2,
            Boolean'Pos(First.Lid.Time.Known)],"wire lid sample matches the real live cache generation and timestamp");
         Request.Data(0):=512;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.OK and Result.Data=[Interfaces.Unsigned_64(First.Source.Revision),1,2,
            Boolean'Pos(First.Source.Time.Known)],"wire AC sample matches the same live owner");
         Check(Read_Calls=Before+2,"one bounded cache callback per valid request");
         Request.Data(2):=Wire.Time_Page;Request.Data(3):=Interfaces.Unsigned_64(First.Source.Revision);
         Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.OK and Result.Data(1)=1
            and (Result.Data(2) or Interfaces.Shift_Left(Result.Data(3),32))=Interfaces.Unsigned_64(First.Source.Time.Value),
            "generation-checked timestamp page preserves actual full cache timestamp");
         Request.Data(2):=Wire.Raw_Page;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.OK and Result.Data=[Interfaces.Unsigned_64(First.Source.Revision),1,1,0],
            "generation-checked raw page returns actual AC value");
         Request.Data(3):=Request.Data(3)+1;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.Stale and Result.Data=Wire.Empty_Reply,"mismatched generation cannot return mixed data");
         Request.Data(3):=0;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         Check(Result.Status=Wire_Owner.Malformed and Result.Data=Wire.Empty_Reply,"auxiliary page requires a generation token");
         Request.Data(2):=Wire.Summary_Page;

         One.Read_Clock_Available:=False;
         Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
         One.Read_Clock_Available:=True;
         Check(Result.Status=Wire_Owner.OK and Result.Data(1)=5 and Result.Data(2)=0,
            "unknown clock remains distinct on wire with no descriptive state");
         One.Now:=Saved+1000;Request.Data(1):=1;
         Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);One.Now:=Saved;
         Check(Result.Status=Wire_Owner.OK and Result.Data(1)=4 and Result.Data(2)=0,
            "expired wire query does not refresh or return stale AC state");
         Request.Data(2):=Wire.Raw_Page;Request.Data(3):=Interfaces.Unsigned_64(First.Source.Revision);
         One.Now:=Saved+1000;Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);One.Now:=Saved;
         Check(Result.Status=Wire_Owner.OK and Result.Data=[Interfaces.Unsigned_64(First.Source.Revision),4,0,0],
            "same-generation expired raw read carries current availability and empty payload");
      end;
      declare
         Request : M.Message:=Native_Request(NE.Metrics_Source_Page);
         Reply : M.Message;
         Clocks : Natural:=One.Clock_Observations;
         Passes : constant Natural:=Endpoint_Test_Boundary.Pass_Through_Calls;
         Warnings : constant Natural:=Endpoint_Test_Boundary.Warnings;
         Selected : Boolean;
      begin
         Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[0,0,0,0]
            and One.Clock_Observations=Clocks,"unselected serving mode cannot read a live cache");
         NE.Select_Runtime(True,Selected);Check(Selected,"actual endpoint runtime choice");
         Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[Interfaces.Unsigned_64(First.Source.Revision),1,2,1]
            and One.Clock_Observations=Clocks+1,"actual received-message adapter reaches the same AC cache once");
         Request.words(0):=NE.Metrics_Lid_Page;Native_Call(Request,Reply);
         Check(Reply.words=[Interfaces.Unsigned_64(First.Lid.Revision),1,2,1],"actual endpoint lid summary");
         Request:=Native_Request(NE.Metrics_Source_Page,Wire.Time_Page,Interfaces.Unsigned_64(First.Source.Revision));
         Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words(1)=1
            and (Reply.words(2) or Interfaces.Shift_Left(Reply.words(3),Interfaces.Unsigned_32'Size))=
               Interfaces.Unsigned_64(First.Source.Time.Value),"actual endpoint lossless timestamp page");
         Request.words(2):=Wire.Raw_Page;Native_Call(Request,Reply);
         Check(Reply.words=[Interfaces.Unsigned_64(First.Source.Revision),1,1,0],"actual endpoint raw AC observation");
         Request.words(3):=Request.words(3)+1;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words=[Q.Outcome'Pos(Q.Stale),0,0,0],
            "native stale generation maps to existing bounded error reply");
         Request:=Native_Request(NE.Metrics_Source_Page);Clocks:=One.Clock_Observations;
         Request.authorityTag:=0;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words=[Q.Outcome'Pos(Q.Denied),0,0,0],
            "high opaque sender identity does not substitute for received observer stamp");
         Request.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
         Request.tag.protocol:=1;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words=[Q.Outcome'Pos(Q.Malformed),0,0,0],
            "actual protocol flag extraction reaches strict cache decoder");
         Request.tag.protocol:=0;Request.tag.ipc.Accepts_Capability:=True;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words(0)=Q.Outcome'Pos(Q.Malformed),
            "actual IPC flag extraction reaches strict cache decoder");
         Request.tag.ipc:=(others=>False);Request.words(3):=Interfaces.Unsigned_64'Last;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words(0)=Q.Outcome'Pos(Q.Malformed),
            "unrepresentable expected generation never converts or reads a cache");
         Check(One.Clock_Observations=Clocks,"rejected native requests never invoke the cache clock");
         Request:=Native_Request(NE.Metrics_Source_Page);Native_Call(Request,Reply,False);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[0,0,0,0]
            and One.Clock_Observations=Clocks,"closed local scheduler admission suppresses cache reads");
         ACPI_Native_Power_Entry.Blocked_State:=True;Native_Call(Request,Reply);ACPI_Native_Power_Entry.Blocked_State:=False;
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=[0,0,0,0]
            and One.Clock_Observations=Clocks,"power retention blocks native cache reader");
         Check(Endpoint_Test_Boundary.Pass_Through_Calls=Passes and Endpoint_Test_Boundary.Warnings=Warnings,
            "recognized native cache queries never enter power/passive handlers or warnings");
         Request:=Native_Request(0);Request.words(1):=0;Native_Call(Request,Reply);
         Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words(0)=Q.Revision(Owner.all)
            and Reply.words(1)=Q.Phase'Enum_Rep(Q.Complete),"existing core metrics page still routes through original observer handler");
      end;
      Check (First.Lid.Revision>0 and First.Source.Revision>0
        and First.Lid.Time.Known and First.Source.Time.Known,"cached generations and timestamps retained");
      D.Read_Power (Owner.all, 100_000, Copy);
      Check (Copy.Lid=First.Lid and Copy.Source=First.Source,"repeated reads preserve copied cache metadata");
      One.Read_Clock_Available:=False;D.Read_Power (Owner.all,100_000,Value);One.Read_Clock_Available:=True;
      One.Read_Clock_Available := False;
      D.Read_Batteries (Owner.all, 100_000, Batteries); One.Read_Clock_Available := True;
      Check (Batteries.Status = CP.Clock_Unknown and Batteries.Count = 0 and Batteries.Items = BV.Empty_Entries,
        "unknown trusted clock clears battery inventory payload");
      Check (Value.Lid.Status=CP.Clock_Unknown and Value.Source.Status=CP.Clock_Unknown
        and Value.Lid.Value=OV.Empty_Lid_Sample and Value.Source.Value=OV.Empty_Power_Source_Sample,
        "unknown trusted clock cannot certify a live sample");
      One.Now:=0;D.Read_Power (Owner.all,100_000,Value);
      D.Read_Batteries (Owner.all, 100_000, Batteries);
      Check (Batteries.Status = CP.Clock_Regressed and Batteries.Count = 0 and Batteries.Items = BV.Empty_Entries,
        "regressed clock clears battery inventory payload");
      Check (Value.Lid.Status=CP.Clock_Regressed and Value.Source.Status=CP.Clock_Regressed
        and Value.Lid.Value=OV.Empty_Lid_Sample and Value.Source.Value=OV.Empty_Power_Source_Sample,
        "regressing trusted clock returns no descriptive value");
      One.Now:=Saved+1000;D.Read_Power (Owner.all,1,Value);
      D.Read_Batteries (Owner.all, 1, Batteries);
      Check (Batteries.Status = CP.Too_Old and Batteries.Count = 0 and Batteries.Items = BV.Empty_Entries,
        "expired battery inventory never returns previous capacity");
      Check (Value.Lid.Status=CP.Too_Old and Value.Source.Status=CP.Too_Old
        and Value.Lid.Value=OV.Empty_Lid_Sample and Value.Source.Value=OV.Empty_Power_Source_Sample,
        "caller maximum age filters cached samples without refreshing them");
      One.Now:=Interfaces.Unsigned_64'Last;D.Read_Power (Owner.all,CP.Microseconds'Last,Value);
      Check (Value.Lid.Status=CP.Available and Value.Source.Status=CP.Available,
        "full64bit age and clock values remain representable without overflow");
      One.Now:=Saved;D.Read_Power(Owner.all,100_000,Value);
      D.Read_Batteries (Owner.all, 100_000, Batteries); Check_Battery_Wire (Batteries);
      Check(Value.Lid=First.Lid and Value.Source=First.Source,"age/clock queries never mutate or consume samples");
      Check(One.Eval_Calls=Calls and One.Reads=Reads and One.Writes=Writes and Log.Count=Lines,
        "all cached queries perform no AML, hardware IO or logging");
   end Check_Cached_Initial;
   procedure Check_Wire_Refresh is
      Request : Wire.Packet := (Label=>Q.Read_Metrics,Length=>Wire.Request_Length,
         Data=>[512,100_000,Wire.Time_Page,Interfaces.Unsigned_64(Initial_Source_Generation)],others=><>);
      Result : Wire_Owner.Reply;
      Calls : constant Natural:=One.Eval_Calls;
      Reads : constant Natural:=One.Reads;
      Writes : constant Natural:=One.Writes;
      Lines : constant Natural:=Log.Count;
   begin
      Check_Battery (BV.Sampled, BV.No_Failure, 15000, True);
      Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.Stale and Result.Data=Wire.Empty_Reply,
         "actual EC Notify refresh between subpages rejects the old generation");
      Request.Data(2):=Wire.Summary_Page;Request.Data(3):=0;
      Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.OK and Result.Data(0)>Interfaces.Unsigned_64(Initial_Source_Generation)
         and Result.Data(1)=1 and Result.Data(2)=1,"new summary observes refreshed offline state and generation");
      Request.Data(2):=Wire.Raw_Page;Request.Data(3):=Result.Data(0);
      Wire_Owner.Dispatch(Owner.all,ACPI_Protocol.Observer_Tag,Request,Result,True);
      Check(Result.Status=Wire_Owner.OK and Result.Data(1)=1 and Result.Data(2)=0 and Result.Data(3)=0,
         "new-generation raw read returns actual offline zero without replay");
      Check(One.Eval_Calls=Calls and One.Reads=Reads and One.Writes=Writes and Log.Count=Lines,
         "stale and coherent subpage queries perform no AML, register IO or logging");
   end Check_Wire_Refresh;
   procedure General_Event is
      Before_Evaluations : constant Natural := One.Eval_Calls;
      Before_Reads : constant Natural := One.Reads;
      Before_Writes : constant Natural := One.Writes;
      Before_Clears : constant Natural := ACPI_SCI_Events.General_Clears;
      Before_Enables : constant Natural := ACPI_SCI_Events.General_Enables;
   begin
      Check(D.Observe.General_GPEs=1,"exactly one independently modeled general source");
      Log.Clear;
      D.GPE_Event(ACPI_SCI_Events.General_Source,Disposition);
      Check(Disposition=ACPI_SCI_Events.Handled and not ACPI_SCI_Events.General_Enabled,
         "general callback masks and copies");
      Check(Log.Count=0 and One.Eval_Calls=Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,
         "general callback runs no AML, EC commands or fact logging");
      Check_Cached_Unavailable("captured general GPE before handler Notify");
      Run_To_Idle;
      Check(Log.Saw("descriptor=34 gpio=42") and not Log.Saw("descriptor=32 gpio=42"),
         "actual L09 AML mutation and Notify replace prior HID observation");
      Check(One.Eval_Calls>Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,
         "general handler uses same AML owner without EC transaction");
      Check(ACPI_SCI_Events.General_Clears=Before_Clears+1 and ACPI_SCI_Events.General_Enables=Before_Enables+1
         and ACPI_SCI_Events.General_Enabled,"successful level handler clears/rearms once");
      Check(D.Diagnostics_Quiet(Owner.all) and not Q.Notifications_Pending(Owner.all)
         and not Q.Retained_Results_Pending(Owner.all),"general result and Notify fully retired");
   end General_Event;
   procedure Failed_Identity is
   begin
      Check(D.Observe.Consumers_Started and not D.Observe.EC_Armed,"failed EC identity confers no EC source authority");
      Run_To_Idle;
      Check(D.Observe.Stage=D.Live and One.Runtime.Current=One.Runtime.Status.Idle,
         "ordinary identity error isolates affected families without stopping the shared owner");
      Check(ACPI_Native_Diagnostic_Output.Saw("isolated=TRUE"),
         "isolated identity failure retains typed consumer diagnostics: "&ACPI_Native_Diagnostic_Output.Latest);
      Check(One.Reads=0 and One.Writes=0 and ACPI_SCI_Events.Enables=0,
         "failed EC identity confers no EC IO or enabled source");
      Check(Log.Saw("HID-I2C candidate"),
         "ordinary unrelated identity failure preserves independently resolved HID candidate");
      declare Before : constant Natural := One.Eval_Calls;begin
         D.GPE_Event(ACPI_SCI_Events.EC_Source,Disposition);
         Check(Disposition=ACPI_SCI_Events.Unbound and One.Eval_Calls=Before,
            "unavailable EC cannot dispatch a handler despite healthy independent owner");
      end;
      D.Begin_Stop;Check_Cached_Unavailable("stopping");D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
      D.Close(Owner.all,Drain_Result);
      Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"known failed identity permits clean domain retirement");
   end Failed_Identity;
   procedure Independent_Consumers(General_Count : Natural := 0) is
   begin
      Check(D.Observe.Stage=D.Live and D.Observe.Consumers_Started and not D.Observe.EC_Armed
         and D.Observe.General_GPEs=General_Count,"independent consumers do not establish EC authority");
      Log.Clear;
      D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);
      Check(Disposition=ACPI_SCI_Events.Handled and Log.Count=0,"independent fixed callback only copies");
      Run_To_Idle;
      Check(Log.Saw("power-button") and Log.Saw("lid OPEN") and Log.Saw("AC ONLINE")
         and Log.Saw("remaining=20000 full=40000") and Log.Saw("descriptor=32 gpio=42"),
         "independent same-Core startup observations remain useful without EC");
      if General_Count=1 then General_Event;end if;
      Check(One.Reads=0 and One.Writes=0,"unavailable EC never accessed by independent consumers");
      D.Begin_Stop;Check_Cached_Unavailable("stopping");D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
      D.Close(Owner.all,Drain_Result);
      Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"independent consumers stop without invented EC backing");
   end Independent_Consumers;
begin
   Loader.Load(Context,Work,Snapshot_Fixture_Deadline,Owner,Loaded);
   Check(Loaded.Status=L.Loaded and then Owner/=null,"actual snapshot-with-driver completion " & Loaded.Status'Image);
   Check(Q.Notifications_Ready(Owner.all),"native loader requested and enabled notification capture");
   Check(Q.Notifications_Pending(Owner.all)=(Mode in "startup"|"prepare-notify"|"prepare-active-notify"|"prepare-burst"),"startup INI queue witness");
   Metadata.Advance(Owner.all,Accepted);Check(Accepted,"same Core");
   One.Runtime_Access.Enter_Runtime(Owner.all,Runtime_Result);Check(Runtime_Result=One.Runtime.Status.Runtime_Entered,"runtime handoff");
   One.Boot_Ready:=Mode not in "no-ec"|"absent"|"identity-failed"|"general-no-ec";ACPI_SCI_Events.Armed_State:=Mode/="no-sci";
   if Mode="disabled" then
      Q.Handle(Owner.all,Q.Snapshot_Provider,(Label=>Q.Configure_Notifications,Data=>[Q.Revision(Owner.all),0,0,0],others=><>),Reply);
      Check(Reply.Status=Q.OK and not Q.Notifications_Ready(Owner.all),"explicit disabled-policy witness");
   end if;
   ACPI_SCI_Events.Observe_Server(Owner.all'Unchecked_Access);
   ACPI_SCI_Events.General_Available:=Mode in "general"|"general-no-ec"|"overlap"|"general-revoked"|"general-mask-failed"|"general-clear-failed"|"general-enable-failed";
   Check_Cached_Unavailable("before event-domain admission");
   if Mode in "prepare-only"|"prepare-notify"|"prepare-start"|"prepare-burst" then
      declare Before_Evaluations:constant Natural:=One.Eval_Calls;
         Before_Reads:constant Natural:=One.Reads;Before_Writes:constant Natural:=One.Writes;
      begin
         ACPI_SCI_Events.Armed_State:=False;
         D.Prepare_Consumers(Owner.all,Accepted);
         Check(Accepted and D.Observe.Consumers_Started and not D.Attempted and D.Observe.Stage=D.Fresh,"metadata routes prepare before SCI/EC enrollment");
         D.Route_Prepared(Owner.all,Owner_Result);
         if Mode="prepare-burst" then
            Check(Owner_Result=ACPI_Native_Server.Owner_Pending and Q.Notifications_Pending(Owner.all),
              "bounded startup fanout preserves runnable work beyond one batch");
            D.Route_Prepared(Owner.all,Owner_Result);
            Check(Owner_Result=ACPI_Native_Server.Owner_Idle and not Q.Notifications_Pending(Owner.all),
              "next serialized turn drains remaining captured notifications");
         end if;
         Check(Owner_Result/=ACPI_Native_Server.Owner_Refused and not Q.Notifications_Pending(Owner.all),"captured startup Notify routes without EC");
         D.Prepare_Consumers(Owner.all,Accepted);Check(Accepted,"prepare is idempotent for same private owner");
         Check(One.Eval_Calls=Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,"prepare/route performs no fresh AML or EC hardware query");
         if Mode/="prepare-start" then
            D.Begin_Stop;D.Close(Owner.all,Drain_Result);
            Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"prepared but unstarted consumer routes retire");
            Ada.Text_IO.Put_Line("Native event domain "&Mode&" checks"&Checks'Image);return;
         end if;
         ACPI_SCI_Events.Armed_State:=True;
      end;
   end if;
   if Mode in "runtime-expiry"|"runtime-expiry-incomplete"|"runtime-expiry-pending"|"prepare-active-notify" then
      declare
         Original_Time : constant Interfaces.Unsigned_64 := One.Now;
         Window_Length : constant Interfaces.Unsigned_64 := 10;
         Before_Evaluations : constant Natural := One.Eval_Calls;
         Before_Reads : constant Natural := One.Reads;
         Before_Writes : constant Natural := One.Writes;
      begin
         if Mode="prepare-active-notify" then
            ACPI_SCI_Events.Armed_State:=False;
            D.Prepare_Consumers(Owner.all,Accepted);
            Check(Accepted and not D.Attempted,"prepare before captured-notification window");
         end if;
         One.Runtime_Access.Begin_Event(Owner.all,Original_Time+Window_Length,Runtime_Result);
         Check(Runtime_Result=One.Runtime.Status.Event_Opened,"finite runtime event opens");
         if Mode="prepare-active-notify" then
            Check(Q.Notifications_Pending(Owner.all),"captured notification exists while runtime active");
            D.Route_Prepared(Owner.all,Owner_Result);
            Check(Owner_Result=ACPI_Native_Server.Owner_Idle and Q.Notifications_Pending(Owner.all),
              "active startup window defers captured receipt without acknowledging it");
            Check(One.Runtime.Current=One.Runtime.Status.Active and D.Observe.Stage=D.Fresh,
              "deferred routing preserves runtime ownership and healthy preparation");
         end if;
         One.Now:=Original_Time+Window_Length+1;
         if Mode="runtime-expiry-pending" then One.Guard:=True;end if;
         One.Runtime_Access.End_Event(Owner.all,Mode/="runtime-expiry-incomplete",Runtime_Result);
         if Mode="runtime-expiry-incomplete" then
            Check(Runtime_Result=One.Runtime.Status.Evaluation_Failed and One.Runtime.Current=One.Runtime.Status.Stopped,
              "expired incomplete evaluation still stops runtime");
         elsif Mode="runtime-expiry-pending" then
            Check(Runtime_Result=One.Runtime.Status.IO_Pending and One.Runtime.Current=One.Runtime.Status.Stopped,
              "expired held hardware boundary still stops runtime");
         else
            Check(Runtime_Result=One.Runtime.Status.Event_Completed and One.Runtime.Current=One.Runtime.Status.Idle,
              "expired completed quiet event closes through actual runtime");
            if Mode="prepare-active-notify" then
               D.Route_Prepared(Owner.all,Owner_Result);
               Check(Owner_Result/=ACPI_Native_Server.Owner_Refused and not Q.Notifications_Pending(Owner.all),
                 "captured receipt routes after original active window closes");
               Check(D.Observe.Stage=D.Fresh and not D.Attempted,"deferred receipt does not enroll EC or SCI");
            end if;
            One.Runtime_Access.Begin_Event(Owner.all,Original_Time+Window_Length,Runtime_Result);
            Check(Runtime_Result=One.Runtime.Status.Delay_Refused and One.Runtime.Current=One.Runtime.Status.Stopped,
              "closing expired event never revives original deadline");
         end if;
         Check(One.Eval_Calls=Before_Evaluations and One.Reads=Before_Reads and One.Writes=Before_Writes,
           "cleanup and receipt routing perform no fresh AML or register operations");
         Ada.Text_IO.Put_Line("Native event domain "&Mode&" checks"&Checks'Image);return;
      end;
   end if;
   if Mode in "selection-no-clock" | "selection-no-room" then
      if Mode="selection-no-clock" then One.Read_Clock_Available := False;
      else One.Now := Interfaces.Unsigned_64 (One.T.Microseconds'Last) - 5_000_000 + 1; end if;
      declare Accepted : Boolean; begin
         EC_Source.Configure (Owner.all, Accepted);
         Check (not Accepted and then EC_Source.Observe.Stage=EC_Source.Failed
           and then EC_Source.Observe.Why=EC_Source.Window_Refused, "selection rejects unavailable or unrepresentable original deadline");
         Check (One.Maximum_Window_Length=0 and then One.Reads=0 and then One.Writes=0,
            "no runtime event or IO before valid selection window");
      end;
      Check (Q.Valid (Owner.all), "valid refused selection owner");
      Ada.Text_IO.Put_Line ("Native event domain "&Mode&" checks"&Checks'Image); return;
   end if;
   One.Expire_After_Candidate:=Mode="candidate-expiry";
   if Mode="slow-discovery" then One.Delay_After_Candidate := 700_000; end if;
   if Mode="button-slow" then One.Button_Test_Delay:=700_000;
   elsif Mode="button-expired" then One.Button_Test_Delay:=6_000_000;
   elsif Mode="button-unsafe" then One.Button_Test_Unsafe:=True;end if;
   D.Start(Owner.all,Start_Result);
   if Mode in "button-slow"|"button-expired"|"button-unsafe" then
      if Mode="button-slow" then
         Check(Start_Result=D.Ready and One.Delay_In_Sample_HID=0 and One.Last_Window_Length=5_000_000,
            "actual native button discovery admits700ms lookup under finite5s window");
         Check(ACPI_Native_Power_Buttons.Control_Configured and One.Runtime.Current=One.Runtime.Status.Idle,
            "actual button bridge is published only after original owner closes safely");
      else
         Check(Start_Result=D.Refused and D.Observe.Why=D.Button_Discovery_Refused,
            "expired or unsafe button discovery remains refused");
         Check(not ACPI_Native_Power_Buttons.Control_Configured and One.Runtime.Current=One.Runtime.Status.Stopped,
            "expiry/FieldIndeterminate cannot publish button or continue runtime");
      end if;
      Ada.Text_IO.Put_Line("Native event domain "&Mode&" checks"&Checks'Image);return;
   end if;
   declare View:constant ACPI_Native_EC_Discovery.Report:=ACPI_Native_EC_Discovery.Observe;
   begin
      if View.Status=ACPI_Native_EC_Discovery.Ready then
         Check(View.Candidate_Described and View.Candidate_Path_Complete
           and View.Candidate_Node=C.Values.Position(View.Description.Device),
           "ready selector retains independently copied described candidate identity");
      elsif not View.Candidate_Described then
         Check(not View.Candidate_Path_Complete and View.Candidate_Node=C.Namespace.Root
           and View.Candidate_Path=[View.Candidate_Path'Range=>' '],
           "no described candidate publishes canonical empty candidate diagnostics");
      end if;
   end;
   if Mode/="normal" then NE.Select_Runtime(True,Accepted);Check(Accepted,"native fixture runtime selection");end if;
   if Mode in "battery-absent"|"battery-none"|"battery-unknown"|"battery-legacy"|"battery-multiple"|"battery-full"|"battery-overflow" then
      Check(Start_Result=D.Ready,"battery fixture event-domain admission");
      if Mode = "battery-overflow" then
         Run_To_Idle;
         Check (D.Observe.Stage=D.Live and One.Runtime.Current=One.Runtime.Status.Idle
           and ACPI_Native_Diagnostic_Output.Saw("sample refusal family=BATTERY isolated=TRUE")
           and ACPI_Native_Diagnostic_Output.Saw("battery guard=INVENTORY_LIMIT"),
           "oversized inventory is completely refused and the closed battery family is isolated");
         declare
            Batteries:BV.View;Power:CP.Snapshot;
         begin
            D.Read_Batteries(Owner.all,100_000,Batteries);D.Read_Power(Owner.all,100_000,Power);
            Check(Batteries.Status=CP.Not_Ready and Batteries.Count=0,
               "inventory limit publishes no truncated battery prefix");
            Check(Power.Lid.Status=CP.Available and Power.Source.Status=CP.Available,
               "isolated battery failure preserves independent lid and AC readers");
         end;
      else
         Run_To_Idle;
         declare
            First, Copy : BV.View;
            Calls : constant Natural := One.Eval_Calls;
            Reads : constant Natural := One.Reads;
            Writes : constant Natural := One.Writes;
            Lines : constant Natural := Log.Count;
            Expected_Count : constant BV.Battery_Count :=
              (if Mode = "battery-none" then 0 elsif Mode = "battery-multiple" then 2
               elsif Mode = "battery-full" then BV.Maximum_Batteries else 1);
         begin
            D.Read_Batteries (Owner.all, 100_000, First);
            Check (First.Status = ACPI_Cached_Power_Values.Available and First.Count = Expected_Count
              and First.Revision > 0 and First.Time.Known, "complete generation-bound battery fixture inventory");
            if Mode = "battery-absent" then
               Check (First.Items (1).Status = BV.Known_Absent and First.Items (1).Reason = BV.Absent
                 and First.Items (1).Value = BV.Empty_Sample, "known absent battery is explicit and has no previous payload");
            elsif Mode = "battery-unknown" then
               declare Item : BV.Sample renames First.Items (1).Value; begin
                  Check (First.Items (1).Status = BV.Sampled and First.Items (1).Reason = BV.No_Failure,
                    "unknown quantities remain a successfully sampled battery");
                  Check (not Item.Design_Capacity.Known and Item.Design_Capacity.Value = 0
                    and not Item.Last_Full_Capacity.Known and Item.Last_Full_Capacity.Value = 0
                    and not Item.Design_Voltage.Known and Item.Design_Voltage.Value = 0,
                    "unknown static quantities have explicit unknown flags and canonical zero storage");
                  Check (not Item.Dynamic.Rate.Known and Item.Dynamic.Rate.Value = 0
                    and not Item.Dynamic.Remaining_Capacity.Known and Item.Dynamic.Remaining_Capacity.Value = 0
                    and not Item.Dynamic.Voltage.Known and Item.Dynamic.Voltage.Value = 0,
                    "unknown dynamic quantities do not become known zero or stale values");
               end;
            elsif Mode = "battery-legacy" then
               declare Item : BV.Sample renames First.Items (1).Value; begin
                  Check (First.Items (1).Status = BV.Sampled and Item.Source = BV.Legacy_Reported
                    and Item.Units = BV.Energy_And_Power, "missing BIX falls back to actual BIF with explicit source/units");
                  Check (Item.Design_Capacity.Known and Item.Design_Capacity.Value = 43000
                    and Item.Last_Full_Capacity.Known and Item.Last_Full_Capacity.Value = 41000
                    and Item.Design_Voltage.Known and Item.Design_Voltage.Value = 12000,
                    "legacy capacities and design voltage are copied without invented BIX metadata");
               end;
            elsif Mode in "battery-multiple" | "battery-full" then
               for Index in 1 .. Expected_Count loop
                  declare Item : BV.Sample renames First.Items (BV.Slot (Index)).Value; begin
                     Check (First.Items (BV.Slot (Index)).Status = BV.Sampled
                       and Item.Source = BV.Extended_Reported
                       and Item.Units = (if Index = 1 then BV.Charge_And_Current else BV.Energy_And_Power),
                       "per-slot information source and units remain distinct");
                     Check (Item.Dynamic.Remaining_Capacity.Known and Item.Dynamic.Remaining_Capacity.Value =
                       Interfaces.Unsigned_32 (if Index = 1 then 20000 else 20000 + Index),
                       "per-slot capacities copied without overwrite, merge or truncation");
                  end;
               end loop;
            end if;
            for Index in Expected_Count + 1 .. BV.Maximum_Batteries loop
               Check (First.Items (BV.Slot (Index)) = BV.Empty_Entry, "unused inventory slots are empty");
            end loop;
            D.Read_Batteries (Owner.all, 100_000, Copy);
            Check (First = Copy, "battery variant repeated reads preserve generation and values");
            Check_Battery_Wire (First);
            Check (One.Eval_Calls = Calls and One.Reads = Reads and One.Writes = Writes and Log.Count = Lines,
              "battery variant reads run no AML, register IO or logging");
         end;
      end if;
      D.Begin_Stop; Check_Cached_Unavailable ("battery fixture stopping");
      ACPI_SCI_Events.Armed_State := False;
      D.Confirm_Disarm ((State => ACPI_SCI_Quiesce.Quiesced)); Drain_Only;
      D.Close (Owner.all, Drain_Result);
      Check (Drain_Result = D.Drained and D.Quiet (Owner.all), "battery fixture retires owned sources");
   elsif Mode in "general-revoked"|"general-mask-failed"|"general-clear-failed"|"general-enable-failed" then
      Check(Start_Result=D.Ready,"failure scenario admitted before fault");Run_To_Idle;General_Source_Failure;
   elsif Mode="overlap" then
      Check(Start_Result=D.Ready,"overlap startup ready");Run_To_Idle;Overlapping_Events;
   elsif Mode="disabled" then
      Check(Start_Result=D.Refused and ACPI_SCI_Events.Enables=0 and One.Writes=0,"disabled capture refuses source admission");
   elsif Mode in "ancestor-shared"|"ancestor-pruned"|"ancestor-functional" then
      declare
         Parent_Count:constant Positive:=8;
         Leaf_Count:constant Positive:=32;
         Base_EC_Parents:constant Positive:=1;
         Nondevice_Parents:constant Positive:=1;
         Expected_Links:constant Natural:=Parent_Count+Leaf_Count+Base_EC_Parents+Nondevice_Parents;
         Observed_Identities:Interfaces.Unsigned_64:=0;
         View:constant ACPI_Native_EC_Discovery.Report:=ACPI_Native_EC_Discovery.Observe;
      begin
         Check(Start_Result=D.Ready and View.Status=ACPI_Native_EC_Discovery.Ready,
           "shared ancestor scan retains the independent real EC candidate");
         Check(View.Relevance_Parent_Links=Expected_Links,
           "metadata-only relevance visits each relevant nonroot link once");
         Read_Scalar("AOBS",Observed_Identities);
         Check(Observed_Identities=(if Mode="ancestor-pruned" then 0 else Interfaces.Unsigned_64(Leaf_Count)),
           "absent parent prunes identities while functioning-only parent preserves descendants");
         Check(One.Reads=0 and One.Writes=0,"metadata ancestry performs no EC register access");
         D.Begin_Stop;D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
         D.Close(Owner.all,Drain_Result);Check(Drain_Result=D.Drained and D.Quiet(Owner.all),
           "shared ancestor scan leaves no retained owner on close");
      end;
   elsif Mode="candidate-expiry" then
      declare View:constant ACPI_Native_EC_Discovery.Report:=ACPI_Native_EC_Discovery.Observe;
      begin
         Check(Start_Result=D.EC_Unavailable and D.Observe.Stage=D.Live
           and View.Status=ACPI_Native_EC_Discovery.Window_Expired,
           "quiet post-candidate expiry preserves independent live consumers");
         Check(View.Candidate_Described and View.Candidate_Path_Complete
           and View.Candidate_Node=C.Values.Position(View.Description.Device),
           "expired scan retains the originally described candidate node");
         Check(Ada.Strings.Fixed.Index(View.Candidate_Path,"EC0")>0
           and View.At_Node/=View.Candidate_Node and View.Device_Path/=View.Candidate_Path,
           "last scan checkpoint and candidate paths remain distinct after expiry");
         Check(not D.Observe.EC_Armed and One.Reads=0 and One.Writes=0,
           "copied candidate diagnostics establish no EC authority or register access");
      end;
      Independent_Consumers(0);
   elsif Mode="identity-failed" then
      Check(Start_Result=D.EC_Unavailable,"failed EC discovery permits independent consumers only");
      Check(D.Observe.EC=ACPI_Native_EC_Discovery.Discovery_Failed
         and ACPI_Native_EC_Discovery.Observe.Node_Errors>0,
         "failed identity remains explicit discovery failure, never confirmed absence");
      Check(ACPI_SCI_Events.Enables=0 and One.Writes=0,"failed identity creates no source enable or EC write");
      Failed_Identity;
   elsif Mode="general-no-ec" then
      Check(Start_Result=D.EC_Unavailable and D.Observe.EC=ACPI_Native_EC_Discovery.No_EC
         and ACPI_Native_EC_Discovery.Observe.Node_Errors=0,"general source with independently confirmed EC absence");
      Independent_Consumers(1);
   elsif Mode in "no-ec"|"no-sci"|"absent"|"absent-with-backing" then
      Check(Start_Result=(if Mode="no-sci" then D.SCI_Unavailable else D.EC_Unavailable),"explicit unavailable event support");
      if Mode in "absent"|"absent-with-backing" then
         Check(D.Observe.EC=ACPI_Native_EC_Discovery.No_EC,"complete successful namespace scan establishes absent EC independently of early backing");
      elsif Mode="no-ec" then
         Check(D.Observe.EC=ACPI_Native_EC_Discovery.Boot_Unavailable,"identified EC without backing remains unavailable, never absent");
      end if;
      Check(ACPI_SCI_Events.Enables=0 and One.Writes=0,"unavailable source never invented");
      D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);Check(Disposition=ACPI_SCI_Events.Handled and Log.Count=0,"fixed callback only copies");
      if Mode="no-sci" then
         Run_To_Idle;Check(Log.Saw("power-button") and not Log.Saw("acpi: lid"),"no SCI preserves fixed-only behavior");
         D.Begin_Stop;Check_Cached_Unavailable("stopping");D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
         D.Close(Owner.all,Drain_Result);Check(Drain_Result=D.Drained and D.Quiet(Owner.all),"no-SCI source closes");
      else Independent_Consumers;end if;
   else
      Check(Start_Result=D.Ready and not Q.Notifications_Pending(Owner.all),"domain admits source after startup routing");
      if Mode="slow-discovery" then
         Check (One.Maximum_Window_Length = 5_000_000 and then One.Delay_After_Candidate = 0,
            "separate five-second discovery window admits 700ms metadata scan");
      end if;
      if Mode="slow-consumer" then One.Delay_In_Sample_HID:=700_000;end if;
      Run_To_Idle;
      if Mode="slow-consumer" then
         Check(One.Delay_In_Sample_HID=0 and then One.Last_Window_Length=5_000_000 and then One.Runtime.Current=One.Runtime.Status.Idle,
           "native full-namespace sample admits700ms modeled HID lookup under finite5s window");
      end if;
      Check(Log.Saw("lid OPEN") and Log.Saw("AC ONLINE") and Log.Saw("remaining=20000 full=40000") and Log.Saw("descriptor=32 gpio=42"),"initial actual four-family facts through concrete owner");
      if Mode="normal" then Check_Cached_Initial;end if;
      if Mode="general" then General_Event;end if;
      Log.Clear;
      D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);D.Fixed_Event(ACPI_SCI_Registers.Power_Button,Disposition);
      Check(Log.Count=0,"no logger inside fixed callback");Run_To_Idle;Check(Log.Saw("power-button presses= 2"),"copied button count logged outside callback");Log.Clear;
      One.Status_Unknown:=Mode="retained";One.End_Fails:=Mode="ec-close-failed";
      D.GPE_Event(16#6E#,Disposition);Check(Disposition=ACPI_SCI_Events.Handled and Log.Count=0,"EC edge callback contains no logging");
      Check_Cached_Unavailable("captured EC event before query Notify");
      if Mode in "retained"|"ec-close-failed" then
         for Step in 1..8 loop D.Advance(Owner.all,Owner_Result);exit when Owner_Result=ACPI_Native_Server.Owner_Refused;end loop;
         Check(Owner_Result=ACPI_Native_Server.Owner_Refused and not D.Diagnostics_Quiet(Owner.all) and Log.Count=0,"unknown command retains owner without diagnostics IPC");
         Check(EC_Source.Observe.Stage=EC_Source.Failed and then EC_Source.Observe.Why=
           (if Mode="retained" then EC_Source.Query_Uncertain else EC_Source.Window_Refused),
           "EC original failure survives window cleanup: "&EC_Source.Observe.Why'Image);
         declare Before_Reads:constant Natural:=One.Reads;Before_Writes:constant Natural:=One.Writes;begin
            D.Advance(Owner.all,Owner_Result);
            Check(One.Reads=Before_Reads and One.Writes=Before_Writes,
              "failed query or close never replays hardware command");
         end;
         D.Begin_Stop;Check_Cached_Unavailable("stopping");D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));D.Close(Owner.all,Drain_Result);
         if Mode="retained" then
            Check(Drain_Result=D.Retained and not D.Quiet(Owner.all) and Log.Count=0,
              "SCI disarm cannot erase indeterminate command/Field ownership");
         else
            declare Before_Reads:constant Natural:=One.Reads;Before_Writes:constant Natural:=One.Writes;begin
               Drain_Only;D.Close(Owner.all,Drain_Result);
               Check(Drain_Result=D.Drained and D.Quiet(Owner.all) and not One.Guard,
                 "known window-close failure drains after command ownership is quiet");
               Check(One.Reads=Before_Reads and One.Writes=Before_Writes,
                 "ordinary close failure stop never replays an EC transaction");
               Check(not Log.Saw("lid CLOSED") and not Log.Saw("remaining=15000"),
                 "stopping drops captured consumer refresh after close failure");
            end;
         end if;
      else
         if Mode="stop-notify" then
            for Step in 1..8 loop D.Advance(Owner.all,Owner_Result);exit when Q.Notifications_Pending(Owner.all);end loop;
            Check(Q.Notifications_Pending(Owner.all) and Log.Count=0,"pending Notify/fact stop witness");
         else
            One.Query_Windows:=0;
            Run_To_Idle;
            if Mode="slow-discovery" then
               Check (One.Query_Windows>0 and then One.Last_Window_Length=5_000_000,
                  "500ms EC query followed by finite5s consumer refresh");
            end if;
            Check(Log.Saw("query Q58") and Log.Saw("query Q59 UNMAPPED"),"unknown queries logged and consumed without domain failure");
            Check(Log.Saw("lid CLOSED") and Log.Saw("AC OFFLINE") and Log.Saw("remaining=15000 full=40000") and Log.Saw("descriptor=33 gpio=42"),"actual query/Notify refreshed all four families");
            if Mode="normal" then Check_Wire_Refresh;end if;
         end if;
         if Mode="battery-recovery" then
            Refresh_Battery("BAD0","ACQUISITION_FAILED reason=STATUS_FAILED","remaining=15000");
            Refresh_Battery("GOOD","remaining=12345 full=40000","ACQUISITION_FAILED");
            Refresh_Battery("BAD1","ACQUISITION_FAILED reason=INFORMATION_FAILED","remaining=12345");
            Refresh_Battery("GOD1","remaining=13000 full=40000","ACQUISITION_FAILED");
         end if;
         D.Begin_Stop;Check_Cached_Unavailable("stopping");ACPI_SCI_Events.Armed_State:=False;D.Confirm_Disarm((State=>ACPI_SCI_Quiesce.Quiesced));Drain_Only;
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
   Check_Cached_Unavailable("final failed or closed domain");
   Check(Q.Valid(Owner.all),"valid final protocol/Core");
   Ada.Text_IO.Put_Line("Native event domain "&Mode&" checks"&Checks'Image);
end Domain_Tests;
