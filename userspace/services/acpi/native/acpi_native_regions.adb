pragma Ada_2022;
with AML_Field_Notes;
with ACPI_Native_Pre_Device;
with ACPI_One_Service;
with ACPI_Region_Initialization;
with ACPI_Region_Activation_Order;
with AML_Decode;
with ACPI_Native_Diagnostic_Output;
with ACPI_Native_Log;
package body ACPI_Native_Regions is
   package C renames ACPI_Service;
   package One renames ACPI_One_Service;
   package Root renames One.Root;
   package ECT renames Root.Fields.EC_Commands.T;
   package Maps renames Root.Backing.Regions.Cache;
   use type Root.Fields.Field_Phase,One.Runtime.Status.Phase;
   use type AML_Decode.Integer_Value;
   use type C.Namespace.Region_Space_ID;
   use type C.Values.Access_Status, Root.Fields.Field_Sequence, AML_Field_Notes.Node_Position, Maps.Outcome;
   Attempted,Announced,Working : Boolean := False;
   type Capture_Point is (Not_Captured, After_Prepare, After_Loader);
   type Diagnostic_Record is record
      Point : Capture_Point := Not_Captured;
      Visited, Emitted, Accepted : Boolean := False;
      Before_Idle, Before_Uncertain, Before_EC_Quiet : Boolean := False;
      After_Idle, After_Uncertain, After_EC_Quiet : Boolean := False;
      Preparation : Root.Fields.Preparation_Report;
      EC : Root.Fields.EC_Boot_Report;
      Fields : Root.Fields.Field_Report;
   end record;
   Saved : Diagnostic_Record;
   Before_Emitted : Boolean := False;
   -- Fixed copied invocation facts; never retain an AML handle for logging.
   Path_Segments : constant Positive := 32;
   Segment_Characters : constant Positive := 4;
   Path_Characters : constant Positive := Path_Segments * (Segment_Characters + 1);
   type Invocation_Record is record
      Seen : Boolean := False;
      Node : C.Namespace.Node_ID := C.Namespace.Root;
      Path : String (1 .. Path_Characters) := [others => ' '];
      Path_Complete : Boolean := False;
      Space, Direction : AML_Decode.Integer_Value := 0;
      Budget, Charged : Natural := 0;
      Access_Result : C.Values.Access_Status := C.Values.Wrong_Phase;
      Execution : AML_Execute.Execution_Status := AML_Execute.No_Return;
      EC_Before, EC_After : Root.Fields.EC_Commands.Observation;
      Engine_After : ECT.Transaction_Observation;
      Field_Before : Root.Fields.Field_Sequence := 0;
      Field_After : Root.Fields.Field_Trace_Report;
      Field_Path, Region_Path : String (1 .. Path_Characters) := [others => ' '];
      Field_Path_Complete, Region_Path_Complete, Region_Metadata : Boolean := False;
      Region_Base, Region_Bytes : AML_Decode.Integer_Value := 0;
      Pool_Profile_Found, Outside_Pool : Boolean := False;
      Pool_Result : Maps.Outcome := Maps.Not_Ready;
      Pool_Raw : Maps.Word := 0;
   end record;
   Connect_Invocation, Disconnect_Invocation : Invocation_Record;
   procedure Report_Connection;
   function May_Open_Diagnostic_Channel return Boolean is
     (not Working and then not Root.Shutdown.Observe.Working
      and then Root.Fields.Control_Idle
      and then not Root.Fields.Observe.IO_Pending
      and then not Root.Fields.Uncertain.Present
      and then Root.Fields.EC_Commands.Quiescent);
   procedure Capture_Path (Service : C.State; Node : C.Namespace.Node_ID;
      Text : out String; Complete : out Boolean) is
      Cursor : C.Namespace.Node_ID := Node;
      Item : C.Node_Descriptor;
      Status : C.Values.Access_Status;
      At_Character : Natural := Text'Last;
   begin
      Text := [Text'Range => ' ']; Complete := False;
      for Depth in 1 .. Path_Segments loop
         if Cursor = C.Namespace.Root then
            Text(At_Character) := '\'; Complete := True; return;
         end if;
         C.Describe_Node(Service,Cursor,Item,Status);
         if Status /= C.Values.Available then return; end if;
         Text(At_Character - Segment_Characters + 1 .. At_Character) := C.Values.Node_Name(Item);
         At_Character := At_Character - Segment_Characters;
         if C.Values.Parent_Position(Item) = C.Namespace.Root then
            Text(At_Character) := '\'; Complete := True; return;
         end if;
         Text(At_Character) := '.';
         At_Character := At_Character - 1;
         Cursor := C.Values.Parent_Position(Item);
      end loop;
      -- A full buffer is explicitly truncated, never presented as a full path.
   end Capture_Path;
   procedure Capture_Current is
   begin
      Saved.After_Idle := Root.Fields.Control_Idle;
      Saved.After_Uncertain := Root.Fields.Uncertain.Present;
      Saved.After_EC_Quiet := Root.Fields.EC_Commands.Quiescent;
      Saved.Preparation := Root.Fields.Preparation_Observation;
      Saved.EC := Root.Fields.EC_Boot_Observation;
      Saved.Fields := Root.Fields.Observe;
   end Capture_Current;
   procedure Capture_If_Unvisited is
   begin
      if Saved.Point /= Not_Captured then return; end if;
      Capture_Current; Saved.Point := After_Loader;
   end Capture_If_Unvisited;
   procedure Report_Copied_Diagnostics (Point : Emission_Point := After_Cleanup) is
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
      Line : constant String := "acpi: stage region-prepare " &
         (if Saved.Accepted then "ok " else "fail ") & Saved.Preparation.Status'Image &
         " at=" & Saved.Point'Image & " visited=" & Bit(Saved.Visited) &
         " idle=" & Bit(Saved.After_Idle) & " uncertain=" & Bit(Saved.After_Uncertain) &
         " ec-quiet=" & Bit(Saved.After_EC_Quiet) &
         " before=" & Bit(Saved.Before_Idle) & Bit(Saved.Before_Uncertain) & Bit(Saved.Before_EC_Quiet) &
         " context=" & Bit(Saved.Preparation.Context_Current) &
         " module=" & Bit(Saved.Preparation.Module_Completed) &
         " anchor=" & Bit(Saved.Preparation.Anchor_Present) &
         " core=" & Bit(Saved.Preparation.Core_Valid) &
         " owner=" & Bit(Saved.Preparation.Owner_Ready) &
         " scope=" & Bit(Saved.Preparation.Scope_Ready) &
         " mux=" & Bit(Saved.Preparation.Mux_Ready) &
         " admit=" & Bit(Saved.Preparation.Hardware_Admitting) &
         " node=" & Saved.Preparation.Failed_Node'Image &
         " access=" & Saved.Preparation.Access_Failure'Image;
      Detail : constant String := "acpi: stage region-prepare-size limit=" & Saved.Preparation.Binding_Limit'Image &
         " metadata-bytes=" & Saved.Preparation.Metadata_Bytes'Image &
         " allocated=" & Saved.Preparation.Allocated_Metadata_Bytes'Image &
         " regions=" & Saved.Preparation.Region_Entries'Image &
         " read=" & Saved.Preparation.Read_Fields'Image &
         " write=" & Saved.Preparation.Write_Fields'Image &
         " missing=" & Saved.Preparation.Missing_Fields'Image &
         " ec=" & Saved.EC.Status'Image & " fields=" & Saved.Fields.Stage'Image &
         " field-module=" & Bit(Saved.Fields.Module_Completed) &
         " registers=" & Saved.Fields.Registers'Image;
   begin
      if Point=Before_Cleanup then
         if Before_Emitted then return; end if; Before_Emitted:=True;
      else
         if Saved.Emitted then return; end if; Saved.Emitted:=True;
      end if;
      ACPI_Native_Diagnostic_Output.Write("acpi: stage diagnostic-checkpoint at=" & Point'Image,May_Open_Diagnostic_Channel);
      -- No Core/provider access below: only the fixed copied observation.
      ACPI_Native_Diagnostic_Output.Write(Line,May_Open_Diagnostic_Channel,Saved.Accepted);
      ACPI_Native_Diagnostic_Output.Write(Detail,May_Open_Diagnostic_Channel,Saved.Accepted);
      Report_Connection;
   end Report_Copied_Diagnostics;
   type Controller is null record;
   Context : Controller;
   Memory_Space : constant C.Namespace.Region_Space_ID := 0;
   IO_Space : constant C.Namespace.Region_Space_ID := 1;
   PCI_Space : constant C.Namespace.Region_Space_ID := 2;
   EC_Space : constant C.Namespace.Region_Space_ID := 3;
   Table_Space : constant C.Namespace.Region_Space_ID := 126;
   Connect_Code : constant := 1;
   Notice_Argument_Count : constant Natural := 2;
   function Handler_Ready (Owner : Controller; Region : C.Node_Descriptor;
      Data : C.Namespace.Operation_Region_Attributes) return Boolean is
      pragma Unreferenced(Owner,Region);
   begin
      if Root.Fields.Observe.Stage/=Root.Fields.Owner_Ready or else Root.Fields.Observe.IO_Pending then return False;end if;
      case Data.Space is
         -- These root handlers are installed by the existing local mux/Core.
         -- This announces a handler, never authority to each claimed address:
         -- every actual datum still requires its exact native admission.
         when Memory_Space|IO_Space|PCI_Space|Table_Space=>return True;
         when EC_Space=>return Root.Fields.EC_Handler_Available
            and then Root.Hardware.Observe.Lock_Ready;
         when others=>return False;
      end case;
   end Handler_Ready;
   function Permanent_PCI_Root (Owner : Controller; Region : C.Node_Descriptor) return Boolean is
      pragma Unreferenced(Owner,Region);
   begin return False;end Permanent_PCI_Root;
   procedure Invoke (Service : aliased in out C.State; Owner : in out Controller;
      Node : C.Namespace.Node_ID; Args : AML_Execute.Arguments; Argument_Count : Natural;
      Budget : Natural; Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      pragma Unreferenced(Owner);
      Notice : Invocation_Record;
      Description : C.Node_Descriptor;
      Data : C.Namespace.Operation_Region_Attributes;
      Metadata_Status : C.Values.Access_Status;
   begin
      Result:=(AML_Execute.Unsupported_Value,0);Status:=C.Values.Wrong_Phase;
      if Argument_Count/=Notice_Argument_Count then return;end if;
      Notice.Seen := True; Notice.Node := Node;
      Notice.Space := Args(0); Notice.Direction := Args(1); Notice.Budget := Budget;
      Capture_Path(Service,Node,Notice.Path,Notice.Path_Complete);
      Notice.EC_Before := Root.Fields.EC_Commands.Observe;
      Notice.Field_Before := Root.Fields.Field_Observation.Sequence;
      -- Latch before entering AML, including a failed/partial connect. Only
      -- a fully attributable Disconnect below clears this announcement duty.
      if Args(1)=Connect_Code then Announced:=True;end if;
      if One.Runtime.Current=One.Runtime.Status.Active then
         One.Runtime.Invoke_Retained(Service,Node,Args,C.Values.Argument_Count(Argument_Count),Budget,Result,Status);
      elsif One.Runtime.Current=One.Runtime.Status.Fresh then
         Root.Evaluation.Invoke_Retained(Service,Node,Args,C.Values.Argument_Count(Argument_Count),Budget,Result,Status);
      end if;
      Notice.Access_Result := Status; Notice.Execution := Result.Status;
      Notice.Charged := Result.Charged;
      Notice.Field_After := Root.Fields.Field_Observation;
      if not Notice.Field_After.Saturated and then Notice.Field_After.Sequence /= Notice.Field_Before then
         if Notice.Field_After.Item.Known_Field and then Notice.Field_After.Item.Field <=
           AML_Field_Notes.Node_Position(C.Namespace.Node_ID'Last) then
            Capture_Path(Service,C.Namespace.Node_ID(Notice.Field_After.Item.Field),Notice.Field_Path,Notice.Field_Path_Complete);
         end if;
         if Notice.Field_After.Item.Known_Region and then Notice.Field_After.Item.Region <=
           AML_Field_Notes.Node_Position(C.Namespace.Node_ID'Last) then
            Capture_Path(Service,C.Namespace.Node_ID(Notice.Field_After.Item.Region),Notice.Region_Path,Notice.Region_Path_Complete);
            C.Describe_Node(Service,C.Namespace.Node_ID(Notice.Field_After.Item.Region),Description,Metadata_Status);
            if Metadata_Status=C.Values.Available then
               C.Read_Region_Descriptor(Service,Description,Data,Metadata_Status);
               if Metadata_Status=C.Values.Available then
                  Notice.Region_Metadata:=True;Notice.Region_Base:=Data.Address;Notice.Region_Bytes:=Data.Length;
               end if;
            end if;
         end if;
      end if;
      if Notice.Region_Metadata then
         declare Count : constant Natural := Maps.Observe(Root.Backing.Memory).Regions; begin
            for I in 1..Count loop
               declare Profile : constant Maps.Profile := Maps.Read_Profile(Root.Backing.Memory,I); begin
                  if Profile.Available and then Profile.Geometry.Address=Notice.Region_Base and then
                    Profile.Geometry.Length=Notice.Region_Bytes then
                     Notice.Pool_Profile_Found:=True;Notice.Pool_Result:=Profile.Result;Notice.Pool_Raw:=Profile.Raw_Status;
                     Notice.Outside_Pool:=Profile.Result=Maps.Kernel_Refused and then
                       Profile.Raw_Status=Maps.ABI.Refusal_Word(Maps.ABI.Outside_Pool);
                  end if;
               end;
            end loop;
         end;
      end if;
      Notice.EC_After := Root.Fields.EC_Commands.Observe;
      Notice.Engine_After := Root.Fields.EC_Commands.Engine_Observation;
      if Args(1)=Connect_Code then Connect_Invocation := Notice;
      else Disconnect_Invocation := Notice; end if;
   end Invoke;
   package Regions is new ACPI_Region_Initialization(C,Controller,Handler_Ready,Permanent_PCI_Root,Invoke,
      Require_Every_Handler=>False,Coalesce_Local_Notifications=>True);
   use type Regions.Phase;
   package Order is new ACPI_Region_Activation_Order (C);
   Run : Regions.Session;
   type Scope_Set is array (C.Namespace.Node_ID) of Boolean;
   type Ancestor_Map is array (C.Namespace.Node_ID) of C.Namespace.Node_ID;
   Due_After : Ancestor_Map := [others => C.Namespace.Root];
   Required_Ancestors : Scope_Set := [others=>False];
   Prepared_Root : C.Node_Descriptor := C.Values.No_Node;
   Activated, Seen_Ancestors : Scope_Set := [others => False];
   Ancestor_Steps : Natural := 0;
   Last_Ancestor : C.Namespace.Node_ID := C.Namespace.Root;
   Connect_Report, Disconnect_Report : Regions.Report;
   Connect_Captured, Disconnect_Captured : Boolean := False;
   Unavailable_Limit : constant Positive := 8;
   subtype Copied_Count is Natural range 0 .. Unavailable_Limit;
   type Unavailable_Reason is
     (Handler_Absent, Unsupported_Space, Controller_Not_Ready, IO_Pending,
      EC_Handler_Not_Ready, Global_Lock_Not_Ready, Group_Peer_Unavailable, Metadata_Unavailable);
   type Unavailable_Record is record
      Node : C.Namespace.Node_ID := C.Namespace.Root;
      Path : String (1 .. Path_Characters) := [others => ' '];
      Path_Complete, Metadata : Boolean := False;
      Space : C.Namespace.Region_Space_ID := 0;
      Address, Bytes : AML_Decode.Integer_Value := 0;
      Why : Unavailable_Reason := Handler_Absent;
   end record;
   type Unavailable_Rows is array (Positive range 1 .. Unavailable_Limit) of Unavailable_Record;
   Unavailable_Copy : Unavailable_Rows;
   Unavailable_Used : Copied_Count := 0;
   Unavailable_Total : Natural := 0;
   Unavailable_Captured, Unavailable_Emitted : Boolean := False;
   function Trimmed (Text : String) return String is
      First : Integer := Text'First;
      Last : Integer := Text'Last;
   begin
      while First <= Last and then Text (First) = ' ' loop First := First + 1; end loop;
      while Last >= First and then Text (Last) = ' ' loop Last := Last - 1; end loop;
      return Text (First .. Last);
   end Trimmed;
   procedure Capture_Unavailable (Service : C.State) is
      View : Regions.Registration_Observation;
      Descriptor : C.Node_Descriptor;
      Data : C.Namespace.Operation_Region_Attributes;
      Status : C.Values.Access_Status;
   begin
      Unavailable_Total := Natural (Regions.Observe (Run).Unavailable);
      for Node in C.Namespace.Node_ID loop
         View := Regions.Observe_Registration (Run, Node);
         if View.Present and then not View.Group_Available and then Unavailable_Used < Unavailable_Limit then
            Unavailable_Used := Unavailable_Used + 1;
            declare Item : Unavailable_Record renames Unavailable_Copy (Unavailable_Used); begin
               Item.Node := Node; Capture_Path (Service, Node, Item.Path, Item.Path_Complete);
               C.Describe_Node (Service, Node, Descriptor, Status);
               if Status = C.Values.Available then C.Read_Region_Descriptor (Service, Descriptor, Data, Status); end if;
               Item.Metadata := Status = C.Values.Available;
               if not Item.Metadata then Item.Why := Metadata_Unavailable;
               else
                  Item.Space := Data.Space; Item.Address := Data.Address; Item.Bytes := Data.Length;
                  if View.Handler_Present then Item.Why := Group_Peer_Unavailable;
                  elsif Root.Fields.Observe.Stage /= Root.Fields.Owner_Ready then Item.Why := Controller_Not_Ready;
                  elsif Root.Fields.Observe.IO_Pending then Item.Why := IO_Pending;
                  elsif Data.Space = EC_Space then
                     Item.Why := (if not Root.Fields.EC_Handler_Available then EC_Handler_Not_Ready
                       elsif not Root.Hardware.Observe.Lock_Ready then Global_Lock_Not_Ready else Handler_Absent);
                  elsif Data.Space not in Memory_Space | IO_Space | PCI_Space | Table_Space then
                     Item.Why := Unsupported_Space;
                  else Item.Why := Handler_Absent;
                  end if;
               end if;
            end;
         end if;
      end loop;
      Unavailable_Captured := True;
   end Capture_Unavailable;

   procedure Report_Connection is
      function Bit (Value : Boolean) return String is (if Value then "1" else "0");
      function Hex (Value : AML_Decode.Integer_Value) return String is
         Hex_Digits : constant String := "0123456789ABCDEF";
         Radix : constant := 16;
         Text : String(1..16);Remaining : AML_Decode.Integer_Value := Value;
      begin
         for I in reverse Text'Range loop Text(I):=Hex_Digits(Natural(Remaining mod Radix)+1);Remaining:=Remaining/Radix;end loop;
         return Text;
      end Hex;
      Current : constant Regions.Report := Regions.Observe(Run);
      Pending : constant Boolean := Regions.Pending(Run);
      Final_Command : constant Root.Fields.EC_Commands.Observation := Root.Fields.EC_Commands.Observe;
      Final_Engine : constant ECT.Transaction_Observation := Root.Fields.EC_Commands.Engine_Observation;
      Final_Idle : constant Boolean := Root.Fields.Control_Idle;
      Final_Uncertain : constant Boolean := Root.Fields.Uncertain.Present;
      Final_EC_Quiet : constant Boolean := Root.Fields.EC_Commands.Quiescent;
      procedure Emit (Text : String) is
      begin ACPI_Native_Diagnostic_Output.Write(Text,May_Open_Diagnostic_Channel); end Emit;
      procedure Emit_EC (At_Point : String; Command : Root.Fields.EC_Commands.Observation;
         Engine : ECT.Transaction_Observation) is
         use type ECT.Microseconds, ECT.Byte;
         Elapsed : constant ECT.Microseconds :=
           (if Engine.Has_Began and then Engine.Last_Sample>=Engine.Began then Engine.Last_Sample-Engine.Began else 0);
      begin
         Emit("acpi: stage ec-command at=" & At_Point & " owner=" & Command.Stage'Image &
           " token=" & Command.Token'Image & " kind=" & Command.Kind'Image & " hardware=" & Command.Hardware'Image &
           " guard=" & Bit(Command.Guard_Held) & " recovery=" & Bit(Command.Recovery) &
           " effects=" & Bit(Command.Effects_Possible) & " collected=" & Bit(Command.Collected) &
           " stopping=" & Bit(Command.Stopping) & " why=" & Command.Why'Image);
         Emit("acpi: stage ec-engine at=" & At_Point & " status=" & Engine.Status'Image &
           " phase=" & Engine.Stage'Image & " last-active=" & Engine.Last_Active'Image &
           " command=" & Engine.Command'Image & " cell=" & Engine.Cell'Image &
           " remaining=" & Engine.Remaining'Image & " recovery=" & Bit(Engine.Recovery) &
           " effects=" & Bit(Engine.Effects_Possible) & " stopping=" & Bit(Engine.Stopping));
         Emit("acpi: stage ec-clock at=" & At_Point & " deadline-us=" & Engine.Deadline'Image &
           " began-us=" & Engine.Began'Image & " sample-us=" & Engine.Last_Sample'Image &
           " floor-us=" & Engine.Last_Time'Image & " wake-us=" & Engine.Wake'Image &
           " elapsed-us=" & Elapsed'Image & " sample=" & Bit(Engine.Sample_Available) &
           " began=" & Bit(Engine.Has_Began) & " time=" & Bit(Engine.Has_Time));
         Emit("acpi: stage ec-status at=" & At_Point & " attempted=" & Bit(Engine.Status_Attempted) &
           " known=" & Bit(Engine.Status_Known) & " result=" & Engine.Status_Result'Image &
           " byte=" & Engine.Last_Status'Image &
           " ibf=" & Bit(Engine.Status_Known and then (Engine.Last_Status and ECT.Input_Full)/=0) &
           " obf=" & Bit(Engine.Status_Known and then (Engine.Last_Status and ECT.Output_Full)/=0) &
           " burst=" & Bit(Engine.Status_Known and then (Engine.Last_Status and ECT.Burst_Mode)/=0) &
           " sci-evt=" & Bit(Engine.Status_Known and then (Engine.Last_Status and ECT.Event_Pending)/=0));
      end Emit_EC;
      procedure Emit (Direction : String; Captured : Boolean;
         Info : Regions.Report; Notice : Invocation_Record) is
      begin
         Emit("acpi: stage region-" & Direction &
           " captured=" & Bit(Captured) & " stage=" & Info.Stage'Image &
           " error-node=" & Info.Error_Node'Image & " error=" & Info.Error'Image &
           " regions=" & Info.Regions'Image & " methods=" & Info.Methods'Image & " unavailable=" & Info.Unavailable'Image &
           " charged=" & Info.Charged'Image & " reported=" & Info.Reported_Charge'Image &
           " cleanup=" & Info.Cleanup_Result'Image & " fatal=" & Bit(Info.Fatal.Present));
         Emit("acpi: stage region-invoke direction=" & Direction &
           " seen=" & Bit(Notice.Seen) & " node=" & Notice.Node'Image &
           " path=" & Notice.Path & " path-complete=" & Bit(Notice.Path_Complete) &
           " space=" & Notice.Space'Image & " direction=" & Notice.Direction'Image &
           " access=" & Notice.Access_Result'Image & " execution=" & Notice.Execution'Image &
           " invoke-charged=" & Notice.Charged'Image & " budget=" & Notice.Budget'Image &
           " ec-before=" & Notice.EC_Before.Token'Image & " ec-after=" & Notice.EC_After.Token'Image &
           " field-before=" & Notice.Field_Before'Image & " field-after=" & Notice.Field_After.Sequence'Image &
           " field-saturated=" & Bit(Notice.Field_After.Saturated));
         if Notice.Seen and then Notice.Field_After.Sequence/=Notice.Field_Before then
            Emit("acpi: stage region-field direction=" & Direction &
              " sequence=" & Notice.Field_After.Sequence'Image & " saturated=" & Bit(Notice.Field_After.Saturated) &
              " boundary=" & Notice.Field_After.Item.At_Point'Image &
              " operation=" & (if Notice.Field_After.Item.For_Write then "WRITE_FIELD" else "READ_FIELD") &
              " field=" & Notice.Field_After.Item.Field'Image & " known=" & Bit(Notice.Field_After.Item.Known_Field) &
              " path=" & Notice.Field_Path & " path-complete=" & Bit(Notice.Field_Path_Complete));
            Emit("acpi: stage region-field-target region=" & Notice.Field_After.Item.Region'Image &
              " known=" & Bit(Notice.Field_After.Item.Known_Region) & " path=" & Notice.Region_Path &
              " path-complete=" & Bit(Notice.Region_Path_Complete) & " space=" & Notice.Field_After.Item.Space'Image &
              " metadata=" & Bit(Notice.Region_Metadata) & " base=0x" & Hex(Notice.Region_Base) &
              " bytes=" & Notice.Region_Bytes'Image & " bit-offset=" & Notice.Field_After.Item.Bit_Offset'Image &
              " bits=" & Notice.Field_After.Item.Bit_Length'Image & " flags=" & Notice.Field_After.Item.Flags'Image);
            Emit("acpi: stage region-field-binding cached-direct=" & Bit(Notice.Field_After.Item.Direct_Binding) &
              " cached-writes=" & Bit(Notice.Field_After.Item.Writes_Enabled) &
              " datums=" & Notice.Field_After.Item.Read_Datums'Image &
              " locked-enabled=" & Bit(Notice.Field_After.Item.Locked_Fields_Enabled) &
              " execution=" & Notice.Field_After.Item.Status'Image);
         end if;
         if Notice.Pool_Profile_Found then
            Emit("acpi: stage region-backing result=" & Notice.Pool_Result'Image &
              " raw=" & Notice.Pool_Raw'Image & " addr=0x" & Hex(Notice.Region_Base) &
              " len=" & Notice.Region_Bytes'Image & " outside-pool=" & Bit(Notice.Outside_Pool));
            if Notice.Outside_Pool then
               Emit("acpi: region-unbacked addr=0x" & Hex(Notice.Region_Base) &
                 " len=" & Notice.Region_Bytes'Image & " reason=OUTSIDE_POOL no-covering-pool-member");
            end if;
         end if;
         Emit_EC(Direction,Notice.EC_After,Notice.Engine_After);
      end Emit;
   begin
      if Unavailable_Captured and then not Unavailable_Emitted then
         Unavailable_Emitted := True;
         for I in 1 .. Unavailable_Used loop
            declare Item : Unavailable_Record renames Unavailable_Copy (I); begin
               Emit ("acpi: stage region-unavailable fail " & Item.Why'Image & " node=" & Trimmed (Item.Node'Image) &
                 " path=" & Trimmed (Item.Path) & " path-complete=" & Bit (Item.Path_Complete) &
                 " space=" & Trimmed (Item.Space'Image) & " metadata=" & Bit (Item.Metadata) &
                 " base=0x" & Hex (Item.Address) & " bytes=" & Trimmed (Item.Bytes'Image));
            end;
         end loop;
         if Unavailable_Total > 0 then
            Emit ("acpi: stage region-unavailable-summary ok count=" & Trimmed (Unavailable_Total'Image) &
              " captured=" & Trimmed (Unavailable_Used'Image) &
              " omitted=" & Trimmed (Natural'(Unavailable_Total - Natural (Unavailable_Used))'Image));
         end if;
      end if;
      Emit("connect",Connect_Captured,Connect_Report,Connect_Invocation);
      Emit("disconnect",Disconnect_Captured,Disconnect_Report,Disconnect_Invocation);
      Emit_EC("retire",Final_Command,Final_Engine);
      Emit("acpi: stage diagnostic-boundary idle=" & Bit(Final_Idle) &
         " uncertain=" & Bit(Final_Uncertain) & " ec-quiet=" & Bit(Final_EC_Quiet) &
         " copied-log-dropped=" & ACPI_Native_Log.Copied_Dropped'Image);
      Emit("acpi: stage region-owner stage=" & Current.Stage'Image &
         " announced=" & Bit(Announced) & " pending=" & Bit(Pending) &
         " cleanup=" & Current.Cleanup_Result'Image & " working=" & Bit(Working) &
         " ancestor-steps=" & Ancestor_Steps'Image & " last-ancestor=" & Last_Ancestor'Image);
   end Report_Connection;
   function Observe return Report is
      Info : constant Regions.Report:=Regions.Observe(Run);
   begin
      return (Attempted=>Attempted,Connected=>Info.Stage=Regions.Connected,Announced=>Announced,
         Pending=>Regions.Pending(Run),Working=>Working,Regions=>Info.Regions,Unavailable=>Info.Unavailable,
         Methods=>Info.Methods,Charged=>Info.Charged,Error=>Info.Error,
         Ancestor_Steps=>Ancestor_Steps,Last_Ancestor=>Last_Ancestor);
   end Observe;
   function Quiet return Boolean is (not Working and then not Announced and then not Regions.Pending(Run)
      and then ACPI_Native_Pre_Device.Quiet);
   procedure Prepare (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Budget);
      Accepted : Boolean;
      Ordered : Boolean;
      Metadata_Charge : Natural;
      Metadata_Action : Regions.Scope_Action;
      Root_Status : C.Values.Access_Status;
   begin
      Charged:=0;Outcome:=ACPI_Device_Policy.Regions_Unavailable;
      if Attempted or else Working then return;end if;Attempted:=True;Working:=True;
      Saved.Visited := True;
      Saved.Before_Idle := Root.Fields.Control_Idle;
      Saved.Before_Uncertain := Root.Fields.Uncertain.Present;
      Saved.Before_EC_Quiet := Root.Fields.EC_Commands.Quiescent;
      Root.Evaluation.Prepare_Fields(Service,Accepted);
      Saved.Accepted := Accepted; Capture_Current; Saved.Point := After_Prepare;
      if not Accepted then Working:=False;return;end if;
      Regions.Prepare(Run,Service,Context,Accepted);
      if not Accepted or else Regions.Observe(Run).Stage/=Regions.Prepared then
         Outcome:=ACPI_Device_Policy.Region_Initialization_Failed;Working:=False;return;
      end if;
      Capture_Unavailable (Service);
      C.Describe_Node(Service,C.Namespace.Root,Prepared_Root,Root_Status);
      if Root_Status/=C.Values.Available or else not C.Current_Binding(Service,Prepared_Root) then
         Outcome:=ACPI_Device_Policy.Region_Initialization_Failed;Working:=False;return;
      end if;
      -- A local REG follows the closest proper ancestor whose initializer
      -- the generic walker considers. No platform name or OSYS value here.
      for Scope in C.Namespace.Node_ID loop
         if Regions.Has_Prepared_Group(Run,Scope) then
            Order.Required_Ancestor(Service,Scope,Due_After(Scope),Ordered);
            if not Ordered then Working:=False;return;end if;
            Required_Ancestors(Due_After(Scope)):=True;
            -- Preserve the existing prerequisite prefix for consumers, while
            -- consuming groups with no REG invocation without requiring INI
            -- success from an inactive ancestor. This performs no AML/I/O.
            if not Regions.Has_Notification_Work (Run, Scope) then
               Activated (Scope) := True;
               Regions.Connect_Scope (Run, Service, Context, Scope, 0, Metadata_Charge, Metadata_Action);
               if Metadata_Charge /= 0 or else Metadata_Action not in Regions.Scope_Completed | Regions.Already_Completed then
                  Outcome := ACPI_Device_Policy.Region_Initialization_Failed; Working := False; return;
               end if;
            end if;
         end if;
      end loop;
      Outcome:=ACPI_Device_Policy.Regions_Prepared;Working:=False;
   end Prepare;
   procedure Classify_Prerequisite
     (Service : C.State; Scope : C.Namespace.Node_ID;
      Required, Accepted : in out Boolean) is
   begin
      Required:=False;Accepted:=False;
      if Working or else not Attempted or else Regions.Observe(Run).Stage/=Regions.Prepared then return;end if;
      if not C.Current_Binding(Service,Prepared_Root) then return;end if;
      -- The device walker separately validates its complete same-Core topology;
      -- this callback only reports membership, never grants Field authority.
      Required:=Required_Ancestors(Scope);Accepted:=True;
   end Classify_Prerequisite;
   procedure After_Ancestor
     (Service : aliased in out C.State; Scope : C.Namespace.Node_ID; Initialization_Completed : Boolean;
      Budget : Natural; Charged : in out Natural;
      Outcome : in out ACPI_Device_Policy.Region_Outcome) is
      Step_Charge : Natural;
      Action : Regions.Scope_Action;
   begin
      Charged:=0;Outcome:=ACPI_Device_Policy.Region_Initialization_Failed;
      if Working or else not Attempted or else Regions.Observe(Run).Stage/=Regions.Prepared then return;end if;
      if Seen_Ancestors(Scope) then Outcome:=ACPI_Device_Policy.No_Regions_Required;return;end if;
      Seen_Ancestors(Scope):=True;Last_Ancestor:=Scope;
      if Ancestor_Steps=Natural'Last then return;end if;Ancestor_Steps:=Ancestor_Steps+1;
      Working:=True;
      for Group_Scope in C.Namespace.Node_ID loop
         if Regions.Has_Prepared_Group(Run,Group_Scope) and then not Activated(Group_Scope)
           and then Due_After(Group_Scope)=Scope then
            if not Initialization_Completed then
               Outcome:=ACPI_Device_Policy.Regions_Unavailable;Working:=False;return;
            end if;
            Activated(Group_Scope):=True;
            Regions.Connect_Scope(Run,Service,Context,Group_Scope,Budget-Charged,Step_Charge,Action);
            Connect_Report:=Regions.Observe(Run);Connect_Captured:=True;
            if Step_Charge>Budget-Charged then Working:=False;return;end if;
            Charged:=Charged+Step_Charge;
            if Action not in Regions.Scope_Completed | Regions.Already_Completed then Working:=False;return;end if;
         end if;
      end loop;
      Outcome:=ACPI_Device_Policy.Regions_Connected;Working:=False;
   end After_Ancestor;
   procedure Finish
     (Service : aliased in out C.State; Budget : Natural;
      Charged : in out Natural; Outcome : in out ACPI_Device_Policy.Region_Outcome) is
      pragma Unreferenced (Budget);
      Accepted : Boolean;
   begin
      Charged:=0;Outcome:=ACPI_Device_Policy.Region_Initialization_Failed;
      if Working or else not Attempted then return;end if;Working:=True;
      Regions.Finish_Connect(Run,Service,Context,Accepted);
      Connect_Report:=Regions.Observe(Run);Connect_Captured:=True;
      if Accepted then Outcome:=Regions.Outcome(Run);end if;
      Working:=False;
   end Finish;
   procedure Disconnect (Service : aliased in out C.State; Budget : Natural;
      Charged : out Natural; Completed : out Boolean) is
      Accepted : Boolean;
   begin
      Charged:=0;Completed:=False;if Working then return;end if;
      Working:=True;ACPI_Native_Pre_Device.Close(Service);Working:=False;
      if not ACPI_Native_Pre_Device.Quiet then return;end if;
      if not Announced and then not Regions.Pending(Run) then Completed:=True;return;end if;
      Working:=True;
      Regions.Disconnect(Run,Service,Context,Budget,Accepted);
      Disconnect_Report := Regions.Observe(Run); Disconnect_Captured := True;
      Charged:=Regions.Observe(Run).Charged;
      Completed:=Accepted and then Regions.Observe(Run).Stage=Regions.Disconnected and then not Regions.Pending(Run);
      if Completed then Announced:=False;end if;
      Working:=False;
   end Disconnect;
end ACPI_Native_Regions;
