pragma Ada_2022;
with CuBit.Messages;
with ACPI_FADT.Transactions;
with AML_Decode;
with ACPI_NVS_Write_Policy;
with ACPI_Backend_Memory_Access;
with ACPI_EC_Boot_Table;
with Firmware_Tables;
with ACPI_Clock_Source;
with ACPI_Native_Log;
with ACPI_Native_Delays;
with AML_Delays;with ACPI_Native_Field_Profile;
with CuBit.Handles.Devices;
package body ACPI_Local_Hardware.Pooled_Fields is
   use type Scope.Depth_Count,Evaluation.Context_Access;
   use type EC_Types.Microseconds,EC_Types.Read_Register,EC_Types.Write_Register;
   use type Core.Values.Access_Status, Core.Execution_Phase_Result, Core.Values.Module_Outcome;
   use type AML_Decode.Byte, Core.Context_Key;
   use type Core.Namespace.Region_Space_ID;
   use type Core.Namespace.Field_Initialization_Outcome;
   CMOS_Module_Read_Logged, CMOS_Module_Read_Pending : Boolean := False;
   CMOS_Module_Read_Value : Word := 0;
   function Check_Field_Access (For_Write : Boolean) return Boolean is
      pragma Unreferenced (For_Write);
      Accepted : Boolean;
   begin
      if Field_State /= Owner_Ready then return False; end if;
      Scope.Check_Datum (Transaction,Accepted); return Accepted;
   end Check_Field_Access;
   procedure AML_Lock_Clock (Now : out Word; Available : out Boolean) is
   begin ACPI_Clock_Source.Read (Now, Available); end AML_Lock_Clock;
   procedure AML_Lock_Wait (Target : Word; Completed : out Boolean) is
      Now, Remaining, Milliseconds : Word;
      Result : AML_Delays.Outcome;
      use type AML_Delays.Outcome;
      Microseconds_Per_Millisecond : constant Word := 1_000;
   begin
      Completed := False;
      ACPI_Clock_Source.Read (Now, Completed);
      if not Completed or else Now >= Target then return; end if;
      Remaining := Target - Now;
      if Remaining <= Word (AML_Delays.Stall_Microseconds'Last) then
         ACPI_Native_Delays.Perform ((Kind => AML_Delays.Stall_Delay,
           Microseconds => AML_Delays.Stall_Microseconds (Remaining)), Result);
      else
         Milliseconds := Remaining / Microseconds_Per_Millisecond
           + (if Remaining mod Microseconds_Per_Millisecond = 0 then 0 else 1);
         if Milliseconds > Word (AML_Delays.Sleep_Milliseconds'Last) then Completed := False; return; end if;
         ACPI_Native_Delays.Perform ((Kind => AML_Delays.Sleep_Delay,
           Milliseconds => AML_Delays.Sleep_Milliseconds (Milliseconds)), Result);
      end if;
      Completed := Result = AML_Delays.Completed;
   end AML_Lock_Wait;
   function AML_Lock_Last_Result return AML_Global_Locks.Outcome is
     (AML_Lock.Observe (AML_Lock_State).Last_Result);
   function AML_Lock_First_Failure return AML_Global_Locks.Outcome is (First_AML_Lock_Failure);
   function AML_Lock_Had_Failure return Boolean is (AML_Lock_Failure_Seen);
   function AML_Lock_Attempts return Natural is (Natural (AML_Lock.Observe (AML_Lock_State).Attempts));
   procedure Operate_AML_Lock
     (IO : in out Mux.Context; Domain : AML_Frame_Handles.Invocation_Domain;
      Action : AML_Global_Locks.Operation; Timeout : AML_Mutexes.Timeout;
      Outcome : in out AML_Global_Locks.Outcome) is
      use type AML_Global_Locks.Operation, AML_Global_Locks.Outcome;
   begin
      Outcome := AML_Global_Locks.Refused;
      -- Cleanup must remain possible after ordinary provider admission closes.
      if Action = AML_Global_Locks.Acquire_Lease and then not Provider_Ready (IO) then return; end if;
      AML_Lock.Operate (AML_Lock_State, Domain, Action, Timeout, Outcome);
      if Outcome in AML_Global_Locks.Succeeded | AML_Global_Locks.Timed_Out then
         if not AML_Lock_Good_Logged then
            AML_Lock_Good_Logged := True;
            ACPI_Native_Log.Copied ("acpi: stage aml-global-lock ok " & Outcome'Image &
              " attempts=" & AML_Lock_Attempts'Image, Peer_Quiet => Control_Idle, Success => True);
         end if;
      elsif not AML_Lock_Failure_Seen then
         AML_Lock_Failure_Seen := True; First_AML_Lock_Failure := Outcome;
         ACPI_Native_Log.Copied ("acpi: stage aml-global-lock fail " &
           AML_Lock.Observe (AML_Lock_State).Last_Reason'Image &
           " result=" & Outcome'Image & " attempts=" & AML_Lock_Attempts'Image,
           Peer_Quiet => Control_Idle, Success => False);
      end if;
   end Operate_AML_Lock;

   function Control_Idle return Boolean is
      S : constant Scope.Statistics := Scope.Observe (Transaction);
   begin
      return not AML_Lock.Observe (AML_Lock_State).Working
        and then not AML_Lock.Observe (AML_Lock_State).Cleanup_Required
        and then S.Depth = 0 and then not S.Working and then not S.Held
        and then not S.Cleanup_Required and then not Mux.Pending (IO);
   end Control_Idle;
   function PCI_Setup_Allowed return Boolean is
      S : constant Scope.Statistics := Scope.Observe (Transaction);
   begin
      return Field_State = Owner_Ready and then not Finished and then Scope.Ready (Transaction)
        and then S.Depth = 0 and then not S.Held and then ACPI_Local_Hardware.Admitting;
   end PCI_Setup_Allowed;
   procedure PCI_Deadlines (Setup_Milliseconds,Data_Microseconds : out Word; Available : out Boolean) is
      Budget : Lock_Protocol.Attempt_Budget;
      Setup_Available : Boolean := False;
   begin
      Setup_Milliseconds := 0; Data_Microseconds := 0; Available := False;
      Begin_Window (Data_Microseconds,Budget,Available);
      if not Available then return; end if;
      PCI_Setup_Deadline (Setup_Milliseconds,Setup_Available);
      if not Setup_Available then Setup_Milliseconds := 0; end if;
      -- Independent IPC milliseconds; never derive them from a fallback
      -- microsecond clock. This does not mint/extend the physical lease.
   end PCI_Deadlines;
   procedure Write_NVS (Physical,Virtual : Word; Width : NVS.T.Access_Width;
      Value : Word; Perform : Boolean; Result : in out ACPI_NVS_Writes.Completion) is
      FACS_Address : constant Word := (if Facts.X_FACS.Present and then Facts.X_FACS.Value /= 0
         then Facts.X_FACS.Value else Word (Facts.FACS));
      Output : Word;
      Completed : Boolean;
   begin
      Result := ACPI_NVS_Writes.Unavailable;
      if not ACPI_NVS_Write_Policy.Permitted (Physical,Width,FACS_Address) then return; end if;
      if not Perform then Result := ACPI_NVS_Writes.Ready; return; end if;
      if not Check_Field_Access (True) then return; end if;
      ACPI_Backend_Memory_Access.Transfer (Virtual,Width,True,Value,Output,Completed);
      Result := (if Completed then ACPI_NVS_Writes.Completed else ACPI_NVS_Writes.Unknown);
   end Write_NVS;

   function EC_Boot_Observation return EC_Boot_Report is (EC_Info);
   function Field_Observation return Field_Trace_Report is (Last_Field_Trace);
   procedure Note_Field (Item : AML_Field_Notes.Event) is
   begin
      ACPI_Native_Field_Profile.Note (Item);
      Last_Field_Trace.Item := Item;
      if Last_Field_Trace.Sequence = Field_Sequence'Last then
         Last_Field_Trace.Saturated := True;
      else Last_Field_Trace.Sequence := Last_Field_Trace.Sequence + 1; end if;
   end Note_Field;
   function EC_Quiet return Boolean is
     (not EC_Frame_Open and then
       (not EC_Guard_Retained or else
        (Scope.Observe(Transaction).Depth=0 and then not Scope.Pending(Transaction))));
   function EC_Owned return Boolean is
     (EC_Info.Status=EC_Ready and then EC_Frame_Open and then Scope.Ready(Transaction)
      and then Scope.Observe(Transaction).Held);
   procedure EC_Enter (For_Query,For_Write : Boolean; Deadline : EC_Types.Microseconds; Accepted : out Boolean) is
      pragma Unreferenced(For_Query);
      Window : Word;Good:Boolean;
   begin
      Accepted:=False;if EC_Info.Status/=EC_Ready or else not EC_Quiet then return;end if;
      Inspect_IO_Window(Window,Good);
      if not Good or else Deadline<=0 or else Word(Deadline)>Window then return;end if;
      EC_Outer_Depth:=Scope.Observe(Transaction).Depth;
      Scope.Begin_Field(Transaction,True,For_Write,Accepted);
      EC_Frame_Open:=Accepted;
      EC_Guard_Retained:=not Accepted and then EC_Outer_Depth=0 and then Scope.Pending(Transaction);
   end EC_Enter;
   procedure EC_Leave (Released : out Boolean) is
   begin
      Released:=False;if not EC_Frame_Open then return;end if;
      Scope.End_Field(Transaction,Released);
      EC_Frame_Open:=Scope.Observe(Transaction).Depth/=EC_Outer_Depth;
      EC_Guard_Retained:=not Released and then EC_Outer_Depth=0 and then Scope.Pending(Transaction);
   end EC_Leave;
   procedure EC_Clock (Now : out EC_Types.Microseconds; Available : out Boolean) is
      Value:Word;
   begin
      ACPI_Clock_Source.Read(Value,Available);Now:=0;
      Available:=Available and then Value<=Word(EC_Types.Microseconds'Last);
      if Available then Now:=EC_Types.Microseconds(Value);end if;
   end EC_Clock;
   procedure EC_Deadline (Until_Time : out EC_Types.Microseconds; Available : out Boolean) is
      Value:Word;
   begin
      Inspect_IO_Window(Value,Available);Until_Time:=0;
      Available:=Available and then Value>0 and then Value<=Word(EC_Types.Microseconds'Last);
      if Available then Until_Time:=EC_Types.Microseconds(Value);end if;
   end EC_Deadline;
   procedure EC_Pause (Target : EC_Types.Microseconds; Available : out Boolean) is
      Now:EC_Types.Microseconds;Result:AML_Delays.Outcome;
      use type AML_Delays.Outcome;
   begin
      EC_Clock(Now,Available);if not Available or else Now>=Target then return;end if;
      Available:=Target-Now<=EC_Types.Microseconds(AML_Delays.Stall_Microseconds'Last);
      if not Available then return;end if;
      -- Transfer a remaining duration within the SAME native delay domain;
      -- never reinterpret raw HPET/publication time as an IPC-clock deadline.
      ACPI_Native_Delays.Perform((Kind=>AML_Delays.Stall_Delay,
         Microseconds=>AML_Delays.Stall_Microseconds(Target-Now)),Result);
      Available:=Result=AML_Delays.Completed;
   end EC_Pause;
   procedure Wait_EC (Target : EC_Types.Microseconds; Available : out Boolean) renames EC_Pause;
   function EC_Cell_Allowed (Cell : EC_Types.Cell_Offset; For_Write : Boolean) return Boolean is
      pragma Unreferenced(Cell,For_Write);
   begin
      -- An early ECDT provision is a root EC address-space handler, not a
      -- namespace-device association or _REG announcement. Namespace setup
      -- must reject multiple/different interfaces before source enrollment.
      return EC_Info.Status=EC_Ready and then Admitting and then Field_State=Owner_Ready;
   end EC_Cell_Allowed;
   procedure EC_Read (Register : EC_Types.Read_Register; Value : out EC_Types.Byte;
      Result : out EC_Types.Access_Result) is
      Good:Boolean;Raw:Word;
   begin
      Value:=0;Result:=EC_Types.Refused;
      if not EC_Owned then return;end if;Scope.Check_Datum(Transaction,Good);if not Good then return;end if;
      Raw:=(if Register=EC_Types.Status_Register then
         CuBit.Handles.Devices.In_8(EC_Control_Ports,CuBit.ACPI_Windows.Port_Number(EC_Info.Control))
         else CuBit.Handles.Devices.In_8(EC_Data_Ports,CuBit.ACPI_Windows.Port_Number(EC_Info.Data)));
      if Raw<=Word(EC_Types.Byte'Last) then Value:=EC_Types.Byte(Raw);Result:=EC_Types.Completed;end if;
   end EC_Read;
   procedure EC_Write (Register : EC_Types.Write_Register; Value : EC_Types.Byte;
      Result : out EC_Types.Access_Result) is
      Good:Boolean;
   begin
      Result:=EC_Types.Refused;
      if not EC_Owned then return;end if;Scope.Check_Datum(Transaction,Good);if not Good then return;end if;
      Good:=(if Register=EC_Types.Command_Register then
         CuBit.Handles.Devices.Out_8(EC_Control_Ports,CuBit.ACPI_Windows.Port_Number(EC_Info.Control),Interfaces.Unsigned_8(Value))
         else CuBit.Handles.Devices.Out_8(EC_Data_Ports,CuBit.ACPI_Windows.Port_Number(EC_Info.Data),Interfaces.Unsigned_8(Value)));
      if Good then Result:=EC_Types.Completed;end if;
   end EC_Write;
   procedure Inspect_EC_Boot (Service : Core.State) is
      package E renames ACPI_EC_Boot_Table;
      use type E.Decode_Status,E.Register_Status,E.Path_Status;
      Table:Natural:=0;
      Inspection_Ceiling:constant Positive:=4_096;
      System_IO_Space:constant Firmware_Tables.Byte:=1;
      procedure Find_Port (Address:Word;Ports:out CuBit.Handles.IO_Ports;Found:out Boolean) is
      begin
         Ports:=CuBit.Handles.No_IO_Ports;Found:=False;
         for I in 1 .. Grant.IO_Used loop
            if Address>=Word(Grant.IO(I).First) and then Address-Word(Grant.IO(I).First)<Word(Grant.IO(I).Count) then
               Ports:=Grant.IO(I).Ports;Found:=True;return;
            end if;
         end loop;
      end Find_Port;
   begin
      if EC_Info.Status/=EC_Not_Inspected then return;end if;EC_Info.Status:=EC_Absent;
      for I in 1 .. Core.Table_Count(Service) loop
         if Core.Table_Info(Service,I).Signature="ECDT" then
            if Table/=0 then EC_Info.Status:=EC_Multiple;return;end if;Table:=I;
         end if;
      end loop;
      if Table=0 then return;end if;
      if Core.Table_Info(Service,Table).Extent>Inspection_Ceiling then EC_Info.Status:=EC_Inspection_Limit;return;end if;
      declare
         Bytes:Firmware_Tables.Bytes(1 .. Core.Table_Info(Service,Table).Extent);
         Facts:E.Description;Good,Other:Boolean;
      begin
         for Offset in Bytes'Range loop Bytes(Offset):=Core.Table_Byte(Service,Table,Offset-Bytes'First);end loop;
         Facts:=E.Decode(Bytes);
         if Facts.Status/=E.Decoded then EC_Info.Status:=EC_Description_Refused;return;end if;
         if E.Check_Registers(Bytes)/=E.Byte_Registers_Described or else Facts.Control.Space/=System_IO_Space or else Facts.Data.Space/=System_IO_Space then
            EC_Info.Status:=EC_Registers_Refused;return;
         end if;
         if E.Read_Path(Bytes).Status/=E.Parsed then EC_Info.Status:=EC_Path_Refused;return;end if;
         EC_Info.Control:=Facts.Control.Address;EC_Info.Data:=Facts.Data.Address;
         EC_Info.UID:=Facts.UID;EC_Info.GPE:=EC_Types.Byte(Facts.GPE_Bit);
         Find_Port(EC_Info.Control,EC_Control_Ports,Good);Find_Port(EC_Info.Data,EC_Data_Ports,Other);
         EC_Info.Status:=(if Good and Other then EC_Ready else EC_Port_Outside_Grant);
      end;
   end Inspect_EC_Boot;
   procedure Validate_Declaration (Item : Declaration; Geometry : AML_Field_Layout.Register_Geometry;
      Origin : out Maps.Source; Accepted : out Boolean) is
   begin
      Origin := (Item.Name,CuBit.Messages.To_Word (Self),0,Word (Item.Node));
      Accepted := Item.Node > Core.Namespace.Root
        and then Item.Region.Space = Core.Namespace.Region_Space_ID (AML_Field_Layout.Memory_Space)
        and then Geometry.Region.Space = AML_Field_Layout.Memory_Space and then not Geometry.Region.PCI.Known
        and then Geometry.Region.Base = Item.Region.Address and then Geometry.Region.Length = Item.Region.Length;
      -- Core supplies a live declaration and exact geometry. The independent
      -- platform pool/map and envelope checks provide memory authority. Zero
      -- Namespace_Revision is unknown metadata, not a claimed current revision.
   end Validate_Declaration;
   function Provider_Ready (IO : Mux.Context) return Boolean is
     (Field_State = Owner_Ready and then not AML_Lock.Observe (AML_Lock_State).Working
      and then Scope.Ready (Transaction)
      and then (Scope.Observe (Transaction).Held or else ACPI_Local_Hardware.Admitting)
      and then Mux.Is_Ready (IO));
   procedure Observe_Local_Region_Handler
     (IO : Mux.Context; Region : Core.Namespace.Node_ID;
      Data : Core.Namespace.Operation_Region_Attributes;
      Ready : in out Boolean; Generation : in out Natural) is
   begin
      Ready := False;
      Generation := 0;
      if Region = Core.Namespace.Root or else not Provider_Ready (IO) then
         return;
      end if;
      -- This is a local, read-only install snapshot. It grants no address
      -- authority; each Field transaction still uses the current mux policy.
      case Data.Space is
         when 0 | 1 | 2 =>
            Ready := True;  -- memory, IO and PCI config handlers
         when 3 =>
            Ready := EC_Info.Status = EC_Ready and then EC_Commands.Quiescent;
         when 5 =>
            Ready := CMOS_Handler_Available;
         when others =>
            null;
      end case;
      if Ready and then Local_Install_Acknowledged then
         Generation := Local_Install_Generation;
      else
         Ready := False;
      end if;
   end Observe_Local_Region_Handler;
   function EC_Handler_Available return Boolean is
     (EC_Info.Status=EC_Ready and then Cache/=null and then Completed and then Has_Anchor
      and then Provider_Ready(IO) and then EC_Commands.Quiescent);
   function Provider_Pending (IO : Mux.Context) return Boolean is
      S : constant Scope.Statistics := Scope.Observe (Transaction);
   begin
      return Mux.Pending (IO) or else AML_Lock.Observe (AML_Lock_State).Working
        or else S.Working or else S.Faulted
        or else (S.Depth = 0 and then S.Cleanup_Required and then not Scope.AML_Borrowed (Transaction));
   end Provider_Pending;
   procedure Resolve_Module (IO : in out Mux.Context; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is
   begin
      Mux.Select_Target (IO,Geometry,False,Target,Accepted);
      if not Accepted then Mux.Select_Target (IO,Geometry,True,Target,Accepted); end if;
      if not Accepted and then Geometry.Region.Space = AML_Field_Layout.PCI_Config_Space then
         Mux.Resolve_PCI (IO,Geometry,True,Target,Accepted);
      end if;
   end Resolve_Module;
   procedure Resolve_Platform (IO : in out Mux.Context; Region : Core.Namespace.Operation_Region_Attributes;
      Name : AML_Names.Segment; Node : Core.Namespace.Node_ID; Geometry : AML_Field_Layout.Register_Geometry;
      Target : in out AML_Field_Access.Target; Accepted : in out Boolean) is
      Deadline : Word;
      Budget : Lock_Protocol.Attempt_Budget;
      Available : Boolean;
   begin
      Target := AML_Field_Access.No_Target; Accepted := False;
      if not Provider_Ready (IO) then return; end if;
      Begin_Window (Deadline,Budget,Available);
      if Available then Mux.Resolve (IO,(Region,Name,Node),Geometry,Deadline,Target,Accepted); end if;
      -- Mapping deadline only; no physical lease is minted or extended here.
   end Resolve_Platform;
   procedure Begin_Field (IO : in out Mux.Context; Requires_Lock, For_Write : Boolean; Accepted : in out Boolean) is
      Since : constant ACPI_Native_Field_Profile.Tick := ACPI_Native_Field_Profile.Stamp;
   begin
      Accepted := False;
      if Provider_Ready (IO) then Scope.Begin_Field (Transaction,Requires_Lock,For_Write,Accepted); end if;
      ACPI_Native_Field_Profile.Finish (ACPI_Native_Field_Profile.Field_Entry, Since);
   end Begin_Field;
   procedure End_Field (IO : in out Mux.Context; Accepted : in out Boolean) is
      Since : constant ACPI_Native_Field_Profile.Tick := ACPI_Native_Field_Profile.Stamp;
   begin
      if Mux.Pending (IO) then Scope.Mark_Fault (Transaction); end if;
      Scope.End_Field (Transaction,Accepted); Accepted := Accepted and then not Mux.Pending (IO);
      ACPI_Native_Field_Profile.Finish (ACPI_Native_Field_Profile.Field_Exit, Since);
      if CMOS_Module_Read_Pending and then Scope.Observe (Transaction).Depth = 0 then
         CMOS_Module_Read_Pending := False;
         if Accepted and then Control_Idle then
            CMOS_Module_Read_Logged := True;
            -- Log only after the actual AML field scope has released its I/O.
            -- The direct startup handler probe bypasses Dispatch_Profile.
            CuBit.Messages.debugPrint ("acpi: cmos-aml module read status=COMPLETED cell=13 byte=" &
              CMOS_Module_Read_Value'Image & " scope-ended=TRUE" & ASCII.LF);
         end if;
      end if;
   end End_Field;
   procedure Dispatch_Profile (IO : in out Mux.Context; Item : AML_Field_Access.Request; Reply : out AML_Field_Access.Response) is
      use type AML_Field_Access.Completion, ACPI_FADT.Transactions.Access_Width;
      Since : constant ACPI_Native_Field_Profile.Tick := ACPI_Native_Field_Profile.Stamp;
      Geometry : AML_Field_Layout.Register_Geometry;
      Known : Boolean := False;
   begin
      if not Completed and then not CMOS_Module_Read_Logged and then not Item.For_Write
        and then Mux.CMOS_Available (IO)
      then
         Mux.Describe (IO,Item.Destination,Geometry,Known);
      end if;
      Mux.Dispatch (IO,Item,Reply);
      if Known and then Geometry.Region.Space = AML_Field_Layout.System_CMOS_Space
        and then Geometry.Width = ACPI_FADT.Transactions.Byte_Access
        and then Geometry.Region.Base <= 16#0D#
        and then Geometry.Byte_Offset = 16#0D# - Geometry.Region.Base
        and then Reply.Status = AML_Field_Access.Completed and then Reply.Value <= 255
      then
         CMOS_Module_Read_Value := Reply.Value; CMOS_Module_Read_Pending := True;
      end if;
      ACPI_Native_Field_Profile.Finish (ACPI_Native_Field_Profile.Field_Dispatch, Since);
   end Dispatch_Profile;
   function Observe return Field_Report is
     ((Field_State,Attempted,Completed,Finished,Provider_Pending (IO) or else ACPI_Local_Hardware.Pending,Mux.Register_Count (IO)));
   function Owns (Service : Core.State) return Boolean is
     (Completed and then Members_Completed and then Has_Anchor and then Core.Valid (Service) and then Core.Current_Context (Service,Anchor));
   function Runtime_Ready (Service : Core.State) return Boolean is
     (Finished and then Cache/=null and then Owns (Service) and then Provider_Ready (IO)
      and then Evaluation.Observe(Cache.all).Status in Evaluation.Bindings.Built|Evaluation.Bindings.Built_With_Gaps);
   function Runtime_Idle return Boolean is (Control_Idle and then not ACPI_Local_Hardware.Pending);
   procedure Claim_Runtime (Service : Core.State; Accepted : out Boolean) is
   begin
      Accepted := not Runtime_Claimed and then Runtime_Ready (Service) and then Runtime_Idle;
      if Accepted then Runtime_Claimed := True; end if;
   end Claim_Runtime;
   procedure Runtime_Evaluate
     (Service : aliased in out Core.State; Item : Core.Node_Descriptor; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Runtime_Claimed or else not Runtime_Ready (Service) or else not Runtime_Idle then return; end if;
      Evaluation.Evaluate_Named (Cache.all,Service,IO,Item,Budget,Result,Status);
   end Runtime_Evaluate;
   procedure Runtime_Invoke
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Runtime_Claimed or else not Runtime_Ready (Service) or else not Runtime_Idle then return; end if;
      Evaluation.Invoke_Retained (Cache.all,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Runtime_Invoke;
   procedure Runtime_Invoke_Values
     (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count; Budget : Natural;
      Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if not Runtime_Claimed or else not Runtime_Ready (Service) or else not Runtime_Idle then return; end if;
      Evaluation.Invoke_With_Arguments (Cache.all,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Runtime_Invoke_Values;
   function CMOS_Handler_Available return Boolean is
     (Mux.CMOS_Available (IO) and then ACPI_Native_CMOS_Ports.Available);
   procedure CMOS_Transfer (Plan : ACPI_CMOS_Transactions.Transfer;
      Value : out ACPI_CMOS_Transactions.I.Byte; Outcome : out ACPI_CMOS_Transactions.Completion) is
   begin
      Value := 0; Outcome := ACPI_CMOS_Transactions.Refused;
      if not Check_Field_Access (Plan.For_Write) then return; end if;
      ACPI_Native_CMOS_Ports.Transfer_Byte (Plan, Value, Outcome);
   end CMOS_Transfer;
   procedure Begin_Startup (Symbol : AML_Field_Access.Endpoint_ID; Accepted : out Boolean) is
      CMOS_Ready : Boolean;

   begin
      Accepted := False;
      if Field_State /= Owner_Fresh or else Attempted then return; end if;
      Field_State := Owner_Failed;
      if not ACPI_Local_Hardware.Admitting then return; end if;
      ACPI_Native_CMOS_Ports.Initialize (Facts.Century, CMOS_Ready);
      Mux.Initialize (IO,Symbol,Accepted);
      if Accepted then
         if Local_Install_Generation = Natural'Last then
            Accepted := False;
            return;
         end if;
         Local_Install_Generation := Local_Install_Generation + 1;
         Local_Install_Acknowledged := True;
         Field_State := Owner_Ready;
         if CMOS_Ready and then Mux.CMOS_Available (IO) then
            -- One bounded, read-only startup diagnostic through the actual
            -- handler and retained field scope. Status D has no read-to-clear
            -- interrupt side effect. Never bypass the mux or raw-access ports.
            declare
               Target : AML_Field_Access.Target;
               Reply : AML_Field_Access.Response;
               Selected, Begun, Ended : Boolean := False;
            begin
               Mux.Select_Target (IO,
                 ((AML_Field_Layout.System_CMOS_Space,0,64,(Known => False)),
                  16#0D#,ACPI_FADT.Transactions.Byte_Access),False,Target,Selected);
               if Selected then Begin_Field (IO,False,False,Begun); end if;
               if Begun then
                  Mux.Dispatch (IO,(Target,False,0),Reply);
                  Ended := True; End_Field (IO,Ended);
                  CuBit.Messages.debugPrint ("acpi: cmos-handler probe status=" & Reply.Status'Image &
                    " byte=" & Reply.Value'Image & " scope-ended=" & Ended'Image & ASCII.LF);
               else
                  CuBit.Messages.debugPrint ("acpi: cmos-handler probe unavailable selected=" & Selected'Image & ASCII.LF);
               end if;
            end;
         end if;
      end if;
   end Begin_Startup;
   procedure Execute_Module_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Execution_Report; Status : out Core.Values.Access_Status) is
   begin
      Result := (others => <>); Status := Core.Values.Wrong_Phase;
      if Attempted or else Finished then return; end if;
      Attempted := True;
      if not Core.Valid (Service) or else not Provider_Ready (IO) then Field_State := Owner_Failed; return; end if;
      Inspect_EC_Boot (Service);
      ACPI_Native_Field_Profile.Start;
      Evaluation.Bindings.Startup.Execute_Frozen (Service,IO,Budget,Result,Status);
      ACPI_Native_Field_Profile.Stop;
      Completed := Status = Core.Values.Available and then Result.Phase = Core.Module_Execution_Reported
        and then Result.Modules.Outcome = Core.Values.Modules_Completed;
      if not Completed then Field_State := Owner_Failed; return; end if;
      -- Members are initialized only after this callback returns. A root
      -- descriptor requires Ready and cannot anchor the Catalog_Executed Core.
      -- Capture its opaque owner identity without changing the execution result.
      Anchor := Core.Capture_Context (Service);
      Has_Anchor := Anchor /= Core.No_Context;
      if not Has_Anchor then Field_State := Owner_Failed; Status := Core.Values.Wrong_Phase; end if;
   exception when others => ACPI_Native_Field_Profile.Stop; raise;
   end Execute_Module_Startup;
   procedure Initialize_Member_Startup (Service : in out Core.State; Budget : Natural;
      Result : out Core.Namespace.Field_Initialization_Report; Status : out Core.Values.Access_Status) is
   begin
      Result := (others => <>);Status := Core.Values.Wrong_Phase;
      if Members_Attempted or else Finished then return;end if;
      Members_Attempted := True;
      if Provider_Pending(IO) or else ACPI_Local_Hardware.Pending then
         Result.Failure := AML_Execute.Field_Indeterminate;
         Result.Reconciliation_Required := True;Status := Core.Values.Busy;
         Field_State := Owner_Failed;return;
      end if;
      if not Completed or else not Has_Anchor or else not Core.Valid(Service)
        or else not Core.Current_Context(Service,Anchor) or else not Provider_Ready(IO)
      then return;end if;
      Evaluation.Bindings.Startup.Initialize_Members(Service,IO,Budget,Result,Status);
      Members_Completed := Status=Core.Values.Available
        and then Result.Outcome=Core.Namespace.Initialization_Completed
        and then Result.Members.Missing=0 and then Result.Members.Unsupported=0
        and then not Result.Fatal.Present and then not Result.Reconciliation_Required;
      if not Members_Completed then Field_State := Owner_Failed;end if;
   end Initialize_Member_Startup;
   function Ensure_Cache(Service:Core.State) return Boolean is
      Info:Evaluation.Allocation_Report;
   begin
      if Cache_Attempted then return Cache/=null;end if;
      Cache_Attempted:=True;
      Evaluation.Allocate(Service,IO,Metadata_Capacity_Ceiling,Metadata_Byte_Ceiling,Cache,Info);
      Cache_Capacity:=Info.Capacity;Cache_Bytes:=Info.Bytes;Allocated_Cache_Bytes:=Info.Allocated_Bytes;
      Cache_Status:=(case Info.Status is
         when Evaluation.Allocated=>Prepared,
         when Evaluation.Size_Unavailable=>Binding_Size_Unavailable,
         when Evaluation.Capacity_Exceeded=>Binding_Capacity_Exceeded,
         when Evaluation.Allocation_Refused=>Binding_Allocation_Refused);
      -- Reachable process-lifetime owner: retain through runtime/failure and
      -- uncertain cleanup. Process exit reclaims its CuAlloc storage.
      return Cache/=null;
   end Ensure_Cache;
   procedure Prepare_Fields (Service : Core.State; Accepted : out Boolean) is
      Details : Evaluation.Bindings.Report;
   begin
      Accepted := False;
      Last_Preparation :=
        (Module_Completed => Completed, Anchor_Present => Has_Anchor,
         Core_Valid => Core.Valid (Service),
         Context_Current => Core.Valid (Service) and then Core.Current_Context (Service, Anchor),
         Owner_Ready => Field_State = Owner_Ready,
         Scope_Ready => Scope.Ready (Transaction), Mux_Ready => Mux.Is_Ready (IO),
         Hardware_Admitting => ACPI_Local_Hardware.Admitting, others => <>);
      if Runtime_Claimed then
         Last_Preparation.Status := Runtime_Already_Claimed; return;
      elsif not Owns (Service) then
         Last_Preparation.Status := Startup_Owner_Mismatch; return;
      elsif not Provider_Ready (IO) then
         Last_Preparation.Status := Provider_Not_Ready; return;
      end if;
      if not Ensure_Cache(Service) then
         Last_Preparation.Status:=Cache_Status;
         Last_Preparation.Binding_Limit:=Cache_Capacity;
         Last_Preparation.Metadata_Bytes:=Cache_Bytes;return;
      end if;
      Last_Preparation.Binding_Limit:=Cache_Capacity;
      Last_Preparation.Metadata_Bytes:=Cache_Bytes;
      Last_Preparation.Allocated_Metadata_Bytes:=Allocated_Cache_Bytes;
      Evaluation.Prepare (Cache.all,Service,IO,Accepted);
      Details := Evaluation.Observe (Cache.all);
      Last_Preparation.Failed_Node := Details.Failed_Node;
      Last_Preparation.Access_Failure := Details.Failure;
      Last_Preparation.Region_Entries := Details.Region_Entries;
      Last_Preparation.Read_Fields := Details.Read_Fields;
      Last_Preparation.Write_Fields := Details.Write_Fields;
      Last_Preparation.Missing_Fields := Details.Missing_Fields;
      if Accepted then Last_Preparation.Status := Prepared;
      elsif Provider_Pending (IO) then Last_Preparation.Status := Provider_IO_Pending;
      else
         case Details.Status is
            when Evaluation.Bindings.Not_Attempted | Evaluation.Bindings.Already_Consumed =>
               Last_Preparation.Status := Binding_Cache_Not_Published;
            when Evaluation.Bindings.Provider_Unavailable =>
               Last_Preparation.Status := Binding_Provider_Unavailable;
            when Evaluation.Bindings.Pending_IO =>
               Last_Preparation.Status := Binding_Pending_IO;
            when Evaluation.Bindings.Namespace_Unavailable =>
               Last_Preparation.Status := Binding_Namespace_Unavailable;
            when Evaluation.Bindings.Binding_Refused =>
               Last_Preparation.Status := Binding_Metadata_Refused;
            when Evaluation.Bindings.Built | Evaluation.Bindings.Built_With_Gaps =>
               Last_Preparation.Status := Binding_Export_Refused;
         end case;
      end if;
   end Prepare_Fields;
   procedure Drain_Late_Regions
     (Service : aliased in out Core.State;
      Turn_ID : Positive;
      Report : out Late_Region_Report) is
      Inner : Evaluation.Bindings.Fields.Connection_Turn_Report;
   begin
      Report := (others => <>);
      if not Local_Install_Acknowledged or else Local_Install_Generation = 0
        or else Cache = null
      then return; end if;
      if Runtime_Claimed then
         if not Runtime_Ready (Service) or else not Runtime_Idle then return; end if;
      elsif not Owns (Service) or else not Provider_Ready (IO)
        or else Provider_Pending (IO)
      then return; end if;
      -- The local mux is installed and currently admitting. The per-region
      -- observer still checks each actual handler before any AML callback.
      Evaluation.Drain_Region_Connections
        (Cache.all, Service, IO, Turn_ID, Positive (Local_Install_Generation),
         Install_Acknowledged => Local_Install_Acknowledged, Report => Inner);
      Report := (Accepted => Inner.Accepted, Attempted => Inner.Attempted,
         Connected => Inner.Connected, Failed => Inner.Failed, More => Inner.More);
   end Drain_Late_Regions;
   function Preparation_Observation return Preparation_Report is (Last_Preparation);
   procedure Evaluate_Named (Service : aliased in out Core.State; Item : Core.Node_Descriptor;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
      Ready:Boolean;
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if Runtime_Claimed or else not Owns (Service) or else not Provider_Ready (IO) then return; end if;
      if Cache=null then Prepare_Fields(Service,Ready);if not Ready then return;end if;end if;
      Evaluation.Evaluate_Named (Cache.all,Service,IO,Item,Budget,Result,Status);
   end Evaluate_Named;
   procedure Invoke_Retained (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
      Ready:Boolean;
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if Runtime_Claimed or else not Owns (Service) or else not Provider_Ready (IO) then return; end if;
      if Cache=null then Prepare_Fields(Service,Ready);if not Ready then return;end if;end if;
      Evaluation.Invoke_Retained (Cache.all,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Invoke_Retained;
   procedure Invoke_With_Arguments (Service : aliased in out Core.State; Node : Core.Namespace.Node_ID;
      Args : Core.Values.Arguments; Argument_Count : Core.Values.Argument_Count;
      Budget : Natural; Result : out Core.Values.Result; Status : out Core.Values.Access_Status) is
      Ready:Boolean;
   begin
      Result := (AML_Execute.Unsupported_Value,0); Status := Core.Values.Wrong_Phase;
      if Runtime_Claimed or else not Owns (Service) or else not Provider_Ready (IO) then return; end if;
      if Cache=null then Prepare_Fields(Service,Ready);if not Ready then return;end if;end if;
      Evaluation.Invoke_With_Arguments (Cache.all,Service,IO,Node,Args,Argument_Count,Budget,Result,Status);
   end Invoke_With_Arguments;
   procedure Finish_Startup (Accepted : out Boolean) is
   begin
      Accepted := Completed and then Members_Completed and then Has_Anchor and then not Finished and then Provider_Ready (IO);
      if Accepted then Mux.Close_Setup (IO); Finished := True; end if;
   end Finish_Startup;
   procedure Close is
      Stopped:Boolean;
   begin
      Local_Install_Acknowledged := False;
      Field_State := Owner_Closed;
      EC_Commands.Request_Stop(Stopped);
   end Close;
   procedure Cleanup (Completed : out Boolean) is
      Global_Result : AML_Global_Locks.Outcome;
      use type AML_Global_Locks.Outcome;
   begin
      Local_Install_Acknowledged := False;
      Field_State := Owner_Closed;
      AML_Lock.Maintain (AML_Lock_State, Global_Result);
      Scope.Cleanup (Transaction,Completed);
      EC_Commands.Reconcile;
      Completed := Completed and then Global_Result = AML_Global_Locks.Succeeded
        and then not Mux.Pending (IO) and then EC_Commands.Quiescent;
   end Cleanup;
   function Uncertain return AML_Field_Access.Observation is (Mux.Uncertain (IO));
end ACPI_Local_Hardware.Pooled_Fields;
