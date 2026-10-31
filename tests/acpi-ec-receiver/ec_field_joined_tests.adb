with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_Lid;
with ACPI_EC_Types;
with ACPI_EC_Transactions;
with ACPI_EC_Registers;
with ACPI_EC_Endpoint;
with ACPI_EC_Event_Types;
with ACPI_EC_Event_Publisher;
with ACPI_EC_Event_Receiver;
with ACPI_EC_Methods;
with ACPI_EC_Native_Calls;
with CuBit.Handles.IPC;
with CuBit.Handles.Testing;
with CuBit.Messages;
with CuBit.Process_IDs;
with ACPI_EC_Field_Bindings;
with AML_Decode;
with ACPI_Field_Remote_Provider;
with ACPI_Region_Protocol;
with ACPI_FADT.Transactions;
with AML_Delays;
with AML_Execute;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Field_Targets;
with AML_Names;
with Firmware_Tables;
procedure EC_Field_Joined_Tests is
   package EC renames ACPI_EC_Types;
   package Wire renames ACPI_Region_Protocol;
   package A renames AML_Field_Access;
   package T renames ACPI_FADT.Transactions;
   use type EC.Byte, EC.Microseconds, EC.Read_Register, EC.Write_Register;
   use type Wire.Time_Instant;
   use type Interfaces.Unsigned_64;
   Checks, IO_Count, Wire_Calls : Natural := 0;
   Now : EC.Microseconds := 1;
   Last_Write : EC.Microseconds := 0;
   Input_Pending, Output_Ready, Fail_Status : Boolean := False;
   Stage : Natural := 0;
   Command, Written : EC.Byte := 0;
   Cell_Value : EC.Byte := 16#A4#;
   Live, Query_Driving, Has_Event : Boolean := False;
   Event_Number : EC.Byte := 1;
   Query_Commands : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC joined check" & Checks'Image; end if; end Check;
   procedure Clock (Time : out EC.Microseconds; Available : out Boolean) is
   begin Time := Now; Available := True; end Clock;
   function Owned return Boolean is (True);
   procedure Read (Register : EC.Read_Register; Value : out EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (Live or Query_Driving);
      IO_Count := IO_Count + 1; Value := 0; Result := EC.Completed;
      if Fail_Status then Result := EC.Indeterminate; return; end if;
      if Register = EC.Status_Register then
         if Input_Pending then
            Check (Now >= Last_Write + EC.Input_Assertion_Delay);
            Input_Pending := False;
            if Stage = 1 and Command = EC.Query_Command then
               Output_Ready := True;
            elsif Stage = 2 then
               Check (Written = 16#10#);
               if Command = EC.Read_Command then Output_Ready := True; end if;
            elsif Stage = 3 then
               Check (Command = EC.Write_Command); Cell_Value := Written; Stage := 0;
            end if;
         end if;
         if Output_Ready then Value := EC.Output_Full; end if;
         if Has_Event then Value := Value or EC.Event_Pending; end if;
      else
         Check (Output_Ready); Output_Ready := False;
         if Command = EC.Query_Command then Value := Event_Number; Has_Event := False;
         else Value := Cell_Value; end if;
         Stage := 0;
      end if;
   end Read;
   procedure Write (Register : EC.Write_Register; Value : EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (Live or Query_Driving);
      IO_Count := IO_Count + 1; Check (not Input_Pending and not Output_Ready);
      Input_Pending := True; Last_Write := Now; Written := Value; Stage := Stage + 1;
      if Stage = 1 then
         Check (Register = EC.Command_Register and (Value = EC.Read_Command or Value = EC.Write_Command or Value = EC.Query_Command)); Command := Value;
         if Value = EC.Query_Command then Query_Commands := Query_Commands + 1; end if;
      else Check (Register = EC.Data_Register); end if;
      Result := EC.Completed;
   end Write;
   package Engine is new ACPI_EC_Transactions (Read, Write, Clock, Owned);
   package Backend is new ACPI_EC_Registers (Engine);
   use type Backend.Owner_ID;
   package Endpoint is new ACPI_EC_Endpoint (Backend);
   Sender : constant Backend.Owner_ID := 16#ABCD_1234_0000_0001#;
   Register_ID : constant A.Register_ID := 16#FEDC_BA98_7654_3210#;
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   use type H.Reply, Interfaces.Unsigned_8;
   Save_Allowed, Delivery_Allowed : Boolean := True;
   Saves, Answers, Current_Answers : Natural := 0;
   Live_Handle : H.Reply := H.No_Reply;
   Received_Sender : M.Process_ID := CuBit.Process_IDs.From_Word (Interfaces.Unsigned_64 (Sender));
   Last_Saved, Last_Current : M.Message := M.NULL_MESSAGE;
   function Save return H.IPC.Saved_Reply is
   begin
      Saves := Saves + 1; Check (not Live);
      if not Save_Allowed then return (Saved => False, Why => H.ABI.Table_Full); end if;
      Live := True;
      Live_Handle := H.Testing.Reply_For_Test (2 ** 32 + Interfaces.Unsigned_64 (Saves));
      return (Saved => True, Handle => Live_Handle);
   end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin
      Check (Live and Item = Live_Handle); Live := False; Live_Handle := H.No_Reply;
      Answers := Answers + 1; Last_Saved := Msg; return Delivery_Allowed;
   end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Interfaces.Unsigned_64 is
   begin
      Check (M.To_Word (From) = M.To_Word (Received_Sender));
      Current_Answers := Current_Answers + 1; Last_Current := Msg; return 1;
   end Answer_Current;
   package Native is new ACPI_EC_Native_Calls (Backend, Endpoint, Save, Answer_Saved, Answer_Current);
   use type Native.Outcome;
   function Native_Message (Item : Wire.Packet) return M.Message is
      Msg : M.Message := M.NULL_MESSAGE;
   begin
      Check (Item.Flags = 0);
      Msg.tag := (label => Item.Label, length => Item.Length, protocol => 0, ipc => <>, reserved => Item.Reserved);
      Msg.words := M.MessageWords (Item.Data); return Msg;
   end Native_Message;
   procedure Drive (Result : out Native.Report) is
   begin
      for Step in 1 .. 1000 loop
         Native.Advance (Result);
         if Result.Status /= Native.Awaiting_Transaction then return; end if;
         Now := Now + 1;
      end loop;
      raise Program_Error with "bounded native EC owner did not complete";
   end Drive;
   package Events renames ACPI_EC_Event_Types;
   use type Events.Publication, Events.Token_ID, Events.Controller_ID, Events.Epoch_ID, Events.Event_Number;
   use type Backend.Submission;
   Enqueue_Calls, Backpressure_Left : Natural := 0;
   Unknown_Publication, Queued : Boolean := False;
   Queued_Notice : Events.Notice;
   procedure Enqueue (Item : Events.Notice; Result : out Events.Publication) is
   begin
      Check (not Backend.Active and not Query_Driving);
      Enqueue_Calls := Enqueue_Calls + 1;
      if Backpressure_Left > 0 then
         Backpressure_Left := Backpressure_Left - 1; Result := Events.Backpressure; return;
      end if;
      Check (not Queued); Queued := True; Queued_Notice := Item;
      Result := (if Unknown_Publication then Events.Indeterminate else Events.Enqueued);
   end Enqueue;
   Consumer_PID : constant CuBit.Process_IDs.Process_ID :=
     CuBit.Process_IDs.From_Word (16#ABCD_0000_0000_0042#);
   package Publisher is new ACPI_EC_Event_Publisher (Backend, Enqueue, Consumer_PID);
   package Invalid_Publisher is new ACPI_EC_Event_Publisher (Backend, Enqueue);
   use type Publisher.Phase, Invalid_Publisher.Phase;
   procedure Query_Event (Number : EC.Byte; Attempts : Events.Delivery_Budget := 3) is
      Result : Backend.Submission;
   begin
      Has_Event := True; Event_Number := Number;
      Publisher.Start_Query (Now + 1000, 1000, Attempts, Result);
      Check (Result = Backend.Started);
      for Step in 1 .. 1000 loop
         Query_Driving := True; Publisher.Advance; Query_Driving := False;
         exit when Publisher.Observe.Stage /= Publisher.Querying;
         Now := Now + 1;
      end loop;
      Check (Publisher.Observe.Stage /= Publisher.Querying);
   end Query_Event;
   type Transport is limited null record;
   Wrong_Sender : Boolean := False;
   procedure Exchange
     (State : in out Transport; Destination : Natural; Deadline : Wire.Time_Instant;
      Item : Wire.Packet; Reply : out Wire.Packet; Delivered : out Wire.Delivery) is
      pragma Unreferenced (State);
      Result : Native.Report;
      Msg : M.Message;
   begin
      Check (Destination = 1 and Deadline = 99); Wire_Calls := Wire_Calls + 1;
      Received_Sender := CuBit.Process_IDs.From_Word
        (Interfaces.Unsigned_64 (if Wrong_Sender then Sender - 16#1000_0000_0000# else Sender));
      Native.Begin_Call (Received_Sender, Native_Message (Item), Now + 1000, 1000, Result);
      if Result.Status = Native.Awaiting_Transaction then Drive (Result); end if;
      Check (Result.Status in Native.Immediate_Reply | Native.Completion_Reply | Native.Save_Refused);
      Msg := (if Result.Status = Native.Save_Refused then Last_Current else Last_Saved);
      Reply := (Label => Msg.tag.label, Length => Msg.tag.length, Flags => M.Flag_Byte (Msg.tag),
                Reserved => Msg.tag.reserved, Data => Wire.Words (Msg.words));
      Delivered := (if Result.Delivered then Wire.Reply_Received else Wire.Completion_Unknown);
   end Exchange;
   procedure Window (Time, Deadline : out Wire.Time_Instant) is
   begin Time := 10; Deadline := 99; end Window;
   package Provider is new ACPI_Field_Remote_Provider (Transport, Natural, 0, Exchange, Window);
   package Bindings is new ACPI_EC_Field_Bindings (Backend, Provider);
   use type Bindings.Outcome, A.Register_ID, T.Access_Width;
   use type AML_Field_Layout.Region_Geometry;
   package C is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64,
      Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 4);
   package Fields is new C.Field_IO
     (Provider.Context, Provider.Pending, Provider.Describe, Provider.Dispatch,
      Provider.Reconciled, Authorize_Target => Provider.Authorize);
   Service : aliased C.State (1, 4096, 4096, 1);
   IO : Provider.Context;
   Reads, Writes : Fields.Binding_Set;
   Evaluations, Releases, Release_Failures : Natural := 0;
   Inflate_Charge : Boolean := False;
   procedure Evaluate
     (State : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Evaluations := Evaluations + 1;
      Fields.Evaluate_Named (State, IO, Reads, Item, Budget, Result, Status);
      if Inflate_Charge then Result.Charged := Budget + 1; end if;
   end Evaluate;
   procedure Release (State : in out C.State; Handle : in out C.Values.Value_Handle;
                      Status : out C.Values.Access_Status) is
   begin
      Releases := Releases + 1;
      if Release_Failures > 0 then
         Release_Failures := Release_Failures - 1; Status := C.Values.Invalid_Value;
      else C.Release_Result (State, Handle, Status); end if;
   end Release;
   package Lid is new ACPI_Lid (C, Evaluate);
   package Methods is new ACPI_EC_Methods (C, Evaluate, Release);
   package Receiver is new ACPI_EC_Event_Receiver (C, Methods, 2);
   use type Receiver.Enrollment, Receiver.Outcome, Receiver.Close_Result;
   Publisher_PID : constant M.Process_ID := CuBit.Process_IDs.From_Word (Interfaces.Unsigned_64 (Sender));
   Other : aliased C.State (1, 4096, 4096, 1);
   use type Methods.Invocation_Outcome;
   use type C.Stage_Status, C.Freeze_Status, C.Values.Module_Outcome, C.Values.Access_Status;
   use type AML_Execute.Execution_Status, Lid.Query_Outcome, Lid.Lid_State;

   function Find (Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin
      C.Find_Child (Service, Parent, Name, Node, Status); Check (Status = C.Values.Available);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available); return Item;
   end Find;
   Accepted : Boolean;
   EC_Device, Lid_Device : C.Node_Descriptor;
   Field : C.Values.Field_Description; Key : C.Values.Field_Key;
   Status : C.Values.Access_Status;
   Targets : AML_Field_Targets.Vector := [others => A.No_Target];
   Sample : Lid.Query_Report;
   Result : AML_Execute.Execution_Result;
   Before : Natural;
begin
   Check (Ada.Command_Line.Argument_Count = 1);
   declare OK : Boolean; Result : Backend.Submission; begin
      Publisher.Start_Query (Now + 1000, 1000, 3, Result);
      Check (Result = Backend.Busy and IO_Count = 0);
      Invalid_Publisher.Configure (0, 1, OK); Check (not OK);
      Invalid_Publisher.Configure (1, 1, OK); Check (not OK);
      Check (Invalid_Publisher.Observe.Stage = Invalid_Publisher.Configuration_Refused);
      Publisher.Configure (Events.Controller_ID'Last, Events.Epoch_ID'Last, OK); Check (OK);
      Publisher.Configure (1, 1, OK); Check (not OK);
   end;

   declare
      Result : Native.Report;
      Msg : M.Message := Native_Message ((Label => Wire.Read_Operation, Data => [1, Interfaces.Unsigned_64 (Register_ID), 0, 0], others => <>));
      Before_IO : constant Natural := IO_Count;
   begin
      Native.Advance (Result); Check (Result.Status = Native.Idle and not Native.Pending);
      Save_Allowed := False;
      Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Result);
      Check (Result.Status = Native.Save_Refused and Result.Delivered and not Native.Pending and not Backend.Active);
      Check (IO_Count = Before_IO and Current_Answers = 1);
      Save_Allowed := True;
      Msg.tag.ipc.Accepts_Capability := True;
      Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Result);
      Check (Result.Status = Native.Immediate_Reply and Result.Delivered and not Native.Pending);
      Check (Last_Saved.words (0) = Interfaces.Unsigned_64 (Wire.Outcome'Pos (Wire.Malformed)) and IO_Count = Before_IO);
   end;

   declare
      Region : AML_Field_Layout.Region_Geometry :=
        (Space => AML_Field_Layout.Embedded_Control_Space, Base => 0, Length => 256);
      Item : Backend.Admission :=
        (Register => Backend.Register_ID'Last, Epoch => Backend.Epoch_ID'Last,
         Owner => Sender, Cell => 0, Readable => True, Writable => True,
         Exact_Write_Safe => True, Write_Mask => 16#0F#);
      Original : constant Backend.Admission := Item;
      Full : constant AML_Field_Layout.Region_Geometry := Region;
      Built : Bindings.Result;
      Expected : Bindings.Outcome;
   begin
      for Cell in EC.Cell_Offset loop
         Item.Cell := Cell;
         Built := Bindings.Build (Item, Sender, Full);
         Check (Built.Status = Bindings.Ready);
         Check (Built.Item.Register = A.Register_ID'Last
           and Built.Item.Epoch = AML_Field_Access.Counter'Last);
         Check (Built.Item.Geometry.Region = Full
           and Built.Item.Geometry.Byte_Offset = AML_Decode.Integer_Value (Cell)
           and Built.Item.Geometry.Width = T.Byte_Access);
         Check (Built.Item.Readable and Built.Item.Writable
           and Built.Item.Exact_Write_Safe and Built.Item.Write_Mask = 16#0F#);
         Region := (Space => AML_Field_Layout.Embedded_Control_Space,
                    Base => AML_Decode.Integer_Value (Cell), Length => 1);
         Built := Bindings.Build (Item, Sender, Region);
         Check (Built.Status = Bindings.Ready and then Built.Item.Geometry.Byte_Offset = 0);
      end loop;
      for Mutation in 1 .. 16 loop
         Item := Original; Item.Cell := 16; Region := Full;
         Expected := Bindings.Invalid_Admission;
         case Mutation is
            when 1 => Item.Register := 0;
            when 2 => Item.Epoch := 0;
            when 3 => Item.Owner := 0;
            when 4 => Item.Owner := Sender - 16#1000_0000_0000#;
                      Expected := Bindings.Owner_Mismatch;
            when 5 => Item.Readable := False; Item.Writable := False; Item.Write_Mask := 0;
            when 6 => Item.Exact_Write_Safe := False;
            when 7 => Item.Writable := False;
            when 8 => Region.Space := AML_Field_Layout.IO_Space; Expected := Bindings.Invalid_Region;
            when 9 => Region.Length := 0; Expected := Bindings.Invalid_Region;
            when 10 => Region.Base := 256; Expected := Bindings.Invalid_Region;
            when 11 => Region.Length := 257; Expected := Bindings.Invalid_Region;
            when 12 => Region.Base := 17; Region.Length := 1; Expected := Bindings.Cell_Outside_Region;
            when 13 => Region.Base := 15; Region.Length := 1; Expected := Bindings.Cell_Outside_Region;
            when 14 => Region.Base := AML_Decode.Integer_Value'Last; Expected := Bindings.Invalid_Region;
            when 15 => Region.Length := AML_Decode.Integer_Value'Last; Expected := Bindings.Invalid_Region;
            when 16 => Region.Base := 255; Region.Length := 2; Expected := Bindings.Invalid_Region;
         end case;
         Built := Bindings.Build (Item, Sender, Region); Check (Built.Status = Expected);
      end loop;
      Item := Original;
      Check (Bindings.Build (Item, 0, Full).Status = Bindings.Invalid_Admission);
      Item.Writable := False; Item.Write_Mask := 0; Item.Exact_Write_Safe := False;
      Built := Bindings.Build (Item, Sender, Full);
      Check (Built.Status = Bindings.Ready and then Built.Item.Readable
        and then not Built.Item.Writable and then Built.Item.Write_Mask = 0);
      Check (IO_Count = 0 and Wire_Calls = 0);
   end;

   declare
      File : Ada.Streams.Stream_IO.File_Type; Stream : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset;
      Staged : C.Stage_Status; Frozen : C.Freeze_Status; Report : C.Execution_Report;
      Members : C.Namespace.Initialization_Report;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Stream, Last); Ada.Streams.Stream_IO.Close (File);
      declare Data : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Stream (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Data, Staged); Check (Staged = C.Staged);
      end;
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Service, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available);
   end;
   EC_Device := Find (C.Namespace.Root, "EC00"); Lid_Device := Find (C.Namespace.Root, "LID0");
   Provider.Initialize (IO, 1, 7, Accepted); Check (Accepted);
   declare
      Region : constant AML_Field_Layout.Region_Geometry :=
        (Space => AML_Field_Layout.Embedded_Control_Space, Base => 0, Length => 256);
      Admitted : constant Backend.Admission :=
        (Register => Backend.Register_ID (Register_ID), Epoch => 1, Owner => Sender,
         Cell => 16#10#, Readable => True, Writable => True,
         Exact_Write_Safe => True, Write_Mask => 16#FF#);
      Built : constant Bindings.Result := Bindings.Build (Admitted, Sender, Region);
   begin
      Check (Built.Status = Bindings.Ready);
      Backend.Install (Admitted, Accepted); Check (Accepted);
      Provider.Install (IO, Built.Item, Accepted); Check (Accepted);
   end;
   declare
      Result : Native.Report;
      Msg : constant M.Message := Native_Message ((Label => Wire.Read_Operation, Data => [1, Interfaces.Unsigned_64 (Register_ID), 0, 0], others => <>));
      Before_Saves, Before_Answers, Before_IO : Natural;
   begin
      Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Result);
      Check (Result.Status = Native.Awaiting_Transaction and Native.Pending and Live);
      declare Result : Backend.Submission; begin
         Publisher.Start_Query (Now + 1000, 1000, 3, Result);
         Check (Result = Backend.Busy and Publisher.Observe.Stage = Publisher.Ready and Native.Pending);
      end;
      Before_Saves := Saves; Before_Answers := Answers;
      Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Result);
      Check (Result.Status = Native.Immediate_Reply and Result.Delivered);
      Check (Saves = Before_Saves and Answers = Before_Answers and Native.Pending and Live);
      Drive (Result); Check (Result.Status = Native.Completion_Reply and Result.Delivered);
      Check (not Native.Pending and not Live and not Backend.Active);
      Check (Last_Saved.words (0) = Interfaces.Unsigned_64 (Wire.Outcome'Pos (Wire.Done)) and Last_Saved.words (1) = 16#A4#);
      Before_IO := IO_Count; Before_Answers := Answers;
      Native.Advance (Result); Check (Result.Status = Native.Idle and IO_Count = Before_IO and Answers = Before_Answers);
      Delivery_Allowed := False;
      Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Result);
      Check (Result.Status = Native.Awaiting_Transaction); Drive (Result);
      Check (Result.Status = Native.Completion_Reply and not Result.Delivered and not Native.Pending and not Live);
      Before_IO := IO_Count; Before_Answers := Answers;
      Native.Advance (Result); Check (Result.Status = Native.Idle and IO_Count = Before_IO and Answers = Before_Answers);
      Delivery_Allowed := True;
   end;
   Targets (1) := (Provider.Identity (IO), 7, 1, Register_ID);
   declare Result : Receiver.Enrollment; Scope : constant C.Node_Descriptor := Find (C.Namespace.Root, "EC01"); begin
      Receiver.Install (Service, 0, 1, Publisher_PID, EC_Device, Result); Check (Result = Receiver.Invalid_Identity);
      Receiver.Install (Service, 1, 1, CuBit.Process_IDs.No_Process, Scope, Result); Check (Result = Receiver.Invalid_Identity);
      Receiver.Install (Other, 1, 1, Publisher_PID, Scope, Result); Check (Result = Receiver.Invalid_Scope);
      Receiver.Install (Service, Events.Controller_ID'Last, Events.Epoch_ID'Last, Publisher_PID, EC_Device, Result);
      Check (Result = Receiver.Installed);
      Receiver.Install (Service, Events.Controller_ID'Last, Events.Epoch_ID'Last, Publisher_PID, EC_Device, Result);
      Check (Result = Receiver.Already_Active);
      Receiver.Install (Service, 2, 1, Publisher_PID, EC_Device, Result); Check (Result = Receiver.Scope_Conflict);
      Receiver.Install (Service, 1, 1, Publisher_PID, Scope, Result); Check (Result = Receiver.Installed);
      Receiver.Install (Service, 2, 1, Publisher_PID, Find (C.Namespace.Root, "EC02"), Result);
      Check (Result = Receiver.Capacity_Exhausted);
   end;

   C.Read_Field_Descriptor (Service, C.Values.Position (Find (C.Values.Position (EC_Device), "LIDS")), Field, Key, Status);
   Check (Status = C.Values.Available);
   Fields.Add_Read_Binding (Service, IO, Key, Targets, 1, Reads, Status); Check (Status = C.Values.Available);
   Fields.Add_Write_Binding (Service, IO, Key, Targets, 1, Writes, Status); Check (Status = C.Values.Available);
   Lid.Query (Service, Lid_Device, 1000, Sample); Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Closed);
   declare
      Before_IO, Before_Calls, Before_Queries : Natural;
      OK : Boolean;
      Submission : Backend.Submission;
      Saved : Events.Notice;
      procedure Handle_Queued (Count : AML_Decode.Integer_Value) is
         Run, Repeated : Receiver.Delivery;
         Result : Receiver.Outcome;
         Before : Natural;
         Changed : Events.Notice;
         Value : C.Values.Result; Notification : C.Notification_Observation;
         Present : Boolean;
      begin
         Check (Queued and Queued_Notice.Number = 1);
         Check (Queued_Notice.Controller = Events.Controller_ID'Last
           and Queued_Notice.Epoch = Events.Epoch_ID'Last and Queued_Notice.Token /= 0);
         Queued := False;
         Before := Evaluations;
         Changed := Queued_Notice; Changed.Token := Events.Token_ID'Last;
         Receiver.Dispatch (Run, Service,
           CuBit.Process_IDs.From_Word (Interfaces.Unsigned_64 (Sender - 16#1000_0000_0000#)), Changed, 1000, Result);
         Check (Result = Receiver.Wrong_Sender and not Receiver.Consumed (Run) and Evaluations = Before);
         Changed := Queued_Notice; Changed.Epoch := Changed.Epoch - 1;
         Receiver.Dispatch (Run, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result = Receiver.Wrong_Epoch and not Receiver.Consumed (Run));
         Changed := Queued_Notice; Changed.Controller := 2;
         Receiver.Dispatch (Run, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result = Receiver.Unknown_Controller and not Receiver.Consumed (Run));
         Changed := Queued_Notice; Changed.Token := 0;
         Receiver.Dispatch (Run, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result = Receiver.Invalid_Token and not Receiver.Consumed (Run));
         Receiver.Dispatch (Run, Other, Publisher_PID, Queued_Notice, 1000, Result);
         Check (Result = Receiver.Scope_Changed and not Receiver.Consumed (Run));
         Receiver.Dispatch (Run, Service, Publisher_PID, Queued_Notice, 1000, Result);
         Check (Result = Receiver.Completed and Receiver.Consumed (Run) and not Receiver.Pending (Run));
         Check (Evaluations = Before + 1 and C.Retained_Results (Service) = 0);
         Receiver.Dispatch (Run, Service, Publisher_PID, Queued_Notice, 1000, Result);
         Check (Result = Receiver.Already_Consumed and Evaluations = Before + 1);
         Changed := Queued_Notice;
         Receiver.Dispatch (Repeated, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result = Receiver.Duplicate and not Receiver.Consumed (Repeated) and Evaluations = Before + 1);
         Changed.Number := 3;
         Receiver.Dispatch (Repeated, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result = Receiver.Token_Conflict and not Receiver.Consumed (Repeated) and Evaluations = Before + 1);
         Changed.Token := Queued_Notice.Token - 1;
         Receiver.Dispatch (Repeated, Service, Publisher_PID, Changed, 1000, Result);
         Check (Result in Receiver.Old_Token | Receiver.Invalid_Token and Evaluations = Before + 1);
         C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVCT")), Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = Count);
         C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVST")), Value, Status);
         Check (Status = C.Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 0);
         C.Take_Notification (Service, Notification, Present, Status);
         Check (Status = C.Values.Available and Present and Notification.Current_Target
           and Notification.Number = 16#80# and Notification.Path_Length = 1 and Notification.Path (1) = "LID0");
      end Handle_Queued;
   begin
      C.Configure_Notifications (Service, True, Status); Check (Status = C.Values.Available);
      Backpressure_Left := 2; Before_Calls := Enqueue_Calls;
      Query_Event (1); Check (Publisher.Observe.Stage = Publisher.Publication_Pending);
      Check (Publisher.Retained.Present and not Backend.Active and Enqueue_Calls = Before_Calls);
      Saved := Publisher.Retained.Item; Before_IO := IO_Count; Before_Queries := Query_Commands;
      Publisher.Advance; Publisher.Advance;
      Check (Publisher.Observe.Stage = Publisher.Publication_Pending and IO_Count = Before_IO
        and Enqueue_Calls = Before_Calls + 2 and Query_Commands = Before_Queries);
      Publisher.Start_Query (Now + 1000, 1000, 3, Submission); Check (Submission = Backend.Busy);
      Publisher.Advance;
      Check (Publisher.Observe.Stage = Publisher.Ready and not Publisher.Retained.Present and Queued);
      Check (Queued_Notice.Token = Saved.Token and IO_Count = Before_IO and Query_Commands = Before_Queries);
      Handle_Queued (1);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls;
      Publisher.Advance; Check (IO_Count = Before_IO and Enqueue_Calls = Before_Calls);
      Query_Event (0); Check (Publisher.Observe.Stage = Publisher.Ready and not Publisher.Retained.Present);
      Check (Enqueue_Calls = Before_Calls and not Queued);
      Has_Event := False; Before_Queries := Query_Commands;
      Publisher.Start_Query (Now + 1000, 1000, 3, Submission); Check (Submission = Backend.Started);
      Query_Driving := True; Publisher.Advance; Query_Driving := False;
      Check (Publisher.Observe.Stage = Publisher.Ready and Query_Commands = Before_Queries);
      Backpressure_Left := 1; Query_Event (1, 1); Saved := Publisher.Retained.Item;
      Publisher.Advance; Check (Publisher.Observe.Stage = Publisher.Delivery_Stalled);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls; Before_Queries := Query_Commands;
      Publisher.Advance; Check (Enqueue_Calls = Before_Calls and IO_Count = Before_IO);
      Publisher.Resume_Publication (2, OK); Check (OK);
      Publisher.Advance; Check (Queued and Queued_Notice.Token = Saved.Token
        and Query_Commands = Before_Queries and IO_Count = Before_IO);
      Handle_Queued (2);
      Publisher.Start_Query (Now, 1000, 3, Submission); Check (Submission = Backend.Started);
      Query_Driving := True; Publisher.Advance; Query_Driving := False;
      Check (Publisher.Observe.Stage = Publisher.Query_Failed and not Publisher.Observe.Hardware_Recovery);
      Publisher.Resume_Queries (OK); Check (OK and Publisher.Observe.Stage = Publisher.Ready);
      declare
         Run, Fresh : Receiver.Delivery; Result : Receiver.Outcome;
         Closed : Receiver.Close_Result; Enrolled : Receiver.Enrollment;
         Item : Events.Notice := (Controller => 1, Epoch => 1, Token => 1, Number => 3);
         Before_Eval, Before_Release : Natural;
      begin
         Release_Failures := 2;
         Receiver.Dispatch (Run, Service, Publisher_PID, Item, 1000, Result);
         Check (Result = Receiver.Cleanup_Pending and Receiver.Pending (Run) and C.Retained_Results (Service) = 1);
         Before_Eval := Evaluations; Before_Release := Releases;
         Item.Token := 2;
         Receiver.Dispatch (Fresh, Service, Publisher_PID, Item, 1000, Result);
         Check (Result = Receiver.Busy and not Receiver.Consumed (Fresh) and Evaluations = Before_Eval);
         Receiver.Revoke (1, 1, OK); Check (OK);
         Receiver.Dispatch (Fresh, Service, Publisher_PID, Item, 1000, Result);
         Check (Result = Receiver.Disabled and not Receiver.Consumed (Fresh) and Evaluations = Before_Eval);
         Receiver.Install (Service, 1, 2, Publisher_PID, Find (C.Namespace.Root, "EC01"), Enrolled);
         Check (Enrolled = Receiver.Route_Held);
         Receiver.Close (Run, Other, Closed);
         Check (Closed = Receiver.Wrong_Context and Receiver.Pending (Run) and Releases = Before_Release);
         Receiver.Close (Run, Service, Closed); Check (Closed = Receiver.Cleanup_Failed and Receiver.Pending (Run));
         Receiver.Close (Run, Service, Closed);
         Check (Closed = Receiver.Closed and not Receiver.Pending (Run) and Evaluations = Before_Eval);
         Check (Receiver.Observe (Run).Status = Receiver.Cleanup_Complete and C.Retained_Results (Service) = 0);
         Receiver.Install (Service, 1, 1, Publisher_PID, Find (C.Namespace.Root, "EC01"), Enrolled);
         Check (Enrolled = Receiver.Old_Epoch);
         Receiver.Install (Service, 1, 2, Publisher_PID, Find (C.Namespace.Root, "EC01"), Enrolled);
         Check (Enrolled = Receiver.Installed);
         Item.Epoch := 1;
         Receiver.Dispatch (Fresh, Service, Publisher_PID, Item, 1000, Result); Check (Result = Receiver.Wrong_Epoch);
         Item.Epoch := 2;
         Receiver.Dispatch (Fresh, Service, Publisher_PID, Item, 0, Result);
         Check (Result = Receiver.Invocation_Failed and Receiver.Consumed (Fresh) and Evaluations = Before_Eval);
         declare Missing, Again : Receiver.Delivery; begin
            Item.Token := Events.Token_ID'Last; Item.Number := 255;
            Receiver.Dispatch (Missing, Service, Publisher_PID, Item, 1000, Result);
            Check (Result = Receiver.Handler_Missing and Receiver.Consumed (Missing) and Evaluations = Before_Eval);
            Receiver.Dispatch (Again, Service, Publisher_PID, Item, 1000, Result); Check (Result = Receiver.Duplicate);
            Item.Token := 3;
            Receiver.Dispatch (Again, Service, Publisher_PID, Item, 1000, Result); Check (Result = Receiver.Old_Token);
         end;
      end;
      Unknown_Publication := True; Query_Event (1); Saved := Publisher.Retained.Item;
      Publisher.Advance;
      Check (Publisher.Observe.Stage = Publisher.Delivery_Unknown and Publisher.Retained.Present and Queued);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls; Before_Queries := Query_Commands;
      Publisher.Advance; Publisher.Advance;
      Publisher.Resume_Publication (2, OK); Check (not OK);
      Publisher.Start_Query (Now + 1000, 1000, 3, Submission); Check (Submission = Backend.Busy);
      Check (Publisher.Retained.Item.Token = Saved.Token and IO_Count = Before_IO
        and Query_Commands = Before_Queries and Enqueue_Calls = Before_Calls);
   end;
   Fields.Invoke_Scalar (Service, IO, Writes, C.Values.Position (Find (C.Values.Position (EC_Device), "STOR")),
                         [0 => 1, others => 0], 1, 1000, Result);
   Check (Result.Status = AML_Execute.Returned and Cell_Value = 16#A5#);
   Lid.Query (Service, Lid_Device, 1000, Sample); Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Open);
   Before := IO_Count;
   Fields.Invoke_Scalar (Service, IO, Reads, C.Values.Position (Find (C.Values.Position (EC_Device), "STOR")),
                         [others => 0], 1, 1000, Result);
   Check (Result.Status = AML_Execute.Unsupported and IO_Count = Before);
   declare Rejected : Fields.Binding_Set; begin
      C.Read_Field_Descriptor (Service, C.Values.Position (Find (C.Values.Position (EC_Device), "LCKD")), Field, Key, Status);
      Check (Status = C.Values.Available);
      Fields.Add_Read_Binding (Service, IO, Key, Targets, 1, Rejected, Status);
      Check (Status /= C.Values.Available and IO_Count = Before);
   end;
   Wrong_Sender := True; Lid.Query (Service, Lid_Device, 1000, Sample);
   Check (Sample.Outcome = Lid.Evaluation_Failed and IO_Count = Before); Wrong_Sender := False;
   Fail_Status := True; Lid.Query (Service, Lid_Device, 1000, Sample);
   Check (Sample.Execution = AML_Execute.Field_Indeterminate and Provider.Pending (IO));
   Check (Backend.Retained_For_Recovery and Fields.Uncertain (Service).Present);
   Before := Wire_Calls; Lid.Query (Service, Lid_Device, 1000, Sample);
   Check (Sample.Execution = AML_Execute.Field_Indeterminate and Wire_Calls = Before);
   declare
      Run, Again : Receiver.Delivery; Result : Receiver.Outcome;
      Enrolled : Receiver.Enrollment; Closed : Receiver.Close_Result; OK : Boolean;
      Before_Wire : constant Natural := Wire_Calls;
   begin
      Check (not Receiver.Receipt (Run).Present);
      Inflate_Charge := True;
      Receiver.Dispatch (Run, Service, Publisher_PID, Queued_Notice, 1000, Result);
      Inflate_Charge := False;
      Check (Result = Receiver.Field_Uncertain and Receiver.Observe (Run).Field_Uncertainty);
      Check (Receiver.Observe (Run).Invocation.Status = Methods.Provider_Contract_Error);
      Check (Receiver.Consumed (Run) and not Receiver.Pending (Run) and Wire_Calls = Before_Wire);
      declare
         Receipt : constant Events.Notice_Option := Receiver.Receipt (Run);
         Wrong : Events.Notice := Receipt.Item;
         Before_IO : constant Natural := IO_Count;
         Before_Queries : constant Natural := Query_Commands;
         Before_Eval : constant Natural := Evaluations;
      begin
         Check (Receipt.Present);
         Publisher.Confirm_Claim (Publisher_PID, Receipt.Item, OK); Check (not OK);
         Publisher.Confirm_Claim (CuBit.Process_IDs.From_Word (16#0042#), Receipt.Item, OK); Check (not OK);
         Wrong.Number := 2;
         Publisher.Confirm_Claim (Consumer_PID, Wrong, OK); Check (not OK);
         Wrong := Receipt.Item; Wrong.Token := Wrong.Token - 1;
         Publisher.Confirm_Claim (Consumer_PID, Wrong, OK); Check (not OK);
         Wrong := Receipt.Item; Wrong.Epoch := Wrong.Epoch - 1;
         Publisher.Confirm_Claim (Consumer_PID, Wrong, OK); Check (not OK);
         Wrong := Receipt.Item; Wrong.Controller := Wrong.Controller - 1;
         Publisher.Confirm_Claim (Consumer_PID, Wrong, OK); Check (not OK);
         Check (Publisher.Observe.Stage = Publisher.Delivery_Unknown and Publisher.Retained.Present);
         Publisher.Confirm_Claim (Consumer_PID, Receipt.Item, OK);
         Check (OK and Publisher.Observe.Stage = Publisher.Ready and not Publisher.Retained.Present);
         Check (Publisher.Observe.Hardware_Recovery and Backend.Retained_For_Recovery
           and Fields.Uncertain (Service).Present and Receiver.Observe (Run).Field_Uncertainty);
         Check (IO_Count = Before_IO and Query_Commands = Before_Queries and Evaluations = Before_Eval);
         Publisher.Confirm_Claim (Consumer_PID, Receipt.Item, OK); Check (not OK);
      end;
      Receiver.Dispatch (Again, Service, Publisher_PID, Queued_Notice, 1000, Result);
      Check (Result = Receiver.Duplicate and not Receiver.Receipt (Again).Present);
      Receiver.Close (Run, Service, Closed); Check (Closed = Receiver.No_Pending_Result);
      Receiver.Revoke (Events.Controller_ID'Last, Events.Epoch_ID'Last, OK); Check (OK);
      Receiver.Install (Service, Events.Controller_ID'Last, Events.Epoch_ID'Last,
                        Publisher_PID, EC_Device, Enrolled);
      Check (Enrolled = Receiver.Route_Held);
   end;
   Check (C.Retained_Results (Service) = 0);
   declare
      Result : Native.Report;
      Before_IO : constant Natural := IO_Count;
   begin
      Native.Advance (Result);
      Check (Result.Status = Native.Idle and Result.Recovery_Retained
        and not Native.Pending and IO_Count = Before_IO);
   end;
   Check (not Native.Pending and not Native.Faulted and not Live);
   Ada.Text_IO.Put_Line ("EC joined Field checks:" & Checks'Image);
end EC_Field_Joined_Tests;
