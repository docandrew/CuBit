with Ada.Command_Line;
with GNAT.Source_Info;
with Ada.Text_IO;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Interfaces;
with ACPI_Service_Core;
with ACPI_Device_Policy;
with ACPI_Device_Initialization;
with ACPI_Region_Initialization;
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
with ACPI_EC_Backend_Progress;
with ACPI_EC_Backend_Waits;
with ACPI_EC_Backend_Controller;
with ACPI_EC_Clock_Adapter;
with ACPI_EC_Clock_Policy;
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
   procedure Check (Condition : Boolean; Location : String := GNAT.Source_Info.Source_Location) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC joined check" & Checks'Image & " at " & Location; end if; end Check;
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

   package Events renames ACPI_EC_Event_Types;
   use type Events.Publication, Events.Token_ID, Events.Controller_ID, Events.Epoch_ID, Events.Event_Number;
   use type Backend.Submission;
   Enqueue_Calls, Backpressure_Left : Natural := 0;
   Unknown_Publication, Queued : Boolean := False;
   Queued_Notice : Events.Notice;
   procedure Enqueue (Item : Events.Notice; Result : out Events.Publication) is
   begin
      Check (not Query_Driving);
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
   use type Backend.Progress_Kind;
   package Progress is new ACPI_EC_Backend_Progress (Backend, Native, Publisher);
   use type Progress.Action_Kind;
   Waits_Observed : Natural := 0;
   procedure Drive (Result : out Native.Report) is
      Hint, Tick : Progress.Report;
   begin
      for Step in 1 .. 1000 loop
         Hint := Progress.Next (Now);
         Check (Hint.Action in Progress.Advance_Field | Progress.Publish_Notice | Progress.Wait_Until);
         if Hint.Action = Progress.Wait_Until then
            Check (Hint.At_Time > Now); Now := Hint.At_Time;
            Waits_Observed := Waits_Observed + 1;
         end if;
         Progress.Advance (Now, Tick);
         if Tick.Action = Progress.Advance_Field then
            Result := Tick.Field;
            if Result.Status /= Native.Awaiting_Transaction then return; end if;
         end if;
      end loop;
      raise Program_Error with "bounded backend Field progress did not complete";
   end Drive;
   procedure Advance_Event is
      Tick : Progress.Report;
   begin
      Progress.Advance (Now, Tick);
      Check (Tick.Action in Progress.Advance_Query | Progress.Publish_Notice | Progress.No_Work);
   end Advance_Event;
   procedure Query_Event (Number : EC.Byte; Attempts : Events.Delivery_Budget := 3) is
      Result : Backend.Submission;
   begin
      Has_Event := True; Event_Number := Number;
      Publisher.Start_Query (Now + 1000, 1000, Attempts, Result);
      Check (Result = Backend.Started);
      for Step in 1 .. 1000 loop
         Query_Driving := True; Advance_Event; Query_Driving := False;
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
   Unavailable_Service : aliased C.State (1, 4096, 4096, 1);
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
   declare
      Token : Backend.Operation_ID;
      Submission : Backend.Submission;
      Fact : Backend.Query_Completion;
      Tick : Progress.Report;
      Before_IO : constant Natural := IO_Count;
      use type Backend.Operation_ID, EC.Outcome;
   begin
      Backend.Submit_Query (Now + 1000, 1000, Submission, Token);
      Check (Submission = Backend.Started and Token /= 0);
      Tick := Progress.Next (Now);
      Check (Tick.Action = Progress.Inconsistent_Owners and not Tick.Field_Reply_Pending);
      Progress.Advance (Now, Tick);
      Check (Tick.Action = Progress.Inconsistent_Owners and IO_Count = Before_IO);
      --  The test's low-level initiating owner cancels before any write. The
      --  dispatcher itself neither steps nor invents a completion owner.
      Backend.Request_Stop; Backend.Step; Backend.Take_Query (Fact);
      Check (Fact.Present and Fact.Status = EC.Stopped and not Fact.Needs_Recovery);
      Check (Progress.Next (Now).Action = Progress.No_Work and IO_Count = Before_IO);
   end;
   Check (Backend.Next_Progress (Now).Kind = Backend.No_Work);
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
         C.Stage_Table (Unavailable_Service, 1, C.DSDT, Data, Staged); Check (Staged = C.Staged);
      end;
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Service, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available);
      C.Freeze_Catalog (Unavailable_Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Unavailable_Service, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Unavailable_Service, Members, Status); Check (Status = C.Values.Available);
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
   declare
      type Startup_Context is limited record
         Provider_Installed : Boolean := False;
         Notices : Natural := 0;
      end record;
      Context : Startup_Context;
      use type C.Namespace.Region_Space_ID;
      use type C.Node_Descriptor;
      Region_Node : constant C.Node_Descriptor := Find (C.Values.Position (EC_Device), "ERAM");
      procedure Expect (State : aliased in out C.State; Name : AML_Names.Segment;
                        Number : AML_Decode.Integer_Value) is
         Found : C.Namespace.Lookup_Result;
         Value : C.Values.Result;
         Access_Result : C.Values.Access_Status;
         use type C.Namespace.Lookup_Status;
      begin
         C.Find_Object (State, C.Namespace.Root, Name, Found, Access_Result);
         Check (Access_Result = C.Values.Available and Found.Status = C.Namespace.Found);
         C.Observe_Named_Value (State, Found.Node, Value, Access_Result);
         Check (Access_Result = C.Values.Available and Value.Status = AML_Execute.Returned
           and Value.Number = Number);
      end Expect;
      function Ready (Context : Startup_Context; Region : C.Node_Descriptor;
                      Data : C.Namespace.Operation_Region_Attributes) return Boolean is
        (Context.Provider_Installed and then Region = Region_Node and then Data.Space = 3);
      function PCI_Root (Context : Startup_Context; Region : C.Node_Descriptor) return Boolean is
         pragma Unreferenced (Context, Region);
      begin return False; end PCI_Root;
      procedure Invoke (State : aliased in out C.State; Context : in out Startup_Context;
                        Node : C.Namespace.Node_ID; Args : AML_Execute.Arguments;
                        Argument_Count : Natural; Budget : Natural;
                        Result : out C.Values.Result; Status : out C.Values.Access_Status) is
      begin
         Check (Context.Provider_Installed and Argument_Count = 2 and Args (0) = 3 and Args (1) = 1);
         Context.Notices := Context.Notices + 1;
         Fields.Invoke_Retained (State, IO, Reads, Node, Args, Argument_Count, Budget, Result, Status);
      end Invoke;
      package Regions is new ACPI_Region_Initialization (C, Startup_Context, Ready, PCI_Root, Invoke);
      use type Regions.Phase;
      Region_Run, Unavailable_Run : Regions.Session;
      procedure Barrier (State : aliased in out C.State; Budget : Natural;
                         Charged : out Natural; Outcome : out ACPI_Device_Policy.Region_Outcome) is
         OK : Boolean;
      begin
         Expect (State, "STUP", 16#10#);
         Regions.Connect (Region_Run, State, Context, Budget, OK);
         Check (OK);
         Charged := Regions.Observe (Region_Run).Charged;
         Outcome := Regions.Outcome (Region_Run);
      end Barrier;
      package Devices is new ACPI_Device_Initialization (C, Barrier, Evaluate);
      use type Devices.Phase, Devices.Step_Result;
      Device_Run : Devices.Session;
      OK : Boolean;
      Before_IO : constant Natural := IO_Count;
      Count_IO, Count_Evaluations : Natural;
   begin
      --  Readiness is a trusted observation of already installed backing,not
      --  an authority inferred from the firmware's EmbeddedControl declaration.
      Regions.Connect (Unavailable_Run, Unavailable_Service, Context, 10000, OK);
      Check (OK and Regions.Observe (Unavailable_Run).Stage = Regions.Failed
        and Context.Notices = 0 and IO_Count = Before_IO);
      Expect (Unavailable_Service, "STUP", 0);
      Regions.Connect (Unavailable_Run, Unavailable_Service, Context, 10000, OK); Check (not OK);
      Context.Provider_Installed := True;
      Devices.Begin_Initialization (Device_Run, Service, 10000, OK);
      Check (OK and Devices.Observe (Device_Run).Stage = Devices.Awaiting_Regions);
      Expect (Service, "STUP", 16#10#);
      Devices.Complete_Initialization (Device_Run, Service, OK);
      Check (OK and Devices.Observe (Device_Run).Stage = Devices.Completed);
      Check (Regions.Observe (Region_Run).Stage = Regions.Connected
        and Context.Notices = 1 and IO_Count > Before_IO);
      Check (Devices.Observe_Node (Device_Run, C.Values.Position (EC_Device)).Initialization = Devices.Succeeded);
      Expect (Service, "STUP", 2); Expect (Service, "RVAL", 0);
      Count_IO := IO_Count; Count_Evaluations := Evaluations;
      Devices.Begin_Initialization (Device_Run, Service, 10000, OK); Check (not OK);
      Devices.Complete_Initialization (Device_Run, Service, OK); Check (not OK);
      Regions.Connect (Region_Run, Service, Context, 10000, OK); Check (not OK);
      Check (IO_Count = Count_IO and Evaluations = Count_Evaluations and Context.Notices = 1);
   end;
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
      Query_Event (1, 2); Check (Publisher.Observe.Stage = Publisher.Publication_Pending);
      Check (Publisher.Retained.Present and not Backend.Active and Enqueue_Calls = Before_Calls);
      Saved := Publisher.Retained.Item;
      declare
         Msg : constant M.Message := Native_Message ((Label => Wire.Read_Operation,
            Data => [1, Interfaces.Unsigned_64 (Register_ID), 0, 0], others => <>));
         Field_Result : Native.Report;
         Tick : Progress.Report;
         Before_IO : Natural;
      begin
         Native.Begin_Call (Received_Sender, Msg, Now + 1000, 1000, Field_Result);
         Check (Field_Result.Status = Native.Awaiting_Transaction and Native.Pending);
         for Round in 1 .. 2 loop
            Check (Progress.Next (Now).Action = Progress.Advance_Field);
            Before_IO := IO_Count; Progress.Advance (Now, Tick);
            Check (Tick.Action = Progress.Advance_Field and IO_Count <= Before_IO + 1);
            Check (Progress.Next (Now).Action = Progress.Publish_Notice);
            Before_IO := IO_Count; Progress.Advance (Now, Tick);
            Check (Tick.Action = Progress.Publish_Notice and IO_Count = Before_IO and Native.Pending);
         end loop;
         Check (Publisher.Observe.Stage = Publisher.Delivery_Stalled and Enqueue_Calls = Before_Calls + 2);
         Drive (Field_Result); Check (Field_Result.Status = Native.Completion_Reply and not Native.Pending);
         Check (Progress.Next (Now).Action = Progress.No_Work and Progress.Next (Now).Notice_Retained);
      end;
      Before_IO := IO_Count; Before_Queries := Query_Commands;
      Publisher.Resume_Publication (1, OK); Check (OK);
      Advance_Event;
      Check (Publisher.Observe.Stage = Publisher.Ready and not Publisher.Retained.Present and Queued);
      Check (Queued_Notice.Token = Saved.Token and IO_Count = Before_IO and Query_Commands = Before_Queries);
      Handle_Queued (1);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls;
      Advance_Event; Check (IO_Count = Before_IO and Enqueue_Calls = Before_Calls);
      Query_Event (0); Check (Publisher.Observe.Stage = Publisher.Ready and not Publisher.Retained.Present);
      Check (Enqueue_Calls = Before_Calls and not Queued);
      Has_Event := False; Before_Queries := Query_Commands;
      Publisher.Start_Query (Now + 1000, 1000, 3, Submission); Check (Submission = Backend.Started);
      Query_Driving := True; Advance_Event; Query_Driving := False;
      Check (Publisher.Observe.Stage = Publisher.Ready and Query_Commands = Before_Queries);
      Backpressure_Left := 1; Query_Event (1, 1); Saved := Publisher.Retained.Item;
      Advance_Event; Check (Publisher.Observe.Stage = Publisher.Delivery_Stalled);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls; Before_Queries := Query_Commands;
      Advance_Event; Check (Enqueue_Calls = Before_Calls and IO_Count = Before_IO);
      Publisher.Resume_Publication (2, OK); Check (OK);
      Advance_Event; Check (Queued and Queued_Notice.Token = Saved.Token
        and Query_Commands = Before_Queries and IO_Count = Before_IO);
      Handle_Queued (2);
      Publisher.Start_Query (Now, 1000, 3, Submission); Check (Submission = Backend.Started);
      Query_Driving := True; Advance_Event; Query_Driving := False;
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
      Advance_Event;
      Check (Publisher.Observe.Stage = Publisher.Delivery_Unknown and Publisher.Retained.Present and Queued);
      Before_IO := IO_Count; Before_Calls := Enqueue_Calls; Before_Queries := Query_Commands;
      Advance_Event; Advance_Event;
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
   Check (Waits_Observed > 0);
   Check (Backend.Next_Progress (Now).Kind = Backend.No_Work and Backend.Retained_For_Recovery);
   Check (Progress.Next (Now).Action = Progress.No_Work and Progress.Next (Now).Hardware_Recovery);
   declare
      function Invalid_Save return H.IPC.Saved_Reply is
        ((Saved => True, Handle => H.No_Reply));
      package Faulty is new ACPI_EC_Native_Calls
        (Backend, Endpoint, Invalid_Save, Answer_Saved, Answer_Current);
      package Guarded is new ACPI_EC_Backend_Progress (Backend, Faulty, Publisher);
      use type Faulty.Outcome, Guarded.Action_Kind;
      Field : Faulty.Report;
      Tick : Guarded.Report;
      Before_IO : constant Natural := IO_Count;
      Before_Answers : constant Natural := Answers + Current_Answers;
   begin
      --  Inconsistent runtime result is modeled explicitly; no real reply
      --  capability is created or released in this negative case.
      Faulty.Begin_Call (Received_Sender, M.NULL_MESSAGE, Now + 1000, 1000, Field);
      Check (Field.Status = Faulty.Owner_Fault and Faulty.Faulted and Faulty.Pending);
      Tick := Guarded.Next (Now);
      Check (Tick.Action = Guarded.Owner_Fault and Tick.Field_Reply_Pending and Tick.Hardware_Recovery);
      Guarded.Advance (Now, Tick);
      Check (Tick.Action = Guarded.Owner_Fault and IO_Count = Before_IO
        and Answers + Current_Answers = Before_Answers and Backend.Retained_For_Recovery);
   end;
   declare
      type Scenario is (Advancing, Frozen, Refused, Clock_Lost, Clock_Out_Of_Range, Due_During_Pause);
   begin
   for Case_ID in Scenario loop
      declare
         package P renames ACPI_EC_Clock_Policy;
         use type P.Pause_Outcome;
         Time : EC.Microseconds := 1;
         Accesses, Pause_Calls : Natural := 0;
         Addressed, Consumed_Data, In_Pause, Fault_Latched : Boolean := False;
         Pause_Target : EC.Microseconds := 0;
         Saw_Pause_Status : P.Pause_Outcome := P.Budget_Exhausted;
         Saw_Due, Query_Mode : Boolean := False;
         procedure Raw_Read (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
         begin
            if In_Pause and Case_ID = Due_During_Pause then Time := Pause_Target; end if;
            if In_Pause and Case_ID in Clock_Lost | Clock_Out_Of_Range then Fault_Latched := True; end if;
            Value := (if Fault_Latched and Case_ID = Clock_Out_Of_Range then Interfaces.Unsigned_64'Last - 1
                      else Interfaces.Unsigned_64 (Time));
            Available := not (Fault_Latched and Case_ID = Clock_Lost);
         end Raw_Read;
         procedure Raw_Pause (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean) is
         begin
            Pause_Calls := Pause_Calls + 1;
            if Case_ID = Advancing then Time := EC.Microseconds (Until_Time); end if;
            Accepted := Case_ID /= Refused;
         end Raw_Pause;
         package Clock is new ACPI_EC_Clock_Adapter (Raw_Read, Raw_Pause);
         procedure Observe_Pause (Target : EC.Microseconds; Remaining : in out EC.Work_Budget;
                                  Result : out P.Pause_Report) is
         begin
            In_Pause := True; Pause_Target := Target;
            Clock.Pause_Until (Target, Remaining, Result);
            In_Pause := False;
            Saw_Pause_Status := Result.Status;
            Saw_Due := Saw_Due or Result.Status = P.Already_Due;
         end Observe_Pause;
         procedure Local_Read (Register : EC.Read_Register; Value : out EC.Byte; Result : out EC.Access_Result) is
         begin
            Accesses := Accesses + 1; Result := EC.Completed;
            if Register = EC.Status_Register then
               Value := (if (Addressed or Query_Mode) and not Consumed_Data then EC.Output_Full else 0);
            else
               Check (Addressed or Query_Mode); Value := (if Query_Mode then 0 else 16#A5#); Consumed_Data := True;
            end if;
         end Local_Read;
         procedure Local_Write (Register : EC.Write_Register; Value : EC.Byte; Result : out EC.Access_Result) is
         begin
            Accesses := Accesses + 1; Result := EC.Completed;
            if Register = EC.Command_Register then
               Check (Value in EC.Read_Command | EC.Query_Command);
               Query_Mode := Value = EC.Query_Command;
            else Check (Value = 16#10#); Addressed := True; end if;
         end Local_Write;
         package E is new ACPI_EC_Transactions (Local_Read, Local_Write, Clock.Read, Owned);
         package B is new ACPI_EC_Registers (E, 1);
         package Endpoint is new ACPI_EC_Endpoint (B);
         package N is new ACPI_EC_Native_Calls (B, Endpoint, Save, Answer_Saved, Answer_Current);
         procedure No_Publish (Item : Events.Notice; Result : out Events.Publication) is
            pragma Unreferenced (Item);
         begin Result := Events.Backpressure; end No_Publish;
         package Pub is new ACPI_EC_Event_Publisher (B, No_Publish);
         package Pump is new ACPI_EC_Backend_Progress (B, N, Pub);
         package Waits is new ACPI_EC_Backend_Waits (B, Pump, Observe_Pause);
         type Control_Clock_Mode is (Normal, Missing, Overflow);
         Clock_Mode : Control_Clock_Mode := Normal;
         procedure Control_Clock (Value : out EC.Microseconds; Available : out Boolean) is
         begin
            if Clock_Mode = Missing then Value := 0; Available := False;
            elsif Clock_Mode = Overflow then Value := EC.Microseconds'Last; Available := True;
            else Clock.Read (Value, Available);
            end if;
         end Control_Clock;
         package Controller is new ACPI_EC_Backend_Controller
           (B, N, Pub, Pump, Waits, Control_Clock, 99, 12, 2, Answer_Current);
         package Invalid_Controller is new ACPI_EC_Backend_Controller
           (B, N, Pub, Pump, Waits, Control_Clock, 0, 12, 2, Answer_Current);
         use type Controller.Admission_Status, Invalid_Controller.Admission_Status;
         use type N.Outcome, EC.Outcome, EC.Work_Budget;
         use type Pump.Action_Kind;
         Msg : constant M.Message := Native_Message ((Label => Wire.Read_Operation,
            Data => [1, 17, 0, 0], others => <>));
         Field : N.Report;
         Tick : Waits.Report;
         Remaining : EC.Work_Budget := 0;
         Admitted_Call, Rejected_Call : Controller.Call_Report;
         Invalid_Call : Invalid_Controller.Call_Report;
         Available, Tried_Busy : Boolean := False;
         Before_Saves, Before_Current : Natural;
         Installed, Stopping : Boolean := False;
         Before_Answers : constant Natural := Answers;
         Before_IO : Natural;
         Query_Status : Controller.Admission_Status;
         Submission : B.Submission;
         use type B.Submission, Pub.Phase;
      begin
         B.Install ((Register => 17, Epoch => 1,
            Owner => B.Owner_ID (M.To_Word (Received_Sender)), Cell => 16#10#,
            Readable => True, others => <>), Installed);
         Check (Installed);
         Before_Saves := Saves; Before_Current := Current_Answers;
         Invalid_Controller.Begin_Call (Received_Sender, Msg, Invalid_Call);
         Check (Invalid_Call.Status = Invalid_Controller.Invalid_Policy and Invalid_Call.Replied);
         Check (Saves = Before_Saves and Current_Answers = Before_Current + 1
           and Accesses = 0 and not B.Active);
         Clock_Mode := Missing;
         Controller.Begin_Call (Received_Sender, Msg, Rejected_Call);
         Check (Rejected_Call.Status = Controller.Clock_Unavailable and Rejected_Call.Replied);
         Clock_Mode := Overflow;
         Controller.Begin_Call (Received_Sender, Msg, Rejected_Call);
         Check (Rejected_Call.Status = Controller.Deadline_Overflow and Rejected_Call.Replied);
         Check (Saves = Before_Saves and Accesses = 0 and not B.Active
           and Controller.Remaining_Pauses = 0);
         Clock_Mode := Normal;
         Controller.Begin_Call (Received_Sender, Msg, Admitted_Call);
         Check (Admitted_Call.Status = Controller.Admitted and Controller.Remaining_Pauses = 2);
         Field := Admitted_Call.Field;
         Check (Field.Status = N.Awaiting_Transaction);
         for Attempt in 1 .. 64 loop
            Before_IO := Accesses;
            Controller.Advance (Tick, Available);
            Remaining := Controller.Remaining_Pauses;
            Check (Available or else Case_ID in Clock_Lost | Clock_Out_Of_Range);
            if Case_ID in Frozen | Refused and then Tick.Wait_Checked and then not Tried_Busy then
               Before_Saves := Saves;
               Controller.Begin_Call (Received_Sender, Msg, Rejected_Call);
               Check (Rejected_Call.Status = Controller.Refused and Rejected_Call.Replied);
               Check (Saves = Before_Saves and Controller.Remaining_Pauses = Remaining);
               Tried_Busy := True;
            end if;
            Check (Accesses <= Before_IO + 1);
            Stopping := Stopping or Tick.Stop_Requested;
            if Case_ID in Frozen | Refused and Tick.Wait_Checked then
               Check (Time = 1 and Accesses = 2);
               Check (Tick.Pause_Result.Status in P.Pause_Returned | P.Pause_Refused | P.Budget_Exhausted);
            end if;
            if Tick.Progress_Result.Action = Pump.Advance_Field then
               Field := Tick.Progress_Result.Field;
               exit when Field.Status /= N.Awaiting_Transaction;
            end if;
         end loop;
         Check (Field.Status = N.Completion_Reply and Field.Delivered and not N.Pending);
         Check (Answers = Before_Answers + 1 and not Live);
         if Case_ID in Frozen | Refused then
            Check (Stopping and Pause_Calls = 2 and Remaining = 0);
            Check (Field.Hardware = EC.Budget_Exhausted and Field.Recovery_Retained
              and Field.Effects_Possible and Accesses = 2 and not Addressed);
            Check (Saw_Pause_Status = P.Budget_Exhausted);
         elsif Case_ID in Clock_Lost | Clock_Out_Of_Range then
            Check (Stopping and Pause_Calls = 0 and Remaining = 1);
            Check (Field.Hardware = EC.Clock_Unavailable and Field.Recovery_Retained
              and Field.Effects_Possible and Accesses = 2 and not Addressed);
            Check (Saw_Pause_Status = (if Case_ID = Clock_Lost then P.Clock_Unavailable
                                      else P.Clock_Representation_Limit));
         else
            Check (not Stopping and Remaining = 0);
            Check (Pause_Calls = (if Case_ID = Advancing then 2 else 0));
            Check (Field.Hardware = EC.Succeeded and not Field.Recovery_Retained and Consumed_Data);
            Check (Saw_Due = (Case_ID = Due_During_Pause));
         end if;
         Check (Pump.Next (Time).Action = Pump.No_Work);
         Check (B.Retained_For_Recovery = (Case_ID not in Advancing | Due_During_Pause));
         Check (B.Active = B.Retained_For_Recovery);
         Before_IO := Accesses;
         Pub.Configure (1, 1, Installed); Check (Installed);
         Clock_Mode := Missing;
         Controller.Start_Query (1, Query_Status, Submission);
         Check (Query_Status = Controller.Clock_Unavailable and Submission = B.Denied);
         Check (Accesses = Before_IO and Controller.Remaining_Pauses = Remaining);
         Clock_Mode := Normal;
         if Case_ID in Advancing | Due_During_Pause then
            Addressed := False; Consumed_Data := False;
            Controller.Start_Query (1, Query_Status, Submission);
            Check (Query_Status = Controller.Admitted and Submission = B.Started
              and Controller.Remaining_Pauses = 2);
            Controller.Start_Query (1, Query_Status, Submission);
            Check (Query_Status = Controller.Refused and Submission = B.Busy
              and Controller.Remaining_Pauses = 2);
            Clock_Mode := Missing;
            Controller.Advance (Tick, Available);
            Check (not Available and Tick.Stop_Requested and Accesses = Before_IO);
            Check (Pub.Observe.Stage = Pub.Query_Failed and not B.Active
              and not B.Retained_For_Recovery and Pub.Observe.Hardware = EC.Stopped);
            Clock_Mode := Normal;
            Pub.Resume_Queries (Installed); Check (Installed);
            Controller.Start_Query (1, Query_Status, Submission);
            Check (Query_Status = Controller.Admitted and Controller.Remaining_Pauses = 2);
            for Attempt in 1 .. 64 loop
               Before_IO := Accesses;
               Controller.Advance (Tick, Available);
               Check (Available and Accesses <= Before_IO + 1);
               exit when Pub.Observe.Stage /= Pub.Querying;
            end loop;
            Check (Pub.Observe.Stage = Pub.Ready and Pub.Observe.Hardware = EC.No_Event
              and not B.Active and not B.Retained_For_Recovery);
            Check (Answers = Before_Answers + 1 and not Live);
         else
            Controller.Start_Query (1, Query_Status, Submission);
            Check (Query_Status in Controller.Refused | Controller.Clock_Unavailable);
            Check (Accesses = Before_IO and B.Retained_For_Recovery
              and Controller.Remaining_Pauses = Remaining);
         end if;
      end;
   end loop;
   end;
   Ada.Text_IO.Put_Line ("EC joined Field checks:" & Checks'Image);
end EC_Field_Joined_Tests;
