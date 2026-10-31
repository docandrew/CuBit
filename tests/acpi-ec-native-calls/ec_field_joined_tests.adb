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
   Live : Boolean := False;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC joined check" & Checks'Image; end if; end Check;
   procedure Clock (Time : out EC.Microseconds; Available : out Boolean) is
   begin Time := Now; Available := True; end Clock;
   function Owned return Boolean is (True);
   procedure Read (Register : EC.Read_Register; Value : out EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (Live);
      IO_Count := IO_Count + 1; Value := 0; Result := EC.Completed;
      if Fail_Status then Result := EC.Indeterminate; return; end if;
      if Register = EC.Status_Register then
         if Input_Pending then
            Check (Now >= Last_Write + EC.Input_Assertion_Delay);
            Input_Pending := False;
            if Stage = 2 then
               Check (Written = 16#10#);
               if Command = EC.Read_Command then Output_Ready := True; end if;
            elsif Stage = 3 then
               Check (Command = EC.Write_Command); Cell_Value := Written; Stage := 0;
            end if;
         end if;
         if Output_Ready then Value := EC.Output_Full; end if;
      else
         Check (Output_Ready); Output_Ready := False; Value := Cell_Value; Stage := 0;
      end if;
   end Read;
   procedure Write (Register : EC.Write_Register; Value : EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (Live);
      IO_Count := IO_Count + 1; Check (not Input_Pending and not Output_Ready);
      Input_Pending := True; Last_Write := Now; Written := Value; Stage := Stage + 1;
      if Stage = 1 then
         Check (Register = EC.Command_Register and (Value = EC.Read_Command or Value = EC.Write_Command)); Command := Value;
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
   procedure Evaluate
     (State : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin Fields.Evaluate_Named (State, IO, Reads, Item, Budget, Result, Status); end Evaluate;
   package Lid is new ACPI_Lid (C, Evaluate);
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
   C.Read_Field_Descriptor (Service, C.Values.Position (Find (C.Values.Position (EC_Device), "LIDS")), Field, Key, Status);
   Check (Status = C.Values.Available);
   Fields.Add_Read_Binding (Service, IO, Key, Targets, 1, Reads, Status); Check (Status = C.Values.Available);
   Fields.Add_Write_Binding (Service, IO, Key, Targets, 1, Writes, Status); Check (Status = C.Values.Available);
   Lid.Query (Service, Lid_Device, 1000, Sample); Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Closed);
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
