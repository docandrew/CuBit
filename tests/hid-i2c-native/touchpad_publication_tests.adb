pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Parameters;
with HID_Launch_Adapter;
with GPIO_Lease_Control;
with HID_GPIO_Reset;
with HID_Touchpad_Native;
with HID_Touchpad_Pump;
with HID_Touchpad_Native_Wait;
with HID_Touchpad_Publication;
with CuBit.Input;
with GPIO_Channel_Badges;
with CuBit.Messages;
with GPIO_Client_Controls;
with GPIO_Control_Wire;
with HID_I2C_Grant_Creation;
with HID_I2C_Buffered_Client;
with HID_I2C_Touchpad;
with Input_Pending;
with Pointer_Pending;
with HID_I2C_Descriptors;
with I2C_Transfer_Wire;
with CuBit.Grant_References;
with CuBit.Process_IDs;
procedure Touchpad_Publication_Tests is
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package G renames CuBit.Grant_References;
   package P renames CuBit.Process_IDs;
   use type W.Word, W.Count, D.Bytes, D.Byte_Count, G.Reference;
   Input_Address : constant W.Word := 16#10_0000#;
   Output_Address : constant W.Word := 16#20_0000#;
   Maximum : constant W.Capacity := 512;
   Input_Page, Output_Page : D.Bytes (0 .. 4095) := [others => 0];
   Expected_Bytes : constant W.Word := 4096; Expected_Pages : constant W.Word := 1;
   Input_Allocated, Output_Allocated : Boolean := False;
   Allow_Output_Allocation, Allow_Input_Grant, Allow_Output_Grant, Allow_Submit, Allow_Release : constant Boolean := True;
   Uncertain_Output : constant Boolean := False;
   Input_Retired, Output_Retired : Boolean := False;
   Input_Reference : G.Reference := (24, 1); Output_Reference : G.Reference := (25, 1);
   Allocate_Calls, Free_Calls, Create_Calls, Submit_Calls, Revoke_Calls, Clear_Calls, Read_Calls : Natural := 0;
   Checks : Natural := 0;
   Last_Request : W.Request; Last_Token : W.Word := 0;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Natural'Image (Checks); end if; end Check;
   procedure Allocate (Bytes : W.Word; Address : out W.Word; OK : out Boolean) is
   begin
      Check (Bytes = Expected_Bytes); Allocate_Calls := Allocate_Calls + 1;
      OK := True;
      if not Input_Allocated then Address := Input_Address; Input_Allocated := True;
      elsif Allow_Output_Allocation then Address := Output_Address; Output_Allocated := True;
      else Address := 0; OK := False; end if;
   end Allocate;
   procedure Release (Address, Bytes : W.Word; OK : out Boolean) is
   begin
      Check (Bytes = Expected_Bytes); Free_Calls := Free_Calls + 1; OK := Allow_Release;
      if not OK then return; end if;
      if Address = Input_Address then Input_Allocated := False;
      elsif Address = Output_Address then Output_Allocated := False;
      else raise Program_Error; end if;
   end Release;
   procedure Clear (Address, Bytes : W.Word) is
   begin
      Check (Bytes = Expected_Bytes); Clear_Calls := Clear_Calls + 1;
      if Address = Input_Address then Check (Input_Retired or else Create_Calls = 0); Input_Page (0 .. Natural (Bytes) - 1) := [others => 0];
      elsif Address = Output_Address then Check (Output_Retired or else Create_Calls = 0); Output_Page (0 .. Natural (Bytes) - 1) := [others => 0];
      else raise Program_Error; end if;
   end Clear;
   procedure Store (Address : W.Word; Data : D.Bytes) is
   begin Check (Address = Input_Address); Input_Page (0 .. Data'Length - 1) := Data; end Store;
   procedure Load (Address : W.Word; Data : out D.Bytes) is
   begin Check (Address = Output_Address and then Output_Retired); Read_Calls := Read_Calls + 1; Data := Output_Page (0 .. Data'Length - 1); end Load;
   procedure Create (Address, Pages : W.Word; Writable : Boolean; Reference : out G.Reference; Status : out HID_I2C_Grant_Creation.Status) is
      OK : Boolean;
   begin
      Check (Pages = Expected_Pages); Create_Calls := Create_Calls + 1; Reference := (others => <>);
      if Address = Input_Address then
         Check (not Writable); OK := Allow_Input_Grant;
         if OK then Input_Reference.generation := Input_Reference.generation + 1; Reference := Input_Reference; Input_Retired := False; end if;
      elsif Address = Output_Address then
         Check (Writable); OK := Allow_Output_Grant;
         if OK then Output_Reference.generation := Output_Reference.generation + 1; Reference := Output_Reference; Output_Retired := False; end if;
      else raise Program_Error; end if;
      Status := (if OK then HID_I2C_Grant_Creation.Created else HID_I2C_Grant_Creation.Refused);
      if Address = Output_Address and then Uncertain_Output then
         Status := HID_I2C_Grant_Creation.Uncertain; Output_Retired := False;
      end if;
   end Create;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean) is
   begin
      Check (Token > 0); Submit_Calls := Submit_Calls + 1; Last_Request := Item; Last_Token := Token; Accepted := Allow_Submit;
      Check ((Item.Read_Count = 0 or else not Output_Retired) and then (Item.Write_Count = 0 or else not Input_Retired));
   end Submit;
   procedure Revoke (Item : G.Reference; Accepted : out Boolean) is
   begin
      Check (Item = Input_Reference or else Item = Output_Reference); Revoke_Calls := Revoke_Calls + 1; Accepted := True;
   end Revoke;
   function Retired (Item : G.Reference) return Boolean is
   begin
      if Item = Input_Reference then return Input_Retired;
      elsif Item = Output_Reference then return Output_Retired;
      else raise Program_Error; end if;
   end Retired;
   package C is new HID_I2C_Buffered_Client (Maximum, Allocate, Release, Clear, Store, Load, Create, Submit, Revoke, Retired);
   Token_Counter : W.Word := 0;
   procedure Next_Token (Token : out W.Word; Available : out Boolean) is
   begin Token_Counter := Token_Counter + 1; Token := Token_Counter; Available := True; end Next_Token;
   procedure Arm (Deadline : W.Millisecond; Reply : out GPIO_Lease_Control.Reply) is
   begin
      Check (Deadline > 0); 
      Reply := (GPIO_Lease_Control.Completed, 42);
   end Arm;
   procedure Poll_Arm (Reply : out GPIO_Lease_Control.Reply) is
   begin Reply := (GPIO_Lease_Control.Pending, 0); end Poll_Arm;
   procedure Release_Latch (Ticket : W.Word; Deadline : W.Millisecond; Reply : out GPIO_Lease_Control.Reply) is
   begin
      Check (Ticket = 42 and then Deadline > 0); 
      Reply := (GPIO_Lease_Control.Completed, Ticket);
   end Release_Latch;
   procedure Poll_Release (Reply : out GPIO_Lease_Control.Reply) is
   begin Reply := (GPIO_Lease_Control.Pending, 0); end Poll_Release;
   package GW renames GPIO_Control_Wire;
   GPIO_Token : GW.Word := 0;
   GPIO_Calls : Natural := 0;
   GPIO_Request : GW.Request;
   GPIO_Server : constant P.Process_ID := P.From_Word (16#F321_4567_0000_9876#);
   procedure GPIO_Submit (Item : GW.Request; Token : GW.Word; Accepted : out Boolean) is
   begin GPIO_Request := Item; GPIO_Token := Token; GPIO_Calls := GPIO_Calls + 1; Accepted := True; end GPIO_Submit;
   package Controls is new GPIO_Client_Controls (GPIO_Submit);
   Cached_Now : W.Word := 1;
   Cached_Available : Boolean := True;
   procedure Clock (Value : out GW.Word; Available : out Boolean) is
   begin Value := Cached_Now; Available := Cached_Available; end Clock;
   package Reset is new HID_GPIO_Reset.Bridge (Controls, GPIO_Server, 20, Next_Token, Clock);
   package App is new HID_I2C_Touchpad (C, 512, 32, 0, 5, 2, 8, 16, 32, 64, 8, Next_Token,
      Reset.Begin_Arm_Reset, Reset.Poll_Arm_Reset, Reset.Begin_Release_Reset, Reset.Poll_Release_Reset);
   package Live is new App.GPIO_Input (Controls, GPIO_Server, 50, 100, 20);
   Badge : constant GW.Word := GPIO_Channel_Badges.Canonical ((160, GPIO_Channel_Badges.Level_Triggered, GPIO_Channel_Badges.Active_Low));
   package Native is new HID_Touchpad_Native (Reset, App, Live, Badge);
   package M renames CuBit.Messages;
   use type Native.Destination, Native.Event_Result;
   package I renames CuBit.Input;
   use type I.Device_Class, I.Delivery_Class, I.Source_Generation, P.Process_ID;
   Consumer_One : constant P.Process_ID := P.From_Word (16#F123_4567_0000_ABCD#);
   Consumer_Two : constant P.Process_ID := P.From_Word (16#E123_4567_0000_ABCD#);
   Source_Identity : constant P.Process_ID := P.From_Word (16#D123_4567_0000_9876#);
   Current_Consumer : P.Process_ID := Consumer_One;
   Allow_Publication : Boolean := False;
   Publication_Attempts : Natural := 0;
   Last_Publication : M.Message := M.NULL_MESSAGE;
   function Consumer return P.Process_ID is (Current_Consumer);
   function Try_Publish (Target : P.Process_ID; Message : M.Message) return Boolean is
   begin
      Check (Target = Current_Consumer); Last_Publication := Message;
      Publication_Attempts := Publication_Attempts + 1; return Allow_Publication;
   end Try_Publish;
   package Publication is new HID_Touchpad_Publication.Publisher (App, 1, Consumer, Try_Publish);
   Published : Publication.Report;
   procedure Flush (Now : W.Word; Accepted : Boolean) is
   begin Allow_Publication := Accepted; Publication.Flush (Now, Published); end Flush;
   Pump_Time : W.Word := 61;
   Pump_Clock : Boolean := True;
   Queued_Completion : M.CompletionEntry := M.NULL_COMPLETION;
   Completion_Queued, Event_Queued, Request_Queued : Natural := 0;
   Samples, Cache_Updates : Natural := 0;
   Trace : String (1 .. 6) := [others => ' '];
   Trace_Count : Natural := 0;
   procedure Visit (Kind : Character) is
   begin
      Trace_Count := Trace_Count + 1;
      Check (Trace_Count <= Trace'Length);
      Trace (Trace_Count) := Kind;
   end Visit;
   procedure Sample_Time (Now : out W.Word; Available : out Boolean) is
   begin Samples := Samples + 1; Now := Pump_Time; Available := Pump_Clock; end Sample_Time;
   procedure Cache_Time (Now : W.Word; Available : Boolean) is
   begin Cache_Updates := Cache_Updates + 1; Cached_Now := Now; Cached_Available := Available; end Cache_Time;
   procedure Poll_Completion (Item : out M.CompletionEntry; Found : out Boolean) is
   begin
      Visit ('C'); Found := Completion_Queued > 0; Item := M.NULL_COMPLETION;
      if Found then Completion_Queued := Completion_Queued - 1; Item := Queued_Completion; end if;
   end Poll_Completion;
   procedure Poll_Event (Publisher : out P.Process_ID; Item : out M.Message; Found : out Boolean) is
   begin
      Visit ('E'); Found := Event_Queued > 0; Publisher := P.No_Process; Item := M.NULL_MESSAGE;
      if Found then Event_Queued := Event_Queued - 1; end if;
   end Poll_Event;
   procedure Drain_Request (Found : out Boolean) is
   begin Visit ('R'); Found := Request_Queued > 0;
      if Found then Request_Queued := Request_Queued - 1; end if;
   end Drain_Request;
   procedure System_Event (Publisher : P.Process_ID; Item : M.Message;
      Stop : out Boolean; Handled : out Boolean) is
   begin
      Check (Publisher = P.No_Process and then Natural (Item.tag.length) = 0);
      Stop := False; Handled := True;
   end System_Event;
   package Pump is new HID_Touchpad_Pump
     (Native, Publication, 2, Sample_Time, Cache_Time, Poll_Completion,
      Poll_Event, Drain_Request, System_Event);
   Pump_Result : Pump.Report;
   Dispatch_Limit : W.Word := Pump.No_Startup_Deadline;
   package Waiter is new HID_Touchpad_Native_Wait (Pump);
   use type Pump.Wait_Kind;
   procedure Pump_Turn is
      Before : constant Natural := Samples;
   begin
      Trace_Count := 0; Trace := [others => ' '];
      Pump.Turn (Dispatch_Limit, Pump_Result);
      Check (Samples = Before + 1 and then Cache_Updates = Samples);
   end Pump_Turn;
   procedure Check_Wire (Item : Input_Pending.Item) is
      Message : M.Message := Last_Publication;
      Report : I.Source_Report;
      Valid : Boolean;
   begin
      Check (Message.authorityTag = 0); -- encoding does not forge kernel authority
      Message.authorityTag := I.Publisher_Tag; -- model kernel stamping, not transport
      I.Decode (Message, Source_Identity, Report, Valid);
      Check (Valid and then Report.sourceIdentity = P.To_Word (Source_Identity) and then Report.generation = 1);
      Check (Report.device = I.RELATIVE_POINTER and then Report.delivery = I.ACCUMULABLE_DISPLACEMENT);
      Check (Report.sequence = Item.Sequence and then Report.payload = Item.Payload and then Report.flags (I.RESYNCHRONIZE) = Item.Recover);
      Check (I.Pointer_Time (Report.snapshot) = Item.Observed_Ms and then (Report.snapshot and 16#FF#) = (Item.Payload and 16#FF#));
   end Check_Wire;
   use type App.Device.Phase, Live.Owner.Start_Result, Live.Owner.Phase;
   use type Controls.Receive_Result, GW.Control_Kind;
   Live_Start : Live.Owner.Start_Result;
   Event_Result : Native.Event_Result;
   Generation : GW.Word := 0;
   procedure App_Step (Now : W.Word; Available : Boolean) is
   begin Cached_Now := Now; Native.Step (Now, Available); end App_Step;
   procedure GPIO_Complete (Now : W.Word) is
      Result : Native.Completion_Report;
      Entry_Value : M.CompletionEntry := M.NULL_COMPLETION;
      Reply_Lease : constant GW.Word := (if Live.Observe.At_Phase = Live.Owner.Cold then 42 else 43);
      Reply_Generation : constant GW.Word := (if GPIO_Request.Kind = GW.Acknowledge then GPIO_Request.Generation_To_Ack else 0);
      Words : constant GW.Message_Words := GW.Encode_Completion ((GPIO_Request.Kind, GW.Completed, Reply_Lease, Reply_Generation));
      Expected : constant Native.Destination := (if Live.Observe.At_Phase = Live.Owner.Cold then Native.GPIO_Reset_Control else Native.GPIO_Input_Control);
   begin
      Cached_Now := Now; Entry_Value.valid := True; Entry_Value.token := GPIO_Token; Entry_Value.from := P.To_Word (GPIO_Server);
      Entry_Value.msg.tag.label := GW.Label_Of (GPIO_Request.Kind); Entry_Value.msg.tag.protocol := M.Protocol_Flags (GW.Protocol_Version);
      Entry_Value.msg.tag.length := Words'Length;
      for Index in Words'Range loop Entry_Value.msg.words (Index - Words'First) := Words (Index); end loop;
      Native.Receive_Completion (Entry_Value, Now, True, Result);
      Check (Result.Frame_Valid and then Result.Routed_To = Expected and then Result.GPIO_Result = Controls.Accepted); App_Step (Now, True);
   end GPIO_Complete;
   procedure Native_Event (Lease, Epoch : GW.Word) is
      Message : M.Message := M.NULL_MESSAGE;
      Words : constant GW.Message_Words := GW.Encode_Event ((Lease, Epoch));
   begin
      Message.tag.label := GW.OP_GPIO_EVENT; Message.tag.protocol := M.Protocol_Flags (GW.Protocol_Version);
      Message.tag.length := Words'Length; Message.authorityTag := Badge;
      for Index in Words'Range loop Message.words (Index - Words'First) := Words (Index); end loop;
      Native.Receive_Event (GPIO_Server, Message, Event_Result);
   end Native_Event;
   procedure Read_Event (Now : W.Word) is
   begin
      Generation := Generation + 1; Native_Event (43, Generation);
      Check (Event_Result = Native.Input_Buffered); App_Step (Now, True);
      Check (App.Observe.At_Phase = App.Device.Read_Input and then Live.Observe.At_Phase = Live.Owner.Reading);
   end Read_Event;
   package Ambiguous is new HID_I2C_Touchpad (C, 512, 32, 0, 5, 2, 8, 16, 32, 64, 8, Next_Token, Arm, Poll_Arm, Release_Latch, Poll_Release);
   package Budgeted is new HID_I2C_Touchpad (C, 512, 32, 0, 5, 2, 8, 16, 32, 64, 8, Next_Token, Arm, Poll_Arm, Release_Latch, Poll_Release);
   package Too_Small is new HID_I2C_Touchpad (C, 512, 32, 0, 5, 2, 8, 16, 1, 64, 8, Next_Token, Arm, Poll_Arm, Release_Latch, Poll_Release);
   use type App.Start_Result, App.Admission_Status, App.Device.Failure;
   use type App.Source.Status, App.Schema.Count;
   use type Ambiguous.Start_Result, Ambiguous.Admission_Status;
   use type Budgeted.Start_Result, Budgeted.Admission_Status, Budgeted.Bindings.Work_Count;
   use type Too_Small.Start_Result, Too_Small.Admission_Status, Too_Small.Schema.Status, Too_Small.Schema.Count;
   use type C.Close_Result, C.Transactions.Completion_Result, C.Transactions.Delivery_Status;
   use type Input_Pending.Item;
   Settings : App.Configuration := (5, 100_000, (0, 4095), (0, 4095), (1, 4), (1, 4),
      App.Source.Pointer.As_Reported, App.Source.Pointer.Reversed, App.Source.Pointer.Require_Confident,
      [App.Source.Primary_Button, App.Source.Ignore, App.Source.Secondary_Button], App.Source.Caller_Bounds);
   Started : App.Start_Result;
   Closed : C.Close_Result;
   Server : constant P.Process_ID := P.From_Word (16#F123_4567_0000_1234#);
   Descriptor : D.Bytes (0 .. 511) := [others => 0]; Used : Natural := 0;
   Header : D.Bytes (0 .. 29) := [30, 0, 0, 1, 0, 0, 16#22#, 0, 16#24#, 0, 32, 0, 0, 0, 0, 0, 16#30#, 0, 16#32#, 0, others => 0];
   Saved : Input_Pending.Item;
   procedure Item (Prefix : D.Byte; Value : App.Schema.Word; Width : Positive := 1) is
      Remaining : App.Schema.Word := Value;
   begin
      Descriptor (Used) := Prefix; Used := Used + 1;
      for N in 1 .. Width loop Descriptor (Used) := D.Byte (Remaining mod 256); Used := Used + 1; Remaining := Remaining / 256; end loop;
   end Item;
   procedure End_Collection is
   begin Descriptor (Used) := 16#C0#; Used := Used + 1; end End_Collection;
   procedure Build (Duplicate : Boolean := False) is
   begin
      Used := 0;
      -- Decoy top-level mouse first: the real touchpad root is ordinal two.
      Item (16#05#, 1); Item (16#09#, 2); Item (16#A1#, 1); End_Collection;
      Item (16#05#, 13); Item (16#09#, 5); Item (16#A1#, 1); Item (16#85#, 7);
      Item (16#09#, 16#22#); Item (16#A1#, 2);
      Item (16#15#, 0); Item (16#25#, 1); Item (16#09#, 16#47#); Item (16#09#, 16#42#);
      Item (16#75#, 1); Item (16#95#, 2); Item (16#81#, 2); Item (16#95#, 6); Item (16#81#, 3);
      Item (16#09#, 16#51#); Item (16#75#, 8); Item (16#95#, 1); Item (16#25#, 255); Item (16#81#, 2);
      Item (16#05#, 1); Item (16#26#, 4095, 2); Item (16#75#, 16);
      Item (16#09#, 16#30#); Item (16#81#, 2); Item (16#09#, 16#31#); Item (16#81#, 2); End_Collection;
      Item (16#05#, 13); Item (16#09#, 16#54#); Item (16#25#, 5);
      Item (16#75#, 8); Item (16#95#, 1); Item (16#81#, 2);
      Item (16#09#, 16#56#); Item (16#26#, 65_535, 2); Item (16#75#, 16); Item (16#55#, 12);
      Item (16#66#, 16#1001#, 2); Item (16#81#, 2);
      Item (16#05#, 9); Item (16#09#, 1); Item (16#09#, 2); Item (16#09#, 3); Item (16#25#, 1);
      Item (16#75#, 1); Item (16#95#, 3); Item (16#81#, 2); Item (16#95#, 5); Item (16#81#, 3); End_Collection;
      if Duplicate then Item (16#05#, 13); Item (16#09#, 5); Item (16#A1#, 1); End_Collection; end if;
      Header (4) := D.Byte (Used mod 256); Header (5) := D.Byte (Used / 256);
   end Build;
   procedure Packet (X, Y : Natural; Buttons : D.Byte := 1) is
   begin
      Output_Page (0 .. 31) := [others => 0];
      Output_Page (0 .. 4) := [13, 0, 7, 3, 11];
      Output_Page (5) := D.Byte (X mod 256); Output_Page (6) := D.Byte (X / 256);
      Output_Page (7) := D.Byte (Y mod 256); Output_Page (8) := D.Byte (Y / 256);
      Output_Page (9 .. 12) := [1, 100, 0, Buttons];
   end Packet;
   generic
      with procedure Receive (Token : W.Word; Sender : P.Process_ID; Delivery : C.Transactions.Delivery_Status;
         Label : W.Operation; Protocol : W.Word; Words : W.Message_Words; Now : W.Word;
         Clock_Available : Boolean; Result : out C.Transactions.Completion_Result);
      with procedure Step (Now : W.Word; Clock_Available : Boolean);
   procedure Complete (Data : D.Bytes; Now : W.Word; Retire : Boolean := True);
   procedure Complete (Data : D.Bytes; Now : W.Word; Retire : Boolean := True) is
      Completion : constant W.Completion := (W.Completed, 77, True, True, Last_Request.Write_Count, Last_Request.Read_Count);
      Received : C.Transactions.Completion_Result;
   begin
      Check (W.Word (Data'Length) <= W.Word (Last_Request.Read_Count));
      if Last_Request.Read_Count > 0 then Output_Page (0 .. Natural (Last_Request.Read_Count) - 1) := [others => 0]; end if;
      if Data'Length > 0 then Output_Page (0 .. Data'Length - 1) := Data; end if;
      Receive (Last_Token, Server, C.Transactions.Delivered, W.Transfer_Reply_Operation,
         W.Protocol_Version, W.Encode_Completion (Completion), Now, True, Received);
      Check (Received = C.Transactions.Accepted);
      if Retire then Input_Retired := True; Output_Retired := True; end if;
      Step (Now, True);
   end Complete;
   procedure Native_Receive (Token : W.Word; Sender : P.Process_ID; Delivery : C.Transactions.Delivery_Status;
      Label : W.Operation; Protocol : W.Word; Words : W.Message_Words; Now : W.Word;
      Clock_Available : Boolean; Result : out C.Transactions.Completion_Result) is
      Entry_Value : M.CompletionEntry := M.NULL_COMPLETION;
      Received : Native.Completion_Report;
   begin
      Check (Delivery = C.Transactions.Delivered);
      Cached_Now := Now; Entry_Value.valid := True; Entry_Value.token := Token; Entry_Value.from := P.To_Word (Sender);
      Entry_Value.msg.tag.label := Label; Entry_Value.msg.tag.protocol := M.Protocol_Flags (Protocol);
      Entry_Value.msg.tag.length := Words'Length;
      for Index in Words'Range loop Entry_Value.msg.words (Index - Words'First) := Words (Index); end loop;
      Native.Receive_Completion (Entry_Value, Now, Clock_Available, Received);
      Check (Received.Frame_Valid and then Received.Routed_To = Native.I2C_Transfer); Result := Received.I2C_Result;
   end Native_Receive;
   procedure App_Complete is new Complete (Native_Receive, App_Step);
   procedure Ambiguous_Complete is new Complete (Ambiguous.Receive, Ambiguous.Step);
   procedure Budgeted_Complete is new Complete (Budgeted.Receive, Budgeted.Step);
   procedure Small_Complete is new Complete (Too_Small.Receive, Too_Small.Step);
   procedure Payload (Buttons : Pointer_Pending.Button_State; X, Y : Pointer_Pending.Displacement := 0) is
   begin Check (App.Count > 0); Check (App.Head.Payload = Pointer_Pending.Encode ((Buttons, X, Y, 0, 0))); end Payload;
begin
   Check (Ada.Command_Line.Argument_Count = 0 or else
      (Ada.Command_Line.Argument_Count = 1 and then Ada.Command_Line.Argument (1) in "startup-cutoff" | "startup-after-cutoff"));
   Build;
   Settings.Binding_Budget := 0; App.Start (Server, 16#20#, Settings, 1000, 1, True, Started);
   Check (Started = App.Invalid_Budget and then Allocate_Calls = 0);
   Settings.Binding_Budget := 100_000; Settings.X_Gain := (1, 1);
   App.Start (Server, 16#20#, Settings, 1000, 1, True, Started); Check (Started = App.Invalid_Calibration and then Allocate_Calls = 0);
   Settings.X_Gain := (1, 4); Settings.Coordinates := App.Source.Report_Bounds;
   Settings.X_Range := (0, 0); Settings.Y_Range := (0, 0);
   App.Start (Server, 16#20#, Settings, 1000, 1, True, Started);
   Check (Started = App.Started and then not App.Ready and then Input_Page (0 .. 1) = [16#20#, 0]);
   App_Complete (Header, 10); App_Complete ([], 20);
   Check (App.Observe.At_Phase = App.Device.Arming_Reset and then Submit_Calls = 2);
   Check (Reset.Wake_Deadline (20, 5000) = 1000);
   Check (Native.Wake_Deadline (20, 19, False) = 19);
   GPIO_Complete (21); Native_Event (42, 1); Check (Event_Result = Native.Reset_Latched);
   App_Complete ([], 30); Check (App.Wake_Deadline (31, 5000) = 1000); App.Step (40, True);
   App_Complete ([0, 0], 50); Check (App.Observe.At_Phase = App.Device.Releasing_Reset); GPIO_Complete (51);
   App_Complete (Descriptor (0 .. Used - 1), 60);
   Check (App.Ready and then App.Admission.Outcome = App.Source_Configured and then App.Admission.Touchpad = 2);
   Payload (0); Check (App.Head.Recover and then App.Head.Observed_Ms = 60);
   Check (Native.Wake_Deadline (60, 5000, True) = 61);
   Check (Native.Wake_Deadline (60, 5000, False) = 5000);
   Flush (60, False); Check (Published.Consumer_Changed and then Published.Blocked and then Published.Accepted = 0);
   Saved := App.Head; Check_Wire (Saved);
   Flush (60, False); Check (not Published.Consumer_Changed and then App.Head = Saved and then Published.Pending = 1);
   Check_Wire (Saved); Flush (60, True);
   Check (Published.Accepted = 1 and then Published.Pending = 0 and then App.Count = 0);
   declare Attempts : constant Natural := Publication_Attempts; begin
      Current_Consumer := P.No_Process; Flush (60, False);
      Check (Published.No_Consumer and then Published.Consumer_Changed and then Publication_Attempts = Attempts);
      Current_Consumer := Consumer_One; Flush (60, True); Check (Published.Consumer_Changed and then App.Count = 0);
   end;
   Pump_Turn; Live_Start := (if Live.Observe.At_Phase = Live.Owner.Arming then Live.Owner.Started else Live.Owner.Refused);
   Check (Pump_Result.Wait_For = Pump.Until_Time and then Pump_Result.Deadline = 111);
   Check (Live_Start = Live.Owner.Started and then not Live.Ready and then GPIO_Request.Kind = GW.Arm);
   Check (Live.Control_Token = GPIO_Token);
   Check (Live.Wake_Deadline (61, 5000) = 111);
   Check (Native.Wake_Deadline (61, 5000, False) = 111);
   if Ada.Command_Line.Argument_Count = 1 then
      declare
         Before : constant Natural := Submit_Calls;
         Words : constant GW.Message_Words := GW.Encode_Completion ((GW.Arm, GW.Completed, 43, 0));
      begin
         Native_Event (43, 1); Check (Event_Result = Native.Input_Buffered);
         Queued_Completion.valid := True; Queued_Completion.token := GPIO_Token;
         Queued_Completion.from := P.To_Word (GPIO_Server);
         Queued_Completion.msg.tag.label := GW.OP_GPIO_ARM;
         Queued_Completion.msg.tag.protocol := M.Protocol_Flags (GW.Protocol_Version);
         Queued_Completion.msg.tag.length := Words'Length;
         for Index in Words'Range loop Queued_Completion.msg.words (Index - Words'First) := Words (Index); end loop;
         Completion_Queued := 1;
         Pump_Time := (if Ada.Command_Line.Argument (1) = "startup-cutoff" then 62 else 63);
         Dispatch_Limit := 62;
         Pump_Turn;
         Check (Pump_Result.Stopping and then not Pump_Result.Ready);
         Check (Submit_Calls = Before and then App.Observe.At_Phase /= App.Device.Read_Input);
         Check (Live.Observe.Stopping and then GPIO_Request.Kind = GW.Release_Pin);
         Check (GPIO_Request.Limit.At_Millisecond = Pump_Time + 20 and then not Live.Quiescent);
         GPIO_Complete (Pump_Time + 1); Check (Live.Quiescent and then App.Quiescent and then Submit_Calls = Before);
         App.Close (Closed); Check (Closed = C.Closed);
         Flush (Pump_Time + 1, True); Check (Native.Drained);
         Ada.Text_IO.Put_Line ("HID touchpad publication checks" & Natural'Image (Checks)); return;
      end;
   end if;
   GPIO_Complete (62);
   Check (Live.Ready and then Live.Control_Token = 0);
   Read_Event (70);
   Packet (100, 200);
   declare Data : constant D.Bytes := Output_Page (0 .. 31); begin App_Complete (Data, 71, False); end;
   Check (App.Count = 0 and then App.Wake_Deadline (71, 5000) = 72 and then GPIO_Request.Kind = GW.Arm);
   Input_Retired := True; Output_Retired := True; App_Step (72, True);
   Check (GPIO_Request.Kind = GW.Acknowledge and then Live.Observe.At_Phase = Live.Owner.Acknowledging); GPIO_Complete (73);
   Check (App.Ready and then App.Input_Observation.Outcome = App.Source.Input_Updated); Payload (1);
   Check (App.Head.Observed_Ms = 72 and then not App.Head.Recover); Saved := App.Head; Flush (73, True); Check_Wire (Saved);
   Read_Event (80); Packet (104, 208);
   declare Data : constant D.Bytes := Output_Page (0 .. 31); begin App_Complete (Data, 81); end;
   GPIO_Complete (81); Payload (1, 1, -2); Saved := App.Head; Check (App.Head = Saved and then App.Count = 1);
   Read_Event (82); Packet (108, 216);
   declare Data : constant D.Bytes := Output_Page (0 .. 31); begin App_Complete (Data, 83); end;
   GPIO_Complete (83); Payload (1, 2, -4); Check (App.Count = 1 and then App.Head.Sequence = Saved.Sequence and then App.Head.Observed_Ms = 81);
   Saved := App.Head; Flush (83, False); Check (App.Head = Saved and then Published.Blocked);
   Check_Wire (Saved);
   Read_Event (84); Packet (108, 216, 0);
   declare Data : constant D.Bytes := Output_Page (0 .. 31); begin App_Complete (Data, 85); end;
   GPIO_Complete (85); Check (App.Count = 2);
   Flush (86, True); Check_Wire (Saved);
   Check (Published.Accepted = 1 and then Published.Pending = 1); Payload (0);
   Saved := App.Head; Flush (87, True); Check_Wire (Saved);
   Check (App.Count = 0 and then App.Wake_Deadline (87, 5000) = 5000);
   Check (Native.Wake_Deadline (87, 5000, True) = 5000);
   Read_Event (90); Packet (65535, 216);
   declare Data : constant D.Bytes := Output_Page (0 .. 31); begin App_Complete (Data, 91); end;
   GPIO_Complete (91); Check (App.Ready and then App.Input_Observation.Outcome = App.Source.Input_Rejected); Payload (0); Check (App.Head.Recover); Saved := App.Head; Flush (91, True); Check_Wire (Saved);
   Read_Event (100); App_Complete ([1, 0], 101);
   GPIO_Complete (101); Check (App.Ready and then App.Input_Observation.Outcome = App.Source.Source_Reset); Payload (0); Saved := App.Head; Flush (101, True); Check_Wire (Saved);
   Current_Consumer := Consumer_Two; Flush (102, False); Payload (0);
   Check (Published.Consumer_Changed and then Published.Consumer_Identity = Consumer_Two and then App.Head.Recover and then App.Head.Observed_Ms = 102);
   Saved := App.Head; Check_Wire (Saved); Flush (102, True); Check (App.Count = 0);
   Read_Event (110); App_Complete ([0, 0], 111);
   Check (not App.Ready and then App.Observe.Reason = App.Device.Unexpected_Device_Reset and then App.Quiescent);
   Check (not Live.Ready and then not Live.Quiescent and then GPIO_Request.Kind = GW.Release_Pin);
   GPIO_Complete (112); Check (Live.Quiescent and then Live.Control_Token = 0);
   Payload (0); App.Close (Closed); Check (Closed = C.Closed and then not Input_Allocated and then not Output_Allocated);
   Check (not Native.Drained);
   Current_Consumer := P.No_Process; Flush (112, True);
   Check (Published.No_Consumer and then not Native.Drained);
   Current_Consumer := Consumer_One;
   Saved := App.Head; Flush (112, False);
   Check (Published.Blocked and then not Native.Drained);
   Saved := App.Head; Flush (112, False);
   Check (Published.Blocked and then not Native.Drained and then App.Head = Saved);
   Flush (112, True); Check_Wire (Saved); Check (App.Count = 0 and then Native.Drained);
   Build (True);
   declare Config : constant Ambiguous.Configuration := (5, 100_000, (0, 4095), (0, 4095), (1, 4), (1, 4),
      Ambiguous.Source.Pointer.As_Reported, Ambiguous.Source.Pointer.Reversed, Ambiguous.Source.Pointer.Require_Confident,
      [Ambiguous.Source.Primary_Button, Ambiguous.Source.Ignore, Ambiguous.Source.Secondary_Button], Ambiguous.Source.Caller_Bounds);
      Status : Ambiguous.Start_Result; Event : Ambiguous.Device.Interrupt_Result;
   begin
      Ambiguous.Start (Server, 16#20#, Config, 1000, 200, True, Status); Check (Status = Ambiguous.Started);
      Ambiguous_Complete (Header, 210); Ambiguous_Complete ([], 220); Ambiguous.Reset_Interrupt (42, Event);
      Ambiguous_Complete ([], 230); Ambiguous.Step (240, True); Ambiguous_Complete ([0, 0], 250);
      Ambiguous_Complete (Descriptor (0 .. Used - 1), 260);
      Check (not Ambiguous.Ready and then Ambiguous.Admission.Outcome = Ambiguous.Touchpad_Ambiguous and then Ambiguous.Quiescent);
      Ambiguous.Close (Closed); Check (Closed = C.Closed);
   end;
   Build;
   declare Config : constant Budgeted.Configuration := (5, 1, (0, 4095), (0, 4095), (1, 4), (1, 4),
      Budgeted.Source.Pointer.As_Reported, Budgeted.Source.Pointer.Reversed, Budgeted.Source.Pointer.Require_Confident,
      [Budgeted.Source.Primary_Button, Budgeted.Source.Ignore, Budgeted.Source.Secondary_Button], Budgeted.Source.Caller_Bounds);
      Status : Budgeted.Start_Result; Event : Budgeted.Device.Interrupt_Result;
   begin
      Budgeted.Start (Server, 16#20#, Config, 1000, 300, True, Status); Check (Status = Budgeted.Started);
      Budgeted_Complete (Header, 310); Budgeted_Complete ([], 320); Budgeted.Reset_Interrupt (42, Event);
      Budgeted_Complete ([], 330); Budgeted.Step (340, True); Budgeted_Complete ([0, 0], 350);
      Budgeted_Complete (Descriptor (0 .. Used - 1), 360);
      Check (not Budgeted.Ready and then Budgeted.Admission.Outcome = Budgeted.Root_Budget_Exhausted
         and then Budgeted.Admission.Root_Charge = 1 and then Budgeted.Admission.Binding_Charge = 0);
      Budgeted.Close (Closed); Check (Closed = C.Closed);
   end;
   declare Config : constant Too_Small.Configuration := (5, 100_000, (0, 4095), (0, 4095), (1, 4), (1, 4),
      Too_Small.Source.Pointer.As_Reported, Too_Small.Source.Pointer.Reversed, Too_Small.Source.Pointer.Require_Confident,
      [Too_Small.Source.Primary_Button, Too_Small.Source.Ignore, Too_Small.Source.Secondary_Button], Too_Small.Source.Caller_Bounds);
      Status : Too_Small.Start_Result; Event : Too_Small.Device.Interrupt_Result;
   begin
      Too_Small.Start (Server, 16#20#, Config, 1000, 400, True, Status); Check (Status = Too_Small.Started);
      Small_Complete (Header, 410); Small_Complete ([], 420); Too_Small.Reset_Interrupt (42, Event);
      Small_Complete ([], 430); Too_Small.Step (440, True); Small_Complete ([0, 0], 450);
      Small_Complete (Descriptor (0 .. Used - 1), 460);
      Check (not Too_Small.Ready and then Too_Small.Admission.Outcome = Too_Small.Parser_Refused
         and then Too_Small.Admission.Parser = Too_Small.Schema.Need_Capacity and then Too_Small.Admission.Fields_Required > 1);
      Too_Small.Close (Closed); Check (Closed = C.Closed);
   end;
   Check (not Input_Allocated and then not Output_Allocated and then Submit_Calls > 0 and then Free_Calls = 8);
   Check (GPIO_Calls = 10 and then Publication_Attempts > 0);
   -- The actual parser/device/GPIO composition is now stopped. Exercise fair
   -- queue scheduling without replacing its lifecycle with a policy mock.
   Pump_Time := 120; Completion_Queued := 3; Event_Queued := 3; Request_Queued := 3;
   Pump_Turn;
   Check (Trace = "CERCER" and then Pump_Result.Completions = 2 and then
      Pump_Result.Events = 2 and then Pump_Result.Requests = 2);
   Check (Pump_Result.Wait_For = Pump.Immediate and then Pump_Result.Stopping);
   Check (Completion_Queued = 1 and then Event_Queued = 1 and then Request_Queued = 1);
   Pump_Turn;
   Check (Trace = "CERCER" and then Pump_Result.Completions = 1 and then
      Pump_Result.Events = 1 and then Pump_Result.Requests = 1);
   Check (Pump_Result.Wait_For = Pump.Activity and then Pump_Result.Quiescent);
   Pump_Time := 119; Pump_Turn;
   Check (Pump_Result.Wait_For = Pump.Clock_Backoff and then not Cached_Available and then Pump_Result.Stopping);
   Pump_Time := 121; Pump_Turn;
   Check (Pump_Result.Clock_Available and then Pump_Result.Stopping and then not Pump_Result.Ready);
   Pump_Clock := False; Pump_Turn;
   Check (Pump_Result.Wait_For = Pump.Clock_Backoff and then not Pump_Result.Clock_Available);
   Pump_Clock := True; Pump_Time := 122; Pump_Turn;
   Check (Pump_Result.Stopping and then Pump_Result.Quiescent and then Pump_Result.Wait_For = Pump.Activity);
   declare
      Plan : Pump.Report := Pump_Result;
      Result : M.Activity_Result;
      Backoff : constant Waiter.Backoff_Milliseconds := 7;
      use type M.Activity_Result;
   begin
      Plan.Wait_For := Pump.Immediate; Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Work_Available and then M.Wait_Calls = 0 and then M.Sleep_Calls = 0);
      Plan.Wait_For := Pump.Activity; M.Wait_Result := M.Work_Available;
      Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Work_Available and then M.Wait_Argument = M.Wait_Forever and then M.Sleep_Calls = 0);
      Plan.Wait_For := Pump.Until_Time; Plan.Deadline := 123; M.Wait_Result := M.Deadline_Reached;
      Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Deadline_Reached and then M.Wait_Argument = 123 and then M.Wait_Calls = 2);
      M.Wait_Result := M.Unavailable; Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Deadline_Reached and then M.Sleep_Calls = 1 and then M.Sleep_Argument = Backoff);
      Plan.Wait_For := Pump.Clock_Backoff; M.Sleep_Status := W.Word'Last;
      Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Unavailable and then M.Wait_Calls = 3 and then M.Sleep_Calls = 2);
      M.Sleep_Status := 0; Waiter.Await (Plan, Backoff, Result);
      Check (Result = M.Deadline_Reached and then M.Wait_Calls = 3 and then M.Sleep_Calls = 3);
   end;
   declare
      procedure Submit_Test (Item : GW.Request; Token : GW.Word; Accepted : out Boolean) is
      begin Check (Item.Kind = GW.Arm and then Token > 0); Accepted := True; end Submit_Test;
      package Timed is new GPIO_Client_Controls (Submit_Test);
      State : Timed.State;
      Started_Control : Timed.Start_Result;
      Received_Control : Timed.Receive_Result;
      use type Timed.Phase, Timed.Start_Result, Timed.Receive_Result;
      Item : GW.Request := (GW.Arm, (GW.Within, 110));
      Reply : constant GW.Message_Words := GW.Encode_Completion ((GW.Arm, GW.Completed, 42, 0));
   begin
      Check (Timed.Wake_Deadline (State, 100, 5000) = 5000);
      Timed.Start (State, Item, GPIO_Server, 1, 100, True, Started_Control);
      Check (Started_Control = Timed.Started);
      Check (Timed.Wake_Deadline (State, 100, 5000) = 110);
      Check (Timed.Wake_Deadline (State, 100, 105) = 105);
      Check (Timed.Wake_Deadline (State, 111, 5000) = 110);
      Check (Timed.Observe (State).At_Phase = Timed.Waiting);
      Timed.Step (State, 110, True);
      Check (Timed.Observe (State).At_Phase = Timed.Quarantined);
      Check (Timed.Observe (State).Effects_Possible);
      Check (Timed.Wake_Deadline (State, 110, 5000) = 5000);
      Timed.Receive (State, 1, GPIO_Server, Timed.Delivered,
         GW.OP_GPIO_ARM, GW.Protocol_Version, Reply, 111, True, Received_Control);
      Check (Received_Control = Timed.Accepted and then Timed.Observe (State).Late);
      Check (Timed.Wake_Deadline (State, 111, 5000) = 111);
      Timed.Acknowledge (State);
      Check (Timed.Wake_Deadline (State, 111, 5000) = 5000);
      Item := (GW.Arm, (Kind => GW.Forever));
      Timed.Start (State, Item, GPIO_Server, 2, 112, True, Started_Control);
      Check (Started_Control = Timed.Started);
      Check (Timed.Wake_Deadline (State, 112, 5000) = 5000);
      Timed.Step (State, 112, False);
      Check (Timed.Observe (State).At_Phase = Timed.Quarantined);
      Check (Timed.Wake_Deadline (State, GW.Word'Last, GW.Word'Last) = GW.Word'Last);
   end;
   declare
      package Launch is new HID_Launch_Adapter (App);
      package Params renames Parameters;
      use type Launch.Status, Params.HID_Register_Value, App.D.Register_Number;
      use type App.Source.Coordinate_Policy, App.Bindings.Work_Count;
      Canonical : constant String :=
        "(HID_Parameters descriptor_register => 0" &
        " x => (HID_Axis numerator => 65535 denominator => 2147483647 direction => HID_Direction.As_Reported)" &
        " y => (HID_Axis numerator => 1 denominator => 4 direction => HID_Direction.Reversed)" &
        " confidence => HID_Confidence.Accept_Unknown" &
        " buttons => (HID_Buttons button_1 => HID_Button.Primary_Button button_2 => HID_Button.Secondary_Button button_3 => HID_Button.Ignore)" &
        " contact_limit => 5 binding_budget => 4294967295" &
        " timing => (HID_Timing startup => 1000 control => 50 read => 100 cleanup => 20 idle_backoff => 7))";
      Raw, Saved : Params.HID_Parameters_Value;
      Decoded : Boolean;
      Translated : Launch.Result;
      Bound : Launch.Deadline_Result;
      procedure Reject (Old, Replacement : String) is
         At_Byte : constant Natural := Ada.Strings.Fixed.Index (Canonical, Old);
      begin
         Check (At_Byte > 0);
         Params.Decode (Canonical (1 .. At_Byte - 1) & Replacement &
            Canonical (At_Byte + Old'Length .. Canonical'Last), Raw, Decoded);
         Check (not Decoded);
      end Reject;
   begin
      Params.Decode (Canonical, Raw, Decoded);
      Check (Decoded and then Raw.descriptor_register = 0);
      Saved := Raw; Translated := Launch.Translate (Raw);
      Check (Translated.Outcome = Launch.Accepted and then Translated.Descriptor_Register = 0);
      Check (Translated.Settings.Coordinates = App.Source.Report_Bounds);
      Check (Translated.Settings.Binding_Budget = App.Bindings.Work_Count'Last);
      for Cut in 0 .. Canonical'Length - 1 loop
         Params.Decode (Canonical (1 .. Cut), Raw, Decoded); Check (not Decoded);
      end loop;
      Params.Decode (Canonical & "x", Raw, Decoded); Check (not Decoded);
      Reject ("descriptor_register => 0", "descriptor_register => 65536");
      Reject ("denominator => 2147483647", "denominator => 0");
      Reject ("startup => 1000", "startup => 0");
      Reject ("contact_limit => 5", "contact_limit => 33");
      Raw := Saved; Raw.contact_limit := Params.HID_Contact_Limit_Value'Last;
      Check (Launch.Translate (Raw).Outcome = Launch.Contact_Capacity_Exceeded);
      Bound := Launch.Deadline (W.Millisecond'Last - 1, 1);
      Check (Bound.Available and then Bound.Limit = W.Millisecond'Last);
      Check (not Launch.Deadline (W.Millisecond'Last, 1).Available);
      Check (not Launch.Deadline (W.Word'Last, 1).Available);
      Bound := Launch.Deadline (0, Params.HID_Milliseconds_Value'Last);
      Check (Bound.Available and then Bound.Limit = W.Word (Params.HID_Milliseconds_Value'Last));
   end;
   Ada.Text_IO.Put_Line ("HID touchpad publication checks" & Natural'Image (Checks));
end Touchpad_Publication_Tests;
