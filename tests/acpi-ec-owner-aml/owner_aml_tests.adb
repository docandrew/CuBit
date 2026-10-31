pragma Ada_2022;
with Ada.Command_Line; with Ada.Text_IO; with Ada.Streams; with Ada.Streams.Stream_IO;
with Interfaces; with ACPI_Service_Core; with ACPI_Lid; with ACPI_EC_Types;
with ACPI_EC_Backend_Owner; with ACPI_EC_Backend_Owner.With_Receive;
with ACPI_EC_Event_Types; with ACPI_EC_Clock_Policy;
with ACPI_EC_Methods; with ACPI_EC_Event_Receiver;
with ACPI_GPE_Lifecycle;
with CuBit.Handles.IPC; with CuBit.Handles.Testing; with CuBit.Messages;
with ACPI_Field_Remote_Provider; with ACPI_Region_Protocol;
with ACPI_FADT.Transactions; with AML_Delays; with AML_Execute; with AML_Field_Access;
with AML_Field_Layout; with AML_Field_Targets; with AML_Names; with Firmware_Tables;
procedure Owner_AML_Tests is
   package EC renames ACPI_EC_Types; package Wire renames ACPI_Region_Protocol;
   package A renames AML_Field_Access; package T renames ACPI_FADT.Transactions;
   package M renames CuBit.Messages; package H renames CuBit.Handles;
   package ET renames ACPI_EC_Event_Types;
   package G renames ACPI_GPE_Lifecycle;
   use type EC.Byte, EC.Microseconds, EC.Work_Budget, EC.Read_Register, EC.Write_Register;
   use type Wire.Time_Instant, Wire.Delivery, Interfaces.Unsigned_64, Interfaces.Unsigned_32, M.Process_ID;
   Mode : constant String := Ada.Command_Line.Argument (2);
   Burst_Mode : constant Boolean := Mode in "gpe-burst-edge" | "gpe-burst-level" |
      "gpe-burst-unknown-publication" | "gpe-burst-uncertain-second";
   GPE_Mode : constant Boolean := Mode in "gpe-edge" | "gpe-level" | "gpe-uncertain-field" | "gpe-budget" or Burst_Mode;
   Scan_Mode : constant Boolean := Mode = "event-scan" or GPE_Mode;
   Checks, IO_Count, Wire_Calls, Saves, Answers, Publications, Currents : Natural := 0;
   Evaluations : Natural := 0;
   In_Publication : Boolean := False;
   Published : ET.Notice_Option;
   Burst_Notices : array (Positive range 1 .. 2) of ET.Notice;
   Query_Number : EC.Byte := 1;
   Event_Mode : constant Boolean := Mode in "event-normal" | "event-unknown-publication" |
      "event-uncertain-field" | "event-budget" | "event-scan" or GPE_Mode;
   Masked, Upper_Settled : Boolean := False;
   Masks, Clears, Enables : Natural := 0;
   Source : constant Interfaces.Unsigned_64 := 16#CAFE_9876_0000_0001#;
   use type G.Action_Result, G.Phase, G.Source_State;
   Now : EC.Microseconds := 1; Last_Write : EC.Microseconds := 0;
   Input_Pending, Output_Ready, Fail_Status, Query_Ready, Live : Boolean := False;
   Stage : Natural := 0; Command, Written : EC.Byte := 0; Cell_Value : EC.Byte := 16#A4#;
   Sender : constant M.Process_ID := M.From_Word (16#ABCD_1234_0000_0001#);
   Register_ID : constant A.Register_ID := 16#FEDC_BA98_7654_3210#;
   Epoch : constant A.Counter := 16#ABCD_EF01_0000_0001#;
   Last_Reply : M.Message := M.NULL_MESSAGE;
   Delivery_Allowed, Reply_Seen : Boolean := True;
   procedure Check (OK : Boolean) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Mode & " joined check" & Checks'Image; end if; end Check;
   procedure Mask_Source (ID : Interfaces.Unsigned_64; Result : out G.IO_Outcome) is
   begin Check (ID = Source and not Masked); Masks := Masks + 1; Masked := True; Result := G.Completed; end Mask_Source;
   procedure Clear_Source (ID : Interfaces.Unsigned_64; Result : out G.IO_Outcome) is
   begin Check (ID = Source and Masked); Clears := Clears + 1; Result := G.Completed; end Clear_Source;
   procedure Enable_Source (ID : Interfaces.Unsigned_64; Result : out G.IO_Outcome) is
   begin Check (ID = Source and Masked and Upper_Settled); Enables := Enables + 1; Masked := False; Result := G.Completed; end Enable_Source;
   procedure No_Inline_Handler (ID : Interfaces.Unsigned_64; Budget : Natural;
      Result : out G.Handler_Outcome; Charged : out Natural) is
      pragma Unreferenced (ID, Budget);
   begin Check (False); Result := G.Not_Invoked; Charged := 0; end No_Inline_Handler;
   package GPE is new G.Driver (Interfaces.Unsigned_64, 0, Mask_Source, Clear_Source, Enable_Source, No_Inline_Handler);
   GPE_Run : GPE.Session;
   procedure Clock (Time : out EC.Microseconds; Available : out Boolean) is
   begin Time := Now; Available := True; end Clock;
   function Owned return Boolean is (True);
   procedure Read (Register : EC.Read_Register; Value : out EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (not GPE_Mode or Masked);
      IO_Count := IO_Count + 1; Value := 0; Result := EC.Completed;
      if Fail_Status then Result := EC.Indeterminate; return; end if;
      if Register = EC.Status_Register then
         if Input_Pending then
            Check (Now >= Last_Write + EC.Input_Assertion_Delay);
            Input_Pending := False;
            if Stage = 1 and Command = EC.Query_Command then Output_Ready := True;
            elsif Stage = 2 then
               Check (Written = 16#10#);
               if Command = EC.Read_Command then Output_Ready := True; end if;
            elsif Stage = 3 then
               Check (Command = EC.Write_Command); Cell_Value := Written; Stage := 0;
            end if;
         end if;
         if Output_Ready then Value := EC.Output_Full; end if;
         if Query_Ready then Value := Value or EC.Event_Pending; end if;
      else
         Check (Output_Ready); Output_Ready := False; Value := (if Command = EC.Query_Command then Query_Number else Cell_Value); Stage := 0;
         if Command = EC.Query_Command then
            Query_Ready := Burst_Mode and Query_Number = 1;
            if Query_Ready then Query_Number := 2; end if;
         end if;
      end if;
   end Read;
   procedure Write (Register : EC.Write_Register; Value : EC.Byte; Result : out EC.Access_Result) is
   begin
      Check (not GPE_Mode or Masked);
      IO_Count := IO_Count + 1; Check (not Input_Pending and not Output_Ready);
      Input_Pending := True; Last_Write := Now; Written := Value; Stage := Stage + 1;
      if Stage = 1 then
         Check (Register = EC.Command_Register and (Value in EC.Read_Command | EC.Write_Command | EC.Query_Command)); Command := Value;
      else Check (Register = EC.Data_Register); end if;
      Result := EC.Completed;
   end Write;
   procedure Pause (Target : EC.Microseconds; Remaining : in out EC.Work_Budget;
      Result : out ACPI_EC_Clock_Policy.Pause_Report) is
   begin
      if Remaining = 0 then Result := (Status => ACPI_EC_Clock_Policy.Budget_Exhausted, others => <>);
      else Remaining := Remaining - 1; Now := Target; Result := (ACPI_EC_Clock_Policy.Pause_Returned, Target); end if;
   end Pause;
   function Save return H.IPC.Saved_Reply is
   begin Check (not Live); Live := True; Saves := Saves + 1;
      return (True, H.Testing.Reply_For_Test (2 ** 32 + Interfaces.Unsigned_64 (Saves))); end Save;
   function Answer_Saved (Item : H.Reply; Msg : M.Message) return Boolean is
   begin Check (Live and H.Valid (Item)); Live := False; Answers := Answers + 1;
      Last_Reply := Msg; Reply_Seen := True; return Delivery_Allowed;
   end Answer_Saved;
   function Answer_Current (From : M.Process_ID; Msg : M.Message) return Interfaces.Unsigned_64 is
   begin Check (From = Sender); Currents := Currents + 1; Last_Reply := Msg; Reply_Seen := True; return 1; end Answer_Current;
   procedure Publish (Item : ET.Notice; Result : out ET.Publication) is
      use type ET.Controller_ID, ET.Epoch_ID, ET.Event_Number;
   begin
      In_Publication := True;
      Check (Item.Controller = 42 and Item.Epoch = 1);
      if Burst_Mode then
         Check (Publications < Burst_Notices'Length and Natural (Item.Number) = Publications + 1);
         Burst_Notices (Publications + 1) := Item;
      else Check (Item.Number = 1 and not Published.Present); end if;
      Publications := Publications + 1;
      Published := (Present => True, Item => Item);
      Result := (if Mode = "event-unknown-publication" or else
        (Mode = "gpe-burst-unknown-publication" and Publications = 1) then ET.Indeterminate else ET.Enqueued);
      In_Publication := False;
   end Publish;
   package Owner is new ACPI_EC_Backend_Owner
     (Read, Write, Owned, Publish, 1000, 1000, 64, Expected_Consumer => Sender,
      Clock => Clock, Pause => Pause, Save => Save, Answer_Saved => Answer_Saved, Answer_Current => Answer_Current);
   use type Owner.Setup_Result, Owner.Query_Request_Result;
   procedure Event (From : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      Result : Owner.Query_Request_Result;
   begin
      Stop := From = M.No_Process and Item.tag.label = 16#100#; Handled := Stop;
      if From = Sender and Item.tag.label = 16#101# then
         if Scan_Mode then Owner.Request_Query_Scan (1, 3, Result);
         else Owner.Request_Query (1, Result); end if;
         Check (Result = Owner.Request_Queued); Handled := True;
      end if;
   end Event;
   function Idle return Boolean is (True);
   package Receiver is new Owner.With_Receive (Handle_Event => Event, Wait_Idle => Idle);
   use type Receiver.Stop_Reason;
   type Transport is limited null record;
   procedure Exchange (State : in out Transport; Destination : Natural; Deadline : Wire.Time_Instant;
      Item : Wire.Packet; Reply : out Wire.Packet; Delivered : out Wire.Delivery) is
      pragma Unreferenced (State);
      Msg : M.Message := M.NULL_MESSAGE; Report : Receiver.Report;
   begin
      Check (Destination = 1 and Deadline = 99); Wire_Calls := Wire_Calls + 1;
      Reply_Seen := False; Msg.tag := (label => Item.Label, length => Item.Length,
         protocol => 0, ipc => <>, reserved => Item.Reserved); Msg.words := M.MessageWords (Item.Data);
      M.Enqueue ((if Mode = "wrong-owner" then M.From_Word (16#1234_1234_0000_0001#) else Sender), Msg);
      for Step in 1 .. 128 loop
         Receiver.Turn (Report); exit when Reply_Seen or Report.Reason /= Receiver.Serving;
      end loop;
      if Reply_Seen and Delivery_Allowed then
         Reply := (Label => Last_Reply.tag.label, Length => Last_Reply.tag.length,
            Flags => M.Flag_Byte (Last_Reply.tag), Reserved => Last_Reply.tag.reserved, Data => Wire.Words (Last_Reply.words));
         Delivered := Wire.Reply_Received;
      else Reply := (others => <>); Delivered := Wire.Completion_Unknown; end if;
   end Exchange;
   procedure Window (Time, Deadline : out Wire.Time_Instant) is
   begin Time := 10; Deadline := 99; end Window;
   package Provider is new ACPI_Field_Remote_Provider (Transport, Natural, 0, Exchange, Window);
   package C is new ACPI_Service_Core (AML_Delays.Unavailable_Provider,
      Namespace_Node_Capacity => 64, Aggregate_Method_Capacity => 4096, Retained_Result_Capacity => 4);
   package Fields is new C.Field_IO (Provider.Context, Provider.Pending, Provider.Describe,
      Provider.Dispatch, Provider.Reconciled, Authorize_Target => Provider.Authorize);
   Service : aliased C.State (1,4096,4096, Load_Capacity => 1); IO : Provider.Context; Reads, Writes : Fields.Binding_Set;
   procedure Evaluate (State : aliased in out C.State; Item : C.Node_Descriptor; Budget : Natural;
      Result : out C.Values.Result; Status : out C.Values.Access_Status) is
   begin
      Check (not In_Publication); Evaluations := Evaluations + 1;
      Fields.Evaluate_Named (State, IO, Reads, Item, Budget, Result, Status);
   end Evaluate;
   package Lid is new ACPI_Lid (C, Evaluate);
   package Methods is new ACPI_EC_Methods (C, Evaluate);
   package Events is new ACPI_EC_Event_Receiver (C, Methods, 2);
   use type Events.Enrollment, Events.Outcome, ET.Notice, ET.Token_ID;
   use type Owner.Event_State;
   use type Owner.Query_Scan_Phase, Owner.Query_Request_Count, Owner.Query_Response;
   Backend_PID : constant M.Process_ID := M.From_Word (16#FEED_1234_0000_0001#);
   use type C.Stage_Status, C.Freeze_Status, C.Values.Module_Outcome, C.Values.Access_Status;
   use type AML_Execute.Execution_Status, Lid.Query_Outcome, Lid.Lid_State;
   function Find (Parent : C.Namespace.Node_ID; Name : AML_Names.Segment) return C.Node_Descriptor is
      Node : C.Namespace.Node_ID; Item : C.Node_Descriptor; Status : C.Values.Access_Status;
   begin C.Find_Child (Service, Parent, Name, Node, Status); Check (Status = C.Values.Available);
      C.Describe_Node (Service, Node, Item, Status); Check (Status = C.Values.Available); return Item; end Find;
   EC_Device, Lid_Device : C.Node_Descriptor;
   Setup : Owner.Setup_Result; Accepted : Boolean; Status : C.Values.Access_Status;
   Field : C.Values.Field_Description; Key : C.Values.Field_Key;
   Targets : AML_Field_Targets.Vector := [others => A.No_Target]; Sample : Lid.Query_Report;
   Result : AML_Execute.Execution_Result; Before : Natural;
   procedure Run_Burst is
      Enrollment : Events.Enrollment;
      Deliveries : array (Positive range 1 .. 2) of Events.Delivery;
      Duplicate : Events.Delivery;
      Outcome : Events.Outcome;
      Turn : Receiver.Report;
      GPE_Result : G.Action_Result;
      Notification : C.Notification_Observation;
      Present, Claimed : Boolean;
      Value : C.Values.Result;
      Before_Evaluations, Before_Wire : Natural;
      Changed : ET.Notice;
      procedure Drain_Scan is
      begin
         for Step in 1 .. 256 loop
            Receiver.Turn (Turn); exit when Owner.Observe.Queued_Queries = 0 or Turn.Reason /= Receiver.Serving;
         end loop;
         Check (Turn.Reason = Receiver.Serving and Owner.Observe.Queued_Queries = 0
           and Owner.Observe.Scan.Last_Empty_Response = Owner.No_Pending_Event);
         Check (Publications = 2 and Burst_Notices (1).Token < Burst_Notices (2).Token);
      end Drain_Scan;
      procedure Still_Masked is
      begin
         GPE.Finish (GPE_Run, GPE_Result);
         Check (GPE_Result = G.Wrong_Phase and Masked and Enables = 0
           and GPE.Observe (GPE_Run).Stage = G.Executing);
      end Still_Masked;
   begin
      C.Configure_Notifications (Service, True, Status); Check (Status = C.Values.Available);
      Events.Install (Service, 42, 1, Backend_PID, EC_Device, Enrollment); Check (Enrollment = Events.Installed);
      GPE.Start (GPE_Run, Source, (if Mode = "gpe-burst-edge" then G.Edge_Triggered else G.Level_Triggered), GPE_Result);
      Check (GPE_Result = G.Accepted and Masked and Masks = 1);
      GPE.Begin_Deferred (GPE_Run, 2000, GPE_Result); Check (GPE_Result = G.Accepted);
      Cell_Value := 16#A5#;
      declare Item : M.Message := M.NULL_MESSAGE; begin
         Item.tag.label := 16#101#; Query_Ready := True; M.Enqueue_Event (Sender, Item);
      end;
      Before_Evaluations := Evaluations; Before_Wire := Wire_Calls;
      for Step in 1 .. 128 loop
         Receiver.Turn (Turn); exit when Publications > 0 or Turn.Reason /= Receiver.Serving;
      end loop;
      Check (Turn.Reason = Receiver.Serving and Publications = 1 and Published.Present);
      if Mode = "gpe-burst-unknown-publication" then
         Check (Owner.Observe.Event_Phase = Owner.Event_Delivery_Unknown);
         for Step in 1 .. 4 loop Receiver.Turn (Turn); end loop;
         Check (Publications = 1 and Owner.Observe.Queued_Queries = 1);
      else Drain_Scan; end if;
      Check (Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      Still_Masked;
      for I in Deliveries'Range loop
         if I = 2 then
            Check (Publications = 2 and Owner.Observe.Queued_Queries = 0);
            Check (C.Pending_Notifications (Service) = 0 and not Upper_Settled);
            Still_Masked;
         end if;
         Before_Evaluations := Evaluations;
         Fail_Status := Mode = "gpe-burst-uncertain-second" and I = 2;
         Events.Dispatch (Deliveries (I), Service, Backend_PID, Burst_Notices (I), 1000, Outcome);
         Check (Events.Consumed (Deliveries (I)) and Events.Receipt (Deliveries (I)).Present
           and Events.Receipt (Deliveries (I)).Item = Burst_Notices (I));
         if Fail_Status then
            Check (Outcome = Events.Field_Uncertain and Provider.Pending (IO)
              and Owner.Observe.Hardware_Recovery and C.Pending_Notifications (Service) = 0);
         else
            Check (Outcome = Events.Completed and Evaluations = Before_Evaluations + 1
              and not Events.Pending (Deliveries (I)) and C.Pending_Notifications (Service) = 1);
            Still_Masked;
            C.Take_Notification (Service, Notification, Present, Status);
            Check (Status = C.Values.Available and Present and Notification.Current_Target
              and Notification.Number = 16#80# and Notification.Path_Length = 1 and Notification.Path (1) = "LID0");
            Lid.Query (Service, Lid_Device, 1000, Sample);
            Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Open and Masked);
            C.Take_Notification (Service, Notification, Present, Status);
            Check (Status = C.Values.Available and not Present);
         end if;
         Before_Evaluations := Evaluations; Before_Wire := Wire_Calls;
         Events.Dispatch (Deliveries (I), Service, Backend_PID, Burst_Notices (I), 1000, Outcome);
         Check (Outcome = Events.Already_Consumed and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
         if Mode = "gpe-burst-unknown-publication" and I = 1 then
            Check (Owner.Observe.Notice.Present and Publications = 1 and Masked);
            Changed := Burst_Notices (1); Changed.Token := Changed.Token + 1;
            Owner.Confirm_Claim (Sender, Changed, Claimed); Check (not Claimed);
            Owner.Confirm_Claim (Sender, Events.Receipt (Deliveries (1)).Item, Claimed); Check (Claimed);
            Drain_Scan;
            Check (Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
         end if;
      end loop;
      C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVCT")), Value, Status);
      Check (Status = C.Values.Available and Value.Status = AML_Execute.Returned
        and Value.Number = (if Fail_Status then 1 else 2));
      C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVOR")), Value, Status);
      Check (Status = C.Values.Available and Value.Status = AML_Execute.Returned
        and Value.Number = (if Fail_Status then 1 else 16#12#));
      Still_Masked;
      if Fail_Status then
         GPE.Complete_Deferred (GPE_Run, G.Handler_Completion_Unknown,
           Events.Observe (Deliveries (1)).Invocation.Charged + Events.Observe (Deliveries (2)).Invocation.Charged, GPE_Result);
      else
         Check (not Provider.Pending (IO) and not Owner.Observe.Hardware_Recovery
           and Owner.Observe.Queued_Queries = 0 and C.Pending_Notifications (Service) = 0);
         Upper_Settled := True;
         GPE.Complete_Deferred (GPE_Run, G.Handler_Succeeded,
           Events.Observe (Deliveries (1)).Invocation.Charged + Events.Observe (Deliveries (2)).Invocation.Charged, GPE_Result);
      end if;
      Check (GPE_Result = G.Accepted and Masked and Enables = 0);
      GPE.Finish (GPE_Run, GPE_Result);
      if Fail_Status then
         Check (GPE_Result = G.Wrong_Phase and Masked and Enables = 0 and GPE.Observe (GPE_Run).Stage = G.Failed);
      else Check (GPE_Result = G.Accepted and not Masked and Enables = 1 and Clears = 1); end if;
      Before_Evaluations := Evaluations; Before_Wire := Wire_Calls;
      Events.Dispatch (Duplicate, Service, Backend_PID, Burst_Notices (2), 1000, Outcome);
      Check (Outcome = Events.Duplicate
        and not Events.Consumed (Duplicate) and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      if Fail_Status then
         Changed := Burst_Notices (2); Changed.Token := Changed.Token + 1;
         Events.Dispatch (Duplicate, Service, Backend_PID, Changed, 1000, Outcome);
         Check (Outcome = Events.Field_Uncertain and not Events.Consumed (Duplicate)
           and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      end if;
   end Run_Burst;
   procedure Run_Event is
      Enrollment : Events.Enrollment;
      First, Repeated : Events.Delivery;
      Outcome : Events.Outcome;
      Notice, Changed : ET.Notice;
      Turn : Receiver.Report;
      Before_Evaluations, Before_Wire : Natural;
      Value : C.Values.Result;
      Notification : C.Notification_Observation;
      Present : Boolean;
      GPE_Result : G.Action_Result;
   begin
      C.Configure_Notifications (Service, True, Status); Check (Status = C.Values.Available);
      Events.Install (Service, 42, 1, Backend_PID, EC_Device, Enrollment);
      Check (Enrollment = Events.Installed);
      if GPE_Mode then
         GPE.Start (GPE_Run, Source, (if Mode = "gpe-edge" then G.Edge_Triggered else G.Level_Triggered), GPE_Result);
         Check (GPE_Result = G.Accepted and Masked and Masks = 1 and Enables = 0);
         GPE.Begin_Deferred (GPE_Run, 1000, GPE_Result);
         Check (GPE_Result = G.Accepted and GPE.Observe (GPE_Run).Deferred);
      end if;
      Cell_Value := 16#A5#;
      declare Item : M.Message := M.NULL_MESSAGE; begin
         Item.tag.label := 16#101#; Query_Ready := True; M.Enqueue_Event (Sender, Item);
      end;
      Before_Evaluations := Evaluations; Before_Wire := Wire_Calls;
      for Step in 1 .. 128 loop
         Receiver.Turn (Turn); exit when Published.Present or Turn.Reason /= Receiver.Serving;
      end loop;
      Check (Published.Present and Publications = 1 and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      Notice := Published.Item;
      if Scan_Mode then
         for Step in 1 .. 128 loop
            Receiver.Turn (Turn); exit when Owner.Observe.Queued_Queries = 0;
         end loop;
         Check (Owner.Observe.Queued_Queries = 0 and Owner.Observe.Scan.Phase = Owner.No_Head_Scan
            and Owner.Observe.Scan.Last_Empty_Response = Owner.No_Pending_Event);
         Check (Publications = 1 and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
         if GPE_Mode then
            GPE.Finish (GPE_Run, GPE_Result);
            Check (GPE_Result = G.Wrong_Phase and Masked and Enables = 0);
            Check (GPE.Observe (GPE_Run).Stage = G.Executing and not Upper_Settled);
         end if;
      end if;
      if Mode = "event-unknown-publication" then
         Check (Owner.Observe.Event_Phase = Owner.Event_Delivery_Unknown and Owner.Observe.Notice.Present);
         for Step in 1 .. 4 loop Receiver.Turn (Turn); end loop;
         Check (Publications = 1 and Evaluations = Before_Evaluations);
      else Check (Owner.Observe.Event_Phase = Owner.Event_Ready and not Owner.Observe.Notice.Present);
      end if;
      Events.Dispatch (First, Service, M.From_Word (16#1234_1234_0000_0001#), Notice, 1000, Outcome);
      Check (Outcome = Events.Wrong_Sender and not Events.Consumed (First) and Evaluations = Before_Evaluations);
      Changed := Notice; Changed.Epoch := 2;
      Events.Dispatch (First, Service, Backend_PID, Changed, 1000, Outcome);
      Check (Outcome = Events.Wrong_Epoch and not Events.Consumed (First) and Wire_Calls = Before_Wire);
      Fail_Status := Mode in "event-uncertain-field" | "gpe-uncertain-field";
      Events.Dispatch (First, Service, Backend_PID, Notice, (if Mode in "event-budget" | "gpe-budget" then 0 else 1000), Outcome);
      Check (Events.Consumed (First) and Events.Receipt (First).Present and Events.Receipt (First).Item = Notice);
      if Mode in "event-uncertain-field" | "gpe-uncertain-field" then
         Check (Outcome = Events.Field_Uncertain and Provider.Pending (IO) and Fields.Uncertain (Service).Present);
         Check (Owner.Observe.Hardware_Recovery and C.Pending_Notifications (Service) = 0);
      elsif Mode in "event-budget" | "gpe-budget" then
         Check (Outcome = Events.Invocation_Failed and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
         Check (not Provider.Pending (IO) and C.Pending_Notifications (Service) = 0);
      else
         Check (Outcome = Events.Completed and not Events.Pending (First) and Evaluations = Before_Evaluations + 1);
         if GPE_Mode then
            Check (C.Pending_Notifications (Service) = 1 and Masked);
            GPE.Finish (GPE_Run, GPE_Result);
            Check (GPE_Result = G.Wrong_Phase and Enables = 0);
         end if;
         C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVCT")), Value, Status);
         Check (Status = C.Values.Available and Value.Status = AML_Execute.Returned and Value.Number = 1);
         C.Observe_Named_Value (Service, C.Values.Position (Find (C.Namespace.Root, "EVST")), Value, Status);
         Check (Status = C.Values.Available and Value.Status = AML_Execute.Returned and Value.Number = 1);
         C.Take_Notification (Service, Notification, Present, Status);
         Check (Status = C.Values.Available and Present and Notification.Current_Target and Notification.Number = 16#80#
           and Notification.Path_Length = 1 and Notification.Path (1) = "LID0");
         Lid.Query (Service, Lid_Device, 1000, Sample);
         Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Open);
         C.Take_Notification (Service, Notification, Present, Status); Check (Status = C.Values.Available and not Present);
      end if;
      if GPE_Mode then
         if Mode = "gpe-uncertain-field" then
            GPE.Complete_Deferred (GPE_Run, G.Handler_Completion_Unknown, Events.Observe (First).Invocation.Charged, GPE_Result);
         elsif Mode = "gpe-budget" then
            GPE.Complete_Deferred (GPE_Run, G.Handler_Budget_Exhausted, 0, GPE_Result);
         else
            Check (Owner.Observe.Queued_Queries = 0 and not Owner.Observe.Hardware_Recovery
              and not Provider.Pending (IO) and not Events.Pending (First) and C.Pending_Notifications (Service) = 0);
            Upper_Settled := True;
            GPE.Complete_Deferred (GPE_Run, G.Handler_Succeeded, Events.Observe (First).Invocation.Charged, GPE_Result);
         end if;
         Check (GPE_Result = G.Accepted and Masked and Enables = 0);
         GPE.Finish (GPE_Run, GPE_Result);
         if Mode in "gpe-uncertain-field" | "gpe-budget" then
            Check (GPE_Result = G.Wrong_Phase and GPE.Observe (GPE_Run).Stage = G.Failed
              and Masked and Enables = 0 and GPE.Observe (GPE_Run).Source = G.Known_Masked);
         else
            Check (GPE_Result = G.Accepted and GPE.Observe (GPE_Run).Stage = G.Finished
              and not Masked and Enables = 1 and Clears = 1);
         end if;
      end if;
      Before_Evaluations := Evaluations; Before_Wire := Wire_Calls;
      Events.Dispatch (First, Service, Backend_PID, Notice, 1000, Outcome);
      Check (Outcome = Events.Already_Consumed and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      Events.Dispatch (Repeated, Service, Backend_PID, Notice, 1000, Outcome);
      Check (Outcome = Events.Duplicate and not Events.Consumed (Repeated) and Evaluations = Before_Evaluations and Wire_Calls = Before_Wire);
      if Mode = "event-unknown-publication" then
         Owner.Confirm_Claim (M.From_Word (16#1234_1234_0000_0001#), Notice, Accepted); Check (not Accepted);
         Changed := Notice; Changed.Token := Changed.Token + 1;
         Owner.Confirm_Claim (Sender, Changed, Accepted); Check (not Accepted);
         Owner.Confirm_Claim (Sender, Events.Receipt (First).Item, Accepted);
         Check (Accepted and Owner.Observe.Event_Phase = Owner.Event_Ready and not Owner.Observe.Notice.Present);
         Check (Evaluations = Before_Evaluations and Wire_Calls = Before_Wire and Publications = 1);
      end if;
   end Run_Event;
begin
   M.Reset;
   declare
      File : Ada.Streams.Stream_IO.File_Type; Data : Ada.Streams.Stream_Element_Array (1 .. 4096);
      Last : Ada.Streams.Stream_Element_Offset; Staged : C.Stage_Status; Frozen : C.Freeze_Status;
      Report : C.Execution_Report; Members : C.Namespace.Initialization_Report;
   begin
      Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
      Ada.Streams.Stream_IO.Read (File, Data, Last); Ada.Streams.Stream_IO.Close (File);
      declare Image : Firmware_Tables.Bytes (1 .. Natural (Last)); begin
         for I in Image'Range loop Image (I) := Firmware_Tables.Byte (Data (Ada.Streams.Stream_Element_Offset (I))); end loop;
         C.Stage_Table (Service, 1, C.DSDT, Image, Staged); Check (Staged = C.Staged);
      end;
      C.Freeze_Catalog (Service, Frozen); Check (Frozen = C.Catalog_Frozen_OK);
      C.Execute_Frozen (Service, 1000, Report, Status);
      Check (Status = C.Values.Available and Report.Modules.Outcome = C.Values.Modules_Completed);
      C.Initialize_Members (Service, Members, Status); Check (Status = C.Values.Available);
   end;
   Owner.Configure (Owner.Cell_List'[1 => (Register => Owner.Register_ID (Register_ID),
      Epoch => Owner.Epoch_ID (Epoch), Owner => Sender, Cell => 16#10#, Readable => True,
      Writable => True, Exact_Write_Safe => True, Write_Mask => 16#FF#)], 42, 1, Setup);
   Check (Setup = Owner.Configured);
   EC_Device := Find (C.Namespace.Root, "EC00"); Lid_Device := Find (C.Namespace.Root, "LID0");
   Provider.Initialize (IO, 1, 7, Accepted); Check (Accepted);
   Provider.Install (IO, (Register => Register_ID, Epoch => Epoch,
      Geometry => (Region => (Space => AML_Field_Layout.Embedded_Control_Space, Base => 0, Length => 256),
         Byte_Offset => 16#10#, Width => T.Byte_Access),
      Readable => True, Writable => True, Exact_Write_Safe => True, Write_Mask => 16#FF#), Accepted); Check (Accepted);
   Targets (1) := (Provider.Identity (IO), 7, Epoch, Register_ID);
   C.Read_Field_Descriptor (Service, C.Values.Position (Find (C.Values.Position (EC_Device), "LIDS")), Field, Key, Status);
   Check (Status = C.Values.Available);
   Fields.Add_Read_Binding (Service, IO, Key, Targets, 1, Reads, Status); Check (Status = C.Values.Available);
   Fields.Add_Write_Binding (Service, IO, Key, Targets, 1, Writes, Status); Check (Status = C.Values.Available);
   if Mode = "queued-query" then
      declare Item : M.Message := M.NULL_MESSAGE; begin Item.tag.label := 16#101#; Query_Ready := True; M.Enqueue_Event (Sender, Item); end;
   elsif Mode = "stop" then
      declare Item : M.Message := M.NULL_MESSAGE; begin Item.tag.label := 16#100#; M.Enqueue_Event (M.No_Process, Item); end;
   end if;
   Delivery_Allowed := Mode /= "lost-reply"; Fail_Status := Mode = "hardware-unknown";
   if Burst_Mode then Run_Burst;
   elsif Event_Mode then Run_Event;
   else
   Lid.Query (Service, Lid_Device, 1000, Sample);
   if Mode in "normal" | "queued-query" then
      Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Closed);
      Fields.Invoke_Scalar (Service, IO, Writes, C.Values.Position (Find (C.Values.Position (EC_Device), "STOR")),
         [0 => 1, others => 0], 1, 1000, Result);
      Check (Result.Status = AML_Execute.Returned and Cell_Value = 16#A5#);
      Lid.Query (Service, Lid_Device, 1000, Sample); Check (Sample.Outcome = Lid.Sampled and Sample.State = Lid.Open);
      Check (Currents = 0 and Saves = Answers and not Live and Publications = (if Mode = "queued-query" then 1 else 0));
      Before := IO_Count; Fields.Invoke_Scalar (Service, IO, Reads,
         C.Values.Position (Find (C.Values.Position (EC_Device), "STOR")), [others => 0], 1, 1000, Result);
      Check (Result.Status = AML_Execute.Unsupported and IO_Count = Before);
   elsif Mode = "wrong-owner" then Check (Sample.Outcome = Lid.Evaluation_Failed and IO_Count = 0 and not Provider.Pending (IO));
   else
      Check (Sample.Execution = AML_Execute.Field_Indeterminate and Provider.Pending (IO)
         and Fields.Uncertain (Service).Present);
      Before := Wire_Calls; Lid.Query (Service, Lid_Device, 1000, Sample);
      Check (Sample.Execution = AML_Execute.Field_Indeterminate and Wire_Calls = Before);
      if Mode = "hardware-unknown" then Check (Owner.Observe.Hardware_Recovery);
      elsif Mode = "lost-reply" then Check (not Owner.Observe.Hardware_Recovery and not Live);
      else Check (IO_Count = 0 and Saves = 0); end if;
   end if;
   end if;
   Check (C.Valid (Service) and C.Retained_Results (Service) = 0 and M.Test.Mixed_Polls = 0);
   Ada.Text_IO.Put_Line ("Owner AML checks" & Checks'Image);
end Owner_AML_Tests;
