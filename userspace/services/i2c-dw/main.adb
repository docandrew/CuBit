pragma Ada_2022;
with CuBit.Waits;
with Interfaces;
with CuBit.Messages; with CuBit.Process_IDs; with CuBit.Driver_Readiness;
with CuBit.Launch_Configurations; with CuBit.Control_Events;
with CuBit.Channel_Bells; with CuBit.Deadlines;
with Launch_Configuration; with Startup;
with DW_I2C_Transfer; with Intel_LPSS_I2C_Profile;
with I2C_Channel_Controller; with I2C_Native_Channel_Service;
with I2C_Native_Clock; with I2C_Native_Registers; with I2C_Mapping_Policy;
with I2C_Work_Policy; with I2C_Log;
with CuBit.Device_Interrupts; with I2C_Interrupt_Binding;
with CuBit.Monotonic;
procedure Main is
   package M renames CuBit.Messages;
   package PID renames CuBit.Process_IDs;
   package LC renames Launch_Configuration;
   package E renames CuBit.Control_Events;
   package T renames DW_I2C_Transfer;
   package P renames Intel_LPSS_I2C_Profile;
   package Interrupts renames CuBit.Device_Interrupts;
   package IRQ is new I2C_Interrupt_Binding (Interrupts.EVENT_IRQ);
   package Registers renames I2C_Native_Registers.Window;
   package Device is new I2C_Channel_Controller
     (I2C_Native_Clock.Read, Registers.Reset_Read, Registers.Reset_Write,
      Registers.Config_Read, Registers.Config_Write, Registers.Data_Read, Registers.Data_Write);
   package C renames Device.Controller;
   package Service is new I2C_Native_Channel_Service (Device);
   package Channels renames Service.Channels;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_16, Interfaces.Unsigned_8;
   use type M.Protocol_Flags, M.IPC_Flags, M.MessageTag, PID.Process_ID;
   use type C.Phase, C.Admission, Registers.Outcome, E.Event_Kind;
   use type Channels.Wake_Kind, IRQ.Mode, C.Interrupt_Result, M.Wake_Result, T.Tick;
   Register_Virtual : constant I2C_Mapping_Policy.Address := 16#6000_0000_0000#;
   Register_Bytes : constant LC.Byte_Count := 4096;
   I2C_Role : constant Interfaces.Unsigned_64 := 32;
   Unknown_Request_Label : constant Interfaces.Unsigned_32 := 16#F001#;
   Registration_Refused : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   Reset_Steps : constant T.Limit := 4096;
   Configuration_Steps : constant T.Limit := 4096;
   Startup_Steps : constant := 16_384;
   Poll_Milliseconds : constant Interfaces.Unsigned_64 := I2C_Work_Policy.Poll_Milliseconds;
   Health_Milliseconds : constant Interfaces.Unsigned_64 := 5000;
   Interrupt_Prepare_Milliseconds : constant T.Tick := 1000;
   Events_Per_Turn : constant := 8;
   Config : LC.Generic_PCI_Configuration;
   Configured, Ready, Reported : Boolean;
   Mapping : Registers.Report;
   IRQ_Block : Interrupts.Interrupt_Block;
   Binding : IRQ.Selection;
   IRQ_Result : C.Interrupt_Result;
   IRQ_Notices, IRQ_Ignored : Interfaces.Unsigned_64 := 0;
   IRQ_Wait_Failure_Noted : Boolean := False;
   Admission : C.Admission;
   Ignored : Interfaces.Unsigned_64;
   Next_Health : Interfaces.Unsigned_64 := 0;
   Clock_Warning, Retained_Logged : Boolean := False;
   Events_Seen, Requests_Seen : Interfaces.Unsigned_64 := 0;
   Woke : Channels.Wake := (Kind => Channels.Work_Waiting, others => <>);
   procedure Count (Value : in out Interfaces.Unsigned_64) is
   begin if Value < Interfaces.Unsigned_64'Last then Value := Value + 1; end if; end Count;
   procedure Report_Startup (Available : Boolean; Delivered : out Boolean) is
      Good : Boolean := Available;
      Report : M.Message := M.NULL_MESSAGE;
      Tag : M.MessageTag;
      Registration : Interfaces.Unsigned_64;
   begin
      if Good and then Startup.Registration.Present then
         Registration := M.registerDriver (I2C_Role, Startup.Registration.Value);
         Good := Registration /= Registration_Refused and then Registration = PID.To_Word (M.Own_Process);
         I2C_Log.Stage ("registration", Good, (if Good then "primary" else "refused"));
      end if;
      Report.tag.label := (if Good then CuBit.Driver_Readiness.Ready_Label
                           else CuBit.Driver_Readiness.Not_Present_Label);
      -- Generic_Report is down-layer, replies immediately and cannot call us back.
      Tag := M.capSend (Startup.Ready, Report, M.Wait_Forever);
      Delivered := Good = Available and then Tag = M.NULL_TAG;
      I2C_Log.Stage ("readiness-report", Delivered, (if Delivered then "acknowledged" else "refused"));
      I2C_Log.Summary (Good and then Delivered);
   end Report_Startup;
   procedure Wait_Retained (Reason : String) is
   begin
      I2C_Log.Stage ("retained", False, Reason);
      I2C_Log.Disable ("retained-supervision");
      -- Startup touched a controller whose quiescence is uncertain. Keep its
      -- MMIO mapping and process for supervision; no reset/retry or exit here.
      loop Ignored := CuBit.Waits.Sleep_For (Health_Milliseconds); end loop;
   end Wait_Retained;
   procedure Event_Message (Publisher : PID.Process_ID; Message : M.Message) is
      Item : E.Event;
      Noted : Boolean;
   begin
      if Publisher /= PID.No_Process or else Message.tag.protocol /= 0
        or else Message.tag.ipc /= M.No_IPC_Flags or else Message.tag.reserved /= 0
        or else Message.authorityTag /= 0
      then return; end if;
      Count (Events_Seen);
      if Message.tag.label = Interrupts.EVENT_IRQ then
         if IRQ.Notice (Binding, Publisher, Message) then Count (IRQ_Notices);
         else Count (IRQ_Ignored); end if;
         return;
      end if;
      CuBit.Channel_Bells.Note_Notice (Message.tag.label, Message.tag.length,
        [Message.words (0), Message.words (1), Message.words (2), Message.words (3)], Noted);
      if Noted then return; end if;
      Item := E.Decode (Message.tag.label, Message.tag.length,
                        Message.words (0), Message.words (1), Message.words (2));
      Service.On_Event (Publisher, Item);
      if Item.Kind in E.Grant_Revoked | E.Grant_Returned then
         -- Preserve the existing logger's conservative grant-lifetime handling.
         -- Client channel mapping retirement is separately mediated by Service.
         I2C_Log.Disable ("grant-lifetime-event");
      end if;
   end Event_Message;
   procedure Poll_Events is
      Message : M.Message;
      Publisher : PID.Process_ID;
   begin
      for N in 1 .. Events_Per_Turn loop
         exit when not M.Poll_Event (Message, Publisher);
         Event_Message (Publisher, Message);
      end loop;
   end Poll_Events;
   procedure Handle_Request (Caller : PID.Process_ID; Message : M.Message) is
      Is_Open, Opened : Boolean;
      Reply : M.Message;
   begin
      Count (Requests_Seen);
      Service.Handle_Open (Caller, Message, Is_Open, Reply, Opened);
      if not Is_Open then
         Reply := M.NULL_MESSAGE;
         Reply.tag.label := Unknown_Request_Label;
         Reply.tag.length := 1;
         Reply.words (0) := Interfaces.Unsigned_64 (Message.tag.label);
      end if;
      Ignored := M.reply (Caller, Reply);
   end Handle_Request;
begin
   CuBit.Launch_Configurations.Get (Config, Configured);
   if not Configured or else Config.Window_Total /= 1
     or else Config.Windows (1).Bytes < Register_Bytes or else not P.Supported (P.PCI_ID (Config.Vendor), P.PCI_ID (Config.Device))
   then
      I2C_Log.Stage ("identify", False, "launch-config");
      Report_Startup (False, Reported); I2C_Log.Disable ("launch-refused"); return;
   end if;
   if Startup.Controller.Present then
      IRQ_Block := Interrupts.Block_Of (Startup.Controller.Value);
   end if;
   Binding := IRQ.Select_Mode (Config.Delivery, Config.Vectors, Startup.Controller.Present,
      IRQ_Block.Valid, IRQ_Block.Head, IRQ_Block.Count);
   if Binding.Delivery = IRQ.Invalid then
      I2C_Log.Stage ("interrupt", False, "launch-binding-refused");
      Report_Startup (False, Reported); I2C_Log.Disable ("interrupt-binding"); return;
   end if;
   I2C_Log.Stage ("identify", True, (if Binding.Delivery = IRQ.MSI then "msi-channel" else "polled-channel"),
      I2C_Log.Hex ("vendor", Config.Vendor) & I2C_Log.Hex ("device", Config.Device) &
      I2C_Log.Hex ("rev", Config.Revision) & I2C_Log.Hex ("location", Config.Location) &
      I2C_Log.Hex ("bar-bytes", Config.Windows (1).Bytes) &
      I2C_Log.Hex ("irq-vector", (if Binding.Delivery = IRQ.MSI then Interfaces.Unsigned_64 (Binding.Head) else 0)));
   Registers.Bind (Startup.Registers, I2C_Mapping_Policy.Address (Config.Windows (1).First),
      I2C_Mapping_Policy.Address (Config.Windows (1).Bytes), Register_Virtual, Mapping);
   if Mapping.Status /= Registers.Bound or else not Registers.Ready then
      I2C_Log.Stage ("mapping", False, Registers.Outcome'Image (Mapping.Status));
      Report_Startup (False, Reported); I2C_Log.Disable ("mapping-refused"); return;
   end if;
   Device.Start ((P.PCI_ID (Config.Vendor), P.PCI_ID (Config.Device),
      Config.Windows (1).First, Config.Windows (1).Bytes,
      Reset_Steps, Configuration_Steps, Startup_Steps), Admission);
   if Admission /= C.Accepted then
      Report_Startup (False, Reported); Wait_Retained (C.Admission'Image (Admission)); return;
   end if;
   for N in 1 .. Startup_Steps + 1 loop
      exit when Device.Observe.At_Phase in C.Available | C.Faulted;
      declare Before : constant C.Phase := Device.Observe.At_Phase; begin
         Device.Advance;
         if Before /= Device.Observe.At_Phase and then Device.Observe.At_Phase /= C.Faulted then
            I2C_Log.Stage (C.Phase'Image (Before), True, "complete");
         end if;
      end;
   end loop;
   if Device.Observe.At_Phase /= C.Available then
      I2C_Log.Stage ("startup", False, C.Failure'Image (Device.Observe.Reason));
      Report_Startup (False, Reported); Wait_Retained ("startup-not-quiescent"); return;
   end if;
   if Binding.Delivery = IRQ.MSI then
      declare Now : T.Tick; Available : Boolean; begin
         I2C_Native_Clock.Read (Now, Available);
         if not Available or else Now >= T.Tick'Last - Interrupt_Prepare_Milliseconds then
            Report_Startup (False, Reported); Wait_Retained ("interrupt-clock"); return;
         end if;
         Device.Prepare_Interrupt_Mode (Now + Interrupt_Prepare_Milliseconds, IRQ_Result);
         if IRQ_Result /= C.Quiet then
            I2C_Log.Stage ("interrupt", False, "source-mask-unconfirmed");
            Report_Startup (False, Reported); Wait_Retained ("interrupt-prepare"); return;
         end if;
         I2C_Log.Stage ("interrupt", True, "msi-masked-ready");
      end;
   else
      I2C_Log.Stage ("interrupt", True, "polling-fallback",
         " interval-ms=" & Poll_Milliseconds'Image);
   end if;
   Service.Configure (Startup.Channels.Com_Cubit_Os_I2C_Transfer_Per_Client,
      Startup.Channels.Com_Cubit_Os_I2C_Transfer_Total, Startup.Channels.Com_Cubit_Os_I2C_Transfer_Class, Ready);
   if not Ready then
      I2C_Log.Stage ("channels", False, "configuration-refused");
      Report_Startup (False, Reported); I2C_Log.Disable ("channel-table-refused"); return;
   end if;
   Report_Startup (True, Reported);
   if not Reported then I2C_Log.Disable ("readiness-refused"); return; end if;
   loop
      case Woke.Kind is
         when Channels.Message_Received =>
            if Woke.From = PID.No_Process then Event_Message (Woke.From, Woke.Msg);
            else Handle_Request (Woke.From, Woke.Msg);
            end if;
         when others => null;
      end case;
      Woke := (Kind => Channels.Work_Waiting, others => <>);
      declare
         Caller : PID.Process_ID;
         Message : M.Message;
         Found : Boolean;
      begin
         M.Poll_Service_Request (Caller, Message, Found);
         if Found then Handle_Request (Caller, Message); end if;
      end;
      Poll_Events;
      Service.Turn;
      if Service.Stopped then
         I2C_Log.Stage ("stop", True, "quiescent"); I2C_Log.Disable ("stop"); return;
      end if;
      if Service.Retained and then not Retained_Logged then
         I2C_Log.Stage ("runtime", False, "retained " & C.Failure'Image (Device.Observe.Reason));
         Retained_Logged := True;
      end if;
      declare
         Now : constant Interfaces.Unsigned_64 := CuBit.Monotonic.Milliseconds;
         Until_Time : CuBit.Deadlines.Deadline := CuBit.Deadlines.Forever;
      begin
         if Now = Interfaces.Unsigned_64'Last then
            if not Clock_Warning then I2C_Log.Note ("health clock=unavailable"); Clock_Warning := True; end if;
         else
            if Now >= Next_Health then
               I2C_Log.Note ("health phase=" & C.Phase'Image (Device.Observe.At_Phase) &
                  " requests=" & Requests_Seen'Image & " events=" & Events_Seen'Image &
                  " irq=" & IRQ_Notices'Image & " ignored-irq=" & IRQ_Ignored'Image);
               Next_Health := (if Now < Interfaces.Unsigned_64'Last - Health_Milliseconds
                               then Now + Health_Milliseconds else Interfaces.Unsigned_64'Last - 1);
            end if;
            Until_Time := CuBit.Deadlines.At_Monotonic
              ((if Device.Has_Payload and then not Service.Retained then
                   (if Now < Interfaces.Unsigned_64'Last - Poll_Milliseconds
                    then Now + Poll_Milliseconds else Interfaces.Unsigned_64'Last - 1)
                else Next_Health));
         end if;
         if Service.Retained then
            -- Ready rings must not spin a retained supervisor wait.
            Ignored := CuBit.Waits.Sleep_For (Health_Milliseconds);
         elsif Device.Has_Payload and then Binding.Delivery = IRQ.MSI then
            if Device.Observe.Waiting_For_Hardware then
               Device.Arm_Interrupt_Wait (IRQ_Result);
               if IRQ_Result in C.Armed | C.Already_Armed then
                  declare Event : aliased M.Message; Result : M.Wake_Result; begin
                     -- Pending channel ring entries must not turn a physical
                     -- hardware wait into a ready-ring busy loop. The kernel
                     -- wait still receives lifecycle/Stop/IRQ notices and IPC.
                     M.Wait_For_Activity_Or_Event
                       (Interfaces.Unsigned_64 (Device.Waiting_Deadline), Event, Result);
                     if Result = M.Event_Taken then Event_Message (PID.No_Process, Event);
                     elsif Result = M.Unavailable then
                        Device.Reject_Interrupt_Wait;
                        if not IRQ_Wait_Failure_Noted then
                           I2C_Log.Stage ("interrupt", False, "event-wait-unavailable");
                           IRQ_Wait_Failure_Noted := True;
                        end if;
                     end if;
                  end;
               else
                  Device.Reject_Interrupt_Wait;
               end if;
            end if;
            -- Software configuration/FIFO progress continues next bounded
            -- turn without the polling sleep. Only actual No_Action waits.
         elsif Device.Has_Payload then
            -- Work_Burst/Budget assumes a bounded polling interval. Other
            -- channels may already be ready; Arm must not bypass this yield.
            Ignored := CuBit.Waits.Sleep_For (Poll_Milliseconds);
         else
            Service.Sleep (Until_Time, Woke);
         end if;
      end;
   end loop;
end Main;
