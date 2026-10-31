pragma Ada_2022;
with CuBit.Grant_References;
with CuBit.Messages;
with CuBit.Process_IDs;
with GPIO_Native_Client;
with HID_GPIO_Input;
with HID_GPIO_Reset;
with HID_I2C_Descriptors;
with HID_I2C_Native_Buffers;
with HID_I2C_Touchpad;
with HID_I2C_Wire;
with HID_Launch_Adapter;
with HID_Native_Clock;
with HID_Native_Log;
with HID_Native_Queues;
with HID_Native_Readiness;
with HID_Native_System_Events;
with HID_Startup_Readiness;
with HID_Touchpad_Native;
with HID_Touchpad_Native_Publication;
with HID_Touchpad_Native_Wait;
with HID_Touchpad_Pump;
with I2C_Transfer_Wire;
with Parameters;
with Startup;
procedure Main is
   package W renames I2C_Transfer_Wire;
   package D renames HID_I2C_Descriptors;
   package G renames CuBit.Grant_References;
   package P renames CuBit.Process_IDs;
   package Clock renames HID_Native_Clock;
   package Startup_Policy renames HID_Startup_Readiness;
   use type W.Word, D.Byte_Count;
   Raw : constant Parameters.HID_Parameters_Value := Parameters.Value;
   -- Compiled resource ceilings, not claims about the attached hardware.
   Report_Bytes : constant D.Byte_Count := D.Byte_Count (G.Page_Bytes);
   Input_Bytes : constant D.Byte_Count := 1024;
   Output_Bytes : constant D.Byte_Count := 1024;
   Transfer_Bytes : constant W.Capacity := W.Capacity
     (Report_Bytes + HID_I2C_Wire.Register_Selector'Length);
   Contacts : constant := 32;
   Slots : constant := 32;
   Global_Depth : constant := 16;
   Collection_Depth : constant := 16;
   Fields : constant := 1024;
   Usages : constant := 2048;
   Collections : constant := 128;
   Queue_Rounds : constant := 8;
   Output_Burst : constant := 8;
   Last_Token : W.Word := 0;
   procedure Next_Token (Token : out W.Word; Available : out Boolean) is
   begin
      Token := 0; Available := Last_Token < W.Word'Last - 1;
      if Available then Last_Token := Last_Token + 1; Token := Last_Token; end if;
   end Next_Token;
   package GPIO is new GPIO_Native_Client (Startup.Gpio);
   package Memory is new HID_I2C_Native_Buffers (Transfer_Bytes, Startup.I2c);
   GPIO_Binding : constant GPIO.Binding := GPIO.Resolve;
   I2C_Binding : constant Memory.Endpoint_Client.Binding := Memory.Endpoint_Client.Resolve;
   use type GPIO.Binding_Status, Memory.Endpoint_Client.Binding_Status;
   function GPIO_Peer return P.Process_ID is
     (if GPIO_Binding.Status = GPIO.Endpoint_Resolved then GPIO_Binding.Sender else P.No_Process);
   function I2C_Peer return P.Process_ID is
     (if I2C_Binding.Status = Memory.Endpoint_Client.Endpoint_Resolved then I2C_Binding.Sender else P.No_Process);
   function GPIO_Badge return W.Word is
     (if GPIO_Binding.Status = GPIO.Endpoint_Resolved then GPIO_Binding.Badge else 0);
   GPIO_Identity : constant P.Process_ID := GPIO_Peer;
   package Reset is new HID_GPIO_Reset.Bridge
     (GPIO.Controls, GPIO_Identity, HID_GPIO_Reset.Cleanup_Milliseconds (Raw.timing.cleanup), Next_Token, Clock.Cached_Time);
   package App is new HID_I2C_Touchpad
     (Memory.Client, Report_Bytes, Input_Bytes, Output_Bytes, Contacts, Slots,
      Global_Depth, Collection_Depth, Fields, Usages, Collections, Next_Token,
      Reset.Begin_Arm_Reset, Reset.Poll_Arm_Reset, Reset.Begin_Release_Reset, Reset.Poll_Release_Reset);
   package Live is new App.GPIO_Input
     (GPIO.Controls, GPIO_Identity, HID_GPIO_Input.Window (Raw.timing.control),
      HID_GPIO_Input.Window (Raw.timing.read), HID_GPIO_Input.Window (Raw.timing.cleanup));
   package Native is new HID_Touchpad_Native (Reset, App, Live, GPIO_Badge);
   package Publication is new HID_Touchpad_Native_Publication (App, Startup.Mouse_Events, Output_Burst);
   package Log is new HID_Native_Log (Startup.Logstore);
   package Events is new HID_Native_System_Events (Log.Grant_Notice);
   package Pump is new HID_Touchpad_Pump (Native, Publication.Output, Queue_Rounds,
      Clock.Sample_Time, Clock.Cache_Time, HID_Native_Queues.Poll_Completion,
      HID_Native_Queues.Poll_Event, HID_Native_Queues.Drain_Request, Events.Handle);
   package Waiter is new HID_Touchpad_Native_Wait (Pump);
   package Reports is new HID_Native_Readiness (Startup.Ready);
   package Launch is new HID_Launch_Adapter (App);
   use type Launch.Status, App.Start_Result, App.Admission_Status, D.Decode_Status;
   use type App.Source.Reports.Coordinate_Status;
   use type Startup_Policy.Choice, Memory.Client.Close_Result, Pump.Wait_Kind;
   Settings : constant Launch.Result := Launch.Translate (Raw);
   Readiness : Startup_Policy.State;
   Selected : Startup_Policy.Choice;
   Binding_Result : Log.Binding_Result;
   Now : W.Word;
   Available, Can_Start, Ack, Queued : Boolean;
   Startup_Limit : Launch.Deadline_Result;
   Started : App.Start_Result;
   Progress : Pump.Report;
   Activity : CuBit.Messages.Activity_Result;
   Closed : Memory.Client.Close_Result := Memory.Client.Still_Owned;
   Close_Attempted : Boolean := False;
   function Coordinate_Details return String is
      Geometry : constant App.Source.Reports.Coordinate_Report := App.Admission.Binding.Coordinates;
   begin
      if Geometry.Outcome /= App.Source.Reports.Uniform_Coordinates then
         return " coordinates=" & App.Source.Reports.Coordinate_Status'Image (Geometry.Outcome);
      end if;
      return " x-min=" & App.Schema.Numeric_Value'Image (Geometry.Bounds.X_Minimum) &
         " x-max=" & App.Schema.Numeric_Value'Image (Geometry.Bounds.X_Maximum) &
         " y-min=" & App.Schema.Numeric_Value'Image (Geometry.Bounds.Y_Minimum) &
         " y-max=" & App.Schema.Numeric_Value'Image (Geometry.Bounds.Y_Maximum);
   end Coordinate_Details;
   procedure Report_Once (Item : Startup_Policy.Outgoing) is
      Limit : constant Launch.Deadline_Result := Launch.Deadline (Now, Raw.timing.control);
   begin
      Ack := False;
      if Available and then Limit.Available then Reports.Send (Item, Limit.Limit, Ack);
      elsif Item = Startup_Policy.Report_Declined then
         Reports.Decline_Without_Clock (Queued);
         -- Queued is deliberately not promoted to acknowledgement.
      end if;
      Startup_Policy.Delivery (Readiness, Ack);
      Log.Stage (Log.Readiness, Item = Startup_Policy.Report_Ready and then Ack,
         (if Ack then Log.Confirmed else Log.Effects_Uncertain));
      Log.Summary;
   end Report_Once;
begin
   Log.Initialize (Binding_Result);
   Log.Stage (Log.Identify, True, Log.Confirmed,
      " acpi-id=PNP0C50 mode=polled" & Log.Hex ("descriptor-register", W.Word (Raw.descriptor_register)));
   Clock.Sample_Time (Now, Available); Clock.Cache_Time (Now, Available);
   Startup_Limit := Launch.Deadline (Now, Raw.timing.startup);
   Can_Start := GPIO_Binding.Status = GPIO.Endpoint_Resolved and then
      I2C_Binding.Status = Memory.Endpoint_Client.Endpoint_Resolved;
   Log.Stage (Log.Bindings, Can_Start,
      (if Can_Start then Log.Confirmed else Log.Invalid_Authority),
      " i2c=" & Memory.Endpoint_Client.Binding_Status'Image (I2C_Binding.Status) &
      " gpio=" & GPIO.Binding_Status'Image (GPIO_Binding.Status) &
      " logger=" & Log.Binding_Result'Image (Binding_Result) &
      Log.Hex ("gpio-badge", GPIO_Badge));
   Can_Start := Can_Start and then Settings.Outcome = Launch.Accepted and then
      Available and then Startup_Limit.Available;
   if Can_Start then
      Log.Stage (Log.Publication, True, Log.Confirmed);
      App.Start (I2C_Peer, Settings.Descriptor_Register, Settings.Settings,
         Startup_Limit.Limit, Now, Available, Started);
      Can_Start := Started = App.Started;
      Log.Stage (Log.Buffers, Can_Start,
         (if Can_Start then Log.Confirmed else Log.Operation_Refused),
         " start=" & App.Start_Result'Image (Started));
   else
      Log.Stage (Log.Buffers, False,
         (if not Available then Log.Clock_Failure else Log.Invalid_Configuration),
         " configuration=" & Launch.Status'Image (Settings.Outcome));
   end if;
   if not Can_Start then
      Startup_Policy.Decide (Readiness, Now, 0, Available, False, True, Selected);
      Report_Once (Startup_Policy.Outgoing (Selected));
      Live.Request_Stop (Now, Available);
   end if;
   loop
      declare Dispatch_Limit : constant W.Word :=
        (if Startup_Limit.Available then Startup_Policy.Wake_Deadline
           (Readiness, Startup_Limit.Limit, Pump.No_Startup_Deadline)
         else Pump.No_Startup_Deadline);
      begin Pump.Turn (Dispatch_Limit, Progress); end;
      Clock.Cached_Time (Now, Available);
      if App.Observe.Descriptor.Status = D.Decoded then
         Log.Stage (Log.Descriptor, True, Log.Confirmed,
            Log.Hex ("vendor", W.Word (App.Observe.Descriptor.Value.Vendor_ID)) &
            Log.Hex ("product", W.Word (App.Observe.Descriptor.Value.Product_ID)) &
            Log.Hex ("firmware", W.Word (App.Observe.Descriptor.Value.Firmware_Version)) &
            Log.Hex ("report-bytes", W.Word (App.Observe.Descriptor.Value.Report_Length)));
      end if;
      if App.Admission.Outcome = App.Source_Configured then
         Log.Stage (Log.Reset, True, Log.Confirmed);
         Log.Stage (Log.Report_Parser, True, Log.Confirmed, Coordinate_Details);
      elsif App.Admission.Outcome = App.Source_Refused then
         Log.Stage (Log.Report_Parser, False, Log.Unsupported_Report,
            " source=" & App.Source.Configure_Status'Image (App.Admission.Binding.Outcome) &
            " scale=" & App.Source.Pointer.Status'Image (App.Admission.Binding.Policy) & Coordinate_Details);
      end if;
      if Progress.Ready then Log.Stage (Log.Serving_Pin, True, Log.Confirmed); end if;
      if Startup_Policy.Selected (Readiness) = Startup_Policy.No_Report then
         declare Can_Report : constant Boolean := Available and then
            Launch.Deadline (Now, Raw.timing.control).Available;
         begin
            Startup_Policy.Decide (Readiness, Now, Startup_Limit.Limit, Available,
               Progress.Ready, Progress.Stopping or else not Can_Report, Selected);
         end;
         if Selected /= Startup_Policy.No_Report then Report_Once (Startup_Policy.Outgoing (Selected)); end if;
      end if;
      if Startup_Policy.Must_Stop (Readiness) then Live.Request_Stop (Now, Available); end if;
      if Progress.Stopping or else Startup_Policy.Must_Stop (Readiness) then
         Log.Stage (Log.Readiness, False,
            (if Live.Quiescent then Log.Operation_Refused else Log.Cleanup_Pending),
            " phase=" & App.Device.Phase'Image (App.Observe.At_Phase) &
            " reason=" & App.Device.Failure'Image (App.Observe.Reason) &
            " gpio=" & Live.Owner.Failure'Image (Live.Observe.Reason));
         Log.Summary;
         if Live.Quiescent then
            if not Close_Attempted then
               Close_Attempted := True; App.Close (Closed);
            end if;
            if Closed = Memory.Client.Closed and then Native.Drained then
               Log.Disable;
               if Log.Detached then return; end if;
            end if;
            -- Failed/uncertain close is retained, never retried implicitly.
         end if;
      end if;
      -- Main may have submitted cleanup after Pump chose its wait. Recompute
      -- deadlines so a fresh release cannot be hidden by an old idle plan.
      if Available and then Progress.Wait_For /= Pump.Immediate then
         declare Limit : W.Word := Native.Wake_Deadline
           (Now, W.Word'Last, not Progress.Publication.No_Consumer);
            Relative_Retry : Boolean := False;
         begin
            if Startup_Policy.Selected (Readiness) = Startup_Policy.No_Report and then Startup_Limit.Available then
               Limit := Startup_Policy.Wake_Deadline (Readiness, Startup_Limit.Limit, Limit);
            end if;
            if Progress.Publication.No_Consumer and then App.Count > 0 and then
              (Progress.Stopping or else Startup_Policy.Must_Stop (Readiness))
            then
               declare Retry : constant Launch.Deadline_Result := Launch.Deadline (Now, Raw.timing.idle_backoff); begin
                  if Retry.Available then Limit := W.Word'Min (Limit, Retry.Limit);
                  else Relative_Retry := True; end if;
               end;
            end if;
            Progress.Deadline := Limit;
            Progress.Wait_For := (if Limit <= Now then Pump.Immediate
               elsif Limit = W.Word'Last then Pump.Activity else Pump.Until_Time);
            if Relative_Retry and then Progress.Wait_For = Pump.Activity then
               Progress.Wait_For := Pump.Clock_Backoff;
            end if;
         end;
      end if;
      Waiter.Await (Progress, Waiter.Backoff_Milliseconds (Raw.timing.idle_backoff), Activity);
   end loop;
end Main;
