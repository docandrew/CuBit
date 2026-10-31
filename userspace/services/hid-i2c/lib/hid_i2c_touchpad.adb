pragma Ada_2022;
package body HID_I2C_Touchpad with SPARK_Mode is
   use type W.Word, Schema.Word, Schema.Byte, Schema.Count, Schema.Status;
   use type Bindings.Work_Count, Source.Configure_Status, Device.Phase, Device.Request_Result;
   use type Source.Coordinate_Policy;
   Touchpad_Usage : constant Schema.Word := 16#000D_0005#;
   Application_Collection : constant Schema.Byte := 1;
   Core : Device.State;
   Signal : Source.State;
   Active_Settings : Configuration := (Source.Reports.Frames.Contact_Limit'First, 0,
      (0, 0), (0, 0), (1, 1), (1, 1), Source.Pointer.As_Reported,
      Source.Pointer.As_Reported, Source.Pointer.Require_Confident, [others => Source.Ignore], Source.Caller_Bounds);
   Current_Time : W.Word := W.Word'Last;
   Info : Admission_Report;
   Input_Info : Source.Report;
   Layout : Schema.Result;
   Fields : Schema.Field_Table (1 .. Maximum_Fields);
   Usages : Schema.Usage_Table (1 .. Maximum_Usages);
   Collections : Schema.Collection_Table (1 .. Maximum_Collections);
   procedure Compile_Report (Descriptor : D.Descriptor; Data : D.Bytes; Accepted : out Boolean) is
      Root : Schema.Count := 0;
   begin
      Accepted := False; Info := (others => <>);
      if W.Word (Data'Length) /= W.Word (Descriptor.Report_Length) then Info.Outcome := Parser_Refused; return; end if;
      Schema.Parse (Schema.Bytes (Data), Fields, Usages, Collections, Layout);
      Info.Parser := Layout.Outcome; Info.At_Byte := Layout.At_Byte;
      Info.Fields_Required := Layout.Fields_Required; Info.Usages_Required := Layout.Usages_Required;
      Info.Collections_Required := Layout.Collections_Required;
      if Layout.Outcome /= Schema.Decoded then Info.Outcome := Parser_Refused; return; end if;
      for N in 1 .. Natural (Layout.Collections_Required) loop
         if Info.Root_Charge = Active_Settings.Binding_Budget then Info.Outcome := Root_Budget_Exhausted; return; end if;
         Info.Root_Charge := Info.Root_Charge + 1;
         if Collections (N).Parent = 0 and then Collections (N).Kind = Application_Collection and then
            Collections (N).Usage = Touchpad_Usage then
            if Root /= 0 then Info.Outcome := Touchpad_Ambiguous; return; end if;
            Root := Schema.Count (N);
         end if;
      end loop;
      if Root = 0 then Info.Outcome := Touchpad_Missing; return; end if;
      Info.Touchpad := Root;
      Source.Configure (Signal, Layout, Fields, Usages, Collections, Root, Active_Settings.Contact_Limit,
         Active_Settings.Binding_Budget - Info.Root_Charge, Active_Settings.X_Range, Active_Settings.Y_Range,
         Active_Settings.X_Gain, Active_Settings.Y_Gain, Active_Settings.X_Direction, Active_Settings.Y_Direction,
         Active_Settings.Confidence, Active_Settings.Buttons, Current_Time, Info.Binding_Charge, Info.Binding,
         Active_Settings.Coordinates);
      if Info.Binding.Outcome /= Source.Source_Configured then Info.Outcome := Source_Refused; return; end if;
      Info.Outcome := Source_Configured; Accepted := True;
   end Compile_Report;
   procedure Deliver_Input (Data : D.Bytes) is
   begin Source.Feed (Signal, Schema.Bytes (Data), Current_Time, Input_Info); end Deliver_Input;
   procedure Input_Lost is
   begin Source.Reset (Signal, Current_Time, Input_Info); end Input_Lost;
   procedure Start (Sender : CuBit.Process_IDs.Process_ID; Descriptor_Register : D.Register_Number;
      Settings : Configuration; Deadline : W.Millisecond; Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result) is
      Policy : Source.Pointer.State;
      Decision : Source.Pointer.Decision;
      Requested : Device.Request_Result;
   begin
      Result := Busy; if Device.Observe (Core).At_Phase /= Device.Cold then return; end if;
      Result := Invalid_Budget; if Settings.Binding_Budget = 0 then return; end if;
      if Settings.Coordinates = Source.Caller_Bounds then
         Source.Pointer.Configure (Policy, Settings.X_Range, Settings.Y_Range, Settings.X_Gain,
            Settings.Y_Gain, Settings.X_Direction, Settings.Y_Direction, Settings.Confidence, Decision);
         Result := Invalid_Calibration; if not Source.Pointer.Ready (Policy) then return; end if;
      end if;
      Active_Settings := Settings; Current_Time := Now;
      Device.Start (Core, Sender, Descriptor_Register, Deadline, Now, Clock_Available, Requested);
      Result := (if Requested = Device.Accepted then Started else Device_Not_Ready);
   end Start;
   function Observe return Device.Report is (Device.Observe (Core));
   function Admission return Admission_Report is (Info);
   function Input_Observation return Source.Report is (Input_Info);
   function Ready return Boolean is (Device.Ready (Core) and then Source.Ready (Signal));
   function Quiescent return Boolean is (Device.Quiescent (Core));
   function Count return Input_Pending.Count_Type is (Source.Count (Signal));
   function Head return Input_Pending.Item is (Source.Head (Signal));
   procedure Acknowledge is
   begin Source.Acknowledge (Signal); end Acknowledge;
   procedure Consumer_Changed (Now : W.Word) is
   begin Source.Consumer_Changed (Signal, Now, Input_Info); end Consumer_Changed;
   function Wake_Deadline (Now, Otherwise : W.Word) return W.Word is
     (Source.Wake_Deadline (Signal, Now, Device.Wake_Deadline (Core, Now, Otherwise)));
   procedure Receive (Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
      Delivery : Bus.Transactions.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Bus.Transactions.Completion_Result) is
   begin
      Current_Time := Now;
      Device.Receive (Core, Token, Sender, Delivery, Label, Protocol, Words, Now, Clock_Available, Result);
   end Receive;
   procedure Step (Now : W.Word; Clock_Available : Boolean) is
   begin Current_Time := Now; Device.Step (Core, Now, Clock_Available); end Step;
   procedure Reset_Interrupt (Ticket : W.Word; Result : out Device.Interrupt_Result) is
   begin Device.Reset_Interrupt (Core, Ticket, Result); end Reset_Interrupt;
   procedure Request_Input (Deadline : W.Millisecond; Now : W.Word; Clock_Available : Boolean;
      Result : out Device.Request_Result) is
   begin Current_Time := Now; Device.Request_Input (Core, Deadline, Now, Clock_Available, Result); end Request_Input;
   procedure Request_Stop (Now : W.Word) is
   begin Current_Time := Now; Device.Request_Stop (Core); end Request_Stop;
   procedure Close (Result : out Bus.Close_Result) is
   begin Device.Close (Core, Result); end Close;
   package body GPIO_Input is
      use type Owner.Phase;
      State : Owner.State;
      procedure Start (Now : W.Word; Clock_Available : Boolean; Result : out Owner.Start_Result) is
      begin Current_Time := Now; Owner.Start (State, Core, Now, Clock_Available, Result); end Start;
      procedure Step (Now : W.Word; Clock_Available : Boolean) is
      begin Current_Time := Now; Owner.Step (State, Core, Now, Clock_Available); end Step;
      procedure Receive_Control (Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : W.Operation; Protocol : W.Word;
         Words : GPIO_Control_Wire.Message_Words; Now : W.Word; Clock_Available : Boolean;
         Result : out Controls.Receive_Result) is
      begin
         Current_Time := Now;
         Owner.Receive_Control (State, Token, Sender, Delivery, Label, Protocol, Words, Now, Clock_Available, Result);
      end Receive_Control;
      procedure Receive_Event (Value : GPIO_Control_Wire.Event; Result : out Owner.Event_Result) is
      begin Owner.Receive_Event (State, Value, Result); end Receive_Event;
      function Observe return Owner.Report is (Owner.Observe (State));
      function Control_Token return W.Word is (Owner.Control_Token (State));
      function Wake_Deadline (Now, Otherwise : W.Word) return W.Word is
        (Owner.Wake_Deadline (State, Now, Device.Wake_Deadline (Core, Now, Otherwise)));
      function Ready return Boolean is
        (Observe.At_Phase in Owner.Armed | Owner.Reading | Owner.Acknowledging and then not Observe.Stopping and then
           Source.Ready (Signal) and then Device.Observe (Core).At_Phase in Device.Serving | Device.Read_Input);
      function Quiescent return Boolean is (Owner.Quiescent (State) and then Device.Quiescent (Core));
      procedure Request_Stop (Now : W.Word; Clock_Available : Boolean) is
      begin Current_Time := Now; Owner.Request_Stop (State, Core, Now, Clock_Available); end Request_Stop;
   end GPIO_Input;
end HID_I2C_Touchpad;
