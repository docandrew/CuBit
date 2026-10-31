pragma Ada_2022;
with I2C_Transfer_Wire;
with GPIO_Lease_Control;
with HID_I2C_Descriptors;
with HID_I2C_Buffered_Client;
with HID_I2C_Device;
with GPIO_Client_Controls;
with GPIO_Control_Wire;
with HID_GPIO_Input;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
with HID_Touchpad_Contact;
with HID_Report_Metadata;
with HID_Input_Source;
with Input_Pending;
with CuBit.Process_IDs;
generic
   with package Bus is new HID_I2C_Buffered_Client (<>);
   Maximum_Report, Maximum_Input, Output_Capacity : HID_I2C_Descriptors.Byte_Count;
   Maximum_Contacts, Maximum_Slots, Maximum_Global_Depth, Maximum_Collection_Depth : Positive;
   Maximum_Fields, Maximum_Usages, Maximum_Collections : Positive;
   with procedure Next_Token (Token : out I2C_Transfer_Wire.Word; Available : out Boolean);
   with procedure Begin_Arm_Reset (Deadline : I2C_Transfer_Wire.Millisecond; Reply : out GPIO_Lease_Control.Reply);
   with procedure Poll_Arm_Reset (Reply : out GPIO_Lease_Control.Reply);
   with procedure Begin_Release_Reset (Ticket : I2C_Transfer_Wire.Word;
      Deadline : I2C_Transfer_Wire.Millisecond; Reply : out GPIO_Lease_Control.Reply);
   with procedure Poll_Release_Reset (Reply : out GPIO_Lease_Control.Reply);
package HID_I2C_Touchpad with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   package L renames GPIO_Lease_Control;
   package D renames HID_I2C_Descriptors;
   package Schema is new HID_Report_Schema (Maximum_Global_Depth, Maximum_Collection_Depth);
   package Names is new HID_Field_Usages (Schema);
   package Bindings is new HID_Input_Bindings (Schema, Names);
   package Values is new HID_Input_Values (Schema);
   package Contacts is new HID_Touchpad_Contact (Schema, Names, Bindings, Values);
   package Metadata is new HID_Report_Metadata (Schema, Names, Bindings, Values);
   package Source is new HID_Input_Source (Maximum_Contacts, Maximum_Slots, Schema, Names, Bindings, Values, Contacts, Metadata);
   type Configuration is record
      Contact_Limit : Source.Reports.Frames.Contact_Limit;
      Binding_Budget : Bindings.Work_Count;
      X_Range, Y_Range : Source.Pointer.Axis_Range;
      X_Gain, Y_Gain : Source.Pointer.Gain;
      X_Direction, Y_Direction : Source.Pointer.Direction;
      Confidence : Source.Pointer.Confidence_Policy;
      Buttons : Source.Button_Map;
      Coordinates : Source.Coordinate_Policy;
   end record;
   type Admission_Status is (Unconfigured, Parser_Refused, Touchpad_Missing,
      Touchpad_Ambiguous, Root_Budget_Exhausted, Source_Refused, Source_Configured);
   type Admission_Report is record
      Outcome : Admission_Status := Unconfigured;
      Parser : Schema.Status := Schema.Empty_Descriptor;
      At_Byte, Fields_Required, Usages_Required, Collections_Required : Schema.Count := 0;
      Touchpad : Schema.Count := 0;
      Root_Charge, Binding_Charge : Bindings.Work_Count := 0;
      Binding : Source.Configure_Report;
   end record;
   procedure Compile_Report (Descriptor : D.Descriptor; Data : D.Bytes; Accepted : out Boolean);
   procedure Deliver_Input (Data : D.Bytes);
   procedure Input_Lost;
   -- Internal callback adapters for this one process-owned device instance.
   package Device is new HID_I2C_Device (Bus, Maximum_Report, Maximum_Input, Output_Capacity,
      Next_Token, Begin_Arm_Reset, Poll_Arm_Reset, Begin_Release_Reset, Poll_Release_Reset, Compile_Report, Deliver_Input, Input_Lost);
   type Start_Result is (Started, Busy, Invalid_Calibration, Invalid_Budget, Device_Not_Ready);
   procedure Start (Sender : CuBit.Process_IDs.Process_ID; Descriptor_Register : D.Register_Number;
      Settings : Configuration; Deadline : W.Millisecond; Now : W.Word;
      Clock_Available : Boolean; Result : out Start_Result);
   function Observe return Device.Report;
   function Admission return Admission_Report;
   function Input_Observation return Source.Report;
   function Ready return Boolean;
   function Quiescent return Boolean;
   function Count return Input_Pending.Count_Type;
   function Head return Input_Pending.Item with Pre => Count > 0;
   procedure Acknowledge with Pre => Count > 0;
   procedure Consumer_Changed (Now : W.Word);
   function Wake_Deadline (Now, Otherwise : W.Word) return W.Word;
   procedure Receive (Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
      Delivery : Bus.Transactions.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out Bus.Transactions.Completion_Result);
   procedure Step (Now : W.Word; Clock_Available : Boolean);
   procedure Reset_Interrupt (Ticket : W.Word; Result : out Device.Interrupt_Result);
   procedure Request_Input (Deadline : W.Millisecond; Now : W.Word; Clock_Available : Boolean;
      Result : out Device.Request_Result);
   procedure Request_Stop (Now : W.Word);
   procedure Close (Result : out Bus.Close_Result);
   generic
      with package Controls is new GPIO_Client_Controls (<>);
      Expected_Sender : CuBit.Process_IDs.Process_ID;
      Control_Window, Read_Window, Cleanup_Window : HID_GPIO_Input.Window;
   package GPIO_Input is
      package Owner is new HID_GPIO_Input.Owner
        (Controls, Device, Expected_Sender, Control_Window, Read_Window, Cleanup_Window, Next_Token);
      procedure Start (Now : W.Word; Clock_Available : Boolean; Result : out Owner.Start_Result);
      procedure Step (Now : W.Word; Clock_Available : Boolean);
      procedure Receive_Control (Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
         Delivery : Controls.Delivery_Status; Label : W.Operation; Protocol : W.Word;
         Words : GPIO_Control_Wire.Message_Words; Now : W.Word; Clock_Available : Boolean;
         Result : out Controls.Receive_Result);
      procedure Receive_Event (Value : GPIO_Control_Wire.Event; Result : out Owner.Event_Result);
      function Observe return Owner.Report;
      function Control_Token return W.Word;
      function Wake_Deadline (Now, Otherwise : W.Word) return W.Word;
      -- Hardware/control progress only; excludes queued pointer publication.
      function Ready return Boolean;
      function Quiescent return Boolean;
      procedure Request_Stop (Now : W.Word; Clock_Available : Boolean);
      -- Owns serving GPIO state alongside the SAME private Core and parser.
      -- Caller steps the parent first, then this owner with the same clock.
      -- Native adapters authenticate event metadata before Receive_Event.
      -- Quiescent aggregates GPIO and I2C; parser/pointer queue remains exposed
      -- only through Head/Acknowledge for accepted publication.
   end GPIO_Input;
   -- Serialized single owner, one hardware device per generic instance. No IPC
   -- or GPIO admission is synthesized here. Calibration/button mapping and all
   -- capacities are explicit. Native owner publishes Head nonblocking and only
   -- acknowledges acceptance; its queue can flush releases after device stop.
end HID_I2C_Touchpad;
