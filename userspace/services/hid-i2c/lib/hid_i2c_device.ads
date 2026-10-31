pragma Ada_2022;
with I2C_Transfer_Wire;
with GPIO_Lease_Control;
with HID_I2C_Descriptors;
with HID_I2C_Wire;
with HID_I2C_Buffered_Client;
with CuBit.Process_IDs;
generic
   with package Bus is new HID_I2C_Buffered_Client (<>);
   Maximum_Report, Maximum_Input, Output_Capacity : HID_I2C_Descriptors.Byte_Count;
   with procedure Next_Token (Token : out I2C_Transfer_Wire.Word; Available : out Boolean);
   -- Trusted admitted GPIO latch adapter: bounded, nonblocking and nonreentrant.
   -- Arm succeeds only once an interrupt generation is really armed. A pending
   -- asynchronous GPIO command must complete in its owner before returning Armed.
   -- A returned ticket, even on refusal, still requires bounded local release.
   with procedure Begin_Arm_Reset (Deadline : I2C_Transfer_Wire.Millisecond; Reply : out GPIO_Lease_Control.Reply);
   with procedure Poll_Arm_Reset (Reply : out GPIO_Lease_Control.Reply);
   with procedure Begin_Release_Reset (Ticket : I2C_Transfer_Wire.Word;
      Deadline : I2C_Transfer_Wire.Millisecond; Reply : out GPIO_Lease_Control.Reply);
   with procedure Poll_Release_Reset (Reply : out GPIO_Lease_Control.Reply);
   with procedure Compile_Report (Descriptor : HID_I2C_Descriptors.Descriptor;
      Data : HID_I2C_Descriptors.Bytes; Accepted : out Boolean);
   with procedure Deliver_Input (Data : HID_I2C_Descriptors.Bytes);
   with procedure Input_Lost;
package HID_I2C_Device with SPARK_Mode is
   package W renames I2C_Transfer_Wire;
   package L renames GPIO_Lease_Control;
   package D renames HID_I2C_Descriptors;
   package Wire renames HID_I2C_Wire;
   package T renames Bus.Transactions;
   use type T.Phase, L.Operation;
   type Phase is (Cold, Fetch_HID, Power_On, Arming_Reset, Resetting, Await_Reset,
      Read_Reset, Releasing_Reset, Fetch_Report, Serving, Read_Input, Draining, Stopped, Faulted);
   type Failure is (None, Requested_Stop, Buffer_Setup_Failed, Clock_Unavailable,
      Clock_Reversed, Deadline_Expired, Token_Unavailable, Submit_Refused,
      Transfer_Failed, Read_Refused, Descriptor_Refused, Capacity_Refused,
      Reset_Arm_Failed, Reset_Not_Acknowledged, Reset_Release_Failed, Reset_Control_Unresolved,
      Report_Layout_Refused, Unexpected_Device_Reset);
   type Stage_Count is range 0 .. 5;
   type Report is record
      At_Phase : Phase := Cold;
      Reason : Failure := None;
      Completed : Stage_Count := 0;
      Descriptor : D.Decode_Result;
      Last_Start : Bus.Start_Result := Bus.Request_Rejected;
      Last_Transfer : T.Report;
      Last_Frame : Wire.Frame_Result;
      Reset_Ticket : W.Word := 0;
      Reset_Still_Armed, Latch_Uncertain, Memory_Uncertain : Boolean := False;
      Pending_Control : L.Operation := L.No_Control;
      Last_Control : L.Reply;
      Input_Packets : W.Word := 0;
   end record;
   type Request_Result is (Accepted, Not_Ready, Busy);
   type Interrupt_Result is (Latched, Unrelated);
   type State is limited private;
   function Observe (S : State) return Report;
   function Ready (S : State) return Boolean;
   function Quiescent (S : State) return Boolean;
   function Wake_Deadline (S : State; Now, Otherwise : W.Word) return W.Word;
   procedure Start (S : in out State; Sender : CuBit.Process_IDs.Process_ID;
      Descriptor_Register : D.Register_Number; Deadline : W.Millisecond;
      Now : W.Word; Clock_Available : Boolean; Result : out Request_Result);
   procedure Receive (S : in out State; Token : W.Word; Sender : CuBit.Process_IDs.Process_ID;
      Delivery : T.Delivery_Status; Label : W.Operation; Protocol : W.Word;
      Words : W.Message_Words; Now : W.Word; Clock_Available : Boolean;
      Result : out T.Completion_Result);
   procedure Step (S : in out State; Now : W.Word; Clock_Available : Boolean);
   procedure Reset_Interrupt (S : in out State; Ticket : W.Word; Result : out Interrupt_Result);
   procedure Request_Input (S : in out State; Deadline : W.Millisecond;
      Now : W.Word; Clock_Available : Boolean; Result : out Request_Result);
   procedure Request_Stop (S : in out State);
   procedure Close (S : in out State; Result : out Bus.Close_Result);
   -- One serialized event-loop owner. I2C submission/completion is asynchronous;
   -- no waits, polling loops, automatic command replay or implicit reset retry.
   -- Main routes authentic completions/IRQ tickets and schedules Step. GPIO
   -- arming/release adapters must satisfy their stated admission/lifetime contract;
   -- this unit does not implement the remote GPIO control protocol.
private
   Scratch_Length : constant Positive := Positive (Integer'Max (D.Descriptor_Bytes,
      Integer'Max (Integer (Maximum_Report), Integer (Maximum_Input))));
   type State is limited record
      Client : Bus.State;
      Info : Report;
      Scratch : D.Bytes (1 .. Scratch_Length) := [others => 0];
      Deadline : W.Millisecond := 0;
      Previous : W.Word := 0;
      Have_Clock, Interrupt_Pending, Release_Attempted, Loss_Reported : Boolean := False;
   end record;
   function Observe (S : State) return Report is (S.Info);
   function Ready (S : State) return Boolean is (S.Info.At_Phase = Serving);
   function Quiescent (S : State) return Boolean is
     (S.Info.At_Phase in Stopped | Faulted and then
      Bus.Observe (S.Client).At_Phase in T.Idle | T.Finished and then
      S.Info.Pending_Control = L.No_Control and then
      not S.Info.Reset_Still_Armed and then not S.Info.Latch_Uncertain and then not S.Info.Memory_Uncertain);
end HID_I2C_Device;
