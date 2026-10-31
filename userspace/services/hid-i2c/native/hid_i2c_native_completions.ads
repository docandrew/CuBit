pragma Ada_2022;
with CuBit.Messages;
with HID_GPIO_Reset;
with HID_I2C_Device;
with HID_GPIO_Input;
generic
   with package Reset is new HID_GPIO_Reset.Bridge (<>);
   with package Device is new HID_I2C_Device
     (Next_Token => Reset.Next_Token, Begin_Arm_Reset => Reset.Begin_Arm_Reset, Poll_Arm_Reset => Reset.Poll_Arm_Reset,
      Begin_Release_Reset => Reset.Begin_Release_Reset, Poll_Release_Reset => Reset.Poll_Release_Reset,
      others => <>);
   with package Input is new HID_GPIO_Input.Owner
     (Controls => Reset.Controls, Device => Device, Expected_Sender => Reset.Expected_Sender,
      Next_Token => Reset.Next_Token, others => <>);
package HID_I2C_Native_Completions is
   package W renames Device.W;
   type Destination is (Invalid_Envelope, GPIO_Reset_Control, GPIO_Input_Control, I2C_Transfer);
   type Report is record
      Routed_To : Destination := Invalid_Envelope;
      Frame_Valid : Boolean := False;
      GPIO_Result : Reset.Controls.Receive_Result := Reset.Controls.Unrelated;
      I2C_Result : Device.T.Completion_Result := Device.T.Unrelated;
   end record;
   procedure Receive (S : in out Device.State; Input_State : in out Input.State; Entry_Value : CuBit.Messages.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Report);
   -- The owning native loop polls the completion queue ONCE and routes here.
   -- GPIO and I2C must use one process-global fresh token allocator. No polling
   -- another token's queue or learning a sender from received data. Bridge and
   -- device retain separately resolved opaque full64-bit endpoint lifetimes.
   -- Only live serving/reset tokens select their states; other entries go through
   -- device token/sender matching. Invalid frames become empty malformed input
   -- for the matched owner, preserving possible effects rather than dropping
   -- an owned reply. No automatic replay, controller stepping or waits.
end HID_I2C_Native_Completions;
