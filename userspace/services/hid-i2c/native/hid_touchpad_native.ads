pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with HID_GPIO_Reset;
with HID_I2C_Touchpad;
generic
   with package Reset is new HID_GPIO_Reset.Bridge (<>);
   with package App is new HID_I2C_Touchpad
     (Next_Token => Reset.Next_Token, Begin_Arm_Reset => Reset.Begin_Arm_Reset,
      Poll_Arm_Reset => Reset.Poll_Arm_Reset, Begin_Release_Reset => Reset.Begin_Release_Reset,
      Poll_Release_Reset => Reset.Poll_Release_Reset, others => <>);
   with package Live is new App.GPIO_Input
     (Controls => Reset.Controls, Expected_Sender => Reset.Expected_Sender, others => <>);
   Expected_Badge : GPIO_Control_Wire.Word;
package HID_Touchpad_Native is
   package W renames App.W;
   package C renames Reset.Controls;
   type Destination is (Invalid_Envelope, GPIO_Reset_Control, GPIO_Input_Control, I2C_Transfer);
   type Completion_Report is record
      Routed_To : Destination := Invalid_Envelope;
      Frame_Valid : Boolean := False;
      GPIO_Result : C.Receive_Result := C.Unrelated;
      I2C_Result : App.Bus.Transactions.Completion_Result := App.Bus.Transactions.Unrelated;
   end record;
   procedure Receive_Completion (Entry_Value : CuBit.Messages.CompletionEntry;
      Now : W.Word; Clock_Available : Boolean; Result : out Completion_Report);
   type Event_Result is (Unrelated, Invalid_Binding, Wrong_Publisher, Wrong_Badge,
      Invalid_Frame, Reset_Latched, Input_Buffered, Input_Duplicate);
   procedure Receive_Event (Publisher : CuBit.Process_IDs.Process_ID;
      Message : CuBit.Messages.Message; Result : out Event_Result);
   procedure Step (Now : W.Word; Clock_Available : Boolean);
   function Drained return Boolean;
   -- Device/GPIO quiescence AND no queued pointer report remains. Buffer
   -- closure and logger detachment remain separate exit gates.
   function Wake_Deadline (Now, Otherwise : W.Word; Include_Output : Boolean)
      return W.Word;
   -- Suppress pointer-queue retries when no consumer exists. GPIO and I2C
   -- deadlines remain active regardless of output-consumer availability.
   -- Main polls each native queue once and supplies its kernel metadata here.
   -- Cached Reset.Clock must already be set to the SAME Now/availability before
   -- Receive/Step. No queue reads, waits, mutable Core export or duplicate parser.
   -- Step advances App once then Live once; publication stays Head/Acknowledge
   -- through App, only on accepted output. Native readiness/teardown use Live's
   -- aggregate readiness/quiescence, not just App's I2C observation.
end HID_Touchpad_Native;
