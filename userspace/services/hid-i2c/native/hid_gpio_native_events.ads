pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
with HID_GPIO_Input;
generic
   with package Input is new HID_GPIO_Input.Owner (<>);
   Expected_Badge : GPIO_Control_Wire.Word;
package HID_GPIO_Native_Events is
   package Device renames Input.Device;
   type Result is (Unrelated, Invalid_Binding, Wrong_Publisher, Wrong_Badge,
      Invalid_Frame, Reset_Latched, Input_Buffered, Input_Duplicate);
   procedure Receive (D : in out Device.State; S : in out Input.State;
      Publisher : CuBit.Process_IDs.Process_ID; Message : CuBit.Messages.Message; Outcome : out Result);
   -- One event decoder, one admitted GPIO binding, two lifecycle owners.
   -- Before serving starts, only Device's exact reset lease may latch. Once
   -- Input owns the serving lifecycle, events go only to that owner. Late
   -- events cannot restart reset or input during stop. No Step/ACK/wait here.
end HID_GPIO_Native_Events;
