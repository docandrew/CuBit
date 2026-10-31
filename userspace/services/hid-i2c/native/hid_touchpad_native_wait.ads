pragma Ada_2022;
with CuBit.Messages;
with HID_Touchpad_Pump;
generic
   with package Pump is new HID_Touchpad_Pump (<>);
package HID_Touchpad_Native_Wait is
   use type Pump.W.Word;
   subtype Backoff_Milliseconds is Pump.W.Word range 1 .. Pump.W.Word'Last - 1;
   procedure Await (Plan : Pump.Report; Fallback : Backoff_Milliseconds;
      Result : out CuBit.Messages.Activity_Result);
   -- Fallback is explicit launch policy, used for clock failure or unavailable
   -- activity waits. It bounds retry frequency; it is never an operation limit.
end HID_Touchpad_Native_Wait;
