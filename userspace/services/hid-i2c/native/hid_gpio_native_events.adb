pragma Ada_2022;
with GPIO_Native_Event_Frames;
package body HID_GPIO_Native_Events is
   package Frames renames GPIO_Native_Event_Frames;
   use type Frames.Status, Input.Phase, Device.Interrupt_Result;
   procedure Receive (D : in out Device.State; S : in out Input.State;
      Publisher : CuBit.Process_IDs.Process_ID; Message : CuBit.Messages.Message; Outcome : out Result) is
      Parsed : constant Frames.Decoding := Frames.Decode (Publisher, Input.Expected_Sender, Message, Expected_Badge);
      Latched : Device.Interrupt_Result;
      Buffered : Input.Event_Result;
   begin
      case Parsed.Outcome is
         when Frames.Unrelated => Outcome := Unrelated;
         when Frames.Invalid_Binding => Outcome := Invalid_Binding;
         when Frames.Wrong_Publisher => Outcome := Wrong_Publisher;
         when Frames.Wrong_Badge => Outcome := Wrong_Badge;
         when Frames.Invalid_Frame => Outcome := Invalid_Frame;
         when Frames.Decoded =>
            if Input.Observe (S).At_Phase = Input.Cold then
               Device.Reset_Interrupt (D, Device.W.Word (Parsed.Value.Lease), Latched);
               Outcome := (if Latched = Device.Latched then Reset_Latched else Unrelated);
            else
               Input.Receive_Event (S, Parsed.Value, Buffered);
               Outcome := (case Buffered is when Input.Buffered => Input_Buffered,
                  when Input.Duplicate => Input_Duplicate, when Input.Unrelated => Unrelated);
            end if;
      end case;
   end Receive;
end HID_GPIO_Native_Events;
