pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with GPIO_Control_Wire;
package GPIO_Native_Event_Frames is
   package W renames GPIO_Control_Wire;
   type Status is (Unrelated, Invalid_Binding, Wrong_Publisher, Wrong_Badge, Invalid_Frame, Decoded);
   type Decoding (Outcome : Status := Unrelated) is record
      case Outcome is
         when Decoded => Value : W.Event;
         when others => null;
      end case;
   end record;
   function Decode (Publisher, Expected_Sender : CuBit.Process_IDs.Process_ID;
      Message : CuBit.Messages.Message; Expected_Badge : W.Word) return Decoding;
   -- Only real Poll_Event metadata; binding comes from typed endpoint Describe.
   -- Metadata and GPv1 scope are checked before any reset/serving owner sees data.
end GPIO_Native_Event_Frames;
