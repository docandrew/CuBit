pragma Ada_2022;
with Channel_Wake_ABI;
with CuBit.Control_Events;
package body GPIO_Event_Policy with SPARK_Mode is
   package PID renames CuBit.Process_IDs;
   package CV renames CuBit.Control_Events;
   use type PID.Process_ID, Interfaces.Unsigned_32, CV.Control_Kind;
   function Classify (Publisher : PID.Process_ID;
      Label : Interfaces.Unsigned_32; Length : Interfaces.Unsigned_8;
      W0, W1, W2 : Interfaces.Unsigned_64) return Action is
      Item : CV.Event;
   begin
      if Publisher /= PID.No_Process then return Ignore; end if;
      if Label = Channel_Wake_ABI.Wake_Label and then Length in 1 .. Channel_Wake_ABI.Wakes_Per_Event
      then return Route_Channel_Wake; end if;
      Item := CV.Decode (Label, Length, W0, W1, W2);
      case Item.Kind is
         when CV.Grant_Revoked | CV.Grant_Returned => return Disable_Logging;
         when CV.Control => return (if Item.Control = CV.Stop then Stop_After_Drain else Ignore);
         when CV.Not_Ours => return Ignore;
      end case;
   end Classify;
end GPIO_Event_Policy;
