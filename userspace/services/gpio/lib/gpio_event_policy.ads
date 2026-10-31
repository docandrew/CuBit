pragma Ada_2022;
with Interfaces;
with CuBit.Process_IDs;
package GPIO_Event_Policy with SPARK_Mode, Pure is
   type Action is (Ignore, Route_Channel_Wake, Disable_Logging, Stop_After_Drain);
   function Classify (Publisher : CuBit.Process_IDs.Process_ID;
      Label : Interfaces.Unsigned_32; Length : Interfaces.Unsigned_8;
      W0, W1, W2 : Interfaces.Unsigned_64) return Action;
   -- Publisher is kernel event-queue metadata. Payloads cannot impersonate it.
   -- No IRQ action yet: parent stays disabled pending ONESHOT/epoch integration.
end GPIO_Event_Policy;
