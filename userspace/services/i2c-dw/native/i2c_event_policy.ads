pragma Ada_2022;
with Interfaces;
with CuBit.Process_IDs;
package I2C_Event_Policy with SPARK_Mode is
   type Action is (Ignore, Route_Channel_Wake, Disable_Logging, Stop_After_Drain);
   function Classify
     (Publisher : CuBit.Process_IDs.Process_ID;
      Label : Interfaces.Unsigned_32; Length : Interfaces.Unsigned_8;
      W0, W1, W2 : Interfaces.Unsigned_64) return Action;
   -- Publisher comes from the event-receive syscall, never event words.
   -- Only kernel notices may affect lifecycle or channel lifetime. Stop is
   -- latched; Main refuses new transfers while existing bounded work drains.
   -- There are no held client grant acquisitions between Main iterations.
end I2C_Event_Policy;
