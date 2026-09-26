with Config_Object_Receiver;
with CuBit.Messages;
with CuBit.Memory_Grants;

--  Slot 62 is reserved exclusively for this instance's deferred Set reply.
--  Service startup must leave it empty; this instance has one owning thread.
package Config_Object_Native is new Config_Object_Receiver
  (Saved_Reply_Slot => 62,
   Acquire => CuBit.Memory_Grants.Acquire,
   Return_Acquisition => CuBit.Memory_Grants.Return_Acquisition,
   Save_Reply => CuBit.Messages.saveReplyCap,
   Send_Reply => CuBit.Messages.replyCap);
