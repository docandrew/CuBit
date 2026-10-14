with Config_Object_Receiver;
with Config_Object_Replies;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Memory_Grants;

--  The native instance: its deferred Set or definition reply is a reply
--  handle (CuBit.Handles.IPC.Save_Reply); this instance has one owning thread.
package Config_Object_Native is new Config_Object_Receiver
  (Reply_Handle => CuBit.Handles.Reply,
   Acquire => CuBit.Memory_Grants.Acquire,
   Return_Acquisition => CuBit.Memory_Grants.Return_Acquisition,
   Save_Reply => Config_Object_Replies.Save,
   Send_Saved_Reply => CuBit.Messages.replyCap,
   Send_Current_Reply => CuBit.Messages.reply);
