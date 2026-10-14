with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;

-- The driver's channel to its supervisor, devmgr (CAP-002). Its endpoint is
-- the startup capability "device-manager"; every request to devmgr is
-- asynchronous (submit + completion), never a call: devmgr calls down into
-- the driver, so a blocking call back up could deadlock (docs IPC DAG).
-- A runtime grant (register, GGTT, reset, display, PHY, pipe pages; a log
-- publisher endpoint) is requested with Submit_Accepting and arrives in the
-- completion: devmgr answers REPLY_OK carrying a narrowed copy of its
-- device-memory root (or an endpoint), read with Granted_*.
-- Driver-only: this unit names the driver's own startup table.
package Intel_GPU_Supervisor is
   subtype Message is CuBit.Messages.Message;
   subtype MessageTag is CuBit.Messages.MessageTag;
   subtype Completion is CuBit.Messages.CompletionEntry;

   function Endpoint return CuBit.Handles.Endpoint;

   function Submit (Msg : Message; Token : Unsigned_64) return Boolean;
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean;

   -- The reply's tag as the server wrote it: without the kernel's
   -- capability-transfer flags.
   function Reply_Tag (Receipt : Completion) return MessageTag;

   -- What a grant reply carries; the null handle when it carries nothing or
   -- something of another kind (that handle is closed again).
   function Granted_Region (Receipt : Completion) return CuBit.Handles.Device_Memory;
   function Granted_Endpoint (Receipt : Completion) return CuBit.Handles.Endpoint;
end Intel_GPU_Supervisor;
