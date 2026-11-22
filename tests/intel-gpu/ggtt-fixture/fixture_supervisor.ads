with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;
--  Hosted stand-in for Intel_GPU_Supervisor (the driver's channel to
--  devmgr): records the grant request's exact shape, answers through
--  CuBit.Messages' completion fixture, and grants a fixture region.
package Intel_GPU_Supervisor is
   subtype Message is CuBit.Messages.Message;
   subtype MessageTag is CuBit.Messages.MessageTag;
   subtype Completion is CuBit.Messages.CompletionEntry;
   function Endpoint return CuBit.Handles.Endpoint;
   function Submit (Msg : Message; Token : Unsigned_64) return Boolean;
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean;
   function Reply_Tag (Receipt : Completion) return MessageTag;
   function Granted_Region (Receipt : Completion) return CuBit.Handles.Device_Memory;
   function Granted_Endpoint (Receipt : Completion) return CuBit.Handles.Endpoint;
   --  The region every grant reply carries here.
   Region_Slot : constant := 26;
end Intel_GPU_Supervisor;
