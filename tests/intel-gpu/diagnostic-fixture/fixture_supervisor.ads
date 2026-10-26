with Interfaces; use Interfaces;
with CuBit.Messages;
with CuBit.Handles;
--  Hosted stand-in for Intel_GPU_Supervisor: counts grant requests; a
--  completion "carries" an endpoint when its status word says so.
package Intel_GPU_Supervisor is
   subtype Message is CuBit.Messages.Message;
   subtype MessageTag is CuBit.Messages.MessageTag;
   subtype Completion is CuBit.Messages.CompletionEntry;
   Submissions : Natural := 0;
   Accept_Submissions : Boolean := True;
   Last_Label : Unsigned_64 := 0;
   Carries_Endpoint : Boolean := False;
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean;
   function Reply_Tag (Receipt : Completion) return MessageTag is (Receipt.msg.tag);
   function Granted_Endpoint (Receipt : Completion) return CuBit.Handles.Endpoint is
     (if Carries_Endpoint then (Id => 1) else CuBit.Handles.No_Endpoint);
end Intel_GPU_Supervisor;
