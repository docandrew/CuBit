with Interfaces; use Interfaces;
with CuBit.Messages;
--  Hosted stand-in for the driver's supervisor channel: submissions go to
--  the fixture's devmgr model (CuBit.Messages.capSubmit) and replies carry
--  no capability.
package Intel_GPU_Supervisor is
   subtype Message is CuBit.Messages.Message;
   subtype MessageTag is CuBit.Messages.Tag_Type;
   subtype Completion is CuBit.Messages.CompletionEntry;
   --  The devmgr endpoint the production driver submits to.
   Devmgr : constant := 15;
   function Submit (Msg : Message; Token : Unsigned_64) return Boolean is
     (CuBit.Messages.capSubmit (Devmgr, Msg, Token));
   function Submit_Accepting (Msg : Message; Token : Unsigned_64) return Boolean
     renames Submit;
   function Reply_Tag (Receipt : Completion) return MessageTag is (Receipt.msg.tag);
end Intel_GPU_Supervisor;
