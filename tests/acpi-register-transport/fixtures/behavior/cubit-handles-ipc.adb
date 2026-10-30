with Transport_Model;
package body CuBit.Handles.IPC is
   function Call
     (Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Unsigned_64)
      return CuBit.Messages.MessageTag is
   begin
      return Transport_Model.Call (Target, Msg, Deadline);
   end Call;
end CuBit.Handles.IPC;
