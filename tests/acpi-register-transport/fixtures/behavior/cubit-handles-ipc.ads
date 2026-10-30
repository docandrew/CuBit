with Interfaces; use Interfaces;
with CuBit.Messages;
package CuBit.Handles.IPC is
   function Call
     (Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Unsigned_64)
      return CuBit.Messages.MessageTag;
end CuBit.Handles.IPC;
