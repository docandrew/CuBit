-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; package body CuBit.Messages is
 function Own_Process return Process_ID is (Process_ID(Sender_Model.Own_ID));
end CuBit.Messages;