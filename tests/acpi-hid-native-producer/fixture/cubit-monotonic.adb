-- Hosted test model only; never a native runtime/provider implementation.
with Sender_Model; package body CuBit.Monotonic is
 function Milliseconds return Interfaces.Unsigned_64 is
 begin Sender_Model.Clock_Reads:=Sender_Model.Clock_Reads+1; return Sender_Model.Times(Positive'Min(Sender_Model.Clock_Reads,3)); end Milliseconds;
end CuBit.Monotonic;