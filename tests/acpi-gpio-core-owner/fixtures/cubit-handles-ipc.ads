-- Hosted test model only; never a native runtime/provider implementation.
with Interfaces; with CuBit.Messages; package CuBit.Handles.IPC is
 type Description(Current : Boolean := False) is record case Current is
 when True => Kind : ABI.Kind := ABI.Endpoint; Held : Rights; Authority_Tag,Object : Interfaces.Unsigned_64 := 0;
 when False => null; end case; end record;
 function Describe(Item : Endpoint) return Description;
 function Call(Target : Endpoint; Msg : in out CuBit.Messages.Message; Deadline : Interfaces.Unsigned_64) return CuBit.Messages.MessageTag;
end CuBit.Handles.IPC;