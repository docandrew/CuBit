-- Hosted test model only; never a native runtime/provider implementation.
with Interfaces; package CuBit.Messages is
 subtype U64 is Interfaces.Unsigned_64; subtype U32 is Interfaces.Unsigned_32;
 type Process_ID is new U64;
 function To_Word(P : Process_ID) return U64 is (U64(P));
 function From_Word(W : U64) return Process_ID is (Process_ID(W));
 function Is_Process(P : Process_ID) return Boolean is (P/=0);
 function Own_Process return Process_ID;
 type MessageTag is record label : U32 := 0; length,protocol,ipc : Interfaces.Unsigned_8 := 0; reserved : Interfaces.Unsigned_16 := 0; end record;
 type MessageWords is array(Natural range 0 .. 3) of U64;
 type Message is record tag : MessageTag; words : MessageWords := [others=>0]; end record;
 NULL_MESSAGE : constant Message := (others=><>);
 Kernel_Reply_First : constant U32 := 16#FFFF_0000#;
 function Flag_Byte(T : MessageTag) return Interfaces.Unsigned_8 is (Interfaces."or" (T.protocol,T.ipc));
end CuBit.Messages;