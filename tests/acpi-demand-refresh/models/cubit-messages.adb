package body CuBit.Messages is
 use type Interfaces.Unsigned_8;
 function To_Word(P:Process_ID) return Word is (P.Value);
 function Test_Process(W:Word) return Process_ID is ((Value=>W));
 function Flag_Byte(T:MessageTag) return Interfaces.Unsigned_8 is
 (Interfaces.Unsigned_8(T.protocol) or Interfaces.Shift_Left(Interfaces.Unsigned_8(T.ipc),4));
end CuBit.Messages;
