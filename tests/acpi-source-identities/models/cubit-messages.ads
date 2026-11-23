with Interfaces;
package CuBit.Messages is
 subtype Word is Interfaces.Unsigned_64;
 type Process_ID is private;
 function To_Word(P:Process_ID) return Word;
 function Test_Process(W:Word) return Process_ID;
 type Protocol_Flags is mod 16;type IPC_Flags is mod 16;
 type MessageTag is record label:Interfaces.Unsigned_32:=0;length:Interfaces.Unsigned_8:=0;protocol:Protocol_Flags:=0;ipc:IPC_Flags:=0;reserved:Interfaces.Unsigned_16:=0;end record;
 function Flag_Byte(T:MessageTag) return Interfaces.Unsigned_8;
 type MessageWords is array(0..3) of Word;
 type Message is record tag:MessageTag;words:MessageWords:=[others=>0];authorityTag:Word:=0;end record;
 NULL_MESSAGE:constant Message:=(others=><>);
 Current_Reply_Authority:Boolean:=False;
private
 type Process_ID is record Value:Word:=0;end record;
end CuBit.Messages;
