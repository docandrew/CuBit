with Interfaces;with System;
package CuBit.Messages is
 subtype Word is Interfaces.Unsigned_64;
 type Process_ID is private;
 function Test_Word(Item:Process_ID) return Word;
 Test_PID_Word:constant Word:=16#FEDC_BA98_7654_3210#;
 type Protocol_Flags is mod 2**4;
 type IPC_Flags is mod 2**4;
 type MessageTag is record
 label:Interfaces.Unsigned_32:=0;length:Interfaces.Unsigned_8:=0;
 protocol:Protocol_Flags:=0;ipc:IPC_Flags:=0;reserved:Interfaces.Unsigned_16:=0;
 end record;
 function Flag_Byte(Tag:MessageTag) return Interfaces.Unsigned_8;
 type MessageWords is array(0..3) of Word;
 type Message is record tag:MessageTag;words:MessageWords:=[others=>0];authorityTag:Word:=0;end record;
 NULL_MESSAGE:constant Message:=(others=><>);
 type CompletionEntry is record marker:Word:=0;end record;
 Wait_Forever:constant Word:=Word'Last;
 type Wait_Result is (Available,Unavailable);
 Stop_Label:constant Interfaces.Unsigned_32:=16#FF01#;
 Press_Label:constant Interfaces.Unsigned_32:=16#FF02#;
 Observer_Label:constant Interfaces.Unsigned_32:=16#FF03#;
 type Scenario is (Default_Case,Pending_Watch,Press_Wakeup);
 procedure Reset(Kind:Scenario);
 Current_Reply_Authority:Boolean:=False;
 Replies,Waits,Turn:Natural:=0;
 Last_Replied_PID:Word:=0;
 function Poll_Completion(Into:System.Address) return Word;
 function Poll_Event(Item:out Message;Publisher:out Process_ID) return Boolean;
 procedure Poll_Service_Request(From:out Process_ID;Item:out Message;Found:out Boolean);
 function reply(To_Process:Process_ID;Item:Message) return Word;
 function Wait_For_Activity_Until(Deadline:Word) return Wait_Result;
private
 type Process_ID is record Value:Word:=0;end record;
end CuBit.Messages;
