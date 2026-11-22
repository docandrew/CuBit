with CuBit.Messages;
package CuBit.Handles.IPC is
 type Refusal is (No_Room);
 type Saved_Reply(Saved:Boolean:=False) is record
 case Saved is when True=>Handle:Reply;when False=>Why:Refusal:=No_Room;end case;end record;
 function Save_Reply return Saved_Reply;
 function Answer(Item:Reply;Msg:CuBit.Messages.Message) return Boolean;
 procedure Reset;
 function Live_Count return Natural;
 Fail_Save,Lose_Answer,In_Callback:Boolean:=False;
 Save_Calls,Answer_Calls:Natural:=0;
 Last_Answer:CuBit.Messages.Message:=CuBit.Messages.NULL_MESSAGE;
end CuBit.Handles.IPC;
