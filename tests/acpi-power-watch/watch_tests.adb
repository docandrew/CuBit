pragma Ada_2022;
with Ada.Text_IO;with ACPI_Power_Inventory_Watches;
with ACPI_Power_Watch_Protocol;with ACPI_Protocol;
with ACPI_Native_Power_Inventory;with CuBit.Messages;with CuBit.Handles;with CuBit.Handles.IPC;
procedure Watch_Tests is
 package P renames ACPI_Power_Watch_Protocol;package IPC renames CuBit.Handles.IPC;package M renames CuBit.Messages;
 use type P.Word,P.Status,M.MessageWords;
 Checks:Natural:=0;
 procedure Check(B:Boolean;Why:String) is
 begin Checks:=Checks+1;if not B then raise Program_Error with Why;end if;end;
 type Token is new Natural;
 Live:array(Token range 1..64)of Boolean:=[others=>False];
 Next_Token:Token:=1;Saves,Answers:Natural:=0;Fail_Save,Lose:Boolean:=False;
 Last_ID,Last_Generation:P.Word:=0;Last_Status:P.Status:=P.Unavailable;
 procedure Save(T:out Token;Saved:out Boolean) is
 begin Saves:=Saves+1;Saved:=not Fail_Save;T:=0;
  if Saved then T:=Next_Token;Next_Token:=Next_Token+1;Check(not Live(T),"unique saved authority");Live(T):=True;end if;end;
 procedure Answer(T:in out Token;ID:P.Word;Status:P.Status;Generation:P.Word;Delivered:out Boolean) is
 begin Check(T/=0 and then Live(T),"one consumption");Live(T):=False;T:=0;Answers:=Answers+1;
 Last_ID:=ID;Last_Status:=Status;Last_Generation:=Generation;Delivered:=not Lose;end;
 package W is new ACPI_Power_Inventory_Watches(Token,0,Maximum_Generation=>4,Save_Current=>Save,Answer_Saved=>Answer);
 use type W.Delivery;
 S:W.State;R:W.Response;Deferred:Boolean;Previous,Progress:W.Delivery;
 Input:W.Request:=(Authority=>P.Word(ACPI_Protocol.Observer_Tag),Sender=>16#FEDC_BA98_7654_3210#,Length=>4,ID=>1,Seen=>1,others=>0);
 procedure Watch is begin W.Watch(S,Input,True,R,Deferred,Previous);Check(W.Valid(S),"valid watch");end;
 NR:M.Message;Handled,Now,Worked:Boolean;
 procedure Native(Sender:P.Word;ID,Seen:P.Word) is Q:M.Message:=M.NULL_MESSAGE;
 begin Q.tag.label:=P.Watch;Q.tag.length:=4;Q.authorityTag:=P.Word(ACPI_Protocol.Observer_Tag);Q.words:=[ID,Seen,0,0];
 M.Current_Reply_Authority:=True;ACPI_Native_Power_Inventory.Dispatch(M.Test_Process(Sender),Q,True,NR,Handled,Now);
 Check(Handled,"native operation");if Now then M.Current_Reply_Authority:=False;end if;end;
begin
 Check(W.Valid(S) and W.Quiet(S) and W.Generation(S)=1,"initial synchronizable inventory");
 Input.Authority:=0;Watch;Check(R.Status=P.Denied and R.Generation=0 and Saves=0,"denied no state/transport");
 Input.Authority:=P.Word(ACPI_Protocol.Observer_Tag);
 for Variant in 1..6 loop
  declare Bad:W.Request:=Input;begin
   case Variant is when 1=>Bad.Length:=3;when 2=>Bad.ID:=0;when 3=>Bad.Zero_1:=1;when 4=>Bad.Zero_2:=1;when 5=>Bad.Flags:=1;when 6=>Bad.Reserved:=1;end case;
   W.Watch(S,Bad,True,R,Deferred,Previous);Check(R.Status=P.Malformed and not Deferred and Saves=0,"malformed no transport");end;
 end loop;
 W.Watch(S,Input,False,R,Deferred,Previous);Check(R.Status=P.Unavailable and Saves=0,"unsafe boundary");
 Input.Seen:=P.Word'Last;Watch;Check(R.Status=P.Future_Generation and Saves=0,"full-width future generation");
 Input.Seen:=0;Watch;Check(R.Status=P.Changed and R.Generation=1 and not Deferred,"synchronize no save");Input.Seen:=1;
 Watch;Check(Deferred and Saves=1 and not W.Quiet(S) and not W.Runnable(S),"park without polling");
 Fail_Save:=True;Input.ID:=2;Watch;Check(not Deferred and R.Status=P.Unavailable and Answers=0,"failed replace preserves predecessor");Fail_Save:=False;
 Watch;Check(Deferred and Answers=1 and Last_Status=P.Superseded and Last_ID=1,"same sender atomic replacement");
 for I in 2..8 loop Input.Sender:=P.Word(I);Input.ID:=P.Word(I+10);Watch;Check(Deferred,"independent holder");end loop;
 Input.Sender:=99;Watch;Check(not Deferred and R.Status=P.Full and Answers=1,"full cannot evict another holder");
 W.Changed(S);W.Changed(S);Check(W.Generation(S)=3 and W.Runnable(S) and Answers=1,"copy callbacks no replies");
 W.Advance(S,False,Progress);Check(Progress=W.No_Attempt and Answers=1,"defer until quiet");
 Lose:=True;W.Advance(S,True,Progress);Check(Progress=W.Caller_Gone and Last_Generation=3,"caller gone consumed once");Lose:=False;
 for I in 1..7 loop W.Advance(S,True,Progress);Check(Progress=W.Answered and Last_Status=P.Changed and Last_Generation=3,"fair coalesced generation delivery");end loop;
 Check(W.Quiet(S) and W.Valid(S) and Answers=9,"all owned tokens consumed");
 W.Advance(S,True,Progress);Check(Progress=W.No_Attempt and Answers=9,"no replay");
 Input.Sender:=1;Input.Seen:=3;Input.ID:=77;Watch;Check(Deferred,"watch before maximum");W.Changed(S);Check(W.Generation(S)=4,"maximum valid generation");W.Changed(S);
 W.Advance(S,True,Progress);Check(Progress=W.Answered and Last_Status=P.Exhausted and Last_Generation=4,"no generation wrap");
 Input.Seen:=4;Watch;Check(R.Status=P.Exhausted and not Deferred,"exhausted admission closed");W.Begin_Stop(S);Watch;Check(R.Status=P.Stopping and W.Quiet(S),"stopping precedence");
 for T in Live'Range loop Check(not Live(T),"no leaked model authority");end loop;
 -- Actual native adapter with full-width kernel sender identity and checked IPC model.
 IPC.Reset;Native(16#FEDC_BA98_7654_3210#,1,0);Check(Now and NR.words=[1,0,1,P.Schema] and IPC.Save_Calls=0,"native initial reply");
 Native(16#FEDC_BA98_7654_3210#,2,1);Check(not Now and IPC.Live_Count=1,"native saved watch");
 declare Q:M.Message:=M.NULL_MESSAGE;begin
  ACPI_Native_Power_Inventory.Dispatch(M.Test_Process(1),Q,False,NR,Handled,Now);
  Check(not Handled and Now and IPC.Live_Count=1,"ordinary request resets reply suppression");
  Q.tag.label:=P.Watch;Q.tag.length:=4;Q.words:=[99,1,0,0];
  ACPI_Native_Power_Inventory.Dispatch(M.Test_Process(1),Q,True,NR,Handled,Now);
  Check(Handled and Now and NR.words(1)=P.Status'Enum_Rep(P.Denied) and NR.words(2)=0 and IPC.Live_Count=1,"native forged observer stamp refused");
  Q.authorityTag:=P.Word(ACPI_Protocol.Observer_Tag);Q.words(3):=1;
  ACPI_Native_Power_Inventory.Dispatch(M.Test_Process(1),Q,True,NR,Handled,Now);
  Check(Handled and Now and NR.words(1)=P.Status'Enum_Rep(P.Malformed) and IPC.Live_Count=1,"native nonzero spare word refused");
 end;

 Native(16#7654_3210#,3,1);Check(not Now and IPC.Live_Count=2,"upper sender bits distinguish holders");
 IPC.In_Callback:=True;ACPI_Native_Power_Inventory.Changed;Check(IPC.Answer_Calls=0,"native callback only facts");IPC.In_Callback:=False;
 ACPI_Native_Power_Inventory.Advance(False,Worked);Check(not Worked and IPC.Live_Count=2,"native unsafe boundary");
 IPC.Lose_Answer:=True;ACPI_Native_Power_Inventory.Advance(True,Worked);Check(Worked and IPC.Live_Count=1 and IPC.Last_Answer.words(2)=2,"native lost client consumes");IPC.Lose_Answer:=False;
 ACPI_Native_Power_Inventory.Advance(True,Worked);Check(Worked and ACPI_Native_Power_Inventory.Quiet and IPC.Live_Count=0,"native fair second delivery");
 Native(10,4,2);Check(not Now,"native park until stop");IPC.In_Callback:=True;ACPI_Native_Power_Inventory.Begin_Stop;IPC.In_Callback:=False;
 ACPI_Native_Power_Inventory.Advance(True,Worked);Check(Worked and IPC.Live_Count=0 and IPC.Last_Answer.words(1)=P.Status'Enum_Rep(P.Stopping),"native stop reply");
 ACPI_Native_Power_Inventory.Advance(True,Worked);Check(not Worked,"native stop no replay");
 Ada.Text_IO.Put_Line("Power inventory watch checks"&Checks'Image);
end Watch_Tests;
