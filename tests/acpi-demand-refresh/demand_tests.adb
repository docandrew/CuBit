pragma Ada_2022;
with Ada.Text_IO;with Interfaces;with ACPI_Demand_Refresh;with ACPI_Native_Battery_Reads;
with ACPI_Requests;with ACPI_Protocol;with ACPI_Endpoint;with CuBit.Messages;with CuBit.Handles.IPC;with Demand_Test_Control;
procedure Demand_Tests is
 package IPC renames CuBit.Handles.IPC;package M renames CuBit.Messages;package T renames Demand_Test_Control;
 use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,M.MessageWords;
 Checks:Natural:=0;
 procedure Check(B:Boolean;Why:String) is
 begin Checks:=Checks+1;if not B then raise Program_Error with Why;end if;end;
 package L is new ACPI_Demand_Refresh(Minimum_Interval=>10,Maximum_Serial=>3);
 S:L.State;Ticket:Interfaces.Unsigned_64;New_Attempt,Accepted:Boolean;
 Server:ACPI_Requests.State;Q,Reply:M.Message:=M.NULL_MESSAGE;Handled,Now,Worked:Boolean;
 procedure Read is
 begin M.Current_Reply_Authority:=True;ACPI_Native_Battery_Reads.Dispatch(Server,Q,True,Reply,Handled,Now);
 if Now then M.Current_Reply_Authority:=False;end if;end;
 procedure Packet is
 begin Q:=M.NULL_MESSAGE;Q.tag.length:=4;Q.authorityTag:=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);Q.words:=[22,5,0,0];end;
 procedure Clear_Readers is
 begin for I in 1..8 loop ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);end loop;Check(ACPI_Native_Battery_Reads.Quiet and IPC.Live_Count=0,"all saved authorities consumed");end;
begin
 Check(L.Valid(S) and not L.Pending(S),"initial demand state");
 L.Request(S,0,False,Ticket,New_Attempt,Accepted);Check(not Accepted and Ticket=0,"unknown clock refused");
 L.Request(S,100,True,Ticket,New_Attempt,Accepted);Check(Accepted and New_Attempt and Ticket=1,"first explicit attempt");
 for I in 100..109 loop L.Request(S,Interfaces.Unsigned_64(I),True,Ticket,New_Attempt,Accepted);Check(Accepted and not New_Attempt and Ticket=1,"coalesce no deadline/cooldown reset");end loop;
 L.Request(S,99,True,Ticket,New_Attempt,Accepted);Check(not Accepted,"regressed clock cannot coalesce");
 L.Complete(S);Check(L.Ready(S,1) and not L.Ready(S,2),"completion matches attempt serial");
 L.Request(S,109,True,Ticket,New_Attempt,Accepted);Check(not Accepted,"minimum interval");
 L.Request(S,110,True,Ticket,New_Attempt,Accepted);Check(Accepted and New_Attempt and Ticket=2,"interval boundary");L.Complete(S);
 L.Request(S,Interfaces.Unsigned_64'Last,True,Ticket,New_Attempt,Accepted);Check(Accepted and Ticket=3,"clock maximum safely subtracts");L.Complete(S);
 L.Request(S,Interfaces.Unsigned_64'Last,True,Ticket,New_Attempt,Accepted);Check(not Accepted and Ticket=0,"serial exhausted never wraps");L.Begin_Stop(S);Check(L.Stopping(S) and L.Valid(S),"stop valid");
 -- Actual native saved-reader queue and codec with explicit source/cache models.
 IPC.Reset;Packet;T.Cache_Status:=1;Read;Check(Handled and Now and IPC.Save_Calls=0 and T.Attempts=0,"fresh read no saved reply or AML demand");
 T.Cache_Status:=4;Q.authorityTag:=0;Read;Check(Now and IPC.Save_Calls=0,"forged stamp refused before demand");Packet;Q.words(2):=Interfaces.Unsigned_64'Last;Read;Check(Now and IPC.Save_Calls=0,"wide selector refused");
 Packet;Q.tag.reserved:=1;Read;Check(Now and IPC.Save_Calls=0,"spare tag refused");Packet;Q.words(3):=8;Read;Check(Now and IPC.Save_Calls=0,"future cache generation no refresh");
 Packet;Q.words(2):=1;Q.words(3):=7;Read;Check(Now and IPC.Save_Calls=0,"timestamp page descriptive only");Packet;
 IPC.Fail_Save:=True;Read;Check(Now and IPC.Live_Count=0 and T.Commit_Calls=0 and T.Attempts=0,"save refusal cannot start producer");IPC.Fail_Save:=False;
 Read;Check(not Now and IPC.Live_Count=1 and T.Attempts=1,"expired read deferred for one fixed attempt");
 T.Cache_Status:=0;
 for I in 2..8 loop Read;Check(not Now and T.Attempts=1 and IPC.Live_Count=I,"pending header coalesces");end loop;
 Read;Check(Now and IPC.Live_Count=8 and T.Attempts=1,"full pool retains prior readers");
 ACPI_Native_Battery_Reads.Advance(Server,False,True,Worked);Check(not Worked and IPC.Live_Count=8,"unsafe boundary retains replies");
 ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);Check(not Worked,"no premature answer");
 T.Cache_Status:=1;T.Generation:=8;T.Completed:=True;IPC.Lose_Answer:=True;
 ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);Check(Worked and IPC.Live_Count=7 and IPC.Last_Answer.words=[8,1,0,0],"fresh result even when caller gone consumes once");IPC.Lose_Answer:=False;Clear_Readers;
 ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);Check(not Worked,"no delivery replay");
 T.Cache_Status:=4;T.Clock:=101;Read;Check(Now and T.Attempts=1,"request storm rate limited");
 T.Clock:=110;T.Completed:=False;T.Commit_Allowed:=False;Read;Check(not Now and IPC.Live_Count=1,"failed post-save commit retains authority");
 ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);Check(Worked and IPC.Last_Answer.words=[0,0,0,0] and IPC.Live_Count=0,"commit refusal answers unavailable once");T.Commit_Allowed:=True;
 T.Clock_Known:=False;Read;Check(Now and IPC.Live_Count=0,"unknown clock no admission");T.Clock_Known:=True;T.Clock:=99;Read;Check(Now and IPC.Live_Count=0,"regressed clock no admission");
 T.Clock:=110;Q.words(2):=16+6;Q.words(3):=8;Read;Check(not Now and T.Attempts=2,"auxiliary stale read joins fixed producer");T.Generation:=9;T.Cache_Status:=1;T.Completed:=True;
 ACPI_Native_Battery_Reads.Advance(Server,True,True,Worked);Check(Worked and IPC.Last_Answer.tag.label=ACPI_Endpoint.Reply_Error and IPC.Last_Answer.words(0)=3,"expected generation rechecked after refresh");
 Packet;T.Clock:=120;T.Cache_Status:=4;T.Completed:=False;Read;Check(not Now,"next explicit interval admitted");IPC.In_Callback:=True;ACPI_Native_Battery_Reads.Begin_Stop;IPC.In_Callback:=False;
 ACPI_Native_Battery_Reads.Advance(Server,False,False,Worked);Check(not Worked and IPC.Live_Count=1,"stop retains unsafe reply");Clear_Readers;
 Check(T.Attempts=3,"no timer or automatic retry");Ada.Text_IO.Put_Line("Demand refresh checks"&Checks'Image);
end Demand_Tests;
