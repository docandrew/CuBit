package body CuBit.Handles.IPC is
 Live:array(1..Maximum_Slots)of Boolean:=[others=>False];
 function Live_Count return Natural is
 N:Natural:=0;begin for B of Live loop if B then N:=N+1;end if;end loop;return N;end;
 procedure Reset is
 begin
 if Live_Count/=0 then raise Program_Error with "reset would discard a live saved authority";end if;
 Fail_Save:=False;Lose_Answer:=False;In_Callback:=False;Save_Calls:=0;Answer_Calls:=0;Last_Answer:=CuBit.Messages.NULL_MESSAGE;
 end;
 function Save_Reply return Saved_Reply is
 begin
 if In_Callback or not CuBit.Messages.Current_Reply_Authority then raise Program_Error with "save outside received/quiet request";end if;
 Save_Calls:=Save_Calls+1;
 if Fail_Save then return(Saved=>False,Why=>No_Room);end if;
 for I in Live'Range loop
 if not Live(I) then
 Live(I):=True;CuBit.Messages.Current_Reply_Authority:=False;return(Saved=>True,Handle=>(Slot=>I));
 end if;end loop;return(Saved=>False,Why=>No_Room);
 end;
 function Answer(Item:Reply;Msg:CuBit.Messages.Message) return Boolean is
 begin
 if In_Callback or else Item.Slot=0 or else not Live(Item.Slot) then raise Program_Error with "saved reply reused or answered in callback";end if;
 Live(Item.Slot):=False;Answer_Calls:=Answer_Calls+1;Last_Answer:=Msg;return not Lose_Answer;
 end;
end CuBit.Handles.IPC;
