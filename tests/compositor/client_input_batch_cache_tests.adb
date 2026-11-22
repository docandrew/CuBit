with Ada.Text_IO;
with Client_Input_Batch_Cache;
procedure Client_Input_Batch_Cache_Tests is
   package C renames Client_Input_Batch_Cache;
   package W renames C.W;
   package P renames C.P;
   package DP renames C.DP;
   use type W.Word;
   use type C.State;
   use type DP.Status_Code;
   S : C.State;
   Batch : W.B.Batch;
   Page : W.Snapshot_Words;
   Receipt : DP.Wire_Message;
   Accepted : Boolean;
   Item : DP.Input_Result;
begin
   for Length in W.B.Count loop
      for More in Boolean loop
         if Length > 0 or not More then
            C.Clear (S);
            Batch := (Length => Length, Through => 100 + W.Word (Length), More => More, others => <>);
            for I in 1 .. Length loop Batch.Items (I) := (True,100+W.Word(I),6,42,65+W.Word(I),0); end loop;
            Page := W.Encode (Batch,42,123,100);
            Receipt := P.Encode (P.Receipt'(DP.Success,123,Length,Batch.Through,More));
            C.Load (S,Page,Receipt,42,123,100,Accepted);
            pragma Assert (Accepted and C.Remaining(S)=Length and C.Expected_After(S)=100);
            if Length > 0 then
               declare Before : constant C.State := S; begin
                  C.Load(S,Page,Receipt,42,123,100,Accepted);
                  pragma Assert(not Accepted and S=Before);
                  C.Take(S,43,100,Item); pragma Assert(Item.Status/=DP.Success and S=Before);
                  C.Take(S,42,101,Item); pragma Assert(Item.Status/=DP.Success and S=Before);
               end;
            end if;
            for I in 1 .. Length loop
               C.Take(S,42,99+W.Word(I),Item);
               pragma Assert(Item.Status=DP.Success and then Item.Value.Serial=100+W.Word(I)
                 and then Item.Value.Payload0=65+W.Word(I) and then Item.Value.More_Pending=(I<Length or More));
               pragma Assert(C.Remaining(S)=Length-I and C.Expected_After(S)=100+W.Word(I));
            end loop;
            C.Take(S,42,Batch.Through,Item); pragma Assert(Item.Status/=DP.Success);
            C.Clear(S);
            declare Before : constant C.State := S; begin
               Receipt.Words(3):=Receipt.Words(3)+1;
               C.Load(S,Page,Receipt,42,123,100,Accepted); pragma Assert(not Accepted and S=Before);
               Receipt:=P.Encode(P.Receipt'(DP.Success,123,Length,Batch.Through,More));
               Receipt.Words(2):=(if Length=8 then 7 else W.Word(Length+1));
               C.Load(S,Page,Receipt,42,123,100,Accepted); pragma Assert(not Accepted and S=Before);
               Receipt:=P.Encode(P.Receipt'(DP.Success,123,Length,Batch.Through,More));
               if Length>0 then
                  Receipt:=P.Encode(P.Receipt'(DP.Success,123,Length,Batch.Through,not More));
                  C.Load(S,Page,Receipt,42,123,100,Accepted); pragma Assert(not Accepted and S=Before);
                  Receipt:=P.Encode(P.Receipt'(DP.Success,123,Length,Batch.Through,More));
               end if;
               C.Load(S,Page,Receipt,42,124,100,Accepted); pragma Assert(not Accepted and S=Before);
               Page(7):=1;
               C.Load(S,Page,Receipt,42,123,100,Accepted); pragma Assert(not Accepted and S=Before);
            end;
         end if;
      end loop;
   end loop;
   C.Clear(S);
   Batch:=(Length=>1,Through=>W.Word'Last,others=><>);
   Batch.Items(1):=(True,W.Word'Last,10,42,0,0);
   Page:=W.Encode(Batch,42,123,W.Word'Last-1);
   Receipt:=P.Encode(P.Receipt'(DP.Success,123,1,W.Word'Last,False));
   C.Load(S,Page,Receipt,42,123,W.Word'Last-1,Accepted); pragma Assert(Accepted);
   C.Take(S,42,W.Word'Last-1,Item);
   pragma Assert(Item.Status=DP.Success and then Item.Value.Serial=W.Word'Last);
   C.Take(S,42,W.Word'Last,Item); pragma Assert(Item.Status/=DP.Success);
   Ada.Text_IO.Put_Line("CLIENT INPUT CACHE: PASS ordered consumption, per-event acknowledgment, binding and atomic rejection");
end Client_Input_Batch_Cache_Tests;
