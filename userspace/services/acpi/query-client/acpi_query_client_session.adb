pragma Ada_2022;
with ACPI_Passive_Query.Replies;
package body ACPI_Query_Client_Session is
   use ACPI_Query_Client_Results;
   package L renames ACPI_Query_Client_Lifetime;
   package W renames ACPI_Passive_Query;
   package R renames ACPI_Passive_Query.Replies;
   use type L.Phase,W.Decode_Status,T.Call_Status,T.Word;
   Life:L.State;
   Loan:Loan_Reference:=No_Reference;
   Revoke_Accepted:Boolean:=False;
   Original,Response:Buffer:=[others=>0];
   Result:Report;
   function Status return L.Phase is(L.Observe(Life).Current);
   function Cleanup_Pending return Boolean is(L.Has_Loan(Life));
   procedure Start(Request_Data:P.Byte_Array;Deadline:T.Word;Accepted:out Boolean) is
      OK,Ignored,Answered:Boolean;
      Reply:T.Packet;
      Value:T.Reply_Decode;
   begin
      Accepted:=Status=L.Idle;if not Accepted then return;end if;
      Original:=[others=>0];Response:=[others=>0];Result:=(others=><>);Revoke_Accepted:=False;
      if Request_Data'Length>Maximum_Bytes or else
        P.Decode(Request_Data,W.Node_Limit'Last).Status/=W.Decoded then
         Result.Status:=Invalid_Request;L.Begin_Grant(Life,False,Ignored);return;
      end if;
      for I in Request_Data'Range loop Original(I-Request_Data'First):=Request_Data(I);end loop;
      Store_Request(Original,OK);
      if not OK then Result.Status:=Store_Failed;L.Begin_Grant(Life,False,Ignored);return;end if;
      Create(Loan,OK);L.Begin_Grant(Life,OK,Ignored);
      if not OK then Loan:=No_Reference;Result.Status:=Create_Refused;return;end if;
      Result.Status:=Pending;
      Call(Loan,T.Word(Maximum_Bytes),Deadline,Reply,Answered);
      if not Answered then Result.Status:=Call_Failed;
      else
         Value:=T.Decode_Reply(Reply,T.Word(Maximum_Bytes));
         if not Value.Decoded then Result.Status:=Malformed_Reply;
         else
            Result.Has_Transport:=True;Result.Transport:=Value.Value;
            if Value.Value.Status/=T.Completed then Result.Status:=Transport_Refused;end if;
         end if;
      end if;
      L.Record_Reply(Life,Result.Status=Pending,Ignored);
   end Start;
   procedure Progress(Did_Work:out Boolean) is
      Confirmed,Accepted,Ignored:Boolean;
   begin
      Did_Work:=False;if Status/=L.Awaiting_Retirement then return;end if;
      if not Revoke_Accepted then Revoke(Loan,Revoke_Accepted);end if;
      Confirmed:=Retirement_Confirmed(Loan);
      L.Observe_Retirement(Life,Confirmed,Did_Work);
      if not Did_Work or else not L.May_Read_Payload(Life) then return;end if;
      Copy_Response(Response,Accepted);
      if not Accepted then Result.Status:=Copy_Failed;
      else
         declare Header:constant R.Header_Result:=R.Read_Envelope(Response,W.Node_Limit'Last);begin
            Accepted:=Header.Accepted and then R.Extent(Header.Value)=Result.Transport.Written
              and then R.Matches_Request(Response,Original,W.Node_Limit'Last);
         end;
         Result.Status:=(if Accepted then Completed else Invalid_Payload);
      end if;
      if not Accepted then Response:=[others=>0];end if;
      L.Finish_Payload(Life,Accepted,Ignored);
   end Progress;
   procedure Take(Value:out Report;Data:out Buffer;Taken:out Boolean) is
      Ignored:Boolean;
   begin
      Value:=(others=><>);Data:=[others=>0];Taken:=Status=L.Result_Ready;
      if not Taken then return;end if;
      Value:=Result;if L.Payload_Available(Life) then Data:=Response;end if;
      L.Consume(Life,Ignored);Loan:=No_Reference;Result:=(others=><>);Original:=[others=>0];Response:=[others=>0];
   end Take;
end ACPI_Query_Client_Session;
