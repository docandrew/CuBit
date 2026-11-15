pragma Ada_2022;
with ACPI_Startup_Completion;
with ACPI_Native_Consumer_Completion;with Sender_Model;with Interfaces;with Ada.Command_Line;with Ada.Text_IO;
procedure Completion_Tests is
 package S renames Sender_Model;
 use type Interfaces.Unsigned_64;
 function Boundary return Boolean is begin S.Boundary_Reads:=S.Boundary_Reads+1;return S.Boundary and then(S.Boundary_Reads=1 or else S.Second_Boundary);end;
 package T is new ACPI_Native_Consumer_Completion(Boundary);
 package P renames T.Protocol;
 use type T.Outcome,P.Phase_Status;
 Mode:constant Natural:=Natural'Value(Ada.Command_Line.Argument(1));
 Result:T.Report;Declared:P.Phase_Status:=P.Completed;
 Request:Interfaces.Unsigned_64:=16#F000_0000_0000_0001#;
 Epoch,Deadline:Interfaces.Unsigned_64;
 Checks:Natural:=0;
 Expected_Dispatch : constant Boolean := Mode in 1..15|22..23|25..27|35..44|46..58;
 Expected_Initial_Retention : constant Boolean := Expected_Dispatch and then Mode not in 1|25|35|41..43|46;
 Expected_Final_Retention : constant Boolean := Expected_Dispatch and then Mode not in 1|22|25|35|40..44|46|56..57;
 Expected_Released : constant Interfaces.Unsigned_64 :=
   (if Mode in 47|55|58 then 1 elsif Mode=48 then Interfaces.Unsigned_64'Last
    elsif Mode=49 then 16#1_0000_0000# else 0);
 procedure Check(OK:Boolean) is begin Checks:=Checks+1;if not OK then raise Program_Error with "completion test "&Checks'Image;end if;end;
begin
 S.Reset;S.Times:=[100,200,200];S.Mode:=Mode;S.Own_ID:=16#F123_4567_0000_0007#;Epoch:=S.Own_ID;Deadline:=10000;
 case Mode is
 when 16=>S.Boundary:=False;when 17=>Request:=0;when 18=>Epoch:=7;
 when 19=>S.Current:=False;when 20=>S.Read_Right:=False;when 21=>S.Good_Tag:=False;
 when 24=>Deadline:=Interfaces.Unsigned_64'Last;when 25=>Deadline:=150;when 26=>S.Times(2):=10000;
 when 27=>S.Times(2):=50;when 28=>Deadline:=100;when 29=>Deadline:=0;
 when 30=>S.Good_Peer:=False;when 31=>S.Good_Kind:=False;when 32=>Epoch:=0;when 33=>S.Own_ID:=0;
 when 34=>S.Second_Boundary:=False;
 when 35=>Request:=Interfaces.Unsigned_64'Last;
 when 40|42|50=>Declared:=P.Incomplete;when 41|52=>Declared:=P.Completed_With_Errors;when 43|51=>Declared:=P.Failed;
 when 44=>Deadline:=250;when 45=>S.Times(1):=Interfaces.Unsigned_64'Last;
 when 46=>S.Times:=[Interfaces.Unsigned_64'Last-5,Interfaces.Unsigned_64'Last-5,Interfaces.Unsigned_64'Last-5];Deadline:=Interfaces.Unsigned_64'Last-1;
 when others=>null;end case;
 T.Submit(77,Request,Epoch,Deadline,Declared,Result);
 Check(Result.Attempted and not Result.Working);
 Check(Result.Dispatched=Expected_Dispatch);
 Check(ACPI_Startup_Completion.Requires_Retention(Result.Dispatched,Result.Receipt_Known,Result.Answer,Result.Released)=Expected_Initial_Retention);
 if Mode in 1..5|25|35|41..43|46..49|58 then
  Check(Result.State=T.Receipt_Confirmed and Result.Receipt_Known and S.Calls=1 and T.Quiet);
  Check(T.Completed_Received=(Mode in 1|25|35|46..49|58));
 elsif Mode in 6..15|38..39|50..54 then
  Check(Result.State=T.Invalid_Reply and not Result.Receipt_Known and not T.Quiet);
  T.Confirm(Result);Check(S.Calls=2 and not Result.Receipt_Known and not T.Quiet);
 elsif Mode in 22..23|26..27|36..37|40|44|55..56 then
  Check(Result.State=T.Completion_Unknown and S.Calls=1 and not T.Completed_Received and not T.Quiet);
  T.Submit(77,2,Epoch,10000,P.Completed,Result);
  Check(S.Calls=1 and Result.Declared=Declared);
  if Mode=36 then T.Abandon_Confirmation;Result:=T.Observe;
  else if Mode=37 then S.Boundary:=False;end if;T.Confirm(Result);end if;
  Check(Result.Confirmation_Attempted and S.Calls=(if Mode in 26..27|36..37 then 1 else 2));
  Check(T.Completed_Received=(Mode in 22|44|55..56));
  Check(Result.Declared=Declared and Result.Request=Request and Result.Epoch=Epoch);
 elsif Mode=57 then
  Check(Result.State=T.Invalid_Reply and not Result.Receipt_Known);
  T.Confirm(Result);Check(Result.State=T.Receipt_Confirmed and Result.Receipt_Known and Result.Released=0 and S.Calls=2);
 else
  Check(S.Calls=0 and T.Quiet and not T.Completed_Received);
  Check(Result.State=(case Mode is when 16|34=>T.Boundary_Refused,when 17|18|32|33=>T.Request_Refused,when 19..21|30..31=>T.Endpoint_Refused,when others=>T.Clock_Refused));
 end if;
 Check(Result.Released=Expected_Released);
 declare Calls:constant Natural:=S.Calls;begin
 if Mode=58 then S.Mode:=1;end if;
 T.Submit(77,99,Epoch,10000,P.Completed,Result);
 Check(S.Calls=Calls and Result.Request=Request and Result.Epoch=Epoch and Result.Declared=Declared);
 T.Confirm(Result);Check(S.Calls=Calls and Result.Released=Expected_Released);
 end;
 Check(S.Allocations=0 and S.Creates=0 and S.Revokes=0);
 Check(ACPI_Startup_Completion.Requires_Retention(Result.Dispatched,Result.Receipt_Known,Result.Answer,Result.Released)=Expected_Final_Retention);
 Ada.Text_IO.Put_Line("Consumer completion checks"&Checks'Image);
end Completion_Tests;
