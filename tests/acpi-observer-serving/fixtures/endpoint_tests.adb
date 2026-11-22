pragma Ada_2022;
with ACPI_Native_Endpoint;with ACPI_Serving_State;with ACPI_Requests;with ACPI_Endpoint;with ACPI_Protocol;
with ACPI_Service;with AML_Objects;with ACPI_Hosted_Heap;with Firmware_Tables;
with CuBit.Messages;with Interfaces;with Ada.Text_IO;with Ada.Command_Line;with Ada.Streams;with Ada.Streams.Stream_IO;
pragma Elaborate_All(ACPI_Hosted_Heap);
procedure Endpoint_Tests is
 package N renames ACPI_Native_Endpoint;package P renames ACPI_Serving_State;package R renames ACPI_Requests;package M renames CuBit.Messages;
 package IO renames Ada.Streams.Stream_IO;use type R.Outcome,R.Phase,M.MessageWords,P.Mode,ACPI_Service.Metrics,Interfaces.Unsigned_64,Interfaces.Unsigned_32;
 Server:R.State(1,256,256,1,16#1_0000_0007#,AML_Objects.Max_Objects,AML_Objects.Max_Bytes,AML_Objects.Max_Elements);
 Checks:Natural:=0;Accepted,Released:Boolean;Answer:R.Response;
 procedure Check(OK:Boolean;Why:String:="") is begin Checks:=Checks+1;if not OK then raise Program_Error with Checks'Image&" "&Why;end if;end;
 File:IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..256);Last:Ada.Streams.Stream_Element_Offset;Data:Firmware_Tables.Bytes(1..256);
 Mode:constant Natural:=Natural'Value(Ada.Command_Line.Argument(2));
 type Mutation_Labels is array(Positive range<>)of Interfaces.Unsigned_32;
 Mutations:constant Mutation_Labels:=[R.Start_Snapshot,R.Begin_Table,R.Write_Chunk,R.Commit_Table,R.Finish_Snapshot,R.Configure_Notifications,R.Claim_Notification,R.Acknowledge_Notification];
 type Senders is array(Positive range<>)of M.Process_ID;
 IDs:constant Senders:=[0,1,255,256,16#1_0000_0007#,M.Process_ID'Last];
 Facts:constant P.Eligibility:=(others=>True);Request,Reply:M.Message;
 Expected:M.MessageWords;
 procedure Dispatch(From:M.Process_ID) is Before:constant R.Revision_Number:=R.Revision(Server);Metrics_Before:constant ACPI_Service.Metrics:=R.Observe(Server);begin
 N.Dispatch(Server,From,Request,Reply);Check(R.Valid(Server) and R.Revision(Server)=Before and not R.Retained_Results_Pending(Server) and R.Observe(Server)=Metrics_Before,"observer dispatch preserves catalog epoch and roots");end;
 begin
 IO.Open(File,IO.In_File,Ada.Command_Line.Argument(1));IO.Read(File,Raw,Last);IO.Close(File);
 for I in 1..Natural(Last) loop Data(I):=Firmware_Tables.Byte(Raw(Ada.Streams.Stream_Element_Offset(I)));end loop;
 R.Handle(Server,R.Snapshot_Provider,(Label=>R.Start_Snapshot,Data=>[R.Revision(Server),1,0,0],others=><>),Answer);Check(Answer.Status=R.OK);
 R.Import_Block(Server,R.Snapshot_Provider,R.Revision(Server),1,ACPI_Service.DSDT,Data(1..Natural(Last)),Answer);Check(Answer.Status=R.OK);
 R.Handle(Server,R.Snapshot_Provider,(Label=>R.Finish_Snapshot,Data=>[R.Revision(Server),0,0,0],others=><>),Answer);Check(Answer.Status=R.OK and R.Current(Server)=R.Complete);
 if Mode=1 then N.Select_Runtime(True,Accepted);Check(Accepted);
 elsif Mode=2 then N.Select_Observer(Facts,True,Accepted);Check(Accepted);
 end if;
 Expected:=[R.Revision(Server),Interfaces.Unsigned_64(Mode),(if Mode=2 then 1 else 0),(if Mode=1 then 1 else 0)];
 for From of IDs loop
 Request:=(tag=>(label=>R.Read_Metrics,others=><>),words=>[N.Metrics_Serving_State_Page,0,0,0],authorityTag=>Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag));
 Dispatch(From);Check(Reply.tag.label=ACPI_Endpoint.Reply_OK and Reply.words=Expected,"native serving status exact typed words");
 for Mutation in 1..8 loop
 declare Bad:M.Message:=Request;Saved:constant M.Message:=Request;begin
 case Mutation is
 when 1=>Bad.authorityTag:=0;Bad.words(1):=Interfaces.Unsigned_64(ACPI_Protocol.Observer_Tag);
 when 2=>Bad.tag.length:=3;when 3=>Bad.tag.protocol:=1;when 4=>Bad.tag.ipc:=1;
 when 5=>Bad.tag.reserved:=1;when 6=>Bad.words(1):=1;when 7=>Bad.words(2):=Interfaces.Unsigned_64'Last;when 8=>Bad.words(3):=1;
 end case;Request:=Bad;Dispatch(From);Check(Reply.tag.label=ACPI_Endpoint.Reply_Error,"malformed/undeclared serving-state read refused");Request:=Saved;
 end;end loop;
 -- Observer authority never gains snapshot/import/notification mutation rights.
 for Label of Mutations loop
 Request.tag.label:=Label;Request.words:=[R.Revision(Server),0,0,0];Dispatch(From);
 Check(Reply.tag.label=ACPI_Endpoint.Reply_Error and Reply.words(0)=R.Outcome'Pos(R.Denied),"observer cannot mutate snapshot");end loop;
 Request.tag.label:=R.Read_Metrics;Request.words:=[0,0,0,0];Dispatch(From);Check(Reply.tag.label=ACPI_Endpoint.Reply_OK,"copied catalog metrics stay readable");
 Request.tag.label:=R.Read_Table_Info;Request.words:=[R.Revision(Server),1,0,0];Dispatch(From);Check(Reply.tag.label=ACPI_Endpoint.Reply_OK,"copied table metadata stays readable");
 Request.tag.label:=R.Read_Table_Chunk;Request.words:=[R.Revision(Server),1,0,0];Dispatch(From);Check(Reply.tag.label=ACPI_Endpoint.Reply_OK,"copied table bytes stay readable");
 end loop;
 Check(N.Serving_State.Current=(if Mode=0 then P.Not_Serving elsif Mode=1 then P.Local_Runtime else P.Observer_Only));
 R.Release_Storage(Server,Released);Check(Released);Ada.Text_IO.Put_Line("Observer endpoint checks"&Checks'Image);
end Endpoint_Tests;
