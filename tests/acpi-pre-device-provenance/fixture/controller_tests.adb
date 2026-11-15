-- Hosted regression main; physical/kernel boundaries are modeled.
with Interfaces;with Ada.Command_Line; with Ada.Text_IO; with ACPI_Native_Pre_Device; with ACPI_Native_Delays; with ACPI_Service; with Prep_Model; with Sender_Model; with CuBit.Handles;
with GPIO_Bootstrap_Payload;
procedure Controller_Tests is
 package P renames ACPI_Native_Pre_Device; package M renames Prep_Model; package S renames Sender_Model;
 use type P.Phase,P.Failure,ACPI_Service.Node_Descriptor,GPIO_Bootstrap_Payload.Request;
 use type Interfaces.Unsigned_64;
 Mode : constant Positive:=Positive'Value(Ada.Command_Line.Argument(1));
 Service : aliased ACPI_Service.State;
 Accepted : Boolean; Charged,Checks,Before : Natural:=0;
 Target : CuBit.Handles.Endpoint:=77;
 Snapshot:P.Submitted_Request;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with "case" & Mode'Image & " check" & Checks'Image; end if; end Check;
begin
 M.Mode:=Mode; S.Reset; S.Mode:=1; ACPI_Native_Delays.Test_Start;
 case Mode is
 when 7=>M.Busy:=True;
 when 8=>Target:=0;
 when 13=>S.Mode:=6;
 when 14=>S.Mode:=33;
 when 15=>S.Mode:=34;
 when 16=>S.Mode:=43; S.Confirmed:=False;
 when 17=>S.Good_Tag:=False;
 when 19=>S.Mode:=49;
 when 22=>S.Mode:=17;
 when others=>null;
 end case;
 Check(not P.Submitted.Present);
 P.Configure(Target,10000,Accepted); Check(Accepted);
 P.Configure(Target,99999,Accepted); Check(not Accepted);
 P.Prepare(Service,(if Mode=23 then 2 else 100),Charged,Accepted);
 Check(not P.Observe.Working);
 Snapshot:=P.Submitted;Check(Snapshot.Present=(S.Calls>0));
 if Snapshot.Present then
 Check(Snapshot.Data.Epoch=16#1_0000_0007# and Snapshot.Request_ID=1);
 Check(Snapshot.Device=ACPI_Service.Node_Descriptor(M.Source_Device));
 Check(Snapshot.Receipt_Known=P.Observe.Admission_Known);
 Check(Snapshot.Data.Path(1)="_SB_" and Snapshot.Data.Path(2)="GPI0");
 end if;
 M.Source_Device:=16#F111_2222_0000_0042#;
 if Mode in 1 | 2 | 8 | 13 | 14 | 17 | 20 | 22 then
 Check(Accepted and P.Observe.Stage in P.Prepared | P.Prepared_Without_Admission);
 else Check(not Accepted); end if;
 Check(Charged=(if Mode in 7|24 then 0 elsif Mode=23 then 2 else 3));
 if Mode in 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 21 | 23 | 24 | 25 then Check(S.Calls=0);
 elsif Mode=17 then Check(S.Calls=0 and P.Observe.Why=P.Admission_Refused);
 elsif Mode in 14 | 15 | 19 then Check(S.Calls=2);
 else Check(S.Calls=1); end if;
 if S.Calls>0 then Check(M.Roots=0 and M.Pauses=1); end if;
 if Mode in 15 | 19 then Check(P.Observe.Why=P.Admission_Unknown and not P.Quiet and M.Resumes=0);
 elsif Mode=16 then Check(P.Observe.Why=P.Grant_Cleanup_Pending and not P.Quiet and S.Frees=0 and M.Resumes=0);
 elsif Mode=18 then Check(P.Observe.Why=P.Resume_Refused and M.Resumes=1);
 elsif Mode=24 then Check(P.Observe.Why=P.Incomplete_Initializer_History and M.Discoveries=0);
 elsif Mode=25 then Check(P.Observe.Why=P.Initializer_History_Changed and M.Payloads=0);
 elsif Mode=9 then Check(P.Observe.Why=P.Fatal_AML);
 elsif Mode=10 then Check(P.Observe.Why=P.Field_Uncertain);
 end if;
 Before:=M.Discoveries; P.Prepare(Service,100,Charged,Accepted);
 Check(not Accepted and Charged=0 and M.Discoveries=Before);
 S.Confirmed:=True; P.Close(Service);
 if Mode in 15 | 19 | 21 then Check(not P.Quiet); else Check(P.Quiet); end if;
 Check(M.Discoveries=Before and S.Calls<=2 and not P.Observe.Working);
 declare Later:constant P.Submitted_Request:=P.Submitted;begin
 Check(Later.Present=Snapshot.Present);
 if Later.Present then Check(Later.Data=Snapshot.Data and Later.Device=Snapshot.Device and Later.Request_ID=Snapshot.Request_ID);end if;
 end;
 Ada.Text_IO.Put_Line("Pre-device checks" & Checks'Image);
end Controller_Tests;