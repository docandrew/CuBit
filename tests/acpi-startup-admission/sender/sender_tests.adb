-- Hosted regression main; physical/kernel boundaries are modeled.
with Ada.Text_IO; with Interfaces; with Sender_Model; with ACPI_Native_GPIO_Admission; with GPIO_Bootstrap_Payload;
procedure Sender_Tests is
 package P renames GPIO_Bootstrap_Payload; package Model renames Sender_Model;
 use type Interfaces.Unsigned_64;
 Checks : Natural:=0;
 procedure Check(OK : Boolean) is begin Checks:=Checks+1; if not OK then raise Program_Error with Checks'Image; end if; end Check;
 procedure Scenario(Mode : Positive) is
 function Boundary return Boolean;
 package S is new ACPI_Native_GPIO_Admission(Boundary);
 use type S.Outcome;
 function Boundary return Boolean is begin
 Check(S.Observe.Working); S.Advance_Cleanup;
 Model.Boundary_Reads:=Model.Boundary_Reads+1;
 return Model.Boundary and then (Model.Boundary_Reads=1 or else Model.Second_Boundary);
 end Boundary;
 Data : P.Request;
 R : S.Report; Before_Calls,Before_Frees : Natural;
 ID : Interfaces.Unsigned_64 := 16#1_0000_0011#;
 begin
 Model.Reset; Model.Mode:=Mode;
 Data.Spans:=1; Data.Segments:=2; Data.Path(1):="_SB_"; Data.Path(2):="GPI0";
 Data.HID:="INTC1057"; Data.Table:="DSDT"; Data.Table_ID:="TESTGPIO"; Data.Epoch:=16#1_0000_0007#; Data.STA:=15;
 Data.Items(1):=(Base=>16#F000_0000#,Length=>4096,From=>P.Memory32_Fixed,Writable=>True);
 case Mode is
 when 18 => Model.Boundary:=False;
 when 19 => Data.Epoch:=7;
 when 20 => ID:=0;
 when 21 => Model.Current:=False;
 when 22 => Model.Good_Kind:=False;
 when 23 => Model.Read_Right:=False;
 when 24 => Model.Good_Tag:=False;
 when 25 => Model.Good_Peer:=False;
 when 26 => Model.Allocate_OK:=False;
 when 28 => Model.Create_OK:=False;
 when 29 => Model.Times(1):=Interfaces.Unsigned_64'Last-999;
 when 30 => Model.Times(2):=1100;
 when 31 => Model.Times(2):=99;
 when 32 => Model.Second_Boundary:=False;
 when 43 => Model.Confirmed:=False;
 when 44 => Model.Confirmed:=False; Model.Revoke_OK:=False;
 when 45 => Model.Revoke_OK:=False;
 when 54 => Model.Times:=[16#1_0000_0064#,16#1_0000_0064#,16#1_0000_00C8#];
 when 55 => Model.Times:=[others=>Interfaces.Unsigned_64'Last-1001];
 when 56 => Model.Times(1):=Interfaces.Unsigned_64'Last-1000;
 when 57 => Data.Epoch:=0; Model.Own_ID:=0;
 when 58 => Model.Own_ID:=0;
 when 59 => Data.Spans:=0;
 when 60 => Data.Segments:=0;
 when others => null;
 end case;
 S.Submit(77,Data,ID,R); Check(R.Attempted and not R.Working);
 Check(Model.Calls<=1 and Model.Creates<=1 and Model.Frees<=1);
 if Mode in 1 .. 17 | 43 .. 45 | 54 | 55 then
 Check(R.State=S.Reply_Confirmed and R.Reply_Known and R.Calls=1);
 elsif Mode in 18 .. 32 | 56 .. 60 then
 Check(not R.Dispatched and not R.Reply_Known and Model.Calls=0);
 elsif Mode in 33 | 34 | 46 .. 49 | 51 then
 Check(R.State=S.Completion_Unknown and not R.Reply_Known and not S.Quiet);
 else Check(R.State=S.Invalid_Reply and not R.Reply_Known and not S.Quiet);
 end if;
 if Mode in 18 .. 25 | 29 | 56 .. 60 then Check(Model.Allocations=0 and Model.Creates=0); end if;
 if Mode in 33 .. 42 | 46 .. 53 then
 Check(R.Grant_Pending and R.Memory_Owned and Model.Revokes=0 and Model.Frees=0);
 S.Advance_Cleanup; Check(Model.Revokes=0 and Model.Frees=0);
 if Mode=46 then Model.Boundary:=False;
 elsif Mode=47 then Model.Times(3):=Interfaces.Unsigned_64'Last;
 elsif Mode=48 then Model.Times(3):=99; end if;
 S.Confirm(R); Check(R.Confirmation_Attempted and not R.Working);
 if Mode in 33 | 50 | 51 then Check(R.Reply_Known and R.State=S.Reply_Confirmed and S.Quiet);
 else Check(not R.Reply_Known and not S.Quiet); end if;
 Check(not R.Grant_Pending and not R.Memory_Owned and Model.Frees=1);
 Check(Model.Calls=(if Mode in 46 .. 48 then 1 else 2));
 end if;
 if Mode in 43 | 44 then
 Check(R.Grant_Pending and R.Memory_Owned and not S.Quiet and Model.Frees=0);
 S.Advance_Cleanup; Check(Model.Frees=0);
 Model.Confirmed:=True; S.Advance_Cleanup; Check(S.Quiet and Model.Frees=1);
 elsif Mode not in 33 .. 42 | 46 .. 53 then Check(S.Quiet); end if;
 Before_Calls:=Model.Calls; Before_Frees:=Model.Frees;
 S.Submit(77,Data,ID,R); S.Confirm(R); S.Advance_Cleanup;
 Check(Model.Calls=Before_Calls and Model.Frees=Before_Frees);
 Check(Model.Calls<=2 and not R.Working);
 end Scenario;
begin
 for Mode in 1 .. 60 loop Scenario(Mode); end loop;
 for Mode in 1 .. 3 loop
 declare
 function Boundary return Boolean is (True);
 package S is new ACPI_Native_GPIO_Admission(Boundary);
 use type S.Outcome;
 Data : P.Request; R : S.Report; Before_Clock : Natural;
 begin
 Model.Reset; Model.Mode:=33; Model.Confirmed:=Mode/=2; Model.Revoke_OK:=Mode/=3;
 Data.Spans:=1; Data.Segments:=2; Data.Path(1):="_SB_"; Data.Path(2):="GPI0";
 Data.HID:="INTC1057"; Data.Table:="DSDT"; Data.Table_ID:="TESTGPIO"; Data.Epoch:=16#1_0000_0007#; Data.STA:=15;
 Data.Items(1):=(Base=>16#F000_0000#,Length=>4096,From=>P.Memory32_Fixed,Writable=>True);
 S.Submit(77,Data,16#1_0000_0011#,R);
 Check(R.State=S.Completion_Unknown and Model.Calls=1 and Model.Frees=0);
 Before_Clock:=Model.Clock_Reads;
 S.Abandon_Confirmation; R:=S.Observe;
 Check(R.Confirmation_Attempted and not R.Working and Model.Calls=1);
 Check(Model.Clock_Reads=Before_Clock and not S.Quiet and not R.Reply_Known);
 if Mode=2 then
 Check(R.Grant_Pending and R.Memory_Owned and Model.Frees=0);
 Model.Confirmed:=True; S.Advance_Cleanup; R:=S.Observe;
 end if;
 Check(not R.Grant_Pending and not R.Memory_Owned and Model.Frees=1);
 S.Confirm(R); S.Submit(77,Data,16#1_0000_0011#,R); S.Abandon_Confirmation;
 Check(Model.Calls=1 and Model.Frees=1 and not S.Quiet);
 end;
 end loop;
 Ada.Text_IO.Put_Line("GPIO sender checks" & Checks'Image);
end Sender_Tests;