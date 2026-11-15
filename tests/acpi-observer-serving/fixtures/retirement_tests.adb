pragma Ada_2022;
with ACPI_Local_Shutdown;with ACPI_Serving_State;with Interfaces;with Ada.Text_IO;
procedure Retirement_Tests is
 subtype Word is Interfaces.Unsigned_64;use type Word;
 package P renames ACPI_Serving_State;use type P.Mode;
 Checks:Natural:=0;
 procedure Check(OK:Boolean) is begin Checks:=Checks+1;if not OK then raise Program_Error with Checks'Image;end if;end;
 Close_OK,Fields_Done,HW_Idle,Others_Idle,Continues:Boolean:=False;
 Closes,Local_Closes,HW_Stops,Maintains,Cleanups,Views:Natural:=0;
 HW_Deadline:constant Word:=16#8000_0000_0000_0017#;
 Other_Deadline:constant Word:=16#8000_0000_0000_0019#;
 procedure Close_Consumers(Accepted:out Boolean) is begin Closes:=Closes+1;Accepted:=Close_OK;end;
 procedure Close_Local is begin Local_Closes:=Local_Closes+1;end;
 procedure Stop_HW is begin HW_Stops:=HW_Stops+1;end;
 procedure Maintain_HW(Wake:out Word;May_Continue:out Boolean) is begin Maintains:=Maintains+1;Wake:=HW_Deadline;May_Continue:=Continues;end;
 procedure Cleanup(Completed:out Boolean) is begin Cleanups:=Cleanups+1;Completed:=Fields_Done;end;
 function Hardware_May_Exit return Boolean is(HW_Idle);
 procedure Other_View(Idle:out Boolean;Wake:out Word) is begin Views:=Views+1;Idle:=Others_Idle;Wake:=Other_Deadline;end;
 package C is new ACPI_Local_Shutdown(Close_Consumers,Close_Local,Stop_HW,Maintain_HW,Cleanup,Hardware_May_Exit,Other_View);
 use type C.Phase;
 function Bit(N,Position:Natural) return Boolean is((N/2**Position) mod 2=1);
 Accepted,Selected:Boolean;Facts:constant P.Eligibility:=(others=>True);
 begin
 for N in 0..15 loop for Closed in Boolean loop
 declare Run:C.Context;Mode:P.Session;Ready:Boolean;Before_Cleanup,Before_Maintain:Natural;begin
 Close_OK:=Closed;Fields_Done:=Bit(N,0);HW_Idle:=Bit(N,1);Others_Idle:=Bit(N,2);Continues:=Bit(N,3);
 Closes:=0;Local_Closes:=0;HW_Stops:=0;Maintains:=0;Cleanups:=0;Views:=0;
 C.Request_Stop(Run,Accepted);Check(Accepted=Closed and Closes=1 and not C.May_Retire(Run));
 Check(Local_Closes=(if Closed then 1 else 0) and HW_Stops=Local_Closes);
 C.Advance(Run);Ready:=Closed and Fields_Done and HW_Idle and Others_Idle;
 Check(C.May_Retire(Run)=Ready);
 Check(C.Observe(Run).Stage=(if not Closed then C.Refused elsif Ready then C.Quiescent elsif not HW_Idle or not Others_Idle then C.Waiting else C.Retained));
 Check(Maintains=(if Closed then 1 else 0) and Cleanups=Maintains and Views=Maintains);
 P.Select_Observer(Mode,Facts,C.May_Retire(Run),Selected);Check(Selected=Ready and P.Observe(Mode).Current=(if Ready then P.Observer_Only else P.Not_Serving));
 if Closed and not Ready then
 Check(C.Observe(Run).Wake_Deadline_Milliseconds=(if not HW_Idle then HW_Deadline elsif not Others_Idle then Other_Deadline else C.No_Deadline));
 end if;
 Before_Cleanup:=Cleanups;Before_Maintain:=Maintains;
 C.Request_Stop(Run,Accepted);Check(Accepted=Closed and Closes=1 and Local_Closes=(if Closed then 1 else 0));
 Fields_Done:=True;HW_Idle:=True;Others_Idle:=True;C.Advance(Run);
 Check(C.May_Retire(Run)=Closed);
 Check(Cleanups=Before_Cleanup+(if Closed and not Bit(N,0) then 1 else 0));
 Check(Maintains=Before_Maintain+(if Closed and not Ready then 1 else 0));
 P.Select_Runtime(Mode,True,Selected);Check(not Selected);
 end;end loop;end loop;
 Ada.Text_IO.Put_Line("Observer retirement checks"&Checks'Image);
end Retirement_Tests;
