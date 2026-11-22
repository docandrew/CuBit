pragma Ada_2022;
with ACPI_Serving_State;with Ada.Text_IO;
procedure Policy_Tests is
 package P renames ACPI_Serving_State;use type P.Mode,P.Reason,P.Snapshot;
 Checks:Natural:=0;procedure Check(OK:Boolean) is begin Checks:=Checks+1;if not OK then raise Program_Error with Checks'Image;end if;end;
 Gate_Count:constant:=15;All_Gates:constant:=2**Gate_Count-1;
 function Bit(Value,Position:Natural) return Boolean is((Value/2**Position) mod 2=1);
 function Facts(N:Natural) return P.Eligibility is
 ((Bit(N,0),Bit(N,1),Bit(N,2),Bit(N,3),Bit(N,4),Bit(N,5),Bit(N,6),Bit(N,7),Bit(N,8),Bit(N,9),Bit(N,10),Bit(N,11),Bit(N,12),Bit(N,13),Bit(N,14)));
 Accepted:Boolean;
 begin
 for N in 0..All_Gates loop for Retired in Boolean loop
 declare Run:P.Session;Before:P.Snapshot;begin
 P.Select_Observer(Run,Facts(N),Retired,Accepted);Check(Accepted=(N=All_Gates and Retired));
 Before:=P.Observe(Run);Check(Before.Consumed and (Before.Selected=Accepted));
 Check(Before.Current=(if Accepted then P.Observer_Only else P.Not_Serving));
 Check(Before.Why=(if Accepted then P.Runtime_Not_Admitted else P.None));
 P.Select_Runtime(Run,True,Accepted);Check(not Accepted and P.Observe(Run)=Before);
 P.Select_Observer(Run,Facts(All_Gates),True,Accepted);Check(not Accepted and P.Observe(Run)=Before);
 end;end loop;end loop;
 for Admitted in Boolean loop declare Run:P.Session;Before:P.Snapshot;begin
 P.Select_Runtime(Run,Admitted,Accepted);Check(Accepted=Admitted);Before:=P.Observe(Run);
 Check(Before.Consumed and (Before.Selected=Admitted) and Before.Current=(if Admitted then P.Local_Runtime else P.Not_Serving) and Before.Why=P.None);
 P.Select_Observer(Run,Facts(All_Gates),True,Accepted);Check(not Accepted and P.Observe(Run)=Before);
 end;end loop;
 Ada.Text_IO.Put_Line("Observer serving policy checks"&Checks'Image);
end Policy_Tests;
