pragma Ada_2022;
with Ada.Text_IO;
with HID_Reset_Policy;
procedure Policy_Tests is
   package P renames HID_Reset_Policy;
   use type P.Word;
   Checks:Natural:=0;
   procedure Check(OK:Boolean) is
   begin Checks:=Checks+1;if not OK then raise Program_Error with "reset policy";end if;end Check;
   procedure Deadline(Now,Overall,Expected:P.Word;Good:Boolean) is
      Until_Time:P.Word;Accepted:Boolean;
   begin P.Ack_Deadline(Now,Overall,Until_Time,Accepted);Check(Accepted=Good and then Until_Time=Expected);end Deadline;
begin
   Deadline(0,10000,1000,True);Deadline(1,10000,1001,True);
   Deadline(100,1100,1100,True);Deadline(100,1099,1099,True);
   Deadline(100,101,101,True);Deadline(100,100,0,False);
   Deadline(101,100,0,False);Deadline(0,0,0,False);
   Deadline(0,P.Word'Last,0,False);Deadline(P.Word'Last,P.Word'Last-1,0,False);
   Deadline(P.Word'Last-2000,P.Word'Last-1,P.Word'Last-1000,True);
   Deadline(P.Word'Last-1001,P.Word'Last-1,P.Word'Last-1,True);
   Deadline(P.Word'Last-2,P.Word'Last-1,P.Word'Last-1,True);
   Check(P.Elan(16#04F3#)and not P.Elan(0)and not P.Elan(16#FFFF#));
   Check(P.Bogus_IRQ(16#04F3#,16#FF#,16#FF#));
   Check(not P.Bogus_IRQ(16#04F3#,0,16#FF#));
   Check(not P.Bogus_IRQ(16#04F3#,16#FF#,0));
   Check(not P.Bogus_IRQ(16#04F2#,16#FF#,16#FF#));
   Ada.Text_IO.Put_Line("HID reset policy checks"&Checks'Image);
end Policy_Tests;
