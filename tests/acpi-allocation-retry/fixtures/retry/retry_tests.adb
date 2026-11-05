with Ada.Text_IO;
with AML_Allocation_Retry;
procedure Retry_Tests is
   First_Refuses, Second_Refuses : Boolean := False;
   Calls, Collections, Queries : Natural := 0;
   Refused : Boolean := False;
   procedure Attempt (Collect_Before : Boolean) is
   begin
      Calls:=Calls+1;
      pragma Assert (Calls<=2);
      pragma Assert (Collect_Before=(Calls=2));
      if Collect_Before then Collections:=Collections+1; end if;
      Refused:=(if Calls=1 then First_Refuses else Second_Refuses);
   end Attempt;
   function Capacity_Refused return Boolean is
   begin Queries:=Queries+1; return Refused; end Capacity_Refused;
   package R is new AML_Allocation_Retry(Attempt,Capacity_Refused);
   Report : R.Report;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin Checks:=Checks+1; if not B then raise Program_Error with Checks'Image; end if; end Check;
begin
   for First in Boolean loop
      for Second in Boolean loop
         First_Refuses:=First; Second_Refuses:=Second;
         Calls:=0; Collections:=0; Queries:=0;
         R.Run(Report);
         Check(Calls=(if First then 2 else 1));
         Check(Collections=(if First then 1 else 0));
         Check(Queries=1);
         Check(Report.Retried=First);
         Check(Natural(Report.Attempts)=Calls);
         Check(Refused=(First and Second));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line("BOUNDED ALLOCATION RETRY" & Checks'Image);
end Retry_Tests;
