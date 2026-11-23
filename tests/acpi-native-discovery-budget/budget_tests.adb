with Ada.Text_IO;with Interfaces;with ACPI_Native_Discovery_Budget;
procedure Budget_Tests is
 package B renames ACPI_Native_Discovery_Budget;use type Interfaces.Unsigned_64;
 D:Interfaces.Unsigned_64;A:Boolean;Checks:Natural:=0;
 procedure Check(Good:Boolean) is begin Checks:=Checks+1;if not Good then raise Program_Error with Checks'Image;end if;end;
 procedure Probe(Now:Interfaces.Unsigned_64;Known:Boolean;Expected:Boolean) is
 begin
  B.Make_Deadline(Now,Known,D,A);Check(A=Expected);
  if A then Check(D>Now and D-Now=5_000_000 and D/=Interfaces.Unsigned_64'Last);
  else Check(D=0);end if;
 end;
 Threshold:constant Interfaces.Unsigned_64:=Interfaces.Unsigned_64'Last-5_000_000;
begin
 for I in 0..1000 loop Probe(Interfaces.Unsigned_64(I),True,True);Probe(Interfaces.Unsigned_64(I),False,False);end loop;
 for I in 0..64 loop
  Probe(Threshold-1-Interfaces.Unsigned_64(I),True,True);
  Probe(Threshold+Interfaces.Unsigned_64(I),True,False);
  Probe(Interfaces.Unsigned_64'Last-Interfaces.Unsigned_64(I),False,False);
 end loop;
 Probe(Interfaces.Unsigned_64'Last,True,False);Probe(0,False,False);
 Ada.Text_IO.Put_Line("Native discovery budget checks"&Checks'Image);
end Budget_Tests;
