with Ada.Text_IO; with ACPI_Delay_Domains;
procedure Domain_Tests is
 package D renames ACPI_Delay_Domains; use type D.Tick, D.Phase;
 Checks : Natural := 0;
 procedure Check (OK : Boolean) is begin Checks := Checks + 1;
  if not OK then raise Program_Error with Checks'Image; end if; end Check;
begin
 for Scenario in 1 .. 14 loop
  declare S : D.State; Accepted : Boolean; Until_Time : D.Tick;
  begin
   D.Begin_Event (S, 10, 20, True, Accepted); Check (not Accepted and D.Current (S) = D.Fresh);
   D.End_Wait (S, 10, 11, True, Accepted); Check (not Accepted and not D.Working (S));
   if Scenario in 1 .. 4 then
    D.Start (S, 10, (case Scenario is when 1 => 0, when 2 => 10,
      when 3 => D.Tick'Last, when others => 20), Scenario /= 4, Accepted);
    Check (not Accepted and D.Current (S) = D.Stopped and D.Deadline (S) = 0);
    D.Start (S, 10, 30, True, Accepted); Check (not Accepted);
   else
    D.Start (S, 10, 20, True, Accepted); Check (Accepted);
    D.Start (S, 10, 90, True, Accepted); Check (not Accepted and D.Deadline (S) = 20);
    if Scenario in 5 .. 7 then
     D.Enter_Runtime (S, (if Scenario = 5 then 9 elsif Scenario = 6 then 20 else 10),
       Scenario /= 7, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
    else
     D.Enter_Runtime (S, 11, True, Accepted); Check (Accepted and D.Deadline (S) = 0);
     D.Begin_Wait (S, Until_Time, Accepted); Check (not Accepted and Until_Time = 0);
     if Scenario = 8 then
      D.Begin_Event (S, 10, 30, True, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
     elsif Scenario = 9 then
      D.Begin_Event (S, 30, 30, True, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
     else
      D.Begin_Event (S, 30, 40, True, Accepted); Check (Accepted);
      D.Begin_Event (S, 31, 90, True, Accepted); Check (not Accepted and D.Deadline (S) = 40);
      D.Begin_Wait (S, Until_Time, Accepted); Check (Accepted and Until_Time = 40 and D.Working (S));
      D.End_Event (S, 31, True, Accepted); Check (not Accepted and D.Working (S));
      if Scenario = 10 then
       D.Stop (S); Check (D.Working (S) and D.Current (S) = D.Stopped);
       D.End_Wait (S, 30, 31, True, Accepted); Check (not Accepted and not D.Working (S));
      elsif Scenario = 11 then
       D.Begin_Wait (S, Until_Time, Accepted); Check (not Accepted and D.Working (S) and D.Current (S) = D.Stopped);
       D.End_Wait (S, 30, 31, True, Accepted); Check (not Accepted and not D.Working (S));
      elsif Scenario = 12 then
       D.End_Wait (S, 29, 31, True, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
      elsif Scenario = 13 then
       D.End_Wait (S, 31, 30, True, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
      else
       D.End_Wait (S, 30, 39, True, Accepted); Check (Accepted and not D.Working (S));
       D.End_Event (S, 40, True, Accepted); Check (not Accepted and D.Current (S) = D.Stopped);
      end if;
     end if;
    end if;
   end if;
   D.Stop (S); D.Begin_Event (S, 50, 60, True, Accepted); Check (not Accepted);
   D.Enter_Runtime (S, 50, True, Accepted); Check (not Accepted);
  end;
 end loop;
 Ada.Text_IO.Put_Line ("Delay domain boundary checks" & Checks'Image);
end Domain_Tests;