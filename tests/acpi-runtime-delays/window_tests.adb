with Ada.Text_IO;
with ACPI_Delay_Domains;
with ACPI_Native_Delays;
with AML_Delays;
with Delay_Test_Clock;
procedure Window_Tests is
   package D renames ACPI_Delay_Domains;
   package N renames ACPI_Native_Delays;
   package C renames Delay_Test_Clock;
   use type D.Tick, D.Phase, AML_Delays.Outcome;
   Checks : Natural := 0;
   Accepted : Boolean;
   Deadline : D.Tick;
   Result : AML_Delays.Outcome;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Scenario in 1 .. 7 loop
      declare
         S : D.State;
      begin
         D.Inspect_Window (S, 10, True, Deadline, Accepted);
         Check (not Accepted and Deadline = 0 and D.Current (S) = D.Fresh);
         D.Start (S, 10, 100, True, Accepted); Check (Accepted);
         D.Inspect_Window (S, 20, True, Deadline, Accepted);
         Check (Accepted and Deadline = 100 and D.Deadline (S) = 100 and not D.Working (S));
         D.Inspect_Window (S, 30, True, Deadline, Accepted);
         Check (Accepted and Deadline = 100 and D.Current (S) = D.Startup_Active);
         case Scenario is
            when 1 =>
               D.Enter_Runtime (S, 29, True, Accepted);
               Check (not Accepted and D.Current (S) = D.Stopped);
            when 2 .. 4 =>
               D.Inspect_Window (S, (if Scenario = 2 then 100 elsif Scenario = 3 then 29 else 31),
                 Scenario /= 4, Deadline, Accepted);
               Check (not Accepted and Deadline = 0 and D.Current (S) = D.Stopped);
            when 5 =>
               D.Begin_Wait (S, Deadline, Accepted);
               Check (Accepted and Deadline = 100 and D.Working (S));
               D.Inspect_Window (S, 31, True, Deadline, Accepted);
               Check (not Accepted and Deadline = 0 and D.Working (S) and D.Current (S) = D.Stopped);
               D.End_Wait (S, 31, 32, True, Accepted);
               Check (not Accepted and not D.Working (S));
            when 6 =>
               D.Enter_Runtime (S, 31, True, Accepted); Check (Accepted);
               D.Inspect_Window (S, 32, True, Deadline, Accepted);
               Check (not Accepted and Deadline = 0 and D.Current (S) = D.Runtime_Idle);
               D.Begin_Event (S, 40, 200, True, Accepted); Check (Accepted);
               D.Inspect_Window (S, 50, True, Deadline, Accepted);
               Check (Accepted and Deadline = 200 and D.Deadline (S) = 200);
               D.End_Event (S, 51, True, Accepted); Check (Accepted);
               D.Inspect_Window (S, 52, True, Deadline, Accepted);
               Check (not Accepted and Deadline = 0 and D.Current (S) = D.Runtime_Idle);
            when 7 =>
               D.Stop (S); Check (D.Current (S) = D.Stopped);
               D.Inspect_Window (S, 40, True, Deadline, Accepted);
               Check (not Accepted and Deadline = 0 and not D.Working (S));
         end case;
      end;
   end loop;
   N.Inspect_Window (Deadline, Accepted);
   Check (not Accepted and Deadline = 0 and C.Sleeps = 0 and C.Yields = 0);
   N.Begin_Startup (10_000, Accepted); Check (Accepted);
   C.Now := 20; N.Inspect_Window (Deadline, Accepted);
   Check (Accepted and Deadline = 10_000 and C.Sleeps = 0 and C.Yields = 0);
   C.Now := 30; N.Inspect_Window (Deadline, Accepted);
   Check (Accepted and Deadline = 10_000);
   C.Now := 31; N.Enter_Runtime (Accepted); Check (Accepted);
   C.Now := 32; N.Inspect_Window (Deadline, Accepted);
   Check (not Accepted and Deadline = 0);
   C.Now := 40; N.Begin_Event (20_000, Accepted); Check (Accepted);
   C.Now := 50; N.Inspect_Window (Deadline, Accepted);
   Check (Accepted and Deadline = 20_000);
   C.Now := 49; N.Inspect_Window (Deadline, Accepted);
   Check (not Accepted and Deadline = 0);
   C.Now := 60; N.Begin_Event (30_000, Accepted); Check (not Accepted);
   N.Begin_Startup (40_000, Accepted); Check (not Accepted);
   N.Perform ((AML_Delays.Stall_Delay, 0), Result);
   Check (Result = AML_Delays.Unavailable and C.Sleeps = 0 and C.Yields = 0);
   Ada.Text_IO.Put_Line ("Delay window observation checks" & Checks'Image);
end Window_Tests;
