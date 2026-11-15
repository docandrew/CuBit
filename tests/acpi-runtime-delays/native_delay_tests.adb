with Ada.Command_Line; with Ada.Text_IO; with ACPI_Native_Delays;
with AML_Delays; with Delay_Test_Clock; with Interfaces;
procedure Native_Delay_Tests is
 use type AML_Delays.Outcome, Interfaces.Unsigned_64;
 package D renames ACPI_Native_Delays; package C renames Delay_Test_Clock;
 Checks : Natural := 0; Accepted : Boolean; Result : AML_Delays.Outcome;
 Mode : constant String := Ada.Command_Line.Argument (1);
 procedure Check (OK : Boolean) is begin Checks := Checks + 1;
 if not OK then raise Program_Error with Mode & Checks'Image; end if; end Check;
 procedure Perform_Zero is begin D.Perform ((AML_Delays.Stall_Delay, 0), Result); end Perform_Zero;
begin
 if Mode = "clock-unavailable" then
  C.Available := False; D.Begin_Startup (10000, Accepted); Check (not Accepted);
  C.Available := True; D.Begin_Startup (10000, Accepted); Check (not Accepted);
  Perform_Zero; Check (Result = AML_Delays.Unavailable);
 elsif Mode = "startup-expiry" then
  D.Begin_Startup (10000, Accepted); Check (Accepted);
  C.Now := 10000; D.Enter_Runtime (Accepted); Check (not Accepted);
  D.Begin_Event (20000, Accepted); Check (not Accepted);
 elsif Mode = "clock-reversed" or Mode = "clock-reversed-catchup" then
  D.Begin_Startup (10000, Accepted); Check (Accepted); Perform_Zero; Check (Result = AML_Delays.Completed);
  C.Now := 9;
  if Mode = "clock-reversed-catchup" then D.Perform ((AML_Delays.Sleep_Delay, 1), Result);
  else Perform_Zero; end if;
  Check (Result = AML_Delays.Failed);
  C.Now := 20; D.Enter_Runtime (Accepted); Check (not Accepted);
 else
  D.Begin_Startup (10000, Accepted); Check (Accepted);
  D.Begin_Startup (99999, Accepted); Check (not Accepted);
  Perform_Zero; Check (Result = AML_Delays.Completed);
  D.Enter_Runtime (Accepted); Check (Accepted);
  Perform_Zero; Check (Result = AML_Delays.Unavailable and C.Sleeps = 0);
  C.Now := 20000; D.Begin_Event (30000, Accepted); Check (Accepted);
  D.Begin_Event (99999, Accepted); Check (not Accepted);
  if Mode = "reentrant" then C.Reenter := True;
  elsif Mode = "stop-during-wait" then C.Close_In_Sleep := True;
  elsif Mode /= "normal" then raise Program_Error with "unknown mode"; end if;
  D.Perform ((AML_Delays.Sleep_Delay, 1), Result);
  if Mode = "normal" then
   Check (Result = AML_Delays.Completed and C.Now = 21000 and C.Sleeps = 1 and C.Yields = 1);
   D.End_Event (Accepted); Check (Accepted);
   D.End_Event (Accepted); Check (not Accepted);
   C.Now := 31000; D.Begin_Event (32000, Accepted); Check (Accepted);
   D.Begin_Event (99999, Accepted); Check (not Accepted);
   D.Perform ((AML_Delays.Sleep_Delay, 1), Result); Check (Result = AML_Delays.Failed);
  else Check (Result = AML_Delays.Failed); end if;
  C.Now := 40000; D.End_Event (Accepted); Check (not Accepted);
  D.Begin_Event (50000, Accepted); Check (not Accepted);
  D.Begin_Startup (50000, Accepted); Check (not Accepted);
  Perform_Zero; Check (Result = AML_Delays.Unavailable);
 end if;
 Ada.Text_IO.Put_Line ("Native delay " & Mode & " checks" & Checks'Image);
end Native_Delay_Tests;
