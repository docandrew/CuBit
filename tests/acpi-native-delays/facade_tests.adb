pragma Ada_2022;
with Ada.Text_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with AML_Delays;
with ACPI_Native_Delays;
with Delay_Model;
procedure Facade_Tests is
   use type AML_Delays.Outcome;
   package D renames ACPI_Native_Delays;
   package M renames Delay_Model;
   Mode : constant String := Ada.Command_Line.Argument (1);
   Item : AML_Delays.Request := (AML_Delays.Stall_Delay, 10);
   Result : AML_Delays.Outcome;
   Expected : AML_Delays.Outcome := AML_Delays.Completed;
   Deadline : Unsigned_64 := 10_000;
   Accepted : Boolean;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Why : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Why; end if; end Check;
   function Sleep_Mode return Boolean is
     (Mode in "sleep" | "zero-sleep" | "stopped-sleep" | "late" | "refuse-yield" | "refuse-sleep");
   Before : Natural;
begin
   D.Perform (Item, Result);
   Check (Result = AML_Delays.Unavailable and M.Reads = 0 and M.Yields = 0 and M.Sleeps = 0,
     "unopened owner never invokes the clock or scheduler");
   if Sleep_Mode then Item := (AML_Delays.Sleep_Delay, 1); end if;
   if Mode = "zero-sleep" then Item := (AML_Delays.Sleep_Delay, 0);
   elsif Mode = "zero-stall" then Item := (AML_Delays.Stall_Delay, 0);
   elsif Mode = "unavailable" then M.Available := False; Expected := AML_Delays.Unavailable;
   elsif Mode = "regress" then M.Now := 100; M.Step := 0; M.Regress := True; Expected := AML_Delays.Failed;
   elsif Mode in "stopped-stall" | "stopped-sleep" then M.Stopped := True; Expected := AML_Delays.Failed;
   elsif Mode = "late" then M.Late := True; Expected := AML_Delays.Failed;
   elsif Mode = "refuse-yield" then M.Refuse_Yield := True; Expected := AML_Delays.Failed;
   elsif Mode = "refuse-sleep" then M.Refuse_Sleep := True; Expected := AML_Delays.Failed;
   elsif Mode = "overflow" then M.Now := Unsigned_64'Last - 5; M.Step := 0; Deadline := Unsigned_64'Last; Expected := AML_Delays.Failed;
   elsif Mode = "expired" then M.Now := Deadline; Expected := AML_Delays.Failed;
   end if;
   D.Begin_Startup (Deadline, Accepted); Check (Accepted, "native owner opens once");
   D.Begin_Startup (Unsigned_64'Last, Accepted); Check (not Accepted, "active deadline cannot be extended");
   D.Perform (Item, Result); Check (Result = Expected, "native typed result " & Result'Image);
   if not Sleep_Mode then Check (M.Yields = 0 and M.Sleeps = 0, "Stall invokes no scheduling syscall");
   elsif Mode = "refuse-yield" then Check (M.Yields = 1 and M.Sleeps = 0, "failed yield is not replayed or followed by sleep");
   else Check (M.Yields = M.Sleeps and M.Sleeps > 0, "Sleep always yields, including zero duration");
   end if;
   if Mode = "stopped-stall" then Check (M.Reads = 65_536, "finite busy-read budget");
   elsif Mode = "stopped-sleep" then Check (M.Reads = 4_096 and M.Sleeps = 4_095, "finite wake budget");
   end if;
   Before := M.Reads + M.Yields + M.Sleeps;
   if Expected /= AML_Delays.Completed then
      D.Perform ((AML_Delays.Stall_Delay, 0), Result);
      Check (Result = AML_Delays.Unavailable and M.Reads + M.Yields + M.Sleeps = Before,
        "failure closes domain rather than resetting its budget");
   end if;
   D.Close_Startup;
   D.Perform (Item, Result);
   Check (Result = AML_Delays.Unavailable and M.Reads + M.Yields + M.Sleeps = Before,
     "observer phase cannot execute native delay work");
   D.Begin_Startup (Unsigned_64'Last, Accepted); Check (not Accepted, "closed epoch cannot be reopened");
   Ada.Text_IO.Put_Line ("Native delay facade checks" & Checks'Image);
end Facade_Tests;
