with Ada.Text_IO;
with Interfaces;
with ACPI_EC_Types;
with ACPI_EC_Clock_Policy;
with ACPI_EC_Clock_Adapter;
procedure EC_Clock_Tests is
   package T renames ACPI_EC_Types;
   package P renames ACPI_EC_Clock_Policy;
   use type T.Microseconds, T.Work_Budget, Interfaces.Unsigned_64;
   use type P.Sample_Status, P.Pause_Outcome;
   Raw : Interfaces.Unsigned_64 := 0;
   Ready, Accept_Pause : Boolean := True;
   Reads, Pauses, Checks : Natural := 0;
   Last_Pause : Interfaces.Unsigned_64 := 0;
   procedure Check (Condition : Boolean) is
   begin Checks := Checks + 1; if not Condition then raise Program_Error with "EC clock check" & Checks'Image; end if; end Check;
   procedure Read_Raw (Value : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Reads := Reads + 1; Value := Raw; Available := Ready; end Read_Raw;
   procedure Pause_Raw (Until_Time : Interfaces.Unsigned_64; Accepted : out Boolean) is
   begin Pauses := Pauses + 1; Last_Pause := Until_Time; Accepted := Accept_Pause; end Pause_Raw;
   package Clock is new ACPI_EC_Clock_Adapter (Read_Raw, Pause_Raw);
   procedure Reading (Value : Interfaces.Unsigned_64; Present : Boolean; Expected : P.Sample_Status) is
      Time : T.Microseconds;
      Available : Boolean;
   begin
      Raw := Value; Ready := Present;
      Clock.Read (Time, Available);
      Check (Clock.Last_Status = Expected and Available = (Expected = P.Sampled));
      Check ((if Available then Interfaces.Unsigned_64 (Time) = Value else Time = 0));
   end Reading;
begin
   Reading (0, True, P.Sampled);
   Reading (Interfaces.Unsigned_64 (T.Microseconds'Last), True, P.Sampled);
   Reading (Interfaces.Unsigned_64 (T.Microseconds'Last) + 1, True, P.Representation_Limit);
   Reading (Interfaces.Unsigned_64'Last, True, P.Representation_Limit);
   Reading (Interfaces.Unsigned_64'Last, False, P.Unavailable);
   for Start in T.Microseconds range 0 .. 16 loop
      for Distance in T.Microseconds range 1 .. 2_001 loop
         declare
            Remaining : T.Work_Budget := 2;
            Result : P.Pause_Report;
            Before : constant Natural := Pauses;
            Expected : constant T.Microseconds := Start + T.Microseconds'Min (Distance, P.Default_Maximum_Slice);
         begin
            Raw := Interfaces.Unsigned_64 (Start); Ready := True;
            Clock.Pause_Until (Start + Distance, Remaining, Result);
            Check (Remaining = 1 and Result.Status = P.Pause_Returned);
            Check (Result.Requested = Expected and Last_Pause = Interfaces.Unsigned_64 (Expected) and Pauses = Before + 1);
         end;
      end loop;
   end loop;
   for Distance in T.Microseconds range 1 .. 1_001 loop
      declare
         Remaining : T.Work_Budget := 1;
         Result : P.Pause_Report;
      begin
         Raw := Interfaces.Unsigned_64 (T.Microseconds'Last - Distance);
         Clock.Pause_Until (T.Microseconds'Last, Remaining, Result);
         Check (Remaining = 0 and Result.Status = P.Pause_Returned);
         Check (Result.Requested > T.Microseconds (Raw)
           and Result.Requested - T.Microseconds (Raw) <= P.Default_Maximum_Slice);
      end;
   end loop;
   declare
      Remaining : T.Work_Budget := 0;
      Result : P.Pause_Report;
      Before_Reads : Natural := Reads;
      Before_Pauses : Natural := Pauses;
   begin
      Clock.Pause_Until (1, Remaining, Result);
      Check (Result.Status = P.Budget_Exhausted and Reads = Before_Reads and Pauses = Before_Pauses);
      Remaining := 1; Raw := 10;
      Clock.Pause_Until (10, Remaining, Result);
      Check (Result.Status = P.Already_Due and Result.Requested = 0 and Remaining = 0 and Pauses = Before_Pauses);
      Remaining := 1; Ready := False;
      Clock.Pause_Until (11, Remaining, Result);
      Check (Result.Status = P.Clock_Unavailable and Remaining = 0 and Pauses = Before_Pauses);
      Remaining := 1; Ready := True; Raw := Interfaces.Unsigned_64'Last;
      Clock.Pause_Until (11, Remaining, Result);
      Check (Result.Status = P.Clock_Representation_Limit and Remaining = 0 and Pauses = Before_Pauses);
      Raw := 10; Accept_Pause := False; Remaining := 1;
      Clock.Pause_Until (11, Remaining, Result);
      Check (Result.Status = P.Pause_Refused and Remaining = 0 and Result.Requested = 11);
      Accept_Pause := True;
      --  A stopped clock never consumes the engine's transaction budget here.
      --  The independent pause episode must therefore run out explicitly.
      Remaining := 3; Before_Reads := Reads; Before_Pauses := Pauses;
      for Attempt in 1 .. 3 loop
         Clock.Pause_Until (11, Remaining, Result);
         Check (Result.Status = P.Pause_Returned and Result.Requested = 11);
      end loop;
      Check (Remaining = 0 and Reads = Before_Reads + 3 and Pauses = Before_Pauses + 3);
      Clock.Pause_Until (11, Remaining, Result);
      Check (Result.Status = P.Budget_Exhausted and Reads = Before_Reads + 3 and Pauses = Before_Pauses + 3);
   end;
   Ada.Text_IO.Put_Line ("EC clock checks:" & Checks'Image);
end EC_Clock_Tests;
