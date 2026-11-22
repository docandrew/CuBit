--  Hosted tests for Desktop_Launch_Notice: refusal and early-end notices,
--  their words, and the bounded watch of launched children.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with Desktop_Launch_Notice; use Desktop_Launch_Notice;

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package CE renames CuBit.Child_Exits;
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  Distinct identities for the watch (PID in the low half).
   function Child (N : Positive) return Process_ID is (From_Word (Unsigned_64 (N)));

   N : Notice;
   T : Watch_Table;
   Shown : Boolean;
begin
   Show_Refusal (N, "Penny", LA.Startup_Table_Missing);
   Check (N.Why = Refused and then Title (N) = "Penny could not start", "refusal title");
   Check (Name_Of (N) = "Penny", "the app's name");
   Check (Detail (N) = "Reason: it has no startup table (rebuild it).", "refusal detail");

   --  Names longer than a menu label are cut, never overrun.
   Show_Refusal (N, [1 .. 40 => 'x'], LA.Executable_Not_Found);
   Check (N.Length = Maximum_Name, "long name cut to the label bound");

   --  A child that opens a window started: its later end is not a failure.
   Watch (T, Child (10), "Files");
   Window_Opened (T, Child (10));
   Ended (T, N, (Process => Child (10), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "an end after a window is not a startup failure");

   --  Stopped before a window: crashed or killed while starting.
   Watch (T, Child (11), "Penny");
   Ended (T, N, (Process => Child (11), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown and then Title (N) = "Penny stopped while starting", "early stop title");
   Check (Detail (N) = "It was stopped (crashed or killed) before it opened a window.",
          "early stop detail");
   --  Its watch ended: a second report is not shown again.
   Ended (T, N, (Process => Child (11), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "one report per watched child");

   --  Exit code 0 without a window is a normal end; nonzero is a failure.
   Watch (T, Child (12), "Tool");
   Ended (T, N, (Process => Child (12), Kind => CE.Exited, Code => 0), Shown);
   Check (not Shown, "a clean exit is not a failure");
   Watch (T, Child (13), "Tool");
   Ended (T, N, (Process => Child (13), Kind => CE.Exited, Code => 3), Shown);
   Check (Shown and then Detail (N) = "It exited with code 3 before it opened a window.",
          "nonzero exit detail");

   --  Unknown processes (not launched from the menu) are ignored.
   Ended (T, N, (Process => Child (99), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "unwatched children are not reported");

   --  Bounded: past capacity the oldest watch is replaced, others kept.
   for K in 1 .. Watch_Capacity + 1 loop
      Watch (T, Child (100 + K), "App");
   end loop;
   Ended (T, N, (Process => Child (100 + Watch_Capacity + 1), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown, "the newest watch is kept when full");
   Ended (T, N, (Process => Child (102), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown, "other watches survive a replacement");

   --  Every reason has a detail line.
   for Reason in LA.Launch_Failure loop
      Show_Refusal (N, "App", Reason);
      Check (Detail (N)'Length > String'("Reason: .")'Length, "detail for " & Reason'Image);
   end loop;

   if Failures = 0 then
      Put_Line ("desktop-launch-notice:" & Checks'Image & " checks PASS");
   else
      Put_Line ("desktop-launch-notice:" & Failures'Image & " of" & Checks'Image &
                " checks FAILED");
      raise Program_Error;
   end if;
end Main;
