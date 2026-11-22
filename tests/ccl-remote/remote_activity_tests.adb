--  The ccl-remote activity ring (userspace/ccl/remote/remote_activity):
--  the window's list of what the paired host did. Checks the shown
--  argument (one line, cut with an ellipsis), the ring's growth from
--  Initial_Entries to the named ceiling, and that past the ceiling the
--  oldest entries give way, newest last.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Remote_Activity; use Remote_Activity;

procedure Remote_Activity_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;
begin
   --  Shown: one printable line.
   Check (Shown ("(proc.list)") = "(proc.list)", "short text unchanged");
   Check (Shown ("  (+ 1" & ASCII.LF & ASCII.HT & "  2)  ") = "(+ 1 2)", "controls and runs become one space");
   declare
      Long : constant String (1 .. 4 * Maximum_Argument_Shown) := [others => 'x'];
      Cut : constant String := Shown (Long);
   begin
      Check (Cut'Length = Maximum_Argument_Shown, "a long argument is cut to the maximum");
      Check (Cut (Cut'Last - Ellipsis'Length + 1 .. Cut'Last) = Ellipsis, "and ends in the ellipsis");
   end;
   declare
      Exact : constant String (1 .. Maximum_Argument_Shown) := [others => 'y'];
   begin
      Check (Shown (Exact) = Exact, "an argument of exactly the maximum is not cut");
   end;

   --  Lines.
   Add (61_000, Evaluate, "(logs.recent ""desktop"")", OK);
   Check (Count = 1 and Capacity = Initial_Entries, "the first entry allocates the initial ring");
   Check (Line (Entry_At (1)) = "evaluate (logs.recent ""desktop""): ok", "evaluate line: " & Line (Entry_At (1)));
   Add (62_000, Hello, "", Refused);
   Check (Line (Entry_At (2)) = "hello: refused", "no argument: " & Line (Entry_At (2)));
   Check (Clock_Image (3_723_000) = "1:02:03", "uptime clock: " & Clock_Image (3_723_000));
   Check (Clock_Image (0) = "0:00:00", "uptime clock at 0");

   --  Growth to the ceiling, then the oldest gives way.
   Reset;
   Check (Count = 0 and Capacity = 0 and Added = 0, "reset releases the ring");
   for I in 1 .. Maximum_Entries + 10 loop
      Add (Unsigned_64 (I), Get, "file-" & I'Image, (if I mod 3 = 0 then Failed else OK));
      if I = Initial_Entries + 1 then
         Check (Capacity = 2 * Initial_Entries, "the ring doubles when full");
      end if;
   end loop;
   Check (Capacity = Maximum_Entries, "the ring stops at the ceiling");
   Check (Count = Maximum_Entries, "a full ring holds the ceiling's worth");
   Check (Added = Unsigned_64 (Maximum_Entries + 10), "every entry is counted");
   Check (Entry_At (1).Time_Ms = 11, "the oldest kept is the 11th");
   Check (Entry_At (Count).Time_Ms = Unsigned_64 (Maximum_Entries + 10), "the newest is last");
   for N in 2 .. Count loop
      Check (Entry_At (N).Time_Ms = Entry_At (N - 1).Time_Ms + 1, "entries stay in order at" & N'Image);
   end loop;
   Reset;

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("remote_activity_tests: PASS");
   else
      Ada.Text_IO.Put_Line ("remote_activity_tests: FAIL" & Failures'Image);
      raise Program_Error;
   end if;
end Remote_Activity_Tests;
