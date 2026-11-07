--  A build's output as ccl-remote keeps it (userspace/ccl/remote/
--  remote_output_lines, used by Remote_Jobs): whole lines whatever pieces
--  the outlet ring delivers. The regression: ccl-build's standard error
--  arrived byte by byte, and each byte became its own "[stderr] x" line.
--  Checks byte-by-byte pieces, pieces across lines, an unterminated last
--  line (handed over by Finish, once), empty lines, the ceiling, and Reset.
pragma Ada_2022;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Remote_Output_Lines; use Remote_Output_Lines;

procedure Remote_Output_Lines_Tests is
   Failures : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL: " & What);
      end if;
   end Check;

   LF : constant Character := ASCII.LF;
   Prefix : constant String := "[stderr] ";
   --  What a job's output holds: each emitted line, prefixed, ended.
   Kept : Unbounded_String;
   Lines : Natural := 0;
   procedure Keep (Line : String) is
   begin
      Append (Kept, Prefix & Line & LF);
      Lines := Lines + 1;
   end Keep;
   procedure Feed_Kept is new Feed (Keep);
   procedure Finish_Kept is new Finish (Keep);

   A : Assembler;
   Message : constant String := "ccl-build: posix_spawn failed" & LF & "second line" & LF;
begin
   --  Byte by byte, as the laptop's unbuffered standard error came.
   Reset (A);
   for C of Message loop
      Feed_Kept (A, [1 => C]);
   end loop;
   Finish_Kept (A);
   Check (To_String (Kept) = Prefix & "ccl-build: posix_spawn failed" & LF & Prefix & "second line" & LF,
          "byte-by-byte pieces make two whole lines: " & To_String (Kept));
   Check (Lines = 2, "two lines, not one per byte:" & Lines'Image);

   --  Pieces that cut lines anywhere; an unterminated last line comes with
   --  Finish, and only then.
   Kept := Null_Unbounded_String;
   Lines := 0;
   Feed_Kept (A, "one" & LF & "tw");
   Feed_Kept (A, "o" & LF & LF & "thr");
   Check (To_String (Kept) = Prefix & "one" & LF & Prefix & "two" & LF & Prefix & LF,
          "complete lines only, an empty line kept: " & To_String (Kept));
   Check (Pending (A) = 3, "the partial line is held back");
   Feed_Kept (A, "ee");
   Finish_Kept (A);
   Check (Lines = 4 and then Pending (A) = 0, "Finish hands over the last line");
   Finish_Kept (A);
   Check (Lines = 4, "a second Finish hands over nothing");
   Check (To_String (Kept) (Length (Kept) - Prefix'Length - 5 .. Length (Kept)) = Prefix & "three" & LF,
          "the last line whole: " & To_String (Kept));

   --  A line past the ceiling: pieces of the ceiling's length.
   Kept := Null_Unbounded_String;
   Lines := 0;
   declare
      Long : constant String (1 .. Maximum_Line_Bytes + 10) := [others => 'x'];
   begin
      Feed_Kept (A, Long & LF);
      Check (Lines = 2, "a long line in two pieces:" & Lines'Image);
      Check (Length (Kept) = 2 * Prefix'Length + Long'Length + 2, "every byte kept");
   end;

   --  Reset: a new run starts with nothing held back.
   Feed_Kept (A, "left over");
   Reset (A);
   Lines := 0;
   Finish_Kept (A);
   Check (Lines = 0, "Reset drops the previous run's partial line");

   if Failures = 0 then
      Ada.Text_IO.Put_Line ("remote output lines: PASS");
   else
      Ada.Text_IO.Put_Line ("remote output lines: FAIL (" & Failures'Image & ")");
      raise Program_Error;
   end if;
end Remote_Output_Lines_Tests;
