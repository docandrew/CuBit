--  The program GNAT builds on CuBit (tests/gnat): it uses the full Ada
--  run-time library. It writes its transcript to standard output and to
--  transcript.txt (Ada.Text_IO, both), raises and handles an exception,
--  finalizes a controlled object, sorts a vector (Ada.Containers) and
--  exits with Exit_Code. The test compares the transcript with the same
--  program's on Linux.
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Text_IO;

procedure Hello is
   use Ada.Text_IO;

   Transcript : File_Type;

   --  Each line goes to standard output and to the transcript file.
   procedure Say (Line : String) is
   begin
      Put_Line (Line);
      Put_Line (Transcript, Line);
   end Say;

   --  A controlled object reports its finalization.
   type Tracker is new Ada.Finalization.Limited_Controlled with record
      Name : Character;
   end record;
   overriding procedure Finalize (Object : in out Tracker);
   overriding procedure Finalize (Object : in out Tracker) is
   begin
      Say ("finalized " & Object.Name);
   end Finalize;

   package Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   package Sorting is new Integer_Vectors.Generic_Sorting;

   Unusual : exception;

   procedure Fail (Depth : Natural) is
   begin
      if Depth = 0 then
         raise Unusual with "raised three calls deep";
      end if;
      Fail (Depth - 1);
   end Fail;

   type Integers is array (Positive range <>) of Integer;
   Unsorted : constant Integers := [5, 3, 9, 1, 7];
   Values : Integer_Vectors.Vector;
   Total : Integer := 0;
   Exit_Code : constant := 7;
begin
   Create (Transcript, Out_File, "transcript.txt");
   Say ("hello from Ada");
   declare
      Scoped : Tracker;
   begin
      Scoped.Name := 'A';
      Say ("controlled object in scope");
   end;
   begin
      Fail (3);
      Say ("not reached");
   exception
      when E : Unusual =>
         Say ("handled " & Ada.Exceptions.Exception_Name (E) & ": " &
              Ada.Exceptions.Exception_Message (E));
   end;
   --  A check the compiler inserts: the run-time library raises it.
   declare
      Small : Positive := Ada.Command_Line.Argument_Count + 1;
   begin
      Small := Small - 1;
      Say ("not reached" & Small'Image);
   exception
      when Constraint_Error =>
         Say ("handled Constraint_Error");
   end;
   for Value of Unsorted loop
      Values.Append (Value);
   end loop;
   Sorting.Sort (Values);
   for Value of Values loop
      Total := Total + Value;
      Say ("value" & Integer'Image (Value));
   end loop;
   Say ("total" & Integer'Image (Total));
   Close (Transcript);
   Ada.Command_Line.Set_Exit_Status (Exit_Code);
end Hello;
