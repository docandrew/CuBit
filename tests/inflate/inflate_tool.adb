------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Hosted driver of Inflate_Streams for tests/inflate/check.py: each line
--  of standard input is one case,
--    FORMAT IN_CHUNK OUT_CHUNK CEILING INPUT OUTPUT
--  (FORMAT zlib or raw; chunks in bytes, the input handed over IN_CHUNK
--  bytes at a time and decoded into a buffer of OUT_CHUNK bytes), and each
--  answer line is
--    FINISHED CONSUMED PRODUCED    (the output written to OUTPUT)
--    REFUSED REASON CONSUMED PRODUCED
--  Built with -gnata, so every contract of the unit is checked on every
--  call as well.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Inflate_Streams; use Inflate_Streams;

procedure Inflate_Tool is
   use Ada.Strings.Fixed;

   function Image (N : Byte_Count) return String is (Trim (N'Image, Ada.Strings.Left));

   procedure Run (Line : String) is
      Fields : array (1 .. 6) of Natural := [others => 0];
      Last_Of : array (1 .. 6) of Natural := [others => 0];
      Count : Natural := 0;
      I : Natural := Line'First;
   begin
      while I <= Line'Last and then Count < 6 loop
         while I <= Line'Last and then Line (I) = ' ' loop
            I := I + 1;
         end loop;
         exit when I > Line'Last;
         Count := Count + 1;
         Fields (Count) := I;
         while I <= Line'Last and then Line (I) /= ' ' loop
            I := I + 1;
         end loop;
         Last_Of (Count) := I - 1;
      end loop;
      if Count /= 6 then
         Put_Line ("BAD_CASE");
         return;
      end if;
      declare
         function F (K : Positive) return String is (Line (Fields (K) .. Last_Of (K)));
         Kind : constant Stream_Format := (if F (1) = "raw" then Raw_Deflate else Zlib_Stream);
         In_Chunk : constant Positive := Positive'Value (F (2));
         Out_Chunk : constant Positive := Positive'Value (F (3));
         Ceiling_Bytes : constant Byte_Count := Byte_Count'Value (F (4));
         Size : constant Natural := Natural (Ada.Directories.Size (F (5)));
         Data : Byte_Array (1 .. Size);
         Buffer : Byte_Array (1 .. Out_Chunk) := [others => 0];
         D : Inflater;
         In_File, Out_File : Ada.Streams.Stream_IO.File_Type;
         Lo : Positive := 1;
         Hi : Natural;
         In_Next, Out_Next : Positive;
         R : Progress := Need_Input;
      begin
         Ada.Streams.Stream_IO.Open (In_File, Ada.Streams.Stream_IO.In_File, F (5));
         Byte_Array'Read (Ada.Streams.Stream_IO.Stream (In_File), Data);
         Ada.Streams.Stream_IO.Close (In_File);
         Ada.Streams.Stream_IO.Create (Out_File, Ada.Streams.Stream_IO.Out_File, F (6));
         Start (D, Kind, Ceiling_Bytes);
         Outer :
         loop
            if Lo > Data'Last then
               Finish (D, R);
               exit Outer;
            end if;
            Hi := Natural'Min (Lo + In_Chunk - 1, Data'Last);
            In_Next := Lo;
            loop
               Out_Next := 1;
               Inflate (D, Data (Lo .. Hi), In_Next, Buffer, Out_Next, R);
               Byte_Array'Write (Ada.Streams.Stream_IO.Stream (Out_File), Buffer (1 .. Out_Next - 1));
               exit when R /= Output_Full;
            end loop;
            exit Outer when R in Finished | Refused;
            Lo := In_Next;
         end loop Outer;
         Ada.Streams.Stream_IO.Close (Out_File);
         --  Calls after the end change nothing.
         declare
            Again : Progress;
            N1 : Positive := 1;
            N2 : Positive := 1;
         begin
            Inflate (D, Data (1 .. 0), N1, Buffer, N2, Again);
            if Again /= R then
               Put_Line ("NOT_FINAL");
               return;
            end if;
         end;
         if R = Finished then
            Put_Line ("FINISHED " & Image (Total_In (D)) & " " & Image (Total_Out (D)));
         else
            Put_Line ("REFUSED " & Name (Failure (D)) & " " & Image (Total_In (D)) & " "
                      & Image (Total_Out (D)));
         end if;
      end;
   end Run;
begin
   if Ada.Command_Line.Argument_Count /= 0 then
      Put_Line ("usage: inflate_tool < cases");
      return;
   end if;
   while not End_Of_File loop
      Run (Get_Line);
      Flush;
   end loop;
end Inflate_Tool;
