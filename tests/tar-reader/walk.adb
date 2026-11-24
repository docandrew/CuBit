------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  walk ARCHIVE [skip]: Tar_Archives over a tar file on the Linux host, one
--  line per event ("D path", "F path size", "S path", then "END" or
--  "REFUSED reason"), for run.sh to compare with what Python's tarfile and
--  git archive wrote. A Linux-hosted check of the reader, not CuBit.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Tar_Archives; use Tar_Archives;

procedure Walk is
   use Ada.Streams;
   File : Stream_IO.File_Type;
   W : Walker;
   E : Event;
   Raw : Stream_Element_Array (1 .. BLOCK_BYTES);
   Got : Stream_Element_Offset;
   B : Block;
   Data_Bytes : Byte_Count := 0;
   Specials : constant Specials_Policy :=
     (if Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (2) = "skip"
      then Skip_Specials else Refuse_Specials);
begin
   Stream_IO.Open (File, Stream_IO.In_File, Ada.Command_Line.Argument (1));
   Start (W, (Maximum_Entries => 100_000, Maximum_Bytes => 2**40), Specials);
   loop
      if Stream_IO.End_Of_File (File) then
         Finish (W, E);
      else
         Stream_IO.Read (File, Raw, Got);
         if Got < Raw'Last then
            --  A partial block: the archive is cut short.
            Finish (W, E);
         else
            for K in B'Range loop
               B (K) := Unsigned_8 (Raw (Stream_Element_Offset (K) + 1));
            end loop;
            Feed (W, B, E);
         end if;
      end if;
      case E.Kind is
         when Directory_Entry => Put_Line ("D " & Path (W));
         when File_Start =>
            Put_Line ("F " & Path (W) & Byte_Count'Image (E.Size));
         when Skipped_Special => Put_Line ("S " & Path (W));
         when File_Data => Data_Bytes := Data_Bytes + Byte_Count (E.Data_Length);
         when Finished =>
            Put_Line ("END" & Byte_Count'Image (Data_Bytes));
            exit;
         when Refused =>
            Put_Line ("REFUSED " & E.Why'Image);
            exit;
         when Nothing => null;
      end case;
   end loop;
   Stream_IO.Close (File);
end Walk;
