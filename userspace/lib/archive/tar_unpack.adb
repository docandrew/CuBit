------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  Unpacking a tar archive into a new directory (see the spec).
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Tar_Unpack is
   use Tar_Archives;

   --  Archive bytes read at once, and file bytes written at once.
   CHUNK_BYTES : constant := 64 * BLOCK_BYTES;
   STAGING_BYTES : constant := 64 * BLOCK_BYTES;
   SEPARATOR : constant Character := '/';
   TEMPORARY_PREFIX : constant String := ".";
   TEMPORARY_SUFFIX : constant String := ".unpacking";
   MAXIMUM_NAME_BYTES : constant := 255;

   type Text_Access is access String;
   procedure Free is new Ada.Unchecked_Deallocation (String, Text_Access);
   type Made;
   type Made_Access is access Made;
   type Made is record
      Path : Text_Access;
      Directory : Boolean;
      Older : Made_Access;
   end record;
   procedure Free is new Ada.Unchecked_Deallocation (Made, Made_Access);

   procedure Note (Newest : in out Made_Access; Path : String; Directory : Boolean) is
   begin
      Newest := new Made'(Path => new String'(Path), Directory => Directory, Older => Newest);
   end Note;

   --  Remove everything made, newest first.
   procedure Undo (S : in out Store; Newest : in out Made_Access) is
      Next : Made_Access;
   begin
      while Newest /= null loop
         Next := Newest.Older;
         if Newest.Directory then
            Remove_Directory (S, Newest.Path.all);
         else
            Remove_File (S, Newest.Path.all);
         end if;
         Free (Newest.Path);
         Free (Newest);
         Newest := Next;
      end loop;
   end Undo;

   procedure Forget (Newest : in out Made_Access) is
      Next : Made_Access;
   begin
      while Newest /= null loop
         Next := Newest.Older;
         Free (Newest.Path);
         Free (Newest);
         Newest := Next;
      end loop;
   end Forget;

   procedure Set_Failed (Result : in out Report; Path : String) is
      N : constant Natural := Natural'Min (Path'Length, Result.Failed_Path'Length);
   begin
      Result.Ended_As := Write_Failed;
      Result.Failed_Length := N;
      Result.Failed_Path (1 .. N) := Path (Path'First .. Path'First + N - 1);
   end Set_Failed;

   function Single_Name (Text : String) return Boolean is
     (Text'Length in 1 .. MAXIMUM_NAME_BYTES and then Confined (Text)
      and then (for all C of Text => C /= SEPARATOR));

   procedure Unpack
     (S : in out Store; Parent, Directory : String; Ceiling : Limits;
      Specials : Specials_Policy; Result : out Report)
   is
      Target : constant String := Parent & SEPARATOR & Directory;
      Temporary : constant String := Parent & SEPARATOR & TEMPORARY_PREFIX & Directory & TEMPORARY_SUFFIX;
      Newest : Made_Access := null;

      --  One pass: Writing False only checks; True makes the tree.
      procedure Walk (Writing : Boolean) is
         W : Walker;
         E : Event;
         Offset : Byte_Count := 0;
         Chunk : String (1 .. CHUNK_BYTES);
         Last : Natural;
         Ok : Boolean;
         B : Block;
         Open_File : Boolean := False;
         File_Offset : Byte_Count := 0;
         Staging : String (1 .. STAGING_BYTES);
         Staged : Natural := 0;
         --  The directory the last file went into, made already.
         Last_Parent : Text_Access := null;
         Stop : Boolean := False;

         procedure Fail (Path : String) is
         begin
            Set_Failed (Result, Path);
            Stop := True;
         end Fail;

         procedure Flush is
         begin
            if Staged > 0 and then not Stop then
               Write (S, File_Offset, Staging (1 .. Staged), Ok);
               if not Ok then
                  Fail (Temporary & SEPARATOR & Path (W));
                  return;
               end if;
               File_Offset := File_Offset + Byte_Count (Staged);
               Staged := 0;
            end if;
         end Flush;

         --  Every directory above Below (a path under Temporary), made when absent.
         procedure Make_Parents (Below : String) is
            Cut : Natural := 0;
         begin
            for I in reverse Below'Range loop
               if Below (I) = SEPARATOR then
                  Cut := I;
                  exit;
               end if;
            end loop;
            if Cut = 0 then
               return;
            end if;
            declare
               Up : constant String := Below (Below'First .. Cut - 1);
            begin
               if Last_Parent /= null and then Last_Parent.all = Up then
                  return;
               end if;
               for I in Up'Range loop
                  exit when Stop;
                  if Up (I) = SEPARATOR or else I = Up'Last then
                     declare
                        Step : constant String := Up (Up'First .. (if I = Up'Last then I else I - 1));
                     begin
                        if not Exists (S, Temporary & SEPARATOR & Step) then
                           Make_Directory (S, Temporary & SEPARATOR & Step, Ok);
                           if Ok then
                              Note (Newest, Temporary & SEPARATOR & Step, Directory => True);
                           else
                              Fail (Temporary & SEPARATOR & Step);
                           end if;
                        end if;
                     end;
                  end if;
               end loop;
               if not Stop then
                  Free (Last_Parent);
                  Last_Parent := new String'(Up);
               end if;
            end;
         end Make_Parents;

         procedure Take (E : Event; B : Block) is
         begin
            case E.Kind is
               when Directory_Entry =>
                  if Writing then
                     Make_Parents (Path (W));
                     if not Stop and then not Exists (S, Temporary & SEPARATOR & Path (W)) then
                        Make_Directory (S, Temporary & SEPARATOR & Path (W), Ok);
                        if Ok then
                           Note (Newest, Temporary & SEPARATOR & Path (W), Directory => True);
                        else
                           Fail (Temporary & SEPARATOR & Path (W));
                        end if;
                     end if;
                  end if;
               when File_Start =>
                  if Writing then
                     Make_Parents (Path (W));
                     if not Stop then
                        Create_File (S, Temporary & SEPARATOR & Path (W), Ok);
                        if Ok then
                           Note (Newest, Temporary & SEPARATOR & Path (W), Directory => False);
                           Open_File := True;
                           File_Offset := 0;
                           Staged := 0;
                           if E.Size = 0 then
                              Close_File (S);
                              Open_File := False;
                           end if;
                        else
                           Fail (Temporary & SEPARATOR & Path (W));
                        end if;
                     end if;
                  end if;
               when File_Data =>
                  if Writing then
                     if Staged + E.Data_Length > Staging'Length then
                        Flush;
                     end if;
                     for K in 0 .. E.Data_Length - 1 loop
                        Staging (Staged + 1 + K) := Character'Val (B (K));
                     end loop;
                     Staged := Staged + E.Data_Length;
                     if E.Last_Piece then
                        Flush;
                        Close_File (S);
                        Open_File := False;
                     end if;
                  end if;
               when Skipped_Special =>
                  if not Writing then
                     Result.Skipped := Result.Skipped + 1;
                  end if;
               when Refused =>
                  Result.Why := E.Why;
                  if E.Why = Unsafe_Path then
                     Result.Entry_Length := Rejected_Name (W)'Length;
                     Result.Entry_Path (1 .. Result.Entry_Length) := Rejected_Name (W);
                  end if;
                  Result.Ended_As :=
                    (if E.Why in Too_Many_Entries | Too_Many_Bytes then Too_Large else Refused);
                  Stop := True;
               when Finished =>
                  Stop := True;
               when Nothing =>
                  null;
            end case;
            if E.Kind in Directory_Entry | File_Start | Skipped_Special then
               Result.Entry_Length := Path (W)'Length;
               Result.Entry_Path (1 .. Result.Entry_Length) := Path (W);
            end if;
         end Take;
      begin
         Start (W, Ceiling, Specials);
         Read_Loop :
         loop
            Read (S, Offset, Chunk, Last, Ok);
            if not Ok then
               Result.Ended_As := Unreadable;
               Stop := True;
               exit Read_Loop;
            end if;
            declare
               Whole : constant Natural := (if Last < Chunk'First then 0 else Last / BLOCK_BYTES);
            begin
               for K in 0 .. Whole - 1 loop
                  for J in Block_Offset loop
                     B (J) := Character'Pos (Chunk (K * BLOCK_BYTES + J + 1));
                  end loop;
                  Feed (W, B, E);
                  Take (E, B);
                  exit Read_Loop when Stop;
               end loop;
               if Whole * BLOCK_BYTES < Last or else Last < Chunk'Last then
                  --  The archive ends here (a partial block is not one).
                  Finish (W, E);
                  Take (E, B);
                  exit Read_Loop;
               end if;
            end;
            Offset := Offset + Byte_Count (Last);
         end loop Read_Loop;
         if Open_File then
            Close_File (S);
         end if;
         Free (Last_Parent);
         if not Writing then
            Result.Entries := Entries (W);
            Result.Bytes := File_Bytes (W);
         end if;
      end Walk;
   begin
      Result := (others => <>);
      if not Single_Name (Directory) then
         Result.Ended_As := Bad_Directory;
         return;
      elsif Exists (S, Target) or else Exists (S, Temporary) then
         Result.Ended_As := Target_Exists;
         return;
      end if;
      --  The temporary directory first: a target this program may not
      --  write is refused before the archive is read.
      declare
         Ok : Boolean;
      begin
         Make_Directory (S, Temporary, Ok);
         if not Ok then
            Set_Failed (Result, Temporary);
            return;
         end if;
         Note (Newest, Temporary, Directory => True);
         --  Pass 1: the whole archive, checked; nothing written.
         Result.Ended_As := Unpacked;
         Walk (Writing => False);
         --  Pass 2: the tree under the temporary name, then renamed.
         if Result.Ended_As = Unpacked then
            Walk (Writing => True);
         end if;
         if Result.Ended_As = Unpacked then
            Rename (S, Temporary, Target, Ok);
            if not Ok then
               Set_Failed (Result, Target);
            end if;
         end if;
         if Result.Ended_As = Unpacked then
            Forget (Newest);
         else
            Undo (S, Newest);
         end if;
      end;
   end Unpack;
end Tar_Unpack;
