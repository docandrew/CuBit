------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  Linux-hosted: the production FAT32 driver (Fat32) on an image file,
--  with only its block IPC replaced (tests/filesystem-interop/fixture).
--  Arguments: IMAGE [WINDOW_FIRST WINDOW_BLOCKS] (a partition's sectors).
--  Reads commands from standard input, one per line (words separated by
--  tabs, so names may hold spaces), and prints one
--  result line each ("<command>: <Status> [details]"):
--    mount                    (first: the admission result, ro or rw)
--    ls PATH                  then one "  name<TAB>kind<TAB>size" per entry
--    stat PATH                kind, size, mode and modified time
--    put PATH HOSTFILE [SLICE [OFFSET]]   create if missing, write the file
--    get PATH HOSTFILE        read the whole file out
--    mkdir PATH | rm PATH | rmdir PATH | mv OLD NEW | truncate PATH SIZE
--    rmkeep PATH              unlink, keeping the clusters (an open file)
--    reclaim NUMBER           free what rmkeep kept
--    flush                    durable, FSInfo written, marked clean
--    free                     free and total clusters
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Direct_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces; use Interfaces;
with CuBit.Handles.Fixture;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;
with Fat32; use Fat32;

procedure Fat_Tool is
   V : Volume;
   Admission : Admission_Result;
   Window_First : constant Unsigned_64 :=
     (if Ada.Command_Line.Argument_Count >= 2 then Unsigned_64'Value (Ada.Command_Line.Argument (2)) else 0);
   Window_Blocks : constant Unsigned_64 :=
     (if Ada.Command_Line.Argument_Count >= 3 then Unsigned_64'Value (Ada.Command_Line.Argument (3)) else 0);

   type Byte is mod 256;
   type Bytes is array (Natural range <>) of Byte;
   type Bytes_Access is access Bytes;

   function Word (Line : String; N : Positive) return String is
      First : Natural := Line'First;
      Last : Natural;
      Count : Natural := 0;
   begin
      loop
         while First <= Line'Last and then Line (First) = ASCII.HT loop
            First := First + 1;
         end loop;
         exit when First > Line'Last;
         Last := First;
         while Last < Line'Last and then Line (Last + 1) /= ASCII.HT loop
            Last := Last + 1;
         end loop;
         Count := Count + 1;
         if Count = N then
            return Line (First .. Last);
         end if;
         First := Last + 1;
      end loop;
      return "";
   end Word;

   function Load (Path : String) return Bytes_Access is
      package IO is new Ada.Direct_IO (Byte);
      F : IO.File_Type;
      Data : Bytes_Access;
   begin
      IO.Open (F, IO.In_File, Path);
      Data := new Bytes (0 .. Natural (IO.Size (F)) - 1);
      for I in Data'Range loop
         IO.Read (F, Data (I));
      end loop;
      IO.Close (F);
      return Data;
   end Load;

   procedure Save (Path : String; Data : Bytes) is
      package IO is new Ada.Direct_IO (Byte);
      F : IO.File_Type;
   begin
      IO.Create (F, IO.Out_File, Path);
      for B of Data loop
         IO.Write (F, B);
      end loop;
      IO.Close (F);
   end Save;

   procedure Report (Command : String; S : Status; Detail : String := "") is
   begin
      Put_Line (Command & ": " & Status'Image (S) & (if Detail = "" then "" else " " & Detail));
   end Report;

   function Kind_Of (Item : Inode) return String is
     (if Shift_Right (Item.typeAndPermissions, 12) = 4 then "dir" else "file");

   procedure Do_Ls (Path : String) is
      Number : Unsigned_32;
      S : Status;
      Dir, Item : Inode;
      Entries : Listed_Records;
      Count : Natural;
      Cursor, Next : Unsigned_64 := 0;
      Total : Natural := 0;
   begin
      Resolve (V, Path, Number, S);
      if S = Done then
         Read_Object (V, Number, Dir, S);
      end if;
      if S /= Done then
         Report ("ls " & Path, S);
         return;
      end if;
      loop
         Read_Directory_Page (V, Dir, Cursor, Entries, Count, Next, S);
         exit when S not in Done | Listing_End;
         for K in 0 .. Count - 1 loop
            declare
               Name : String (1 .. Entries (K).Length);
               St : Status;
            begin
               for I in Name'Range loop
                  Name (I) := Character'Val (Entries (K).Name (I));
               end loop;
               Read_Object (V, Entries (K).Number, Item, St);
               Put_Line ("  " & Name & ASCII.HT & Kind_Of (Item) & ASCII.HT
                         & Trim (Unsigned_32'Image (Item.sizeLo), Ada.Strings.Left));
               Total := Total + 1;
            end;
         end loop;
         exit when S = Listing_End;
         if Next <= Cursor then
            S := Malformed;   --  a cursor that does not move
            exit;
         end if;
         Cursor := Next;
      end loop;
      Report ("ls " & Path, (if S = Listing_End then Done else S), Trim (Total'Image, Ada.Strings.Left));
   end Do_Ls;

   procedure Do_Put (Path, Host : String; Slice : Positive; Start : Unsigned_64) is
      Data : constant Bytes_Access := Load (Host);
      Number : Unsigned_32;
      S : Status;
      Item : Inode;
      Done_Bytes : Unsigned_64;
      Position : Natural := 0;
      Slash : constant Natural := Index (Path, "/", Ada.Strings.Backward);
   begin
      Resolve (V, Path, Number, S);
      if S = Not_Found then
         declare
            Parent : Unsigned_32;
         begin
            Resolve (V, (if Slash = 0 then "" else Path (Path'First .. Slash - 1)), Parent, S);
            if S = Done then
               Create_File (V, Parent, Path ((if Slash = 0 then Path'First else Slash + 1) .. Path'Last), Number, S);
            end if;
         end;
      end if;
      if S = Done then
         Read_Object (V, Number, Item, S);
      end if;
      while S = Done and then Position < Data'Length loop
         declare
            N : constant Natural := Natural'Min (Slice, Data'Length - Position);
         begin
            Write_Data (V, Number, Item, Start + Unsigned_64 (Position), Data (Position)'Address,
                        Unsigned_64 (N), Done_Bytes, S);
            Position := Position + N;
         end;
      end loop;
      Report ("put " & Path, S, Trim (Unsigned_32'Image (Number), Ada.Strings.Left));
   end Do_Put;

   procedure Do_Get (Path, Host : String) is
      Number : Unsigned_32;
      S : Status;
      Item : Inode;
      Done_Bytes : Unsigned_64;
   begin
      Resolve (V, Path, Number, S);
      if S = Done then
         Read_Object (V, Number, Item, S);
      end if;
      if S /= Done then
         Report ("get " & Path, S);
         return;
      end if;
      declare
         Data : constant Bytes_Access := new Bytes (0 .. Natural (Item.sizeLo) - 1);
         Position : Natural := 0;
      begin
         --  Odd-sized reads, to cross sectors and clusters unaligned.
         while S = Done and then Position < Data'Length loop
            declare
               N : constant Natural := Natural'Min (77_777, Data'Length - Position);
            begin
               Read_Data (V, Item, Unsigned_64 (Position), Data (Position)'Address, Unsigned_64 (N),
                          Done_Bytes, S);
               if S = Done and then Done_Bytes /= Unsigned_64 (N) then
                  S := Malformed;
               end if;
               Position := Position + N;
            end;
         end loop;
         if S = Done then
            Save (Host, Data.all);
         end if;
      end;
      Report ("get " & Path, S, Trim (Unsigned_32'Image (Item.sizeLo), Ada.Strings.Left));
   end Do_Get;

begin
   Open_Image (Ada.Command_Line.Argument (1));
   while not End_Of_File loop
      declare
         Line : constant String := Get_Line;
         Command : constant String := Word (Line, 1);
         A : constant String := Word (Line, 2);
         B : constant String := Word (Line, 3);
         C : constant String := Word (Line, 4);
         Number, Replaced_Number : Unsigned_32;
         S, Parent_S : Status;
         Item : Inode;
      begin
         if Command = "" or else Command (Command'First) = '#' then
            null;
         elsif Command = "mount" then
            Mount (V, CuBit.Handles.Fixture.Endpoint_Of (1), 1, (slot => 1, generation => 1),
                   Grant_Buffer'Address, Grant_Buffer'Length, (First => Window_First, Blocks => Window_Blocks),
                   Admission);
            Put_Line ("mount: " & Admission_Result'Image (Admission)
                      & (if Admission = Admitted then (if Read_Only (V) then " ro" else " rw") else ""));
         elsif Command = "ls" then
            Do_Ls (A);
         elsif Command = "stat" then
            Resolve (V, A, Number, S);
            if S = Done then
               Read_Object (V, Number, Item, S);
            end if;
            Report ("stat " & A, S, (if S = Done then Kind_Of (Item) & Unsigned_32'Image (Item.sizeLo)
                                     & Unsigned_16'Image (Item.typeAndPermissions)
                                     & Unsigned_32'Image (Item.modifiedTime) else ""));
         elsif Command = "put" then
            Do_Put (A, B, (if C = "" then 40_000 else Positive'Value (C)),
                    (if Word (Line, 5) = "" then 0 else Unsigned_64'Value (Word (Line, 5))));
         elsif Command = "get" then
            Do_Get (A, B);
         elsif Command = "mkdir" then
            Make_Directory (V, A, Number, Parent_S, S);
            Report ("mkdir " & A, (if Parent_S /= Done then Parent_S else S));
         elsif Command = "rm" or else Command = "rmkeep" then
            Unlink (V, A, Command = "rmkeep", Number, Item, S);
            Report (Command & " " & A, S, Trim (Unsigned_32'Image (Number), Ada.Strings.Left));
         elsif Command = "reclaim" then
            Reclaim (V, Unsigned_32'Value (A), S);
            Report ("reclaim " & A, S);
         elsif Command = "rmdir" then
            Remove_Directory (V, A, Number, S);
            Report ("rmdir " & A, S);
         elsif Command = "mv" then
            Rename (V, A, B, False, Replaced_Number, Item, S);
            Report ("mv " & A & " " & B, S);
         elsif Command = "truncate" then
            Resolve (V, A, Number, S);
            if S = Done then
               Resize (V, Number, Unsigned_64'Value (B), Item, S);
            end if;
            Report ("truncate " & A, S);
         elsif Command = "flush" then
            Flush (V, S);
            Report ("flush", S);
         elsif Command = "free" then
            Put_Line ("free:" & Unsigned_64'Image (Free_Clusters (V)) & Unsigned_64'Image (Total_Clusters (V)));
         else
            Put_Line (Command & ": unknown command");
         end if;
      end;
   end loop;
   Close_Image;
end Fat_Tool;
