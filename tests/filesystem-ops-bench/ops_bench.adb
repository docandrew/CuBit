with Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Hosted A/B benchmark of the production Ext2 code (CPU side): the device
--  is an in-memory copy of a Linux-made image (fixture), so timings measure
--  the filesystem code, not I/O. Each workload runs Rounds times; the median
--  per operation is printed, with the device commands it issued.
procedure Ops_Bench is
   Files : constant := 1000;
   Rounds : constant := 7;
   Fs : Filesystem;
   Admission : Admission_Result;
   Written : Write_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Flushed : Flush_Status;
   Removed : Remove_Status;
   Dir, Number, Big : Unsigned_32;
   Item : Inode;
   Completed : Unsigned_64;
   Block : constant String (1 .. 4096) := [others => 'x'];
   Chunk : constant String (1 .. 512 * 1024) := [others => 'y'];
   Into : String (1 .. 512 * 1024);
   Seq_Bytes : constant := 16 * 1024 * 1024;

   type Workload is (Create_Write, Commit, Open_Read, Read_Inode, Lookup_Miss, List,
                     Seq_Write, Seq_Read, Unlink);
   type Samples is array (1 .. Rounds) of Duration;
   Results : array (Workload) of Samples;
   Ops : constant array (Workload) of Positive :=
     [Create_Write | Open_Read | Lookup_Miss | Unlink => Files, Read_Inode => 10 * Files,
      Commit => 1, List => 100, Seq_Write | Seq_Read => Seq_Bytes / Chunk'Length];
   Commands_Of : array (Workload) of Natural := [others => 0];

   function Name (Index : Positive) return String is
     ("f" & Index'Image (2 .. Index'Image'Last));

   Start : Time;
   Before : Natural;
   procedure Begin_Timing is
   begin
      Before := Commands;
      Start := Clock;
   end Begin_Timing;
   procedure End_Timing (W : Workload; R : Positive) is
   begin
      Results (W) (R) := To_Duration (Clock - Start);
      Commands_Of (W) := Commands - Before;
   end End_Timing;

   function Median (S : Samples) return Duration is
      T : Samples := S;
      Saved : Duration;
   begin
      for I in T'Range loop
         for J in I + 1 .. T'Last loop
            if T (J) < T (I) then
               Saved := T (I); T (I) := T (J); T (J) := Saved;
            end if;
         end loop;
      end loop;
      return T ((T'First + T'Last) / 2);
   end Median;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   configureCache (16);
   initBlockDevice (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   makeDirectoryPath (Fs, "bench", Dir, Lookup, Written);
   pragma Assert (Written = Write_Complete);
   Flush (Fs, Flushed);
   for R in 1 .. Rounds loop
      Begin_Timing;
      for I in 1 .. Files loop
         createFile (Fs, Dir, Name (I), Number, Written);
         pragma Assert (Written = Write_Complete);
         readInode (Fs, Number, Item, Read_Result);
         writeData (Fs, Number, Item, 0, Block'Address, Block'Length, Completed, Written);
         pragma Assert (Written = Write_Complete and Completed = Block'Length);
      end loop;
      End_Timing (Create_Write, R);
      Begin_Timing;
      Flush (Fs, Flushed);
      pragma Assert (Flushed = Flush_Complete);
      End_Timing (Commit, R);
      Begin_Timing;
      for I in 1 .. Files loop
         resolvePath (Fs, "bench/" & Name (I), Number, Lookup);
         pragma Assert (Lookup = Lookup_Found);
         readInode (Fs, Number, Item, Read_Result);
         readData (Fs, Item, 0, Into'Address, Block'Length, Completed, Read_Result);
         pragma Assert (Read_Result = Read_Complete and Completed = Block'Length);
      end loop;
      End_Timing (Open_Read, R);
      Begin_Timing;
      for Pass in 1 .. 10 loop
         for I in 1 .. Files loop
            readInode (Fs, Dir + Unsigned_32 (I), Item, Read_Result);
            pragma Assert (Read_Result = Read_Complete);
         end loop;
      end loop;
      End_Timing (Read_Inode, R);
      Begin_Timing;
      for I in 1 .. Files loop
         resolvePath (Fs, "bench/missing" & Name (I), Number, Lookup);
         pragma Assert (Lookup = Lookup_Not_Found);
      end loop;
      End_Timing (Lookup_Miss, R);
      readInode (Fs, Dir, Item, Read_Result);
      Begin_Timing;
      for Pass in 1 .. Ops (List) loop
         declare
            Entries : Listed_Records;
            Count, Seen : Natural := 0;
            Cursor, Next : Unsigned_64 := 0;
            Status : Directory_Read_Status;
         begin
            loop
               readDirectoryPage (Fs, Item, Cursor, Entries, Count, Next, Status);
               Seen := Seen + Count;
               exit when Status /= Directory_Page_Complete;
               Cursor := Next;
            end loop;
            pragma Assert (Status = Directory_End and Seen = Files);
         end;
      end loop;
      End_Timing (List, R);
      createFile (Fs, Dir, "big", Big, Written);
      pragma Assert (Written = Write_Complete);
      readInode (Fs, Big, Item, Read_Result);
      Begin_Timing;
      for I in 0 .. Ops (Seq_Write) - 1 loop
         writeData (Fs, Big, Item, Unsigned_64 (I) * Chunk'Length, Chunk'Address,
                    Chunk'Length, Completed, Written);
         pragma Assert (Written = Write_Complete);
      end loop;
      Flush (Fs, Flushed);
      End_Timing (Seq_Write, R);
      Begin_Timing;
      for I in 0 .. Ops (Seq_Read) - 1 loop
         readData (Fs, Item, Unsigned_64 (I) * Chunk'Length, Into'Address, Chunk'Length,
                   Completed, Read_Result);
         pragma Assert (Read_Result = Read_Complete and Completed = Chunk'Length);
      end loop;
      End_Timing (Seq_Read, R);
      Begin_Timing;
      for I in 1 .. Files loop
         unlinkPath (Fs, "bench/" & Name (I), False, Number, Item, Removed);
         pragma Assert (Removed = Remove_Complete);
      end loop;
      unlinkPath (Fs, "bench/big", False, Number, Item, Removed);
      pragma Assert (Removed = Remove_Complete);
      Flush (Fs, Flushed);
      End_Timing (Unlink, R);
   end loop;
   for W in Workload loop
      Put_Line (W'Image & " ns/op" &
                Natural'Image (Natural (Float (Median (Results (W))) * 1.0E9 / Float (Ops (W)))) &
                " commands" & Commands_Of (W)'Image);
   end loop;
   Detach (Fs, Flushed);
end Ops_Bench;
