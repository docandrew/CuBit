with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Device commands per operation on a journaled volume (io_counts.py): a
--  create with a 4 KiB write and an unlink must cost no device writes until
--  the commit (only cache-filling reads), and the commit must write the
--  small files' data in its data-first pass.
procedure IO_Counts is
   Files : constant := 1000;
   Fs : Filesystem;
   Admission : Admission_Result;
   Written : Write_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Flushed : Flush_Status;
   Removed : Remove_Status;
   Dir, Number : Unsigned_32;
   Item : Inode;
   Completed : Unsigned_64;
   Block : constant String (1 .. 4096) := [others => 'x'];
   Start, Creates, Unlinks : Natural;
   --  At most one cache-filling read per ten operations.
   Reads_Per_Ten : constant := 1;

   function Name (Index : Positive) return String is
     ("f" & Index'Image (2 .. Index'Image'Last));
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   makeDirectoryPath (Fs, "r1", Dir, Lookup, Written);
   pragma Assert (Written = Write_Complete);
   Flush (Fs, Flushed);
   Start := Device_Commands;
   for I in 1 .. Files loop
      createFile (Fs, Dir, Name (I), Number, Written);
      pragma Assert (Written = Write_Complete);
      readInode (Fs, Number, Item, Read_Result);
      writeData (Fs, Number, Item, 0, Block'Address, Block'Length, Completed, Written);
      pragma Assert (Written = Write_Complete and Completed = Block'Length);
   end loop;
   Creates := Device_Commands - Start;
   Flush (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Start := Device_Commands;
   for I in 1 .. Files loop
      unlinkPath (Fs, "r1/" & Name (I), False, Number, Item, Removed);
      pragma Assert (Removed = Remove_Complete);
   end loop;
   Unlinks := Device_Commands - Start;
   Flush (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   --  append+fsync: each fsync of one new block and its metadata is one
   --  commit with one barrier (the commit block is FUA), as jbd2's.
   createFile (Fs, Dir, "synced", Number, Written);
   readInode (Fs, Number, Item, Read_Result);
   writeData (Fs, Number, Item, 0, Block'Address, Block'Length, Completed, Written);
   Flush (Fs, Flushed);
   declare
      Requests_Before, Flushes_Before, Requests_After, Flushes_After : Unsigned_64;
      Syncs : constant := 100;
   begin
      deviceRequests (Requests_Before, Flushes_Before);
      for I in 1 .. Syncs loop
         readInode (Fs, Number, Item, Read_Result);
         --  An append: new block, bitmap, inode (the metadata a commit logs).
         writeData (Fs, Number, Item, Unsigned_64 (I) * Block'Length, Block'Address,
                    Block'Length, Completed, Written);
         pragma Assert (Written = Write_Complete);
         Flush (Fs, Flushed);
         pragma Assert (Flushed = Flush_Complete);
      end loop;
      deviceRequests (Requests_After, Flushes_After);
      Put_Line ("FSYNC REQUESTS" & Unsigned_64'Image ((Requests_After - Requests_Before) / Syncs) &
                " FLUSHES" & Unsigned_64'Image ((Flushes_After - Flushes_Before) / Syncs));
      pragma Assert (Flushes_After - Flushes_Before <= Syncs);
   end;
   Detach (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("CREATE COMMANDS" & Creates'Image & " UNLINK COMMANDS" & Unlinks'Image);
   pragma Assert (Creates <= Files * Reads_Per_Ten / 10);
   pragma Assert (Unlinks <= Files * Reads_Per_Ten / 10);
   Close_Image;
end IO_Counts;
