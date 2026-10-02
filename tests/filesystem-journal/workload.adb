with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  A journaled workload for power-cut injection (see crash.py): creation,
--  sparse growth into indirect blocks, overwrite, shrink, rename and a large
--  unsynchronized write, with a flush after each step. "SYNCED k" is printed
--  only after step k's flush completed: its state must survive any cut.
procedure Workload is
   Fs : Filesystem;
   Admission : Admission_Result;
   Flushed : Flush_Status;
   Write_Result : Write_Status;
   Truncated : Truncate_Status;
   Renamed : Rename_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Completed : Unsigned_64;

   procedure Fill (Name : String; Offset, Length : Unsigned_64; Value : Character) is
      Number : Unsigned_32;
      Item : Inode;
      Data : constant String (1 .. Natural (Length)) := [others => Value];
   begin
      resolvePath (Fs, Name, Number, Lookup);
      pragma Assert (Lookup = Lookup_Found);
      readInode (Fs, Number, Item, Read_Result);
      pragma Assert (Read_Result = Read_Complete);
      writeData (Fs, Number, Item, Offset, Data'Address, Length, Completed, Write_Result);
      pragma Assert (Write_Result = Write_Complete and Completed = Length);
   end Fill;

   procedure Create (Name : String) is
      Number : Unsigned_32;
   begin
      createFile (Fs, ROOT_INODE, Name, Number, Write_Result);
      pragma Assert (Write_Result = Write_Complete);
   end Create;

   procedure Resize (Name : String; Size : Unsigned_64) is
      Number : Unsigned_32;
      Item : Inode;
   begin
      resolvePath (Fs, Name, Number, Lookup);
      pragma Assert (Lookup = Lookup_Found);
      resizeFile (Fs, Number, Size, Item, Truncated);
      pragma Assert (Truncated = Truncate_Complete);
   end Resize;

   procedure Sync (Step : Positive) is
   begin
      Flush (Fs, Flushed);
      pragma Assert (Flushed = Flush_Complete);
      Put_Line ("SYNCED" & Step'Image);
   end Sync;

   Kib : constant := 1024;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   pragma Assert (Admission = Admitted);
   Create ("alpha");
   Fill ("alpha", 0, 3 * Kib, 'A');
   Sync (1);
   Fill ("alpha", 50 * Kib, 20 * Kib, 'B'); -- sparse, into the indirect tree
   Create ("beta");
   Fill ("beta", 0, 40 * Kib, 'C');
   Sync (2);
   Resize ("alpha", 10 * Kib);
   Fill ("victim", 5 * Kib, 6 * Kib, 'D'); -- a Linux-created file's blocks
   Sync (3);
   renamePath (Fs, "beta", "gamma", Renamed);
   pragma Assert (Renamed = Rename_Complete);
   Fill ("gamma", 40 * Kib, 30 * Kib, 'E');
   Create ("delta");
   Fill ("delta", 0, 200 * Kib, 'F');
   Sync (4);
   Resize ("gamma", 0);
   Fill ("delta", 100 * Kib, 8 * Kib, 'G');
   --  New blocks right after a release: they must not be gamma's, which
   --  its uncommitted truncate still references on disk.
   Create ("epsilon");
   Fill ("epsilon", 0, 70 * Kib, 'H');
   Sync (5);
   Detach (Fs, Flushed);
   pragma Assert (Flushed = Flush_Complete);
   Put_Line ("DETACHED");
   Close_Image;
end Workload;
