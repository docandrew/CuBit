with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  Namespace operations (mkdir, unlink, rmdir, unlink of an open file with
--  reclaim at last close) on a mke2fs-made ext2 or ext3 image, driven by
--  namespace.py. "SYNCED k" is printed only after step k's flush completed.
--  With CUBIT_FAIL set, the first operation that does not return its
--  expected status prints "OP FAILED" and the volume is detached; otherwise
--  that is a test failure.
procedure Namespace is
   Fs : Filesystem;
   Admission : Admission_Result;
   Flushed : Flush_Status;
   Written : Write_Status;
   Removed : Remove_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Number : Unsigned_32;
   Completed : Unsigned_64;
   Kib : constant := 1024;
   Many : constant := 24; -- enough long names to grow the root directory
   Injecting : constant Boolean := Ada.Environment_Variables.Exists ("CUBIT_FAIL");

   procedure Stop is
   begin
      Detach (Fs, Flushed);
      Put_Line ("DETACH " & Flushed'Image);
      Close_Image;
      GNAT.OS_Lib.OS_Exit (if Injecting then 0 else 1);
   end Stop;

   procedure Expect (Label : String; Ok : Boolean; Detail : String) is
   begin
      if not Ok then
         Put_Line ("OP FAILED " & Label & " " & Detail);
         Stop;
      end if;
   end Expect;

   procedure Sync (Step : Positive) is
   begin
      Flush (Fs, Flushed);
      Expect ("flush", Flushed = Flush_Complete, Flushed'Image);
      Put_Line ("SYNCED" & Step'Image);
   end Sync;

   procedure Make (Path : String; Expected : Write_Status := Write_Complete) is
   begin
      makeDirectoryPath (Fs, Path, Number, Lookup, Written);
      Expect ("mkdir " & Path, Lookup = Lookup_Found and then Written = Expected,
              Lookup'Image & " " & Written'Image);
   end Make;

   procedure Remove_Directory (Path : String; Expected : Remove_Status := Remove_Complete) is
   begin
      removeDirectoryPath (Fs, Path, Number, Removed);
      Expect ("rmdir " & Path, Removed = Expected, Removed'Image);
   end Remove_Directory;

   procedure Unlink (Path : String; Expected : Remove_Status := Remove_Complete) is
      Item : Inode;
   begin
      unlinkPath (Fs, Path, False, Number, Item, Removed);
      Expect ("unlink " & Path, Removed = Expected, Removed'Image);
   end Unlink;

   function Many_Name (Index : Positive) return String is
     ("many-directories-with-long-names-to-fill-blocks-" & Index'Image (2 .. Index'Image'Last));
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, 1, (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   Expect ("admission", Admission = Admitted, Admission'Image);
   Make ("d1");
   Sync (1);
   Make ("d1/sub");
   Sync (2);
   declare
      Item : Inode;
      Data : constant String (1 .. 30 * Kib) := [others => 'F'];
   begin
      Make ("d1/f");  -- replaced below by a file of that name
      Remove_Directory ("d1/f");
      resolvePath (Fs, "d1", Number, Lookup);
      Expect ("lookup d1", Lookup = Lookup_Found, Lookup'Image);
      createFile (Fs, Number, "f", Number, Written);
      Expect ("create d1/f", Written = Write_Complete, Written'Image);
      readInode (Fs, Number, Item, Read_Result);
      Expect ("read d1/f", Read_Result = Read_Complete, Read_Result'Image);
      writeData (Fs, Number, Item, 0, Data'Address, Data'Length, Completed, Written);
      Expect ("write d1/f", Written = Write_Complete, Written'Image);
   end;
   Sync (3);
   Unlink ("victim");
   Sync (4);
   declare
      Item : Inode;
      Target : Unsigned_32;
      More : constant String (1 .. 5 * Kib) := [others => 'G'];
      Back : String (1 .. 35 * Kib);
   begin
      --  A handle still holds d1/f: the name goes, the inode stays usable.
      unlinkPath (Fs, "d1/f", True, Target, Item, Removed);
      Expect ("unlink open d1/f", Removed = Remove_Complete and then
              Item.numHardLinks = 0, Removed'Image);
      resolvePath (Fs, "d1/f", Number, Lookup);
      Expect ("d1/f gone", Lookup = Lookup_Not_Found, Lookup'Image);
      writeData (Fs, Target, Item, 30 * Kib, More'Address, More'Length, Completed, Written);
      Expect ("write unlinked", Written = Write_Complete, Written'Image);
      readData (Fs, Item, 0, Back'Address, Back'Length, Completed, Read_Result);
      Expect ("read unlinked", Read_Result = Read_Complete and then
              Completed = Back'Length and then
              Back = (1 .. 30 * Kib => 'F') & More, Read_Result'Image);
      --  Durable while still open: a crash now leaves an orphan.
      Sync (5);
      reclaimInode (Fs, Target, Removed);
      Expect ("reclaim", Removed = Remove_Complete, Removed'Image);
   end;
   Sync (6);
   --  Refusals change nothing.
   Remove_Directory ("d1", Remove_Not_Empty);
   Remove_Directory ("keep", Remove_Not_Empty);
   Remove_Directory ("plain", Remove_Wrong_Type);
   Unlink ("keep", Remove_Wrong_Type);
   Unlink ("d1/sub", Remove_Wrong_Type);
   Unlink ("nothing", Remove_Not_Found);
   Unlink ("nothing/x", Remove_Not_Found);
   Remove_Directory ("d1/nothing", Remove_Not_Found);
   Make ("plain", Write_Already_Exists);
   Make ("d1/sub", Write_Already_Exists);
   Remove_Directory ("d1/sub");
   Sync (7);
   Remove_Directory ("d1");
   Sync (8);
   for Index in 1 .. Many loop
      Make (Many_Name (Index));
   end loop;
   Sync (9);
   for Index in reverse 1 .. Many loop
      Remove_Directory (Many_Name (Index));
   end loop;
   Sync (10);
   Detach (Fs, Flushed);
   Expect ("detach", Flushed = Flush_Complete, Flushed'Image);
   Put_Line ("DETACHED");
   Close_Image;
end Namespace;
