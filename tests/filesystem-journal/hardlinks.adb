with CuBit.Handles.Fixture;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with Ext2_Support; use type Ext2_Support.File_Admission;
with CuBit.Messages; use CuBit.Messages;
with Volume_Admission; use Volume_Admission;

--  FS-031: files with several names (hard links), on a mke2fs-made ext2 or
--  ext3 image whose staged tree has them (mke2fs -d keeps hard links),
--  driven by hardlinks.py. Unlinking one name keeps the file for its other
--  names (link count down by one, nothing freed, not orphaned); the last
--  name frees it, at once or (held) at the last close. A rename over one
--  name of a linked file is the same. "SYNCED k" follows step k's flush.
--  Staged: one = two (links 2), keep/three = keep/four (2), held = held2
--  (2), plain (1). Each file's bytes are given by hardlinks.py.
procedure Hardlinks is
   Fs : Filesystem;
   Admission : Admission_Result;
   Flushed : Flush_Status;
   Removed : Remove_Status;
   Lookup : Directory_Lookup_Status;
   Read_Result : Read_Status;
   Written : Write_Status;
   Renamed : Rename_Status;
   Injecting : constant Boolean := Ada.Environment_Variables.Exists ("CUBIT_FAIL");
   Kib : constant := 1024;
   Appended : constant String (1 .. 3 * Kib) := [others => 'H'];

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

   function Number_Of (Path : String) return Unsigned_32 is
      Number : Unsigned_32;
   begin
      resolvePath (Fs, Path, Number, Lookup);
      Expect ("lookup " & Path, Lookup = Lookup_Found, Lookup'Image);
      return Number;
   end Number_Of;

   procedure Absent (Path : String) is
      Number : Unsigned_32;
   begin
      resolvePath (Fs, Path, Number, Lookup);
      Expect ("gone " & Path, Lookup = Lookup_Not_Found, Lookup'Image);
   end Absent;

   function Inode_Of (Number : Unsigned_32) return Inode is
      Item : Inode;
   begin
      readInode (Fs, Number, Item, Read_Result);
      Expect ("read inode", Read_Result = Read_Complete, Read_Result'Image);
      return Item;
   end Inode_Of;

   procedure Links (Path : String; Count : Unsigned_16) is
      Item : constant Inode := Inode_Of (Number_Of (Path));
   begin
      Expect ("links " & Path, Item.numHardLinks = Count and then
              Ext2_Support.Check_File (Item) = Ext2_Support.File_Allowed,
              Item.numHardLinks'Image);
   end Links;

   --  The first Size bytes of Path; its whole size must be Size.
   procedure Same_Size (Path : String; Size : Unsigned_64) is
      Item : constant Inode := Inode_Of (Number_Of (Path));
   begin
      Expect ("size " & Path, fileSize (Item) = Size, fileSize (Item)'Image);
   end Same_Size;

   --  Unlink one name of Number's file: it keeps Remaining links.
   procedure Unlink_Name
     (Path : String; Held : Boolean; Number : Unsigned_32; Remaining : Unsigned_16)
   is
      Released : Unsigned_32;
      Item : Inode;
   begin
      unlinkPath (Fs, Path, Held, Released, Item, Removed);
      Expect ("unlink " & Path, Removed = Remove_Complete, Removed'Image);
      Absent (Path);
      if Remaining = 0 then
         Expect ("released " & Path, Released = Number and then Item.numHardLinks = 0,
                 Released'Image);
      else
         --  Neither released nor orphaned: the file lives on.
         Expect ("kept " & Path, Released = 0 and then Item.numHardLinks = Remaining and then
                 Item.deletedTime = 0, Released'Image & Item.numHardLinks'Image);
         Expect ("kept on disk " & Path, Inode_Of (Number).numHardLinks = Remaining and then
                 Inode_Of (Number).deletedTime = 0, "");
      end if;
   end Unlink_Name;

   One, Three, Held, Plain : Unsigned_32;
begin
   Open_Image (Ada.Command_Line.Argument (1));
   initBlockDevice (Fs, CuBit.Handles.Fixture.Endpoint_Of (1), 1,
                    (slot => 1, generation => 1), Grant_Buffer'Address,
                    Grant_Buffer'Length, Admission);
   Expect ("admission", Admission = Admitted, Admission'Image);

   --  Two names, one inode: both open as an ordinary file.
   One := Number_Of ("one");
   Expect ("one = two", Number_Of ("two") = One, "");
   Links ("one", 2);
   Three := Number_Of ("keep/three");
   Expect ("three = four", Number_Of ("keep/four") = Three, "");
   Links ("keep/three", 2);
   Held := Number_Of ("held");
   Links ("held", 2);
   Plain := Number_Of ("plain");
   Links ("plain", 1);

   --  1. One name goes; the other keeps the file, its bytes and one link.
   Unlink_Name ("one", False, One, 1);
   Links ("two", 1);
   Sync (1);

   --  2. A rename over one name of a linked file: the replaced name's
   --  file keeps its other name; the renamed file takes the name.
   declare
      Replaced_Number : Unsigned_32;
      Replaced : Inode;
   begin
      renamePath (Fs, "plain", "keep/four", False, Replaced_Number, Replaced, Renamed);
      Expect ("rename over keep/four", Renamed = Rename_Complete and then
              Replaced_Number = 0 and then Replaced.numHardLinks = 1,
              Renamed'Image & Replaced_Number'Image);
      Absent ("plain");
      Expect ("keep/four is plain", Number_Of ("keep/four") = Plain, "");
      Links ("keep/four", 1);
      Links ("keep/three", 1);
   end;
   Sync (2);

   --  3. One name of a file a handle holds: still not orphaned (it has a
   --  name); the handle keeps writing it, its other name sees that.
   Unlink_Name ("held", True, Held, 1);
   declare
      Item : Inode := Inode_Of (Held);
      Size : constant Unsigned_64 := fileSize (Item);
      Completed : Unsigned_64;
      Back : String (1 .. Appended'Length);
   begin
      writeData (Fs, Held, Item, Size, Appended'Address, Appended'Length, Completed, Written);
      Expect ("write held", Written = Write_Complete and then Completed = Appended'Length,
              Written'Image);
      Expect ("still one link", Inode_Of (Held).numHardLinks = 1, "");
      Same_Size ("held2", Size + Appended'Length);
      readData (Fs, Inode_Of (Number_Of ("held2")), Size, Back'Address, Back'Length,
                Completed, Read_Result);
      Expect ("read held2", Read_Result = Read_Complete and then Completed = Back'Length and then
              Back = Appended, Read_Result'Image);
   end;
   Sync (3);

   --  4. The last name of a file nobody holds: freed now.
   Unlink_Name ("two", False, One, 0);
   Sync (4);

   --  5. The last name of a held file: orphaned until the last close.
   Unlink_Name ("held2", True, Held, 0);
   declare
      Item : constant Inode := Inode_Of (Held);
      Back : String (1 .. Appended'Length);
      Completed : Unsigned_64;
   begin
      readData (Fs, Item, fileSize (Item) - Appended'Length, Back'Address, Back'Length,
                Completed, Read_Result);
      Expect ("read held orphan", Read_Result = Read_Complete and then Back = Appended,
              Read_Result'Image);
   end;
   Sync (5);
   reclaimInode (Fs, Held, Removed);
   Expect ("reclaim held", Removed = Remove_Complete, Removed'Image);
   Sync (6);

   --  6. The last name of the rename's survivor.
   Unlink_Name ("keep/three", False, Three, 0);
   Sync (7);

   Detach (Fs, Flushed);
   Expect ("detach", Flushed = Flush_Complete, Flushed'Image);
   Put_Line ("DETACHED");
   Close_Image;
end Hardlinks;
