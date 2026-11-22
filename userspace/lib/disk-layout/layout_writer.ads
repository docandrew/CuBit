------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Writes the whole install to a block device in Install_Steps' order, and
--  reads every part back before the disk is made bootable (SYS-001
--  installer v2, SYS-002):
--
--    Partitioning        zero the ESP's reserved sectors (its boot sector
--                        and backup) and the root's first block, so neither
--                        is a filesystem until its last write below; then
--                        the GPT (backup copy first, the protective MBR
--                        last), read back and compared
--    Formatting_Root     the root's ext2 metadata, except the group-0
--                        superblock
--    Filling_Storehouse  the root's files in tree order (Root_Files'
--    Writing_Generation  entries from Layout.Generation_First on are the
--                        generation records), then the group-0 superblock
--    Verifying_Root      every root block written read back: metadata
--                        compared with what was written, file bytes handed
--                        to Check (the caller compares their SHA-256 with
--                        the source's)
--    Writing_ESP         the ESP without its boot sectors and FSInfo
--    Verifying_ESP       read back likewise
--    Making_Bootable     after Flush: FSInfo, the backup boot sector, then
--                        the boot sector, each read back
--
--  Every region a structure relies on being zero (FATs, bitmaps, inode
--  tables, directory clusters) is written or zeroed explicitly: the disk's
--  old contents are never trusted. Enter reports each step as it starts.
--
--  Put and Zero take byte offsets from the disk's start, always multiples
--  of the device block, with Data a whole number of device blocks (at most
--  Batch_Bytes); Get reads likewise. They report failure, and the writer
--  stops at the first. Read supplies a file's bytes: exactly Data'Length
--  of them from Offset, in increasing order, never past the file's size.
--  Check receives a file's bytes as read back from the disk, in order from
--  offset 0 (an empty file: once, empty); Final marks its last piece, where
--  Matches says whether the file's bytes are the ones Read gave. Flush
--  makes every completed write durable (the device's cache).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with FAT32_Image;
with Ext2_Image;
with Install_Steps;
with Install_Tree;

generic
   with procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean);
   with procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean);
   with procedure Get (Offset : Byte_Offset; Data : out Bytes; OK : out Boolean);
   with procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean);
   with procedure Check
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : Bytes; Final : Boolean; Matches : out Boolean);
   with procedure Flush (OK : out Boolean);
   with procedure Enter (Step : Install_Steps.Step);
package Layout_Writer
  with SPARK_Mode,
       --  The files' layouts (tens of KiB): kept here, not on the stack.
       Abstract_State => Layouts
is

   use type Install_Steps.Step;

   --  The largest Put: 16 blocks of 4 KiB (one UFS slot's buffer).
   Batch_Bytes : constant := 65_536;

   type Install_Layout is record
      Disk       : Layout_Request;
      ESP_Serial : Unsigned_32 := 0;
      ESP_Label  : FAT32_Image.Volume_Label := [others => ' '];
      Root       : Ext2_Image.Volume_Request;
      --  The root tree's first generation record (Writing_Generation from
      --  there on); past the tree's count: none.
      Generation_First : Install_Tree.Entry_Index := Install_Tree.Entry_Index'Last;
   end record;

   type Write_Result is
     (Written,
      Device_Failed,          --  a write, read or flush failed
      Source_Failed,          --  a file could not be read, or changed under the install
      Layout_Invalid,
      ESP_Files_Do_Not_Fit,
      Root_Files_Do_Not_Fit,
      Verify_Failed);         --  what was read back is not what was written

   --  Reached: the last step entered (Done once Written). Only a Written
   --  result has passed Making_Bootable; any other leaves the disk
   --  unbootable (Install_Steps).
   procedure Write_Disk
     (Shape : Disk_Shape; Layout : Install_Layout;
      ESP_Files, Root_Files : Install_Tree.Tree; Result : out Write_Result;
      Reached : out Install_Steps.Step)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => Layouts),
          Post => (if Result = Written then Reached = Install_Steps.Done
                   else Install_Steps.Rank (Reached) <= Install_Steps.Rank (Install_Steps.Making_Bootable));

end Layout_Writer;
