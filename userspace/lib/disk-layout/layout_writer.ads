------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Writes the whole install to a block sink: the ESP formatted FAT32 with
--  its files (the boot files), the root formatted ext2 with its files (the
--  image closure), then the GPT (backup copy first, the protective MBR
--  last), so an interrupted run never leaves a partition table that points
--  at unformatted partitions. Every region a structure relies on being
--  zero (FATs, bitmaps, inode tables, directory clusters) is written or
--  zeroed explicitly: the disk's old contents are never trusted.
--
--  Put and Zero take byte offsets from the disk's start, always multiples
--  of the device block, with Data a whole number of device blocks (at most
--  Batch_Bytes); they report failure, and the writer stops at the first.
--  Read supplies a file's bytes: exactly Data'Length of them from Offset,
--  never past the file's size. The caller flushes the device after Written.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with FAT32_Image;
with Ext2_Image;
with Install_Tree;

generic
   with procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean);
   with procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean);
   with procedure Read
     (Volume : Install_Volume; File : Install_Tree.Entry_Index; Offset : Unsigned_64;
      Data : out Bytes; OK : out Boolean);
package Layout_Writer
  with SPARK_Mode,
       --  The files' layouts (tens of KiB): kept here, not on the stack.
       Abstract_State => Layouts
is

   --  The largest Put: 16 blocks of 4 KiB (one UFS slot's buffer).
   Batch_Bytes : constant := 65_536;

   type Install_Layout is record
      Disk       : Layout_Request;
      ESP_Serial : Unsigned_32 := 0;
      ESP_Label  : FAT32_Image.Volume_Label := [others => ' '];
      Root       : Ext2_Image.Volume_Request;
   end record;

   type Write_Result is
     (Written, Device_Failed, Source_Failed, Layout_Invalid, ESP_Files_Do_Not_Fit,
      Root_Files_Do_Not_Fit);

   procedure Write_Disk
     (Shape : Disk_Shape; Layout : Install_Layout;
      ESP_Files, Root_Files : Install_Tree.Tree; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => Layouts);

end Layout_Writer;
