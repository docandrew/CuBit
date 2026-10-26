------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Writes the whole installer layout to a block sink: the ESP formatted
--  FAT32, the root formatted ext2, then the GPT (backup copy first, the
--  protective MBR last), so an interrupted run never leaves a partition
--  table that points at unformatted partitions. Every region a structure
--  relies on being zero (FATs, bitmaps, inode tables, the root cluster) is
--  zeroed explicitly: the disk's old contents are never trusted.
--
--  Put and Zero take byte offsets from the disk's start, always multiples
--  of the device block, with Data a whole number of device blocks; they
--  report failure, and the writer stops at the first one. The caller
--  flushes the device after Written.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;
with FAT32_Image;
with Ext2_Image;

generic
   with procedure Put (Offset : Byte_Offset; Data : Bytes; OK : out Boolean);
   with procedure Zero (Offset : Byte_Offset; Length : Unsigned_64; OK : out Boolean);
package Layout_Writer with SPARK_Mode is

   type Install_Layout is record
      Disk       : Layout_Request;
      ESP_Serial : Unsigned_32 := 0;
      ESP_Label  : FAT32_Image.Volume_Label := "CUBIT ESP  ";
      Root       : Ext2_Image.Volume_Request;
   end record;

   type Write_Result is (Written, Device_Failed, Layout_Invalid);

   procedure Write_Disk
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk);

end Layout_Writer;
