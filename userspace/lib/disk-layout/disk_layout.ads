------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The installer's disk layout (SYS-001, HW-001): a GPT with two
--  partitions, an EFI system partition (FAT32) and the ext2 root, on a disk
--  of 512- or 4096-byte logical blocks. Pure and proved (SPARK): the plan
--  is computed here, and the encoders (GPT_Image, FAT32_Image, Ext2_Image)
--  build every on-disk structure from it. Nothing here touches a device:
--  writers are generic over a block sink.
--
--  Layout (UEFI 2.10 5.3; 1 MiB alignment as Linux and Windows use):
--    LBA 0                      protective MBR
--    LBA 1                      primary GPT header
--    LBA 2 ..                   primary entry array (128 x 128 bytes)
--    1 MiB ..                   EFI system partition (FAT32)
--    next 1 MiB boundary ..     root (ext2), to the last usable block
--    .. LBA N-2                 backup entry array
--    LBA N-1                    backup GPT header
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Partition_Tables;

package Disk_Layout with SPARK_Mode, Pure is

   subtype Bytes is Partition_Tables.Bytes;
   subtype GUID is Partition_Tables.GUID;

   --  Logical block sizes the layout supports (the IdeaPad's UFS: 4096).
   subtype Device_Block_Bytes is Positive range 512 .. 4096
     with Static_Predicate => Device_Block_Bytes in 512 | 4096;

   --  Disks up to 2 ** 40 blocks (4 PiB at 4 KiB): every byte offset fits
   --  in 64 bits with room to spare.
   Maximum_Disk_Blocks : constant := 2 ** 40;
   subtype Block_Address is Unsigned_64 range 0 .. Maximum_Disk_Blocks - 1;
   subtype Block_Count is Unsigned_64 range 0 .. Maximum_Disk_Blocks;
   subtype Byte_Offset is Unsigned_64 range 0 .. Maximum_Disk_Blocks * 4096;

   Mebibyte : constant := 2 ** 20;
   Alignment_Bytes : constant := Mebibyte;

   --  GPT entry array: UEFI's minimum, 128 entries of 128 bytes.
   Entry_Count : constant := 128;
   Entry_Bytes : constant := Partition_Tables.Minimum_Entry_Bytes;
   Entry_Array_Bytes : constant := Entry_Count * Entry_Bytes;

   --  The ESP's size is the caller's choice, in MiB: at least what FAT32
   --  needs (65525 clusters of 4 KiB, plus its FATs), at most 64 GiB.
   Minimum_ESP_MiB : constant := 260;
   subtype ESP_MiB is Unsigned_64 range Minimum_ESP_MiB .. 65_536;

   --  The root is at least this large (room for the image closure).
   Minimum_Root_MiB : constant := 512;

   --  What the caller decides: nothing is defaulted.
   type Layout_Request is record
      ESP_Size     : ESP_MiB := Minimum_ESP_MiB;
      Disk_Id      : GUID := [others => 0];
      ESP_Id       : GUID := [others => 0];
      Root_Id      : GUID := [others => 0];
   end record;

   --  The two filesystems an install writes.
   type Install_Volume is (ESP_Volume, Root_Volume);

   type Disk_Shape is record
      Block_Size : Device_Block_Bytes := 4096;
      Blocks     : Block_Count := 0;
   end record;

   function Entry_Blocks (Shape : Disk_Shape) return Block_Count is
     (Block_Count (Entry_Array_Bytes / Shape.Block_Size));

   function Alignment_Blocks (Shape : Disk_Shape) return Block_Count is
     (Block_Count (Alignment_Bytes / Shape.Block_Size));

   function MiB_Blocks (Shape : Disk_Shape; MiB : Unsigned_64) return Block_Count is
     (Block_Count (MiB * (Mebibyte / Unsigned_64 (Shape.Block_Size))))
     with Pre => MiB <= ESP_MiB'Last;

   --  The GPT's own blocks at the end of the disk, and the usable range.
   function First_Usable (Shape : Disk_Shape) return Block_Address is
     (2 + Entry_Blocks (Shape));
   function Last_Usable (Shape : Disk_Shape) return Block_Address is
     (Shape.Blocks - 2 - Entry_Blocks (Shape))
     with Pre => Shape.Blocks > 2 + Entry_Blocks (Shape);

   function ESP_First (Shape : Disk_Shape) return Block_Address is
     (Alignment_Blocks (Shape));
   function ESP_Last (Shape : Disk_Shape; Request : Layout_Request)
     return Block_Address is
     (ESP_First (Shape) + MiB_Blocks (Shape, Request.ESP_Size) - 1);
   function Root_First (Shape : Disk_Shape; Request : Layout_Request)
     return Block_Address is
     (ESP_Last (Shape, Request) + 1);   --  already 1 MiB aligned

   --  The disk holds the layout: both GPT copies, the ESP, and a root of at
   --  least Minimum_Root_MiB.
   function Fits (Shape : Disk_Shape; Request : Layout_Request) return Boolean is
     (Shape.Blocks >
        Root_First (Shape, Request) + MiB_Blocks (Shape, Minimum_Root_MiB)
          + 2 + Entry_Blocks (Shape));

   function Root_Last (Shape : Disk_Shape; Request : Layout_Request)
     return Block_Address is
     (Last_Usable (Shape))
     with Pre => Fits (Shape, Request);

   ---------------------------------------------------------------------------
   --  Little-endian fields in a block buffer, at 0-based byte offsets.
   ---------------------------------------------------------------------------
   procedure Put_8 (Data : in out Bytes; Offset : Natural; Value : Unsigned_8)
     with Pre => Data'First = 1 and then Offset < Data'Length,
          Post => Data (Offset + 1) = Value;
   procedure Put_16 (Data : in out Bytes; Offset : Natural; Value : Unsigned_16)
     with Pre => Data'First = 1 and then Data'Length >= 2
                 and then Offset <= Data'Length - 2;
   procedure Put_32 (Data : in out Bytes; Offset : Natural; Value : Unsigned_32)
     with Pre => Data'First = 1 and then Data'Length >= 4
                 and then Offset <= Data'Length - 4;
   procedure Put_64 (Data : in out Bytes; Offset : Natural; Value : Unsigned_64)
     with Pre => Data'First = 1 and then Data'Length >= 8
                 and then Offset <= Data'Length - 8;
   --  Text as ASCII bytes, at most Data's room from Offset.
   procedure Put_Text (Data : in out Bytes; Offset : Natural; Text : String)
     with Pre => Data'First = 1 and then Text'Length <= Data'Length
                 and then Offset <= Data'Length - Text'Length;
   procedure Put_GUID (Data : in out Bytes; Offset : Natural; Value : GUID)
     with Pre => Data'First = 1 and then Data'Length >= 16
                 and then Offset <= Data'Length - 16;

end Disk_Layout;
