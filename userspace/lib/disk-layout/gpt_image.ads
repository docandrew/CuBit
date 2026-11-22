------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Encodes the installer's GPT (UEFI 2.10 5.2-5.3): the protective MBR,
--  the entry array (ESP, root) and both headers with their CRC32s.
--  Pure and proved free of run-time errors; Partition_Tables' decoder
--  reads every encoded header back as valid (tests/disk-layout).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;

package GPT_Image with SPARK_Mode, Pure is

   subtype Block_Image is Bytes;

   --  LBA 0: one protective record covering the disk.
   function Protective_MBR (Shape : Disk_Shape) return Block_Image
     with Pre => Shape.Blocks in 2 .. Maximum_Disk_Blocks,
          Post => Protective_MBR'Result'First = 1
                  and then Protective_MBR'Result'Length = Shape.Block_Size;

   subtype Entry_Array is Bytes (1 .. Entry_Array_Bytes);

   ESP_Name  : constant String := "EFI system partition";
   Root_Name : constant String := "CuBit root";

   function Entries (Shape : Disk_Shape; Request : Layout_Request)
     return Entry_Array
     with Pre => Fits (Shape, Request);

   type Header_Copy is (Primary, Backup);

   --  Where each copy's header and entry array live.
   function Header_LBA (Shape : Disk_Shape; Copy : Header_Copy)
     return Block_Address is
     (case Copy is
        when Primary => 1,
        when Backup  => Shape.Blocks - 1)
     with Pre => Shape.Blocks in 2 .. Maximum_Disk_Blocks;

   function Entries_LBA (Shape : Disk_Shape; Copy : Header_Copy)
     return Block_Address is
     (case Copy is
        when Primary => 2,
        when Backup  => Shape.Blocks - 1 - Entry_Blocks (Shape))
     with Pre => Shape.Blocks in 2 + Entry_Blocks (Shape) .. Maximum_Disk_Blocks;

   function Header
     (Shape : Disk_Shape; Request : Layout_Request; Copy : Header_Copy;
      Entries_CRC : Unsigned_32) return Block_Image
     with Pre => Fits (Shape, Request),
          Post => Header'Result'First = 1
                  and then Header'Result'Length = Shape.Block_Size;

end GPT_Image;
