------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Formats the EFI system partition as FAT32 (Microsoft FAT specification
--  1.03; UEFI 2.10 13.3): the volume's plan, its boot sector, FSInfo, the
--  first sector of each FAT and the root directory's label entry. Sectors
--  are the device's logical blocks (512 or 4096 bytes, as UEFI requires);
--  clusters are 4 KiB. Pure and proved free of run-time errors.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Disk_Layout; use Disk_Layout;

package FAT32_Image with SPARK_Mode, Pure is

   Cluster_Bytes      : constant := 4096;
   Reserved_Sectors   : constant := 32;
   FAT_Count          : constant := 2;
   Root_Cluster       : constant := 2;
   FSInfo_Sector      : constant := 1;
   Backup_Boot_Sector : constant := 6;
   --  FAT32 by definition has at least this many clusters (spec 3.5), and
   --  cluster numbers stop below the reserved range.
   Minimum_Clusters   : constant := 65_525;
   Maximum_Clusters   : constant := 16#0FFF_FFF5#;
   --  The volume is at most 64 GiB (Disk_Layout.ESP_MiB).
   Maximum_Sectors    : constant := 2 ** 27;

   subtype Volume_Label is String (1 .. 11);
   subtype Sector_Count is Unsigned_32 range 0 .. Maximum_Sectors;

   type Volume is record
      Sector_Bytes        : Device_Block_Bytes := 4096;
      Sectors             : Sector_Count := 0;
      Sectors_Per_Cluster : Unsigned_32 := 1;
      FAT_Sectors         : Sector_Count := 0;
      Clusters            : Unsigned_32 := 0;
      --  The partition's first LBA (BPB_HiddSec).
      Hidden_Sectors      : Unsigned_32 := 0;
   end record;

   function Valid (Item : Volume) return Boolean is
     (Item.Sectors_Per_Cluster = Cluster_Bytes / Unsigned_32 (Item.Sector_Bytes)
      and then Item.FAT_Sectors >= 1
      and then Item.Clusters in Minimum_Clusters .. Maximum_Clusters
      and then Unsigned_64 (Item.Sectors) >=
        Reserved_Sectors + FAT_Count * Unsigned_64 (Item.FAT_Sectors)
          + Unsigned_64 (Item.Clusters) * Unsigned_64 (Item.Sectors_Per_Cluster)
      --  Every cluster has a FAT entry (4 bytes each, two reserved).
      and then (Unsigned_64 (Item.Clusters) + 2) * 4 <=
        Unsigned_64 (Item.FAT_Sectors) * Unsigned_64 (Item.Sector_Bytes));

   --  The plan for a partition of Sectors device blocks starting at LBA
   --  First. Valid (Plan) holds when the partition is large enough for
   --  FAT32 (Disk_Layout.Minimum_ESP_MiB is).
   function Plan
     (Sector_Bytes : Device_Block_Bytes; Sectors : Sector_Count;
      First : Unsigned_32) return Volume
     with Post => Plan'Result.Sector_Bytes = Sector_Bytes
                  and then Plan'Result.Sectors = Sectors;

   --  Where the regions start, in sectors from the volume's start.
   function FAT_Start (Item : Volume; Copy : Positive) return Unsigned_64 is
     (Reserved_Sectors + Unsigned_64 (Copy - 1) * Unsigned_64 (Item.FAT_Sectors))
     with Pre => Copy <= FAT_Count;
   function Data_Start (Item : Volume) return Unsigned_64 is
     (Reserved_Sectors + FAT_Count * Unsigned_64 (Item.FAT_Sectors));

   subtype Sector_Image is Bytes;

   function Boot_Sector
     (Item : Volume; Volume_Id : Unsigned_32; Label : Volume_Label)
      return Sector_Image
     with Pre => Valid (Item),
          Post => Boot_Sector'Result'First = 1
                  and then Boot_Sector'Result'Length = Item.Sector_Bytes;

   function FSInfo (Item : Volume) return Sector_Image
     with Pre => Valid (Item),
          Post => FSInfo'Result'First = 1
                  and then FSInfo'Result'Length = Item.Sector_Bytes;

   --  Entries 0 and 1 (media, clean) and 2 (the root directory's one
   --  cluster, end of chain); the rest of each FAT is zero (free).
   function First_FAT_Sector (Item : Volume) return Sector_Image
     with Pre => Valid (Item),
          Post => First_FAT_Sector'Result'First = 1
                  and then First_FAT_Sector'Result'Length = Item.Sector_Bytes;

   --  The root directory's first sector: the volume label entry.
   function Root_Directory_Sector (Item : Volume; Label : Volume_Label)
     return Sector_Image
     with Pre => Valid (Item),
          Post => Root_Directory_Sector'Result'First = 1
                  and then Root_Directory_Sector'Result'Length = Item.Sector_Bytes;

end FAT32_Image;
