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
with Install_Tree;

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

   ---------------------------------------------------------------------------
   --  The files (Install_Tree) laid out at format time: every directory
   --  takes one cluster (at most Entries_Per_Cluster entries), each file a
   --  contiguous run, all allocated in order from cluster 3 (the root is
   --  cluster 2): directories first, then files. Names are 8.3 (no long
   --  file names): the payload's ESP names must fit it.
   ---------------------------------------------------------------------------
   Directory_Entry_Bytes : constant := 32;
   Entries_Per_Cluster   : constant := Cluster_Bytes / Directory_Entry_Bytes;
   subtype Short_Name is String (1 .. 11);

   --  Source as an 8.3 directory name (upper case, space padded). Valid:
   --  1-8 name characters, an optional 1-3 character extension, from
   --  A-Z 0-9 _ - (lower case is folded).
   procedure To_Short_Name (Source : String; Result : out Short_Name; Valid : out Boolean);

   type Cluster_Numbers is array (Install_Tree.Entry_Index) of Unsigned_32;

   type Tree_Layout is record
      --  The tree fits: valid names, small enough directories, clusters.
      Fits     : Boolean := False;
      Start    : Cluster_Numbers := [others => 0];   --  0: an empty file
      Clusters : Cluster_Numbers := [others => 0];
      --  One past the last cluster allocated.
      Next     : Unsigned_32 := Root_Cluster + 1;
   end record;

   procedure Plan_Tree
     (Item : Volume; Files : Install_Tree.Tree; Result : out Tree_Layout)
     with Pre => Valid (Item),
          Post => (if Result.Fits then
                     Result.Next >= Root_Cluster + 1
                     and then Unsigned_64 (Result.Next) <= Unsigned_64 (Item.Clusters) + Root_Cluster);

   --  The free clusters and the next free one reflect Layout.
   function FSInfo (Item : Volume; Layout : Tree_Layout) return Sector_Image
     with Pre => Valid (Item) and then Layout.Fits,
          Post => FSInfo'Result'First = 1
                  and then FSInfo'Result'Length = Item.Sector_Bytes;

   --  How many sectors at the start of each FAT hold allocated entries.
   function Used_FAT_Sectors (Item : Volume; Layout : Tree_Layout) return Unsigned_64
     with Pre => Valid (Item) and then Layout.Fits,
          Post => Used_FAT_Sectors'Result <= Unsigned_64 (Item.FAT_Sectors);

   --  FAT sector Index: entries 0 and 1 (media, clean), the root's chain
   --  and every allocated run; free entries are zero.
   function FAT_Sector
     (Item : Volume; Layout : Tree_Layout; Files : Install_Tree.Tree; Index : Unsigned_64)
      return Sector_Image
     with Pre => Valid (Item) and then Layout.Fits,
          Post => FAT_Sector'Result'First = 1
                  and then FAT_Sector'Result'Length = Item.Sector_Bytes;

   subtype Cluster_Image is Bytes (1 .. Cluster_Bytes);

   --  A directory's cluster (Volume_Root: the root, with the label first).
   function Directory_Cluster
     (Layout : Tree_Layout; Files : Install_Tree.Tree;
      Directory : Install_Tree.Parent_Index; Label : Volume_Label) return Cluster_Image
     with Pre => Layout.Fits and then Directory <= Files.Count;

   --  The sector a cluster starts at, from the volume's start.
   function Cluster_Sector (Item : Volume; Cluster : Unsigned_32) return Unsigned_64 is
     (Data_Start (Item) + Unsigned_64 (Cluster - Root_Cluster) * Unsigned_64 (Item.Sectors_Per_Cluster))
     with Pre => Cluster >= Root_Cluster;

end FAT32_Image;
