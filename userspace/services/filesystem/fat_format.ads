------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The FAT32 on-disk format (FS-030): its numbers and the codecs of its
--  byte-level structures: the boot sector's BIOS parameter block, the
--  FSInfo sector, FAT entries, short directory entries and VFAT long-name
--  parts, and FAT dates and times. Pure and proved (SPARK); no storage, no
--  addresses. Every byte comes from an untrusted disk: decoders refuse
--  rather than assume.
--
--  @description
--  Offsets, values and rules follow Microsoft's "FAT32 File System
--  Specification" (FATGEN 1.03, 2000), cited below as FATGEN with its
--  section. VFAT long names follow the same document's "FAT Long Directory
--  Entries" section.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Fat_Format with Pure, SPARK_Mode is

   type Byte_Array is array (Natural range <>) of Unsigned_8;

   --  Little-endian fields.
   function Le16 (Data : Byte_Array; Position : Natural) return Unsigned_16 is
     (Unsigned_16 (Data (Position)) or
      Shift_Left (Unsigned_16 (Data (Position + 1)), 8))
     with Pre => Position >= Data'First and then Position < Data'Last;

   function Le32 (Data : Byte_Array; Position : Natural) return Unsigned_32 is
     (Unsigned_32 (Data (Position)) or
      Shift_Left (Unsigned_32 (Data (Position + 1)), 8) or
      Shift_Left (Unsigned_32 (Data (Position + 2)), 16) or
      Shift_Left (Unsigned_32 (Data (Position + 3)), 24))
     with Pre => Position >= Data'First and then Data'Last >= 3
                 and then Position <= Data'Last - 3;

   procedure Put16 (Data : in out Byte_Array; Position : Natural; Value : Unsigned_16)
     with Pre  => Position >= Data'First and then Position < Data'Last,
          Post => Le16 (Data, Position) = Value
                  and then (for all I in Data'Range =>
                              (if I /= Position and then I /= Position + 1
                               then Data (I) = Data'Old (I)));

   procedure Put32 (Data : in out Byte_Array; Position : Natural; Value : Unsigned_32)
     with Pre  => Position >= Data'First and then Data'Last >= 3
                  and then Position <= Data'Last - 3,
          Post => Le32 (Data, Position) = Value
                  and then (for all I in Data'Range =>
                              (if I not in Position .. Position + 3
                               then Data (I) = Data'Old (I)));

   ---------------------------------------------------------------------------
   --  Numbers
   ---------------------------------------------------------------------------
   --  A sector of the volume (BPB_TotSec32 is 32-bit), a count of sectors.
   Word_Last : constant := 2 ** 32 - 1;
   type Sector_Number is range 0 .. Word_Last with Size => 32;
   --  Volume positions and sizes in bytes, and sums of sector counts.
   type Byte_Count is range 0 .. 2 ** 62;

   --  A FAT32 cluster number is 28 bits; the top four bits of an entry are
   --  reserved and preserved (FATGEN section 4).
   Entry_Mask : constant Unsigned_32 := 16#0FFF_FFFF#;
   Reserved_Bits : constant Unsigned_32 := 16#F000_0000#;
   type Cluster_Number is range 0 .. 16#0FFF_FFFF# with Size => 32;
   No_Cluster : constant Cluster_Number := 0;
   First_Data_Cluster : constant Cluster_Number := 2;
   --  The highest cluster count FAT32 can address: clusters 2 .. 16#0FFF_FFF6#
   --  (16#0FFF_FFF7# is the bad-cluster mark, FATGEN section 3.5).
   Maximum_Clusters : constant := 16#0FFF_FFF5#;
   subtype Cluster_Count is Cluster_Number range 0 .. Maximum_Clusters;
   subtype Data_Cluster is Cluster_Number range First_Data_Cluster .. Maximum_Clusters + 1;
   --  The FAT type is decided by the count of clusters alone (FATGEN
   --  section 3.5): below 4085 FAT12, below 65525 FAT16, else FAT32.
   Fat16_Minimum_Clusters : constant := 4_085;
   Fat32_Minimum_Clusters : constant := 65_525;

   ---------------------------------------------------------------------------
   --  Boot sector and BIOS parameter block (FATGEN section 3)
   ---------------------------------------------------------------------------
   Boot_Sector_Bytes : constant := 512;
   subtype Boot_Sector is Byte_Array (0 .. Boot_Sector_Bytes - 1);

   Jump_Offset              : constant := 0;    --  BS_jmpBoot
   Bytes_Per_Sector_Offset  : constant := 11;   --  BPB_BytsPerSec
   Sectors_Per_Cluster_Offset : constant := 13; --  BPB_SecPerClus
   Reserved_Sectors_Offset  : constant := 14;   --  BPB_RsvdSecCnt
   Fat_Count_Offset         : constant := 16;   --  BPB_NumFATs
   Root_Entries_Offset      : constant := 17;   --  BPB_RootEntCnt
   Total_Sectors_16_Offset  : constant := 19;   --  BPB_TotSec16
   Media_Offset             : constant := 21;   --  BPB_Media
   Fat_Sectors_16_Offset    : constant := 22;   --  BPB_FATSz16
   Total_Sectors_32_Offset  : constant := 32;   --  BPB_TotSec32
   Fat_Sectors_32_Offset    : constant := 36;   --  BPB_FATSz32
   Extended_Flags_Offset    : constant := 40;   --  BPB_ExtFlags
   Version_Offset           : constant := 42;   --  BPB_FSVer
   Root_Cluster_Offset      : constant := 44;   --  BPB_RootClus
   Info_Sector_Offset       : constant := 48;   --  BPB_FSInfo
   Backup_Boot_Offset       : constant := 50;   --  BPB_BkBootSec
   --  BS_Reserved1: Windows NT and Linux keep a "volume dirty" bit here.
   Boot_State_Offset        : constant := 65;
   Volume_Id_Offset         : constant := 67;   --  BS_VolID
   Signature_Offset         : constant := 510;  --  16#55#, 16#AA#

   Jump_Short     : constant Unsigned_8 := 16#EB#;   --  EB ?? 90
   Jump_Short_Nop : constant Unsigned_8 := 16#90#;
   Jump_Near      : constant Unsigned_8 := 16#E9#;   --  E9 ?? ??
   Signature_Low  : constant Unsigned_8 := 16#55#;
   Signature_High : constant Unsigned_8 := 16#AA#;
   --  BPB_Media: F0, or F8 .. FF.
   Media_Removable : constant Unsigned_8 := 16#F0#;
   Media_Lowest_Fixed : constant Unsigned_8 := 16#F8#;
   --  BPB_ExtFlags: bit 7 set means only the active FAT (bits 0-3) is
   --  current; clear means every FAT mirrors the first.
   Mirroring_Disabled : constant Unsigned_16 := 16#0080#;
   Active_Fat_Mask    : constant Unsigned_16 := 16#000F#;
   --  BS_Reserved1 bit 0: the volume was not cleanly unmounted.
   Boot_State_Dirty   : constant Unsigned_8 := 16#01#;
   --  BPB_FSInfo and BPB_BkBootSec of 16#FFFF#: none.
   No_Sector_16 : constant Unsigned_16 := 16#FFFF#;

   Minimum_Sector_Shift : constant := 9;    --  512 bytes
   Maximum_Sector_Shift : constant := 12;   --  4096 bytes
   subtype Sector_Shift is Natural range Minimum_Sector_Shift .. Maximum_Sector_Shift;
   Maximum_Cluster_Sector_Shift : constant := 7;   --  128 sectors
   subtype Cluster_Sector_Shift is Natural range 0 .. Maximum_Cluster_Sector_Shift;
   --  FATGEN section 3.1 allows clusters of at most 32 KiB for full
   --  compatibility; Windows and Linux also read 64 KiB, which is the limit
   --  here (sector shift plus cluster shift).
   Maximum_Cluster_Shift : constant := 16;
   Maximum_Cluster_Bytes : constant := 2 ** Maximum_Cluster_Shift;
   subtype Fat_Copies is Natural range 1 .. 2;
   subtype Fat_Index is Natural range 0 .. 1;
   Fat_Entry_Bytes : constant := 4;

   type Geometry is record
      Sector_Bits        : Sector_Shift := Minimum_Sector_Shift;
      Cluster_Bits       : Cluster_Sector_Shift := 0;   --  log2 sectors per cluster
      Reserved_Sectors   : Sector_Number := 1;
      Fats               : Fat_Copies := 1;
      Fat_Sectors        : Sector_Number := 1;          --  per FAT
      Total_Sectors      : Sector_Number := 0;
      Data_Start         : Sector_Number := 0;          --  first sector of cluster 2
      Clusters           : Cluster_Count := 0;
      Root               : Cluster_Number := No_Cluster;
      Info_Sector        : Sector_Number := 0;          --  0: none
      Backup_Sector      : Sector_Number := 0;          --  0: none
      Mirrored           : Boolean := True;
      Active_Fat         : Fat_Index := 0;
      Volume_Id          : Unsigned_32 := 0;
   end record;

   --  Sizes by case, not by exponentiation: the provers reason about them.
   function Sector_Bytes (G : Geometry) return Positive is
     (case G.Sector_Bits is
        when 9 => 512, when 10 => 1024, when 11 => 2048, when 12 => 4096)
     with Post => Sector_Bytes'Result in 512 | 1024 | 2048 | 4096;
   function Cluster_Sectors (G : Geometry) return Positive is
     (case G.Cluster_Bits is
        when 0 => 1, when 1 => 2, when 2 => 4, when 3 => 8,
        when 4 => 16, when 5 => 32, when 6 => 64, when 7 => 128)
     with Post => Cluster_Sectors'Result in 1 .. 128;
   --  FAT entries in one sector.
   function Entries_Per_Sector (G : Geometry) return Positive is
     (case G.Sector_Bits is
        when 9 => 128, when 10 => 256, when 11 => 512, when 12 => 1024)
     with Post => Entries_Per_Sector'Result = Sector_Bytes (G) / Fat_Entry_Bytes;

   --  What a geometry admitted by Decode_Boot satisfies: the regions fit in
   --  the volume, in this order, without overlap; every FAT holds an entry
   --  for every cluster; the root, FSInfo and backup sectors are where they
   --  may be.
   function Valid (G : Geometry) return Boolean is
     (Sector_Bytes (G) * Cluster_Sectors (G) <= Maximum_Cluster_Bytes
      and then G.Reserved_Sectors >= 1
      and then G.Fat_Sectors >= 1
      and then G.Active_Fat < G.Fats
      and then Byte_Count (G.Data_Start) =
                 Byte_Count (G.Reserved_Sectors) + Byte_Count (G.Fats) * Byte_Count (G.Fat_Sectors)
      and then G.Clusters >= Fat32_Minimum_Clusters
      and then Byte_Count (G.Data_Start) +
                 Byte_Count (G.Clusters) * Byte_Count (Cluster_Sectors (G))
               <= Byte_Count (G.Total_Sectors)
      and then Byte_Count (G.Fat_Sectors) * Byte_Count (Entries_Per_Sector (G))
               >= Byte_Count (G.Clusters) + 2
      and then G.Root >= First_Data_Cluster
      and then G.Root <= G.Clusters + 1
      and then G.Info_Sector < G.Reserved_Sectors
      and then G.Backup_Sector < G.Reserved_Sectors);

   --  The last cluster of the data region.
   function Last_Cluster (G : Geometry) return Data_Cluster is (G.Clusters + 1)
     with Pre => Valid (G);

   function Cluster_Bytes (G : Geometry) return Positive is
     (Sector_Bytes (G) * Cluster_Sectors (G))
     with Pre => Valid (G), Post => Cluster_Bytes'Result in 512 .. Maximum_Cluster_Bytes;

   --  The first sector of cluster C: inside the data region, with the
   --  whole cluster inside the volume.
   function Cluster_Sector (G : Geometry; C : Cluster_Number) return Sector_Number
     with Pre  => Valid (G) and then C >= First_Data_Cluster and then C <= Last_Cluster (G),
          Post => Cluster_Sector'Result >= G.Data_Start
                  and then Byte_Count (Cluster_Sector'Result) + Byte_Count (Cluster_Sectors (G))
                           <= Byte_Count (G.Total_Sectors);

   --  Where cluster C's entry is in FAT Copy: the sector, inside that FAT,
   --  and the byte offset in it (4-byte aligned, the whole entry inside).
   function Fat_Sector (G : Geometry; Copy : Fat_Index; C : Cluster_Number) return Sector_Number
     with Pre  => Valid (G) and then Copy < G.Fats and then C <= Last_Cluster (G),
          Post => Fat_Sector'Result >= G.Reserved_Sectors
                  and then Fat_Sector'Result < G.Data_Start
                  and then Byte_Count (Fat_Sector'Result) >=
                             Byte_Count (G.Reserved_Sectors) + Byte_Count (Copy) * Byte_Count (G.Fat_Sectors)
                  and then Byte_Count (Fat_Sector'Result) <
                             Byte_Count (G.Reserved_Sectors) + Byte_Count (Copy + 1) * Byte_Count (G.Fat_Sectors);

   function Fat_Offset (G : Geometry; C : Cluster_Number) return Natural
     with Post => Fat_Offset'Result mod Fat_Entry_Bytes = 0
                  and then Fat_Offset'Result <= Sector_Bytes (G) - Fat_Entry_Bytes;

   --  Why a boot sector is not an admissible FAT32 volume. The checks are
   --  FATGEN section 3's (signature, jump, power-of-two sizes, counts),
   --  section 3.5's FAT-type determination by cluster count, and the
   --  device's own bounds.
   type Boot_Result is
     (Boot_Valid,
      No_Boot_Signature,      --  no 55 AA at offset 510
      Bad_Jump,               --  BS_jmpBoot is neither EB ?? 90 nor E9 ?? ??
      Bad_Sector_Size,        --  not 512, 1024, 2048 or 4096
      Sector_Below_Device,    --  smaller than, or not a multiple of, the device block
      Bad_Cluster_Size,       --  sectors per cluster not a power of two up to 128
      Cluster_Too_Large,      --  more than 64 KiB
      No_Reserved_Sectors,
      Bad_Fat_Count,          --  neither one nor two FATs
      Bad_Media,
      Not_Fat32_Layout,       --  a root directory region or 16-bit sizes (FAT12/16)
      Bad_Version,            --  BPB_FSVer is not 0.0
      No_Fat_Sectors,
      No_Data_Region,         --  the reserved sectors and FATs fill the volume
      Fat12_Or_16,            --  fewer than 65525 clusters: not FAT32
      Too_Many_Clusters,
      Fat_Too_Small,          --  a FAT cannot hold an entry for every cluster
      Bad_Root_Cluster,
      Bad_Info_Sector,
      Bad_Backup_Sector,
      Bad_Active_Fat,
      Beyond_Device);         --  the volume is larger than its device window

   subtype Device_Block_Bytes is Positive range 512 .. 65_536;

   procedure Decode_Boot
     (Data         : Boot_Sector;
      Device_Block : Device_Block_Bytes;
      Device_Bytes : Byte_Count;
      G            : out Geometry;
      Result       : out Boot_Result)
     with Post => (if Result = Boot_Valid then Valid (G)
                                            and then Sector_Bytes (G) mod Device_Block = 0
                                            and then Byte_Count (G.Total_Sectors) * Byte_Count (Sector_Bytes (G))
                                                     <= Device_Bytes);

   --  BS_Reserved1's dirty bit.
   function Boot_Dirty (Data : Boot_Sector) return Boolean is
     ((Data (Boot_State_Offset) and Boot_State_Dirty) /= 0);

   ---------------------------------------------------------------------------
   --  FAT entries (FATGEN section 4)
   ---------------------------------------------------------------------------
   Free_Value        : constant Unsigned_32 := 0;
   Bad_Value         : constant Unsigned_32 := 16#0FFF_FFF7#;
   First_End_Value   : constant Unsigned_32 := 16#0FFF_FFF8#;
   End_Value         : constant Unsigned_32 := 16#0FFF_FFFF#;   --  what this driver writes

   type Entry_Kind is
     (Free_Entry,          --  0
      Next_Entry,          --  a cluster of the data region
      Bad_Entry,           --  16#0FFF_FFF7#
      End_Entry,           --  16#0FFF_FFF8# .. 16#0FFF_FFFF#
      Invalid_Entry);      --  1, or a cluster past the data region

   type Decoded_Entry (Kind : Entry_Kind := Free_Entry) is record
      case Kind is
         when Next_Entry => Next : Data_Cluster;
         when others => null;
      end case;
   end record;

   --  An entry as read (all 32 bits), for a volume whose last cluster is
   --  Last. The top four bits are ignored (FATGEN: "the high 4 bits of a
   --  FAT32 FAT entry are reserved").
   function Decode_Entry (Raw : Unsigned_32; Last : Data_Cluster) return Decoded_Entry
     with Post => (case Decode_Entry'Result.Kind is
                     when Free_Entry => (Raw and Entry_Mask) = Free_Value,
                     when Next_Entry => Decode_Entry'Result.Next <= Last
                                        and then Unsigned_32 (Decode_Entry'Result.Next) =
                                                   (Raw and Entry_Mask),
                     when Bad_Entry => (Raw and Entry_Mask) = Bad_Value,
                     when End_Entry => (Raw and Entry_Mask) >= First_End_Value,
                     when Invalid_Entry => True);

   --  The entry to write in place of Old, holding Value in its low 28 bits
   --  and Old's reserved top four bits.
   function Encode_Entry (Old : Unsigned_32; Value : Unsigned_32) return Unsigned_32 is
     ((Old and Reserved_Bits) or (Value and Entry_Mask))
     with Pre  => Value <= Entry_Mask,
          Post => (Encode_Entry'Result and Entry_Mask) = Value
                  and then (Encode_Entry'Result and Reserved_Bits) = (Old and Reserved_Bits);

   --  FAT[1], the second reserved entry, keeps two volume flags (FATGEN
   --  section 4): set bits mean a clean shutdown and no hard error.
   Clean_Shutdown_Bit : constant Unsigned_32 := 16#0800_0000#;
   No_Hard_Error_Bit  : constant Unsigned_32 := 16#0400_0000#;
   Volume_Flags_Cluster : constant Cluster_Number := 1;

   function Volume_Clean (Raw : Unsigned_32) return Boolean is
     ((Raw and Clean_Shutdown_Bit) /= 0 and then (Raw and No_Hard_Error_Bit) /= 0);
   function Marked_Dirty (Raw : Unsigned_32) return Unsigned_32 is
     (Raw and not Clean_Shutdown_Bit)
     with Post => not Volume_Clean (Marked_Dirty'Result);
   function Marked_Clean (Raw : Unsigned_32) return Unsigned_32 is
     (Raw or Clean_Shutdown_Bit)
     with Post => (Marked_Clean'Result and Clean_Shutdown_Bit) /= 0;

   ---------------------------------------------------------------------------
   --  FSInfo sector (FATGEN section 5): hints only. Nothing here is trusted
   --  for correctness: a hint outside the volume reads as unknown.
   ---------------------------------------------------------------------------
   Info_Lead_Offset   : constant := 0;
   Info_Struct_Offset : constant := 484;
   Info_Free_Offset   : constant := 488;
   Info_Next_Offset   : constant := 492;
   Info_Trail_Offset  : constant := 508;
   Info_Lead_Signature   : constant Unsigned_32 := 16#4161_5252#;
   Info_Struct_Signature : constant Unsigned_32 := 16#6141_7272#;
   Info_Trail_Signature  : constant Unsigned_32 := 16#AA55_0000#;
   Unknown_Hint          : constant Unsigned_32 := 16#FFFF_FFFF#;

   type Info_Hints is record
      Signed     : Boolean := False;           --  all three signatures present
      Free_Known : Boolean := False;
      Free       : Cluster_Count := 0;
      Next_Known : Boolean := False;
      Next       : Data_Cluster := First_Data_Cluster;
   end record;

   function Decode_Info (Data : Boot_Sector; G : Geometry) return Info_Hints
     with Pre  => Valid (G),
          Post => (if Decode_Info'Result.Free_Known then
                     Decode_Info'Result.Signed and then Decode_Info'Result.Free <= G.Clusters)
                  and then (if Decode_Info'Result.Next_Known then
                              Decode_Info'Result.Signed
                              and then Decode_Info'Result.Next <= Last_Cluster (G));

   --  The FSInfo sector with these hints and its signatures; other bytes kept.
   procedure Encode_Info (Data : in out Boot_Sector; Free : Cluster_Count; Next : Data_Cluster)
     with Post => Le32 (Data, Info_Free_Offset) = Unsigned_32 (Free)
                  and then Le32 (Data, Info_Next_Offset) = Unsigned_32 (Next)
                  and then Le32 (Data, Info_Lead_Offset) = Info_Lead_Signature
                  and then Le32 (Data, Info_Struct_Offset) = Info_Struct_Signature
                  and then Le32 (Data, Info_Trail_Offset) = Info_Trail_Signature;

   ---------------------------------------------------------------------------
   --  Directory entries (FATGEN section 6)
   ---------------------------------------------------------------------------
   Entry_Bytes : constant := 32;
   Short_Name_Bytes : constant := 11;
   Base_Bytes : constant := 8;
   Extension_Bytes : constant := 3;
   subtype Short_Name is Byte_Array (0 .. Short_Name_Bytes - 1);
   --  Directories hold at most 65536 entries (FATGEN section 6: 2 MiB).
   Maximum_Directory_Entries : constant := 65_536;
   Maximum_Directory_Bytes : constant := Maximum_Directory_Entries * Entry_Bytes;

   Name_Offset          : constant := 0;    --  DIR_Name
   Attributes_Offset    : constant := 11;   --  DIR_Attr
   Case_Offset          : constant := 12;   --  DIR_NTRes
   Created_Tenth_Offset : constant := 13;   --  DIR_CrtTimeTenth
   Created_Time_Offset  : constant := 14;   --  DIR_CrtTime
   Created_Date_Offset  : constant := 16;   --  DIR_CrtDate
   Accessed_Date_Offset : constant := 18;   --  DIR_LstAccDate
   Cluster_High_Offset  : constant := 20;   --  DIR_FstClusHI
   Written_Time_Offset  : constant := 22;   --  DIR_WrtTime
   Written_Date_Offset  : constant := 24;   --  DIR_WrtDate
   Cluster_Low_Offset   : constant := 26;   --  DIR_FstClusLO
   Size_Offset          : constant := 28;   --  DIR_FileSize

   Attribute_Read_Only : constant Unsigned_8 := 16#01#;
   Attribute_Hidden    : constant Unsigned_8 := 16#02#;
   Attribute_System    : constant Unsigned_8 := 16#04#;
   Attribute_Volume_Id : constant Unsigned_8 := 16#08#;
   Attribute_Directory : constant Unsigned_8 := 16#10#;
   Attribute_Archive   : constant Unsigned_8 := 16#20#;
   Attribute_Long_Name : constant Unsigned_8 := 16#0F#;   --  RO | HIDDEN | SYSTEM | VOLUME_ID
   Attribute_Long_Mask : constant Unsigned_8 := 16#3F#;

   End_Marker   : constant Unsigned_8 := 16#00#;   --  this and later entries are free
   Free_Marker  : constant Unsigned_8 := 16#E5#;   --  a deleted entry
   Kanji_Lead   : constant Unsigned_8 := 16#05#;   --  stands for a leading E5 byte
   Space        : constant Unsigned_8 := 16#20#;
   Dot          : constant Unsigned_8 := 16#2E#;
   --  DIR_NTRes (Windows NT, also Linux and macOS): an all-lower-case base
   --  or extension of a short name with no long name.
   Lower_Base      : constant Unsigned_8 := 16#08#;
   Lower_Extension : constant Unsigned_8 := 16#10#;

   type Directory_Entry_Kind is
     (End_Of_Directory, Deleted, Long_Part_Entry, Volume_Label, Dot_Entry,
      Dot_Dot_Entry, File_Entry, Subdirectory_Entry, Malformed_Entry);

   --  The kind of the entry at Position (32 bytes).
   function Classify (Data : Byte_Array; Position : Natural) return Directory_Entry_Kind
     with Pre => Position >= Data'First and then Data'Last >= Entry_Bytes - 1
                 and then Position <= Data'Last - (Entry_Bytes - 1);

   type Short_Entry is record
      Name          : Short_Name := [others => Space];
      Attributes    : Unsigned_8 := 0;
      Case_Flags    : Unsigned_8 := 0;
      Created_Tenth : Unsigned_8 := 0;
      Created_Time  : Unsigned_16 := 0;
      Created_Date  : Unsigned_16 := 0;
      Accessed_Date : Unsigned_16 := 0;
      Written_Time  : Unsigned_16 := 0;
      Written_Date  : Unsigned_16 := 0;
      First         : Cluster_Number := No_Cluster;
      Size          : Unsigned_32 := 0;
   end record;

   --  The entry's fields. Valid_Cluster is False when DIR_FstClusHI has any
   --  of its top four bits set (no FAT32 cluster number has them).
   procedure Decode_Short
     (Data : Byte_Array; Position : Natural; Item : out Short_Entry; Valid_Cluster : out Boolean)
     with Pre => Position >= Data'First and then Data'Last >= Entry_Bytes - 1
                 and then Position <= Data'Last - (Entry_Bytes - 1);

   procedure Encode_Short (Data : in out Byte_Array; Position : Natural; Item : Short_Entry)
     with Pre  => Position >= Data'First and then Data'Last >= Entry_Bytes - 1
                  and then Position <= Data'Last - (Entry_Bytes - 1),
          Post => (for all I in Data'Range =>
                     (if I not in Position .. Position + (Entry_Bytes - 1)
                      then Data (I) = Data'Old (I)));

   --  The short-name checksum a long name's parts carry (FATGEN section 7).
   function Checksum (Name : Short_Name) return Unsigned_8;

   ---------------------------------------------------------------------------
   --  VFAT long-name parts (FATGEN section 7)
   ---------------------------------------------------------------------------
   Long_Ordinal_Offset  : constant := 0;    --  LDIR_Ord
   Long_Type_Offset     : constant := 12;   --  LDIR_Type: 0
   Long_Checksum_Offset : constant := 13;   --  LDIR_Chksum
   Long_Cluster_Offset  : constant := 26;   --  LDIR_FstClusLO: 0
   Long_Last_Flag       : constant Unsigned_8 := 16#40#;   --  LAST_LONG_ENTRY
   Long_Ordinal_Mask    : constant Unsigned_8 := 16#3F#;
   Units_Per_Part       : constant := 13;
   Maximum_Long_Units   : constant := 255;
   Maximum_Parts        : constant := 20;   --  ceiling (255 / 13)
   subtype Part_Ordinal is Natural range 1 .. Maximum_Parts;
   subtype Part_Unit_Index is Natural range 1 .. Units_Per_Part;
   type Part_Units is array (Part_Unit_Index) of Unsigned_16;
   --  Where each of a part's thirteen UCS-2 units is (LDIR_Name1, 2, 3).
   Unit_Offsets : constant array (Part_Unit_Index) of Natural :=
     [1, 3, 5, 7, 9, 14, 16, 18, 20, 22, 24, 28, 30];
   Name_Terminator : constant Unsigned_16 := 16#0000#;
   Name_Padding    : constant Unsigned_16 := 16#FFFF#;

   type Long_Part is record
      Ordinal  : Part_Ordinal := 1;
      Last     : Boolean := False;
      Checksum : Unsigned_8 := 0;
      Units    : Part_Units := [others => Name_Padding];
   end record;

   --  A long-name part: Valid when its ordinal is 1 .. 20, its type 0 and
   --  its cluster field 0.
   procedure Decode_Long (Data : Byte_Array; Position : Natural; Part : out Long_Part; Valid : out Boolean)
     with Pre => Position >= Data'First and then Data'Last >= Entry_Bytes - 1
                 and then Position <= Data'Last - (Entry_Bytes - 1);

   procedure Encode_Long (Data : in out Byte_Array; Position : Natural; Part : Long_Part)
     with Pre  => Position >= Data'First and then Data'Last >= Entry_Bytes - 1
                  and then Position <= Data'Last - (Entry_Bytes - 1),
          Post => (for all I in Data'Range =>
                     (if I not in Position .. Position + (Entry_Bytes - 1)
                      then Data (I) = Data'Old (I)));

   ---------------------------------------------------------------------------
   --  Long names as read: parts arrive last-first, each must follow the one
   --  before (ordinal one less, same checksum), and the set belongs to the
   --  short entry right after it only if that entry's checksum matches.
   ---------------------------------------------------------------------------
   Maximum_Gathered_Units : constant := Maximum_Parts * Units_Per_Part;   --  260
   subtype Gathered_Index is Positive range 1 .. Maximum_Gathered_Units;
   type Gathered_Units is array (Gathered_Index) of Unsigned_16;
   subtype Part_Count is Natural range 0 .. Maximum_Parts;

   type Long_Name_State is record
      Active   : Boolean := False;
      Parts    : Part_Count := 0;          --  how many the set has
      Expected : Part_Count := 0;          --  the next ordinal awaited (0: complete)
      Sum      : Unsigned_8 := 0;
      Units    : Gathered_Units := [others => Name_Padding];
   end record;

   function Idle return Long_Name_State is
     (Active => False, Parts => 0, Expected => 0, Sum => 0, Units => [others => Name_Padding]);

   function Consistent (S : Long_Name_State) return Boolean is
     (if S.Active then S.Parts >= 1 and then S.Expected < S.Parts);

   procedure Gather (S : in out Long_Name_State; Part : Long_Part)
     with Pre  => Consistent (S),
          Post => Consistent (S);

   --  The gathered set names the short entry Name.
   function Complete_For (S : Long_Name_State; Name : Short_Name) return Boolean is
     (S.Active and then S.Expected = 0 and then S.Sum = Checksum (Name));

   subtype Long_Length is Natural range 0 .. Maximum_Long_Units;
   type Long_Units is array (1 .. Maximum_Long_Units) of Unsigned_16;

   --  The name a complete set holds: its units up to the first terminator
   --  (or all of them). Valid is False when it is empty or longer than 255.
   procedure Gathered_Name
     (S : Long_Name_State; Units : out Long_Units; Length : out Long_Length; Valid : out Boolean)
     with Pre  => Consistent (S),
          Post => (if Valid then Length >= 1);

   ---------------------------------------------------------------------------
   --  Dates and times (FATGEN section 6.1). The volume's clock is UTC here:
   --  FAT has no zone, and CuBit's wall clock is UTC (as Linux's tz=UTC).
   ---------------------------------------------------------------------------
   Epoch_Year : constant := 1980;
   Last_Year  : constant := Epoch_Year + 127;
   --  1980-01-01 00:00:00 UTC in Unix seconds.
   Epoch_Unix_Seconds : constant := 315_532_800;
   --  2107-12-31 23:59:58 UTC.
   Last_Unix_Seconds : constant := 4_354_819_198;
   subtype Unix_Seconds is Unsigned_64 range 0 .. 2 ** 40;

   --  A stamp's Unix time; 0 when the date is not a real date (0 days or
   --  months, a 30th of February, ...). The time's fields are clamped.
   function To_Unix (Date, Time : Unsigned_16) return Unix_Seconds;

   --  The stamp for Unix time Seconds, clamped to 1980 .. 2107 (two-second
   --  resolution, as DIR_WrtTime).
   procedure From_Unix (Seconds : Unsigned_64; Date, Time : out Unsigned_16);

end Fat_Format;
