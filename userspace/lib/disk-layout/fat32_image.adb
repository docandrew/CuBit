------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body FAT32_Image with SPARK_Mode is

   --  Boot sector and BPB (FAT spec 3.1, 3.3), zero-based offsets.
   Jump_Offset          : constant := 0;
   Jump_Code            : constant := 16#90_58_EB#;   --  EB 58 90: JMP over the BPB
   OEM_Offset           : constant := 3;
   OEM_Name             : constant String := "CUBIT1.0";
   Bytes_Per_Sector_At  : constant := 11;
   Per_Cluster_At       : constant := 13;
   Reserved_At          : constant := 14;
   FAT_Count_At         : constant := 16;
   Media_At             : constant := 21;
   Fixed_Media          : constant := 16#F8#;
   Per_Track_At         : constant := 24;
   Sectors_Per_Track    : constant := 63;
   Heads_At             : constant := 26;
   Head_Count           : constant := 255;
   Hidden_At            : constant := 28;
   Total_32_At          : constant := 32;
   FAT_Size_32_At       : constant := 36;
   Root_Cluster_At      : constant := 44;
   FSInfo_At            : constant := 48;
   Backup_Boot_At       : constant := 50;
   Drive_Number_At      : constant := 64;
   Fixed_Drive          : constant := 16#80#;
   Boot_Signature_At    : constant := 66;
   Extended_Signature   : constant := 16#29#;
   Volume_Id_At         : constant := 67;
   Volume_Label_At      : constant := 71;
   Type_Text_At         : constant := 82;
   Type_Text            : constant String := "FAT32   ";
   Signature_At         : constant := 510;
   Signature_Word       : constant := 16#AA55#;

   --  FSInfo (FAT spec 5).
   Lead_Signature       : constant := 16#4161_5252#;
   Structure_At         : constant := 484;
   Structure_Signature  : constant := 16#6141_7272#;
   Free_Count_At        : constant := 488;
   Next_Free_At         : constant := 492;
   Trail_At             : constant := 508;
   Trail_Signature      : constant := 16#AA55_0000#;

   --  FAT entries (spec 4.2): media in entry 0, clean-shutdown bits in 1.
   Entry_Bytes          : constant := 4;
   Media_Entry          : constant := 16#0FFF_FFF8#;
   End_Of_Chain         : constant := 16#0FFF_FFFF#;

   --  Directory entry (spec 6): an 11-byte name and its attributes.
   Attribute_At         : constant := 11;
   Volume_Id_Attribute  : constant := 16#08#;

   function Plan
     (Sector_Bytes : Device_Block_Bytes; Sectors : Sector_Count;
      First : Unsigned_32) return Volume
   is
      Per_Cluster : constant Unsigned_64 :=
        Cluster_Bytes / Unsigned_64 (Sector_Bytes);
      Entries_Per_Sector : constant Unsigned_64 :=
        Unsigned_64 (Sector_Bytes) / Entry_Bytes;
      Result : Volume :=
        (Sector_Bytes => Sector_Bytes, Sectors => Sectors,
         Sectors_Per_Cluster => Unsigned_32 (Per_Cluster), FAT_Sectors => 0,
         Clusters => 0, Hidden_Sectors => First);
      FAT : Unsigned_64;
   begin
      if Unsigned_64 (Sectors) <= Reserved_Sectors + 2 * Per_Cluster then
         return Result;
      end if;
      --  The smallest FAT that maps every cluster left after both FATs:
      --  FAT * (Per_Cluster * Entries_Per_Sector + FAT_Count) >=
      --    Sectors - Reserved + 2 * Per_Cluster.
      FAT := (Unsigned_64 (Sectors) - Reserved_Sectors + 2 * Per_Cluster
              + (Per_Cluster * Entries_Per_Sector + FAT_Count) - 1)
             / (Per_Cluster * Entries_Per_Sector + FAT_Count);
      if Reserved_Sectors + FAT_Count * FAT >= Unsigned_64 (Sectors) then
         return Result;
      end if;
      Result.FAT_Sectors := Sector_Count (FAT);
      Result.Clusters := Unsigned_32
        ((Unsigned_64 (Sectors) - Reserved_Sectors - FAT_Count * FAT) / Per_Cluster);
      return Result;
   end Plan;

   function Boot_Sector
     (Item : Volume; Volume_Id : Unsigned_32; Label : Volume_Label)
      return Sector_Image
   is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
   begin
      Put_8 (Result, Jump_Offset, Unsigned_8 (Jump_Code mod 2 ** 8));
      Put_8 (Result, Jump_Offset + 1, Unsigned_8 (Jump_Code / 2 ** 8 mod 2 ** 8));
      Put_8 (Result, Jump_Offset + 2, Unsigned_8 (Jump_Code / 2 ** 16));
      Put_Text (Result, OEM_Offset, OEM_Name);
      Put_16 (Result, Bytes_Per_Sector_At, Unsigned_16 (Item.Sector_Bytes));
      Put_8 (Result, Per_Cluster_At, Unsigned_8 (Item.Sectors_Per_Cluster));
      Put_16 (Result, Reserved_At, Reserved_Sectors);
      Put_8 (Result, FAT_Count_At, FAT_Count);
      Put_8 (Result, Media_At, Fixed_Media);
      Put_16 (Result, Per_Track_At, Sectors_Per_Track);
      Put_16 (Result, Heads_At, Head_Count);
      Put_32 (Result, Hidden_At, Item.Hidden_Sectors);
      Put_32 (Result, Total_32_At, Item.Sectors);
      Put_32 (Result, FAT_Size_32_At, Item.FAT_Sectors);
      Put_32 (Result, Root_Cluster_At, Root_Cluster);
      Put_16 (Result, FSInfo_At, FSInfo_Sector);
      Put_16 (Result, Backup_Boot_At, Backup_Boot_Sector);
      Put_8 (Result, Drive_Number_At, Fixed_Drive);
      Put_8 (Result, Boot_Signature_At, Extended_Signature);
      Put_32 (Result, Volume_Id_At, Volume_Id);
      Put_Text (Result, Volume_Label_At, Label);
      Put_Text (Result, Type_Text_At, Type_Text);
      Put_16 (Result, Signature_At, Signature_Word);
      return Result;
   end Boot_Sector;

   function FSInfo (Item : Volume) return Sector_Image is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
   begin
      Put_32 (Result, 0, Lead_Signature);
      Put_32 (Result, Structure_At, Structure_Signature);
      --  The root directory holds cluster 2; the next free one is 3.
      Put_32 (Result, Free_Count_At, Item.Clusters - 1);
      Put_32 (Result, Next_Free_At, Root_Cluster + 1);
      Put_32 (Result, Trail_At, Trail_Signature);
      return Result;
   end FSInfo;

   function First_FAT_Sector (Item : Volume) return Sector_Image is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
   begin
      Put_32 (Result, 0, Media_Entry);
      Put_32 (Result, Entry_Bytes, End_Of_Chain);
      Put_32 (Result, Root_Cluster * Entry_Bytes, End_Of_Chain);
      return Result;
   end First_FAT_Sector;

   function Root_Directory_Sector (Item : Volume; Label : Volume_Label)
     return Sector_Image
   is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
   begin
      Put_Text (Result, 0, Label);
      Put_8 (Result, Attribute_At, Volume_Id_Attribute);
      return Result;
   end Root_Directory_Sector;

end FAT32_Image;
