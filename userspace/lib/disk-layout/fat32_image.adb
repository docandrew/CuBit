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

   function FSInfo (Item : Volume; Layout : Tree_Layout) return Sector_Image is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
      Used : constant Unsigned_32 := Layout.Next - Root_Cluster;
   begin
      Put_32 (Result, 0, Lead_Signature);
      Put_32 (Result, Structure_At, Structure_Signature);
      Put_32 (Result, Free_Count_At, Item.Clusters - Used);
      Put_32 (Result, Next_Free_At, Layout.Next);
      Put_32 (Result, Trail_At, Trail_Signature);
      return Result;
   end FSInfo;

   procedure To_Short_Name (Source : String; Result : out Short_Name; Valid : out Boolean) is
      Base_Bytes : constant := 8;
      Extension_Bytes : constant := 3;
      Dot : Natural := 0;
      Case_Offset : constant := Character'Pos ('a') - Character'Pos ('A');

      function Allowed (C : Character) return Boolean is
        (C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-');
      function Folded (C : Character) return Character is
        (if C in 'a' .. 'z' then Character'Val (Character'Pos (C) - Case_Offset) else C);
   begin
      Result := [others => ' '];
      Valid := False;
      --  At most 8 + 1 + 3 characters: anything longer is not 8.3.
      if Source'Length not in 1 .. Short_Name'Length + 1 then
         return;
      end if;
      declare
         Name : constant String (1 .. Source'Length) := Source;
      begin
      for I in Name'Range loop
         pragma Loop_Invariant (Dot < I);
         if Name (I) = '.' then
            if Dot /= 0 then
               return;
            end if;
            Dot := I;
         elsif not Allowed (Name (I)) then
            return;
         end if;
      end loop;
      declare
         Base_Last : constant Integer := (if Dot = 0 then Name'Last else Dot - 1);
      begin
         if Base_Last < Name'First or else Base_Last - Name'First + 1 > Base_Bytes then
            return;
         end if;
         if Dot /= 0 and then (Dot = Name'Last or else Name'Last - Dot > Extension_Bytes) then
            return;
         end if;
         for I in Name'First .. Base_Last loop
            Result (I - Name'First + 1) := Folded (Name (I));
         end loop;
         if Dot /= 0 then
            for I in Dot + 1 .. Name'Last loop
               Result (Base_Bytes + I - Dot) := Folded (Name (I));
            end loop;
         end if;
      end;
      end;
      Valid := True;
   end To_Short_Name;

   procedure Plan_Tree
     (Item : Volume; Files : Install_Tree.Tree; Result : out Tree_Layout)
   is
      use Install_Tree;
      Next : Unsigned_64 range Root_Cluster + 1 .. Unsigned_64 (Unsigned_32'Last) := Root_Cluster + 1;
      Limit : constant Unsigned_64 := Unsigned_64 (Item.Clusters) + Root_Cluster;
      OK : Boolean := True;
      Name : Short_Name;
      Valid : Boolean;

      --  "." and ".." (or the root's label), then the children.
      function Entry_Count (Directory : Parent_Index) return Natural is
         Count : Natural range 0 .. Maximum_Entries + 2 := 2;
      begin
         for E in 1 .. Files.Count loop
            pragma Loop_Invariant (Count <= E + 1);
            if Files.Entries (E).Parent = Directory then
               Count := Count + 1;
            end if;
         end loop;
         return Count;
      end Entry_Count;

      procedure Take (E : Entry_Index; Count : Unsigned_64) is
      begin
         if not OK or else Next > Limit or else Count > Limit - Next then
            OK := False;
            return;
         end if;
         Result.Start (E) := (if Count = 0 then 0 else Unsigned_32 (Next));
         Result.Clusters (E) := Unsigned_32 (Count);
         Next := Next + Count;
      end Take;
   begin
      Result := (others => <>);
      if Limit <= Next then
         return;
      end if;
      OK := Entry_Count (Volume_Root) <= Entries_Per_Cluster;
      for E in 1 .. Files.Count loop
         pragma Loop_Invariant (Next <= Limit);
         To_Short_Name (Name_Of (Files, E), Name, Valid);
         OK := OK and then Valid;
         if Files.Entries (E).Kind = Directory then
            OK := OK and then Entry_Count (E) <= Entries_Per_Cluster;
            Take (E, 1);
         end if;
      end loop;
      for E in 1 .. Files.Count loop
         pragma Loop_Invariant (Next <= Limit);
         if Files.Entries (E).Kind = Regular_File then
            --  FAT sizes are 32 bits.
            OK := OK and then Files.Entries (E).Size <= Unsigned_64 (Unsigned_32'Last);
            Take (E, (Files.Entries (E).Size + Cluster_Bytes - 1) / Cluster_Bytes);
         end if;
      end loop;
      Result.Next := Unsigned_32 (Next);
      Result.Fits := OK;
   end Plan_Tree;

   function Entries_Per_Sector (Item : Volume) return Unsigned_64 is
     (Unsigned_64 (Item.Sector_Bytes) / Entry_Bytes);

   function Used_FAT_Sectors (Item : Volume; Layout : Tree_Layout) return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64 (Item.FAT_Sectors),
         (Unsigned_64 (Layout.Next) + Entries_Per_Sector (Item) - 1) / Entries_Per_Sector (Item)));

   function FAT_Sector
     (Item : Volume; Layout : Tree_Layout; Files : Install_Tree.Tree; Index : Unsigned_64)
      return Sector_Image
   is
      Result : Sector_Image (1 .. Item.Sector_Bytes) := [others => 0];
      Per : constant Unsigned_64 := Entries_Per_Sector (Item);
      First : constant Unsigned_64 := (if Index <= Unsigned_64 (Item.FAT_Sectors) then Index * Per else 0);
      Last : constant Unsigned_64 := First + Per - 1;

      procedure Put_Entry (Cluster : Unsigned_64; Value : Unsigned_32) is
      begin
         pragma Assert (Item.Sector_Bytes in 512 | 4096);
         if Cluster in First .. Last and then Cluster - First < Unsigned_64 (Item.Sector_Bytes) / Entry_Bytes then
            Put_32 (Result, Natural (Cluster - First) * Entry_Bytes, Value);
         end if;
      end Put_Entry;
   begin
      Put_Entry (0, Media_Entry);
      Put_Entry (1, End_Of_Chain);
      Put_Entry (Root_Cluster, End_Of_Chain);
      for E in 1 .. Files.Count loop
         declare
            Start : constant Unsigned_64 := Unsigned_64 (Layout.Start (E));
            Count : constant Unsigned_64 := Unsigned_64 (Layout.Clusters (E));
         begin
            --  Only the part of the run inside this sector.
            if Count > 0 and then Start <= Last and then Start + Count - 1 >= First then
               for C in Unsigned_64'Max (Start, First) .. Unsigned_64'Min (Start + Count - 1, Last) loop
                  Put_Entry (C, (if C >= Start + Count - 1 then End_Of_Chain
                                 else Unsigned_32 (Unsigned_64'Min (C + 1, Unsigned_64 (Unsigned_32'Last)))));
               end loop;
            end if;
         end;
      end loop;
      return Result;
   end FAT_Sector;

   --  Directory entry attributes and fields (spec 6).
   Directory_Attribute : constant := 16#10#;
   Archive_Attribute   : constant := 16#20#;
   Write_Date_At       : constant := 24;
   Create_Date_At      : constant := 16;
   Access_Date_At      : constant := 18;
   Cluster_High_At     : constant := 20;
   Cluster_Low_At      : constant := 26;
   Size_At             : constant := 28;
   --  1980-01-01, the first date FAT can hold: (0 << 9) | (1 << 5) | 1.
   Epoch_Date          : constant := 16#0021#;
   Dot_Name            : constant Short_Name := ".          ";
   Dot_Dot_Name        : constant Short_Name := "..         ";

   function Directory_Cluster
     (Layout : Tree_Layout; Files : Install_Tree.Tree;
      Directory : Install_Tree.Parent_Index; Label : Volume_Label) return Cluster_Image
   is
      use Install_Tree;
      Result : Cluster_Image := [others => 0];
      Slot : Natural range 0 .. Entries_Per_Cluster := 0;

      procedure Put_Entry (Name : Short_Name; Attribute : Unsigned_8; Cluster : Unsigned_32;
                           Size : Unsigned_32) is
         At_Byte : constant Natural := Slot * Directory_Entry_Bytes;
      begin
         if Slot >= Entries_Per_Cluster then
            return;
         end if;
         Put_Text (Result, At_Byte, Name);
         Put_8 (Result, At_Byte + Attribute_At, Attribute);
         if Attribute /= Volume_Id_Attribute then
            Put_16 (Result, At_Byte + Create_Date_At, Epoch_Date);
            Put_16 (Result, At_Byte + Access_Date_At, Epoch_Date);
            Put_16 (Result, At_Byte + Write_Date_At, Epoch_Date);
         end if;
         Put_16 (Result, At_Byte + Cluster_High_At, Unsigned_16 (Cluster / 2 ** 16));
         Put_16 (Result, At_Byte + Cluster_Low_At, Unsigned_16 (Cluster mod 2 ** 16));
         Put_32 (Result, At_Byte + Size_At, Size);
         Slot := Slot + 1;
      end Put_Entry;

      Name : Short_Name;
      Valid : Boolean;
   begin
      if Directory = Volume_Root then
         Put_Entry (Label, Volume_Id_Attribute, 0, 0);
      else
         Put_Entry (Dot_Name, Directory_Attribute, Layout.Start (Directory), 0);
         --  ".." of a directory in the root names cluster 0 (spec 6.7).
         Put_Entry (Dot_Dot_Name, Directory_Attribute,
                    (if Files.Entries (Directory).Parent = Volume_Root then 0
                     else Layout.Start (Files.Entries (Directory).Parent)), 0);
      end if;
      for E in 1 .. Files.Count loop
         if Files.Entries (E).Parent = Directory then
            To_Short_Name (Name_Of (Files, E), Name, Valid);
            case Files.Entries (E).Kind is
               when Install_Tree.Directory =>
                  Put_Entry (Name, Directory_Attribute, Layout.Start (E), 0);
               when Regular_File =>
                  Put_Entry (Name, Archive_Attribute, Layout.Start (E),
                             Unsigned_32 (Unsigned_64'Min (Files.Entries (E).Size,
                                                           Unsigned_64 (Unsigned_32'Last))));
            end case;
         end if;
      end loop;
      return Result;
   end Directory_Cluster;

end FAT32_Image;
