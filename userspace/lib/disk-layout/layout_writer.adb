------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Partition_Tables;
with GPT_Image;
with Ext2_Format;

package body Layout_Writer
  with SPARK_Mode, Refined_State => (Layouts => (ESP_Layout, Root_Layout))
is

   ESP_Layout : FAT32_Image.Tree_Layout;
   Root_Layout : Ext2_Image.Tree_Layout;

   use Install_Tree;

   --  Copies Count bytes of File from Offset into Buffer (1 .. Count) and
   --  zero-fills Buffer up to Padded.
   procedure Fill
     (Volume : Install_Volume; File : Entry_Index; Offset, Count : Unsigned_64;
      Padded : Natural; Buffer : in out Bytes; OK : out Boolean)
     with Pre => Buffer'First = 1 and then Buffer'Length = Batch_Bytes
                 and then Count <= Unsigned_64 (Padded) and then Padded <= Batch_Bytes
   is
   begin
      OK := True;
      if Count > 0 then
         Read (Volume, File, Offset, Buffer (1 .. Natural (Count)), OK);
      end if;
      Buffer (Natural (Count) + 1 .. Padded) := [others => 0];
   end Fill;

   procedure Write_ESP
     (Shape : Disk_Shape; Layout : Install_Layout; Files : Tree; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => ESP_Layout)
   is
      use FAT32_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := ESP_First (Shape) * B;
      Item : constant Volume :=
        Plan (Shape.Block_Size,
              --  At most 64 GiB of 512-byte sectors: Maximum_Sectors.
              Sector_Count (Unsigned_64'Min (MiB_Blocks (Shape, Layout.Disk.ESP_Size),
                                             FAT32_Image.Maximum_Sectors)),
              Unsigned_32 (Unsigned_64'Min (ESP_First (Shape), Unsigned_64 (Unsigned_32'Last))));
      Files_Layout : Tree_Layout renames ESP_Layout;
      OK : Boolean := True;
      Source_OK : Boolean := True;
      Buffer : Bytes (1 .. Batch_Bytes) := [others => 0];

      --  Sectors of the volume; never past its end.
      procedure Sector (Index : Unsigned_64; Data : Bytes) is
      begin
         if OK and then Index < Unsigned_64 (Item.Sectors)
           and then Index * B <= Byte_Offset'Last - Base
         then
            Put (Base + Index * B, Data, OK);
         else
            OK := False;
         end if;
      end Sector;

      procedure Clear (Index, Count : Unsigned_64) is
      begin
         if OK and then Index <= Unsigned_64 (Item.Sectors)
           and then Count <= Unsigned_64 (Item.Sectors) - Index
           and then Index * B <= Byte_Offset'Last - Base
         then
            Zero (Base + Index * B, Count * B, OK);
         else
            OK := False;
         end if;
      end Clear;
   begin
      if not Valid (Item) then
         Result := Layout_Invalid;
         return;
      end if;
      Plan_Tree (Item, Files, Files_Layout);
      if not Files_Layout.Fits then
         Result := ESP_Files_Do_Not_Fit;
         return;
      end if;
      Clear (0, Reserved_Sectors);
      declare
         Boot : constant Sector_Image :=
           Boot_Sector (Item, Layout.ESP_Serial, Layout.ESP_Label);
         Info : constant Sector_Image := FSInfo (Item, Files_Layout);
      begin
         Sector (0, Boot);
         Sector (FSInfo_Sector, Info);
         Sector (Backup_Boot_Sector, Boot);
         Sector (Backup_Boot_Sector + FSInfo_Sector, Info);
      end;
      for Copy in 1 .. FAT_Count loop
         Clear (FAT_Start (Item, Copy), Unsigned_64 (Item.FAT_Sectors));
         --  Counted from 1: an unsigned 0 - 1 would wrap.
         for S in 1 .. Used_FAT_Sectors (Item, Files_Layout) loop
            exit when not OK;
            Sector (FAT_Start (Item, Copy) + (S - 1), FAT_Sector (Item, Files_Layout, Files, S - 1));
         end loop;
      end loop;
      Sector (Cluster_Sector (Item, Root_Cluster),
              Directory_Cluster (Files_Layout, Files, Volume_Root, Layout.ESP_Label));
      for E in 1 .. Files.Count loop
         exit when not OK or else not Source_OK;
         declare
            Start : constant Unsigned_32 := Files_Layout.Start (E);
         begin
            case Files.Entries (E).Kind is
               when Directory =>
                  if Start >= Root_Cluster then
                     Sector (Cluster_Sector (Item, Start),
                             Directory_Cluster (Files_Layout, Files, E, Layout.ESP_Label));
                  end if;
               when Regular_File =>
                  --  A contiguous run: whole batches, then the zero-padded tail.
                  declare
                     Size : constant Unsigned_64 := Files.Entries (E).Size;
                     Done : Unsigned_64 := 0;
                  begin
                     while OK and then Source_OK and then Done < Size and then Start >= Root_Cluster loop
                        pragma Loop_Invariant (Done < Size);
                        declare
                           Count : constant Unsigned_64 := Unsigned_64'Min (Size - Done, Batch_Bytes);
                           Padded : constant Natural :=
                             Natural ((Count + B - 1) / B * B);
                        begin
                           Fill (ESP_Volume, E, Done, Count, Padded, Buffer, Source_OK);
                           if Source_OK then
                              Sector (Cluster_Sector (Item, Start) + Done / B, Buffer (1 .. Padded));
                           end if;
                           Done := Done + Count;
                        end;
                     end loop;
                  end;
            end case;
         end;
      end loop;
      Result := (if not Source_OK then Source_Failed elsif OK then Written else Device_Failed);
   end Write_ESP;

   procedure Write_Root
     (Shape : Disk_Shape; Layout : Install_Layout; Files : Tree; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => Root_Layout)
   is
      use Ext2_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := Root_First (Shape, Layout.Disk) * B;
      Root_Bytes : constant Unsigned_64 :=
        (Root_Last (Shape, Layout.Disk) - Root_First (Shape, Layout.Disk) + 1) * B;
      Item : constant Volume := Plan (Root_Bytes, Layout.Root.Inode_Ratio);
      Files_Layout : Tree_Layout renames Root_Layout;
      OK : Boolean := True;
      Source_OK : Boolean := True;
      Buffer : Bytes (1 .. Batch_Bytes) := [others => 0];

      --  Blocks outside the volume are never written: such a request
      --  fails the run (it cannot happen for a valid plan: the volume
      --  ends inside the root partition, which ends inside the disk).
      procedure Block (Index : Unsigned_64; Data : Bytes) is
      begin
         if OK and then Index < Unsigned_64 (Item.Blocks)
           and then Index * Block_Bytes <= Byte_Offset'Last - Base
         then
            Put (Base + Index * Block_Bytes, Data, OK);
         else
            OK := False;
         end if;
      end Block;

      procedure Clear (Index, Count : Unsigned_64) is
      begin
         if OK and then Index <= Unsigned_64 (Item.Blocks)
           and then Count <= Unsigned_64 (Item.Blocks) - Index
           and then Index * Block_Bytes <= Byte_Offset'Last - Base
         then
            Zero (Base + Index * Block_Bytes, Count * Block_Bytes, OK);
         else
            OK := False;
         end if;
      end Clear;

      --  A superblock copy's block: group 0's sits 1024 bytes in (after
      --  the boot area), every backup at its block's start.
      function Superblock_Block (Group : Group_Index) return Block_Image
        with Pre => Valid (Item) and then Files_Layout.Fits and then Group < Item.Groups
      is
         Result : Block_Image := [others => 0];
         Copy : constant Superblock_Image :=
           Superblock (Item, Files_Layout, Layout.Root, Group);
         At_Byte : constant Natural :=
           (if Group = 0 then Ext2_Format.Superblock_Offset else 0);
      begin
         Result (At_Byte + 1 .. At_Byte + Copy'Length) := Copy;
         return Result;
      end Superblock_Block;

      --  A directory's blocks, from Start in allocation order.
      procedure Write_Directory (Directory : Parent_Index; Start, Count : Unsigned_64)
        with Pre => Valid (Item) and then Files_Layout.Fits and then Directory <= Files.Count
      is
         At_Block : Unsigned_64 := Start;
      begin
         for K in 1 .. Unsigned_64'Min (Count, Direct_Blocks) loop
            exit when not OK or else At_Block >= Unsigned_64 (Item.Blocks);
            Block (At_Block, Directory_Block (Files_Layout, Files, Directory, K - 1));
            At_Block := Next_Block (Item, At_Block);
         end loop;
      end Write_Directory;

      --  A file: its indirect blocks, then its data in runs that stay in
      --  one group and one batch.
      procedure Write_File (File : Entry_Index)
        with Pre => Valid (Item) and then Files_Layout.Fits and then File <= Files.Count
      is
         Size : constant Unsigned_64 := Files.Entries (File).Size;
         Meta : constant Unsigned_64 := Unsigned_64'Min (Files_Layout.Meta (File), Maximum_Blocks);
         At_Block : Unsigned_64 :=
           Unsigned_64'Min (Files_Layout.Start (File), Unsigned_64 (Item.Blocks));
         Done : Unsigned_64 := 0;
      begin
         for K in 1 .. Meta loop
            exit when not OK or else At_Block >= Unsigned_64 (Item.Blocks);
            Block (At_Block, Indirect_Block (Item, Files_Layout, File, K - 1));
            At_Block := Next_Block (Item, At_Block);
         end loop;
         while OK and then Source_OK and then Done < Size
           and then At_Block < Unsigned_64 (Item.Blocks)
         loop
            pragma Loop_Invariant (Done < Size and then At_Block < Unsigned_64 (Item.Blocks));
            declare
               Group : constant Group_Index := Group_Index (At_Block / Blocks_Per_Group);
               Room : constant Unsigned_64 :=
                 (if Group < Item.Groups
                  then Group_First (Group) + Group_Blocks (Item, Group) - At_Block else 1);
               Run : constant Unsigned_64 :=
                 Unsigned_64'Min (Room, Unsigned_64'Min
                   ((Size - Done + Block_Bytes - 1) / Block_Bytes, Batch_Bytes / Block_Bytes));
               Count : constant Unsigned_64 := Unsigned_64'Min (Size - Done, Run * Block_Bytes);
            begin
               Fill (Root_Volume, File, Done, Count, Natural (Run * Block_Bytes), Buffer, Source_OK);
               if Source_OK then
                  Block (At_Block, Buffer (1 .. Natural (Run * Block_Bytes)));
               end if;
               Done := Done + Count;
               At_Block := Advance (Item, At_Block, Run);
            end;
         end loop;
      end Write_File;
   begin
      if not Valid (Item) then
         Result := Layout_Invalid;
         return;
      end if;
      Plan_Tree (Item, Files, Files_Layout);
      if not Files_Layout.Fits then
         Result := Root_Files_Do_Not_Fit;
         return;
      end if;
      for Group in 0 .. Item.Groups - 1 loop
         if Has_Backup (Group) then
            Block (Group_First (Group), Superblock_Block (Group));
            for Index in 0 .. Item.Descriptor_Blocks - 1 loop
               Block (Group_First (Group) + 1 + Unsigned_64 (Index),
                      Descriptor_Block (Item, Files_Layout, Index));
            end loop;
         end if;
         Block (Block_Bitmap_Block (Item, Group), Block_Bitmap (Item, Files_Layout, Group));
         Block (Inode_Bitmap_Block (Item, Group), Inode_Bitmap (Item, Files_Layout, Group));
         declare
            Used : constant Unsigned_32 := Used_Table_Blocks (Item, Files_Layout, Group);
         begin
            for Index in 1 .. Used loop
               exit when not OK;
               Block (Inode_Table_Block (Item, Group) + Unsigned_64 (Index - 1),
                      Inode_Table (Item, Files_Layout, Files, Layout.Root, Group, Index - 1));
            end loop;
            Clear (Inode_Table_Block (Item, Group) + Unsigned_64 (Used),
                   Unsigned_64 (Item.Inode_Table_Blocks - Used));
         end;
         exit when not OK;
      end loop;
      Block (Files_Layout.Lost_Found, Lost_Found_Directory);
      Write_Directory (Volume_Root, Files_Layout.Root_Start, Files_Layout.Root_Blocks);
      for E in 1 .. Files.Count loop
         exit when not OK or else not Source_OK;
         case Files.Entries (E).Kind is
            when Directory =>
               Write_Directory (E, Files_Layout.Start (E), Files_Layout.Data (E));
            when Regular_File =>
               Write_File (E);
         end case;
      end loop;
      Result := (if not Source_OK then Source_Failed elsif OK then Written else Device_Failed);
   end Write_Root;

   procedure Write_GPT
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk)
   is
      use GPT_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Table : constant GPT_Image.Entry_Array := Entries (Shape, Layout.Disk);
      Table_CRC : constant Unsigned_32 := Partition_Tables.CRC32 (Table);
      OK : Boolean := True;

      procedure At_Block (Index : Block_Address; Data : Bytes) is
      begin
         if OK then
            Put (Index * B, Data, OK);
         end if;
      end At_Block;
   begin
      At_Block (Entries_LBA (Shape, Backup), Table);
      At_Block (Header_LBA (Shape, Backup),
                Header (Shape, Layout.Disk, Backup, Table_CRC));
      At_Block (Entries_LBA (Shape, Primary), Table);
      At_Block (Header_LBA (Shape, Primary),
                Header (Shape, Layout.Disk, Primary, Table_CRC));
      At_Block (0, Protective_MBR (Shape));
      Result := (if OK then Written else Device_Failed);
   end Write_GPT;

   procedure Write_Disk
     (Shape : Disk_Shape; Layout : Install_Layout;
      ESP_Files, Root_Files : Install_Tree.Tree; Result : out Write_Result)
     with Refined_Global => (In_Out => (ESP_Layout, Root_Layout))
   is
   begin
      Write_ESP (Shape, Layout, ESP_Files, Result);
      if Result = Written then
         Write_Root (Shape, Layout, Root_Files, Result);
      end if;
      if Result = Written then
         Write_GPT (Shape, Layout, Result);
      end if;
   end Write_Disk;

end Layout_Writer;
