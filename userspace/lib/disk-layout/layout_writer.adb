------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Partition_Tables;
with GPT_Image;
with Ext2_Format;

package body Layout_Writer with SPARK_Mode is

   procedure Write_ESP
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk)
   is
      use FAT32_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := ESP_First (Shape) * B;
      Item : constant Volume :=
        Plan (Shape.Block_Size,
              Sector_Count (MiB_Blocks (Shape, Layout.Disk.ESP_Size)),
              Unsigned_32 (ESP_First (Shape)));
      OK : Boolean := True;

      procedure Sector (Index : Unsigned_64; Data : Bytes) is
      begin
         if OK then
            Put (Base + Index * B, Data, OK);
         end if;
      end Sector;

      procedure Clear (Index, Count : Unsigned_64) is
      begin
         if OK then
            Zero (Base + Index * B, Count * B, OK);
         end if;
      end Clear;
   begin
      if not Valid (Item) then
         Result := Layout_Invalid;
         return;
      end if;
      Clear (0, Reserved_Sectors);
      declare
         Boot : constant Sector_Image :=
           Boot_Sector (Item, Layout.ESP_Serial, Layout.ESP_Label);
         Info : constant Sector_Image := FSInfo (Item);
      begin
         Sector (0, Boot);
         Sector (FSInfo_Sector, Info);
         Sector (Backup_Boot_Sector, Boot);
         Sector (Backup_Boot_Sector + FSInfo_Sector, Info);
      end;
      for Copy in 1 .. FAT_Count loop
         Clear (FAT_Start (Item, Copy), Unsigned_64 (Item.FAT_Sectors));
         Sector (FAT_Start (Item, Copy), First_FAT_Sector (Item));
      end loop;
      Clear (Data_Start (Item), Unsigned_64 (Item.Sectors_Per_Cluster));
      Sector (Data_Start (Item), Root_Directory_Sector (Item, Layout.ESP_Label));
      Result := (if OK then Written else Device_Failed);
   end Write_ESP;

   procedure Write_Root
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk)
   is
      use Ext2_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := Root_First (Shape, Layout.Disk) * B;
      Root_Bytes : constant Unsigned_64 :=
        (Root_Last (Shape, Layout.Disk) - Root_First (Shape, Layout.Disk) + 1) * B;
      Item : constant Volume := Plan (Root_Bytes, Layout.Root.Inode_Ratio);
      OK : Boolean := True;

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
        with Pre => Valid (Item) and then Group < Item.Groups
      is
         Result : Block_Image := [others => 0];
         Copy : constant Superblock_Image := Superblock (Item, Layout.Root, Group);
         At_Byte : constant Natural :=
           (if Group = 0 then Ext2_Format.Superblock_Offset else 0);
      begin
         Result (At_Byte + 1 .. At_Byte + Copy'Length) := Copy;
         return Result;
      end Superblock_Block;
   begin
      if not Valid (Item) then
         Result := Layout_Invalid;
         return;
      end if;
      for Group in 0 .. Item.Groups - 1 loop
         if Has_Backup (Group) then
            Block (Group_First (Group), Superblock_Block (Group));
            for Index in 0 .. Item.Descriptor_Blocks - 1 loop
               Block (Group_First (Group) + 1 + Unsigned_64 (Index),
                      Descriptor_Block (Item, Index));
            end loop;
         end if;
         Block (Block_Bitmap_Block (Item, Group), Block_Bitmap (Item, Group));
         Block (Inode_Bitmap_Block (Item, Group), Inode_Bitmap (Item, Group));
         Clear (Inode_Table_Block (Item, Group), Unsigned_64 (Item.Inode_Table_Blocks));
         exit when not OK;
      end loop;
      Block (Inode_Table_Block (Item, 0), First_Inode_Block (Item, Layout.Root));
      Block (Root_Directory_Block (Item), Root_Directory (Item));
      Block (Lost_Found_Block (Item), Lost_Found_Directory (Item));
      Result := (if OK then Written else Device_Failed);
   end Write_Root;

   procedure Write_GPT
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk)
   is
      use GPT_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Table : constant Entry_Array := Entries (Shape, Layout.Disk);
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
     (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result) is
   begin
      Write_ESP (Shape, Layout, Result);
      if Result = Written then
         Write_Root (Shape, Layout, Result);
      end if;
      if Result = Written then
         Write_GPT (Shape, Layout, Result);
      end if;
   end Write_Disk;

end Layout_Writer;
