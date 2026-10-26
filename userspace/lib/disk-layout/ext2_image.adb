------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Ext2_Image with SPARK_Mode is

   package EF renames Ext2_Format;

   --  struct ext2_super_block (Linux include/linux/ext2_fs.h), offsets.
   S_Inodes_Count        : constant := 16#00#;
   S_Blocks_Count        : constant := 16#04#;
   S_Free_Blocks_Count   : constant := 16#0C#;
   S_Free_Inodes_Count   : constant := 16#10#;
   S_First_Data_Block    : constant := 16#14#;
   S_Log_Block_Size      : constant := 16#18#;
   S_Log_Frag_Size       : constant := 16#1C#;
   S_Blocks_Per_Group    : constant := 16#20#;
   S_Frags_Per_Group     : constant := 16#24#;
   S_Inodes_Per_Group    : constant := 16#28#;
   S_Wtime               : constant := 16#30#;
   S_Max_Mnt_Count       : constant := 16#36#;
   S_Magic               : constant := 16#38#;
   S_State               : constant := 16#3A#;
   S_Errors              : constant := 16#3C#;
   S_Lastcheck           : constant := 16#40#;
   S_Creator_OS          : constant := 16#48#;
   S_Rev_Level           : constant := 16#4C#;
   S_First_Ino           : constant := 16#54#;
   S_Inode_Size          : constant := 16#58#;
   S_Block_Group_Nr      : constant := 16#5A#;
   S_Feature_Compat      : constant := 16#5C#;
   S_Feature_Incompat    : constant := 16#60#;
   S_Feature_Ro_Compat   : constant := 16#64#;
   S_UUID                : constant := 16#68#;
   S_Volume_Name         : constant := 16#78#;

   Log_Block_Size        : constant := 2;          --  4096 = 1024 << 2
   Unlimited_Mounts      : constant := 16#FFFF#;   --  -1: no forced check
   Valid_FS              : constant := 1;          --  EXT2_VALID_FS
   Errors_Continue       : constant := 1;          --  EXT2_ERRORS_CONTINUE
   Creator_Linux         : constant := 0;          --  EXT2_OS_LINUX
   Feature_Incompat_Filetype : constant := 16#0002#;
   Feature_Ro_Sparse_Super   : constant := 16#0001#;
   Feature_Ro_Large_File     : constant := 16#0002#;

   --  struct ext2_group_desc.
   Bg_Block_Bitmap       : constant := 0;
   Bg_Inode_Bitmap       : constant := 4;
   Bg_Inode_Table        : constant := 8;
   Bg_Free_Blocks_Count  : constant := 12;
   Bg_Free_Inodes_Count  : constant := 14;
   Bg_Used_Dirs_Count    : constant := 16;

   --  struct ext2_inode.
   I_Mode                : constant := 0;
   I_Size                : constant := 4;
   I_Atime               : constant := 8;
   I_Ctime               : constant := 12;
   I_Mtime               : constant := 16;
   I_Links_Count         : constant := 26;
   I_Blocks              : constant := 28;   --  in 512-byte units
   I_Block               : constant := 40;   --  i_block[0]
   Sector_Bytes          : constant := 512;

   Lost_Found_Mode       : constant := 16#41C0#;   --  S_IFDIR | 0700
   --  The root's links: ".", its own "..", and lost+found's "..".
   Root_Links            : constant := 3;
   Directory_Links       : constant := 2;
   --  Directories in use in group 0: the root and lost+found.
   Directories_In_Use    : constant := 2;
   --  Data blocks in use in group 0: one for each directory.
   Directory_Blocks      : constant := 2;

   Lost_Found_Text       : constant String := "lost+found";

   function Has_Backup (Group : Group_Index) return Boolean is
      function Power_Of (Base : Unsigned_64) return Boolean
        with Pre => Base in 3 | 5 | 7
      is
         Power : Unsigned_64 := Base;
      begin
         while Power < Unsigned_64 (Group) loop
            pragma Loop_Invariant (Power in Base .. Unsigned_64 (Group_Index'Last));
            pragma Loop_Variant (Increases => Power);
            Power := Power * Base;
         end loop;
         return Power = Unsigned_64 (Group);
      end Power_Of;
   begin
      return Group <= 1 or else Power_Of (3) or else Power_Of (5) or else Power_Of (7);
   end Has_Backup;

   function Plan (Partition_Bytes : Unsigned_64; Ratio : Bytes_Per_Inode)
     return Volume
   is
      Blocks : Unsigned_64 :=
        Unsigned_64'Min (Partition_Bytes / Block_Bytes, Maximum_Blocks);
      Result : Volume;

      --  Groups, inodes and descriptor blocks for Blocks.
      procedure Shape is
         Groups : constant Unsigned_64 :=
           Unsigned_64'Max (1, (Blocks + Blocks_Per_Group - 1) / Blocks_Per_Group);
         Inodes : constant Unsigned_64 := Blocks * Block_Bytes / Unsigned_64 (Ratio);
         Per_Group : Unsigned_64 := (Inodes + Groups - 1) / Groups;
      begin
         Per_Group := (Per_Group + Inodes_Per_Block - 1) / Inodes_Per_Block * Inodes_Per_Block;
         Per_Group := Unsigned_64'Max (Inodes_Per_Block,
                                       Unsigned_64'Min (Per_Group, Blocks_Per_Group));
         Result :=
           (Blocks             => Unsigned_32 (Blocks),
            Groups             => Group_Total (Groups),
            Inodes_Per_Group   => Unsigned_32 (Per_Group),
            Inode_Table_Blocks => Unsigned_32 (Per_Group / Inodes_Per_Block),
            Descriptor_Blocks  => Unsigned_32
              ((Groups + Descriptors_Per_Block - 1) / Descriptors_Per_Block));
      end Shape;
   begin
      Shape;
      --  A last group too small for its metadata is dropped.
      if Result.Groups > 1
        and then Last_Group_Blocks (Result) <
                 Group_Overhead_Limit (Result) + Minimum_Group_Data
      then
         Blocks := Unsigned_64 (Result.Groups - 1) * Blocks_Per_Group;
         Shape;
      end if;
      return Result;
   end Plan;

   function Group_Blocks (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (if Group = Item.Groups - 1 then Last_Group_Blocks (Item) else Blocks_Per_Group)
     with Pre => Valid (Item) and then Group < Item.Groups;

   function Free_Blocks (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Group_Blocks (Item, Group)
      - (First_Data_Block (Item, Group) - Group_First (Group))
      - (if Group = 0 then Directory_Blocks else 0))
     with Pre => Valid (Item) and then Group < Item.Groups;

   function Free_Inodes (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Unsigned_64 (Item.Inodes_Per_Group) - (if Group = 0 then First_Inode else 0))
     with Pre => Valid (Item) and then Group < Item.Groups;

   function Superblock
     (Item : Volume; Request : Volume_Request; Group : Group_Index)
      return Superblock_Image
   is
      Result : Superblock_Image := [others => 0];
      Free_Block_Total : Unsigned_64 := 0;
      Inode_Total : constant Unsigned_32 := Item.Groups * Item.Inodes_Per_Group;
      --  Every group's inodes are free but the reserved ones and lost+found
      --  (Free_Inodes), all of which are in group 0.
      Free_Inode_Total : constant Unsigned_32 := Inode_Total - First_Inode;
   begin
      for G in 0 .. Item.Groups - 1 loop
         pragma Loop_Invariant (Free_Block_Total <= Unsigned_64 (G) * Blocks_Per_Group);
         Free_Block_Total := Free_Block_Total + Free_Blocks (Item, G);
      end loop;
      Put_32 (Result, S_Inodes_Count, Inode_Total);
      Put_32 (Result, S_Blocks_Count, Item.Blocks);
      Put_32 (Result, S_Free_Blocks_Count, Unsigned_32 (Free_Block_Total));
      Put_32 (Result, S_Free_Inodes_Count, Free_Inode_Total);
      Put_32 (Result, S_First_Data_Block, 0);
      Put_32 (Result, S_Log_Block_Size, Log_Block_Size);
      Put_32 (Result, S_Log_Frag_Size, Log_Block_Size);
      Put_32 (Result, S_Blocks_Per_Group, Blocks_Per_Group);
      Put_32 (Result, S_Frags_Per_Group, Blocks_Per_Group);
      Put_32 (Result, S_Inodes_Per_Group, Item.Inodes_Per_Group);
      Put_32 (Result, S_Wtime, Request.Created);
      Put_16 (Result, S_Max_Mnt_Count, Unlimited_Mounts);
      Put_16 (Result, S_Magic, EF.Signature);
      Put_16 (Result, S_State, Valid_FS);
      Put_16 (Result, S_Errors, Errors_Continue);
      Put_32 (Result, S_Lastcheck, Request.Created);
      Put_32 (Result, S_Creator_OS, Creator_Linux);
      Put_32 (Result, S_Rev_Level, EF.Dynamic_Revision);
      Put_32 (Result, S_First_Ino, First_Inode);
      Put_16 (Result, S_Inode_Size, Inode_Bytes);
      Put_16 (Result, S_Block_Group_Nr, Unsigned_16 (Group mod 2 ** 16));
      Put_32 (Result, S_Feature_Compat, 0);
      Put_32 (Result, S_Feature_Incompat, Feature_Incompat_Filetype);
      Put_32 (Result, S_Feature_Ro_Compat, Feature_Ro_Sparse_Super or Feature_Ro_Large_File);
      Put_GUID (Result, S_UUID, Request.Id);
      Put_Text (Result, S_Volume_Name, Request.Name);
      return Result;
   end Superblock;

   function Descriptor_Block (Item : Volume; Index : Unsigned_32) return Block_Image is
      Result : Block_Image := [others => 0];
      First : constant Unsigned_64 := Unsigned_64 (Index) * Descriptors_Per_Block;
      Last : constant Unsigned_64 :=
        Unsigned_64'Min (First + Descriptors_Per_Block, Unsigned_64 (Item.Groups)) - 1;
   begin
      for G in First .. Last loop
         declare
            Group : constant Group_Index := Group_Index (G);
            At_Byte : constant Natural :=
              Natural (G - First) * EF.Group_Descriptor_Bytes;
         begin
            Put_32 (Result, At_Byte + Bg_Block_Bitmap,
                    Unsigned_32 (Block_Bitmap_Block (Item, Group)));
            Put_32 (Result, At_Byte + Bg_Inode_Bitmap,
                    Unsigned_32 (Inode_Bitmap_Block (Item, Group)));
            Put_32 (Result, At_Byte + Bg_Inode_Table,
                    Unsigned_32 (Inode_Table_Block (Item, Group)));
            Put_16 (Result, At_Byte + Bg_Free_Blocks_Count,
                    Unsigned_16 (Free_Blocks (Item, Group)));
            Put_16 (Result, At_Byte + Bg_Free_Inodes_Count,
                    Unsigned_16 (Free_Inodes (Item, Group)));
            Put_16 (Result, At_Byte + Bg_Used_Dirs_Count,
                    (if Group = 0 then Directories_In_Use else 0));
         end;
      end loop;
      return Result;
   end Descriptor_Block;

   --  Sets bits First .. Last - 1 of a bitmap block.
   procedure Set_Bits (Map : in out Block_Image; First, Last : Unsigned_64)
     with Pre => Last <= Blocks_Per_Group
   is
   begin
      if First >= Last then
         return;
      end if;
      for Bit in First .. Last - 1 loop
         Map (Natural (Bit / EF.Bits_Per_Byte) + 1) :=
           Map (Natural (Bit / EF.Bits_Per_Byte) + 1)
           or Shift_Left (1, Natural (Bit mod EF.Bits_Per_Byte));
      end loop;
   end Set_Bits;

   function Block_Bitmap (Item : Volume; Group : Group_Index) return Block_Image is
      Result : Block_Image := [others => 0];
      Used : constant Unsigned_64 :=
        First_Data_Block (Item, Group) - Group_First (Group)
        + (if Group = 0 then Directory_Blocks else 0);
   begin
      Set_Bits (Result, 0, Used);
      --  Past the volume's end, the last group's bits are padding (set).
      Set_Bits (Result, Group_Blocks (Item, Group), Blocks_Per_Group);
      return Result;
   end Block_Bitmap;

   function Inode_Bitmap (Item : Volume; Group : Group_Index) return Block_Image is
      Result : Block_Image := [others => 0];
   begin
      if Group = 0 then
         Set_Bits (Result, 0, First_Inode);
      end if;
      Set_Bits (Result, Unsigned_64 (Item.Inodes_Per_Group), Blocks_Per_Group);
      return Result;
   end Inode_Bitmap;

   procedure Put_Directory_Inode
     (Data : in out Block_Image; Inode : Positive; Mode, Links : Unsigned_16;
      Block : Unsigned_64; Created : Unsigned_32)
     with Pre => Inode <= Inodes_Per_Block and then Block <= Maximum_Blocks
   is
      At_Byte : constant Natural := (Inode - 1) * Inode_Bytes;
   begin
      Put_16 (Data, At_Byte + I_Mode, Mode);
      Put_32 (Data, At_Byte + I_Size, Block_Bytes);
      Put_32 (Data, At_Byte + I_Atime, Created);
      Put_32 (Data, At_Byte + I_Ctime, Created);
      Put_32 (Data, At_Byte + I_Mtime, Created);
      Put_16 (Data, At_Byte + I_Links_Count, Links);
      Put_32 (Data, At_Byte + I_Blocks, Block_Bytes / Sector_Bytes);
      Put_32 (Data, At_Byte + I_Block, Unsigned_32 (Block));
   end Put_Directory_Inode;

   function First_Inode_Block (Item : Volume; Request : Volume_Request)
     return Block_Image
   is
      Result : Block_Image := [others => 0];
   begin
      Put_Directory_Inode (Result, Root_Inode, EF.Directory_Mode, Root_Links,
                           Root_Directory_Block (Item), Request.Created);
      Put_Directory_Inode (Result, Lost_Found_Inode, Lost_Found_Mode, Directory_Links,
                           Lost_Found_Block (Item), Request.Created);
      return Result;
   end First_Inode_Block;

   --  One directory record at At_Byte spanning Span bytes.
   procedure Put_Record
     (Data : in out Block_Image; At_Byte : Natural; Inode : Unsigned_32;
      Span : Natural; Name : String)
     with Pre => Name'Length in 1 .. EF.Maximum_Name_Bytes
                 and then Span in EF.Record_Header_Bytes .. Block_Bytes
                 and then Name'Length <= Span - EF.Record_Header_Bytes
                 and then At_Byte <= Block_Bytes - Span
   is
   begin
      Put_32 (Data, At_Byte + EF.Inode_Field, Inode);
      Put_16 (Data, At_Byte + EF.Span_Field, Unsigned_16 (Span));
      Put_8 (Data, At_Byte + EF.Length_Field, Unsigned_8 (Name'Length));
      Put_8 (Data, At_Byte + EF.Type_Field, EF.Filetype_Directory);
      Put_Text (Data, At_Byte + EF.Record_Header_Bytes, Name);
   end Put_Record;

   Dot_Span : constant := EF.Record_Header_Bytes + EF.Record_Alignment;   --  12

   function Root_Directory (Item : Volume) return Block_Image is
      pragma Unreferenced (Item);
      Result : Block_Image := [others => 0];
   begin
      Put_Record (Result, 0, Root_Inode, Dot_Span, ".");
      Put_Record (Result, Dot_Span, Root_Inode, Dot_Span, "..");
      Put_Record (Result, 2 * Dot_Span, Lost_Found_Inode,
                  Block_Bytes - 2 * Dot_Span, Lost_Found_Text);
      return Result;
   end Root_Directory;

   function Lost_Found_Directory (Item : Volume) return Block_Image is
      pragma Unreferenced (Item);
      Result : Block_Image := [others => 0];
   begin
      Put_Record (Result, 0, Lost_Found_Inode, Dot_Span, ".");
      Put_Record (Result, Dot_Span, Root_Inode, Block_Bytes - Dot_Span, "..");
      return Result;
   end Lost_Found_Directory;

end Ext2_Image;
