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
   I_Size_High           : constant := 108;
   Single_Indirect_Slot  : constant := 12;
   Double_Indirect_Slot  : constant := 13;
   Sector_Bytes          : constant := 512;

   Lost_Found_Mode       : constant := 16#41C0#;   --  S_IFDIR | 0700
   Directory_Links       : constant := 2;          --  "." and the parent's entry
   File_Links            : constant := 1;
   Lost_Found_Text       : constant String := "lost+found";
   Word_Modulus          : constant := 2 ** 32;

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

   function Group_Of (Block : Unsigned_64) return Group_Index is
     (Group_Index (Block / Blocks_Per_Group))
     with Pre => Block <= Maximum_Blocks;

   function Group_End (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (Group_First (Group) + Group_Blocks (Item, Group))
     with Pre => Valid (Item) and then Group < Item.Groups;

   function Next_Block (Item : Volume; Block : Unsigned_64) return Unsigned_64 is
      Group : constant Group_Index := Group_Of (Block);
   begin
      if Block + 1 < Group_End (Item, Group) then
         return Block + 1;
      elsif Group + 1 < Item.Groups then
         return First_Data_Block (Item, Group + 1);
      else
         return Unsigned_64 (Item.Blocks);
      end if;
   end Next_Block;

   function Advance (Item : Volume; Block, Steps : Unsigned_64) return Unsigned_64 is
      At_Block : Unsigned_64 := Block;
      Left : Unsigned_64 := Steps;
   begin
      while Left > 0 and then At_Block < Unsigned_64 (Item.Blocks) loop
         pragma Loop_Invariant (At_Block < Unsigned_64 (Item.Blocks));
         pragma Loop_Variant (Decreases => Left);
         declare
            Group : constant Group_Index := Group_Of (At_Block);
            Room : constant Unsigned_64 := Group_End (Item, Group) - At_Block;
         begin
            if Room = 0 then
               return Unsigned_64 (Item.Blocks);   --  never: At_Block is inside its group
            elsif Left < Room then
               return At_Block + Left;
            end if;
            Left := Left - Room;
            At_Block :=
              (if Group + 1 < Item.Groups then First_Data_Block (Item, Group + 1)
               else Unsigned_64 (Item.Blocks));
         end;
      end loop;
      return At_Block;
   end Advance;

   ---------------------------------------------------------------------------
   --  Directories: records packed into blocks, the last of each block
   --  spanning to its end.
   ---------------------------------------------------------------------------
   No_Target : constant Unsigned_64 := Unsigned_64'Last;

   function Parent_Inode (Layout : Tree_Layout; Files : Tree; Directory : Parent_Index)
     return Unsigned_32 is
     (if Directory = Volume_Root then Root_Inode
      elsif Files.Entries (Directory).Parent = Volume_Root then Root_Inode
      else Layout.Inode (Files.Entries (Directory).Parent))
     with Pre => Directory <= Files.Count;

   function Self_Inode (Layout : Tree_Layout; Directory : Parent_Index) return Unsigned_32 is
     (if Directory = Volume_Root then Root_Inode else Layout.Inode (Directory));

   --  Packs Directory's records; writes block Target of them into Image
   --  (none: No_Target) and reports how many blocks they take.
   procedure Pack
     (Layout : Tree_Layout; Files : Tree; Directory : Parent_Index; Target : Unsigned_64;
      Image : in out Block_Image; Blocks : out Unsigned_64)
     with Pre => Directory <= Files.Count
   is
      subtype Record_Start is Natural range 0 .. Block_Bytes - EF.Record_Header_Bytes;
      Block : Unsigned_64 := 0;
      Offset : Natural range 0 .. Block_Bytes := 0;
      Last_Offset : Record_Start := 0;

      procedure Close is
      begin
         if Block = Target then
            Put_16 (Image, Last_Offset + EF.Span_Field, Unsigned_16 (Block_Bytes - Last_Offset));
         end if;
      end Close;

      procedure Place (Inode : Unsigned_32; Name : String; Kind : Unsigned_8)
        with Pre => Name'Length in 1 .. EF.Maximum_Name_Bytes
      is
         Need : constant Natural := EF.Record_Bytes (Name'Length);
      begin
         if Offset + Need > Block_Bytes then
            Close;
            --  A directory has at most Direct_Blocks blocks (Plan_Tree).
            if Block < Unsigned_64'Last - 1 then
               Block := Block + 1;
            end if;
            Offset := 0;
         end if;
         pragma Assert (Offset + Need <= Block_Bytes);
         if Block = Target then
            Put_32 (Image, Offset + EF.Inode_Field, Inode);
            Put_16 (Image, Offset + EF.Span_Field, Unsigned_16 (Need));
            Put_8 (Image, Offset + EF.Length_Field, Unsigned_8 (Name'Length));
            Put_8 (Image, Offset + EF.Type_Field, Kind);
            Put_Text (Image, Offset + EF.Record_Header_Bytes, Name);
         end if;
         Last_Offset := Offset;
         Offset := Offset + Need;
      end Place;
   begin
      Place (Self_Inode (Layout, Directory), ".", EF.Filetype_Directory);
      Place (Parent_Inode (Layout, Files, Directory), "..", EF.Filetype_Directory);
      if Directory = Volume_Root then
         Place (Lost_Found_Inode, Lost_Found_Text, EF.Filetype_Directory);
      end if;
      for E in 1 .. Files.Count loop
         if Files.Entries (E).Parent = Directory then
            Place (Layout.Inode (E), Name_Of (Files, E),
                   (case Files.Entries (E).Kind is
                      when Install_Tree.Directory => EF.Filetype_Directory,
                      when Regular_File => EF.Filetype_Regular));
         end if;
      end loop;
      Close;
      Blocks := Block + 1;
   end Pack;

   function Directory_Blocks (Layout : Tree_Layout; Files : Tree; Directory : Parent_Index)
     return Unsigned_64
     with Pre => Directory <= Files.Count
   is
      Scratch : Block_Image := [others => 0];
      Blocks : Unsigned_64;
   begin
      Pack (Layout, Files, Directory, No_Target, Scratch, Blocks);
      return Blocks;
   end Directory_Blocks;

   function Directory_Block
     (Layout : Tree_Layout; Files : Tree; Directory : Parent_Index; Index : Unsigned_64)
      return Block_Image
   is
      Result : Block_Image := [others => 0];
      Blocks : Unsigned_64;
   begin
      Pack (Layout, Files, Directory, Index, Result, Blocks);
      return Result;
   end Directory_Block;

   function Lost_Found_Directory return Block_Image is
      Result : Block_Image := [others => 0];
      Dot_Span : constant := EF.Record_Header_Bytes + EF.Record_Alignment;   --  12
   begin
      Put_32 (Result, EF.Inode_Field, Lost_Found_Inode);
      Put_16 (Result, EF.Span_Field, Dot_Span);
      Put_8 (Result, EF.Length_Field, 1);
      Put_8 (Result, EF.Type_Field, EF.Filetype_Directory);
      Put_Text (Result, EF.Record_Header_Bytes, ".");
      Put_32 (Result, Dot_Span + EF.Inode_Field, Root_Inode);
      Put_16 (Result, Dot_Span + EF.Span_Field, Block_Bytes - Dot_Span);
      Put_8 (Result, Dot_Span + EF.Length_Field, 2);
      Put_8 (Result, Dot_Span + EF.Type_Field, EF.Filetype_Directory);
      Put_Text (Result, Dot_Span + EF.Record_Header_Bytes, "..");
      return Result;
   end Lost_Found_Directory;

   ---------------------------------------------------------------------------
   --  Planning.
   ---------------------------------------------------------------------------
   procedure Plan_Tree (Item : Volume; Files : Tree; Result : out Tree_Layout) is
      Blocks : constant Unsigned_64 := Unsigned_64 (Item.Blocks);
      Cursor : Unsigned_64 := First_Data_Block (Item, 0);
      Next_Inode : Unsigned_64 range First_Inode + 1 .. Unsigned_64'Last := First_Inode + 1;
      Inode_Limit : constant Unsigned_64 :=
        Unsigned_64 (Item.Groups) * Unsigned_64 (Item.Inodes_Per_Group);
      OK : Boolean := True;

      --  Takes Count blocks at the cursor: False when they run past the end.
      procedure Take (Count : Unsigned_64; First : out Unsigned_64) is
      begin
         First := Cursor;
         if Count = 0 then
            return;
         end if;
         if not OK or else Cursor >= Blocks then
            OK := False;
            return;
         end if;
         --  Advance stops at the volume's end: one more block must remain
         --  for the last of Count to be inside.
         if Count > 0 then
            declare
               Last : constant Unsigned_64 := Advance (Item, Cursor, Count - 1);
            begin
               if Last >= Blocks then
                  OK := False;
                  return;
               end if;
               Cursor := Next_Block (Item, Last);
            end;
         end if;
      end Take;

      procedure Number (E : Entry_Index)
        with Pre => Next_Inode <= Inode_Limit + 1,
             Post => Next_Inode in Next_Inode'Old .. Next_Inode'Old + 1
                     and then (if Next_Inode /= Next_Inode'Old then
                                 Next_Inode'Old <= Inode_Limit
                                 and then Next_Inode'Old - First_Inode <= Maximum_Entries);
      procedure Number (E : Entry_Index) is
      begin
         if Next_Inode > Inode_Limit or else Next_Inode - First_Inode > Maximum_Entries then
            OK := False;
            return;
         end if;
         Result.Inode (E) := Unsigned_32 (Next_Inode);
         Result.By_Inode (Entry_Index (Next_Inode - First_Inode)) := E;
         Next_Inode := Next_Inode + 1;
      end Number;
   begin
      Result := (others => <>);
      if Cursor >= Blocks then
         return;
      end if;
      --  Inodes: directories first (so the groups' directory counts follow
      --  from one range), then files, in tree order.
      for E in 1 .. Files.Count loop
         pragma Loop_Invariant
           (Next_Inode <= Inode_Limit + 1 and then Next_Inode - First_Inode <= Unsigned_64 (Maximum_Entries) + 1);
         if Files.Entries (E).Kind = Install_Tree.Directory then
            Number (E);
         end if;
      end loop;
      Result.Used.Directories := Unsigned_32 (Next_Inode - First_Inode - 1);
      for E in 1 .. Files.Count loop
         pragma Loop_Invariant
           (Next_Inode <= Inode_Limit + 1 and then Next_Inode - First_Inode <= Unsigned_64 (Maximum_Entries) + 1);
         if Files.Entries (E).Kind = Regular_File then
            Number (E);
         end if;
      end loop;
      if not OK then
         return;
      end if;
      Result.Used.Last_Inode := Unsigned_32 (Next_Inode - 1);

      Take (1, Result.Lost_Found);
      Result.Root_Blocks := Directory_Blocks (Result, Files, Volume_Root);
      OK := OK and then Result.Root_Blocks <= Direct_Blocks;
      Take (Result.Root_Blocks, Result.Root_Start);
      for E in 1 .. Files.Count loop
         if Files.Entries (E).Kind = Install_Tree.Directory then
            Result.Data (E) := Directory_Blocks (Result, Files, E);
            OK := OK and then Result.Data (E) <= Direct_Blocks;
            Take (Result.Data (E), Result.Start (E));
         end if;
      end loop;
      for E in 1 .. Files.Count loop
         if Files.Entries (E).Kind = Regular_File then
            Result.Data (E) := Data_Blocks (Files.Entries (E).Size);
            Result.Meta (E) := Indirect_Blocks (Result.Data (E));
            Take (Result.Data (E) + Result.Meta (E), Result.Start (E));
         end if;
      end loop;
      Result.Used.Data_End := Cursor;
      Result.Fits := OK;
   end Plan_Tree;

   ---------------------------------------------------------------------------
   --  Counts that follow from the allocation.
   ---------------------------------------------------------------------------
   function Overhead (Item : Volume; Group : Group_Index) return Unsigned_64 is
     (First_Data_Block (Item, Group) - Group_First (Group))
     with Pre => Valid (Item) and then Group < Item.Groups;

   --  Data blocks of Group below the allocation's end.
   function Used_Data (Item : Volume; Layout : Tree_Layout; Group : Group_Index)
     return Unsigned_64 is
     (if Layout.Used.Data_End >= Group_End (Item, Group)
      then Group_Blocks (Item, Group) - Overhead (Item, Group)
      elsif Layout.Used.Data_End > First_Data_Block (Item, Group)
      then Layout.Used.Data_End - First_Data_Block (Item, Group)
      else 0)
     with Pre => Valid (Item) and then Group < Item.Groups;

   function Free_Blocks (Item : Volume; Layout : Tree_Layout; Group : Group_Index)
     return Unsigned_64 is
     (Group_Blocks (Item, Group) - Overhead (Item, Group) - Used_Data (Item, Layout, Group))
     with Pre => Valid (Item) and then Group < Item.Groups;

   --  Inodes of Group in use: 1 .. Last_Inode.
   function Used_Inodes (Item : Volume; Layout : Tree_Layout; Group : Group_Index)
     return Unsigned_64 is
     (Unsigned_64'Min
        (Unsigned_64 (Item.Inodes_Per_Group),
         (if Unsigned_64 (Layout.Used.Last_Inode) > Unsigned_64 (Group) * Unsigned_64 (Item.Inodes_Per_Group)
          then Unsigned_64 (Layout.Used.Last_Inode) - Unsigned_64 (Group) * Unsigned_64 (Item.Inodes_Per_Group)
          else 0)))
     with Pre => Valid (Item) and then Group < Item.Groups;

   --  Directory inodes of Group: the root, lost+found and the tree's.
   function Used_Directories (Item : Volume; Layout : Tree_Layout; Group : Group_Index)
     return Unsigned_64
     with Pre => Valid (Item) and then Group < Item.Groups
   is
      First : constant Unsigned_64 := Unsigned_64 (Group) * Unsigned_64 (Item.Inodes_Per_Group) + 1;
      Last : constant Unsigned_64 := First + Unsigned_64 (Item.Inodes_Per_Group) - 1;
      Low : constant Unsigned_64 := Unsigned_64'Max (First, Lost_Found_Inode);
      High : constant Unsigned_64 :=
        Unsigned_64'Min (Last, Lost_Found_Inode + Unsigned_64 (Layout.Used.Directories));
   begin
      return (if Root_Inode in First .. Last then 1 else 0)
        + (if Low <= High then High - Low + 1 else 0);
   end Used_Directories;

   function Superblock
     (Item : Volume; Layout : Tree_Layout; Request : Volume_Request; Group : Group_Index)
      return Superblock_Image
   is
      Result : Superblock_Image := [others => 0];
      Free_Block_Total : Unsigned_64 := 0;
      Inode_Total : constant Unsigned_32 := Item.Groups * Item.Inodes_Per_Group;
      Free_Inode_Total : constant Unsigned_32 :=
        (if Layout.Used.Last_Inode <= Inode_Total then Inode_Total - Layout.Used.Last_Inode else 0);
   begin
      for G in 0 .. Item.Groups - 1 loop
         pragma Loop_Invariant (Free_Block_Total <= Unsigned_64 (G) * Blocks_Per_Group);
         Free_Block_Total := Free_Block_Total + Free_Blocks (Item, Layout, G);
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

   function Count_16 (Value : Unsigned_64) return Unsigned_16 is
     (Unsigned_16 (Unsigned_64'Min (Value, Unsigned_64 (Unsigned_16'Last))));

   function Descriptor_Block
     (Item : Volume; Layout : Tree_Layout; Index : Unsigned_32) return Block_Image
   is
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
            --  Each count is at most a group's blocks or inodes (32768).
            Put_16 (Result, At_Byte + Bg_Free_Blocks_Count, Count_16 (Free_Blocks (Item, Layout, Group)));
            Put_16 (Result, At_Byte + Bg_Free_Inodes_Count,
                    Count_16 (Unsigned_64 (Item.Inodes_Per_Group) - Used_Inodes (Item, Layout, Group)));
            Put_16 (Result, At_Byte + Bg_Used_Dirs_Count, Count_16 (Used_Directories (Item, Layout, Group)));
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

   function Block_Bitmap
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Block_Image
   is
      Result : Block_Image := [others => 0];
   begin
      Set_Bits (Result, 0, Overhead (Item, Group) + Used_Data (Item, Layout, Group));
      --  Past the volume's end, the last group's bits are padding (set).
      Set_Bits (Result, Group_Blocks (Item, Group), Blocks_Per_Group);
      return Result;
   end Block_Bitmap;

   function Inode_Bitmap
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Block_Image
   is
      Result : Block_Image := [others => 0];
   begin
      Set_Bits (Result, 0, Used_Inodes (Item, Layout, Group));
      Set_Bits (Result, Unsigned_64 (Item.Inodes_Per_Group), Blocks_Per_Group);
      return Result;
   end Inode_Bitmap;

   function Used_Table_Blocks
     (Item : Volume; Layout : Tree_Layout; Group : Group_Index) return Unsigned_32 is
     (Unsigned_32 ((Used_Inodes (Item, Layout, Group) + Inodes_Per_Block - 1) / Inodes_Per_Block));

   ---------------------------------------------------------------------------
   --  Inodes.
   ---------------------------------------------------------------------------
   procedure Put_Inode
     (Data : in out Block_Image; Slot : Natural; Mode : Unsigned_16; Links : Unsigned_16;
      Size : Unsigned_64; Blocks : Unsigned_64; Created : Unsigned_32)
     with Pre => Slot < Inodes_Per_Block
   is
      At_Byte : constant Natural := Slot * Inode_Bytes;
   begin
      Put_16 (Data, At_Byte + I_Mode, Mode);
      Put_32 (Data, At_Byte + I_Size, Unsigned_32 (Size mod Word_Modulus));
      Put_32 (Data, At_Byte + I_Size_High, Unsigned_32 ((Size / Word_Modulus) mod Word_Modulus));
      Put_32 (Data, At_Byte + I_Atime, Created);
      Put_32 (Data, At_Byte + I_Ctime, Created);
      Put_32 (Data, At_Byte + I_Mtime, Created);
      Put_16 (Data, At_Byte + I_Links_Count, Links);
      Put_32 (Data, At_Byte + I_Blocks,
              Unsigned_32 ((Blocks * (Block_Bytes / Sector_Bytes)) mod Word_Modulus));
   end Put_Inode;

   procedure Put_Pointer
     (Data : in out Block_Image; Slot : Natural; Index : Natural; Block : Unsigned_64)
     with Pre => Slot < Inodes_Per_Block and then Index <= Double_Indirect_Slot
   is
   begin
      Put_32 (Data, Slot * Inode_Bytes + I_Block + Index * EF.Block_Pointer_Bytes,
              Unsigned_32 (Block mod Word_Modulus));
   end Put_Pointer;

   --  A directory's inode: its blocks are direct (at most Direct_Blocks).
   procedure Put_Directory
     (Item : Volume; Data : in out Block_Image; Slot : Natural; Mode : Unsigned_16;
      Links : Unsigned_64; Start, Count : Unsigned_64; Created : Unsigned_32)
     with Pre => Valid (Item) and then Slot < Inodes_Per_Block
                 and then Start <= Unsigned_64 (Item.Blocks)
   is
      Block : Unsigned_64 := Start;
   begin
      Put_Inode (Data, Slot, Mode, Unsigned_16 (Unsigned_64'Min (Links, Unsigned_64 (Unsigned_16'Last))),
                 Unsigned_64'Min (Count, Direct_Blocks) * Block_Bytes,
                 Unsigned_64'Min (Count, Direct_Blocks), Created);
      for K in 0 .. Natural (Unsigned_64'Min (Count, Direct_Blocks)) - 1 loop
         exit when Block >= Unsigned_64 (Item.Blocks);
         Put_Pointer (Data, Slot, K, Block);
         Block := Next_Block (Item, Block);
      end loop;
   end Put_Directory;

   function Child_Directories (Files : Tree; Directory : Parent_Index) return Unsigned_64
     with Post => Child_Directories'Result <= Maximum_Entries;
   function Child_Directories (Files : Tree; Directory : Parent_Index) return Unsigned_64 is
      Count : Natural range 0 .. Maximum_Entries := 0;
   begin
      for E in 1 .. Files.Count loop
         pragma Loop_Invariant (Count < E);
         if Files.Entries (E).Parent = Directory and then Files.Entries (E).Kind = Install_Tree.Directory then
            Count := Count + 1;
         end if;
      end loop;
      return Unsigned_64 (Count);
   end Child_Directories;

   procedure Put_File
     (Item : Volume; Layout : Tree_Layout; Data : in out Block_Image; Slot : Natural;
      File : Entry_Index; Size : Unsigned_64; Created : Unsigned_32)
     with Pre => Valid (Item) and then Slot < Inodes_Per_Block
   is
      Count : constant Unsigned_64 := Layout.Data (File);
      Meta : constant Unsigned_64 := Layout.Meta (File);
      Start : constant Unsigned_64 := Unsigned_64'Min (Layout.Start (File), Unsigned_64 (Item.Blocks));
      Block : Unsigned_64;
   begin
      Put_Inode (Data, Slot, EF.Regular_Mode, File_Links, Size,
                 Unsigned_64'Min (Count, Maximum_Blocks) + Unsigned_64'Min (Meta, Maximum_Blocks),
                 Created);
      if Count > 0 then
         Block := Advance (Item, Start, Unsigned_64'Min (Meta, Maximum_Blocks));
         for K in 0 .. Natural (Unsigned_64'Min (Count, Direct_Blocks)) - 1 loop
            exit when Block >= Unsigned_64 (Item.Blocks);
            Put_Pointer (Data, Slot, K, Block);
            Block := Next_Block (Item, Block);
         end loop;
      end if;
      if Meta >= 1 then
         Put_Pointer (Data, Slot, Single_Indirect_Slot, Start);
      end if;
      if Meta >= 2 and then Start < Unsigned_64 (Item.Blocks) then
         Put_Pointer (Data, Slot, Double_Indirect_Slot, Next_Block (Item, Start));
      end if;
   end Put_File;

   function Inode_Table
     (Item : Volume; Layout : Tree_Layout; Files : Tree; Request : Volume_Request;
      Group : Group_Index; Index : Unsigned_32) return Block_Image
   is
      Result : Block_Image := [others => 0];
      Base : constant Unsigned_64 :=
        Unsigned_64 (Group) * Unsigned_64 (Item.Inodes_Per_Group)
        + Unsigned_64 (Index) * Inodes_Per_Block;
   begin
      for Slot in 0 .. Inodes_Per_Block - 1 loop
         declare
            N : constant Unsigned_64 := Base + Unsigned_64 (Slot) + 1;
         begin
            if N = Root_Inode then
               Put_Directory (Item, Result, Slot, EF.Directory_Mode,
                              Directory_Links + 1 + Child_Directories (Files, Volume_Root),
                              Unsigned_64'Min (Layout.Root_Start, Unsigned_64 (Item.Blocks)),
                              Layout.Root_Blocks, Request.Created);
            elsif N = Lost_Found_Inode then
               Put_Directory (Item, Result, Slot, Lost_Found_Mode, Directory_Links,
                              Unsigned_64'Min (Layout.Lost_Found, Unsigned_64 (Item.Blocks)),
                              1, Request.Created);
            elsif N > First_Inode and then N <= Unsigned_64 (Layout.Used.Last_Inode)
              and then N - First_Inode <= Maximum_Entries
            then
               declare
                  E : constant Entry_Index := Layout.By_Inode (Entry_Index (N - First_Inode));
               begin
                  if E <= Files.Count then
                     case Files.Entries (E).Kind is
                        when Install_Tree.Directory =>
                           Put_Directory (Item, Result, Slot, EF.Directory_Mode,
                                          Directory_Links + Child_Directories (Files, E),
                                          Unsigned_64'Min (Layout.Start (E), Unsigned_64 (Item.Blocks)),
                                          Layout.Data (E), Request.Created);
                        when Regular_File =>
                           Put_File (Item, Layout, Result, Slot, E,
                                     Files.Entries (E).Size, Request.Created);
                     end case;
                  end if;
               end;
            end if;
         end;
      end loop;
      return Result;
   end Inode_Table;

   function Indirect_Block
     (Item : Volume; Layout : Tree_Layout; File : Entry_Index; Index : Unsigned_64)
      return Block_Image
   is
      Result : Block_Image := [others => 0];
      Count : constant Unsigned_64 := Unsigned_64'Min (Layout.Data (File), Maximum_Blocks);
      Meta : constant Unsigned_64 := Unsigned_64'Min (Layout.Meta (File), Maximum_Blocks);
      Start : constant Unsigned_64 := Unsigned_64'Min (Layout.Start (File), Unsigned_64 (Item.Blocks));
      First_Data, Pointers, Block : Unsigned_64;
   begin
      if Index = 1 then
         --  The double indirect block: its children, meta blocks 2 ...
         First_Data := 0;
         Pointers := (if Meta > 2 then Meta - 2 else 0);
         Block := Advance (Item, Start, 2);
      else
         --  A single indirect block: data from Direct_Blocks, then each
         --  child of the double indirect block maps the next block's worth.
         First_Data := Direct_Blocks
           + (if Index = 0 then 0 else (Index - 1) * Pointers_Per_Block);
         Pointers := (if Count > First_Data then Count - First_Data else 0);
         Block := Advance (Item, Start, Meta + First_Data);
      end if;
      Pointers := Unsigned_64'Min (Pointers, Pointers_Per_Block);
      for K in 0 .. Natural (Pointers) - 1 loop
         exit when Block >= Unsigned_64 (Item.Blocks);
         Put_32 (Result, K * EF.Block_Pointer_Bytes, Unsigned_32 (Block mod Word_Modulus));
         Block := Next_Block (Item, Block);
      end loop;
      return Result;
   end Indirect_Block;

end Ext2_Image;
