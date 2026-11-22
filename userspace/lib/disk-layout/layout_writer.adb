------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Partition_Tables;
with GPT_Image;
with Ext2_Format;

package body Layout_Writer
  with SPARK_Mode, Refined_State => (Layouts => (ESP_Layout, Root_Layout, Scratch))
is

   ESP_Layout : FAT32_Image.Tree_Layout;
   Root_Layout : Ext2_Image.Tree_Layout;
   --  Read-back buffer (Verifying passes).
   Scratch : Bytes (1 .. Batch_Bytes) := [others => 0];

   use Install_Tree;
   use type Partition_Tables.Bytes;

   --  Each region is walked twice with the same addresses: once writing,
   --  once reading back (Verifying).
   type Pass is (Writing, Verifying);

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

   --  Writes Data at Offset, or (Verifying) reads it back and compares.
   --  Same False: it was read and differs.
   procedure Place
     (Mode : Pass; Offset : Byte_Offset; Data : Bytes; OK, Same : in out Boolean)
     with Global => (In_Out => Scratch),
          Pre => Data'First = 1 and then Data'Length <= Batch_Bytes
   is
   begin
      if not OK or else not Same then
         return;
      end if;
      case Mode is
         when Writing =>
            Put (Offset, Data, OK);
         when Verifying =>
            Get (Offset, Scratch (1 .. Data'Length), OK);
            Same := not OK or else Scratch (1 .. Data'Length) = Data;
      end case;
   end Place;

   --  A file's bytes back from the disk (Verifying): Count bytes of the
   --  run at Offset, which hold File's bytes from Done; Final: its last.
   procedure Read_Back
     (Volume : Install_Volume; File : Entry_Index; Offset : Byte_Offset;
      Done, Count : Unsigned_64; Padded : Natural; Final : Boolean; OK, Same : in out Boolean)
     with Global => (In_Out => Scratch),
          Pre => Count <= Unsigned_64 (Padded) and then Padded in 1 .. Batch_Bytes
   is
      Matches : Boolean;
   begin
      if not OK or else not Same then
         return;
      end if;
      Get (Offset, Scratch (1 .. Padded), OK);
      if OK then
         Check (Volume, File, Done, Scratch (1 .. Natural (Count)), Final, Matches);
         Same := Matches;
      end if;
   end Read_Back;

   --  Ends a pass: what went wrong first, if anything.
   function Outcome (OK, Same, Source_OK : Boolean) return Write_Result is
     (if not Source_OK then Source_Failed
      elsif not OK then Device_Failed
      elsif not Same then Verify_Failed
      else Written);

   ---------------------------------------------------------------------------
   --  The ESP (FAT32). Its boot sector, backup and FSInfo copies are
   --  written only by Boot_Sectors (Making_Bootable): until then the volume
   --  is not FAT32 to any reader.
   ---------------------------------------------------------------------------
   function ESP_Volume_Of (Shape : Disk_Shape; Layout : Install_Layout) return FAT32_Image.Volume is
     (FAT32_Image.Plan
        (Shape.Block_Size,
         --  At most 64 GiB of 512-byte sectors: Maximum_Sectors.
         FAT32_Image.Sector_Count (Unsigned_64'Min (MiB_Blocks (Shape, Layout.Disk.ESP_Size),
                                                    FAT32_Image.Maximum_Sectors)),
         Unsigned_32 (Unsigned_64'Min (ESP_First (Shape), Unsigned_64 (Unsigned_32'Last)))))
     with Pre => Fits (Shape, Layout.Disk);

   procedure ESP_Body
     (Mode : Pass; Shape : Disk_Shape; Layout : Install_Layout; Files : Tree; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => (ESP_Layout, Scratch))
   is
      use FAT32_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := ESP_First (Shape) * B;
      Item : constant Volume := ESP_Volume_Of (Shape, Layout);
      Files_Layout : Tree_Layout renames ESP_Layout;
      OK : Boolean := True;
      Same : Boolean := True;
      Source_OK : Boolean := True;
      Buffer : Bytes (1 .. Batch_Bytes) := [others => 0];

      --  Sectors of the volume; never past its end.
      procedure Sector (Index : Unsigned_64; Data : Bytes)
        with Pre => Data'First = 1 and then Data'Length <= Batch_Bytes
      is
      begin
         if OK and then Index < Unsigned_64 (Item.Sectors)
           and then Index * B <= Byte_Offset'Last - Base
         then
            Place (Mode, Base + Index * B, Data, OK, Same);
         else
            OK := False;
         end if;
      end Sector;

      procedure Clear (Index, Count : Unsigned_64) is
      begin
         if Mode = Verifying then
            return;   --  zeroed regions are not read back
         elsif OK and then Index <= Unsigned_64 (Item.Sectors)
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
      if Mode = Writing then
         Plan_Tree (Item, Files, Files_Layout);
      end if;
      if not Files_Layout.Fits then
         Result := ESP_Files_Do_Not_Fit;
         return;
      end if;
      --  The reserved region holds the boot sectors and FSInfo: zero now,
      --  written by Boot_Sectors last.
      Clear (0, Reserved_Sectors);
      for Copy in 1 .. FAT_Count loop
         Clear (FAT_Start (Item, Copy), Unsigned_64 (Item.FAT_Sectors));
         --  Counted from 1: an unsigned 0 - 1 would wrap.
         for S in 1 .. Used_FAT_Sectors (Item, Files_Layout) loop
            exit when not OK or else not Same;
            Sector (FAT_Start (Item, Copy) + (S - 1), FAT_Sector (Item, Files_Layout, Files, S - 1));
         end loop;
      end loop;
      Sector (Cluster_Sector (Item, Root_Cluster),
              Directory_Cluster (Files_Layout, Files, Volume_Root, Layout.ESP_Label));
      for E in 1 .. Files.Count loop
         exit when not OK or else not Source_OK or else not Same;
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
                     if Mode = Verifying and then (Size = 0 or else Start < Root_Cluster) then
                        Read_Back (ESP_Volume, E, Base, 0, 0, Natural (B), True, OK, Same);
                     end if;
                     while OK and then Source_OK and then Same and then Done < Size
                       and then Start >= Root_Cluster
                     loop
                        pragma Loop_Invariant (Done < Size);
                        declare
                           Count : constant Unsigned_64 := Unsigned_64'Min (Size - Done, Batch_Bytes);
                           Padded : constant Natural :=
                             Natural ((Count + B - 1) / B * B);
                           At_Sector : constant Unsigned_64 := Cluster_Sector (Item, Start) + Done / B;
                        begin
                           case Mode is
                              when Writing =>
                                 Fill (ESP_Volume, E, Done, Count, Padded, Buffer, Source_OK);
                                 if Source_OK then
                                    Sector (At_Sector, Buffer (1 .. Padded));
                                 end if;
                              when Verifying =>
                                 if At_Sector < Unsigned_64 (Item.Sectors)
                                   and then At_Sector * B <= Byte_Offset'Last - Base
                                 then
                                    Read_Back (ESP_Volume, E, Base + At_Sector * B, Done, Count, Padded,
                                               Done + Count = Size, OK, Same);
                                 else
                                    OK := False;
                                 end if;
                           end case;
                           Done := Done + Count;
                        end;
                     end loop;
                  end;
            end case;
         end;
      end loop;
      Result := Outcome (OK, Same, Source_OK);
   end ESP_Body;

   --  Making_Bootable: FSInfo (and its backup), the backup boot sector,
   --  then the boot sector itself, the write that makes the disk bootable.
   --  Each is read back; a boot sector that does not read back is zeroed
   --  again (the disk stays unbootable).
   procedure Boot_Sectors (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => Scratch, Input => ESP_Layout)
   is
      use FAT32_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := ESP_First (Shape) * B;
      Item : constant Volume := ESP_Volume_Of (Shape, Layout);
      OK : Boolean := True;
      Same : Boolean := True;
   begin
      if not Valid (Item) or else not ESP_Layout.Fits then
         Result := Layout_Invalid;
         return;
      end if;
      declare
         Boot : constant Sector_Image := Boot_Sector (Item, Layout.ESP_Serial, Layout.ESP_Label);
         Info : constant Sector_Image := FSInfo (Item, ESP_Layout);
      begin
         for Mode in Pass loop
            Place (Mode, Base + FSInfo_Sector * B, Info, OK, Same);
            Place (Mode, Base + (Backup_Boot_Sector + FSInfo_Sector) * B, Info, OK, Same);
            Place (Mode, Base + Backup_Boot_Sector * B, Boot, OK, Same);
         end loop;
         Place (Writing, Base, Boot, OK, Same);
         Place (Verifying, Base, Boot, OK, Same);
      end;
      if OK and then not Same then
         declare
            Undo : Boolean := True;
         begin
            Zero (Base, B, Undo);
         end;
      end if;
      Result := Outcome (OK, Same, True);
   end Boot_Sectors;

   ---------------------------------------------------------------------------
   --  The root (ext2). Its group-0 superblock is written last: until then
   --  the partition is not an ext2 volume to the filesystem service.
   ---------------------------------------------------------------------------
   procedure Root_Pass
     (Mode : Pass; Shape : Disk_Shape; Layout : Install_Layout; Files : Tree; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => (Root_Layout, Scratch))
   is
      use Ext2_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Base : constant Byte_Offset := Root_First (Shape, Layout.Disk) * B;
      Root_Bytes : constant Unsigned_64 :=
        (Root_Last (Shape, Layout.Disk) - Root_First (Shape, Layout.Disk) + 1) * B;
      Item : constant Volume := Plan (Root_Bytes, Layout.Root.Inode_Ratio);
      Files_Layout : Tree_Layout renames Root_Layout;
      OK : Boolean := True;
      Same : Boolean := True;
      Source_OK : Boolean := True;
      Buffer : Bytes (1 .. Batch_Bytes) := [others => 0];
      Storing : Boolean := False;
      Generating : Boolean := False;

      --  Blocks outside the volume are never written: such a request
      --  fails the run (it cannot happen for a valid plan: the volume
      --  ends inside the root partition, which ends inside the disk).
      procedure Block (Index : Unsigned_64; Data : Bytes)
        with Pre => Data'First = 1 and then Data'Length <= Batch_Bytes
      is
      begin
         if OK and then Index < Unsigned_64 (Item.Blocks)
           and then Index * Block_Bytes <= Byte_Offset'Last - Base
         then
            Place (Mode, Base + Index * Block_Bytes, Data, OK, Same);
         else
            OK := False;
         end if;
      end Block;

      procedure Clear (Index, Count : Unsigned_64) is
      begin
         if Mode = Verifying then
            return;
         elsif OK and then Index <= Unsigned_64 (Item.Blocks)
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
            exit when not OK or else not Same or else At_Block >= Unsigned_64 (Item.Blocks);
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
            exit when not OK or else not Same or else At_Block >= Unsigned_64 (Item.Blocks);
            Block (At_Block, Indirect_Block (Item, Files_Layout, File, K - 1));
            At_Block := Next_Block (Item, At_Block);
         end loop;
         if Mode = Verifying and then Size = 0 then
            Read_Back (Root_Volume, File, Base, 0, 0, Block_Bytes, True, OK, Same);
         end if;
         while OK and then Source_OK and then Same and then Done < Size
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
               case Mode is
                  when Writing =>
                     Fill (Root_Volume, File, Done, Count, Natural (Run * Block_Bytes), Buffer, Source_OK);
                     if Source_OK then
                        Block (At_Block, Buffer (1 .. Natural (Run * Block_Bytes)));
                     end if;
                  when Verifying =>
                     if At_Block * Block_Bytes <= Byte_Offset'Last - Base then
                        Read_Back (Root_Volume, File, Base + At_Block * Block_Bytes, Done, Count,
                                   Natural (Run * Block_Bytes), Done + Count = Size, OK, Same);
                     else
                        OK := False;
                     end if;
               end case;
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
      if Mode = Writing then
         Plan_Tree (Item, Files, Files_Layout);
      end if;
      if not Files_Layout.Fits then
         Result := Root_Files_Do_Not_Fit;
         return;
      end if;
      for Group in 0 .. Item.Groups - 1 loop
         if Has_Backup (Group) then
            --  Group 0's superblock is the volume's: written last.
            if Group /= 0 then
               Block (Group_First (Group), Superblock_Block (Group));
            end if;
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
               exit when not OK or else not Same;
               Block (Inode_Table_Block (Item, Group) + Unsigned_64 (Index - 1),
                      Inode_Table (Item, Files_Layout, Files, Layout.Root, Group, Index - 1));
            end loop;
            Clear (Inode_Table_Block (Item, Group) + Unsigned_64 (Used),
                   Unsigned_64 (Item.Inode_Table_Blocks - Used));
         end;
         exit when not OK or else not Same;
      end loop;
      Block (Files_Layout.Lost_Found, Lost_Found_Directory);
      Write_Directory (Volume_Root, Files_Layout.Root_Start, Files_Layout.Root_Blocks);
      for E in 1 .. Files.Count loop
         exit when not OK or else not Source_OK or else not Same;
         case Files.Entries (E).Kind is
            when Directory =>
               Write_Directory (E, Files_Layout.Start (E), Files_Layout.Data (E));
            when Regular_File =>
               if Mode = Writing then
                  if E >= Layout.Generation_First and then not Generating then
                     Generating := True;
                     Enter (Install_Steps.Writing_Generation);
                  elsif not Storing and then not Generating then
                     Storing := True;
                     Enter (Install_Steps.Filling_Storehouse);
                  end if;
               end if;
               Write_File (E);
         end case;
      end loop;
      --  The superblock last: the root is an ext2 volume from here on.
      Block (Group_First (0), Superblock_Block (0));
      Result := Outcome (OK, Same, Source_OK);
   end Root_Pass;

   ---------------------------------------------------------------------------
   --  Partitioning.
   ---------------------------------------------------------------------------
   procedure Partition (Shape : Disk_Shape; Layout : Install_Layout; Result : out Write_Result)
     with Pre => Fits (Shape, Layout.Disk), Global => (In_Out => Scratch)
   is
      use GPT_Image;
      B : constant Unsigned_64 := Unsigned_64 (Shape.Block_Size);
      Table : constant GPT_Image.Entry_Array := Entries (Shape, Layout.Disk);
      Table_CRC : constant Unsigned_32 := Partition_Tables.CRC32 (Table);
      OK : Boolean := True;
      Same : Boolean := True;
   begin
      --  First, neither partition is a filesystem: the ESP's reserved
      --  sectors (its boot sector, FSInfo and their backups: a disk an
      --  earlier install left bootable is no longer) and the root's first
      --  block (its superblock).
      Zero (ESP_First (Shape) * B, FAT32_Image.Reserved_Sectors * B, OK);
      if OK then
         Zero (Root_First (Shape, Layout.Disk) * B, Ext2_Image.Block_Bytes, OK);
      end if;
      for Mode in Pass loop
         Place (Mode, Entries_LBA (Shape, Backup) * B, Table, OK, Same);
         Place (Mode, Header_LBA (Shape, Backup) * B, Header (Shape, Layout.Disk, Backup, Table_CRC), OK, Same);
         Place (Mode, Entries_LBA (Shape, Primary) * B, Table, OK, Same);
         Place (Mode, Header_LBA (Shape, Primary) * B, Header (Shape, Layout.Disk, Primary, Table_CRC), OK, Same);
         Place (Mode, 0, Protective_MBR (Shape), OK, Same);
      end loop;
      Result := Outcome (OK, Same, True);
   end Partition;

   procedure Write_Disk
     (Shape : Disk_Shape; Layout : Install_Layout;
      ESP_Files, Root_Files : Install_Tree.Tree; Result : out Write_Result;
      Reached : out Install_Steps.Step)
     with Refined_Global => (In_Out => (ESP_Layout, Root_Layout, Scratch))
   is
      Durable : Boolean;

      procedure Step (Next : Install_Steps.Step) is
      begin
         Reached := Next;
         Enter (Next);
      end Step;
   begin
      Step (Install_Steps.Partitioning);
      Partition (Shape, Layout, Result);
      if Result = Written then
         Step (Install_Steps.Formatting_Root);
         Root_Pass (Writing, Shape, Layout, Root_Files, Result);
      end if;
      if Result = Written then
         Step (Install_Steps.Verifying_Root);
         Flush (Durable);
         Root_Pass (Verifying, Shape, Layout, Root_Files, Result);
         if Result = Written and then not Durable then
            Result := Device_Failed;
         end if;
      end if;
      if Result = Written then
         Step (Install_Steps.Writing_ESP);
         ESP_Body (Writing, Shape, Layout, ESP_Files, Result);
      end if;
      if Result = Written then
         Step (Install_Steps.Verifying_ESP);
         Flush (Durable);
         ESP_Body (Verifying, Shape, Layout, ESP_Files, Result);
         if Result = Written and then not Durable then
            Result := Device_Failed;
         end if;
      end if;
      if Result = Written then
         Step (Install_Steps.Making_Bootable);
         Boot_Sectors (Shape, Layout, Result);
      end if;
      if Result = Written then
         Step (Install_Steps.Flushing);
         Flush (Durable);
         Result := (if Durable then Written else Device_Failed);
      end if;
      if Result = Written then
         Step (Install_Steps.Done);
      elsif Reached = Install_Steps.Flushing then
         --  The boot sector is written but may not be durable: the step
         --  that failed is the one that made the disk bootable.
         Reached := Install_Steps.Making_Bootable;
      end if;
   end Write_Disk;

end Layout_Writer;
