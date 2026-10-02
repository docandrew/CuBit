with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Ext2; use Ext2;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with Volume_Admission; use Volume_Admission;
with Block_Inventory;
with Block_Paths;
with Sector_Accounting;

--  Production resizeFile over a sparse triple-indirect tree at every block
--  geometry. Every transport boundary fails before/partially/after I/O, and
--  every attempted bitmap clear is checked against the last flushed tree.
procedure Triple_Resize is
   use type Inode;
   Fs : Filesystem;
   Result : Inode;
   Status : Truncate_Status;
   Faults : Natural := 0;
   --  Blocks 6 .. 15: triple root, two middles, three leaves, four data.
   Tree_Blocks : constant := 10;
   First_Tree_Block : constant := 6;
begin
   for Geometry in 0 .. 2 loop
      declare
         Size : constant Positive := 1024 * 2 ** Geometry;
         Sectors : constant Sector_Accounting.Block_Sectors :=
           Sector_Accounting.Block_Sectors (Size / 512);
         Bytes : constant Unsigned_64 := Unsigned_64 (Size);
         Pointers : constant Unsigned_64 := Block_Paths.Pointer_Count (Sectors);
         First_Triple : constant Unsigned_64 := Block_Paths.First_Triple (Sectors);
         Middle_Span : constant Unsigned_64 := Block_Paths.Middle_Span (Sectors);
         Sb : Superblock with Import, Address => Disk (1024)'Address;
         Bgd : BlockGroupDescriptor with Import,
           Address => Disk ((if Size = 1024 then 2 else 1) * Size)'Address;
         Item : Inode with Import, Address => Disk (5 * Size)'Address;
         type Pointers_Block is array (0 .. Size / 4 - 1) of Unsigned_32;
         Root : Pointers_Block with Import, Address => Disk (6 * Size)'Address;
         Middle_0 : Pointers_Block with Import, Address => Disk (7 * Size)'Address;
         Leaf_00 : Pointers_Block with Import, Address => Disk (8 * Size)'Address;
         Leaf_01 : Pointers_Block with Import, Address => Disk (11 * Size)'Address;
         Middle_1 : Pointers_Block with Import, Address => Disk (13 * Size)'Address;
         Leaf_10 : Pointers_Block with Import, Address => Disk (14 * Size)'Address;
         End_Of_Tree : constant Unsigned_64 := (First_Triple + Middle_Span + 1) * Bytes;
         --  Each boundary: partial leaf, whole leaf, whole middle, whole root,
         --  unchanged tree and sparse growth past every mapped block.
         Sizes : constant array (Positive range <>) of Unsigned_64 :=
           [0, 17, First_Triple * Bytes, First_Triple * Bytes + 17,
            (First_Triple + 1) * Bytes + 17,
            (First_Triple + Pointers) * Bytes,
            (First_Triple + Pointers) * Bytes + 1,
            (First_Triple + Middle_Span) * Bytes,
            (First_Triple + Middle_Span) * Bytes + 17,
            End_Of_Tree + 2 * Bytes];
         Retained : constant array (Sizes'Range) of Natural :=
           [0, 0, 0, 4, 5, 5, 7, 7, 10, 10];

         procedure Setup is
            Admission : Admission_Result;
         begin
            Reset;
            Check_Reclamation := False;
            Check_Resize_Reclamation := True;
            Reclamation_Block_Bytes := Size;
            Reclamation_First_Block := (if Size = 1024 then 1 else 0);
            Sb.signature := EXT2_SIGNATURE;
            Sb.blockShift := Unsigned_32 (Geometry);
            Sb.inodeCount := 8;
            Sb.blockCount := Unsigned_32 (Disk'Length / Size);
            Sb.freeBlocks := Sb.blockCount - (First_Tree_Block + Tree_Blocks);
            Sb.firstDataBlock := Unsigned_32 (Reclamation_First_Block);
            Sb.blocksPerBlockGroup := Sb.blockCount;
            Sb.inodesPerBlockGroup := 8;
            Sb.majorVersion := 1;
            Sb.incompatibleFeatures := 2;
            Sb.readOnlyFeatures := 2; -- LARGE_FILE: 4 KiB triple offsets
            Sb.inodeSize := 128;
            Bgd := (blockBitmapAddr => 3, inodeBitmapAddr => 4,
                    inodeTableAddr => 5, numFreeBlocks => Unsigned_16 (Sb.freeBlocks),
                    numFreeInodes => 0, numDirectories => 0, padding => 0, reserved => 0);
            Disk (3 * Size .. 4 * Size - 1) := [others => 255];
            for B in First_Tree_Block + Tree_Blocks .. Natural (Sb.blockCount) - 1 loop
               declare
                  Bit : constant Natural := B - Reclamation_First_Block;
               begin
                  Disk (3 * Size + Bit / 8) := Disk (3 * Size + Bit / 8) and
                    not Shift_Left (Unsigned_8 (1), Bit mod 8);
               end;
            end loop;
            Item := NULL_INODE;
            Item.typeAndPermissions := 16#8000#;
            Item.numHardLinks := 1;
            Item.sizeLo := Unsigned_32 (End_Of_Tree and 16#FFFF_FFFF#);
            Item.sizeHi_DirACL := Unsigned_32 (Shift_Right (End_Of_Tree, 32));
            Item.numDiskSectors := Unsigned_32 (Tree_Blocks * Size / 512);
            Item.tripleIndirectBlock := 6;
            Root (0) := 7;
            Middle_0 (0) := 8;
            Leaf_00 (0) := 9;
            Leaf_00 (1) := 10;
            Middle_0 (1) := 11;
            Leaf_01 (0) := 12;
            Root (1) := 13;
            Middle_1 (0) := 14;
            Leaf_10 (0) := 15;
            initBlockDevice (Fs, 1, (slot => 1, generation => 1),
                             Grant_Buffer'Address, Grant_Buffer'Length, Admission);
            pragma Assert (Admission = Admitted);
            Calls := 0;
            Writes := 0;
            Durable := Disk;
         end Setup;

         --  Independent count of blocks still reachable from the inode.
         function Reachable return Natural is
            Count : Natural := 0;
            procedure Walk (Number : Unsigned_32; Depth : Natural) is
               Entries : Pointers_Block
                 with Import, Address => Disk (Natural (Number) * Size)'Address;
            begin
               if Number = 0 then return; end if;
               Count := Count + 1;
               if Depth > 0 then
                  for Child of Entries loop
                     Walk (Child, Depth - 1);
                  end loop;
               end if;
            end Walk;
         begin
            Walk (Item.tripleIndirectBlock, 3);
            return Count;
         end Reachable;
      begin
         for Index in Sizes'Range loop
            declare
               New_Size : constant Unsigned_64 := Sizes (Index);
            begin
               Setup;
               resizeFile (Fs, 1, New_Size, Result, Status);
               pragma Assert (Status = Truncate_Complete and fileSize (Result) = New_Size);
               pragma Assert (Result = Item);
               pragma Assert (Reachable = Retained (Index));
               pragma Assert (Item.numDiskSectors =
                 Unsigned_32 (Retained (Index) * Size / 512));
               pragma Assert (Sb.freeBlocks = Sb.blockCount -
                 Unsigned_32 (First_Tree_Block + Retained (Index)));
               declare
                  Baseline : constant Natural := Calls;
               begin
                  for Boundary in 1 .. Baseline loop
                     for Treatment in Failure_Mode loop
                        for Reply_Kind in Failure_Reply loop
                           --  A flush's zero-byte reply is valid (see resizing).
                           if Reply_Kind /= Short_Transfer then
                              Setup;
                              Fail_At := Boundary;
                              CuBit.Messages.Mode := Treatment;
                              Reply_Style := Reply_Kind;
                              resizeFile (Fs, 1, New_Size, Result, Status);
                              pragma Assert (Failed and Calls = Boundary and Result = NULL_INODE);
                              pragma Assert (Status /= Truncate_Complete);
                              if Writes > 0 then
                                 pragma Assert (Fs.writeQuarantined and
                                   Status = Truncate_Recovery_Required);
                                 resizeFile (Fs, 1, 0, Result, Status);
                                 pragma Assert (Calls = Boundary and
                                   Status = Truncate_Recovery_Required);
                              end if;
                              Faults := Faults + 1;
                           end if;
                        end loop;
                     end loop;
                  end loop;
               end;
            end;
         end loop;

         --  A valid but empty middle, and a valid but empty triple root, are
         --  retired by any shrink that reaches them.
         --  Middle 13 moves to root slot 2 with no leaves; 14 and 15 are free.
         Setup;
         Root (1) := 0;
         Root (2) := 13;
         Middle_1 (0) := 0;
         Item.numDiskSectors := Unsigned_32 ((Tree_Blocks - 2) * Size / 512);
         declare
            Extent : constant Unsigned_64 := (First_Triple + 2 * Middle_Span + 1) * Bytes;
         begin
            Item.sizeLo := Unsigned_32 (Extent and 16#FFFF_FFFF#);
            Item.sizeHi_DirACL := Unsigned_32 (Shift_Right (Extent, 32));
         end;
         Sb.freeBlocks := Sb.freeBlocks + 2;
         Bgd.numFreeBlocks := Bgd.numFreeBlocks + 2;
         for B in 14 .. 15 loop
            declare
               Bit : constant Natural := B - Reclamation_First_Block;
            begin
               Disk (3 * Size + Bit / 8) := Disk (3 * Size + Bit / 8) and
                 not Shift_Left (Unsigned_8 (1), Bit mod 8);
            end;
         end loop;
         Durable := Disk;
         Fs.sb.freeBlocks := Sb.freeBlocks;
         resizeFile (Fs, 1, (First_Triple + 2 * Middle_Span) * Bytes, Result, Status);
         pragma Assert (Status = Truncate_Complete and Root (2) = 0);
         pragma Assert (Reachable = 7);
         Setup;
         Root := [others => 0];
         Item.numDiskSectors := Unsigned_32 (Size / 512);
         Item.sizeLo := 1;
         Item.sizeHi_DirACL := 0;
         Sb.freeBlocks := Sb.blockCount - (First_Tree_Block + 1);
         Bgd.numFreeBlocks := Unsigned_16 (Sb.freeBlocks);
         Disk (3 * Size .. 4 * Size - 1) := [others => 255];
         for B in First_Tree_Block + 1 .. Natural (Sb.blockCount) - 1 loop
            declare
               Bit : constant Natural := B - Reclamation_First_Block;
            begin
               Disk (3 * Size + Bit / 8) := Disk (3 * Size + Bit / 8) and
                 not Shift_Left (Unsigned_8 (1), Bit mod 8);
            end;
         end loop;
         Durable := Disk;
         Fs.sb.freeBlocks := Sb.freeBlocks;
         resizeFile (Fs, 1, 0, Result, Status);
         pragma Assert (Status = Truncate_Complete and Item.tripleIndirectBlock = 0);
         pragma Assert (Item.numDiskSectors = 0 and Sb.freeBlocks = Sb.blockCount - 6);

         --  Growth over an absent first middle must still zero a mapped block
         --  past EOF under the next middle: only the absent subtree is skipped.
         Setup;
         Root (0) := 0;
         Item.numDiskSectors := Unsigned_32 (4 * Size / 512);
         Item.sizeLo := Unsigned_32 ((First_Triple * Bytes) and 16#FFFF_FFFF#);
         Item.sizeHi_DirACL := Unsigned_32 (Shift_Right (First_Triple * Bytes, 32));
         Disk (15 * Size .. 16 * Size - 1) := [others => Character'Pos ('Z')];
         Durable := Disk;
         resizeFile (Fs, 1, (First_Triple + Middle_Span + 1) * Bytes, Result, Status);
         pragma Assert (Status = Truncate_Complete);
         pragma Assert (Disk (15 * Size .. 16 * Size - 1) = [0 .. Size - 1 => 0]);

         --  Aliases across leaves, middles and levels, bad counts and
         --  out-of-volume pointers are rejected before any write.
         for Corruption in 1 .. 7 loop
            Setup;
            case Corruption is
               when 1 => Leaf_10 (0) := 9;               -- data in two leaves
               when 2 => Middle_1 (0) := 8;              -- leaf in two middles
               when 3 => Root (1) := Root (0);           -- middle aliased
               when 4 => Leaf_01 (0) := 7;               -- data aliases a middle
               when 5 => Item.numDiskSectors := Unsigned_32'Last;
               when 6 => Leaf_00 (0) := Sb.blockCount;   -- out of volume
               when others => Root (2) := Sb.blockCount; -- middle out of volume
            end case;
            resizeFile (Fs, 1, 0, Result, Status);
            pragma Assert (Status = Truncate_Invalid and Writes = 0);
         end loop;

         --  Allocations beyond the scratch inventory capacity are reported as
         --  unsupported before any pointer block is read.
         Setup;
         Fs.sb.blockCount := Unsigned_32'Last;
         Item.numDiskSectors :=
           Unsigned_32 (Block_Inventory.Maximum_Blocks + 1) * Unsigned_32 (Size / 512);
         Calls := 0;
         resizeFile (Fs, 1, 0, Result, Status);
         pragma Assert (Status = Truncate_Unsupported and Writes = 0);
         pragma Assert (Calls = 2); -- group descriptor and inode reads only

         --  Growth past the last triple block, or above 2 GiB without
         --  LARGE_FILE, is rejected before mutation.
         Setup;
         resizeFile (Fs, 1, Block_Paths.Block_Limit (Sectors) * Bytes + 1, Result, Status);
         pragma Assert (Status = Truncate_Unsupported and Writes = 0);
         Setup;
         Sb.readOnlyFeatures := 0;
         Fs.sb.readOnlyFeatures := 0;
         resizeFile (Fs, 1, 16#8000_0000#, Result, Status);
         pragma Assert (Status = Truncate_Unsupported and Writes = 0);
      end;
   end loop;
   Put_Line ("TRIPLE-RESIZE-CHECK: PASS" & Faults'Image & " injected failures");
end Triple_Resize;
