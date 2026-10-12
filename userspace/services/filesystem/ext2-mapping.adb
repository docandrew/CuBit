------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Block_Inventory;
with Sector_Accounting;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2.Block_Cache; use Ext2.Block_Cache;

package body Ext2.Mapping is

   use type System.Address;

   procedure resolveBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical, pointerLeaf, pointerMiddle : out Unsigned_32;
      status : out Read_Status)
   is
      path : constant Block_Paths.Block_Path := Block_Paths.Decode
        (Unsigned_64 (logBlock), Sector_Accounting.Block_Sectors (fs.blkSize / 512));

      procedure Load (blockNum : Unsigned_32; cache : in out Pointer_Cache) is
      begin
         if blockNum = 0 or else blockNum < fs.sb.firstDataBlock or else
           blockNum >= fs.sb.blockCount
         then
            status := Read_Out_Of_Range;
            return;
         end if;
         if cache.Block_Number /= blockNum then
            --  Invalidate BEFORE touching the buffer. A multi-transfer read
            --  may overwrite only a prefix before failing; neither the old
            --  key nor the new key may then expose this buffer.
            cache.Block_Number := 0;
            readBlock (fs, blockNum, cache.Data'Address, status);
            if status /= Read_Complete then
               return;
            end if;
            cache.Block_Number := blockNum;
         end if;
         status := Read_Complete;
      end Load;
   begin
      physical := 0;
      pointerLeaf := 0;
      pointerMiddle := 0;
      status := Read_Complete;
      selectCacheIdentity (fs);
      case path.Kind is
         when Block_Paths.Direct =>
            physical := ino.directBlocks (path.Direct_Slot);
         when Block_Paths.Single_Indirect =>
            if ino.singleIndirectBlock = 0 then
               return; -- absent tree: sparse data, not an I/O error
            end if;
            Load (ino.singleIndirectBlock, Single_Cache);
            if status /= Read_Complete then
               return;
            end if;
            physical := Single_Cache.Data (path.Single_Slot);
            pointerLeaf := ino.singleIndirectBlock;
         when Block_Paths.Double_Indirect =>
            if ino.doubleIndirectBlock = 0 then
               return;
            end if;
            Load (ino.doubleIndirectBlock, Double_Root_Cache);
            if status /= Read_Complete then
               return;
            end if;
            declare
               leaf : constant Unsigned_32 := Double_Root_Cache.Data (path.Root_Slot);
            begin
               if leaf = 0 then
                  return;
               end if;
               Load (leaf, Double_Leaf_Cache);
               if status /= Read_Complete then
                  return;
               end if;
               physical := Double_Leaf_Cache.Data (path.Leaf_Slot);
               pointerLeaf := leaf;
            end;
         when Block_Paths.Triple_Indirect =>
            if ino.tripleIndirectBlock = 0 then
               return;
            end if;
            Load (ino.tripleIndirectBlock, Triple_Root_Cache);
            if status /= Read_Complete then
               return;
            end if;
            declare
               middle : constant Unsigned_32 :=
                 Triple_Root_Cache.Data (path.Top_Slot);
            begin
               if middle = 0 then
                  return;
               end if;
               Load (middle, Triple_Middle_Cache);
               if status /= Read_Complete then
                  return;
               end if;
               pointerMiddle := middle;
               declare
                  leaf : constant Unsigned_32 :=
                    Triple_Middle_Cache.Data (path.Middle_Slot);
               begin
                  if leaf = 0 then
                     return;
                  end if;
                  Load (leaf, Triple_Leaf_Cache);
                  if status /= Read_Complete then
                     return;
                  end if;
                  physical := Triple_Leaf_Cache.Data (path.Bottom_Slot);
                  pointerLeaf := leaf;
               end;
            end;
         when Block_Paths.Unsupported =>
            status := Read_File_Range_Unsupported;
            return;
      end case;
      if physical /= 0 and then
        (physical < fs.sb.firstDataBlock or else physical >= fs.sb.blockCount)
      then
         physical := 0;
         status := Read_Out_Of_Range;
      end if;
   end resolveBlock;

   procedure getDataBlock
     (fs : Filesystem; ino : Inode; logBlock : Unsigned_32;
      physical : out Unsigned_32; status : out Read_Status)
   is
      pointerLeaf, pointerMiddle : Unsigned_32;
   begin
      resolveBlock (fs, ino, logBlock, physical, pointerLeaf, pointerMiddle, status);
   end getDataBlock;

   procedure validateBlockTree
     (fs : Filesystem; ino : Inode; status : out Truncate_Status)
   is
      sectors : constant Unsigned_32 := fs.blkSize / 512;
      ptrCount : constant Unsigned_32 := fs.blkSize / 4;
      claimed : constant Unsigned_32 := ino.numDiskSectors / sectors;
      --  Data blocks plus the single, double (root + leaves) and triple
      --  (root + middles + leaves) pointer blocks of a complete tree.
      pointerBlockLimit : constant Unsigned_64 :=
        1 + (1 + Unsigned_64 (ptrCount)) +
        (1 + Unsigned_64 (ptrCount) + Unsigned_64 (ptrCount) * Unsigned_64 (ptrCount));
      geometryLimit : constant Unsigned_64 :=
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (sectors)) +
          pointerBlockLimit;
   begin
      status := Truncate_Invalid;
      if ino.numDiskSectors mod sectors /= 0 or else
        Unsigned_64 (claimed) > geometryLimit or else claimed > fs.sb.blockCount
      then
         return;
      elsif Unsigned_64 (claimed) > Block_Inventory.Maximum_Blocks then
         status := Truncate_Unsupported;
         return;
      end if;
      declare
         blocks : Block_Inventory.Block_Array (1 .. Natural (claimed));
         count : Block_Inventory.Block_Count := 0;
         top, middle, root, leaf : Pointer_Block;
         valid, unique : Boolean := True;
         readStatus : Read_Status;

         procedure Remember (number : Unsigned_32) is
         begin
            if number = 0 then
               return;
            elsif number < fs.sb.firstDataBlock or else number >= fs.sb.blockCount or else
              count = blocks'Length
            then
               valid := False;
               return;
            end if;
            count := count + 1;
            blocks (count) := number;
         end Remember;

         procedure Load (number : Unsigned_32; contents : out Pointer_Block) is
         begin
            Remember (number);
            if not valid then
               return;
            end if;
            readBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                       contents'Address, Storage_Count (fs.blkSize), readStatus);
            if readStatus /= Read_Complete then
               status := Truncate_IO_Error;
               valid := False;
            end if;
         end Load;

         procedure Data_Pointers (contents : Pointer_Block) is
         begin
            for I in 0 .. Natural (ptrCount) - 1 loop
               Remember (contents (I));
               exit when not valid;
            end loop;
         end Data_Pointers;
      begin
         for number of ino.directBlocks loop
            Remember (number);
            exit when not valid;
         end loop;
         if not valid then return; end if;
         if ino.singleIndirectBlock /= 0 then
            Load (ino.singleIndirectBlock, leaf);
            if not valid then return; end if;
            Data_Pointers (leaf);
            if not valid then return; end if;
         end if;
         if ino.doubleIndirectBlock /= 0 then
            Load (ino.doubleIndirectBlock, root);
            if not valid then return; end if;
            for I in 0 .. Natural (ptrCount) - 1 loop
               if root (I) /= 0 then
                  Load (root (I), leaf);
                  if not valid then return; end if;
                  Data_Pointers (leaf);
                  if not valid then return; end if;
               end if;
            end loop;
         end if;
         if ino.tripleIndirectBlock /= 0 then
            Load (ino.tripleIndirectBlock, top);
            if not valid then return; end if;
            for I in 0 .. Natural (ptrCount) - 1 loop
               if top (I) /= 0 then
                  Load (top (I), middle);
                  if not valid then return; end if;
                  for J in 0 .. Natural (ptrCount) - 1 loop
                     if middle (J) /= 0 then
                        Load (middle (J), leaf);
                        if not valid then return; end if;
                        Data_Pointers (leaf);
                        if not valid then return; end if;
                     end if;
                  end loop;
               end if;
            end loop;
         end if;
         if Unsigned_32 (count) /= claimed then
            return;
         end if;
         Block_Inventory.Sort_And_Check (blocks, unique);
         if unique then
            status := Truncate_Complete;
         end if;
      end;
   end validateBlockTree;

end Ext2.Mapping;
