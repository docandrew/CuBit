------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Block_Paths;
with Sector_Accounting;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2_Format; use Ext2_Format;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Files; use Ext2.Files;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;

package body Ext2.Resize is

   use type System.Address;

   --  Whole-tree preflight, then bounded leaf-sized detach/flush/reclaim.
   --  No mutable pointer tree is revisited after its storage has been freed.
   --  Metadata one resize batch dirties: three pointer levels, the inode,
   --  the superblock, a bitmap and a descriptor per group its blocks lie in.
   function resizeCredits (fs : Filesystem) return Natural is
     (5 + 2 * Natural (Unsigned_32'Min
        (Unsigned_32 (Sector_Accounting.Retired_Blocks'Last),
         (if fs.sb.blocksPerBlockGroup = 0 then 1
          else 1 + (fs.sb.blockCount - fs.sb.firstDataBlock) /
                     fs.sb.blocksPerBlockGroup))));

   procedure resizeInner
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
      ino : Inode;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      top, middle, root, leaf : Pointer_Block := [others => 0];
      --  One batch retires at most one leaf's data plus that leaf and its
      --  emptied middle and triple root: P + 3 <= Retired_Blocks'Last.
      retired : Release_List (1 .. Sector_Accounting.Retired_Blocks'Last);
      count : Sector_Accounting.Retired_Blocks := 0;
      keepBlocks : Unsigned_64;
      mutated : Boolean := False;
      failed : Boolean := False;
      ptrCount : Unsigned_32;
      type Leaf_Parent is (Single_Parent, Double_Parent, Triple_Parent);

      procedure Fail (reason : Truncate_Status) is
      begin
         failed := True;
         if mutated or else fs.writeQuarantined then
            fs.writeQuarantined := True;
            invalidateBlockCache;
            status := Truncate_Recovery_Required;
         else
            status := reason;
         end if;
      end Fail;

      procedure Retire (number : Unsigned_32) is
      begin
         if number /= 0 then
            count := count + 1;
            retired (count) := Block_Number (number);
         end if;
      end Retire;

      procedure Load (number : Unsigned_32; contents : out Pointer_Block) is
      begin
         readBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                    contents'Address, Storage_Count (fs.blkSize), readStatus);
         if readStatus /= Read_Complete then Fail (Truncate_IO_Error); end if;
      end Load;

      procedure Publish
        (number : Unsigned_32; contents : Pointer_Block; class : Block_Class) is
      begin
         mutated := True;
         writeBytes (fs, Storage_Offset (number) * Storage_Offset (fs.blkSize),
                     contents'Address, Storage_Count (fs.blkSize), writeStatus, class);
         if writeStatus /= Write_Complete then Fail (Truncate_IO_Error); end if;
      end Publish;

      procedure Commit_Batch is
         fits : Boolean;
         newSectors : Unsigned_32;
      begin
         Sector_Accounting.Plan_Removal
           (ino.numDiskSectors, Sector_Accounting.Block_Sectors (fs.blkSize / 512),
            count, newSectors, fits);
         if not fits then
            Fail (Truncate_Invalid);
            return;
         end if;
         ino.numDiskSectors := newSectors;
         mutated := True;
         if fs.journal.Active then
            --  Journaled: the release is applied by the commit of this
            --  batch's transaction; the size shrinks (the blocks beyond it
            --  are already gone) only in the final batch.
            writeInode (fs, inodeNum, ino, writeStatus);
            if writeStatus = Write_Complete and then count > 0 then
               deferRelease (fs, retired (1 .. count), writeStatus);
            end if;
            if writeStatus /= Write_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
            invalidateBlockCache;
            count := 0;
            --  A batch is one operation: a commit may follow it, never
            --  split it (the file then has holes up to its old size).
            stopHandle (fs);
            startHandle (fs, resizeCredits (fs));
            return;
         end if;
         ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
         ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
         writeInode (fs, inodeNum, ino, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
            return;
         end if;
         invalidateBlockCache;
         --  Freed once the inode and the surviving parent pointers are
         --  durably detached.
         if count > 0 then
            deferRelease (fs, retired (1 .. count), writeStatus);
            if writeStatus /= Write_Complete then
               Fail (Truncate_IO_Error);
               return;
            end if;
         end if;
         count := 0;
      end Commit_Batch;

      function Empty (contents : Pointer_Block) return Boolean is
        (for all I in 0 .. Natural (ptrCount) - 1 => contents (I) = 0);

      --  Remove triple root slot topSlot, whose middle block is now empty,
      --  retiring the root too when that was its last middle.
      procedure Detach_Middle (topSlot : Natural) is
      begin
         Retire (top (topSlot));
         top (topSlot) := 0;
         if Empty (top) then
            Retire (ino.tripleIndirectBlock);
            ino.tripleIndirectBlock := 0;
         else
            Publish (ino.tripleIndirectBlock, top, Root_Pointers);
         end if;
      end Detach_Middle;

      procedure Trim_Leaf
        (number : Unsigned_32; firstLogical : Unsigned_64;
         parent : Leaf_Parent; outerSlot, middleSlot : Natural := 0)
      is
         remains, changed : Boolean := False;
      begin
         --  Validation already read every pointer block. A leaf lying wholly
         --  below the retained extent needs neither I/O nor publication.
         if firstLogical + Unsigned_64 (ptrCount) <= keepBlocks then return; end if;
         Load (number, leaf);
         if failed then return; end if;
         for I in 0 .. Natural (ptrCount) - 1 loop
            if firstLogical + Unsigned_64 (I) >= keepBlocks then
               if leaf (I) /= 0 then
                  Retire (leaf (I));
                  leaf (I) := 0;
                  changed := True;
               end if;
            elsif leaf (I) /= 0 then
               remains := True;
            end if;
         end loop;
         if remains then
            if not changed then return; end if;
            Publish (number, leaf, Leaf_Pointers);
         else
            Retire (number);
            --  Publish only the nearest surviving ancestor; emptied parents
            --  are retired in the same batch after the inode is flushed.
            case parent is
               when Single_Parent =>
                  ino.singleIndirectBlock := 0;
               when Double_Parent =>
                  root (outerSlot) := 0;
                  if Empty (root) then
                     Retire (ino.doubleIndirectBlock);
                     ino.doubleIndirectBlock := 0;
                  else
                     Publish (ino.doubleIndirectBlock, root, Middle_Pointers);
                  end if;
               when Triple_Parent =>
                  middle (middleSlot) := 0;
                  if Empty (middle) then
                     Detach_Middle (outerSlot);
                  else
                     Publish (top (outerSlot), middle, Middle_Pointers);
                  end if;
            end case;
         end if;
         if failed then return; end if;
         Commit_Batch;
      end Trim_Leaf;

      --  Trim one middle block's leaves from the highest logical block down.
      --  A middle wholly below the retained extent is skipped without I/O.
      procedure Trim_Middle (topSlot : Natural) is
         sectors : constant Sector_Accounting.Block_Sectors :=
           Sector_Accounting.Block_Sectors (fs.blkSize / 512);
         firstLogical : constant Unsigned_64 :=
           Block_Paths.First_Triple (sectors) +
             Unsigned_64 (topSlot) * Block_Paths.Middle_Span (sectors);
      begin
         if firstLogical + Block_Paths.Middle_Span (sectors) <= keepBlocks then
            return;
         end if;
         Load (top (topSlot), middle);
         if failed then return; end if;
         for J in reverse 0 .. Natural (ptrCount) - 1 loop
            if middle (J) /= 0 then
               Trim_Leaf (middle (J),
                 firstLogical + Unsigned_64 (J) * Unsigned_64 (ptrCount),
                 Triple_Parent, topSlot, J);
               if failed then return; end if;
               --  The last leaf's removal also retired this middle block.
               exit when top (topSlot) = 0;
            end if;
         end loop;
         --  Also retire a valid but initially empty middle block.
         if top (topSlot) /= 0 and then Empty (middle) then
            Detach_Middle (topSlot);
            if failed then return; end if;
            Commit_Batch;
         end if;
      end Trim_Middle;
   begin
      resizedInode := NULL_INODE;
      status := Truncate_Invalid;
      if fs.writeQuarantined then
         status := Truncate_Recovery_Required;
         return;
      elsif Is_Read_Only (fs.device.description) then
         status := Truncate_Read_Only;
         return;
      elsif not Is_Volatile (fs.device.description) and then
        not Can_Persist (fs.device.description)
      then
         status := Truncate_Durability_Unsupported;
         return;
      end if;
      Ext2.Metadata.readInode (fs, inodeNum, ino, readStatus);
      if readStatus /= Read_Complete then
         status := Truncate_IO_Error;
         return;
      elsif Ext2_Support.Check_File (ino, Unlinked_Allowed => True) /= Ext2_Support.File_Allowed or else
        ino.fileACL /= 0 or else fs.blkSize not in 1024 | 2048 | 4096
      then
         status := Truncate_Unsupported;
         return;
      end if;
      ptrCount := fs.blkSize / 4;
      if newSize > Unsigned_64 (fs.blkSize) *
        Block_Paths.Block_Limit (Sector_Accounting.Block_Sectors (fs.blkSize / 512)) or else
        not Ext2_Support.Size_Admitted (newSize, fs.sb.readOnlyFeatures)
      then
         status := Truncate_Unsupported;
         return;
      end if;
      --  A size change is a content change (POSIX truncate sets mtime).
      if newSize /= fileSize (ino) then
         Stamp (ino, Content_Changed);
      end if;
      validateBlockTree (fs, ino, status);
      if status /= Truncate_Complete then return; end if;
      keepBlocks := newSize / Unsigned_64 (fs.blkSize) +
        (if newSize mod Unsigned_64 (fs.blkSize) = 0 then 0 else 1);
      if newSize > fileSize (ino) then
         zeroExposedRange (fs, ino, fileSize (ino), newSize, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
            return;
         end if;
         Commit_Batch;
      elsif newSize < fileSize (ino) then
         --  Highest logical blocks first: triple, double, single, direct.
         if ino.tripleIndirectBlock /= 0 then
            Load (ino.tripleIndirectBlock, top);
            if failed then return; end if;
            for I in reverse 0 .. Natural (ptrCount) - 1 loop
               if top (I) /= 0 then
                  Trim_Middle (I);
                  if failed then return; end if;
               end if;
               exit when ino.tripleIndirectBlock = 0;
            end loop;
            --  Also retire a valid but initially empty triple root.
            if ino.tripleIndirectBlock /= 0 and then Empty (top) then
               Retire (ino.tripleIndirectBlock);
               ino.tripleIndirectBlock := 0;
               Commit_Batch;
               if failed then return; end if;
            end if;
         end if;
         if ino.doubleIndirectBlock /= 0 then
            Load (ino.doubleIndirectBlock, root);
            if failed then return; end if;
            for I in reverse 0 .. Natural (ptrCount) - 1 loop
               if root (I) /= 0 then
                  Trim_Leaf (root (I),
                    Unsigned_64 (NUM_DIRECT_BLOCKS) + Unsigned_64 (ptrCount) +
                      Unsigned_64 (I) * Unsigned_64 (ptrCount), Double_Parent, I);
                  if failed then return; end if;
               end if;
            end loop;
            --  Also retire a valid but initially empty double root.
            if ino.doubleIndirectBlock /= 0 and then Empty (root) then
               Retire (ino.doubleIndirectBlock);
               ino.doubleIndirectBlock := 0;
               Commit_Batch;
               if failed then return; end if;
            end if;
         end if;
         if ino.singleIndirectBlock /= 0 then
            Trim_Leaf (ino.singleIndirectBlock, NUM_DIRECT_BLOCKS, Single_Parent);
            if failed then return; end if;
         end if;
         for I in ino.directBlocks'Range loop
            if Unsigned_64 (I) >= keepBlocks then
               Retire (ino.directBlocks (I));
               ino.directBlocks (I) := 0;
            end if;
         end loop;
         if count /= 0 or else fileSize (ino) /= newSize then
            Commit_Batch;
         end if;
      end if;
      if not failed and then fs.journal.Active then
         --  The new size, then one commit for the whole resize.
         ino.sizeLo := Unsigned_32 (newSize and 16#FFFF_FFFF#);
         ino.sizeHi_DirACL := Unsigned_32 (Shift_Right (newSize, 32));
         mutated := True;
         writeInode (fs, inodeNum, ino, writeStatus);
         if writeStatus /= Write_Complete then
            Fail (Truncate_IO_Error);
         end if;
      end if;
      if not failed then
         resizedInode := ino;
         status := Truncate_Complete;
      end if;
   end resizeInner;

   procedure resizeInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
   begin
      startHandle (fs, resizeCredits (fs));
      resizeInner (fs, inodeNum, newSize, resizedInode, status);
      stopHandle (fs);
   end resizeInode;

   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status)
   is
   begin
      resizeInode (fs, inodeNum, newSize, resizedInode, status);
   end resizeFile;

   procedure truncateToEmpty
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      emptyInode : out Inode; status : out Truncate_Status)
   is
   begin
      resizeFile (fs, inodeNum, 0, emptyInode, status);
   end truncateToEmpty;

end Ext2.Resize;
