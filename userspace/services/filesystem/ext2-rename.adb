------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  See the spec.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ext2_Support; use type Ext2_Support.File_Admission;
with Ext2.Journal; use Ext2.Journal;
with Ext2.Lookup;
with Dentry_Cache;
with CuBit.File_Access;
with CuBit.Directory_Paths;
with CuBit.Block_Devices; use CuBit.Block_Devices;
with System.Storage_Elements; use System.Storage_Elements;
with Ext2_Format; use Ext2_Format;
with Ext2.Allocation; use Ext2.Allocation;
with Ext2.Block_Cache; use Ext2.Block_Cache;
with Ext2.Directories; use Ext2.Directories;
with Ext2.Mapping; use Ext2.Mapping;
with Ext2.Metadata; use Ext2.Metadata;
with Ext2.Removal; use Ext2.Removal;

package body Ext2.Rename is

   use type System.Address;

   --  A general rename's credits: the source, both parents' and a replaced
   --  target's inode blocks, the new name's, the old name's and a moved
   --  directory's first block, the new parent's growth, a replaced target's
   --  release and the orphan list.
   Move_Credits : constant :=
     4 + 3 + Write_Credits + Remove_Credits + Orphan_Credits;

   --  The directory-record file type for an inode's mode (ext2's
   --  EXT2_FT_*), from the mode's type nibble.
   function fileTypeOf (ino : Inode) return Unsigned_8 is
     (Directory_File_Type (inodeType (ino)));

   function renameLookupFailure
     (result : Directory_Lookup_Status) return Rename_Status is
     (case result is
        when Lookup_Malformed => Rename_Malformed,
        when Lookup_Device_Error => Rename_IO_Error,
        when Lookup_Out_Of_Range => Rename_Out_Of_Range,
        when Lookup_Range_Unsupported => Rename_Range_Unsupported,
        when others => Rename_Source_Not_Found);

   function renameReadFailure (result : Read_Status) return Rename_Status is
     (if result = Read_Out_Of_Range then Rename_Out_Of_Range else Rename_IO_Error);

   --  Whether a direct-only directory has no entries but "." and "..".
   procedure directoryEmpty
     (fs : Filesystem; dir : Inode; empty : out Boolean;
      status : out Rename_Status)
   is
      contents : Directory_Blocks.Block_Data := [others => 0];
      children : Directory_Blocks.Byte_Count;
      counted : Directory_Blocks.Prepare_Result;
      number : Unsigned_32;
      valid : Boolean;
      readStatus : Read_Status;
   begin
      empty := False;
      status := Rename_Complete;
      for index in 0 .. Natural (dir.sizeLo / fs.blkSize) - 1 loop
         directoryBlock (fs, dir, index, number, valid, readStatus);
         if valid then
            readBlock (fs, number, contents'Address, readStatus);
         end if;
         if not valid or else readStatus /= Read_Complete then
            status := (if readStatus = Read_Complete then Rename_Malformed
                       else renameReadFailure (readStatus));
            return;
         end if;
         Directory_Blocks.Count_Children
           (contents, Directory_Blocks.Block_Length (fs.blkSize),
            fs.sb.inodeCount, children, counted);
         if counted /= Directory_Blocks.Prepared then
            status := Rename_Malformed;
            return;
         elsif children /= 0 then
            return;
         end if;
      end loop;
      empty := True;
   end directoryEmpty;

   --  Whether directory candidate is ancestor or candidate itself lies
   --  below (or is) directory moved, following ".." up to the root.
   procedure insideOf
     (fs : Filesystem; candidate, moved : Unsigned_32; inside : out Boolean;
      status : out Rename_Status)
   is
      Maximum_Depth : constant := 4096;
      current : Unsigned_32 := candidate;
      ino : Inode;
      readStatus : Read_Status;
      lookup : Directory_Lookup_Status;
      parent : Unsigned_32;
   begin
      inside := True;
      status := Rename_Complete;
      for depth in 1 .. Maximum_Depth loop
         if current = moved then
            return;
         elsif current = ROOT_INODE then
            inside := False;
            return;
         end if;
         Ext2.Metadata.readInode (fs, current, ino, readStatus);
         if readStatus /= Read_Complete then
            status := renameReadFailure (readStatus);
            return;
         elsif inodeType (ino) /= INODE_DIRECTORY then
            status := Rename_Malformed;
            return;
         end if;
         Ext2.Lookup.lookupInDir (fs, ino, "..", parent, lookup);
         if lookup /= Lookup_Found or else parent = 0 or else parent = current then
            status := (if lookup = Lookup_Found then Rename_Malformed
                       else renameLookupFailure (lookup));
            return;
         end if;
         current := parent;
      end loop;
      status := Rename_Malformed;   --  deeper than any real tree: a loop
   end insideOf;

   --  Change one record of a directory in place: Name's record retargeted
   --  to (number, kind) (Directory_Blocks.Retarget), or with retargetParent
   --  the first block's ".." to number. Only the changed bytes are written.
   procedure retargetRecord
     (fs : in out Filesystem; dir : Inode; name : String;
      retargetParent : Boolean; number : Unsigned_32; kind : Unsigned_8;
      old : out Unsigned_32; status : out Rename_Status)
   is
      contents : Directory_Blocks.Block_Data := [others => 0];
      blockNumber : Unsigned_32;
      valid : Boolean;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      prepared : Directory_Blocks.Prepare_Result;
      first, last : Positive;
      blocks : constant Natural :=
        (if retargetParent then 1 else Natural (dir.sizeLo / fs.blkSize));
   begin
      old := 0;
      for index in 0 .. blocks - 1 loop
         directoryBlock (fs, dir, index, blockNumber, valid, readStatus);
         if valid then
            readBlock (fs, blockNumber, contents'Address, readStatus);
         end if;
         if not valid or else readStatus /= Read_Complete then
            status := (if readStatus = Read_Complete then Rename_Malformed
                       else renameReadFailure (readStatus));
            return;
         end if;
         if retargetParent then
            Directory_Blocks.Retarget_Parent
              (contents, Directory_Blocks.Block_Length (fs.blkSize),
               fs.sb.inodeCount, number, old, first, prepared);
            last := first + 3;
         else
            Directory_Blocks.Retarget
              (contents, Directory_Blocks.Block_Length (fs.blkSize),
               fs.sb.inodeCount, name, number, kind, old, first, last, prepared);
         end if;
         case prepared is
            when Directory_Blocks.Prepared =>
               writeBytes
                 (fs, Storage_Offset (blockNumber) * Storage_Offset (fs.blkSize) +
                    Storage_Offset (first - 1),
                  contents (first)'Address, Storage_Count (last - first + 1),
                  writeStatus, Directory_Data);
               status := (if writeStatus = Write_Complete then Rename_Complete
                          else Rename_IO_Error);
               return;
            when Directory_Blocks.Source_Not_Found => null;
            when others =>
               status := Rename_Malformed;
               return;
         end case;
      end loop;
      status := Rename_Source_Not_Found;
   end retargetRecord;

   --  The general rename (renamePath), under one journal handle. Every
   --  refusal comes before the first write; a failure after it
   --  write-quarantines the volume.
   procedure renameMove
     (fs : in out Filesystem; oldParentNum : Unsigned_32; oldName : String;
      newParentNum : Unsigned_32; newName : String; keepReplaced : Boolean;
      replacedNumber : out Unsigned_32; replaced : out Inode;
      stopped : out Boolean; status : out Rename_Status)
   is
      sameParent : constant Boolean := oldParentNum = newParentNum;
      oldParent, newParent, source, target : Inode := NULL_INODE;
      sourceNum, targetNum, old : Unsigned_32 := 0;
      readStatus : Read_Status;
      writeStatus : Write_Status;
      lookup : Directory_Lookup_Status;
      sourceDirectory, targetDirectory, empty, inside : Boolean := False;
      treeStatus : Truncate_Status;
      removed : Remove_Status;

      procedure Fail (result : Rename_Status) is
      begin
         fs.writeQuarantined := True;
         status := (if result = Rename_IO_Error then Rename_Recovery_Required
                    else result);
      end Fail;

      --  The parent inode a name lives in (one inode when both are one).
      procedure Write_Parents is
      begin
         Stamp (oldParent, Content_Changed);
         writeInode (fs, oldParentNum, oldParent, writeStatus);
         if writeStatus = Write_Complete and then not sameParent then
            Stamp (newParent, Content_Changed);
            writeInode (fs, newParentNum, newParent, writeStatus);
         end if;
      end Write_Parents;
   begin
      replacedNumber := 0;
      replaced := NULL_INODE;
      stopped := False;
      status := Rename_Invalid_Name;
      if not CuBit.Directory_Paths.Valid_Child_Name (oldName) or else
        not CuBit.Directory_Paths.Valid_Child_Name (newName)
      then
         return;
      elsif fs.writeQuarantined or else Is_Read_Only (fs.device.description) then
         status := Rename_Read_Only;
         return;
      end if;

      --  Preflight: read and check everything.
      Ext2.Metadata.readInode (fs, oldParentNum, oldParent, readStatus);
      if readStatus = Read_Complete then
         Ext2.Metadata.readInode (fs, newParentNum, newParent, readStatus);
      end if;
      if readStatus /= Read_Complete then
         status := renameReadFailure (readStatus);
         return;
      elsif not writableDirectory (fs, oldParent) or else
        not writableDirectory (fs, newParent)
      then
         status := Rename_Range_Unsupported;
         return;
      end if;
      Ext2.Lookup.lookupInDir (fs, oldParent, oldName, sourceNum, lookup);
      if lookup /= Lookup_Found then
         status := renameLookupFailure (lookup);
         return;
      end if;
      Ext2.Lookup.lookupInDir (fs, newParent, newName, targetNum, lookup);
      if lookup = Lookup_Not_Found then
         targetNum := 0;
      elsif lookup /= Lookup_Found then
         status := renameLookupFailure (lookup);
         return;
      end if;
      if targetNum = sourceNum then
         --  The same file under both names (or one name): nothing to do.
         status := Rename_Complete;
         return;
      end if;
      Ext2.Metadata.readInode (fs, sourceNum, source, readStatus);
      if readStatus /= Read_Complete then
         status := renameReadFailure (readStatus);
         return;
      end if;
      sourceDirectory := inodeType (source) = INODE_DIRECTORY;
      if source.numHardLinks = 0 or else source.numHardLinks >= Maximum_Links then
         status := (if source.numHardLinks = 0 then Rename_Malformed
                    else Rename_Range_Unsupported);
         return;
      end if;
      if targetNum /= 0 then
         Ext2.Metadata.readInode (fs, targetNum, target, readStatus);
         if readStatus /= Read_Complete then
            status := renameReadFailure (readStatus);
            return;
         end if;
         targetDirectory := inodeType (target) = INODE_DIRECTORY;
         if sourceDirectory and then not targetDirectory then
            status := Rename_Not_Directory;
            return;
         elsif targetDirectory and then not sourceDirectory then
            status := Rename_Is_Directory;
            return;
         elsif targetDirectory then
            if not directOnlyDirectory (fs, target) or else target.fileACL /= 0 or else
              target.deletedTime /= 0 or else target.fragmentBlockAddr /= 0
            then
               status := Rename_Range_Unsupported;
               return;
            end if;
            directoryEmpty (fs, target, empty, status);
            if status /= Rename_Complete then
               return;
            elsif not empty then
               status := Rename_Not_Empty;
               return;
            elsif target.numHardLinks /= 2 or else newParent.numHardLinks <= 2 then
               status := Rename_Malformed;
               return;
            end if;
         else
            --  As unlink: an ordinary single-link file whose block tree
            --  checks out, so its release cannot then be refused.
            if Ext2_Support.Check_File (target) /= Ext2_Support.File_Allowed or else
              target.fileACL /= 0
            then
               status := Rename_Range_Unsupported;
               return;
            end if;
            validateBlockTree (fs, target, treeStatus);
            if treeStatus /= Truncate_Complete then
               status := (if treeStatus = Truncate_IO_Error then Rename_IO_Error
                          else Rename_Range_Unsupported);
               return;
            end if;
         end if;
      end if;
      if sourceDirectory and then not sameParent then
         insideOf (fs, newParentNum, sourceNum, inside, status);
         if status /= Rename_Complete then
            return;
         elsif inside then
            status := Rename_Invalid_Move;
            return;
         elsif not directOnlyDirectory (fs, source) and then
           not writableDirectory (fs, source)
         then
            status := Rename_Range_Unsupported;
            return;
         elsif targetNum = 0 and then newParent.numHardLinks >= Maximum_Links then
            status := Rename_Range_Unsupported;
            return;
         end if;
      end if;

      forgetName (fs, oldParentNum, oldName);
      forgetName (fs, newParentNum, newName);
      uncertainDirectory (fs, oldParentNum);
      uncertainDirectory (fs, newParentNum);
      if sourceDirectory then
         Dentry_Cache.Forget_Directory (Names, fs.device.volumeKey, sourceNum);
      end if;

      --  1. One more link: from here the file always has a name.
      source.numHardLinks := source.numHardLinks + 1;
      Stamp (source, Inode_Changed);
      writeInode (fs, sourceNum, source, writeStatus);
      if writeStatus /= Write_Complete then
         Fail (Rename_IO_Error);
         return;
      end if;

      --  2. The new name: the replaced target's record now names the source,
      --  or a new record.
      unindexDirectory (fs, newParentNum, newParent, writeStatus);
      if writeStatus /= Write_Complete then
         Fail (Rename_IO_Error);
         return;
      end if;
      if sameParent then
         oldParent := newParent;
      end if;
      if targetNum /= 0 then
         retargetRecord (fs, newParent, newName, False, sourceNum,
                         fileTypeOf (source), old, status);
         if status /= Rename_Complete or else old /= targetNum then
            Fail ((if status = Rename_Complete then Rename_Malformed else status));
            return;
         end if;
      else
         declare
            buffer : String (1 .. Natural (fs.blkSize)) with Alignment => 8;
            targetBlock : Unsigned_32;
            entryOffset : Storage_Offset;
            entrySpan : Unsigned_16;
         begin
            findRoom (fs, newParentNum, newParent,
                      ((8 + Unsigned_32 (newName'Length) + 3) / 4) * 4,
                      targetBlock, buffer, entryOffset, entrySpan, writeStatus);
            if writeStatus /= Write_Complete then
               --  Only the extra link is written: undo it.
               source.numHardLinks := source.numHardLinks - 1;
               declare
                  undone : Write_Status;
               begin
                  writeInode (fs, sourceNum, source, undone);
                  if undone /= Write_Complete or else
                    writeStatus = Write_Recovery_Required
                  then
                     Fail (Rename_IO_Error);
                  else
                     status := (case writeStatus is
                        when Write_No_Space => Rename_Range_Unsupported,
                        when Write_File_Range_Unsupported => Rename_Range_Unsupported,
                        when Write_Read_Only => Rename_Read_Only,
                        when others => Rename_IO_Error);
                  end if;
               end;
               return;
            end if;
            if sameParent then
               oldParent := newParent;
            end if;
            declare
               dent : DirectoryEntry
                 with Import, Address => buffer'Address + entryOffset;
               entryName : String (1 .. newName'Length)
                 with Import, Address => buffer'Address + entryOffset + Record_Header_Bytes;
            begin
               dent := (inode => sourceNum, length => entrySpan,
                        nameLength => Unsigned_8 (newName'Length),
                        fileType => fileTypeOf (source));
               entryName := newName;
            end;
            writeBytes
              (fs, Storage_Offset (targetBlock) * Storage_Offset (fs.blkSize),
               buffer'Address, Storage_Count (fs.blkSize), writeStatus,
               Directory_Data);
            if writeStatus /= Write_Complete then
               Fail (Rename_IO_Error);
               return;
            end if;
         end;
      end if;

      --  3. The old name goes.
      unindexDirectory (fs, oldParentNum, oldParent, writeStatus);
      if writeStatus /= Write_Complete then
         Fail (Rename_IO_Error);
         return;
      end if;
      removeEntry (fs, oldParent, oldName, sourceNum, removed);
      if removed /= Remove_Complete then
         Fail ((if removed = Remove_Malformed then Rename_Malformed else Rename_IO_Error));
         return;
      end if;
      if sameParent then
         newParent := oldParent;
      end if;

      --  4. The link count back; a moved directory's ".." and its parents'
      --  link counts (".." counts as a link of the parent).
      source.numHardLinks := source.numHardLinks - 1;
      writeInode (fs, sourceNum, source, writeStatus);
      if writeStatus /= Write_Complete then
         Fail (Rename_IO_Error);
         return;
      end if;
      if sourceDirectory and then not sameParent then
         retargetRecord (fs, source, "", True, newParentNum, 0, old, status);
         if status /= Rename_Complete or else old /= oldParentNum then
            Fail ((if status = Rename_Complete then Rename_Malformed else status));
            return;
         end if;
         oldParent.numHardLinks := oldParent.numHardLinks - 1;
         newParent.numHardLinks := newParent.numHardLinks + 1;
      end if;
      if targetDirectory then
         --  The replaced directory's ".." link to the new parent goes.
         newParent.numHardLinks := newParent.numHardLinks - 1;
      end if;
      if sameParent then
         oldParent.numHardLinks := newParent.numHardLinks;
         newParent := oldParent;
      end if;
      Write_Parents;
      if writeStatus /= Write_Complete then
         Fail (Rename_IO_Error);
         return;
      end if;
      status := Rename_Complete;

      --  5. A replaced target loses its last link.
      if targetNum /= 0 and then targetDirectory then
         declare
            blocks : constant Natural := Natural (target.sizeLo / fs.blkSize);
            retired : Release_List (1 .. blocks);
         begin
            for index in retired'Range loop
               retired (index) := Block_Number (target.directBlocks (index - 1));
            end loop;
            Dentry_Cache.Forget_Directory (Names, fs.device.volumeKey, targetNum);
            target.numHardLinks := 0;
            target.sizeLo := 0;
            target.directBlocks := [others => 0];
            target.numDiskSectors := 0;
            target.deletedTime := deletionTime (fs);
            writeInode (fs, targetNum, target, writeStatus, Exact_Inode);
            if writeStatus = Write_Complete then
               deferRelease (fs, retired, writeStatus);
            end if;
            if writeStatus = Write_Complete then
               freeInode (fs, targetNum, True, writeStatus);
            end if;
            if writeStatus /= Write_Complete then
               Fail (Rename_IO_Error);
            end if;
         end;
      elsif targetNum /= 0 then
         --  As unlink, under this handle (which it stops).
         releaseUnlinked (fs, targetNum, target, keepReplaced, replacedNumber,
                          replaced, removed);
         stopped := True;
         if removed /= Remove_Complete then
            status := (if removed = Remove_Recovery_Required then Rename_Recovery_Required
                       else Rename_IO_Error);
         end if;
         return;
      end if;
   end renameMove;

   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      keepReplaced : Boolean; replacedNumber : out Unsigned_32;
      replaced : out Inode; status : out Rename_Status)
   is
      oldParent, newParent : Unsigned_32;
      oldFirst, newFirst : Positive;
      lookup : Directory_Lookup_Status;
   begin
      replacedNumber := 0;
      replaced := NULL_INODE;
      status := Rename_Invalid_Name;
      if not CuBit.File_Access.Valid_Path (oldPath) or else
        not CuBit.File_Access.Valid_Path (newPath) or else
        oldPath'Length = 0 or else newPath'Length = 0 or else
        oldPath (oldPath'Last) = '/' or else newPath (newPath'Last) = '/'
      then
         return;
      end if;
      resolveParent (fs, oldPath, oldParent, oldFirst, lookup);
      if lookup = Lookup_Found then
         resolveParent (fs, newPath, newParent, newFirst, lookup);
      end if;
      if lookup /= Lookup_Found then
         status := renameLookupFailure (lookup);
         return;
      end if;
      declare
         oldName : String renames oldPath (oldFirst .. oldPath'Last);
         newName : String renames newPath (newFirst .. newPath'Last);
      begin
         --  The common case first: within one directory, to an absent name
         --  that fits the source's block, one block rewritten.
         if oldParent = newParent then
            Ext2.Directories.renameEntry (fs, oldParent, oldName, newName, status);
            if status not in Rename_No_Room | Rename_Destination_Exists then
               return;
            end if;
         end if;
         declare
            stopped : Boolean;
         begin
            startHandle (fs, Move_Credits);
            renameMove (fs, oldParent, oldName, newParent, newName, keepReplaced,
                        replacedNumber, replaced, stopped, status);
            if not stopped then
               stopHandle (fs);
            end if;
         end;
      end;
   end renamePath;

end Ext2.Rename;
