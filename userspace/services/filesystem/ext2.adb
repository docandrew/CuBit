------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 filesystem operations for userspace ramdisk server.
--  Reads directly from a memory-mapped ramdisk image (no disk I/O).
------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

package body Ext2 is

   --  Read bytes from the ramdisk image at a byte offset
   procedure readBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      dest   : System.Address;
      len    : Storage_Count)
   is
      src : constant System.Address := fs.base + offset;
      srcBuf : String (1 .. Natural (len))
        with Import, Address => src;
      dstBuf : String (1 .. Natural (len))
        with Import, Address => dest;
   begin
      dstBuf := srcBuf;
   end readBytes;

   --  Read a full block from the ramdisk
   procedure readBlock
     (fs       : Filesystem;
      blockNum : Unsigned_32;
      dest     : System.Address)
   is
      offset : constant Storage_Offset :=
        Storage_Offset (blockNum) * Storage_Offset (fs.blkSize);
   begin
      readBytes (fs, offset, dest, Storage_Count (fs.blkSize));
   end readBlock;

   function blockSize (sb : Superblock) return Unsigned_32 is
   begin
      return Shift_Left (Unsigned_32'(1024), Natural (sb.blockShift));
   end blockSize;

   function inodeType (ino : Inode) return Unsigned_8 is
   begin
      return Unsigned_8 (Shift_Right (ino.typeAndPermissions, 12) and 16#F#);
   end inodeType;

   function fileSize (ino : Inode) return Unsigned_64 is
   begin
      return Unsigned_64 (ino.sizeHi_DirACL) * 16#1_0000_0000# +
             Unsigned_64 (ino.sizeLo);
   end fileSize;

   procedure init
     (fs   : out Filesystem;
      base : System.Address;
      ok   : out Boolean)
   is
      sb : Superblock;
   begin
      fs.base := base;

      --  Read superblock from offset 1024
      readBytes ((base => base, sb => sb, blkSize => 0),
                 SUPERBLOCK_OFFSET, sb'Address, Superblock'Size / 8);

      if sb.signature /= EXT2_SIGNATURE then
         ok := False;
         return;
      end if;

      fs.sb      := sb;
      fs.blkSize := blockSize (sb);
      ok         := True;
   end init;

   procedure readInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : out Inode)
   is
      --  Determine which block group this inode is in
      blockGroup : constant Unsigned_32 :=
        (inodeNum - 1) / fs.sb.inodesPerBlockGroup;

      --  Index within that block group's inode table
      inodeIndex : constant Unsigned_32 :=
        (inodeNum - 1) mod fs.sb.inodesPerBlockGroup;

      --  Read the block group descriptor
      bgdtOffset : constant Storage_Offset :=
        Storage_Offset ((fs.sb.firstDataBlock + 1) * fs.blkSize) +
        Storage_Offset (blockGroup) * (BlockGroupDescriptor'Size / 8);

      bgd : BlockGroupDescriptor;

      --  Compute byte offset of the inode within the inode table
      inodeTableByteOffset : Storage_Offset;
      inoSize : Unsigned_32;
   begin
      readBytes (fs, bgdtOffset, bgd'Address, BlockGroupDescriptor'Size / 8);

      --  Use inodeSize from superblock if major version >= 1
      if fs.sb.majorVersion >= 1 then
         inoSize := Unsigned_32 (fs.sb.inodeSize);
      else
         inoSize := 128;
      end if;

      inodeTableByteOffset :=
        Storage_Offset (bgd.inodeTableAddr) * Storage_Offset (fs.blkSize) +
        Storage_Offset (inodeIndex) * Storage_Offset (inoSize);

      readBytes (fs, inodeTableByteOffset, ino'Address, Inode'Size / 8);
   end readInode;

   function lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name    : String) return Unsigned_32
   is
      size : constant Unsigned_64 := fileSize (dirIno);
      bytesRead : Unsigned_64 := 0;
      --  Use a stack buffer for one block
      blockBuf : String (1 .. Natural (fs.blkSize))
        with Alignment => 8;
      blockIdx : Natural := 0;
   begin
      --  Walk through each data block of the directory
      while bytesRead < size and blockIdx < NUM_DIRECT_BLOCKS loop
         declare
            blkNum : constant Unsigned_32 :=
              dirIno.directBlocks (blockIdx);
            offset : Storage_Offset := 0;
         begin
            if blkNum = 0 then
               exit;
            end if;

            readBlock (fs, blkNum, blockBuf'Address);

            --  Parse directory entries within this block
            while offset < Storage_Offset (fs.blkSize) and
                  bytesRead < size
            loop
               declare
                  dent : DirectoryEntry
                    with Import,
                         Address => blockBuf'Address + offset;
               begin
                  if dent.inode /= 0 and
                     Natural (dent.nameLength) = name'Length
                  then
                     declare
                        entryName : String (1 .. Natural (dent.nameLength))
                          with Import,
                               Address => blockBuf'Address + offset +
                                          (DirectoryEntry'Size / 8);
                        match : Boolean := True;
                     begin
                        for i in 1 .. name'Length loop
                           if entryName (i) /= name (i) then
                              match := False;
                              exit;
                           end if;
                        end loop;

                        if match then
                           return dent.inode;
                        end if;
                     end;
                  end if;

                  bytesRead := bytesRead + Unsigned_64 (dent.length);
                  offset := offset + Storage_Offset (dent.length);

                  --  Guard against malformed entry
                  exit when dent.length = 0;
               end;
            end loop;
         end;

         blockIdx := blockIdx + 1;
      end loop;

      return 0;  --  Not found
   end lookupInDir;

   function resolvePath
     (fs   : Filesystem;
      path : String) return Unsigned_32
   is
      currentInode : Unsigned_32 := ROOT_INODE;
      ino : Inode;
      nameStart : Natural := path'First;
      nameEnd   : Natural;
   begin
      --  Skip leading '/'
      while nameStart <= path'Last and then path (nameStart) = '/' loop
         nameStart := nameStart + 1;
      end loop;

      --  Empty path = root
      if nameStart > path'Last then
         return ROOT_INODE;
      end if;

      --  Walk each path component
      while nameStart <= path'Last loop
         --  Find end of current component
         nameEnd := nameStart;
         while nameEnd <= path'Last and then path (nameEnd) /= '/' loop
            nameEnd := nameEnd + 1;
         end loop;

         --  Read current directory inode
         readInode (fs, currentInode, ino);

         --  Look up the component
         currentInode := lookupInDir
           (fs, ino, path (nameStart .. nameEnd - 1));

         if currentInode = 0 then
            return 0;  --  Not found
         end if;

         --  Skip trailing '/'
         nameStart := nameEnd + 1;
         while nameStart <= path'Last and then path (nameStart) = '/' loop
            nameStart := nameStart + 1;
         end loop;
      end loop;

      return currentInode;
   end resolvePath;

   --  Get the block number for a given logical block index in a file.
   --  Handles direct blocks and single indirect blocks.
   function getDataBlock
     (fs       : Filesystem;
      ino      : Inode;
      logBlock : Unsigned_32) return Unsigned_32
   is
      ptrsPerBlock : constant Unsigned_32 := fs.blkSize / 4;
   begin
      --  Direct blocks (0..11)
      if logBlock < NUM_DIRECT_BLOCKS then
         return ino.directBlocks (Natural (logBlock));
      end if;

      --  Single indirect (12 .. 12+ptrsPerBlock-1)
      declare
         indirectIdx : constant Unsigned_32 :=
           logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS);
      begin
         if indirectIdx < ptrsPerBlock then
            if ino.singleIndirectBlock = 0 then
               return 0;
            end if;

            --  Read indirect block and extract pointer
            declare
               indBuf : array (0 .. ptrsPerBlock - 1) of Unsigned_32
                 with Alignment => 8;
            begin
               readBlock (fs, ino.singleIndirectBlock, indBuf'Address);
               return indBuf (indirectIdx);
            end;
         end if;
      end;

      --  Double indirect (for larger files)
      declare
         diIdx : constant Unsigned_32 :=
           logBlock - Unsigned_32 (NUM_DIRECT_BLOCKS) - ptrsPerBlock;
         l1Idx : constant Unsigned_32 := diIdx / ptrsPerBlock;
         l2Idx : constant Unsigned_32 := diIdx mod ptrsPerBlock;
      begin
         if l1Idx < ptrsPerBlock then
            if ino.doubleIndirectBlock = 0 then
               return 0;
            end if;

            declare
               l1Buf : array (0 .. ptrsPerBlock - 1) of Unsigned_32
                 with Alignment => 8;
               l2Buf : array (0 .. ptrsPerBlock - 1) of Unsigned_32
                 with Alignment => 8;
            begin
               readBlock (fs, ino.doubleIndirectBlock, l1Buf'Address);
               if l1Buf (l1Idx) = 0 then
                  return 0;
               end if;
               readBlock (fs, l1Buf (l1Idx), l2Buf'Address);
               return l2Buf (l2Idx);
            end;
         end if;
      end;

      return 0;  --  Beyond supported range
   end getDataBlock;

   function readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64) return Unsigned_64
   is
      size : constant Unsigned_64 := fileSize (ino);
      remaining : Unsigned_64;
      pos       : Unsigned_64 := offset;
      bytesRead : Unsigned_64 := 0;
   begin
      if offset >= size then
         return 0;
      end if;

      remaining := size - offset;
      if remaining > count then
         remaining := count;
      end if;

      while remaining > 0 loop
         declare
            logBlock    : constant Unsigned_32 :=
              Unsigned_32 (pos / Unsigned_64 (fs.blkSize));
            blockOffset : constant Unsigned_32 :=
              Unsigned_32 (pos mod Unsigned_64 (fs.blkSize));
            physBlock   : constant Unsigned_32 :=
              getDataBlock (fs, ino, logBlock);
            canRead     : Unsigned_64 :=
              Unsigned_64 (fs.blkSize - blockOffset);
         begin
            if physBlock = 0 then
               --  Sparse block (hole) — fill with zeros
               if canRead > remaining then
                  canRead := remaining;
               end if;
               declare
                  dst : String (1 .. Natural (canRead))
                    with Import, Address => buf + Storage_Offset (bytesRead);
               begin
                  for i in dst'Range loop
                     dst (i) := Character'Val (0);
                  end loop;
               end;
            else
               if canRead > remaining then
                  canRead := remaining;
               end if;

               --  Read from ramdisk
               readBytes (fs,
                          Storage_Offset (physBlock) *
                            Storage_Offset (fs.blkSize) +
                            Storage_Offset (blockOffset),
                          buf + Storage_Offset (bytesRead),
                          Storage_Count (canRead));
            end if;

            bytesRead := bytesRead + canRead;
            pos       := pos + canRead;
            remaining := remaining - canRead;
         end;
      end loop;

      return bytesRead;
   end readData;

end Ext2;
