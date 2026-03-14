------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 filesystem types and operations for userspace ramdisk server.
--  Simplified from kernel/src/filesystem/filesystem-ext2.ads.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements;

package Ext2 is
   use System.Storage_Elements;

   SUPERBLOCK_OFFSET : constant := 1024;
   EXT2_SIGNATURE    : constant := 16#EF53#;
   ROOT_INODE        : constant := 2;

   --  Number of direct block pointers per inode
   NUM_DIRECT_BLOCKS : constant := 12;

   --  File type nibble in inode typeAndPermissions (upper 4 bits of Unsigned_16)
   INODE_DIRECTORY    : constant Unsigned_8 := 16#4#;
   INODE_REGULAR_FILE : constant Unsigned_8 := 16#A#;

   --  Directory entry file types
   FILETYPE_REGULAR   : constant Unsigned_8 := 1;
   FILETYPE_DIRECTORY : constant Unsigned_8 := 2;

   --  Superblock (at byte offset 1024 in the filesystem image)
   type Superblock is record
      inodeCount              : Unsigned_32;
      blockCount              : Unsigned_32;
      reservedBlocks          : Unsigned_32;
      freeBlocks              : Unsigned_32;
      freeInodes              : Unsigned_32;
      firstDataBlock          : Unsigned_32;
      blockShift              : Unsigned_32;  --  log2(blockSize) - 10
      fragmentShift           : Unsigned_32;
      blocksPerBlockGroup     : Unsigned_32;
      fragmentsPerBlockGroup  : Unsigned_32;
      inodesPerBlockGroup     : Unsigned_32;
      lastMountTime           : Unsigned_32;
      lastWriteTime           : Unsigned_32;
      mountCount              : Unsigned_16;
      maxMountCount           : Unsigned_16;
      signature               : Unsigned_16;
      state                   : Unsigned_16;
      errorBehaviour          : Unsigned_16;
      minorVersion            : Unsigned_16;
      lastCheck               : Unsigned_32;
      checkInterval           : Unsigned_32;
      creatorOS               : Unsigned_32;
      majorVersion            : Unsigned_32;
      reservedBlocksUID       : Unsigned_16;
      reservedBlocksGID       : Unsigned_16;
      --  Extended superblock (major version >= 1)
      firstNonReservedInode   : Unsigned_32;
      inodeSize               : Unsigned_16;
   end record with Convention => C;

   --  Block Group Descriptor
   type BlockGroupDescriptor is record
      blockBitmapAddr  : Unsigned_32;
      inodeBitmapAddr  : Unsigned_32;
      inodeTableAddr   : Unsigned_32;
      numFreeBlocks    : Unsigned_16;
      numFreeInodes    : Unsigned_16;
      numDirectories   : Unsigned_16;
      padding          : Unsigned_16;
      reserved         : Unsigned_64;
   end record with Convention => C, Size => 256;

   type TypeAndPermissions is record
      permissions : Unsigned_16;
   end record with Convention => C, Size => 16;

   --  Inode (128 bytes on-disk)
   type DirectBlockArray is array (0 .. NUM_DIRECT_BLOCKS - 1) of Unsigned_32
     with Convention => C;

   type Inode is record
      typeAndPermissions   : Unsigned_16;
      uid                  : Unsigned_16;
      sizeLo               : Unsigned_32;
      accessedTime         : Unsigned_32;
      creationTime         : Unsigned_32;
      modifiedTime         : Unsigned_32;
      deletedTime          : Unsigned_32;
      gid                  : Unsigned_16;
      numHardLinks         : Unsigned_16;
      numDiskSectors       : Unsigned_32;
      flags                : Unsigned_32;
      osSpecific1          : Unsigned_32;
      directBlocks         : DirectBlockArray;
      singleIndirectBlock  : Unsigned_32;
      doubleIndirectBlock  : Unsigned_32;
      tripleIndirectBlock  : Unsigned_32;
      generationNumber     : Unsigned_32;
      fileACL              : Unsigned_32;
      sizeHi_DirACL        : Unsigned_32;
      fragmentBlockAddr    : Unsigned_32;
      osSpecific2A         : Unsigned_32;
      osSpecific2B         : Unsigned_32;
      osSpecific2C         : Unsigned_32;
   end record with Convention => C;

   --  Directory Entry (variable-length, read from disk)
   type DirectoryEntry is record
      inode      : Unsigned_32;
      length     : Unsigned_16;
      nameLength : Unsigned_8;
      fileType   : Unsigned_8;
   end record with Convention => C;

   --  Get block size from superblock
   function blockSize (sb : Superblock) return Unsigned_32;

   --  Get inode type (upper 4 bits of typeAndPermissions >> 12)
   function inodeType (ino : Inode) return Unsigned_8;

   --  Get file size (combining sizeLo and sizeHi)
   function fileSize (ino : Inode) return Unsigned_64;

   --  Block backend for an Ext2 filesystem
   --  NVME uses same IPC protocol as ATA (OP_READ_BLOCK), just different cap slot.
   type BlockBackend is (ATA, NVME);

   --  Context for an Ext2 filesystem
   type Filesystem is record
      base         : System.Address;     --  Base address (unused for disk)
      sb           : Superblock;         --  Cached superblock
      blkSize      : Unsigned_32;        --  Block size in bytes
      backend      : BlockBackend := ATA;
      ataCapSlot   : Unsigned_64 := 0;   --  Cap slot for ATA/NVMe IPC
      ataGrantId   : Unsigned_64 := 0;   --  Grant for data transfer
      ataGrantBuf  : System.Address := System.Null_Address;
      grantBufSize : Unsigned_32 := 32 * 1024;  --  Grant buffer size in bytes
   end record;

   --  Initialize filesystem backed by ATA driver IPC
   procedure initATA
     (fs         : out Filesystem;
      capSlot    : Unsigned_64;
      grantId    : Unsigned_64;
      grantBuf   : System.Address;
      ok         : out Boolean);

   --  Read an inode by number
   procedure readInode
     (fs     : Filesystem;
      inodeNum : Unsigned_32;
      ino    : out Inode);

   --  Write an inode back to disk
   procedure writeInode
     (fs       : Filesystem;
      inodeNum : Unsigned_32;
      ino      : Inode);

   --  Look up a filename in a directory inode, return the inode number.
   --  Returns 0 if not found.
   function lookupInDir
     (fs      : Filesystem;
      dirIno  : Inode;
      name    : String) return Unsigned_32;

   --  Resolve a full path (e.g., "/DOOM1.WAD") to an inode number.
   --  Returns 0 if not found.
   function resolvePath
     (fs   : Filesystem;
      path : String) return Unsigned_32;

   --  Read directory entries into a buffer as newline-separated names.
   --  Returns the number of bytes written to dest.
   function readDir
     (fs       : Filesystem;
      dirIno   : Inode;
      dest     : System.Address;
      destSize : Unsigned_64) return Unsigned_64;

   --  Read file data from an inode starting at the given offset.
   --  Returns the number of bytes actually read.
   function readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64) return Unsigned_64;

   --  Write bytes to the filesystem at a raw byte offset.
   --  Uses OP_WRITE_BLOCK IPC to the block device driver.
   --  Handles non-aligned writes via read-modify-write.
   procedure writeBytes
     (fs     : Filesystem;
      offset : Storage_Offset;
      src    : System.Address;
      len    : Storage_Count);

   --  Write file data to an inode starting at the given offset.
   --  Supports file growth: allocates new blocks as needed and updates
   --  inode size. Returns the number of bytes actually written.
   function writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64) return Unsigned_64;

   --  Allocate a free block from block group 0.
   --  Sets blockNum to the allocated block, ok to True on success.
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      ok       : out Boolean);

   --  Free a previously allocated block.
   procedure freeBlock
     (fs       : in out Filesystem;
      blockNum : Unsigned_32);

   --  Allocate a free inode from block group 0.
   --  Sets inodeNum to the allocated inode (1-based), ok to True on success.
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      ok       : out Boolean);

   --  Free a previously allocated inode.
   procedure freeInode
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32);

   --  Create a new file in a directory.
   --  Returns the new file's inode number, or 0 on failure.
   function createFile
     (fs         : in out Filesystem;
      dirInodeNum : Unsigned_32;
      name       : String;
      fileType   : Unsigned_8) return Unsigned_32;

   --  Truncate a file to newSize bytes. Frees blocks beyond the new size.
   procedure truncateFile
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      newSize  : Unsigned_64);

   --  Add a directory entry pointing to an existing inode.
   --  Returns True on success.
   function addDirectoryEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      inodeNum    : Unsigned_32;
      name        : String;
      fileType    : Unsigned_8) return Boolean;

   --  Remove a directory entry by name.
   --  Returns the inode number of the removed entry, or 0 on failure.
   function removeDirectoryEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      name        : String) return Unsigned_32;

   --  Rename a file within the same directory.
   function renameEntry
     (fs          : in out Filesystem;
      dirInodeNum : Unsigned_32;
      oldName     : String;
      newName     : String) return Boolean;

   --  Write the superblock back to disk
   procedure writeSuperblock (fs : Filesystem);

   --  Write a block group descriptor back to disk
   procedure writeBGD
     (fs         : Filesystem;
      blockGroup : Unsigned_32;
      bgd        : BlockGroupDescriptor);

end Ext2;
