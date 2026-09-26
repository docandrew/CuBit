------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Ext2 filesystem types and operations over authorized block-device sessions.
--  Simplified from kernel/src/filesystem/filesystem-ext2.ads.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with CuBit.Block_Devices;
with CuBit.Filesystems;
with CuBit.Memory_Grants;
with Volume_Admission;
with Ext2_Inodes;

package Ext2 is
   SUPERBLOCK_OFFSET : constant := 1024;
   EXT2_SIGNATURE    : constant := 16#EF53#;
   ROOT_INODE        : constant := 2;

   --  Number of direct block pointers per inode
   NUM_DIRECT_BLOCKS : constant := Ext2_Inodes.NUM_DIRECT_BLOCKS;

   --  File type nibble in inode typeAndPermissions (upper 4 bits of Unsigned_16)
   INODE_DIRECTORY    : constant Unsigned_8 := 16#4#;
   INODE_REGULAR_FILE : constant Unsigned_8 := 16#8#;
   INODE_SYMBOLIC_LINK : constant Unsigned_8 := 16#A#;

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
      blockGroupNumber        : Unsigned_16;
      compatibleFeatures      : Unsigned_32;
      incompatibleFeatures    : Unsigned_32;
      readOnlyFeatures        : Unsigned_32;
   end record with Convention => C;
   pragma Compile_Time_Error
     (Superblock'Size /= 104 * 8, "Ext2 superblock prefix layout changed");

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
   subtype DirectBlockArray is Ext2_Inodes.DirectBlockArray;
   subtype Inode is Ext2_Inodes.Inode;
   use type Ext2_Inodes.Inode;

   NULL_INODE : constant Inode :=
     (typeAndPermissions => 0,
      uid => 0, sizeLo => 0,
      accessedTime => 0, creationTime => 0,
      modifiedTime => 0, deletedTime => 0,
      gid => 0, numHardLinks => 0,
      numDiskSectors => 0, flags => 0,
      osSpecific1 => 0,
      directBlocks => [others => 0],
      singleIndirectBlock => 0,
      doubleIndirectBlock => 0,
      tripleIndirectBlock => 0,
      generationNumber => 0,
      fileACL => 0, sizeHi_DirACL => 0,
      fragmentBlockAddr => 0,
      osSpecific2A => 0, osSpecific2B => 0,
      osSpecific2C => 0);

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

   --  File writes have an explicit terminal state.  In particular, a zero
   --  byte result is not sufficient to distinguish an empty write from an
   --  allocation, range, or transport failure.
   type Write_Status is
     (Write_Complete,
      Write_Read_Only,
      Write_Out_Of_Range,
      Write_Device_Error,
      Write_Recovery_Required,
      Write_Already_Exists,
      Write_No_Space,
      Write_Object_Unsupported,
      Write_File_Range_Unsupported);

   type Read_Status is
     (Read_Complete,
      Read_Out_Of_Range,
      Read_Device_Error,
      Read_Object_Unsupported,
      Read_File_Range_Unsupported);

   type Directory_Read_Status is
     (Directory_Page_Complete,
      Directory_End,
      Directory_Malformed,
      Directory_Device_Error,
      Directory_Out_Of_Range,
      Directory_Range_Unsupported);

   type Directory_Lookup_Status is
     (Lookup_Found, Lookup_Not_Found, Lookup_Malformed,
      Lookup_Device_Error, Lookup_Out_Of_Range, Lookup_Range_Unsupported);

   type Rename_Status is
     (Rename_Complete, Rename_Source_Not_Found, Rename_Destination_Exists,
      Rename_Invalid_Name, Rename_Malformed, Rename_Range_Unsupported,
      Rename_Read_Only, Rename_Out_Of_Range, Rename_IO_Error,
      Rename_Recovery_Required);

   type Flush_Status is
     (Flush_Complete, Flush_Unsupported, Flush_IO_Error,
      Flush_Recovery_Required);

   --  Context for an Ext2 filesystem
   type Filesystem is record
      sb           : Superblock;         --  Cached superblock
      blkSize      : Unsigned_32;        --  Block size in bytes
      device       : CuBit.Block_Devices.Device_Session;
      --  Uncertain metadata after failed rollback requires offline recovery.
      writeQuarantined : Boolean := False;
   end record;

   --  Flush completed writes; volatile/unsupported backends fail explicitly.
   procedure Flush (fs : Filesystem; status : out Flush_Status);

   --  Initialize a filesystem over any Block.Device.V1 endpoint.
   procedure initBlockDevice
     (fs         : out Filesystem;
      capSlot    : Unsigned_64;
      grant      : CuBit.Memory_Grants.Grant_Reference;
      grantBuf   : System.Address;
      grantBytes : Unsigned_32;
      result     : out Volume_Admission.Admission_Result);

   --  Read an inode by number
   procedure readInode
     (fs : Filesystem; inodeNum : Unsigned_32;
      ino : out Inode; status : out Read_Status);

   --  Lookup failures and missing names are distinct; inodeNum is zero
   --  unless lookup succeeds.
   procedure lookupInDir
     (fs : Filesystem; dirIno : Inode; name : String;
      inodeNum : out Unsigned_32; status : out Directory_Lookup_Status);

   --  Creation must distinguish a missing name from unreadable metadata.
   procedure resolvePath
     (fs : Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Directory_Lookup_Status);

   --  Decode one bounded page of directory metadata. Cursor is an opaque
   --  byte position returned by the preceding call (zero starts a scan).
   --  Every ext2 record is validated before its variable-length name is
   --  viewed, and cursor advances only across validated records.
   procedure readDirectoryPage
     (fs       : Filesystem;
      dirIno   : Inode;
      cursor       : Unsigned_64;
      entries      : out CuBit.Filesystems.Directory_Entries;
      entryCount   : out Natural;
      nextCursor   : out Unsigned_64;
      status       : out Directory_Read_Status);

   --  Read file data from an inode starting at the given offset.  End of file
   --  is Read_Complete with a zero-byte result; failures are distinct.
   procedure readData
     (fs     : Filesystem;
      ino    : Inode;
      offset : Unsigned_64;
      buf    : System.Address;
      count  : Unsigned_64;
      bytesRead : out Unsigned_64;
      status    : out Read_Status);

   --  Write file data to an inode starting at the given offset.  A failure
   --  may follow a committed prefix, reported in bytesWritten.
   procedure writeData
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      ino      : in out Inode;
      offset   : Unsigned_64;
      buf      : System.Address;
      count    : Unsigned_64;
      bytesWritten : out Unsigned_64;
      status       : out Write_Status);

   --  Allocate a free block from any block group.
   --  Returns a block only after all reservation metadata writes complete.
   procedure allocateBlock
     (fs       : in out Filesystem;
      blockNum : out Unsigned_32;
      status   : out Write_Status);

   --  Allocate a free inode from any block group.
   --  Returns an initialized inode (1-based) only on complete reservation.
   procedure allocateInode
     (fs       : in out Filesystem;
      inodeNum : out Unsigned_32;
      status   : out Write_Status);

   --  Create an empty regular file. Returns no inode on failure; an error
   --  does not imply absence of on-disk side effects.
   procedure createFile
     (fs         : in out Filesystem;
      dirInodeNum : Unsigned_32;
      name       : String;
      inodeNum   : out Unsigned_32;
      status     : out Write_Status);

   type Truncate_Status is
     (Truncate_Complete, Truncate_Read_Only, Truncate_IO_Error,
      Truncate_Unsupported, Truncate_Durability_Unsupported,
      Truncate_Invalid, Truncate_Recovery_Required);

   --  OPEN_TRUNCATE only requires emptying a regular file. Detach its block
   --  tree and persist that detachment before making any block reusable.
   --  A failed publication/reclamation quarantines further volume writes.
   --  This is not an atomic, journaled transaction: interrupted reclamation
   --  may leak blocks and requires offline recovery.
   procedure truncateToEmpty
     (fs       : in out Filesystem;
      inodeNum : Unsigned_32;
      emptyInode : out Inode;
      status   : out Truncate_Status);

   --  Resize within the supported direct/single/double-indirect extent. Growth is
   --  sparse and exposes zeroes; shrink detaches and flushes before reclaim.
   --  On failure the output must not be published to shared inode aliases.
   --  The same quarantine/durability limitations as OPEN_TRUNCATE apply.
   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status);

   --  Non-overwriting rename within one directory. The replacement is prepared
   --  in memory and must fit in the source directory block. Multi-block moves
   --  and replacement of existing destinations need a separate transaction API.
   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status);

   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      status : out Rename_Status);

end Ext2;
