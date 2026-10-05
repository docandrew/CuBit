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
with Jbd2_Format;

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
      Rename_Recovery_Required,
      Rename_Not_Directory,   --  a directory onto a non-directory
      Rename_Is_Directory,    --  a non-directory onto a directory
      Rename_Not_Empty,       --  onto a directory that has entries
      Rename_Invalid_Move,    --  a directory into its own subtree
      Rename_No_Room);        --  internal: the in-place path needs the general one

   type Flush_Status is
     (Flush_Complete, Flush_Unsupported, Flush_IO_Error,
      Flush_Recovery_Required);

   --  The internal JBD2 journal of an ext3 volume this service writes
   --  transactions to (data=ordered). Inactive for ext2 volumes and for
   --  journals it cannot write, which keep the write-through cache.
   type Journal_State is record
      Active : Boolean := False;
      Journal_Inode : Inode := NULL_INODE;
      Blocks : Unsigned_32 := 0;        -- journal length in blocks
      First : Unsigned_32 := 0;         -- first log block
      Max_Length : Unsigned_32 := 0;    -- log area end (exclusive)
      Sequence : Unsigned_32 := 0;      -- the next transaction's id
      Compat, Incompat : Unsigned_32 := 0;
      Identity : Jbd2_Format.UUID := [others => 0];
      Super_Home : Unsigned_32 := 0;    -- physical block of its superblock
      Dirty_Metadata : Natural := 0;    -- cached blocks awaiting commit
      --  Nesting of open operations (handles). No commit starts while one
      --  is open, so an operation is never split across transactions.
      Handle_Depth : Natural := 0;
      --  The log ring, as jbd2's: committed transactions stay in the log
      --  from Tail (the durable superblock's start, with Tail_Sequence)
      --  to Head (where the next one goes), Live_Blocks long, until the
      --  tail moves past them. Their home writes are issued right after
      --  each commit; the tail moves only when space is needed (or the
      --  transaction freed blocks), after a barrier makes those durable.
      Head, Tail : Unsigned_32 := 0;
      Tail_Sequence : Unsigned_32 := 0;
      Live_Blocks : Unsigned_32 := 0;
   end record;

   --  Journaled volumes: blocks a detach released, freed in the bitmaps
   --  only by the commit of the transaction holding the detach. Until then
   --  no allocation can reuse them, so a detach needs no commit of its own
   --  and new data never overwrites blocks an uncommitted detach still
   --  references (jbd2's rule for freed blocks).
   Maximum_Pending_Releases : constant := 4096;
   subtype Pending_Count is Natural range 0 .. Maximum_Pending_Releases;
   type Pending_Blocks is array (1 .. Maximum_Pending_Releases) of Unsigned_32;

   --  Context for an Ext2 filesystem
   type Filesystem is record
      sb           : Superblock;         --  Cached superblock
      blkSize      : Unsigned_32;        --  Block size in bytes
      device       : CuBit.Block_Devices.Device_Session;
      --  Uncertain metadata after failed rollback requires offline recovery.
      writeQuarantined : Boolean := False;
      journal      : Journal_State;
      pending      : Pending_Blocks := [others => 0];
      pendingCount : Pending_Count := 0;
      --  Where the last allocation ended: the next one without a goal of
      --  its own starts there, not at the volume's first (full) groups.
      allocationHint : Unsigned_32 := 0;
      --  Where the last inode allocation was (group, bitmap byte): the next
      --  search starts there and wraps, rather than rescanning the used
      --  start of the bitmaps each time.
      inodeHintGroup : Unsigned_32 := 0;
      inodeHintByte  : Unsigned_32 := 0;
   end record;

   --  Make every completed write durable. A journaled volume first commits
   --  its running transaction (file data home, then the journal, then the
   --  metadata home); then the device barrier. Volatile/unsupported backends
   --  fail explicitly. A failure quarantines the volume and is reported by
   --  this and every later flush.
   procedure Flush (fs : in out Filesystem; status : out Flush_Status);

   --  Block cache size, set at startup (before the first volume is
   --  admitted) in 4 MiB steps: each step is one way of each of the
   --  1024 sets, 4 KiB per block. Block memory is allocated on first use.
   Megabytes_Per_Way : constant := 4;
   subtype Cache_Megabytes is Positive range Megabytes_Per_Way .. 32
     with Dynamic_Predicate => Cache_Megabytes mod Megabytes_Per_Way = 0;
   --  Fits a 128 MiB machine beside the desktop.
   Default_Cache_Megabytes : constant Cache_Megabytes := 4;

   --  Size the cache. Ignored once block memory exists.
   procedure configureCache (megabytes : Cache_Megabytes);
   function cacheMegabytes return Cache_Megabytes;

   --  Device requests (block device IPC calls) issued since startup, and
   --  the flushes among them: callers profile an operation by the change.
   procedure deviceRequests (requests, flushes : out Unsigned_64);

   --  Commits that had to split an open operation (its credits were
   --  understated). Diagnostic for tests; expected to stay zero.
   function splitCommits return Natural;

   --  Blocks operations placed beside full cache sets. Diagnostic.
   function pressureBlocks return Natural;

   --  Whether the volume has cached writes not yet on the device, or
   --  released blocks not yet freed (a commit does both).
   function dirtyBlocks (fs : Filesystem) return Boolean;

   --  Clean end of the volume's session: flush, then mark an active journal
   --  empty and clear needs_recovery, as a Linux unmount does.
   procedure Detach (fs : in out Filesystem; status : out Flush_Status);

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
      status   : out Write_Status;
      directory : Boolean := False);

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

   --  Resize within the direct/single/double/triple-indirect extent. Growth is
   --  sparse and exposes zeroes; shrink detaches and flushes before reclaim.
   --  Files whose allocation exceeds the validation inventory capacity
   --  (Block_Inventory.Maximum_Blocks) are Truncate_Unsupported.
   --  On failure the output must not be published to shared inode aliases.
   --  The same quarantine/durability limitations as OPEN_TRUNCATE apply.
   procedure resizeFile
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      newSize : Unsigned_64; resizedInode : out Inode;
      status : out Truncate_Status);

   --  The in-place rename within one directory: the replacement is prepared
   --  in memory and must fit in the source directory block (Rename_No_Room
   --  otherwise), and an existing destination is not replaced. renamePath
   --  uses it first and falls back to its general path.
   procedure renameEntry
     (fs : in out Filesystem; dirInodeNum : Unsigned_32;
      oldName, newName : String; status : out Rename_Status);

   --  POSIX rename: across directories and blocks, replacing an existing
   --  newPath (a file by a file, an empty directory by a directory). A
   --  directory moved elsewhere has its ".." and both parents' link counts
   --  updated; moving one into its own subtree is refused. Without a
   --  journal the order never leaves the inode without a name: its link
   --  count is raised, the new name written, the old one removed, the
   --  count lowered (a crash at worst over-counts, as e2fsck repairs).
   --  A replaced file loses its last link. With keepReplaced (handles still
   --  hold it) it keeps its blocks for reclaimInode at last close, as
   --  unlinkPath's keepOrphan; replacedNumber and replaced (links 0) then
   --  name it. replacedNumber is 0 when nothing was replaced.
   procedure renamePath
     (fs : in out Filesystem; oldPath, newPath : String;
      keepReplaced : Boolean; replacedNumber : out Unsigned_32;
      replaced : out Inode; status : out Rename_Status);

   --  Name removal (unlink, rmdir) and directory creation (mkdir). Until the
   --  journal lands these are write-through, ordered so that a crash leaves
   --  only leaks or link over-counts (e2fsck repairs them), never a name for
   --  a freed inode nor a pointer to a freed block: the name goes first, the
   --  link count next, then the blocks (detached durably before release),
   --  then the inode; mkdir writes the new block and inode before the name.
   --  Parent directories may have any number of blocks up to double
   --  indirect; an htree index is cleared before a change. rmdir releases
   --  only directories with direct blocks.
   type Remove_Status is
     (Remove_Complete, Remove_Not_Found, Remove_Invalid_Name,
      Remove_Wrong_Type,  --  unlink of a directory; rmdir of a non-directory
      Remove_Not_Empty, Remove_Malformed, Remove_Unsupported,
      Remove_Read_Only, Remove_Out_Of_Range, Remove_IO_Error,
      Remove_Durability_Unsupported, Remove_Recovery_Required);

   --  ext2's link limit (EXT2_LINK_MAX): mkdir refuses a parent at it.
   Maximum_Links : constant := 32_000;

   --  Remove a regular file's name and its (single) link. With keepOrphan
   --  (handles still hold it) the unlinked inode keeps its blocks for
   --  reclaimInode at last close (POSIX); otherwise it is freed now.
   --  inodeNum and unlinked (links 0) are set once the name is gone, even
   --  if the later reclaim fails.
   procedure unlinkPath
     (fs : in out Filesystem; path : String; keepOrphan : Boolean;
      inodeNum : out Unsigned_32; unlinked : out Inode;
      status : out Remove_Status);

   --  Free an unlinked (zero-link) regular file: blocks, then inode.
   procedure reclaimInode
     (fs : in out Filesystem; inodeNum : Unsigned_32;
      status : out Remove_Status);

   --  Create an empty directory ("." and "..") and link it into dirInodeNum,
   --  whose link count grows by one.
   procedure makeDirectory
     (fs : in out Filesystem; dirInodeNum : Unsigned_32; name : String;
      inodeNum : out Unsigned_32; status : out Write_Status);

   --  lookup reports the parent's resolution; status is meaningful only
   --  when it is Lookup_Found.
   procedure makeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      lookup : out Directory_Lookup_Status; status : out Write_Status);

   --  Remove an empty plain directory; the parent loses its ".." link.
   procedure removeDirectoryPath
     (fs : in out Filesystem; path : String; inodeNum : out Unsigned_32;
      status : out Remove_Status);

end Ext2;
