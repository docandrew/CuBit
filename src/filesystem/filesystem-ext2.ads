-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Ext2 Filesystem
--
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with time;
with Filesystem.vfs;

package Filesystem.Ext2 with
    SPARK_Mode => On
is

package vfs renames Filesystem.vfs;

subtype BlockAddr is Unsigned_32;
subtype InodeAddr is Unsigned_32;   -- 0 = not used

SUPERBLOCK_LBA              : constant vfs.LBA48 := 2;
ROOT_INODE_NUM              : constant InodeAddr := 2;
EXT2_SUPER_MAGIC            : constant Unsigned_16 := 16#EF53#;

-- Defined filesystemState values
-- On mount, the state is set to ERROR. On unmount, it's set to VALID.
-- If on mount, the state is already ERROR, then it was not cleanly
-- unmounted and needs to be checked.
subtype FSState is Unsigned_16;
VALID                       : constant FSState := 1;
ERROR                       : constant FSState := 2;

-- What to do if FS encounters an error
subtype ErrorAction is Unsigned_16;
ERRORS_CONTINUE             : constant ErrorAction := 1;
ERRORS_READONLY             : constant ErrorAction := 2;
ERRORS_PANIC                : constant ErrorAction := 3;

subtype CreatedByOS is Unsigned_32;
OS_LINUX                    : constant CreatedByOS := 0;
OS_HURD                     : constant CreatedByOS := 1;
OS_MASIX                    : constant CreatedByOS := 2;
OS_FREEBSD                  : constant CreatedByOS := 3;
OS_LITES                    : constant CreatedByOS := 4;

-------------------------------------------------------------------------------
-- If an Ext2 filesystem has these features, it can still be used by CuBit for
-- read/write even if CuBit doesn't implement those features.
-------------------------------------------------------------------------------
type CompatibleFeaturesRecord is
record
    directoryBlockPreallocation     : Boolean;
    afsServerInodes                 : Boolean;
    journaling                      : Boolean;
    extendedAttributes              : Boolean;
    nonStdInodeSize                 : Boolean;
    directoryIndexing               : Boolean;
    unused                          : Natural range 0..16#3FF_FFFF#;
end record with Size => 32;

for CompatibleFeaturesRecord use
record
    directoryBlockPreallocation     at 0 range 0..0;
    afsServerInodes                 at 0 range 1..1;
    journaling                      at 0 range 2..2;
    extendedAttributes              at 0 range 3..3;
    nonStdInodeSize                 at 0 range 4..4;
    directoryIndexing               at 0 range 5..5;
    unused                          at 0 range 6..31;
end record;

-------------------------------------------------------------------------------
-- If an Ext2 filesystem has these features, it must not be mounted by CuBit
-- unless CuBit implements those features.
-------------------------------------------------------------------------------
type IncompatibleFeaturesRecord is
record
    compressed                      : Boolean;
    typedDirectories                : Boolean;
    journalReplayRequired           : Boolean;
    journalDeviceUsed               : Boolean;
    unused                          : Natural range 0..16#FFF_FFFF#;
end record with Size => 32;

for IncompatibleFeaturesRecord use
record
    compressed                      at 0 range 0..0;
    typedDirectories                at 0 range 1..1;
    journalReplayRequired           at 0 range 2..2;
    journalDeviceUsed               at 0 range 3..3;
    unused                          at 0 range 4..31;
end record;

-------------------------------------------------------------------------------
-- If an Ext2 filesystem has these features, it may be mounted by CuBit, but
-- read-only.
-------------------------------------------------------------------------------
type ReadOnlyFeaturesRecord is
record
    sparseSuperblock                : Boolean;
    largeFileSupport                : Boolean;
    btreeSortedDirectories          : Boolean;
    unused                          : Natural range 0..16#1FFF_FFFF#;
end record with Size => 32;

for ReadOnlyFeaturesRecord use
record
    sparseSuperblock                at 0 range 0..0;
    largeFileSupport                at 0 range 1..1;
    btreeSortedDirectories          at 0 range 2..2;
    unused                          at 0 range 3..31;
end record;

type CompressionMethodsRecord is
record
    lzv1                            : Boolean;
    lzrw3a                          : Boolean;
    gzip                            : Boolean;
    bzip2                           : Boolean;
    lzo                             : Boolean;
    unused                          : Natural range 0..16#7FF_FFFF#;
end record with Size => 32;

for CompressionMethodsRecord use
record
    lzv1                            at 0 range 0..0;
    lzrw3a                          at 0 range 1..1;
    gzip                            at 0 range 2..2;
    bzip2                           at 0 range 3..3;
    lzo                             at 0 range 4..4;
    unused                          at 0 range 5..31;
end record;

type Ext2Filler is array (Natural range 0..787) of Unsigned_8 
    with Component_Size => 8;

-------------------------------------------------------------------------------
-- The Superblock is always at byte offset 1024. On 1KiB formatted disks, this
-- puts it at the start of the block 1. On 2KiB or larger disks, this puts
-- it within block 0.
--
-- @param blockShift - left shift 1024 by this number to get block size
-- @param fragmentShift - left shift 1024 by this number to get fragment size
-- @param lastMountTime - POSIX time
-- @param lastWriteTime - POSIX time
-- @param signature - should be 16#EF53#
-- @param optionalFeatures - CuBit can still modify a filesystem using these
--  features even if it doesn't support them.
-- @param readOnlyFeatures - If not supported, this filesystem must be mounted
--  read-only.
-- @param volumeName - null-terminated string with only ISO-Latin-1 chars
-------------------------------------------------------------------------------
type Superblock is
record
    inodeCount                      : Unsigned_32;
    blockCount                      : Unsigned_32;
    reservedSuperuserBlocks         : Unsigned_32;
    freeBlockCount                  : Unsigned_32;
    freeInodeCount                  : Unsigned_32;
    superblockNumber                : Unsigned_32;
    blockShift                      : Unsigned_32;
    fragmentShift                   : Unsigned_32;
    blocksPerBlockGroup             : Unsigned_32;
    fragmentsPerBlockGroup          : Unsigned_32;
    inodesPerBlockGroup             : Unsigned_32;
    lastMountTime                   : time.POSIXTime;
    lastWriteTime                   : time.POSIXTime;
    mountsSinceConsistencyCheck     : Unsigned_16;
    mountsBetweenConsistencyChecks  : Unsigned_16;
    signature                       : Unsigned_16;
    filesystemState                 : FSState;
    actionOnError                   : ErrorAction;
    versionMinor                    : Unsigned_16;
    lastConsistencyCheck            : time.POSIXTime;
    timeBetweenConsistencyChecks    : time.POSIXTime;
    createdBy                       : CreatedByOS;
    versionMajor                    : Unsigned_32;
    reservedBlockUserID             : Unsigned_16;
    reservedBlockGroupID            : Unsigned_16;
    -- Extended fields
    firstUnreservedInode            : Unsigned_32;
    inodeSize                       : Unsigned_16;
    superblockGroupNumber           : Unsigned_16;
    compatibleFeatures              : CompatibleFeaturesRecord;
    incompatibleFeatures            : IncompatibleFeaturesRecord;
    readOnlyFeatures                : ReadOnlyFeaturesRecord;
    uuidLo                          : Unsigned_64;
    uuidHi                          : Unsigned_64;
    volumeName                      : String (1..16);
    lastMountPath                   : String (1..64);
    compressionUsed                 : CompressionMethodsRecord;
    filePreallocateBlocks           : Unsigned_8;
    directoryPreallocateBlocks      : Unsigned_8;
    unused206                       : Unsigned_16;
    journalIDLo                     : Unsigned_64;
    journalIDHi                     : Unsigned_64;
    journalInode                    : Unsigned_32;
    journalDevice                   : Unsigned_32;
    orphanInodeHead                 : Unsigned_32;
    unused236                       : Ext2Filler;
end record with Size => 1024*8;

for Superblock use
record
    inodeCount                      at 0   range 0..31;
    blockCount                      at 4   range 0..31;
    reservedSuperuserBlocks         at 8   range 0..31;
    freeBlockCount                  at 12  range 0..31;
    freeInodeCount                  at 16  range 0..31;
    superblockNumber                at 20  range 0..31;
    blockShift                      at 24  range 0..31;
    fragmentShift                   at 28  range 0..31;
    blocksPerBlockGroup             at 32  range 0..31;
    fragmentsPerBlockGroup          at 36  range 0..31;
    inodesPerBlockGroup             at 40  range 0..31;
    lastMountTime                   at 44  range 0..31;
    lastWriteTime                   at 48  range 0..31;
    mountsSinceConsistencyCheck     at 52  range 0..15;
    mountsBetweenConsistencyChecks  at 54  range 0..15;
    signature                       at 56  range 0..15;
    filesystemState                 at 58  range 0..15;
    actionOnError                   at 60  range 0..15;
    versionMinor                    at 62  range 0..15;
    lastConsistencyCheck            at 64  range 0..31;
    timeBetweenConsistencyChecks    at 68  range 0..31;
    createdBy                       at 72  range 0..31;
    versionMajor                    at 76  range 0..31;
    reservedBlockUserID             at 80  range 0..15;
    reservedBlockGroupID            at 82  range 0..15;
    firstUnreservedInode            at 84  range 0..31;
    inodeSize                       at 88  range 0..15;
    superblockGroupNumber           at 90  range 0..15;
    compatibleFeatures              at 92  range 0..31;
    incompatibleFeatures            at 96  range 0..31;
    readOnlyFeatures                at 100 range 0..31;
    uuidLo                          at 104 range 0..63;
    uuidHi                          at 112 range 0..63;
    volumeName                      at 120 range 0..127;
    lastMountPath                   at 136 range 0..511;
    compressionUsed                 at 200 range 0..31;
    filePreallocateBlocks           at 204 range 0..7;
    directoryPreallocateBlocks      at 205 range 0..7;
    unused206                       at 206 range 0..15;
    journalIDLo                     at 208 range 0..63;
    journalIDHi                     at 216 range 0..63;
    journalInode                    at 224 range 0..31;
    journalDevice                   at 228 range 0..31;
    orphanInodeHead                 at 232 range 0..31;
    unused236                       at 236 range 0..6303;
end record;

type BlockGroupDescriptorFiller is array (Natural range 0..13) of Unsigned_8
    with Component_Size => 8;

-------------------------------------------------------------------------------
-- BlockGroupDescriptor
-------------------------------------------------------------------------------
type BlockGroupDescriptor is
record
    blockUsageBitmapAddr            : BlockAddr;
    inodeUsageBitmapAddr            : BlockAddr;
    inodeTableAddr                  : BlockAddr;
    numFreeBlocks                   : Unsigned_16;
    numFreeInodes                   : Unsigned_16;
    numDirectories                  : Unsigned_16;
    unused18                        : BlockGroupDescriptorFiller;
end record with Size => 256;

for BlockGroupDescriptor use
record
    blockUsageBitmapAddr            at 0  range 0..31;
    inodeUsageBitmapAddr            at 4  range 0..31;
    inodeTableAddr                  at 8  range 0..31;
    numFreeBlocks                   at 12 range 0..15;
    numFreeInodes                   at 14 range 0..15;
    numDirectories                  at 16 range 0..15;
    unused18                        at 18 range 0..111;
end record;

-------------------------------------------------------------------------------
-- Inode data structures and definitions
-------------------------------------------------------------------------------
type InodeTypeNibble is new Natural range 0..15 with Size => 4;

INODE_FIFO                : constant InodeTypeNibble := 16#1#;
INODE_CHARACTER_DEVICE    : constant InodeTypeNibble := 16#2#;
INODE_DIRECTORY           : constant InodeTypeNibble := 16#4#;
INODE_BLOCK_DEVICE        : constant InodeTypeNibble := 16#8#;
INODE_REGULAR_FILE        : constant InodeTypeNibble := 16#A#;
INODE_UNIX_SOCKET         : constant InodeTypeNibble := 16#C#;

type InodeTypeAndPermissions is
record
    worldExecute    : Boolean;
    worldWrite      : Boolean;
    worldRead       : Boolean;
    groupExecute    : Boolean;
    groupWrite      : Boolean;
    groupRead       : Boolean;
    userExecute     : Boolean;
    userWrite       : Boolean;
    userRead        : Boolean;
    sticky          : Boolean;
    sgid            : Boolean;
    suid            : Boolean;
    inodeType       : InodeTypeNibble;
end record with Size => 16;

for InodeTypeAndPermissions use
record
    worldExecute    at 0 range 0..0;
    worldWrite      at 0 range 1..1;
    worldRead       at 0 range 2..2;
    groupExecute    at 0 range 3..3;
    groupWrite      at 0 range 4..4;
    groupRead       at 0 range 5..5;
    userExecute     at 0 range 6..6;
    userWrite       at 0 range 7..7;
    userRead        at 0 range 8..8;
    sticky          at 0 range 9..9;
    sgid            at 0 range 10..10;
    suid            at 0 range 11..11;
    inodeType       at 0 range 12..15;
end record;

type InodeFlags is
record
    secureDeletion                  : Boolean;
    keepAfterDelete                 : Boolean;
    compressed                      : Boolean;
    writeNewDataImmediately         : Boolean;
    immutable                       : Boolean;
    appendOnly                      : Boolean;
    dontDump                        : Boolean;
    dontUpdateAccessedTime          : Boolean;
    reserved1                       : Unsigned_8;

    hashIndexedDirectory            : Boolean;
    afsDirectory                    : Boolean;
    journalFileData                 : Boolean;
    reserved2                       : Natural range 0..16#1FFF#;
end record with Size => 32;

for InodeFlags use
record
    secureDeletion                  at 0 range 0..0;
    keepAfterDelete                 at 0 range 1..1;
    compressed                      at 0 range 2..2;
    writeNewDataImmediately         at 0 range 3..3;
    immutable                       at 0 range 4..4;
    appendOnly                      at 0 range 5..5;
    dontDump                        at 0 range 6..6;
    dontUpdateAccessedTime          at 0 range 7..7;
    reserved1                       at 0 range 8..15;

    hashIndexedDirectory            at 0 range 16..16;
    afsDirectory                    at 0 range 17..17;
    journalFileData                 at 0 range 18..18;
    reserved2                       at 0 range 19..31;
end record;

type OSSpecificFields is
record
    fragmentNumber                  : Unsigned_8;
    fragmentSize                    : Unsigned_8;
    reserved1                       : Unsigned_16;
    userIDHi                        : Unsigned_16;
    groupIDHi                       : Unsigned_16;
    reserved2                       : Unsigned_32;
end record with Size => 12*8;

for OSSpecificFields use
record
    fragmentNumber                  at 0 range 0..7;
    fragmentSize                    at 1 range 0..7;
    reserved1                       at 2 range 0..15;
    userIDHi                        at 4 range 0..15;
    groupIDHi                       at 6 range 0..15;
    reserved2                       at 8 range 0..31;
end record;

-------------------------------------------------------------------------------
-- Inode data structure
-- @field directBlockX - pointer directly to data for this inode
-- @field singleIndirectBlock - pointer to first indirect block, which is a
--  block containing array of block IDs containing the data.
-- @field doubleIndirectBlock - pointer to block containing array of indirect
--  block IDs, with each of them containing an array of blocks.
-- @field tripleIndirectBlock - pointer to block containing array of doubly-
--  indirect block IDs, with each of those doubly-indirect blocks containing
--  an array of indirect blocks, and each of those containing an array of
--  direct blocks.
-- @field sizeHiDirACL - if this inode is a file, then the upper 32-bits of the
--  file size. If a directory, it should be treated as a BlockAddr containing
--  the extended directory attributes.
-------------------------------------------------------------------------------
type Inode is
record
    typeAndPermissions              : InodeTypeAndPermissions;
    userIDLo                        : Unsigned_16;
    sizeLo                          : Unsigned_32;
    accessed                        : time.POSIXTime;
    created                         : time.POSIXTime;
    modified                        : time.POSIXTime;
    deleted                         : time.POSIXTime;
    groupIDLo                       : Unsigned_16;
    numHardLinksHere                : Unsigned_16;
    numDiskSectorsUsed              : Unsigned_32;
    flags                           : InodeFlags;
    osSpecific1                     : Unsigned_32;
    directBlock0                    : BlockAddr;
    directBlock1                    : BlockAddr;
    directBlock2                    : BlockAddr;
    directBlock3                    : BlockAddr;
    directBlock4                    : BlockAddr;
    directBlock5                    : BlockAddr;
    directBlock6                    : BlockAddr;
    directBlock7                    : BlockAddr;
    directBlock8                    : BlockAddr;
    directBlock9                    : BlockAddr;
    directBlock10                   : BlockAddr;
    directBlock11                   : BlockAddr;
    singleIndirectBlock             : BlockAddr;
    doubleIndirectBlock             : BlockAddr;
    tripleIndirectBlock             : BlockAddr;
    generation                      : Unsigned_32;
    extendedAttributesBlock         : BlockAddr;
    sizeHiDirACL                    : Unsigned_32;
    fragmentAddr                    : BlockAddr;
    osSpecific2                     : OSSpecificFields;
end record with Size => 1024;

for Inode use
record
    typeAndPermissions              at 0   range 0..15;
    userIDLo                        at 2   range 0..15;
    sizeLo                          at 4   range 0..31;
    accessed                        at 8   range 0..31;
    created                         at 12  range 0..31;
    modified                        at 16  range 0..31;
    deleted                         at 20  range 0..31;
    groupIDLo                       at 24  range 0..15;
    numHardLinksHere                at 26  range 0..15;
    numDiskSectorsUsed              at 28  range 0..31;
    flags                           at 32  range 0..31;
    osSpecific1                     at 36  range 0..31;
    directBlock0                    at 40  range 0..31;
    directBlock1                    at 44  range 0..31;
    directBlock2                    at 48  range 0..31;
    directBlock3                    at 52  range 0..31;
    directBlock4                    at 56  range 0..31;
    directBlock5                    at 60  range 0..31;
    directBlock6                    at 64  range 0..31;
    directBlock7                    at 68  range 0..31;
    directBlock8                    at 72  range 0..31;
    directBlock9                    at 76  range 0..31;
    directBlock10                   at 80  range 0..31;
    directBlock11                   at 84  range 0..31;
    singleIndirectBlock             at 88  range 0..31;
    doubleIndirectBlock             at 92  range 0..31;
    tripleIndirectBlock             at 96  range 0..31;
    generation                      at 100 range 0..31;
    extendedAttributesBlock         at 104 range 0..31;
    sizeHiDirACL                    at 108 range 0..31;
    fragmentAddr                    at 112 range 0..31;
    osSpecific2                     at 116 range 0..95;
end record;

subtype DirectoryEntryType is Unsigned_8;
FILETYPE_UNKNOWN            : DirectoryEntryType := 0;  -- used for padding
FILETYPE_REGULAR_FILE       : DirectoryEntryType := 1;
FILETYPE_DIRECTORY          : DirectoryEntryType := 2;
FILETYPE_CHARACTER_DEVICE   : DirectoryEntryType := 3;
FILETYPE_BLOCK_DEVICE       : DirectoryEntryType := 4;
FILETYPE_FIFO               : DirectoryEntryType := 5;
FILETYPE_SOCKET             : DirectoryEntryType := 6;
FILETYPE_SYMLINK            : DirectoryEntryType := 7;

-------------------------------------------------------------------------------
-- @param inode - 32bit inode number of the file entry. If 0, not used.
-- @param length - displacement from start of this entry to the next entry.
--  entries must be 4-byte aligned and cannot span blocks.
-- @param nameLength - how long the name is.
-- @param entryType - see DirectoryEntryType
--
-- Note that the Directory Entries also contain a name and possibly extra
--  padding at the end. We'll read these separately, since they can be
--  different sizes.
-------------------------------------------------------------------------------
type DirectoryEntry is
record
    inode                           : InodeAddr;
    length                          : Unsigned_16;
    nameLength                      : Unsigned_8;
    entryType                       : DirectoryEntryType;
    --name
    --padding
end record;

-------------------------------------------------------------------------------
-- blockSize - given a Superblock, return the size of its filesystem data block
-------------------------------------------------------------------------------
function blockSize(sb : in Superblock) return Unsigned_32;

-------------------------------------------------------------------------------
-- getBlockGroup - given an inode address and the number of inodes per block
--  group, this function returns the block group containing that inode.
-------------------------------------------------------------------------------
function getBlockGroup(inode : in InodeAddr; 
                       inodesPerBlockGroup : in Unsigned_32)
                       return Unsigned_32;

-------------------------------------------------------------------------------
-- getInodeIndex - given an inode address and block size, this function
--  returns the index into the block group's inode table.
-------------------------------------------------------------------------------
function getInodeIndex(inode : in InodeAddr;
                       blockSize : in Unsigned_32)
                       return Unsigned_32;

-------------------------------------------------------------------------------
-- getContainingBlock - given an index into a block group's inode table, the
--  size of an inode and the size of a block, this function returns the block
--  containing that inode.
-------------------------------------------------------------------------------
function getContainingBlock(index : in Unsigned_32;
                            inodeSize : in Unsigned_32;
                            blockSize : in Unsigned_32)
                            return BlockAddr;


-------------------------------------------------------------------------------
-- print - Output the superblock details
-------------------------------------------------------------------------------
procedure print(sb : in Superblock);


end Filesystem.Ext2;