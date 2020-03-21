-------------------------------------------------------------------------------
-- Cubit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Ext2 Filesystem
-------------------------------------------------------------------------------
with Textmode; use Textmode;

package body Filesystem.Ext2 is

    ---------------------------------------------------------------------------
    -- blockSize
    ---------------------------------------------------------------------------
    function blockSize(sb : in Superblock) return Unsigned_32 with
        SPARK_Mode => On
    is
    begin
        return Shift_Left(Unsigned_32(1024), Natural(sb.blockShift));
    end blockSize;

    
    ---------------------------------------------------------------------------
    -- getBlockGroup
    ---------------------------------------------------------------------------
    function getBlockGroup(inode : in InodeAddr; 
                           inodesPerBlockGroup : in Unsigned_32) 
    return Unsigned_32 with
        SPARK_Mode => On
    is
    begin
        return (inode - 1) / inodesPerBlockGroup;
    end getBlockGroup;


    ---------------------------------------------------------------------------
    -- getInodeIndex
    ---------------------------------------------------------------------------
    function getInodeIndex(inode : in InodeAddr; blockSize : in Unsigned_32)
    return Unsigned_32 with
        SPARK_Mode => On
    is
    begin
        return (inode - 1) mod blockSize;
    end getInodeIndex;


    ---------------------------------------------------------------------------
    -- getContainingBlock
    ---------------------------------------------------------------------------
    function getContainingBlock(index : in Unsigned_32;
                                inodeSize : in Unsigned_32;
                                blockSize : in Unsigned_32) return BlockAddr
    with
        SPARK_Mode => On
    is
    begin
        return BlockAddr((index * inodeSize) / blockSize);
    end getContainingBlock;


    ---------------------------------------------------------------------------
    -- print(Superblock)
    ---------------------------------------------------------------------------
    procedure print(sb : in Superblock) with
        SPARK_Mode => On
    is
    begin
        print("inodeCount "); println(sb.inodeCount);
        print("blockCount "); println(sb.blockCount);
        print("reservedSuperuserBlocks ");    println(sb.reservedSuperuserBlocks);
        print("freeBlockCount "); println(sb.freeBlockCount);
        print("freeInodeCount "); println(sb.freeInodeCount);
        print("superblockNumber ");   println(sb.superblockNumber);
        print("blockShift "); println(sb.blockShift);
        print("fragmentShift ");  println(sb.fragmentShift);
        print("blocksPerBlockGroup ");    println(sb.blocksPerBlockGroup);
        print("fragmentsPerBlockGroup "); println(sb.fragmentsPerBlockGroup);
        print("inodesPerBlockGroup ");    println(sb.inodesPerBlockGroup);
        --print("sb.lastMountTime "); println(Unsigned_32(sb.lastMountTime));
        --print("sb.lastWriteTime "); println(Unsigned_32(sb.lastWriteTime));
        --print("mountsSinceConsistencyCheck ");    println(sb.mountsSinceConsistencyCheck);
        --print("mountsBetweenConsistencyChecks "); println(sb.mountsBetweenConsistencyChecks);
        print("signature ");  println(sb.signature);
        --print("filesystemState ");    println(sb.filesystemState);
        --print("actionOnError ");  println(sb.actionOnError);
        print("versionMinor ");   println(sb.versionMinor);
        --print("lastConsistencyCheck ");   println(sb.lastConsistencyCheck);
        --print("timeBetweenConsistencyChecks ");   println(sb.timeBetweenConsistencyChecks);
        --print("createdBy ");  println(sb.createdBy);
        print("versionMajor ");   println(sb.versionMajor);
        print("reservedBlockUserID ");    println(sb.reservedBlockUserID);
        print("reservedBlockGroupID ");   println(sb.reservedBlockGroupID);
        print("firstUnreservedInode ");   println(sb.firstUnreservedInode);
        print("inodeSize ");  println(sb.inodeSize);
        print("superblockGroupNumber ");  println(sb.superblockGroupNumber);
        --print("compatibleFeatures "); println(sb.compatibleFeatures);
        --print("incompatibleFeatures ");   println(sb.incompatibleFeatures);
        --print("readOnlyFeatures ");   println(sb.readOnlyFeatures);
        print("uuidLo "); println(sb.uuidLo);
        print("uuidHi "); println(sb.uuidHi);
        print("volumeName "); println(sb.volumeName);
        print("lastMountPath ");  println(sb.lastMountPath);
        --print("compressionUsed ");    println(sb.compressionUsed);
        print("filePreallocateBlocks ");  println(sb.filePreallocateBlocks);
        --print("directoryPreallocateBlocks "); println(sb.directoryPreallocateBlocks);
        
        --print("journalIDLo ");    println(sb.journalIDLo);
        --print("journalIDHi ");    println(sb.journalIDHi);
        --print("journalInode ");   println(sb.journalInode);
        --print("journalDevice ");  println(sb.journalDevice);
        --print("orphanInodeHead ");    println(sb.orphanInodeHead);
        
    end print;
end Filesystem.Ext2;