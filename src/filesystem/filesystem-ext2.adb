-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Ext2 Filesystem
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with Debug;
with FileCache;
with Textmode; use Textmode;
with Util;
with Virtmem;

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
    -- getBlockGroup - see which block group contains an inode
    ---------------------------------------------------------------------------
    function getBlockGroup(inodeNum : in InodeAddr;
                           sb       : in Superblock) return BlockGroupNumber with
        SPARK_Mode => On
    is
    begin
        return (inodeNum - 1) / sb.inodesPerBlockGroup;
    end getBlockGroup;


    ---------------------------------------------------------------------------
    -- getInodeIndex - find the inode's index within the block group's inode
    -- table.
    ---------------------------------------------------------------------------
    function getInodeIndex(inodeNum : in InodeAddr;
                           sb       : in Superblock) return Unsigned_32 with
        SPARK_Mode => On
    is
    begin
        return (inodeNum - 1) mod sb.inodesPerBlockGroup;
    end getInodeIndex;


    ---------------------------------------------------------------------------
    -- getContainingBlock
    ---------------------------------------------------------------------------
    function getContainingBlock(index   : in Unsigned_32;
                                sb      : in Superblock) return BlockAddr
    with
        SPARK_Mode => On
    is
    begin
        return BlockAddr((index * Unsigned_32(sb.inodeSize)) / blockSize(sb));
    end getContainingBlock;


    procedure readSuperBlock(device : in Devices.DeviceID;
                             sb     : in out Superblock) with
        SPARK_Mode => On
    is
        buf             : FileCache.BufferPtr;
    begin
        -- Ext2 superblock is always at byte 1024, so if we read the first
        -- "page" (first 8 sectors on 512-byte sector disks) of the disk,
        -- we'll snag it.
        FileCache.readBuffer(device, 0, buf);
        
        -- println("Read buffer: ");
        -- Debug.dumpMem(To_Address(buf.data), 4096);

        -- Get the Superblock
        Util.memCopy(dest   => sb'Address,
                     src    => To_Address(buf.data + 1024),
                     len    => sb'Size / 8);

        FileCache.releaseBuffer(buf);

    end readSuperBlock;


    procedure readBlockGroupDescriptors(device      : in Devices.DeviceID;
                                        sb          : in SuperBlock;
                                        bgdt        : out System.Address;
                                        bgdtOrder   : out BuddyAllocator.Order;
                                        bgdtLength  : out Natural) with
        SPARK_Mode => On
    is
        bgdtSize        : Unsigned_64;
        bgdtPhys        : Virtmem.PhysAddress;

        -- Start and end disk blocks holding the BGDT
        startDiskBlock  : Unsigned_64 := Unsigned_64(sb.superblockNumber + 1);
        numDiskBlocks   : Unsigned_64;
        endDiskBlock    : Unsigned_64;

        buf             : FileCache.BufferPtr;
        writePtr        : System.Address;

        function roundToNearest is new Util.roundToNearest(Unsigned_64);
    begin

        -- Block Group descriptors can occupy more than one block's worth of
        -- space on disk. We'll dynamically allocate the memory required for
        -- them here.
        bgdtLength := Natural(sb.blockCount / sb.blocksPerBlockGroup);
        --print("Number of Block Groups: "); println(bgdtLength);

        bgdtSize := Unsigned_64(bgdtLength * (BlockGroupDescriptor'Size / 8));
        numDiskBlocks := roundToNearest(bgdtSize, 4096) / 4096;
        endDiskBlock := startDiskBlock + numDiskBlocks - 1;
        --print("Size of table: "); println(bgdtSize);
        print("Num disk blocks holding BGDT: "); println(numDiskBlocks);

        -- Each 4K page can hold 128 block group descriptors. This is wasteful
        -- but must suffice until we get a more general allocator
        bgdtOrder := BuddyAllocator.getOrder(bgdtSize);
        --print("Order of allocation: "); println(Unsigned_32(bgdtOrder));

        BuddyAllocator.alloc(bgdtOrder, bgdtPhys);
        --print("Allocated: "); println(bgdtPhys);

        if bgdtPhys = BuddyAllocator.NO_BLOCK_AVAILABLE then
            bgdt := System.Null_Address;
            bgdtLength := 0;
            return;
        end if;

        bgdt := To_Address(Virtmem.P2V(bgdtPhys));

        -- Read the disk blocks holding the BGDT. Copy them into the BGDT,
        -- One page at a time.
        writePtr := bgdt;

        for block in startDiskBlock .. endDiskBlock loop
            --print("Reading disk block "); println(block);
            FileCache.readBuffer(device, block, buf);
            
            --Debug.dumpMem(To_Address(buf.data), 4096);
            
            Util.memCopy(dest   => writePtr,
                         src    => To_Address(buf.data),
                         len    => 4096);

            FileCache.releaseBuffer(buf);

            writePtr := writePtr + 4096;
        end loop;

    end readBlockGroupDescriptors;


    procedure readInode(device      : in Devices.DeviceID;
                        sb          : in Superblock;
                        inodeNum    : in InodeAddr;
                        outInode    : in out Inode)
    is
        numBlockGroups      : Unsigned_32;
        BGDTTableSize       : Unsigned_32;
        inodeBlockGroup     : BlockGroupNumber;
        
        -- Block containing BGDT
        BGDTBlock           : BlockAddr;
        -- Index within that block.
        BGDTOffset          : Unsigned_16;
    begin

        print("Reading inode #"); printdln(inodeNum);

        numBlockGroups := sb.blockCount / sb.blocksPerBlockGroup;
        print("Number of Block Groups: "); printdln(numBlockGroups);

        -- The BlockGroupDescriptors can occupy several blocks, so we need to
        -- see which one we want to read here.
        BGDTTableSize := numBlockGroups * (BlockGroupDescriptor'Size / 8);
        print("BGDT Table Size: "); printdln(BGDTTableSize);

        inodeBlockGroup := getBlockGroup(inodeNum, sb);
        print("Inode Block Group: "); printdln(inodeBlockGroup);

        if sb.readOnlyFeatures.sparseSuperblock then
            -- If the block group is 0, 1 or power of 3,5,7 then it will contain
            -- a backup of the superblock and block group descriptor table.
            println("Sparse Superblocks in use");
        else
            -- Every block group contains the superblock and block group
            -- descriptor table.
            null;
        end if;

        --FileCache.readBuffer(device, , buf);

        -- and the block group descriptor table
        -- Util.memCopy(dest   => bgdt'Address,
        --              src    => To_Address(buf.data + 1024 + SuperBlock'Size / 8),
        --              len    => bgdt'Size / 8);
    end readInode;


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
