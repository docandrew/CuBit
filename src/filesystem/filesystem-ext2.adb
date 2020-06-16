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

    ---------------------------------------------------------------------------
    -- getInodeOffset - given an inode number and a superblock, where inside the
    --  block (containing the inode table for that block group) can we find the
    --  inode itself?
    ---------------------------------------------------------------------------
    function getInodeOffset(inodeNum    : in InodeAddr;
                            sb          : in Superblock) return Unsigned_32
    with
        SPARK_Mode => On
    is
        inodesPerBlock : constant Unsigned_32 := blockSize(sb) / (Inode'Size / 8);
    begin
        return ((inodeNum - 1) mod inodesPerBlock) * (Inode'Size / 8);
    end getInodeOffset;


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
                                        bgdtLength  : out BlockGroupNumber) with
        SPARK_Mode => On
    is
        bgdtSize        : Unsigned_64;
        bgdtPhys        : Virtmem.PhysAddress;

        -- Start and end disk blocks holding the BGDT
        startDiskBlock  : constant Unsigned_64 := Unsigned_64(sb.superblockNumber + 1);
        numDiskBlocks   : Unsigned_64;
        endDiskBlock    : Unsigned_64;

        buf             : FileCache.BufferPtr;
        writePtr        : System.Address;

        function roundToNearest is new Util.roundToNearest(Unsigned_64);
    begin

        -- Block Group descriptors can occupy more than one block's worth of
        -- space on disk. We'll dynamically allocate the memory required for
        -- them here.
        bgdtLength := sb.blockCount / sb.blocksPerBlockGroup;
        --print("Number of Block Groups: "); println(bgdtLength);

        bgdtSize := Unsigned_64(bgdtLength * (BlockGroupDescriptor'Size / 8));
        numDiskBlocks := roundToNearest(bgdtSize, 4096) / 4096;
        endDiskBlock := startDiskBlock + numDiskBlocks - 1;
        --print("Size of table: "); println(bgdtSize);
        print("Num disk blocks holding BGDT: "); println(numDiskBlocks);

        -- Each 4K page can hold 128 block group descriptors. This is wasteful
        -- but must suffice until we get a more general allocator.
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

    ----------------------------------------------------------------------------
    -- testRoot - given numbers a, b, return True if a is a power of b, false
    --  otherwise.
    ----------------------------------------------------------------------------
    function testRoot(a : Unsigned_32; b : Unsigned_32) return Boolean with
        SPARK_Mode => On
    is
        num : Unsigned_32 := b;
    begin
        while a > num loop
            num := num * b;
        end loop;

        return (num = a);
    end testRoot;

    ----------------------------------------------------------------------------
    -- hasSuperblock - given a block group number, and assuming sparse
    --  superblocks are in use, returns True if this block group will contain
    --  the superblock or a superblock copy as the first block in the group.
    --  When sparse superblocks are turned on, shadow copies of the superblock
    --  are kept in block groups 0, 1 and powers of 3, 5 and 7.
    ----------------------------------------------------------------------------
    function hasSparseSuperblock(blockGroup : in BlockGroupNumber)
        return Boolean with SPARK_Mode => On
    is
    begin
        if blockGroup <= 1 then
            return True;
        end if;
        
        return (testRoot(blockGroup, 3) or
                testRoot(blockGroup, 5) or
                testRoot(blockGroup, 7));
        
    end hasSparseSuperblock;


    procedure readInode(device      : in Devices.DeviceID;
                        sb          : in Superblock;
                        bgdt        : in out BlockGroupDescriptorTable;
                        inodeNum    : in InodeAddr;
                        outInode    : in out Inode)
    is
        -- block group the inode is in
        bg      : constant BlockGroupNumber     := getBlockGroup(inodeNum, sb);

        -- what index within the inode table for that block group?
        index   : constant Unsigned_32          := getInodeIndex(inodeNum, sb);

        -- starting block of the inode table?
        table   : constant Unsigned_32          := bgdt(bg).inodeTableAddr;

        -- what FS block holds the index?
        block   : constant BlockAddr            := getContainingBlock(index, sb);

        -- within that block, where is the inode itself?
        offset  : constant Unsigned_32          := getInodeOffset(inodeNum, sb);

        buf : FileCache.BufferPtr;
    begin
                            
        print("Reading inode #    "); printdln(inodeNum);
        print("Inode Block Group: "); printdln(bg);
        print("Inode Table Index: "); printdln(index);
        print("Inode table:  "); println(table);
        print("Block w/ Inode:    "); println(block);
        print("Offset w/in Block: "); println(offset);

        print("Free blocks:  "); println(bgdt(bg).numFreeBlocks);
        print("Free inodes:  "); println(bgdt(bg).numFreeInodes);
        print("Num folders:  "); println(bgdt(bg).numDirectories);
        print("Block Bitmap: "); println(bgdt(bg).blockBitmapAddr);

        -- Read the block with our inode, copy only the data we need.
        FileCache.readBuffer(device, Unsigned_64(table + block), buf);
        
        Util.memCopy(dest   => outInode'Address,
                     src    => To_Address(buf.data + Integer_Address(offset)),
                     len    => Inode'Size / 8);

        FileCache.releaseBuffer(buf);

        Debug.dumpMem(outInode'Address, 128);
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
