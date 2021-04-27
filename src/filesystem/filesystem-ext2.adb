-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- Ext2 Filesystem
-------------------------------------------------------------------------------
with Ada.Unchecked_Deallocation;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with FileCache;
with TextIO; use TextIO;
with Util;

package body Filesystem.Ext2 is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    function setup (device : Devices.DeviceID) return Ext2Filesystem
    is
        fs             : Ext2Filesystem;
        -- numBlockGroups : BlockGroupNumber;
    begin
    
        Ext2.readSuperBlock (device  => device,
                             sb      => fs.sblock);

        if fs.sblock.signature = Ext2.EXT2_SUPER_MAGIC and Ext2.blockSize(fs.sblock) = 4096 then
            fs.device := device;
            
            -- How many Block Groups?
            -- numBlockGroups := fs.sblock.blockCount / fs.sblock.blocksPerBlockGroup;
            -- print ("Number of Block Groups: "); println (numBlockGroups);

            -- Will allocate memory for the bgdt and read it in from disk
            fs.bgdt := readBlockGroupDescriptors (fs.device, fs.sblock);

            if fs.bgdt = null then
                fs.initialized := False;
                return fs;
            end if;

            fs.initialized := True;
            return fs;
        else
            fs.initialized := False;
            return fs;
        end if;
    end setup;

    ---------------------------------------------------------------------------
    -- teardown
    ---------------------------------------------------------------------------
    procedure teardown (fs : in out Ext2Filesystem) is
        procedure free is new Ada.Unchecked_Deallocation (Object => BlockGroupDescriptorTable,
                                                          Name   => BGDTPtr);
    begin
        -- BuddyAllocator.free (ord  => BuddyAllocator.getOrder(fs.bgdt'Size / 8),
        --                      addr => Virtmem.V2P(To_Integer(fs.bgdt.all'Address)));

        free (fs.bgdt);
        fs.device      := (major => Devices.NO_MAJOR, minor => Devices.NO_MINOR, reserved => 0);
        fs.initialized := False;
        fs.bgdt        := null;
    end teardown;

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

    ---------------------------------------------------------------------------
    -- readSuperBlock
    ---------------------------------------------------------------------------
    procedure readSuperBlock(device : in Devices.DeviceID;
                             sb     : in out Superblock) with
        SPARK_Mode => On
    is
        buf : FileCache.BufferPtr;
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

    ---------------------------------------------------------------------------
    -- getBlockGroupDescriptor - read the descriptor for a particular block group
    ---------------------------------------------------------------------------
    function getBlockGroupDescriptor (fs  : in Ext2Filesystem;
                                      grp : in BlockGroupNumber) return BlockGroupDescriptor
    is
    begin
        if fs.initialized then
            return fs.bgdt(grp);
        else
            raise Ext2Exception with "Attempted to get Block Group Descriptor of uninitialized filesystem";
        end if;
    end getBlockGroupDescriptor;

    ---------------------------------------------------------------------------
    -- readBlockGroupDescriptors
    ---------------------------------------------------------------------------
    function readBlockGroupDescriptors (device     : in Devices.DeviceID;
                                        sb         : in SuperBlock) return BGDTPtr
    with
        SPARK_Mode => On
    is
        bgdtLength      : constant Unsigned_32 := sb.blockCount / sb.blocksPerBlockGroup;
        bgdtSize        : Unsigned_64;
        bgdtAddr        : System.Address;
        bgdt            : BGDTPtr;

        -- Start and end disk blocks holding the BGDT
        startDiskBlock  : constant Unsigned_64 := Unsigned_64(sb.superblockNumber + 1);
        numDiskBlocks   : Unsigned_64;
        endDiskBlock    : Unsigned_64;

        buf             : FileCache.BufferPtr;
        writePtr        : System.Address;

        function roundToNearest is new Util.roundToNearest(Unsigned_64);

        -- package ATAC is new System.Address_To_Access_Conversions (BlockGroupDescriptorTable(1..bgdtLength));
        -- use ATAC;
    begin

        -- Block Group descriptors can occupy more than one block's worth of
        -- space on disk. We'll dynamically allocate the memory required for
        -- them here.
        bgdtSize      := Unsigned_64(bgdtLength * (BlockGroupDescriptor'Size / 8));
        numDiskBlocks := roundToNearest (bgdtSize, 4096) / 4096;
        endDiskBlock  := startDiskBlock + numDiskBlocks - 1;

        bgdt := new BlockGroupDescriptorTable(0..bgdtLength-1);
        
        --BuddyAllocator.alloc (BuddyAllocator.getOrder(bgdtSize), bgdtPhys);
        --print("Allocated: "); println(bgdtPhys);

        -- if bgdtPhys = BuddyAllocator.NO_BLOCK_AVAILABLE then
        --     return null;
        -- end if;

        bgdtAddr := bgdt.all'Address;

        -- Read the disk blocks holding the BGDT. Copy them into the BGDT,
        -- One page at a time.
        writePtr := bgdtAddr;

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

        return bgdt;
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

    ---------------------------------------------------------------------------
    -- readInode - given a device identifier, an ext2 superblock and block group
    --  descriptor table, read the contents of a given inode address inodeNum 
    --  into the outInode parameter.
    ---------------------------------------------------------------------------
    function readInode (fs       : in Ext2Filesystem;
                        inodeNum : in InodeAddr) return Inode
    is
        -- block group the inode is in
        bg      : constant BlockGroupNumber := getBlockGroup (inodeNum, fs.sblock);

        -- what index within the inode table for that block group?
        index   : constant Unsigned_32      := getInodeIndex (inodeNum, fs.sblock);

        -- starting block of the inode table?
        table   : constant Unsigned_32      := fs.bgdt(bg).inodeTableAddr;

        -- what FS block holds the index?
        block   : constant BlockAddr        := getContainingBlock (index, fs.sblock);

        -- within that block, where is the inode itself?
        offset  : constant Unsigned_32      := getInodeOffset (inodeNum, fs.sblock);

        buf      : FileCache.BufferPtr;
        outInode : Inode;
    begin
        -- print ("Reading inode #    "); printdln (inodeNum);
        -- print ("Inode Block Group: "); printdln (bg);
        -- print ("Inode Table Index: "); printdln (index);
        -- print ("Inode Table Block  "); println (table);
        -- print ("Block w/ Inode:    "); println (block);
        -- print ("Offset w/in Block: "); println (offset);

        -- print ("Free blocks:  "); println (fs.bgdt(bg).numFreeBlocks);
        -- print ("Free inodes:  "); println (fs.bgdt(bg).numFreeInodes);
        -- print ("Num folders:  "); println (fs.bgdt(bg).numDirectories);
        -- print ("Block Bitmap: "); println (fs.bgdt(bg).blockBitmapAddr);
        -- print ("Inode Bitmap: "); println (fs.bgdt(bg).inodeBitmapAddr);

        -- Read the block with our inode, copy only the data we need.
        FileCache.readBuffer (fs.device, Unsigned_64(table + block), buf);
        
        Util.memCopy (dest => outInode'Address,
                      src  => To_Address (buf.data + Integer_Address(offset)),
                      len  => Inode'Size / 8);

        FileCache.releaseBuffer (buf);

        -- Debug.dumpMem(outInode'Address, 128);
        return outInode;
    end readInode;

    ---------------------------------------------------------------------------
    -- dumpDirs
    ---------------------------------------------------------------------------
    procedure dumpDirs (fs    : in Ext2Filesystem;
                        block : in BlockAddr;
                        size  : in Unsigned_32)
    is
        buf    : FileCache.BufferPtr;
        offset : Integer_Address := 0;
    begin
        -- Read the block
        FileCache.readBuffer (fs.device, Unsigned_64(block), buf);

        PrintDirs : loop
            PrintDir : declare
                dir : DirectoryEntry with Import, Address => To_Address(buf.data + offset);
            begin
                PrintFile : declare
                    filename : String(1..Integer(dir.nameLength)) with
                        Import, Address => dir.name'Address;
                begin
                    print (" Inode: ");
                    print (dir.inode);
                    print (" Name: ");
                    print (filename);

                    case dir.fileType is
                        when FILETYPE_UNKNOWN =>
                            println(" Unknown");
                        when FILETYPE_REGULAR_FILE =>
                            println(" File");
                        when FILETYPE_DIRECTORY =>
                            println("/");
                        when others =>
                            println;
                    end case;
                end PrintFile;

                offset := offset + Integer_Address(dir.length);

                exit PrintDirs when offset >= Integer_Address(size);
            end PrintDir;
        end loop PrintDirs;

        FileCache.releaseBuffer (buf);
    end dumpDirs;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (sb : in Superblock) with
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

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (myInode : in Inode) with SPARK_Mode => On is
        fileTypeChar : character;
        GID : Unsigned_32;
        UID : Unsigned_32;
    begin
        println("typeAndPermissions: ");

        print("userIDLo: "); println(myInode.userIDLo);
        print("sizeLo: "); println(myInode.sizeLo);
        print("accessed: "); println(myInode.accessed);
        print("created: "); println(myInode.created);
        print("modified: "); println(myInode.modified);
        print("deleted: "); println(myInode.deleted);
        print("groupIDLo: "); println(myInode.groupIDLo);
        print("numHardLinksHere: "); println(myInode.numHardLinksHere);
        print("numDiskSectorsUsed: "); println(myInode.numDiskSectorsUsed);
        
        println("flags: ");
        print(" secureDeletion: "); println(myInode.flags.secureDeletion);
        print(" keepAfterDelete: "); println(myInode.flags.keepAfterDelete);
        print(" compressed: "); println(myInode.flags.compressed);
        print(" writeNewDataImmediately: "); println(myInode.flags.writeNewDataImmediately);
        print(" immutable: "); println(myInode.flags.immutable);
        print(" appendOnly: "); println(myInode.flags.appendOnly);
        print(" dontDump: "); println(myInode.flags.dontDump);
        print(" dontUpdateAccessedTime: "); println(myInode.flags.dontUpdateAccessedTime);
        print(" hashIndexedDirectory: "); println(myInode.flags.hashIndexedDirectory);
        print(" afsDirectory: "); println(myInode.flags.afsDirectory);
        print(" journalFileData: "); println(myInode.flags.journalFileData);

        --print("osSpecific1: ");
        print("directBlock0: ");            println(myInode.directBlock0);
        print("directBlock1: ");            println(myInode.directBlock1);
        print("directBlock2: ");            println(myInode.directBlock2);
        print("directBlock3: ");            println(myInode.directBlock3);
        print("directBlock4: ");            println(myInode.directBlock4);
        print("directBlock5: ");            println(myInode.directBlock5);
        print("directBlock6: ");            println(myInode.directBlock6);
        print("directBlock7: ");            println(myInode.directBlock7);
        print("directBlock8: ");            println(myInode.directBlock8);
        print("directBlock9: ");            println(myInode.directBlock9);
        print("directBlock10: ");           println(myInode.directBlock10);
        print("directBlock11: ");           println(myInode.directBlock11);
        print("singleIndirectBlock: ");     println(myInode.singleIndirectBlock);
        print("doubleIndirectBlock: ");     println(myInode.doubleIndirectBlock);
        print("tripleIndirectBlock: ");     println(myInode.tripleIndirectBlock);

        print("generation: ");              println(myInode.generation);
        print("extendedAttributesBlock: "); println(myInode.extendedAttributesBlock);
        print("sizeHi_DirACL: ");           println(myInode.sizeHi_DirACL);
        --print("fragmentAddr: ");            
        --print("osSpecific2: ");
    end print;

end Filesystem.Ext2;
