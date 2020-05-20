-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Block Device buffer cache
-------------------------------------------------------------------------------
with LinkedList;
with Spinlock;
with x86;

Pragma Elaborate_All (Spinlock);

generic with
    BlockSize   : Positive,
    DeviceID    : Unsigned_32;

package BlockDevice with
    SPARK_Mode => On
is
    -- type DeviceID is new Unsigned_32;

    type BlockFlags is (VALID, DIRTY, BUSY);

    --@TODO allocate a bunch of space for the blocks.
    type BlockData is array (0 .. BlockSize - 1) of Unsigned_8;
    type BlockPtr is access BlockData;

    ---------------------------------------------------------------------------
    -- In-memory cache for disk blocks. Note that this is a file system block,
    -- which should be an integer multiple of the disk sector size.
    ---------------------------------------------------------------------------
    type BlockBuffer is record
        flags       : BlockFlags
        device      : DeviceID;
        blockNumber : Unsigned_64;
        lock        : spinlock.Spinlock;
        refCount    : Natural;
        data        : BlockPtr;
    end record;

    package BufferList is new LinkedList(BlockBuffer, BlockDevice.print);

    -- Each block device gets one of these. This is a linked list of BlockBuffers
    -- that will be synchronized to/from disk.
    type BufferCache is record
        lock        : spinlock.Spinlock;
        buffers     : BufferList.List; 
    end record;

    ---------------------------------------------------------------------------
    -- Setup
    ---------------------------------------------------------------------------
    procedure setup;

    ---------------------------------------------------------------------------
    -- getBuffer
    -- Either return the cached buffer for a particular device/block, or
    -- allocate a new one from the list of buffers.
    ---------------------------------------------------------------------------
    procedure getBuffer(dev : in DeviceID; blockNum : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- Print block buffer for debugging. Necessary for LinkedList
    -- instantiation.
    ---------------------------------------------------------------------------
    procedure print(buf : in BlockBuffer);

private
    -- We'll instantiate this package for each block device and keep its
    -- BufferCache object here
    cache : BufferCache;

end BlockDevice;
