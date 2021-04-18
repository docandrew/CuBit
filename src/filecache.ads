-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary
-- File Cache
--
-- @description
-- This package implements the traditional UNIX model of buffer
-- caches. Buffered blocks can either be BUSY (reserved by a process),
-- VALID (holding up-to-date data from disk), or DIRTY (modified by a process,
-- needs to be written to disk). We use a unified buffer cache.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Config;
with Devices;
with Spinlocks;
with Virtmem;

Pragma Elaborate_All (Spinlocks);

package FileCache with
    SPARK_Mode => On
is
    -- When a process attempts to get a block and none are available
    NoBuffersException : exception;

    -- When a process attempts to write a block not in the BUSY state
    WriteBlockNotOwnedException : exception;

    -- When a process attempts to release a block not in the BUSY state
    ReleaseNotOwnedException : exception;

    -- A block here is just a page-aligned chunk of memory that we'll sync
    -- with an underlying file.
    --type Block is array (1 .. Virtmem.PAGE_SIZE) of Unsigned_8;
    --type Block is new Storage_Array(1 .. Storage_Offset(Virtmem.PAGE_SIZE));

    type Buffer;
    type BufferPtr is access all Buffer;
  
    bufferLockName : aliased String := "Buffer Lock";

    ---------------------------------------------------------------------------
    -- In-memory record for a disk/file block.
    --
    -- @field syncNext points to the next buffer that should be synced to/from
    --  the underlying block device.
    ---------------------------------------------------------------------------
    type Buffer is record
        busy        : Boolean;
        valid       : Boolean;
        dirty       : Boolean;

        device      : Devices.DeviceID := (major    => Devices.NO_MAJOR,
                                           minor    => Devices.NO_MINOR,
                                           reserved => 0);
        
        blockNum    : Unsigned_64;
        lock        : Spinlocks.Spinlock := (name => bufferLockName'Access, others => <>);
        -- refCount    : Natural;

        -- Address of the block in linear memory. Necessary since we'll want to
        -- pass the physical address to disk drivers for DMA ops.
        data        : Virtmem.VirtAddress;
        
        prev        : BufferPtr;
        next        : BufferPtr;
        syncNext    : BufferPtr;
    end record;

    type BufferArray is array (1 .. Config.NUM_BLOCK_BUFFERS) of aliased Buffer;

    -- type BufferArray(BlockSize : Positive) is record
    --     arr : array (1 .. Config.NUM_BLOCK_BUFFERS) of aliased Buffer(BlockSize);
    -- end record;
    
    cacheLockName : aliased String := "File Cache Lock";

    -- Each block device gets one of these. Maintain the static array,
    -- LRU list of pointers to the blocks and a mutex.
    type FileCache is record
        lock        : Spinlocks.spinlock := (name => cacheLockName'Access, others => <>);
        buffers     : BufferArray;

        -- last used is head.next
        head        : aliased Buffer;
    end record;

    ---------------------------------------------------------------------------
    -- Setup
    ---------------------------------------------------------------------------
    procedure setup;

    ---------------------------------------------------------------------------
    -- readBuffer
    -- Reads a block device
    -- @param retBuffer - buffer with the contents of the block device, set as
    --  a busy buffer.
    ---------------------------------------------------------------------------
    procedure readBuffer(device     : in Devices.DeviceID;
                         blockNum   : in Unsigned_64;
                         retBuffer  : in out BufferPtr) with
        Post => retBuffer.busy;

    ---------------------------------------------------------------------------
    -- writeBuffer
    -- Writes the buffer contents to the underlying block device.
    ---------------------------------------------------------------------------
    procedure writeBuffer(buf       : in out BufferPtr) with
        Pre => buf.busy;

    ---------------------------------------------------------------------------
    -- releaseBuffer
    -- Release a busy buffer and make it the most recently used
    ---------------------------------------------------------------------------
    procedure releaseBuffer(buf     : in out BufferPtr) with
        Pre => buf.busy;

    ---------------------------------------------------------------------------
    -- Print block buffer for debugging.
    ---------------------------------------------------------------------------
    --procedure print(buf : in BlockBuffer);

private
    -- Keep a static array of BlockBuffer for now.
    --@TODO allocate these at runtime from Slab Allocator, maybe keep one per
    -- block device.
    cache : FileCache;

end FileCache;
