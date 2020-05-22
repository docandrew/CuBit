-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Buffer Cache
-- @description This package implements the traditional UNIX model of buffer
--  caches. Buffered blocks can either be BUSY (reserved by a process),
--  VALID (holding up-to-date data from disk), or DIRTY (modified by a process,
--  needs to be written to disk).
-------------------------------------------------------------------------------
with LinkedList;
with Spinlock;
with x86;

Pragma Elaborate_All (Spinlock);

generic with
    BlockSize   : Positive;
package BufferCache with
    SPARK_Mode => On
is
    NoBuffersException : exception;

    type Block is array (0 .. BlockSize - 1) of Unsigned_8;

    type BufferPtr;
  
    ---------------------------------------------------------------------------
    -- In-memory record for a disk block. Note that if this is a file system
    -- block, data should be an integer multiple of the disk sector size.
    --
    -- @field syncNext points to the next buffer that should be synced to/from
    --  the underlying block device.
    ---------------------------------------------------------------------------
    type Buffer is record
        busy        : Boolean;
        valid       : Boolean;
        dirty       : Boolean;

        device      : Unsigned_32 := -1;
        blockNumber : Unsigned_64;
        lock        : spinlock.Spinlock;
        refCount    : Natural;
        
        data        : Block;
        
        prev        : BufferPtr;
        next        : BufferPtr;
        syncNext    : BufferPtr;
    end record;

    type BufferPtr is access Buffer;

    type BufferArray is array (0 .. NUM_BLOCK_BUFFERS) of Buffer;
    
    -- Each block device gets one of these. Maintain the static array,
    -- LRU list of pointers to the blocks and a mutex.
    type BufferCache is record
        lock        : Spinlock.spinlock;
        buffers     : BufferArray;

        -- last used is head.next
        head        : Buffer;
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
    procedure readBuffer(device     : in Unsigned_32;
                         blockNum   : in Unsigned_64;
                         retBuffer  : out BufferPtr) with
        Post => retBuffer.busy;

    ---------------------------------------------------------------------------
    -- writeBuffer
    -- Writes the buffer contents to the underlying block device.
    ---------------------------------------------------------------------------
    procedure writeBuffer(buf : in out BufferPtr) with
        Pre => buf.busy;

    ---------------------------------------------------------------------------
    -- releaseBuffer
    -- Release a busy buffer and make it the most recently used
    ---------------------------------------------------------------------------
    procedure releaseBuffer(buf : in out BufferPtr) with
        Pre => buf.busy;

    ---------------------------------------------------------------------------
    -- Print block buffer for debugging.
    ---------------------------------------------------------------------------
    --procedure print(buf : in BlockBuffer);

private
    -- Keep a static array of BlockBuffer for now.
    --@TODO allocate these at runtime from Slab Allocator, maybe keep one per
    -- block device.
    cache : BufferCache;

end BufferCache;
