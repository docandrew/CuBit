-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Block device buffer cache
-------------------------------------------------------------------------------

with BlockDevice;
with BuddyAllocator;
with Process;
with Textmode; use Textmode;

package body FileCache with
    SPARK_Mode => Off
is

    ---------------------------------------------------------------------------
    -- setup - initialize the file cache before use
    --
    -- @TODO - keep track of initialization with SPARK ghost variable for
    --  proofs
    ---------------------------------------------------------------------------
    procedure setup with
        SPARK_Mode => Off -- use of Access
    is
        bufDataAddr : Virtmem.PhysAddress;
    begin
        -- link the buffers together.
        cache.head.prev := cache.head'Access;
        cache.head.next := cache.head'Access;
        
        for b in cache.buffers'Range loop
            -- Allocate aligned frame for each buffer.
            BuddyAllocator.alloc(BuddyAllocator.Order(0), bufDataAddr);
            cache.buffers(b).data   := Virtmem.P2V(bufDataAddr);

            cache.buffers(b).next   := cache.head.next;
            cache.buffers(b).prev   := cache.head'Access;
            cache.head.next.prev    := cache.buffers(b)'Access;
            cache.head.next         := cache.buffers(b)'Access;
        end loop;
    end setup;

    ---------------------------------------------------------------------------
    -- getBuffer - given a block device and block address, return a pointer to
    --  the buffer in memory.
    ---------------------------------------------------------------------------
    procedure getBuffer(device      : in Devices.DeviceID;
                        blockNum    : in Unsigned_64;
                        retBuffer   : out BufferPtr) with
        SPARK_Mode => Off
    is
        use Devices; -- "=" operator

        buf : BufferPtr;
    begin
        Spinlock.enterCriticalSection(cache.lock);

        -- This is sort of confusing, since we have "overlapping" loops.
        -- First, see if block is already cached
        TryAgain: loop
            buf := cache.head.next;
            
            -- Iterate through blocks.
            Search: loop
                if buf.device = device and buf.blockNum = blockNum then
                    -- block we want is cached.
                    if not buf.busy then
                        -- sweet. block is cached and nobody owns it
                        retBuffer := buf;
                        Spinlock.exitCriticalSection(cache.lock);
                        
                        return;
                    else
                        -- block is cached, but somebody owns it.
                        -- we'll wait here, then wake up and go
                        -- through the search again.
                        Process.wait(Process.WaitChannel(buf.all'Address),
                                     cache.lock);
                        exit Search;   -- goto TryAgain, effectively.
                    end if;
                end if;

                buf := buf.next;
                -- Block we want isn't cached, try and get it below.
                exit TryAgain when buf = cache.head'Access;
            end loop Search;

        end loop TryAgain;

        -- If we get here, block wasn't cached.
        buf := cache.head.next;
        CacheIt: loop
            if not buf.busy and not buf.dirty then
                buf.device      := device;
                buf.blockNum    := blockNum;
                buf.busy        := True;
                buf.dirty       := False;
                buf.valid       := False;
                retBuffer       := buf;
                Spinlock.exitCriticalSection(cache.lock);
                return;
            end if;

            buf := buf.next;
            exit CacheIt when buf = cache.head'Access;
        end loop CacheIt;

        raise NoBuffersException with "getBlock: No free buffers";
    end getBuffer;


    -- Return a busy buffer with disk contents
    procedure readBuffer(device     : in Devices.DeviceID;
                         blockNum   : in Unsigned_64;
                         retBuffer  : in out BufferPtr) with 
        SPARK_Mode => Off
    is
    begin
        getBuffer(device, blockNum, retBuffer);
        
        if not retBuffer.valid then
            BlockDevice.syncBuffer(retBuffer);
        end if;
    end readBuffer;


    -- Write a buffer's contents to disk
    procedure writeBuffer(buf : in out BufferPtr) with
        SPARK_Mode => Off
    is
    begin
        if not buf.busy then
            raise WriteBlockNotOwnedException with "writeBuffer: Writing non-busy buffer";
        end if;

        buf.dirty := True;
        BlockDevice.syncBuffer(buf); 
    end writeBuffer;


    -- Release a busy buffer
    procedure releaseBuffer(buf : in out BufferPtr) with
        SPARK_Mode => Off
    is
    begin
        if not buf.busy then
            raise ReleaseNotOwnedException with "releaseBuffer: Releasing non-busy buffer";
        end if;

        Spinlock.enterCriticalSection(cache.lock);

        buf.next.prev := buf.prev;
        buf.prev.next := buf.next;
        buf.next := cache.head.next;  -- most recently used
        buf.prev := cache.head'Access;
        
        cache.head.next.prev := buf;
        cache.head.next := buf;

        buf.busy := False;

        -- Wake up any processes waiting on this buffer.
        Process.goAhead(buf.all'Address);

        Spinlock.exitCriticalSection(cache.lock);
    end releaseBuffer;

end FileCache;
