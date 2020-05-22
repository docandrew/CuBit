-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Block device buffer cache
-------------------------------------------------------------------------------
with System.Address_To_Access_Conversions;

package body BufferCache with
    SPARK_Mode => Off
is

    package ToAddr is new System.Address_To_Access_Conversions(Buffer);

    procedure setup with
        SPARK_Mode => Off -- use of access
    is
    begin
        -- link the buffers together.
        cache.head.prev := cache.head'Access;
        cache.head.next := cache.head'Access;
        
        for b of cache.buffers loop
            b.next                  := cache.head.next;
            b.prev                  := cache.head'Access;
            cache.head.next.prev    := b'Access;
            cache.next              := b'Access;
        end loop;
    end setup;


    procedure getBuffer(device      : in Unsigned_32; 
                        blockNum    : in Unsigned_64;
                        retBuffer   : out BufferPtr) with
        SPARK_Mode => Off
    is
        buf : BufferPtr;
    begin
        enterCriticalSection(cache.lock);

        -- This is sort of confusing, since we have "overlapping" loops.
        -- First, see if block is already cached
        TryAgain: loop
            buf := cache.head.next;
            
            -- Iterate through blocks.
            Search: loop
                if buf.device = dev and buf.blockNum = blockNum then
                    -- block we want is cached.
                    if not buf.busy then
                        -- sweet. block is cached and nobody owns it
                        retBuffer := buf;
                        exitCriticalSection(cache.lock);
                        return;
                    else
                        -- block is cached, but somebody owns it.
                        -- we'll wait here, then wake up and go
                        -- through the search again.
                        Process.wait(Process.WaitChannel(buf.all'Address),
                                     cache.lock);
                        exit loop Search;   -- goto TryAgain, effectively.
                    end if;
                end if;

                buf := buf.next;
                -- Block we want isn't cached, try and get it below.
                exit loop TryAgain when buf = cache.head;
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
                exitCriticalSection(cache.lock);
                return;
            end if;

            buf := buf.next;
            exit loop CacheIt when buf = cache.head;
        end loop CacheIt;

        raise NoBufferException with "getBlock: No free buffers";
    end getBuffer;


    -- Return a busy buffer with disk
    procedure readBuffer(device     : in Unsigned_32;
                         blockNum   : in Unsigned_64;
                         retBuffer  : out BufferPtr) with 
        SPARK_Mode => Off
    is
    begin
        getBuffer(device, blockNum, retBuffer);
        
        if not retBuffer.valid then
            --@TODO: find a place to determine, based on the device ID,
            -- which underlying syncBuffer function (ata, atapi, etc.) to
            -- call. We'll want a device registration package or something.
            syncBuffer(buf);
        end if;
    end readBuffer;


    -- Write a buffer's contents to disk
    procedure writeBuffer(buf : in out BufferPtr) with
        SPARK_Mode => Off
    is
    begin
        if not buf.busy
            raise WriteBlockNotOwnedException with "writeBuffer: Writing non-busy buffer";
        end if;

        buf.dirty := True;
        syncBuffer(buf); 
    end writeBuffer;


    -- Release a busy buffer
    procedure releaseBuffer(buf : in out BufferPtr) with
        SPARK_Mode => Off
    is
    begin
        if not buf.busy
            raise ReleaseNotOwnedException with "releaseBuffer: Releasing non-busy buffer";
        end if;

        enterCriticalSection(cache.lock);

        buf.next.prev := buf.prev;
        buf.prev.next := buf.next;
        buf.next := buf.head.next;  -- most recently used
        buf.prev := buf.head'Access;
        buf.head.next.prev := buf;
        buf.head.next := buf;

        buf.busy := False;

        -- Wake up any processes waiting on this buffer.
        Process.goAhead(buf.all'Address);

        exitCriticalSection(cache.lock);
    end releaseBuffer;

end BlockDevice;
