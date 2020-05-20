-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2020 Jon Andrew
--
-- @summary Block Device buffer cache
-------------------------------------------------------------------------------
package body BlockDevice with
    SPARK_Mode => On
is

    procedure setup with
        SPARK_Mode => On
    is
    begin
        BufferList.setup(cache.buffers, Config.NUM_BLOCK_BUFFERS);
    end setup;


    procedure getBuffer(dev : in DeviceID; blockNum : in Unsigned_64) with
        SPARK_Mode => On
    is
    begin
        enterCriticalSection(cache.lock);

        -- If the block is already in the cache   
         
    end getBuffer;

    procedure readBuffer()

end BlockDevice;
