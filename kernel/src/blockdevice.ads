-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- CuBitOS 
--
-- @summary
-- Block Devices
--
-- @description
-- This package abstracts underlying block devices and the function used to
-- synchronize their cached file data to/from disk or other device. Each driver
-- will register their BufferSyncFunction with this package. When a
-- block for that driver/device is dirty and needs to be written, this package
-- will call the registered function and synchronize it to disk.
--
-- Later on, we should expect work on an I/O scheduler to begin here as well.
-------------------------------------------------------------------------------

with FileCache;
with Devices;

package BlockDevice
    with SPARK_Mode => On
is
    type BufferSyncFunction is access procedure(buf : in out FileCache.BufferPtr);

    ---------------------------------------------------------------------------
    -- Block Driver list is just an array of subprogram pointers to call the
    -- appropriate block synchronization method for the device type 
    -- (major number) being accessed.
    ---------------------------------------------------------------------------
    type BlockDriverList is array (Devices.MajorNumber'Range) of BufferSyncFunction;

    ---------------------------------------------------------------------------
    -- registerBlockDriver
    -- Specify the driver to use for a particular category (major number) of
    -- block device. This should be performed as part of the driver's setup
    -- subprogram.
    ---------------------------------------------------------------------------
    procedure registerBlockDriver(major       : Devices.MajorNumber;
                                  bufSyncFunc : BufferSyncFunction);

    ---------------------------------------------------------------------------
    -- syncBuffer
    -- Dispatches the buffer to the appropriate device driver.
    ---------------------------------------------------------------------------
    procedure syncBuffer(buf : in out FileCache.BufferPtr);

end BlockDevice;
