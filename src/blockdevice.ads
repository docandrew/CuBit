-------------------------------------------------------------------------------
-- 
-- @summary Block Device synchronization and 
-------------------------------------------------------------------------------

limited with BufferCache;
with Device;

package BlockDevice
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Block Driver list is just an array of subprogram pointers to call the
    -- appropriate block synchronization method for the device type 
    -- (major number) being accessed.
    ---------------------------------------------------------------------------
    type BlockDriverList is array (Device.MajorNumber'Range) of access procedure (buf : BufferCache.BufferPtr);

    ---------------------------------------------------------------------------
    -- registerBlockDriver
    -- Specify the driver to use for a particular category (major number) of
    -- block device. This should be performed as part of the driver's setup
    -- subprogram.
    ---------------------------------------------------------------------------
    procedure registerBlockDriver(major : Device.MajorNumber;
                                  bufSyncFunc : access procedure(buf : BufferCache.BufferPtr));

    ---------------------------------------------------------------------------
    -- syncBuffer
    -- Dispatches the buffer to the appropriate device driver.
    ---------------------------------------------------------------------------
    procedure syncBuffer(buf : BufferCache.BufferPtr);

end BlockDevice;
