-------------------------------------------------------------------------------
-- 
-- @summary Block Device synchronization and 
-------------------------------------------------------------------------------

with BufferCache;
with Devices;

package BlockDevice
    with SPARK_Mode => On
is
    type BufferSyncFunction is access procedure(buf : in out BufferCache.BufferPtr);

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
    procedure registerBlockDriver(major : Devices.MajorNumber;
                                  bufSyncFunc : BufferSyncFunction);

    ---------------------------------------------------------------------------
    -- syncBuffer
    -- Dispatches the buffer to the appropriate device driver.
    ---------------------------------------------------------------------------
    procedure syncBuffer(buf : in out BufferCache.BufferPtr);

end BlockDevice;
