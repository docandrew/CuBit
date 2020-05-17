with LinkedList;
with Spinlock;
with x86;

Pragma Elaborate_All (Spinlock);

generic
    with BlockSize : Positive;
package BlockDevice with
    SPARK_Mode => On
is
    package BufferList is new LinkedList(BlockBuffer, print);

    type DeviceID is new Unsigned_32;

    type BlockFlags is (VALID, DIRTY, BUSY);

    type BlockData is array (0 .. BlockSize - 1) of Unsigned_8;

    ---------------------------------------------------------------------------
    -- In-memory cache for disk blocks. Note that this is a file-system block,
    -- which should be an integer multiple of the disk sector size.
    ---------------------------------------------------------------------------
    type BlockBuffer is record
        flags       : BlockFlags
        device      : DeviceID;
        blockNumber : Unsigned_64;
        lock        : spinlock.Spinlock;
        refCount    : Natural;
        data        : BlockData;
    end record;

    -- Each block device gets one of these. This is a linked list of BlockBuffers
    -- that will be synchronized to/from disk.
    type BlockDeviceCache is record
        lock        : spinlock.Spinlock;
        buffers     : BufferList.List(BlockBuffer, Config.NUM_BLOCK_BUFFERS); 
    end record;

    ---------------------------------------------------------------------------
    -- Setup
    ---------------------------------------------------------------------------
    procedure setup(cache : BlockDeviceCache);

    ---------------------------------------------------------------------------
    -- Print block buffer for debugging. Necessary for LinkedList
    -- instantiation.
    ---------------------------------------------------------------------------
    procedure print(buf : in BlockBuffer);

end BlockDevice;
