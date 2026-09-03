-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Query mechanism. This is a way for user-space drivers to get information
-- from the kernel.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;

package Sysinfo is

    -- Query Categories, Details
    subtype QueryID is Unsigned_64;

    MAGIC_RAMDISK_ADDRESS : constant QueryID := 1000;
    SECONDARY_STACK_START : constant QueryID := 1001;
    RAMDISK_SIZE          : constant QueryID := 1002;

    -- Framebuffer info queries
    FB_WIDTH              : constant QueryID := 1100;
    FB_HEIGHT             : constant QueryID := 1101;
    FB_PITCH              : constant QueryID := 1102;
    FB_BPP                : constant QueryID := 1103;

    -- Network device info
    NET_IOBASE            : constant QueryID := 1200;

    -- NVMe device info
    NVME_BAR0             : constant QueryID := 1300;
    NVME_DMA_PHYS         : constant QueryID := 1301;

    -- HDA device info
    HDA_BAR0              : constant QueryID := 1500;
    HDA_DMA_PHYS          : constant QueryID := 1501;

    -- VirtIO-GPU device info
    GPU_BAR0              : constant QueryID := 1700;
    GPU_DMA_PHYS          : constant QueryID := 1701;
    GPU_COMMON_OFF        : constant QueryID := 1702;
    GPU_NOTIFY_OFF        : constant QueryID := 1703;
    GPU_ISR_OFF           : constant QueryID := 1704;
    GPU_DEVICE_OFF        : constant QueryID := 1705;
    GPU_NOTIFY_MULT       : constant QueryID := 1706;
    GPU_IS_PRIMARY        : constant QueryID := 1707;

    -- CPU info
    NUM_CPUS              : constant QueryID := 1400;

    -- Memory info
    MEM_FREE              : constant QueryID := 1600;
    MEM_TOTAL             : constant QueryID := 1601;

    REGISTERED_DRIVER     : constant QueryID := 2000;

    subtype DriverID is QueryID range 0..127;

    DRIVER_NULL     : constant DriverID := 0;
    DRIVER_KEYBOARD : constant DriverID := 1;
    DRIVER_ATA      : constant DriverID := 2;
    DRIVER_NETSTACK : constant DriverID := 3;
    DRIVER_PROCMGR  : constant DriverID := 4;
    DRIVER_NVME     : constant DriverID := 5;
    DRIVER_FS       : constant DriverID := 6;
    DRIVER_DEVMGR   : constant DriverID := 7;
    DRIVER_HDA      : constant DriverID := 8;
    DRIVER_MIXER    : constant DriverID := 9;
    DRIVER_MOUSE    : constant DriverID := 10;
    DRIVER_CONFIG   : constant DriverID := 11;
    DRIVER_NETMGR   : constant DriverID := 12;
    DRIVER_LOGSTORE : constant DriverID := 13;
    DRIVER_IPCTEST  : constant DriverID := 14;
    DRIVER_DESKTOP  : constant DriverID := 15;
    DRIVER_DISPLAY  : constant DriverID := 16;
    DRIVER_GPU      : constant DriverID := 17;
    DRIVER_CCL_TEST : constant DriverID := 18;
    DRIVER_CLOCK    : constant DriverID := 19;

    -- List of processes registered as a particular driver.
    type DriverList is array (DriverID) of Process.ProcessID;

    ---------------------------------------------------------------------------
    -- getInfo
    -- Request information from the kernel from userspace.
    ---------------------------------------------------------------------------
    function getInfo (query  : Unsigned_64;
                      detail : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- registerDriver
    -- Register a user-space process as a driver.
    -- @param pid - Process ID of the user-space process
    -- @param driver - unique identifier for the device class.
    ---------------------------------------------------------------------------
    function registerDriver (pid    : Process.ProcessID;
                             driver : DriverID) return Unsigned_64
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- setNetIOBase
    -- Store the BAR0 I/O base for the network device (for driver queries).
    ---------------------------------------------------------------------------
    procedure setNetIOBase (ioBase : Unsigned_64)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- setNvmeInfo
    -- Store the BAR0 and DMA physical addresses for the NVMe device.
    ---------------------------------------------------------------------------
    procedure setNvmeInfo (bar0 : Unsigned_64; dmaPhys : Unsigned_64)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- unregisterDriverByPID
    -- Remove all driver registrations belonging to a given process.
    -- Called during process kill().
    ---------------------------------------------------------------------------
    procedure unregisterDriverByPID (pid : Process.ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- setInfo
    -- Set a sysinfo value from userspace (via SYSCALL_SET_SYSINFO).
    -- Only allows writable query IDs; returns True on success.
    ---------------------------------------------------------------------------
    function setInfo (queryID : Unsigned_64;
                      value   : Unsigned_64) return Boolean
        with SPARK_Mode => On;

end Sysinfo;
