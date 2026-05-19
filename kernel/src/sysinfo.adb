-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- Query mechanism. This is a way for user-space drivers to get information
-- from the kernel.
-------------------------------------------------------------------------------
with System.Storage_Elements;

with acpi;
with BuddyAllocator;
with Modules;
with TextIO; use TextIO;
with Util;
with Video.VGA;

package body Sysinfo is

    registeredDrivers : DriverList := (others => Process.NO_PROCESS)
        with Volatile;
    netIOBase : Unsigned_64 := 0;
    nvmeBar0  : Unsigned_64 := 0;
    nvmeDma   : Unsigned_64 := 0;
    hdaBar0   : Unsigned_64 := 0;
    hdaDma    : Unsigned_64 := 0;
    gpuBar0   : Unsigned_64 := 0;
    gpuDma    : Unsigned_64 := 0;
    gpuCommonOff : Unsigned_64 := 0;
    gpuNotifyOff : Unsigned_64 := 0;
    gpuIsrOff    : Unsigned_64 := 0;
    gpuDeviceOff : Unsigned_64 := 0;
    gpuNotifyMult : Unsigned_64 := 0;
    gpuIsPrimary : Unsigned_64 := 0;
    
    ---------------------------------------------------------------------------
    -- getInfo
    ---------------------------------------------------------------------------
    function getInfo (query  : Unsigned_64;
                      detail : Unsigned_64) return Unsigned_64
        with SPARK_Mode => On
    is
    begin
        case query is
            when MAGIC_RAMDISK_ADDRESS =>
                -- println ("Sysinfo: received query for magic ramdisk address");
                return Util.addrToNum (Modules.MAGIC_RAMDISK_ADDRESS);
            when SECONDARY_STACK_START =>
                -- println ("Sysinfo: received query for secondary stack start");
                return Util.addrToNum (Process.SECONDARY_STACK_START);
            when RAMDISK_SIZE =>
                return Unsigned_64 (Modules.MAGIC_RAMDISK_SIZE);
            when FB_WIDTH =>
                return Unsigned_64(Video.VGA.w);
            when FB_HEIGHT =>
                return Unsigned_64(Video.VGA.h);
            when FB_PITCH =>
                return Unsigned_64(Video.VGA.framebufferPitch);
            when FB_BPP =>
                return Unsigned_64(Video.VGA.framebufferDepth);
            when NET_IOBASE =>
                return netIOBase;
            when NVME_BAR0 =>
                return nvmeBar0;
            when NVME_DMA_PHYS =>
                return nvmeDma;
            when HDA_BAR0 =>
                return hdaBar0;
            when HDA_DMA_PHYS =>
                return hdaDma;
            when GPU_BAR0 =>
                return gpuBar0;
            when GPU_DMA_PHYS =>
                return gpuDma;
            when GPU_COMMON_OFF =>
                return gpuCommonOff;
            when GPU_NOTIFY_OFF =>
                return gpuNotifyOff;
            when GPU_ISR_OFF =>
                return gpuIsrOff;
            when GPU_DEVICE_OFF =>
                return gpuDeviceOff;
            when GPU_NOTIFY_MULT =>
                return gpuNotifyMult;
            when GPU_IS_PRIMARY =>
                return gpuIsPrimary;
            when NUM_CPUS =>
                return Unsigned_64 (acpi.numCPUs);
            when MEM_FREE =>
                return Unsigned_64 (BuddyAllocator.getFreeBytes);
            when MEM_TOTAL =>
                return Unsigned_64 (BuddyAllocator.getTotalBytes);
            when REGISTERED_DRIVER =>
                return Unsigned_64(
                    registeredDrivers(DriverID(detail)));
            when others =>
                return Unsigned_64'Last;
        end case;
    end getInfo;

    ---------------------------------------------------------------------------
    -- registerDriver
    ---------------------------------------------------------------------------
    function registerDriver (pid    : Process.ProcessID;
                             driver : DriverID) return Unsigned_64
        with SPARK_Mode => On
    is
    begin
        registeredDrivers (driver) := pid;
        print ("sysinfo: registered drv ");
        printd (Unsigned_64(driver));
        print (" = pid ");
        printd (Unsigned_64(pid));
        println;
        return Unsigned_64(pid);
    end registerDriver;

    ---------------------------------------------------------------------------
    -- unregisterDriverByPID
    ---------------------------------------------------------------------------
    procedure unregisterDriverByPID (pid : Process.ProcessID)
        with SPARK_Mode => On
    is
    begin
        for d in DriverID loop
            if registeredDrivers(d) = pid then
                registeredDrivers(d) := Process.NO_PROCESS;
            end if;
        end loop;
    end unregisterDriverByPID;

    ---------------------------------------------------------------------------
    -- setNetIOBase
    ---------------------------------------------------------------------------
    procedure setNetIOBase (ioBase : Unsigned_64)
        with SPARK_Mode => On
    is
    begin
        netIOBase := ioBase;
    end setNetIOBase;

    ---------------------------------------------------------------------------
    -- setNvmeInfo
    ---------------------------------------------------------------------------
    procedure setNvmeInfo (bar0 : Unsigned_64; dmaPhys : Unsigned_64)
        with SPARK_Mode => On
    is
    begin
        nvmeBar0 := bar0;
        nvmeDma  := dmaPhys;
    end setNvmeInfo;

    ---------------------------------------------------------------------------
    -- setInfo
    ---------------------------------------------------------------------------
    function setInfo (queryID : Unsigned_64;
                      value   : Unsigned_64) return Boolean
        with SPARK_Mode => On
    is
    begin
        case queryID is
            when NVME_BAR0 =>
                nvmeBar0 := value;
                return True;
            when NVME_DMA_PHYS =>
                nvmeDma := value;
                return True;
            when HDA_BAR0 =>
                hdaBar0 := value;
                return True;
            when HDA_DMA_PHYS =>
                hdaDma := value;
                return True;
            when NET_IOBASE =>
                netIOBase := value;
                return True;
            when GPU_BAR0 =>
                gpuBar0 := value;
                return True;
            when GPU_DMA_PHYS =>
                gpuDma := value;
                return True;
            when GPU_COMMON_OFF =>
                gpuCommonOff := value;
                return True;
            when GPU_NOTIFY_OFF =>
                gpuNotifyOff := value;
                return True;
            when GPU_ISR_OFF =>
                gpuIsrOff := value;
                return True;
            when GPU_DEVICE_OFF =>
                gpuDeviceOff := value;
                return True;
            when GPU_NOTIFY_MULT =>
                gpuNotifyMult := value;
                return True;
            when GPU_IS_PRIMARY =>
                gpuIsPrimary := value;
                return True;
            when RAMDISK_SIZE =>
                Modules.MAGIC_RAMDISK_SIZE :=
                    System.Storage_Elements.Storage_Count (value);
                return True;
            when MAGIC_RAMDISK_ADDRESS =>
                Modules.MAGIC_RAMDISK_ADDRESS :=
                    System.Storage_Elements.To_Address (
                        System.Storage_Elements.Integer_Address (value));
                return True;
            when others =>
                return False;
        end case;
    end setInfo;

end Sysinfo;
