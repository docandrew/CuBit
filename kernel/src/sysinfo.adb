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
with Modules;
with TextIO; use TextIO;
with Util;
with Video.VGA;

package body Sysinfo is

    registeredDrivers : DriverList := (others => Process.NO_PROCESS);
    netIOBase : Unsigned_64 := 0;
    nvmeBar0  : Unsigned_64 := 0;
    nvmeDma   : Unsigned_64 := 0;
    hdaBar0   : Unsigned_64 := 0;
    hdaDma    : Unsigned_64 := 0;
    
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
            when NUM_CPUS =>
                return Unsigned_64 (acpi.numCPUs);
            when REGISTERED_DRIVER =>
                return Unsigned_64(registeredDrivers(DriverID(detail)));
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
        return Unsigned_64(pid);
    end registerDriver;

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
