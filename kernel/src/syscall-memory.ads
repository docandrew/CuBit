-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall memory handlers: MAP_DEVICE, ALLOC_DMA, MAP_INTO, SBRK, MAPFB.
-------------------------------------------------------------------------------
with Process;

package Syscall.Memory with
    SPARK_Mode => Off
is

    -- MAP_DEVICE: arg4 = the CAP_DEVICE_MEM handle covering the pages:
    -- device memory (untagged) maps uncached, firmware memory (the ACPI
    -- hardware grant's NVS runs and FACS page) write-back, and PCI segment
    -- 0 bus 0 configuration (the grant's ECAM slice) uncached, read-only.
    -- Through a platform-pool capability (Platform_Pool_ABI): mapped
    -- uncached inside one pool interval clear of every claim, arg5 = the
    -- address of a refusal record to fill (0: none); 0 or a refusal code.
    procedure handleMapDevice (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
                               retval    : out Unsigned_64);

    -- ALLOC_DMA: arg3 = the mode word (shared/launch DMA_Allocation_ABI),
    -- arg5 = a process handle with RIGHT_GRANT for the target, arg6 (a
    -- device-bound request only) = the caller's CAP_IOPORT covering the PCI
    -- configuration mechanism, the authority to name a PCI function's IOMMU
    -- domain (IOMMU-001).
    procedure handleAllocDma (callerPID : Process.ProcessID;
                              arg0, arg1, arg2, arg3, arg4, arg5, arg6 : Unsigned_64;
                              retval    : out Unsigned_64);

    -- MAP_INTO: arg5 = a process handle with RIGHT_GRANT for the target,
    -- arg6 = the CAP_DEVICE_MEM handle (device kind only).
    procedure handleMapInto (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5, arg6 : Unsigned_64;
                             retval    : out Unsigned_64);

    procedure handleSbrk (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64);

    -- MAPFB: arg0 = the CAP_DEVICE_MEM handle covering the framebuffer.
    procedure handleMapFB (callerPID : Process.ProcessID;
                           arg0      : Unsigned_64;
                           retval    : out Unsigned_64);
end Syscall.Memory;
