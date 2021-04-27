-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Entry Point / Main routine
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Multiboot; use Multiboot;

package kmain with
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- Our kernel's Ada entry point. Jumped to from boot.asm
    ---------------------------------------------------------------------------
    procedure kmain(magic : Unsigned_32; mbInfo_orig : in MultibootInfo)
        with Export => True, Convention => C, External_Name => "kmain";

    ---------------------------------------------------------------------------
    -- Application Processor (AP) entry point, aka SMP entry point
    ---------------------------------------------------------------------------
    procedure apEnter(cpuNum : in Unsigned_32)
        with Export => True, Convention => C, External_Name => "apEnter";
    Pragma No_Return (apEnter);

    ---------------------------------------------------------------------------
    -- When we boot the other cores, we set this value so the CPU knows what
    -- CPU number they are
    ---------------------------------------------------------------------------
    startingCPU : Unsigned_32
        with Export => True, External_Name => "startingCPU";
    
end kmain;