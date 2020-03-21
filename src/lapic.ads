-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Local APIC - Advanced Programmable Interrupt Controller
--
-- template param baseAddr - physical base address of the LAPIC registers,
--  typically 0xFEE0_0000, but may be elsewhere depending on BIOS/EFI.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with InterruptNumbers;
with Virtmem;

generic
    baseAddr : System.Address;
package lapic 
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setupLAPIC_BSP
    -- Perform per-system LAPIC initialization.
    --
    -- Maps the LAPIC registers into the linear memory range, and performs
    -- the timer calibration. This function calls setupLAPIC_AP after timer
    -- calibration to enable LAPIC interrupts on the BSP. This function will
    -- re-map the APIC table as an I/O page, with writethrough and no caching.
    ---------------------------------------------------------------------------
    procedure setupLAPIC_BSP;
    
    ---------------------------------------------------------------------------
    -- setupLAPIC_AP
    -- Perform per-CPU LAPIC initialization. Sets up the LAPIC timer and
    -- enables LAPIC interrupts.
    ---------------------------------------------------------------------------
    procedure setupLAPIC_AP;

    ---------------------------------------------------------------------------
    -- finishIRQ - Acknowledge the interrupt with an end-of-interrupt write to
    --  the eoi register.
    -- @param irqnum - interrupt vector number
    ---------------------------------------------------------------------------
    procedure finishIRQ;

    ---------------------------------------------------------------------------
    -- sendIPI - Send an inter-processor interrupt to another CPU
    -- @param cpuNum - the Local APIC ID of the destination CPU
    -- @param intCommand - value for the lower 32-bits of the interrupt
    --  command register.
    ---------------------------------------------------------------------------
    procedure sendIPI(cpuNum : in Unsigned_8; intCommand : in Unsigned_32);

    ---------------------------------------------------------------------------
    -- bootAP - send the necessary IPI sequence to an AP to tell it to boot.
    -- @param cpuNum - CPU number to start, not 0. (TODO: can our BSP ever be
    --  non-zero?)
    -- @param startVector - page-aligned address in lower 64k that the AP will
    --  begin executing at (typically 0x7000).
    ---------------------------------------------------------------------------
    procedure bootAP(cpuNum : in Unsigned_8; startAddr : in Unsigned_16) with
        Pre => cpuNum /= 0;

-- private
--     package LAPICRegisters is
--         procedure setupRegisters(lapicBase : in virtmem.VirtAddress);
--     end LAPICRegisters;

end lapic;