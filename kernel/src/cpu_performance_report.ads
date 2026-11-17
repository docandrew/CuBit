-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Boot report of the CPU's performance state (PERF-013).
--
-- @description CuBit does not yet manage CPU frequency (PWR-002): no HWP
-- request is written, so the core runs at whatever the firmware left. This
-- one-line boot report shows how fast the boot CPU actually runs, so a
-- weak machine's latency can be read against its clock before any policy:
--  - effective MHz from a dependent-add chain timed against the calibrated
--    TSC (any x86, QEMU included);
--  - APERF/MPERF over the same loop, when CPUID.06H:ECX[0] reports them;
--  - IA32_PERF_STATUS's current ratio, when CPUID.01H:ECX[7] (EST);
--  - HWP enable, capabilities and request, when CPUID.06H:EAX[7] (HWP).
-- Each MSR is read only behind the CPUID bit that makes it architectural,
-- so a CPU or hypervisor without it never takes a #GP here. Read-only: it
-- writes no MSR.
-------------------------------------------------------------------------------
package CPU_Performance_Report with SPARK_Mode => Off is
    -- BSP, after Time.calibrateTSC, interrupts disabled.
    procedure Report;
end CPU_Performance_Report;
