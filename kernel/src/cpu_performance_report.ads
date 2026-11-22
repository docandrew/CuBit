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
-- effective MHz from a dependent-add chain timed against the calibrated
-- TSC (any x86, QEMU included), plus the CPUID facts (turbo, HWP, EPP,
-- APERF/MPERF). It reads no MSR: on the N150 one HWP MSR read raised #GP
-- although CPUID listed HWP (image 27), so MSR state waits for a reader
-- that recovers from #GP (PERF-013).
-------------------------------------------------------------------------------
package CPU_Performance_Report with SPARK_Mode => Off is
    -- BSP, after Time.calibrateTSC, interrupts disabled.
    procedure Report;
end CPU_Performance_Report;
