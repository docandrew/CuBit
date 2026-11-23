-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- @summary
-- One clock on every CPU: IA32_TSC_ADJUST at start-up and a warp check of
-- each secondary CPU against the boot CPU (TSC_Sync_Rules is the decision).
--
-- @description
-- The boot CPU forces a nonzero TSC_ADJUST to 0 (Start_Boot_CPU). Each
-- secondary CPU sets its own to the boot CPU's (one package), then both
-- CPUs run a bounded warp check: the secondary CPU posts a check, the boot
-- CPU joins it from its start-up wait (Serve_Boot_CPU). A warp seen from one
-- side is compensated through TSC_ADJUST once and checked again. A CPU that
-- still disagrees withdraws the shared clock (Clock_Page.Withdraw_Sharing):
-- CPU 0 alone keeps the system's time and user space reads the
-- millisecond word, as on a CPU without an invariant TSC.
--
-- Every wait ends at a named limit on the waiting CPU's own counter.
-------------------------------------------------------------------------------
package TSC_Sync with
    SPARK_Mode => Off -- MSRs, shared spin words, the kernel console
is
    -- The boot CPU, once, after its #GP handler (Msr_Safe) and before the
    -- TSC is calibrated or any clock is anchored on it.
    procedure Start_Boot_CPU;

    -- The boot CPU, from its wait for a secondary CPU to start: join the
    -- warp check that CPU posted, if any.
    procedure Serve_Boot_CPU;

    -- A secondary CPU, interrupts disabled, before its timer starts and
    -- before it signals the boot CPU that it is up.
    procedure Start_Secondary (CPU : Natural);

    -- The boot CPU, after every secondary CPU started: one summary line.
    procedure Report (Secondary_CPUs : Natural);
end TSC_Sync;
