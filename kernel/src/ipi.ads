-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2024 Jon Andrew
--
-- @summary Inter-Processor Interrupt (IPI) support
--
-- @description
-- Standalone package for sending reschedule IPIs to other CPUs.
-- Dependency-free (no Process or Interrupts) to avoid circular deps.
-------------------------------------------------------------------------------
with System;

package IPI with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- init
    -- Store the LAPIC base address for IPI delivery.
    -- Must be called once by BSP after LAPIC setup.
    ---------------------------------------------------------------------------
    procedure init (lapicBase : in System.Address);

    ---------------------------------------------------------------------------
    -- sendReschedule
    -- Send a reschedule IPI (vector 249) to the given CPU.
    -- Used by ready() when a process is enqueued on a remote CPU's list.
    ---------------------------------------------------------------------------
    procedure sendReschedule (targetCPU : in Natural);

end IPI;
