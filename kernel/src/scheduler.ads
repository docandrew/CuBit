-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- System Scheduler - the scheduler runs on each CPU, trying to fetch 
-- processes to run in a round-robin manner.
-------------------------------------------------------------------------------
with PerCPUData;
with Process;
with Spinlocks;

package Scheduler with
    --Abstract_State => (SchedulerState),
    SPARK_Mode => On
is
    SchedulerException : exception;
    
    ---------------------------------------------------------------------------
    -- enter:
    --
    -- Procedure to enter the scheduler from user mode.
    ---------------------------------------------------------------------------
    procedure enter with
        Pre => Spinlocks.isLocked(process.lock);

    ---------------------------------------------------------------------------
    -- schedule
    --
    -- This is the main "event loop" running on each CPU.
    --
    -- Choose a new process to execute. It should pick the highest priority
    --  READY process in proctab. Of those processes with equal priority, the
    --  scheduler will use FIFO mechanics, choosing the process which was last
    --  run.
    ---------------------------------------------------------------------------
    procedure schedule(cpuData : in out PerCPUData.PerCPUData) with
        Global => (In_Out => process.proctab);
    pragma No_Return (schedule);

end Scheduler;