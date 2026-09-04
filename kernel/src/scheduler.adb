-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- System Scheduler
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Mem_mgr;
with Process.Queues;
with TextIO; use TextIO;
with Trace;
with x86;

package body Scheduler with
    -- Refined_State => (
    --     SchedulerState => ()),
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Enter the scheduler from a process.
    -- Checks the kernel stack canary before context switching.
    ---------------------------------------------------------------------------
    procedure enter with
        SPARK_Mode => On
    is
        use type Process.ProcessMode;
        use type Process.ProcessState;
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        -- Check kernel stack canary before context switch
        checkCanary : declare
            use type Process.ProcessKernelStackPtr;
            cpuData : PerCPUData.PerCPUData with Import, Volatile, Address => perCPUAddr;
            pid : constant Process.ProcessID := cpuData.currentPID;
        begin
            if pid /= Process.NO_PROCESS and then
               Process.proctab(pid).kernelStack /= null and then
               Process.proctab(pid).kernelStack.canary /= Process.KSTACK_CANARY
            then
                raise SchedulerException with
                    "Kernel stack overflow detected (canary corrupted)";
            end if;
        end checkCanary;

        -- Eagerly preserve user FP/SIMD state before leaving the process.
        -- Kernel scheduler code is compiled without MMX/SSE, so no restore is
        -- needed until immediately before the next user process runs.
        saveFPU : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
            pid : constant Process.ProcessID := cpuData.currentPID;
        begin
            if pid /= Process.NO_PROCESS and then
               Process.proctab(pid).state /= Process.INVALID and then
               Process.proctab(pid).mode = Process.USER
            then
                Process.saveFPUState (pid);
            end if;
        end saveFPU;

        getCPUContext: declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            Process.switch (cpuData.oldContext'Address, cpuData.schedulerContext);
        end getCPUContext;
    end enter;

    ---------------------------------------------------------------------------
    -- schedule
    -- @NOTE This process is only _called_ once per CPU, at bootup.
    -- Future entries back into this function are through the enter
    --  procedure which means we'll pick up where we left off,
    --  trying to run the next process in the proctab.
    ---------------------------------------------------------------------------
    procedure schedule (cpuData : in out PerCPUData.PerCPUData) with
        SPARK_Mode => On
    is
        use Spinlocks;
        use Process;

        pid : ProcessID;
        ign : ProcessID;
        runStartTSC : Unsigned_64;
    begin

        startSearch : loop
            x86.sti;

            if x86.panicked then
                x86.cli;
                x86.halt;
            end if;

            -- @NOTE This lock is released either by process.start (if this is the process'
            --  first time executing); process.yield (if the process is continuing from the 
            --  call to scheduler.enter from the last time it yielded); or at the symmetric
            --  exitCriticalSection call in this function.
            --
            -- The process must release the lock when it executes, and reacquire it before
            -- coming back here to the scheduler.
            --
            -- println ("Scheduler.schedule: acquiring proctab lock");
            enterCriticalSection (Process.lock);

            -- Remove this process from the ready list. This makes it easier to put on a
            -- different list if it blocks during its run.
            -- println ("Scheduler - Ready List: ");
            -- Process.Queues.print (Process.cpuReadyLists(cpuData.cpuNum));

            Process.Queues.dequeue (Process.cpuReadyLists(cpuData.cpuNum), pid);

            -- print ("Scheduler: running "); print (Process.proctab(pid).name); print(" pid "); println (Integer(pid));

            if pid = Process.NO_PROCESS then
                raise SchedulerException with "Scheduler.schedule: No idle process in ready list";
            end if;

            Process.proctab(pid).state  := RUNNING;

            cpuData.currentPID          := pid;
            cpuData.currentContext      := Process.proctab(pid).context; -- save this address so we can switch back

            -- switch address spaces if appropriate
            cpuData.savedKernelRSP      := Process.proctab(pid).kernelStackTop;
            cpuData.tss.rsp0            := Process.proctab(pid).kernelStackTop;
            
            -- Only change address spaces if we're switching to a user-mode process.
            if Process.proctab(pid).mode = Process.USER then
                Process.switchAddressSpace (pid);
            end if;

            -- print ("Scheduler: Switching to context "); println (cpuData.currentContext);

            -- Restore initialized FP/SIMD state before any user instruction
            -- can execute. Kernel threads are compiled without FP/SIMD.
            if Process.proctab(pid).mode = Process.USER then
                Process.restoreFPUState (pid);
            end if;

            if Process.proctab(pid).readyTSC /= 0 then
                Trace.ObserveDuration
                    (Trace.EVENT_READY_LATENCY,
                     x86.rdtsc - Process.proctab(pid).readyTSC);
                Process.proctab(pid).readyTSC := 0;
            end if;

            Trace.Emit
                (Trace.EVENT_SCHEDULE_RUN,
                 Unsigned_64 (pid),
                 Unsigned_64 (Process.proctab(pid).priority));

            runStartTSC := x86.rdtsc;

            -- Start executing new process
            Process.switch (cpuData.schedulerContext'Address, cpuData.currentContext);

            -- when process pauses its run, we return here.
            -- directSwitch may have changed who's running on this CPU,
            -- so refresh pid from per-CPU state before processing.
            pid := cpuData.currentPID;
            Trace.Emit
                (Trace.EVENT_SCHEDULE_STOP,
                 Unsigned_64 (pid),
                 Unsigned_64 (Process.ProcessState'Pos
                    (Process.proctab(pid).state)));
            Trace.ObserveDuration (Trace.EVENT_RUN_TIME,
                                   x86.rdtsc - runStartTSC);
            cpuData.currentPID := Process.NO_PROCESS;

            -- switch back to kernel page tables if we weren't just running a kernel thread
            if Process.proctab(pid).mode = Process.USER then
                Mem_mgr.switchAddressSpace;
            end if;

            -- Update the process' context pointer.
            case Process.proctab(pid).state is

                when INVALID =>
                    -- Don't save the context here
                    null;
                    -- print ("Scheduler: process "); print (i);
                    -- println (" is terminated");

                when RUNNING =>
                    -- print ("Scheduler: process "); print (i);
                    -- print (" is interrupted, making READY and saving context: ");
                    -- println (cpuData.oldContext);
                    Process.proctab(pid).context := cpuData.oldContext;
                    Process.proctab(pid).state   := READY;

                    -- @TODO adjust priority here if we eat up full time-slice
                    -- put us back on the ready list.
                    Process.Queues.insert (
                        q      => Process.cpuReadyLists(cpuData.cpuNum),
                        pid    => pid,
                        key    => Process.proctab(pid).priority,
                        result => ign);

                when READY =>
                    -- Cross-CPU IPC race: another CPU's reply() called
                    -- notify() which set us back to READY and enqueued us
                    -- before we finished yielding. Already on the ready
                    -- list, just save context.
                    Process.proctab(pid).context := cpuData.oldContext;

                when WAITING | RECEIVING | SENDING | WAITINGFOREVENT |
                     WAITINGFORREPLY | WAITINGFORCOMPLETION | SUSPENDED |
                     SLEEPING =>
                    -- print ("Scheduler: process "); print (i);
                    -- print (" is blocked (waiting), saving context: ");
                    -- println (cpuData.oldContext);
                    Process.proctab(pid).context := cpuData.oldContext;

            end case;

        -- println ("Scheduler.schedule: releasing proctab lock");
        exitCriticalSection (Process.lock);

        end loop startSearch;
    end schedule;

end Scheduler;
