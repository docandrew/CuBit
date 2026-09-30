-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- System Scheduler
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Build;
with Mem_mgr;
with Process.Queues;
with Virtual_Deadlines;
with Process_Lifetime;
with TextIO; use TextIO;
with Trace;
with x86;

-- Ada adapter over hardware context switching and live process-table/GS state.
package body Scheduler is
    ---------------------------------------------------------------------------
    -- Enter the scheduler from a process.
    -- Checks the kernel stack canary before context switching.
    ---------------------------------------------------------------------------
    procedure enter
    is
        use type Process.ProcessMode;
        use type Process.ProcessState;
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        -- Check kernel stack canary before context switch
        checkCanary : declare
            use type Process.ProcessKernelStackPtr;
            cpuData : PerCPUData.PerCPUData with Import, Volatile, Address => perCPUAddr;
            tid : constant Process.ThreadID := cpuData.currentThread;
        begin
            if Process."/=" (tid, Process.NO_THREAD) and then
               Process.threadtab (tid).kernelStack /= null and then
               Process.threadtab (tid).kernelStack.canary /= Process.KSTACK_CANARY
            then
                raise SchedulerException with
                    "Kernel stack overflow detected (canary corrupted)";
            end if;
        end checkCanary;

        -- Eagerly preserve user FP/SIMD state and FS base before leaving the
        -- process.
        -- Kernel scheduler code is compiled without MMX/SSE, so no restore is
        -- needed until immediately before the next user process runs.
        saveFPU : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
            tid : constant Process.ThreadID := cpuData.currentThread;
        begin
            if Process."/=" (tid, Process.NO_THREAD) and then
               Process.threadtab (tid).state /= Process.INVALID and then
               Process.threadtab (tid).mode = Process.USER
            then
                Process.saveUserCPUState (tid);
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
    procedure schedule (cpuData : in out PerCPUData.PerCPUData)
    is
        use Spinlocks;
        use Process;

        pid : ProcessID;
        tid : ThreadID;
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
            -- Quiescent point: this CPU holds no process-table record here.
            Process.cpuOnline (cpuData.cpuNum) := True;
            Process.Process_Table.Quiescent (cpuData.cpuNum);
            Process.Thread_Table.Quiescent (cpuData.cpuNum);

            enterCriticalSection (Process.lock);

            -- Remove this process from the ready list. This makes it easier to put on a
            -- different list if it blocks during its run.
            -- println ("Scheduler - Ready List: ");
            -- Process.Queues.print (Process.cpuReadyLists(cpuData.cpuNum));

            -- The earliest deadline this CPU may run: its own list's head,
            -- unless another CPU's list holds an entry it may take that is
            -- earlier by more than the margin (Virtual_Deadlines.Choose).
            -- A taken thread now belongs to this CPU for later wakeups.
            tid := NO_THREAD;
            if Build.Work_Stealing then
                chooseList : declare
                    package VD renames Virtual_Deadlines;
                    Last : constant VD.CPU := VD.CPU (Process.cpuReadyLists'Last);
                    Heads : VD.CPU_Keys (0 .. Last);
                    Allowed : VD.CPU_Flags (0 .. Last);
                    Chosen : Natural;
                begin
                    for C in Process.cpuReadyLists'Range loop
                        Heads (VD.CPU (C)) :=
                          (if C = cpuData.cpuNum
                           then Process.Queues.headKey (Process.cpuReadyLists (C))
                           else Process.Queues.takeableKey (Process.cpuReadyLists (C)));
                        Allowed (VD.CPU (C)) := Process.cpuOnline (C);
                    end loop;
                    Chosen := Natural (VD.Choose
                      (VD.CPU (cpuData.cpuNum), Heads, Allowed, Process.marginTicks));
                    if Chosen /= cpuData.cpuNum then
                        Process.Queues.takeFirst (Process.cpuReadyLists (Chosen), tid);
                        if tid /= NO_THREAD then
                            Process.threadtab (tid).cpu := cpuData.cpuNum;
                        end if;
                    end if;
                end chooseList;
            end if;

            if tid = NO_THREAD then
                loop
                    Process.Queues.dequeue (Process.cpuReadyLists(cpuData.cpuNum), tid);
                    exit when tid = NO_THREAD or else
                      not Process_Lifetime.Closing (threadtab (tid).lifetime);
                end loop;
            end if;
            pid := (if tid = NO_THREAD then NO_PROCESS else Process.processOf (tid));

            -- print ("Scheduler: running "); print (Process.proctab(pid).name); print(" pid "); println (Integer(pid));

            if pid = Process.NO_PROCESS then
                raise SchedulerException with "Scheduler.schedule: No idle process in ready list";
            end if;

            Process.threadtab (tid).state  := RUNNING;
            Process.threadtab (tid).readiness := Rescheduled;
            Process.cpuRunningKey (cpuData.cpuNum) := Process.runKeyOf (tid);
            Process.noteContextStarted (tid);

            cpuData.currentThread       := tid;
            cpuData.currentContext      := Process.threadtab (tid).context; -- save this address so we can switch back

            -- switch address spaces if appropriate
            cpuData.savedKernelRSP      := Process.threadtab (tid).kernelStackTop;
            cpuData.tss.rsp0            := Process.threadtab (tid).kernelStackTop;

            -- Only change address spaces if we're switching to a user-mode process.
            if Process.threadtab (tid).mode = Process.USER then
                Process.switchAddressSpace (pid);
            end if;

            -- print ("Scheduler: Switching to context "); println (cpuData.currentContext);

            -- Restore initialized FP/SIMD state and FS base before any user
            -- instruction can execute. Kernel threads are compiled without
            -- FP/SIMD.
            if Process.threadtab (tid).mode = Process.USER then
                Process.restoreUserCPUState (tid);
            end if;

            if Process.threadtab (tid).readyTSC /= 0 then
                Trace.ObserveDuration
                    (Trace.EVENT_READY_LATENCY,
                     x86.rdtsc - Process.threadtab (tid).readyTSC);
                Process.threadtab (tid).readyTSC := 0;
            end if;

            Trace.Emit
                (Trace.EVENT_SCHEDULE_RUN,
                 Unsigned_64 (pid),
                 Unsigned_64 (Process.threadtab (tid).priority));

            runStartTSC := x86.rdtsc;

            Process.accountBoundary (Process.NO_THREAD, tid, Scheduler_Start);

            -- Start executing new process
            Process.switch (cpuData.schedulerContext'Address, cpuData.currentContext);

            -- when process pauses its run, we return here.
            -- directSwitch may have changed who's running on this CPU,
            -- so refresh pid from per-CPU state before processing.
            tid := cpuData.currentThread;
            pid := Process.processOf (tid);
            -- Charge the LAST direct-handoff owner before acknowledging its
            -- context stop/reaping. The full chain is not this PID's runtime.
            Process.accountBoundary (tid, Process.NO_THREAD, Scheduler_Stop);
            Trace.Emit
                (Trace.EVENT_SCHEDULE_STOP,
                 Unsigned_64 (tid),
                 Unsigned_64 (Process.ProcessState'Pos
                    (Process.threadtab (tid).state)));
            Trace.ObserveDuration (Trace.EVENT_RUN_TIME,
                                   x86.rdtsc - runStartTSC);
            cpuData.currentThread := Process.NO_THREAD;

            -- switch back to kernel page tables if we weren't just running a kernel thread
            if Process.threadtab (tid).mode = Process.USER then
                Mem_mgr.switchAddressSpace;
            end if;

            -- We are now on the scheduler stack and kernel page tables.
            -- Process.lock prevents the reaper observing this acknowledgement
            -- until the context handoff has completely finished.
            Process.noteContextStopped (tid);

            -- Update the process' context pointer.
            if not Process_Lifetime.Closing (Process.threadtab (tid).lifetime) then
            case Process.threadtab (tid).state is

                when INVALID =>
                    -- Don't save the context here
                    null;
                    -- print ("Scheduler: process "); print (i);
                    -- println (" is terminated");

                when RUNNING =>
                    -- print ("Scheduler: process "); print (i);
                    -- print (" is interrupted, making READY and saving context: ");
                    -- println (cpuData.oldContext);
                    Process.threadtab (tid).context := cpuData.oldContext;
                    Process.threadtab (tid).state   := READY;
                    Process.threadtab (tid).queuedTSC := x86.rdtsc;

                    -- Back on a ready list: here, unless an earlier deadline
                    -- waits here and another CPU is idle (placementFor). With
                    -- nothing running here, only queued work outranks it.
                    Process.cpuRunningKey (cpuData.cpuNum) := Virtual_Deadlines.Idle_Key;
                    Process.placeOn (tid, Process.placementFor (tid));

                when READY =>
                    -- Cross-CPU IPC race: another CPU's reply() called
                    -- notify() which set us back to READY and enqueued us
                    -- before we finished yielding. Already on the ready
                    -- list, just save context.
                    Process.threadtab (tid).context := cpuData.oldContext;

                when WAITING | RECEIVING | SENDING | WAITINGFOREVENT |
                     WAITINGFORREPLY | WAITINGFORCOMPLETION | SUSPENDED |
                     SLEEPING | FUTEXWAITING =>
                    -- print ("Scheduler: process "); print (i);
                    -- print (" is blocked (waiting), saving context: ");
                    -- println (cpuData.oldContext);
                    Process.threadtab (tid).context := cpuData.oldContext;

            end case;
            end if;

        -- println ("Scheduler.schedule: releasing proctab lock");
        exitCriticalSection (Process.lock);

        end loop startSearch;
    end schedule;

end Scheduler;
