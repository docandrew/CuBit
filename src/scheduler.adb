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
with PerCPUData;
with Textmode;
with Util;
with x86;
--with spinlock;

package body Scheduler with
    -- Refined_State => (
    --     SchedulerState => ()),
    SPARK_Mode => On
is
    -- These are just stack locations for where we left off.
    -- oldContext       : System.Address;
    -- currentContext   : System.Address;
    -- schedulerContext : System.Address;
    -- currentPID       : process.ProcessID := 0;

    -- Enter the scheduler from a process. Suspect this is where 
    -- we'll need to add
    -- the per-CPU scheduler context lookup.
    -- TODO: Add checking for stack canary when we enter the scheduler.
    procedure enter with
        SPARK_Mode => On
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        getCPUContext: declare
            cpuData : PerCPUData.PerCPUData with
                Import, Address => perCPUAddr;
        begin
            --textmode.println("Entering scheduler");
            process.switch(cpuData.oldContext'Address, cpuData.schedulerContext);
        end getCPUContext;
    end enter;

    -- Note: this process is only _called_ once, at bootup (per-CPU).
    -- Future entries back into this function are through the enter
    --  procedure which means we'll pick up where we left off,
    --  trying to run the next process in the proctab.
    procedure schedule(cpuData : in out PerCPUData.PerCPUData) with
        SPARK_Mode => On
    is
        use Spinlock;
        use Process;
        pidIndex : ProcessID := 1;
    begin
        -- a bit inefficient, but for now we just linearly search
        -- through the proctab, starting where we last left off.
        startSearch : loop
            x86.sti;

            Textmode.print("CPU "); Textmode.print(cpuData.cpuNum);
            Textmode.println(": Checking for processes to run.");
            -- reached end of proctab without finding any READY
            -- processes, so just idle until the next tick
            if pidIndex = Process.proctab'Last then
                idle;           -- when we return from idle...
                pidIndex := 1;    -- ... we'll go back to the beginning and check.
            end if;

            if x86.panicked then
                x86.cli;
                x86.halt;
            end if;

            -- NOTE: this lock is released either by process.start (if this is the process'
            --  first time executing); process.yield (if the process is continuing from the 
            --  call to scheduler.enter from the last time it yielded); or at the symmetric
            --  exitCriticalSection call in this function (if no processes were READY and
            --  we left the for loop).
            enterCriticalSection(process.lock);
            for i in pidIndex .. process.proctab'Last loop
                if Process.proctab(i).state = READY then
                    Textmode.print("scheduler: found READY process ");
                    Textmode.println(i);
                    
                    Textmode.print("scheduler: switching to pid ");
                    Textmode.print(i);
                    Textmode.print(" context: ");
                    Textmode.println(process.proctab(i).context);
                    
                    Process.proctab(i).state    := RUNNING;
                    cpuData.currentPID          := i;
                    cpuData.currentContext      := process.proctab(i).context; -- save this address so we can switch back

                    -- switch address spaces
                    cpuData.tss.rsp0        := Unsigned_64(proctab(i).kernelStackTop);
                    cpuData.currentPID      := i;
                    cpuData.savedKernelRSP  := To_Address(proctab(i).kernelStackTop);
                    Process.switchAddressSpace(proctab(i).pgTable'Address);

                    -- Start executing new process
                    Process.switch(cpuData.schedulerContext'Address, cpuData.currentContext);
                    
                    -- switch back to kernel page tables
                    Mem_mgr.switchAddressSpace;

                    -- when process pauses its run, we return here
                    -- update the process' context pointer. currentContext was set in enter;
                    Textmode.print("scheduler: saving context for pid ");
                    Textmode.print(i); Textmode.print(": "); 
                    Textmode.println(cpuData.oldContext);
                    
                    cpuData.currentPID          := Process.NO_PROCESS;
                    Process.proctab(i).context  := cpuData.oldContext;
                    Process.proctab(i).state    := READY;
                end if;
            end loop;
            exitCriticalSection(Process.lock);

        end loop startSearch;
    end schedule;

    procedure idle with
        SPARK_Mode => On
    is
    begin
        -- TODO: add some counting of ticks here so we can
        -- keep track of CPU usage.
        x86.halt;
    end idle;

    -- function getCurrentPID return process.ProcessID with
    --     SPARK_Mode => On
    -- is
    -- begin
    --     return currentPID;
    -- end getCurrentPID;

end Scheduler;
