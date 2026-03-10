-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
--
-- Lock ordering (acquire in this order, never reverse):
--   1. mailtab(pid).lock    (per-mailbox, also protects completionTab(pid))
--   2. Process.lock         (global process table)
--   3. cpuReadyLists.lock   (per-CPU scheduler)
--   4. sleepList.lock       (sleep queue)
--
-- @TODO Model lock ordering in SPARK to get formal guarantees of correctness.
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

with BuddyAllocator;
with Capabilities.Operations;
with IPC_Labels;
with IPI;
with Config;
with Mem_mgr;
with PerCPUData;
with Process.IPC;
with Process.Queues;
with Scheduler;
with Segment;
with TextIO; use TextIO;
with x86;

package body Process
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        -- ProcList.setup (allProcs, Config.MAX_PROCESSES);
        FrameLists.setup (Config.MAX_PROCESSES * Config.PAGES_PER_PROCESS);
        -- MsgQueue.setup (Config.MAX_PROCESSES);
    end setup;

    ---------------------------------------------------------------------------
    -- addToProctab
    ---------------------------------------------------------------------------
    procedure addToProctab (proc : in Process) with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.addToProctab: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        proctab(proc.pid) := proc;

        -- println ("Process.addToProctab: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end addToProctab;

    ---------------------------------------------------------------------------
    -- createKernelThread
    ---------------------------------------------------------------------------
    function createKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority) return Process
    is
        proc : Process;
    begin
        PIDTracker.allocSpecificPID (pid);

        proc.pid      := pid;
        proc.ppid     := pid;       -- For IPC, we want this to have its own mailbox.
        proc.name     := name;
        proc.mode     := KERNEL;
        proc.state    := SUSPENDED;
        proc.priority := priority;

        -- Allocate 2 contiguous pages: guard (lower) + stack (upper)
        allocGuardedStack : declare
            function toKStackPtr is new Ada.Unchecked_Conversion
                (System.Address, ProcessKernelStackPtr);
            baseVirt : System.Address;
        begin
            BuddyAllocator.alloc (1, baseVirt);
            if baseVirt = BuddyAllocator.NO_BLOCK_AVAILABLE then
                raise ProcessException with
                    "Unable to allocate kernel stack + guard page";
            end if;
            proc.guardPage   := Virtmem.V2P (baseVirt);
            proc.kernelStack :=
                toKStackPtr (baseVirt + Virtmem.PAGE_SIZE);
            proc.kernelStack.canary := KSTACK_CANARY;
            -- Unmap the guard page so overflow triggers a page fault
            Mem_mgr.createGuardPage (proc.guardPage);
        end allocGuardedStack;
        proc.kernelStackTop := proc.kernelStack.all'Address + ProcessKernelStack'Size / 8;

        proc.kernelStack.filler := (others => 0);

        proc.kernelStack.interruptFrame := (
                interruptNumber => 0,    
                rip             => procStart,
                rsp             => proc.kernelStackTop,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_CODE) or 0,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_DATA) or 0,
                others          => 0
            );

        proc.kernelStack.returnAddress := interruptReturn'Address;
        proc.kernelStack.context       := (rip => start'Address, others => 0);

        proc.context := proc.kernelStack.context'Address;

        return proc;
    end createKernelThread;

    ---------------------------------------------------------------------------
    -- startKernelThread
    ---------------------------------------------------------------------------
    procedure startKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority;
                                 homeCPU    : in Natural := 0)
    is
        proc : Process := createKernelThread (procStart, name, pid, priority);
    begin
        proc.cpu := homeCPU;
        if proc.pid /= 0 then
            addToProctab (proc);
            resume (proc.pid);
        else
            raise ProcessException with "Process.startKernelThread: failed createKernelThread";
        end if;
    end startKernelThread;

    ---------------------------------------------------------------------------
    -- addStackPage
    -- Allocate an additional page of memory for this process or thread's stack,
    -- map it into the process or thread's parent process page tables, and add
    -- it to the list of pages used by this process or thread's parent process.
    ---------------------------------------------------------------------------
    procedure addStackPage (proc : in out Process) is
        newFrame : Virtmem.PhysAddress;
        ok : Boolean;
        MapException : exception;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        BuddyAllocator.allocFrame (newFrame);

        -- @TODO this shouldn't be a fatal error for kernel but works for now to detect errors.
        if newFrame = 0 then
            raise ProcessException with "Unable to allocate memory for Process' stack expansion.";
        end if;
        
        if proc.isThread then
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        -- Map the frame just below the thread's current stack.
        mapPage (phys    => newFrame,
                 virt    => To_Integer(proc.stackTop - Storage_Count((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE)),
                 flags   => Virtmem.PG_USERDATA,
                 myP4    => addrtab(proc.pgTable),
                 success => ok);

        -- print ("Process.addStackPage: Mapping new frame at "); println(To_Integer(proc.stackTop - Storage_Count((proc.numStackFrames + 1) * Virtmem.PAGE_SIZE)));

        if not ok then
            raise MapException with "Process.addStackPage - can't map process' stack";
        end if;

        proc.numStackFrames := proc.numStackFrames + 1;
    end addStackPage;

    ---------------------------------------------------------------------------
    -- addPage
    -- Allocate a page of memory for a process or thread's parent process, map
    -- at the specified address and adds the memory to the process or thread's
    -- parent process frame list so it will be freed on exit.
    ---------------------------------------------------------------------------
    procedure addPage (proc    : in out Process;
                       mapTo   : in System.Address;
                       storage : out System.Address;
                       flags   : in Unsigned_64 := Virtmem.PG_USERDATA) is
        newFrame : Virtmem.PhysAddress;
        ok : Boolean;
        MapException : exception;
    
        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin

        BuddyAllocator.allocFrame (newFrame);

        if newFrame = 0 then
            raise ProcessException with "Process.addPage: Unable to allocate memory for Process";
        end if;

        storage := Virtmem.P2Va (newFrame);

        if proc.isThread then
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        mapPage (phys    => newFrame,
                 virt    => To_Integer(mapTo),
                 flags   => flags,
                 myP4    => addrtab(proc.pgTable),
                 success => ok);

        if not ok then
            raise MapException with "Process.addPage - can't map process page";
        end if;
    end addPage;

    ---------------------------------------------------------------------------
    -- create
    -- Writes directly to proctab(pid) to avoid 12KB stack allocation.
    ---------------------------------------------------------------------------
    function create (procStart    : in System.Address;
                     ppid         : in ProcessID;
                     name         : in ProcessName;
                     priority     : in ProcessPriority;
                     procStack    : in System.Address;
                     thread       : in Boolean := False;
                     requestedPID : in ProcessID := NO_PROCESS) return ProcessID
        with SPARK_Mode => Off
    is
        pid : ProcessID;

        procedure zeroize is new Virtmem.zeroize (Virtmem.P4);
    begin
        if requestedPID /= NO_PROCESS then
            PIDTracker.allocSpecificPID (requestedPID);
            pid := requestedPID;
        else
            PIDTracker.allocPID (pid);
        end if;

        -- sanity checks
        if pid = 0 then
            raise ProcessException with "Unable to create new process. No free PIDs";
        end if;

        -- Clear the proctab entry before populating fields. The entry may
        -- contain stale data from a previously killed process.
        declare
            ignore : System.Address;
        begin
            ignore := Util.memset (proctab(pid)'Address, 0, Process'Size / 8);
        end;

        proctab(pid).pid          := pid;
        proctab(pid).ppid         := ppid;
        proctab(pid).svpid        := ppid;
        proctab(pid).name         := name;
        proctab(pid).mode         := USER;
        proctab(pid).state        := SUSPENDED;
        proctab(pid).priority     := priority;
        proctab(pid).stackTop     := procStack;
        proctab(pid).stackBottom  := procStack - STACK_SIZE;
        proctab(pid).capGeneration := Capabilities.INITIAL_GENERATION;

        -- heap can't be calculated until the image segments are added to this
        -- process, so set to non-canonical address to start
        proctab(pid).heapStart    := BAD_HEAP_ADDRESS;

        -- istart must be max so addSegmentToProcess can compare/update
        proctab(pid).istart       := To_Address (16#FFFF_FFFF_FFFF_FFFF#);

        -- Allocate 2 contiguous pages: guard (lower) + stack (upper)
        allocGuardedStack : declare
            function toKStackPtr is new Ada.Unchecked_Conversion
                (System.Address, ProcessKernelStackPtr);
            baseVirt : System.Address;
        begin
            BuddyAllocator.alloc (1, baseVirt);
            if baseVirt = BuddyAllocator.NO_BLOCK_AVAILABLE then
                raise ProcessException with
                    "Unable to allocate kernel stack + guard page";
            end if;
            proctab(pid).guardPage   := Virtmem.V2P (baseVirt);
            proctab(pid).kernelStack :=
                toKStackPtr (baseVirt + Virtmem.PAGE_SIZE);
            proctab(pid).kernelStack.canary := KSTACK_CANARY;
            -- Unmap the guard page so overflow triggers a page fault
            Mem_mgr.createGuardPage (proctab(pid).guardPage);
        end allocGuardedStack;

        FrameLists.create (proctab(pid).frames, MAX_STACK_FRAMES + MAX_HEAP_FRAMES);

        proctab(pid).kernelStackTop := proctab(pid).kernelStack.all'Address +
                                       ProcessKernelStack'Size / 8;

        -- Build the initial kernel stack.
        proctab(pid).kernelStack.filler := (others => 0);

        -- Since we use iretq to enter usermode initially, we need an "interrupt
        -- frame" to set up the proper rip, rsp, flags and segments.
        proctab(pid).kernelStack.interruptFrame := (
                interruptNumber => 0,
                rip             => procStart,
                rsp             => proctab(pid).stackTop,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_CODE) or 3,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_DATA) or 3,
                others          => 0);

        proctab(pid).kernelStack.returnAddress := interruptReturn'Address;
        proctab(pid).kernelStack.context := (rip => start'Address, others => 0);

        proctab(pid).context := proctab(pid).kernelStack.context'Address;

        if not thread then
            -- For heavyweight processes, set up the send/recv queues and the
            -- address space it (and any child threads) will be using.
            proctab(pid).pgTable := pid;

            mailtab(pid).recvQueue := (
                lock => (name => null, others => <>),
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            mailtab(pid).sendQueue := (
                lock => (name => null, others => <>),
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            proctab(pid).mail := pid;

            -- Grant initial capabilities for well-known services
            Capabilities.Operations.grantInitialCaps (
                table => proctab(pid).caps,
                pid   => Unsigned_64(pid));

            zeroize (addrtab(pid));
            Mem_mgr.mapKernelMemIntoProcess (addrtab(pid));
        else
            -- for threads, point to parent's page table and mailbox
            proctab(pid).pgTable := ppid;
            proctab(pid).mail    := ppid;
        end if;

        -- Add a page for the process' stack
        addStackPage (proctab(pid));

        return pid;
    end create;

    ---------------------------------------------------------------------------
    -- This is where READY processes continue executing after the scheduler 
    --  puts them in the RUNNING state. Note that the scheduler acquires the
    --  proctab lock in schedule;, but we must release that lock here.
    -- @TODO annotate that a process used its full time-slice here so we can
    --  de-prioritize it.
    ---------------------------------------------------------------------------
    procedure yield with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.yield: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        
        Scheduler.enter;

        -- continue execution here after context switch back to this process.
        -- println ("Process.yield: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end yield;

    ---------------------------------------------------------------------------
    -- ready
    -- Move a process into the ready list and change its state to READY
    ---------------------------------------------------------------------------
    procedure ready (pid : ProcessID) with
        SPARK_Mode => On
    is
        ret : ProcessID;
        targetCPU : constant Natural := proctab(pid).cpu;
        currentPID : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        proctab(pid).state := READY;
        Queues.insert (cpuReadyLists(targetCPU), pid,
                       proctab(pid).priority, ret);

        if ret /= pid then
            raise ProcessException with "Process.ready: Error adding pid to ready list.";
        end if;

        -- If the newly readied process has higher priority than the
        -- currently running one, request preemption at interrupt return.
        -- Only meaningful if targeting THIS CPU.
        if targetCPU = PerCPUData.getCPUNumber and then
           currentPID /= NO_PROCESS and then
           proctab(pid).priority > proctab(currentPID).priority
        then
            setNeedReschedule : declare
                perCPUAddr : constant System.Address :=
                    PerCPUData.getPerCPUDataAddr;
                cpuData : PerCPUData.PerCPUData with
                    Import, Volatile, Address => perCPUAddr;
            begin
                cpuData.needReschedule := True;
            end setNeedReschedule;
        end if;

        -- If readying on a remote CPU, send reschedule IPI so it
        -- wakes from idle HLT promptly instead of waiting for timer.
        if targetCPU /= PerCPUData.getCPUNumber then
            IPI.sendReschedule (targetCPU);
        end if;
    end ready;

    ---------------------------------------------------------------------------
    -- Release our hold on a resource and go into WAITING state.
    ---------------------------------------------------------------------------
    procedure wait (channel      : in WaitChannel;
                    resourceLock : in out Spinlocks.spinlock) with
        SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        -- Need to get process lock, otherwise we may be woken up by another
        -- thread during their call to schedule once we release our resource
        -- lock.
        -- println ("Process.wait: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.exitCriticalSection (resourceLock);

        -- Begin waiting and reschedule.
        proctab(pid).state := WAITING;
        proctab(pid).channel := channel;
        Scheduler.enter;

        -- Resume here when woken.
        proctab(pid).channel := NO_CHANNEL;

        -- Should only be woken when we can acquire the resource lock.
        -- println ("Process.wait: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
        Spinlocks.enterCriticalSection (resourceLock);
    end wait;

    ---------------------------------------------------------------------------
    -- goAheadBody
    -- @TODO this is probably a poor implementation, may cause thrashing when
    -- all the woken threads attempt to get the same resource.
    ---------------------------------------------------------------------------
    procedure goAheadBody (channel : in WaitChannel) with
        SPARK_Mode => On
    is
    begin
        for p of proctab loop
            if p.state = WAITING and p.channel = channel then
                p.state := READY;
            end if;
        end loop;
    end goAheadBody;

    ---------------------------------------------------------------------------
    -- goAhead - public interface for internal goAheadBody, to ensure locks are
    -- held.
    ---------------------------------------------------------------------------
    procedure goAhead (channel : in WaitChannel) with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.goAhead: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);
        
        goAheadBody (channel);
        
        -- println ("Process.goAhead: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end goAhead;

    ---------------------------------------------------------------------------
    -- suspend
    ---------------------------------------------------------------------------
    procedure suspend with
        SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        -- println ("Process.suspend: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        -- Begin suspension and reschedule.
        proctab(pid).state := SUSPENDED;
        Scheduler.enter;

        -- Resume here when woken.
        -- Should only be woken when we can acquire the resource lock.
        -- println ("Process.suspend: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end suspend;

    ---------------------------------------------------------------------------
    -- resume
    ---------------------------------------------------------------------------
    procedure resume (pid : ProcessID) with
        SPARK_Mode => On
    is
        ignore : ProcessID;
    begin
        -- println ("Process.resume: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).state /= SUSPENDED then
            raise ProcessException with "Process.resume: Attempting to resume non-suspended process.";
        end if;

        ready (pid);

        -- println ("Process.resume: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end resume;

    ---------------------------------------------------------------------------
    -- notify
    ---------------------------------------------------------------------------
    procedure notify (pid : ProcessID) with
        SPARK_Mode => On
    is
        ignore : ProcessID;
    begin
        Spinlocks.enterCriticalSection (lock);

        if proctab(pid).state /= WAITINGFOREVENT and
           proctab(pid).state /= WAITINGFORREPLY and
           proctab(pid).state /= WAITINGFORCOMPLETION and
           proctab(pid).state /= WAITINGFORNOTIFY then
            raise ProcessException with "Process.notify: process not in a waiting state.";
        end if;

        ready (pid);

        Spinlocks.exitCriticalSection (lock);
    end notify;

    ---------------------------------------------------------------------------
    -- sleep
    ---------------------------------------------------------------------------
    procedure sleep (us : Time.Duration) with
        SPARK_Mode => On
    is
        pid    : ProcessID := PerCPUData.getCurrentPID;
        ignore : ProcessID;
    begin
        proctab(pid).state := SLEEPING;

        Queues.insertDelta (q            => sleepList,
                            pid          => pid,
                            delayFromNow => Integer(us / 1000),
                            result       => ignore);

        yield;
    end sleep;

    ---------------------------------------------------------------------------
    -- This is where the scheduler will initially switch() to.
    ---------------------------------------------------------------------------
    procedure start with
        SPARK_Mode => On
    is
    begin
        -- println ("Process.start: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
        -- Return to interruptReturn.
    end start;

    ---------------------------------------------------------------------------
    -- symbols needed for createFirstProcess to load the init binary image.
    ---------------------------------------------------------------------------
    initBinaryStart     : Util.Symbol with
        Import => True, External_Name => "_binary_build_init_bin_start";

    initBinarySize      : Util.Symbol with
        Import => True, External_Name => "_binary_build_init_bin_size";
    
    ---------------------------------------------------------------------------
    -- createFirstProcess
    -- The first process (called init in other systems) just makes syscalls to
    --  start running the first executable from disk.
    ---------------------------------------------------------------------------
    procedure createFirstProcess with
        SPARK_Mode => Off   -- use of 'Address
    is
        -- This is kind of funky. The initBinarySize gets stored as though it were
        -- an address.
        initSize : constant Storage_Count := Storage_Count(Util.addrToNum (initBinarySize'Address));

        InitImageTooBigException : exception;

        pid          : ProcessID;
        alignedStart : System.Address;
        ignore       : System.Address;
    begin
        if initSize > Virtmem.PAGE_SIZE then
            raise InitImageTooBigException with "Init image is too big to fit in one page.";
        end if;

        -- @TODO put the stack way up on top of lower-half like a real process.
        pid := create (procStart   => To_Address(0),
                       ppid        => 1,
                       name        => "init            ",
                       priority    => 3,
                       procStack   => PROCESS_STACK_TOP_VIRT);

        -- add page to process, copy the init image to it
        addPage (proc    => proctab(pid),
                 mapTo   => To_Address(0),
                 storage => alignedStart,
                 flags   => Virtmem.PG_USERCODE);

        ignore := Util.memcpy (alignedStart,
                               initBinaryStart'Address,
                               initSize);

        resume (pid);

    end createFirstProcess;

    ---------------------------------------------------------------------------
    -- getParent
    ---------------------------------------------------------------------------
    function getParent (pid : in ProcessID) return ProcessID with
        SPARK_Mode => On
    is
    begin
        return proctab(pid).ppid;
    end getParent;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (pid : in ProcessID) with
        SPARK_Mode => On
    is
        p4addr : System.Address;
    begin
        if proctab(pid).isThread then
            p4addr := addrtab(getParent (pid))'Address;
        else
            p4addr := addrtab(pid)'Address;
        end if;

        Virtmem.setActiveP4 (Virtmem.K2P (p4addr));
    end switchAddressSpace;

    ---------------------------------------------------------------------------
    -- removeFromMailQueue
    -- If a process is blocked in the SENDING or RECEIVING states, this 
    -- procedure will identify the appropriate mailbox it is waiting on, and
    -- remove it from the list.
    ---------------------------------------------------------------------------
    procedure removeFromMailQueue (pid : in ProcessID) with
        SPARK_Mode => On
    is
        mailbox : constant ProcessID    := proctab(pid).queueKey;
        state   : constant ProcessState := proctab(pid).state;
        ignore  : ProcessID;
    begin
        if state = SENDING then
            Queues.popItem (mailtab(mailbox).sendQueue, pid, ignore);
        elsif state = RECEIVING then
            Queues.popItem (mailtab(mailbox).recvQueue, pid, ignore);
        else
            raise ProcessException with "Process.removeFromMailQueue called on non-SENDING/RECEIVING process.";
        end if;
    end removeFromMailQueue;

    ---------------------------------------------------------------------------
    -- kill
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID) with
        SPARK_Mode => On
    is
        -- recursively unmaps/deallocates process' full paging hierarchy
        procedure deleteP4 is new Virtmem.deleteP4 (BuddyAllocator.freeFrame);

        ignore : ProcessID;
    begin
        print ("Process.kill: terminating pid "); println (Integer(pid));

        -- Back to kernel addressing.
        Mem_mgr.switchAddressSpace;

        --  Notify parent (who requested the spawn) that child has exited.
        --  Must happen BEFORE acquiring Process.lock because sendEvent
        --  acquires mailtab.lock then calls notify() which acquires
        --  Process.lock — doing it inside would reverse lock ordering.
        if proctab(pid).ppid /= NO_PROCESS then
            notifyParent : declare
                exitMsg : Message := NULL_MESSAGE;
            begin
                exitMsg.tag := (label  => IPC_Labels.EVENT_CHILD_EXIT,
                                length => 1,
                                flags  => 0,
                                badge  => 0);
                exitMsg.words (0) := Unsigned_64 (pid);
                IPC.sendEvent (proctab(pid).ppid, exitMsg);
            end notifyParent;
        end if;

        --  Also notify supervisor if different from parent
        if proctab(pid).svpid /= NO_PROCESS and then
           proctab(pid).svpid /= proctab(pid).ppid
        then
            notifySupervisor : declare
                exitMsg : Message := NULL_MESSAGE;
            begin
                exitMsg.tag := (label  => IPC_Labels.EVENT_CHILD_EXIT,
                                length => 1,
                                flags  => 0,
                                badge  => 0);
                exitMsg.words (0) := Unsigned_64 (pid);
                IPC.sendEvent (proctab(pid).svpid, exitMsg);
            end notifySupervisor;
        end if;

        -- println ("Process.kill: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        -- Remove this process from whatever list it was on (if any)
          case proctab(pid).state is
            when READY =>
                Queues.popItem (cpuReadyLists(proctab(pid).cpu), pid, ignore);

            when SLEEPING =>
                Queues.popItem (sleepList, pid, ignore);
            
            when SENDING | RECEIVING =>
                removeFromMailQueue (pid);

            when others =>
                null;
        end case;

        proctab(pid).state := INVALID;

        -- Clear FPU ownership if killed process owns the FPU
        clearFPU : declare
            perCPUAddr : constant System.Address :=
                PerCPUData.getPerCPUDataAddr;
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            if cpuData.fpuOwner = pid then
                cpuData.fpuOwner := NO_PROCESS;
            end if;
        end clearFPU;

        -- Revoke any shared memory grants this process had created
        IPC.revokeAllGrants (pid);

        -- Bump generation counter to invalidate caps held by others
        if proctab(pid).capGeneration < Capabilities.Generation'Last then
            proctab(pid).capGeneration := proctab(pid).capGeneration + 1;
        end if;

        -- Clear capability table
        Capabilities.Operations.clearTable (proctab(pid).caps);

        if proctab(pid).mode = USER and not proctab(pid).isThread then

            loop
                BuddyAllocator.freeFrame (FrameLists.front(proctab(pid).frames));
                FrameLists.popFront (proctab(pid).frames);

                exit when proctab(pid).frames.length = 0;
            end loop;

            FrameLists.delete (proctab(pid).frames);

            -- Need to unmap Kernel mem here so when we delete page tables we
            -- only delete the process' page tables.
            Mem_mgr.unmapKernelMemFromProcess (addrtab(pid));
            deleteP4 (addrtab(pid));
            proctab(pid).pgTable := NO_PROCESS;
        end if;
        
        -- Remap the guard page so the buddy allocator can reuse it,
        -- then free the 2-page block (guard + stack).
        if proctab(pid).guardPage /= 0 then
            Mem_mgr.removeGuardPage (proctab(pid).guardPage);
            BuddyAllocator.free (1, Virtmem.P2Va (proctab(pid).guardPage));
            proctab(pid).kernelStack := null;
        end if;

        -- Nothing to return to, go back to scheduler.
        Scheduler.enter;

        -- Should never actually get here.
        raise ProcessException with "Somehow returned from scheduler in Process.kill";
    end kill;

    ---------------------------------------------------------------------------
    -- getRunningProcess
    ---------------------------------------------------------------------------
    -- function getRunningProcess return ProcPtr
    -- is
    -- begin
    --     return Proctab(PerCPUData.getCurrentPID)'Access;
    -- end getRunningProcess;

    ---------------------------------------------------------------------------
    -- pageFault
    ---------------------------------------------------------------------------
    procedure pageFault (pid : ProcessID; addr : System.Address) with
        SPARK_Mode => On
    is
        ignore : System.Address;
    begin
        -- Valid stack or heap address? heapEnd and heapStart should always be
        -- page aligned, so we can round down to lower page here when mapping.
        if (addr <= Proctab(pid).stackTop and addr >= Proctab(pid).stackBottom) or
           (addr <= Proctab(pid).heapEnd  and addr >= Proctab(pid).heapStart) then

            print ("Process: Adding page for pid "); print (Integer(pid));
            print (" at "); println (To_Address (To_Integer (addr) and Virtmem.PAGE_MASK));
            addPage (proc    => Proctab(pid),
                     mapTo   => To_Address (To_Integer (addr) and Virtmem.PAGE_MASK),   -- round down
                     storage => ignore);
        else
            -- @TODO use a heuristic here to figure out if this was a stack
            -- overflow, or heap over/underflow and signal the process either way.
            -- (something like distance to stackBottom < distance to heapEnd = stack overflow)
            print ("Process: Illegal memory access at "); print (addr);
            print (" pid "); print (Integer(pid));
            print (" stackTop "); print (Proctab(pid).stackTop);
            print (" stackBottom "); print (Proctab(pid).stackBottom);
            print (" heapStart "); print (Proctab(pid).heapStart);
            print (" heapEnd "); println (Proctab(pid).heapEnd);
            IPC.notifySupervisor (
                pid        => pid,
                faultLabel => IPC_Labels.EVENT_PROCESS_FAULT,
                detail0    => 14,  -- #PF vector
                detail1    => Unsigned_64 (To_Integer (addr)),
                detail2    => 0);
            kill (pid);
        end if;
    end pageFault;

    ---------------------------------------------------------------------------
    -- enableFPU
    -- Turn on FPU state saving/restoring for this process.
    ---------------------------------------------------------------------------
    procedure enableFPU with SPARK_Mode => On
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        newCR0 : Unsigned_64 := x86.getCR0;
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        -- Clear CR0.TS so FPU/SSE instructions execute without #NM.
        -- CR4.OSFXSR and CR4.OSXMMEXCPT are set globally in boot.asm.
        Util.clearBit (newCR0, 3);  -- clear CR0.TS

        proctab(pid).fpu := proctab(pid).kernelStack.all.fpuarea'Address;

        x86.setCR0 (newCR0);

        -- Track this process as the FPU owner on this CPU
        setOwner : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            cpuData.fpuOwner := pid;
        end setOwner;
    end enableFPU;

    ---------------------------------------------------------------------------
    -- disableFPU
    -- Disable FPU across all processes. This will cause an exception when FPU
    -- is used again.
    ---------------------------------------------------------------------------
    procedure disableFPU with SPARK_Mode => On
    is
        newCR0 : Unsigned_64 := x86.getCR0;
    begin
        -- Use CR0.TS (bit 3) to trap FPU/SSE usage. This generates #NM
        -- for both x87 and SSE instructions, enabling lazy context switching.
        -- CR4.OSFXSR stays set so SSE doesn't generate #UD.
        Util.setBit (newCR0, 3);        -- set CR0.TS
        x86.setCR0 (newCR0);
    end disableFPU;

    ---------------------------------------------------------------------------
    -- directSwitch
    -- Direct context switch between two processes, bypassing the scheduler.
    -- Caller MUST hold Process.lock. Target resumes in yield() which
    -- releases Process.lock.
    ---------------------------------------------------------------------------
    procedure directSwitch (fromPID : ProcessID; toPID : ProcessID) with
        SPARK_Mode => Off
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        doSwitch : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            -- Lazy FPU: arm trap if different owner
            if cpuData.fpuOwner /= toPID then
                disableFPU;
            end if;

            -- Update per-CPU state (what scheduler normally does)
            cpuData.currentPID     := toPID;
            cpuData.savedKernelRSP := proctab(toPID).kernelStackTop;
            cpuData.tss.rsp0       := proctab(toPID).kernelStackTop;

            -- Switch address space if target is user process
            if proctab(toPID).mode = USER then
                switchAddressSpace (toPID);
            end if;

            proctab(toPID).state := RUNNING;

            -- asm_switch_to saves fromPID's RSP and loads toPID's RSP.
            -- Target resumes in yield() which releases Process.lock.
            switch (proctab(fromPID).context'Address,
                    proctab(toPID).context);

            -- Resumed: Process.lock is held (by whoever switched back)
        end doSwitch;
    end directSwitch;

    ---------------------------------------------------------------------------
    -- saveFPUState
    ---------------------------------------------------------------------------
    procedure saveFPUState (pid : ProcessID) with SPARK_Mode => On
    is
    begin
        if proctab(pid).fpu /= System.Null_Address then
            x86.fxsave (proctab(pid).fpu);

            -- When saving the FPU state, disable the FPU so we can detect use when
            -- the next process runs, and if used, set up the save area.
            disableFPU;
        end if;

    end saveFPUState;

    ---------------------------------------------------------------------------
    -- restoreFPUState
    ---------------------------------------------------------------------------
    procedure restoreFPUState (pid : ProcessID) with SPARK_Mode => On
    is
    begin
        -- If this process uses the FPU, go ahead and enable it.
        if proctab(pid).fpu /= System.Null_Address then
            x86.fxrstor (proctab(pid).fpu);
            enableFPU;
        end if;
    end restoreFPUState;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (p : ProcPtr) is
    begin
        -- @TODO
        null;
    end print;

    ---------------------------------------------------------------------------
    -- Track which PIDs are in use, allocate new ones.
    ---------------------------------------------------------------------------
    package body PIDTracker with 
        Refined_State => (PIDTrackerState => (pidMap, pidLock, trackerLockName))
    is

        -- TODO: this is basically cut-n-paste from the bootmem allocator.
        -- might be kind of nice to genericize these into a "bitmap" package

        -- Find a free PID and mark it as in use. Uses spinlock
        -- to ensure that two processes don't share the same PID
        -- if this were called by two threads at once.
        procedure allocPID(pid : out ProcessID) with
            SPARK_Mode => On
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            pid := findFreePID;
            -- if no free PIDs, we'll mark PID 0 as used again, which is true.
            markUsed (pid);
            exitCriticalSection (pidLock);
        end allocPID;

        procedure allocSpecificPID (pid : in ProcessID) with
            SPARK_Mode => On
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            
            if pidMap (pid) = False then
                raise ProcessException with "Attempted to use specific PID already in use";
            end if;
            
            markUsed (pid);
            exitCriticalSection (pidLock);
        end allocSpecificPID;


        -- Mark a PID as free.
        procedure freePID(pid : in ProcessID) with
            SPARK_Mode => On
        is
        begin
            markFree(pid);
        end freePID;


        -- Find a free PID
        function findFreePID return ProcessID with
            SPARK_Mode => On
        is
            --block : Unsigned_64;
            --retPID : ProcessID := 0;
        begin

            -- linearly iterate through the list looking for a 0. We reserve
            -- the first few PIDs for the kernel to use for tasks with specific
            -- IDs.
            for i in 16..ProcessID'Last loop
                if (pidMap(i)) then
                    return i;
                end if;
            end loop;

            return 0;
        end findFreePID;


        -- Mark a particular PID as used.
        procedure markUsed(pid : in ProcessID) with
            SPARK_Mode => On
        is
            --block : constant PIDBlock := getBlock(pid);
            --offset : constant PIDOffset := getOffset(pid);
        begin
            --util.setBit(pidMap(block), offset);
            pidMap(pid) := False;
        end markUsed;
        

        -- Mark a PID as free.
        procedure markFree(pid : in ProcessID) with
            SPARK_Mode => On
        is
            -- block : constant PIDBlock := getBlock(pid);
            -- offset : constant PIDOffset := getOffset(pid);
        begin
            --util.clearBit(pidmap(block), offset);
            pidMap(pid) := True;
        end markFree;


        -- -- Return the index into bitmap array in which this PID resides.
        -- function getBlock(pid : in ProcessID) return PIDBlock with
        --     SPARK_Mode => On is
        -- begin
        --     return Natural(pid / 64);
        -- end getBlock;


        -- -- Return the bit within a Unsigned_64 representing this single PID.
        -- function getOffset(pid : in ProcessID) return PIDOffset with
        --     SPARK_Mode => On is
        -- begin
        --     return Natural(pid mod 64);
        -- end getOffset;

    end PIDTracker;

end Process;