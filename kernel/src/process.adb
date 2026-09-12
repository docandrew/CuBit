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
pragma Ada_2022;
with Ada.Unchecked_Conversion;

with BuddyAllocator;
with Build;
with Trace;
with Capabilities.IRQ;
with Capabilities.Operations;
with IPC_Labels;
with IPI;
with Config;
with Interrupt_State;
with Mem_mgr;
with PerCPUData;
with Process.IPC;
with Process.Queues;
with Scheduler;
with Scheduler_Timing;
with Scheduler_Alarm;
with Segment;
with Spinlocks;
with Sysinfo;
with TextIO; use TextIO;
with x86;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body Process is
    use type Scheduling_Shadow.Health;
    -- Separate aligned CPU-local metadata avoids changing assembly-visible
    -- PerCPUData offsets. All reads/writes use the existing Process.lock.
    type Switch_Reason is (Relinquish, Quantum_Expired, Awakened_Peer, Higher_Priority);
    type CPU_Accounting_Record is record
        Clock : Accounting.Clock_State;
        Scheduler_Time : Accounting.Totals;
        Shadow : Scheduling_Shadow.CPU_State;
        Turn : Scheduling_Turns.State;
        Reason : Switch_Reason := Relinquish;
    end record with Alignment => 64;
    cpuAccounting : array (0 .. Config.MAX_CPUS - 1) of CPU_Accounting_Record;

    procedure accountBoundary
      (From_PID, To_PID : ProcessID; Boundary : Accounting_Boundary)
    is
        CPU : CPU_Accounting_Record renames
          cpuAccounting (PerCPUData.getCPUNumber);
        C : Accounting.Charge;
        Now : constant Unsigned_64 := x86.readOrderedTSC;
        function Observed (PID : ProcessID) return Boolean is
          (PID /= NO_PROCESS and then
           PID not in Config.IDLE_PID_BASE .. Config.IDLE_PID_BASE + Config.MAX_SMP_CPUS - 1);
    begin
        Accounting.Transition
          (CPU.Clock, From_PID, To_PID, Now, C);
        if C.Accepted then
            if C.Charged_Owner = NO_PROCESS then
                Accounting.Add_Time (CPU.Scheduler_Time, C.Ticks);
            else
                Accounting.Add_Time (proctab(C.Charged_Owner).execution, C.Ticks);
                Scheduling_Turns.Charge (CPU.Turn, C.Ticks);
            end if;
            case Boundary is
                when Scheduler_Start =>
                    CPU.Reason := Relinquish;
                    if Scheduling_Turns.Remaining (proctab(To_PID).savedTurn) > 0 then
                        Scheduling_Turns.Move (proctab(To_PID).savedTurn, CPU.Turn);
                        Scheduling_Turns.Count (proctab(To_PID).turnCounters,
                          Scheduling_Turns.Resumed_Dispatch);
                    elsif Time.tscPerDuration <= Unsigned_64'Last /
                      Scheduler_Timing.Quantum_Microseconds
                    then
                        CPU.Turn := Scheduling_Turns.Fresh
                          (Time.tscPerDuration * Scheduler_Timing.Quantum_Microseconds);
                        Scheduling_Turns.Count (proctab(To_PID).turnCounters,
                          Scheduling_Turns.Fresh_Dispatch);
                    else
                        CPU.Turn := Scheduling_Turns.Empty;
                    end if;
                    Accounting.Dispatch (proctab(To_PID).execution, Accounting.Scheduled);
                when IPC_Handoff =>
                    Trace.Emit (Trace.EVENT_IPC_HANDOFF,
                      Unsigned_64(From_PID), Unsigned_64(To_PID));
                    Accounting.Dispatch (proctab(To_PID).execution, Accounting.Direct_IPC);
                when Scheduler_Stop =>
                    Scheduling_Turns.Count (proctab(From_PID).turnCounters,
                      (case CPU.Reason is
                         when Higher_Priority => Scheduling_Turns.Higher_Preemption,
                         when Quantum_Expired => Scheduling_Turns.Quantum_Rotation,
                         when Awakened_Peer => Scheduling_Turns.Wake_Rotation,
                         when Relinquish => Scheduling_Turns.Relinquishment));
                    if CPU.Reason = Higher_Priority then
                        Scheduling_Turns.Move (CPU.Turn, proctab(From_PID).savedTurn);
                    else
                        CPU.Turn := Scheduling_Turns.Empty;
                        proctab(From_PID).savedTurn := Scheduling_Turns.Empty;
                    end if;
                when Accounting_Checkpoint => null;
            end case;
            if Build.OneShot_Scheduling and then To_PID /= NO_PROCESS then
                if Build.Wakeup_Scheduling and then
                  Queues.hasAwakenedPeer (cpuReadyLists(PerCPUData.getCPUNumber),
                    proctab(To_PID).priority)
                then
                    Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
                elsif Time.tscPerDuration > 0 and then
                  Scheduling_Turns.Remaining (CPU.Turn) > 0 and then
                  Queues.hasReadyPeer (cpuReadyLists(PerCPUData.getCPUNumber),
                    proctab(To_PID).priority)
                then
                    remainingTurn : declare
                        Ticks : constant Unsigned_64 := Scheduling_Turns.Remaining (CPU.Turn);
                        Delay_Us : Unsigned_64 := Ticks / Time.tscPerDuration;
                    begin
                        if Ticks mod Time.tscPerDuration /= 0 then Delay_Us := Delay_Us + 1; end if;
                        Scheduler_Alarm.Request_Earlier
                          (Scheduler_Alarm.Delay_Microseconds (Unsigned_64'Min (1_000, Delay_Us)));
                    end remainingTurn;
                end if;
            end if;
            if Build.Observe_Scheduling_Budgets then
                if Now > Unsigned_64 (Scheduling_Shadow.Budgets.Time_Units'Last) or else
                   Time.tscPerDuration = 0 or else
                   Time.tscPerDuration > Unsigned_64 (Scheduling_Shadow.Tick_Rate'Last)
                then
                    Scheduling_Shadow.Invalidate (CPU.Shadow);
                else
                    declare
                        Stamp : constant Scheduling_Shadow.Budgets.Time_Units :=
                          Scheduling_Shadow.Budgets.Time_Units (Now);
                    begin
                        if not Scheduling_Shadow.Initialized (CPU.Shadow) and then
                           Scheduling_Shadow.Status (CPU.Shadow) = Scheduling_Shadow.Healthy
                        then
                            Scheduling_Shadow.Initialize
                              (CPU.Shadow, Scheduling_Shadow.Tick_Rate (Time.tscPerDuration), Stamp);
                        end if;
                        if Boundary = Accounting_Checkpoint then
                            if Observed (From_PID) then
                                Scheduling_Shadow.Observe
                                  (CPU.Shadow, proctab(From_PID).shadow, Stamp,
                                   Scheduling_Shadow.Continue_Execution);
                            end if;
                        else
                            if Observed (From_PID) then
                                Scheduling_Shadow.Observe
                                  (CPU.Shadow, proctab(From_PID).shadow, Stamp, Scheduling_Shadow.Stop);
                            end if;
                            if Observed (To_PID) then
                                Scheduling_Shadow.Observe
                                  (CPU.Shadow, proctab(To_PID).shadow, Stamp, Scheduling_Shadow.Dispatch);
                            end if;
                        end if;
                    end;
                end if;
            end if;
        else
            CPU.Turn := Scheduling_Turns.Empty;
            if From_PID /= NO_PROCESS then
                proctab(From_PID).savedTurn := Scheduling_Turns.Empty;
            end if;
            if To_PID /= NO_PROCESS then
                proctab(To_PID).savedTurn := Scheduling_Turns.Empty;
            end if;
            if Build.Observe_Scheduling_Budgets then
                Scheduling_Shadow.Invalidate (CPU.Shadow);
            end if;
        end if;
    end accountBoundary;

    procedure printOwnAccounting is
        PID : constant ProcessID := PerCPUData.getCurrentPID;
        CPU : constant Natural := PerCPUData.getCPUNumber;
        Snapshot : Accounting.Totals;
        Condition : Accounting.Health;
        Shadow : Scheduling_Shadow.Snapshot;
        Shadow_Health : Scheduling_Shadow.Health;
        Generation : Capabilities.Generation;
        Turns : Scheduling_Turns.Counters;
    begin
        Spinlocks.enterCriticalSection (lock);
        accountBoundary (PID, PID, Accounting_Checkpoint);
        Snapshot := proctab(PID).execution;
        Turns := proctab(PID).turnCounters;
        Condition := Accounting.Status (cpuAccounting(CPU).Clock);
        if Build.Observe_Scheduling_Budgets then
            Shadow := Scheduling_Shadow.Inspect (proctab(PID).shadow);
            Shadow_Health := Scheduling_Shadow.Status (cpuAccounting(CPU).Shadow);
            Generation := proctab(PID).capGeneration;
        end if;
        Spinlocks.exitCriticalSection (lock);
        -- No global process enumeration or new authority granted by this
        -- diagnostic. These are lifetime totals, independent of Trace.Reset.
        println ("ACCOUNTING: pid=" & PID'Image &
          " cpu=" & CPU'Image &
          " residency_ticks=" & Snapshot.Residency_Ticks'Image &
          " scheduled=" & Snapshot.Scheduled_Dispatches'Image &
          " direct=" & Snapshot.Direct_Dispatches'Image &
          " fault=" & Natural'Image (Accounting.Health'Pos (Condition)) &
          " saturated=" & Natural'Image (Boolean'Pos (Snapshot.Saturated)));
        println ("TURNS: pid=" & PID'Image &
          " fresh=" & Turns(Scheduling_Turns.Fresh_Dispatch)'Image &
          " resumed=" & Turns(Scheduling_Turns.Resumed_Dispatch)'Image &
          " higher=" & Turns(Scheduling_Turns.Higher_Preemption)'Image &
          " quantum=" & Turns(Scheduling_Turns.Quantum_Rotation)'Image &
          " wake=" & Turns(Scheduling_Turns.Wake_Rotation)'Image &
          " relinquish=" & Turns(Scheduling_Turns.Relinquishment)'Image &
          " timer_opportunities=" & Turns(Scheduling_Turns.Timer_Opportunity)'Image);
        if Build.Observe_Scheduling_Budgets then
            println ("SHADOW-BUDGET: mode=demand-only pid=" & PID'Image &
              " cpu=" & CPU'Image & " generation=" & Generation'Image &
              " charged_ticks=" & Shadow.Totals.Charged_Ticks'Image &
              " dispatches=" & Shadow.Totals.Dispatches'Image &
              " denied=" & Shadow.Totals.Denied'Image &
              " checkpoints=" & Shadow.Totals.Checkpoints'Image &
              " remaining_ticks=" & Shadow.Remaining'Image &
              " credits=" & Shadow.Credits'Image &
              " overrun=" & Natural'Image (Boolean'Pos (Shadow.Overrun)) &
              " fault=" & Natural'Image (Scheduling_Shadow.Health'Pos (Shadow_Health)) &
              " saturated=" & Natural'Image (Boolean'Pos (Shadow.Totals.Saturated)));
        end if;
    end printOwnAccounting;

    ReaperPID : ProcessID := NO_PROCESS;
    procedure retirementWorker with No_Return;
    procedure reclaimProcess (pid : ProcessID);

    procedure wakeReaper is
    begin
        if ReaperPID /= NO_PROCESS and then proctab(ReaperPID).state = SUSPENDED then
            ready (ReaperPID);
        end if;
    end wakeReaper;

    procedure publish (pid : ProcessID) is
    begin
        Spinlocks.enterCriticalSection (mailtab(pid).lock);
        Spinlocks.enterCriticalSection (lock);
        proctab(pid).admitted := True;
        mailtab(pid).closed := False;
        Spinlocks.exitCriticalSection (lock);
        Spinlocks.exitCriticalSection (mailtab(pid).lock);
    end publish;

    procedure noteContextStarted (pid : ProcessID) is
        OK : Boolean;
    begin
        Process_Lifetime.Enter_CPU (proctab(pid).lifetime, OK);
        if not OK then
            raise ProcessException with "Dispatch of executing or retiring process";
        end if;
    end noteContextStarted;

    procedure noteContextStopped (pid : ProcessID) is
        OK : Boolean;
    begin
        Process_Lifetime.Leave_CPU (proctab(pid).lifetime, OK);
        if not OK then
            raise ProcessException with "Context stop without execution presence";
        end if;
        if Process_Lifetime.Can_Reap (proctab(pid).lifetime) then
            wakeReaper;
        end if;
    end noteContextStopped;

    procedure checkTermination is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        if pid /= NO_PROCESS and then
           Process_Lifetime.Closing (proctab(pid).lifetime)
        then
            Spinlocks.enterCriticalSection (lock);
            Scheduler.enter;
            raise ProcessException with "Retired context resumed";
        end if;
    end checkTermination;

    ---------------------------------------------------------------------------
    -- initializeFPUState
    -- Construct the architectural reset state expected by FXRSTOR. Keeping a
    -- valid image for every user process prevents first-use state inheritance.
    ---------------------------------------------------------------------------
    procedure initializeFPUState (state : out FPUState) with SPARK_Mode => On
    is
    begin
        state := (others => 0);

        -- x87 control word 16#037F# at byte offset 0.
        state(1) := 16#7F#;
        state(2) := 16#03#;

        -- MXCSR architectural reset value 16#0000_1F80# at byte offset 24.
        state(25) := 16#80#;
        state(26) := 16#1F#;
    end initializeFPUState;

    ---------------------------------------------------------------------------
    -- setup
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        -- Before AP startup / publication; never reinitialize live locks.
        Spinlocks.Initialize (lock, lockname'Access);
        Spinlocks.Initialize (grantLock, grantLockName'Access);
        Spinlocks.Initialize (sleepList.lock, sleepListLockName'Access);
        -- ProcList.setup (allProcs, Config.MAX_PROCESSES);
        FrameLists.setup (Config.MAX_PROCESSES * Config.PAGES_PER_PROCESS);
        -- MsgQueue.setup (Config.MAX_PROCESSES);
    end setup;

    ---------------------------------------------------------------------------
    -- addToProctab
    ---------------------------------------------------------------------------
    procedure addToProctab (proc : in Process)
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
        if pid = NO_PROCESS then
            PIDTracker.allocPID (proc.pid);
        else
            PIDTracker.allocSpecificPID (pid);
            proc.pid := pid;
        end if;
        proc.ppid     := proc.pid;
        proc.admitted := True;
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

    -- Boot-only, like startKernelThread: uses the bootstrap stack for PCB
    -- construction, never the kernel stack of a retiring application.
    procedure startReaper is
        proc : Process := createKernelThread
          (retirementWorker'Address, "Reaper          ", NO_PROCESS, 5);
    begin
        proc.cpu := 0;
        ReaperPID := proc.pid;
        addToProctab (proc);
        resume (proc.pid);
    end startReaper;

    ---------------------------------------------------------------------------
    -- addStackPage
    -- Allocate an additional page of memory for this process or thread's stack,
    -- map it into the process or thread's parent process page tables, and add
    -- it to the list of pages used by this process or thread's parent process.
    ---------------------------------------------------------------------------
    procedure addStackPage (proc : in out Process) is
        newFrame : Virtmem.PhysAddress;
        ok : Boolean;
        claimed : Boolean;
        frameOwner : ProcessID;
        MapException : exception;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin
        BuddyAllocator.allocFrame (newFrame);

        -- @TODO this shouldn't be a fatal error for kernel but works for now to detect errors.
        if newFrame = 0 then
            raise ProcessException with "Unable to allocate memory for Process' stack expansion.";
        end if;

        if proc.isThread then
            frameOwner := proc.ppid;
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            frameOwner := proc.pid;
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        BuddyAllocator.claimUserFrame
          (newFrame, Unsigned_8 (frameOwner), claimed);
        if not claimed then
            raise ProcessException with
              "Unable to establish stack-frame ownership";
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
        claimed : Boolean;
        frameOwner : ProcessID;
        MapException : exception;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);
    begin

        BuddyAllocator.allocFrame (newFrame);

        if newFrame = 0 then
            raise ProcessException with "Process.addPage: Unable to allocate memory for Process";
        end if;

        storage := Virtmem.P2Va (newFrame);

        if proc.isThread then
            frameOwner := proc.ppid;
            FrameLists.insertFront (proctab(proc.ppid).frames, newFrame);
        else
            frameOwner := proc.pid;
            FrameLists.insertFront (proc.frames, newFrame);
        end if;

        BuddyAllocator.claimUserFrame
          (newFrame, Unsigned_8 (frameOwner), claimed);
        if not claimed then
            raise ProcessException with
              "Unable to establish user-frame ownership";
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
                     stackSize    : in UserStackSize;
                     thread       : in Boolean := False;
                     requestedPID : in ProcessID := NO_PROCESS) return ProcessID

    is
        pid : ProcessID;

        procedure zeroize is new Virtmem.zeroize (Virtmem.P4);
    begin
        if thread then
            -- This dormant path never set isThread and has no live syscall
            -- consumer. Do not admit shared address spaces without lifetime
            -- accounting for every executing member.
            raise ProcessException with "Shared-address-space thread creation unsupported";
        end if;
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
        -- contain stale data from a previously killed process. Preserve the
        -- capability generation counter so recycled PIDs don't reset to
        -- INITIAL_GENERATION (which would let stale caps pass gen checks).
        declare
            savedGen : constant Capabilities.Generation :=
                proctab(pid).capGeneration;
            type Grant_Generation_Array is array (GrantID) of
              Memory_Grants.Live_Grant_Generation;
            type Grant_Reuse_Array is array (GrantID) of Boolean;
            savedGrantGenerations : Grant_Generation_Array;
            savedGrantReuse       : Grant_Reuse_Array;
            ignore   : System.Address;
        begin
            for slot in GrantID loop
                savedGrantGenerations(slot) :=
                  proctab(pid).grants(slot).generation;
                savedGrantReuse(slot) := proctab(pid).grants(slot).reusable;
            end loop;

            ignore := Util.memset (proctab(pid)'Address, 0, Process'Size / 8);
            proctab(pid).requestSequence := IPC_Request_Ids.Initial_Sequence;
            proctab(pid).lifetime := Process_Lifetime.Initial_State;
            proctab(pid).execution := (others => <>);
            proctab(pid).savedTurn := Scheduling_Turns.Empty;
            proctab(pid).turnCounters := [others => 0];
            proctab(pid).shadow := Scheduling_Shadow.Empty_Reservation;
            proctab(pid).admitted := False;
            if savedGen >= Capabilities.INITIAL_GENERATION then
                proctab(pid).capGeneration := savedGen;
            else
                proctab(pid).capGeneration := Capabilities.INITIAL_GENERATION;
            end if;

            for slot in GrantID loop
                proctab(pid).grants(slot).generation :=
                  savedGrantGenerations(slot);
                proctab(pid).grants(slot).reusable := savedGrantReuse(slot);
            end loop;
        end;

        proctab(pid).pid          := pid;
        proctab(pid).ppid         := ppid;
        proctab(pid).svpid        := ppid;
        if ppid /= NO_PROCESS then
            proctab(pid).parentGeneration := proctab(ppid).capGeneration;
        end if;
        proctab(pid).name         := name;
        proctab(pid).mode         := USER;
        proctab(pid).state        := SUSPENDED;
        proctab(pid).priority     := priority;
        proctab(pid).latency      :=
            (class    => LATENCY_NORMAL,
             periodUs => 0,
             budgetUs => 0,
             flags    => 0);
        proctab(pid).stackTop     := procStack;
        proctab(pid).stackBottom  := procStack - stackSize;
        proctab(pid).stackSize    := stackSize;

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

        FrameLists.create
            (proctab(pid).frames,
             Natural (stackSize / Virtmem.FRAME_SIZE) + MAX_HEAP_FRAMES);

        proctab(pid).kernelStackTop := proctab(pid).kernelStack.all'Address +
                                       ProcessKernelStack'Size / 8;

        -- Build the initial kernel stack.
        initializeFPUState (proctab(pid).kernelStack.fpuarea);
        proctab(pid).fpu := proctab(pid).kernelStack.fpuarea'Address;
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
                lock => <>,
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            mailtab(pid).sendQueue := (
                lock => <>,
                head => NO_PROCESS,
                tail => NO_PROCESS
            );

            proctab(pid).mail := pid;
            Spinlocks.enterCriticalSection (mailtab(pid).lock);
            mailtab(pid).closed := True;
            mailtab(pid).ring := (others => <>);
            mailtab(pid).nextReceiveLane := Queued_Messages;
            Spinlocks.exitCriticalSection (mailtab(pid).lock);

            -- Grant initial capabilities for well-known services
            Capabilities.Operations.grantInitialCaps (
                table => proctab(pid).caps,
                pid   => Unsigned_64(pid),
                gen   => proctab(pid).capGeneration);

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
    procedure yield
    is
    begin
        -- println ("Process.yield: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        Scheduler.enter;

        -- continue execution here after context switch back to this process.
        -- println ("Process.yield: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end yield;

    procedure serviceReschedule is
        cpuData : PerCPUData.PerCPUData with Import, Volatile,
          Address => PerCPUData.getPerCPUDataAddr;
    begin
        if not cpuData.needReschedule then return; end if;
        Spinlocks.enterCriticalSection (lock);
        cpuData.needReschedule := False;
        if Build.OneShot_Scheduling and then Build.Wakeup_Scheduling and then
          cpuData.currentPID /= NO_PROCESS and then
          Queues.hasAwakenedPeer (cpuReadyLists(cpuData.cpuNum),
            proctab(cpuData.currentPID).priority)
        then
            Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
        end if;
        if cpuData.currentPID /= NO_PROCESS and then
           proctab(cpuData.currentPID).state = RUNNING and then
           Queues.hasReadyPeer (cpuReadyLists(cpuData.cpuNum),
             proctab(cpuData.currentPID).priority, Queues.Strictly_Higher)
        then
            cpuAccounting(cpuData.cpuNum).Reason := Higher_Priority;
            Scheduler.enter;
        end if;
        Spinlocks.exitCriticalSection (lock);
    end serviceReschedule;

    procedure serviceTimerPreemption is
        PID : constant ProcessID := PerCPUData.getCurrentPID;
        CPU : constant Natural := PerCPUData.getCPUNumber;
    begin
        if PID = NO_PROCESS then return; end if;
        Spinlocks.enterCriticalSection (lock);
        Scheduling_Turns.Count (proctab(PID).turnCounters, Scheduling_Turns.Timer_Opportunity);
        accountBoundary (PID, PID, Accounting_Checkpoint);
        if Queues.hasReadyPeer
          (cpuReadyLists(CPU), proctab(PID).priority, Queues.Strictly_Higher)
        then
            cpuAccounting(CPU).Reason := Higher_Priority;
            Scheduler.enter;
        elsif Queues.hasReadyPeer (cpuReadyLists(CPU), proctab(PID).priority) and then
          (Scheduling_Turns.Remaining (cpuAccounting(CPU).Turn) = 0 or else
           (Build.Wakeup_Scheduling and then
            Queues.hasAwakenedPeer (cpuReadyLists(CPU), proctab(PID).priority)))
        then
            cpuAccounting(CPU).Reason :=
              (if Scheduling_Turns.Remaining (cpuAccounting(CPU).Turn) = 0
               then Quantum_Expired else Awakened_Peer);
            Scheduler.enter;
        end if;
        Spinlocks.exitCriticalSection (lock);
    end serviceTimerPreemption;

    ---------------------------------------------------------------------------
    -- ready
    -- Move a process into the ready list and change its state to READY
    ---------------------------------------------------------------------------
    procedure ready (pid : ProcessID)
    is
        ret : ProcessID;
        targetCPU : constant Natural := proctab(pid).cpu;
        currentPID : constant ProcessID := PerCPUData.getCurrentPID;
    begin
        if proctab(pid).state = INVALID or else
           Process_Lifetime.Closing (proctab(pid).lifetime)
        then
            return; -- A queued notification cannot restart a retiring task.
        end if;
        proctab(pid).readyTSC := x86.rdtsc;
        proctab(pid).readiness := Awakened;
        Trace.Emit (Trace.EVENT_READY, Unsigned_64(pid), Unsigned_64(targetCPU));
        proctab(pid).state := READY;
        Queues.insert (cpuReadyLists(targetCPU), pid,
                       proctab(pid).priority, ret);

        if ret /= pid then
            raise ProcessException with "Process.ready: Error adding pid to ready list.";
        end if;

        if Build.OneShot_Scheduling and then Build.Wakeup_Scheduling and then
          targetCPU = PerCPUData.getCPUNumber and then currentPID /= NO_PROCESS and then
          proctab(pid).priority >= proctab(currentPID).priority
        then
            Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
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
    -- setLatencyContract
    ---------------------------------------------------------------------------
    procedure setLatencyContract
        (pid      : ProcessID;
         class    : LatencyClass;
         periodUs : Unsigned_32;
         budgetUs : Unsigned_32;
         flags    : Unsigned_32)

    is
    begin
        proctab(pid).latency :=
            (class    => class,
             periodUs => periodUs,
             budgetUs => budgetUs,
             flags    => flags);
    end setLatencyContract;

    ---------------------------------------------------------------------------
    -- Release our hold on a resource and go into WAITING state.
    ---------------------------------------------------------------------------
    procedure wait (channel      : in WaitChannel;
                    resourceLock : in out Spinlocks.spinlock)
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
    procedure goAheadBody (channel : in WaitChannel)
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
    procedure goAhead (channel : in WaitChannel)
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
    procedure suspend
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
    procedure resume (pid : ProcessID)
    is
        ignore : ProcessID;
    begin
        -- println ("Process.resume: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        if Process_Lifetime.Closing (proctab(pid).lifetime) then
            Spinlocks.exitCriticalSection (lock);
            return;
        end if;
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
    procedure notify (pid : ProcessID)
    is
        ignore : ProcessID;
    begin
        Spinlocks.enterCriticalSection (lock);

        if Process_Lifetime.Closing (proctab(pid).lifetime) then
            Spinlocks.exitCriticalSection (lock);
            return;
        end if;
        if proctab(pid).state = WAITINGFOREVENT or else
           proctab(pid).state = WAITINGFORREPLY or else
           proctab(pid).state = WAITINGFORCOMPLETION or else
           proctab(pid).state = RECEIVING
        then
            ready (pid);
        elsif proctab(pid).state = READY or else
              proctab(pid).state = RUNNING
        then
            --  Notification producers commonly perform a lock-free state
            --  observation before arriving here.  Another CPU may win the
            --  wakeup race before we acquire Process.lock.  Notifications
            --  are level-triggered by queued work/words, so that second wake
            --  is already satisfied and must be idempotent.
            null;
        else
            raise ProcessException with "Process.notify: process not in a waiting state.";
        end if;

        Spinlocks.exitCriticalSection (lock);
    end notify;

    ---------------------------------------------------------------------------
    -- sleep
    ---------------------------------------------------------------------------
    procedure sleep (us : Time.Duration)
    is
        pid    : ProcessID := PerCPUData.getCurrentPID;
        ignore : ProcessID;
    begin
        -- Publish the blocked state and hand off the running context under
        -- Process.lock. A remote wakeup cannot enqueue this task before its
        -- context has been saved by the scheduler.
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        proctab(pid).state := SLEEPING;

        Queues.insertDeltaNoLock (q            => sleepList,
                                  pid          => pid,
                                  delayFromNow => Integer(us / 1000),
                                  result       => ignore);

        Spinlocks.exitCriticalSection (sleepList.lock);

        Scheduler.enter;
        Spinlocks.exitCriticalSection (lock);
    end sleep;

    ---------------------------------------------------------------------------
    -- This is where the scheduler will initially switch() to.
    ---------------------------------------------------------------------------
    procedure start
    is
    begin
        -- A new context has no suspended switch frame to restore its state.
        -- Keep interrupts masked until interruptReturn restores the initial
        -- RFLAGS together with CS/RSP through IRETQ.
        PerCPUData.resumeHandoff (Interrupt_State.Initial_Context);
        -- println ("Process.start: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
        -- Return to interruptReturn.
    end start;

    ---------------------------------------------------------------------------
    -- switch
    -- Save the outer critical section's restoration policy on this context's
    -- stack. numCLI remains CPU-local: Process.lock is handed across the
    -- switch with interrupts disabled. All callers use that same handoff.
    ---------------------------------------------------------------------------
    procedure switch (oldProc : in System.Address; newProc : in System.Address)
    is
        procedure switchRegisters (oldContext, newContext : System.Address)
            with Import => True, Convention => C,
                 External_Name => "asm_switch_to";
        saved : constant Interrupt_State.Context := PerCPUData.captureHandoff;
    begin
        switchRegisters (oldProc, newProc);

        -- Re-read GS after resumption: the task may have migrated. Using the
        -- old CPU's address here would corrupt another CPU's lock state.
        PerCPUData.resumeHandoff (saved);
    end switch;

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
    procedure createFirstProcess    -- use of 'Address
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
                       procStack   => PROCESS_STACK_TOP_VIRT,
                       stackSize   => INIT_PROCESS_STACK_SIZE);

        -- add page to process, copy the init image to it
        addPage (proc    => proctab(pid),
                 mapTo   => To_Address(0),
                 storage => alignedStart,
                 flags   => Virtmem.PG_USERCODE);

        ignore := Util.memcpy (alignedStart,
                               initBinaryStart'Address,
                               initSize);

        publish (pid);
        resume (pid);

    end createFirstProcess;

    ---------------------------------------------------------------------------
    -- getParent
    ---------------------------------------------------------------------------
    function getParent (pid : in ProcessID) return ProcessID
    is
    begin
        return proctab(pid).ppid;
    end getParent;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (pid : in ProcessID)
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
    -- killProcess
    ---------------------------------------------------------------------------
    function killProcess (pid : ProcessID;
                          expectedGeneration : Capabilities.Generation := 0)
                          return Boolean is
        accepted : Boolean := False;
    begin
        if pid = NO_PROCESS then return False; end if;
        -- Close admission and request stop in the same mailbox/process order
        -- used by producers. No resources are reclaimed by this caller.
        Spinlocks.enterCriticalSection (mailtab(pid).lock);
        Spinlocks.enterCriticalSection (lock);
        if proctab(pid).admitted and then proctab(pid).mode = USER and then
           proctab(pid).state /= INVALID and then
           (expectedGeneration = 0 or else
            expectedGeneration = proctab(pid).capGeneration)
        then
            mailtab(pid).closed := True;
            Process_Lifetime.Request_Stop (proctab(pid).lifetime);
            proctab(pid).receiveDeadlineActive := False;
            wakeReaper;
            IPI.broadcastReschedule;
            accepted := True;
        end if;
        Spinlocks.exitCriticalSection (lock);
        Spinlocks.exitCriticalSection (mailtab(pid).lock);
        return accepted;
    end killProcess;

    procedure retirementWorker is
        victim : ProcessID;
        claimed : Boolean;
    begin
        loop
            victim := NO_PROCESS;
            Spinlocks.enterCriticalSection (lock);
            for p in ProctabType'Range loop
                if proctab(p).admitted and then
                   Process_Lifetime.Can_Reap (proctab(p).lifetime)
                then
                    Process_Lifetime.Claim_Reap (proctab(p).lifetime, claimed);
                    if not claimed then
                        raise ProcessException with "Retirement claim lost under process lock";
                    end if;
                    -- Admission was closed before this claim. INVALID rejects
                    -- administrative operations; PID storage remains reserved.
                    proctab(p).state := INVALID;
                    Queues.detach (cpuReadyLists(proctab(p).cpu), p);
                    Queues.detach (sleepList, p, Queues.Delta_Queue);
                    victim := p;
                    exit;
                end if;
            end loop;
            if victim = NO_PROCESS then
                -- No polling and no lost wakeup: request/CPU-stop uses this
                -- same process lock to ready the suspended worker.
                proctab(ReaperPID).state := SUSPENDED;
                Scheduler.enter;
                Spinlocks.exitCriticalSection (lock);
            else
                Spinlocks.exitCriticalSection (lock);
                reclaimProcess (victim);
                yield;
            end if;
        end loop;
    end retirementWorker;

    procedure reclaimProcess (pid : ProcessID) is
        procedure deleteP4 is new Virtmem.deleteP4 (BuddyAllocator.freeFrame);
        pidReusable, finished : Boolean;
        grantDeferred : Boolean := False;
        parent : constant ProcessID := proctab(pid).ppid;
        parentGen : constant Capabilities.Generation :=
            proctab(pid).parentGeneration;
        exitMsg : Message := NULL_MESSAGE;
    begin
        print ("Process.reclaimProcess: stopped PID "); println (Integer(pid));
        -- Worker owns its own stack and runs on kernel page tables. The
        -- victim's execution presence is zero; cleanup holds no global lock.
        IPC.retireMailboxes (pid);
        IPC.revokeAllGrants (pid);
        IPC.revokeAllGrantsTo (pid);
        --  Unregister IRQ and sysinfo driver registrations
        Capabilities.IRQ.unregisterAllByPID (Unsigned_64 (pid));
        Sysinfo.unregisterDriverByPID (pid);

        -- Bump the generation counter to invalidate caps held by others.  A
        -- PID whose generation space is exhausted must never be reused: doing
        -- so would make terminal-generation capabilities valid for the new
        -- process occupying that PID.
        Capabilities.Operations.advanceGeneration
          (current  => proctab(pid).capGeneration,
           reusable => pidReusable);

        -- A grant mapping pins its backing frames. Retain this dead process's
        -- PID (and therefore its authoritative grant records) until every
        -- borrower has returned.  The address space itself can still be torn
        -- down now; BuddyAllocator defers freeing the pinned data frames.
        IPC.prepareGrantProtectedTeardown
          (pid         => pid,
           pidReusable => pidReusable,
           deferred    => grantDeferred);

        -- Whole DMA allocations must be freed at their original buddy order.
        -- If a grant acquisition pins any constituent frame, retain all DMA
        -- blocks until the final borrower returns it.
        if not grantDeferred then
            IPC.releaseDMAAllocations (pid);
        end if;

        -- Clear capability table
        Capabilities.Operations.clearTable (proctab(pid).caps);

        if proctab(pid).mode = USER and not proctab(pid).isThread then

            while proctab(pid).frames.length > 0 loop
                BuddyAllocator.freeFrame (FrameLists.front(proctab(pid).frames));
                FrameLists.popFront (proctab(pid).frames);

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


        -- Retire before either immediate or grant-deferred PID publication.
        -- No access to proctab(pid) is allowed after publishing the PID free.
        Spinlocks.enterCriticalSection (lock);
        Process_Lifetime.Finish_Reap (proctab(pid).lifetime, finished);
        if not finished then
            raise ProcessException with "Reclamation without exclusive retirement claim";
        end if;
        proctab(pid).admitted := False;
        if grantDeferred then
            IPC.finishGrantProtectedTeardown (pid);
        elsif pidReusable then
            PIDTracker.freePID (pid);
        end if;
        Spinlocks.exitCriticalSection (lock);

        -- Report completed retirement, not merely a requested stop. Bind the
        -- notification to the original parent's generation, never a reused PID.
        if parent /= NO_PROCESS and then parentGen /= 0 then
            exitMsg.tag := (label => IPC_Labels.EVENT_CHILD_EXIT,
                            length => 1, flags => 0, reserved => 0);
            exitMsg.words(0) := Unsigned_64(pid);
            IPC.sendRetirementEvent (parent, parentGen, exitMsg);
        end if;
    end reclaimProcess;

    ---------------------------------------------------------------------------
    -- kill
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID)
    is
    begin
        if not killProcess (pid) then
            raise ProcessException with "Self termination request denied";
        end if;

        -- The stop request released the lock, but Scheduler.enter expects it
        -- held (the next process resuming from yield will release it).
        Spinlocks.enterCriticalSection (lock);

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
    procedure pageFault (pid : ProcessID; addr : System.Address)
    is
        ignore : System.Address;
    begin
        -- Valid stack or heap address? heapEnd and heapStart should always be
        -- page aligned, so we can round down to lower page here when mapping.
        if (addr <= Proctab(pid).stackTop and addr >= Proctab(pid).stackBottom) or
           (addr <= Proctab(pid).heapEnd  and addr >= Proctab(pid).heapStart) then

            -- Check memory quota before allocating
            if Proctab(pid).quota.maxFrames > 0 and then
               Proctab(pid).frames.length >= Proctab(pid).quota.maxFrames
            then
                print ("Process: memory quota exceeded for pid ");
                println (Integer(pid));
                IPC.notifySupervisor (
                    pid        => pid,
                    faultLabel => IPC_Labels.EVENT_PROCESS_FAULT,
                    detail0    => 14,
                    detail1    => Unsigned_64 (To_Integer (addr)),
                    detail2    => Unsigned_64 (Proctab(pid).quota.maxFrames));
                kill (pid);
                return;
            end if;

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
    procedure enableFPU
    is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        -- Clear CR0.TS before eager FXRSTOR64. CR4.OSFXSR and
        -- CR4.OSXMMEXCPT are set globally in boot.asm. CLTS avoids a full
        -- CR0 read/modify/write sequence on every userspace transition.
        x86.clearTaskSwitched;

        -- Record which eagerly-restored state is live for diagnostics.
        setOwner : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            cpuData.fpuOwner := pid;
        end setOwner;
    end enableFPU;

    ---------------------------------------------------------------------------
    -- directSwitch
    -- Direct context switch between two processes, bypassing the scheduler.
    -- Caller MUST hold Process.lock. Target resumes in yield() which
    -- releases Process.lock.
    ---------------------------------------------------------------------------
    procedure directSwitch (fromPID : ProcessID; toPID : ProcessID)
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        doSwitch : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            -- The IPC fast path bypasses Scheduler.enter, so it must perform
            -- the same eager state transition explicitly.
            if proctab(fromPID).mode = USER then
                saveFPUState (fromPID);
            end if;

            -- Update per-CPU state (what scheduler normally does)
            cpuData.currentPID     := toPID;
            cpuData.savedKernelRSP := proctab(toPID).kernelStackTop;
            cpuData.tss.rsp0       := proctab(toPID).kernelStackTop;

            -- Switch address space if target is user process
            if proctab(toPID).mode = USER then
                switchAddressSpace (toPID);
                restoreFPUState (toPID);
            end if;

            proctab(toPID).state := RUNNING;
            proctab(toPID).readiness := Rescheduled;

            -- The lock is transferred with the stack. No reaper can observe
            -- Leave_CPU until asm_switch_to has stopped using fromPID's stack.
            accountBoundary (fromPID, toPID, IPC_Handoff);
            noteContextStopped (fromPID);
            noteContextStarted (toPID);

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
    procedure saveFPUState (pid : ProcessID)
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        if proctab(pid).mode = USER then
            x86.fxsave (proctab(pid).fpu);
            clearLoadedState : declare
                cpuData : PerCPUData.PerCPUData with
                    Import, Volatile, Address => perCPUAddr;
            begin
                cpuData.fpuOwner := NO_PROCESS;
            end clearLoadedState;
        end if;
    end saveFPUState;

    ---------------------------------------------------------------------------
    -- restoreFPUState
    ---------------------------------------------------------------------------
    procedure restoreFPUState (pid : ProcessID)
    is
    begin
        if proctab(pid).mode = USER then
            -- FXRSTOR64 raises #NM while CR0.TS is set, so clear TS before
            -- restoring the process' always-valid initial/saved state image.
            enableFPU;
            x86.fxrstor (proctab(pid).fpu);
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
        Refined_State => (PIDTrackerState => (pidMap, pidLock))
    is

        -- TODO: this is basically cut-n-paste from the bootmem allocator.
        -- might be kind of nice to genericize these into a "bitmap" package

        -- Find a free PID and mark it as in use. Uses spinlock
        -- to ensure that two processes don't share the same PID
        -- if this were called by two threads at once.
        procedure allocPID(pid : out ProcessID)
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            pid := findFreePID;
            -- if no free PIDs, we'll mark PID 0 as used again, which is true.
            markUsed (pid);
            exitCriticalSection (pidLock);
        end allocPID;

        procedure allocSpecificPID (pid : in ProcessID)
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


        -- Mark a PID as free. Acquires pidLock for thread safety.
        procedure freePID(pid : in ProcessID)
        is
            use Spinlocks;
        begin
            enterCriticalSection (pidLock);
            markFree(pid);
            exitCriticalSection (pidLock);
        end freePID;


        -- Find a free PID
        function findFreePID return ProcessID
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
        procedure markUsed(pid : in ProcessID)
        is
            --block : constant PIDBlock := getBlock(pid);
            --offset : constant PIDOffset := getOffset(pid);
        begin
            --util.setBit(pidMap(block), offset);
            pidMap(pid) := False;
        end markUsed;


        -- Mark a PID as free.
        procedure markFree(pid : in ProcessID)
        is
            -- block : constant PIDBlock := getBlock(pid);
            -- offset : constant PIDOffset := getOffset(pid);
        begin
            --util.clearBit(pidmap(block), offset);
            pidMap(pid) := True;
        end markFree;


        -- -- Return the index into bitmap array in which this PID resides.
        -- function getBlock(pid : in ProcessID) return PIDBlock with
        --      is
        -- begin
        --     return Natural(pid / 64);
        -- end getBlock;


        -- -- Return the bit within a Unsigned_64 representing this single PID.
        -- function getOffset(pid : in ProcessID) return PIDOffset with
        --      is
        -- begin
        --     return Natural(pid mod 64);
        -- end getOffset;

    end PIDTracker;

end Process;
