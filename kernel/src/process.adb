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
with ELF_Admission;
with Interrupt_State;
with Locks;
with Mem_mgr;
with Page_Admission;
with PerCPUData;
with Process.Futex;
with Process.IPC;
with Process.User_Memory;
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
      (From_PID, To_PID : ThreadID; Boundary : Accounting_Boundary)
    is
        CPU : CPU_Accounting_Record renames
          cpuAccounting (PerCPUData.getCPUNumber);
        C : Accounting.Charge;
        Now : constant Unsigned_64 := x86.readOrderedTSC;
        -- Idle threads have the reserved idle PIDs' numbers.
        function Observed (PID : ThreadID) return Boolean is
          (PID /= NO_THREAD and then
           Natural (PID) not in Config.IDLE_PID_BASE .. Config.IDLE_PID_BASE + Config.MAX_SMP_CPUS - 1);
    begin
        Accounting.Transition
          (CPU.Clock, From_PID, To_PID, Now, C);
        if C.Accepted then
            if C.Charged_Owner = NO_THREAD then
                Accounting.Add_Time (CPU.Scheduler_Time, C.Ticks);
            else
                Accounting.Add_Time (threadtab (C.Charged_Owner).execution, C.Ticks);
                Scheduling_Turns.Charge (CPU.Turn, C.Ticks);
            end if;
            case Boundary is
                when Scheduler_Start =>
                    CPU.Reason := Relinquish;
                    if Scheduling_Turns.Remaining (threadtab (To_PID).savedTurn) > 0 then
                        Scheduling_Turns.Move (threadtab (To_PID).savedTurn, CPU.Turn);
                        Scheduling_Turns.Count (threadtab (To_PID).turnCounters,
                          Scheduling_Turns.Resumed_Dispatch);
                    elsif Time.tscPerDuration <= Unsigned_64'Last /
                      Scheduler_Timing.Quantum_Microseconds
                    then
                        CPU.Turn := Scheduling_Turns.Fresh
                          (Time.tscPerDuration * Scheduler_Timing.Quantum_Microseconds);
                        Scheduling_Turns.Count (threadtab (To_PID).turnCounters,
                          Scheduling_Turns.Fresh_Dispatch);
                    else
                        CPU.Turn := Scheduling_Turns.Empty;
                    end if;
                    Accounting.Dispatch (threadtab (To_PID).execution, Accounting.Scheduled);
                when IPC_Handoff =>
                    Trace.Emit (Trace.EVENT_IPC_HANDOFF,
                      Unsigned_64(From_PID), Unsigned_64(To_PID));
                    Accounting.Dispatch (threadtab (To_PID).execution, Accounting.Direct_IPC);
                when Scheduler_Stop =>
                    Scheduling_Turns.Count (threadtab (From_PID).turnCounters,
                      (case CPU.Reason is
                         when Higher_Priority => Scheduling_Turns.Higher_Preemption,
                         when Quantum_Expired => Scheduling_Turns.Quantum_Rotation,
                         when Awakened_Peer => Scheduling_Turns.Wake_Rotation,
                         when Relinquish => Scheduling_Turns.Relinquishment));
                    if CPU.Reason = Higher_Priority then
                        Scheduling_Turns.Move (CPU.Turn, threadtab (From_PID).savedTurn);
                    else
                        CPU.Turn := Scheduling_Turns.Empty;
                        threadtab (From_PID).savedTurn := Scheduling_Turns.Empty;
                    end if;
                when Accounting_Checkpoint => null;
            end case;
            if Build.OneShot_Scheduling and then To_PID /= NO_THREAD then
                if Build.Wakeup_Scheduling and then
                  Queues.hasAwakenedPeer (cpuReadyLists(PerCPUData.getCPUNumber),
                    threadtab (To_PID).priority)
                then
                    Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
                elsif Time.tscPerDuration > 0 and then
                  Scheduling_Turns.Remaining (CPU.Turn) > 0 and then
                  Queues.hasReadyPeer (cpuReadyLists(PerCPUData.getCPUNumber),
                    threadtab (To_PID).priority)
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
                                  (CPU.Shadow, threadtab (From_PID).shadow, Stamp,
                                   Scheduling_Shadow.Continue_Execution);
                            end if;
                        else
                            if Observed (From_PID) then
                                Scheduling_Shadow.Observe
                                  (CPU.Shadow, threadtab (From_PID).shadow, Stamp, Scheduling_Shadow.Stop);
                            end if;
                            if Observed (To_PID) then
                                Scheduling_Shadow.Observe
                                  (CPU.Shadow, threadtab (To_PID).shadow, Stamp, Scheduling_Shadow.Dispatch);
                            end if;
                        end if;
                    end;
                end if;
            end if;
        else
            CPU.Turn := Scheduling_Turns.Empty;
            if From_PID /= NO_THREAD then
                threadtab (From_PID).savedTurn := Scheduling_Turns.Empty;
            end if;
            if To_PID /= NO_THREAD then
                threadtab (To_PID).savedTurn := Scheduling_Turns.Empty;
            end if;
            if Build.Observe_Scheduling_Budgets then
                Scheduling_Shadow.Invalidate (CPU.Shadow);
            end if;
        end if;
    end accountBoundary;

    procedure printOwnAccounting is
        PID : constant ProcessID := PerCPUData.getCurrentPID;
        me : constant ThreadID := PerCPUData.getCurrentThread;
        CPU : constant Natural := PerCPUData.getCPUNumber;
        Snapshot : Accounting.Totals;
        Condition : Accounting.Health;
        Shadow : Scheduling_Shadow.Snapshot;
        Shadow_Health : Scheduling_Shadow.Health;
        Generation : Capabilities.Generation;
        Turns : Scheduling_Turns.Counters;
    begin
        Spinlocks.enterCriticalSection (lock);
        accountBoundary (PerCPUData.getCurrentThread, PerCPUData.getCurrentThread, Accounting_Checkpoint);
        Snapshot := threadtab (me).execution;
        Turns := threadtab (me).turnCounters;
        Condition := Accounting.Status (cpuAccounting(CPU).Clock);
        if Build.Observe_Scheduling_Budgets then
            Shadow := Scheduling_Shadow.Inspect (threadtab (me).shadow);
            Shadow_Health := Scheduling_Shadow.Status (cpuAccounting(CPU).Shadow);
            Generation := generationOf (PID);
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
        if ReaperPID /= NO_PROCESS and then threadOf (ReaperPID).state = SUSPENDED then
            ready (mainThreadOf (ReaperPID));
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

    procedure discardUnpublished (pid : ProcessID) is
        procedure deleteP4 is new Virtmem.deleteP4 (BuddyAllocator.freeFrame);
        procedure zeroize is new Virtmem.zeroize (Virtmem.P4);
    begin
        if pid = NO_PROCESS or else proctab(pid).admitted or else
          threadOf (pid).state /= SUSPENDED then
            raise ProcessException with "Invalid unpublished-process rollback";
        end if;
        -- No CPU has used this address space, and no endpoint has been opened.
        -- This is distinct from reclaimProcess's live-process retirement path.
        if proctab(pid).pgTable /= NO_PROCESS then
            Mem_mgr.unmapKernelMemFromProcess (addrtab(pid));
            deleteP4 (addrtab(pid));
            zeroize (addrtab(pid));
            proctab(pid).pgTable := NO_PROCESS;
        end if;
        while proctab(pid).frames.length > 0 loop
            BuddyAllocator.freeFrame (FrameLists.front (proctab(pid).frames));
            FrameLists.popFront (proctab(pid).frames);
        end loop;
        FrameLists.delete (proctab(pid).frames);
        if threadOf (pid).guardPage /= 0 then
            Mem_mgr.removeGuardPage (threadOf (pid).guardPage);
            BuddyAllocator.free (1, Virtmem.P2Va (threadOf (pid).guardPage));
            threadOf (pid).guardPage := 0;
        end if;
        threadOf (pid).kernelStack := null;
        threadOf (pid).kernelStackTop := System.Null_Address;
        threadOf (pid).context := System.Null_Address;
        threadOf (pid).fpu := System.Null_Address;
        Capabilities.Operations.clearTable (proctab(pid).caps);
        threadOf (pid).state := INVALID;
        -- No access to this slot after returning its PID to the allocator.
        PIDTracker.freePID (pid);
    end discardUnpublished;

    procedure noteContextStarted (tid : ThreadID) is
        OK : Boolean;
    begin
        Process_Lifetime.Enter_CPU (threadtab (tid).lifetime, OK);
        if not OK then
            raise ProcessException with "Dispatch of executing or retiring process";
        end if;
    end noteContextStarted;

    procedure noteContextStopped (tid : ThreadID) is
        OK : Boolean;
    begin
        Process_Lifetime.Leave_CPU (threadtab (tid).lifetime, OK);
        if not OK then
            raise ProcessException with "Context stop without execution presence";
        end if;
        if Process_Lifetime.Can_Reap (threadtab (tid).lifetime) then
            wakeReaper;
        end if;
    end noteContextStopped;

    procedure checkTermination is
        pid : constant ProcessID := PerCPUData.getCurrentPID;
        me : constant ThreadID := PerCPUData.getCurrentThread;
    begin
        if pid /= NO_PROCESS and then
           Process_Lifetime.Closing (threadtab (me).lifetime)
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
    addressSpaceLockName : aliased String := "address space";

    -- Threads other than main threads, system-wide (Process.lock). Capped
    -- so that thread creation can never take the thread IDs every process
    -- needs for its main thread: 255 stay available for those, above the
    -- reserved range.
    MAX_EXTRA_THREADS : constant Natural :=
      Natural (ThreadID'Last) - 15 - Natural (ProcessID'Last);
    extraThreads : Natural := 0;
    threadTableLockName : aliased String := "thread table";
    threadTableLock : Spinlocks.Spinlock;

    tableLockName : aliased String := "process table";
    tableLock : Spinlocks.Spinlock;

    procedure setup is
    begin
        -- Before AP startup / publication; never reinitialize live locks.
        Spinlocks.Initialize (lock, lockname'Access);
        Spinlocks.Initialize (grantLock, grantLockName'Access);
        Spinlocks.Initialize (tableLock, tableLockName'Access);
        Spinlocks.Initialize (threadTableLock, threadTableLockName'Access);
        Process_Table.Initialize;
        Thread_Table.Initialize;
        TextIO.enableOutputLocking;
        Spinlocks.Initialize (sleepList.lock, sleepListLockName'Access);
        -- ProcList.setup (allProcs, Config.MAX_PROCESSES);
        FrameLists.setup (Config.MAX_PROCESSES * Config.PAGES_PER_PROCESS);
        -- MsgQueue.setup (Config.MAX_PROCESSES);
    end setup;


    ---------------------------------------------------------------------------
    -- createKernelThread
    ---------------------------------------------------------------------------
    function createKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority) return ProcessID
    is
        newPID : ProcessID;
    begin
        if pid = NO_PROCESS then
            PIDTracker.allocPID (newPID);
            if newPID = NO_PROCESS then
                raise ProcessException with "createKernelThread: no free PID";
            end if;
        else
            PIDTracker.allocSpecificPID (pid);
            newPID := pid;
        end if;

        declare
            proc : Process renames proctab(newPID).E.all;
            thr  : Thread renames threadOf(newPID).E.all;
        begin
            proc.pid      := newPID;
            proc.ppid     := newPID;
            proc.admitted := True;
            proc.name     := name;
            thr.mode      := KERNEL;
            thr.state     := SUSPENDED;
            thr.priority  := priority;

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
                thr.guardPage   := Virtmem.V2P (baseVirt);
                thr.kernelStack :=
                    toKStackPtr (baseVirt + Virtmem.PAGE_SIZE);
                thr.kernelStack.canary := KSTACK_CANARY;
                -- Unmap the guard page so overflow triggers a page fault
                Mem_mgr.createGuardPage (thr.guardPage);
            end allocGuardedStack;
            thr.kernelStackTop := thr.kernelStack.all'Address + ProcessKernelStack'Size / 8;

            thr.kernelStack.filler := (others => 0);

            thr.kernelStack.interruptFrame := (
                    interruptNumber => 0,
                    rip             => procStart,
                    rsp             => thr.kernelStackTop,
                    rflags          => x86.FLAGS_INTERRUPT,
                    cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_CODE) or 0,
                    ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_KERNEL_DATA) or 0,
                    others          => 0
                );

            thr.kernelStack.returnAddress := interruptReturn'Address;
            thr.kernelStack.context       := (rip => start'Address, others => 0);

            thr.context := thr.kernelStack.context'Address;
        end;
        return newPID;
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
        newPID : constant ProcessID :=
          createKernelThread (procStart, name, pid, priority);
    begin
        threadOf(newPID).cpu := homeCPU;
        threadOf(newPID).pinned := True;
        resume (newPID);
    end startKernelThread;

    -- Boot-only, like startKernelThread: never the kernel stack of a
    -- retiring application.
    procedure startReaper is
        newPID : constant ProcessID := createKernelThread
          (retirementWorker'Address, "Reaper          ", NO_PROCESS, 5);
    begin
        threadOf(newPID).cpu := 0;
        threadOf(newPID).pinned := True;
        ReaperPID := newPID;
        resume (newPID);
    end startReaper;

    ---------------------------------------------------------------------------
    -- tryAddPage
    -- Allocate a page of memory for a process or thread's parent process, map
    -- at the specified address and adds the memory to the process or thread's
    -- parent process frame list so it will be freed on exit.
    ---------------------------------------------------------------------------
    procedure tryAddPage
      (proc : in out Process; mapTo : System.Address; storage : out System.Address;
       result : out Page_Allocation_Result;
       flags : Unsigned_64 := Virtmem.PG_USERDATA)
    is
        newFrame : Virtmem.PhysAddress;
        outcome : Page_Allocation.Result;
        frameOwner : constant ProcessID := proc.pid;
        frames : FrameLists.List renames proctab(frameOwner).frames;

        procedure mapPage is new Virtmem.mapPage (BuddyAllocator.allocFrame);

        procedure Track (Frame : Virtmem.PhysAddress; Accepted : out Boolean) is
        begin
            FrameLists.tryInsertFront (frames, Frame, Accepted);
        end Track;
        procedure Forget is
        begin
            FrameLists.popFront (frames);
        end Forget;
        procedure Claim (Frame : Virtmem.PhysAddress; Accepted : out Boolean) is
        begin
            BuddyAllocator.claimUserFrame (Frame, Unsigned_8 (frameOwner), Accepted);
        end Claim;
        procedure Map (Frame : Virtmem.PhysAddress; Accepted : out Boolean) is
        begin
            mapPage (Frame, To_Integer (mapTo), flags, addrtab(proc.pgTable), Accepted);
        end Map;
        procedure Acquire is new Page_Allocation.Acquire
          (Virtmem.PhysAddress, BuddyAllocator.allocFrame, BuddyAllocator.freeFrame,
           Track, Forget, Claim, Map);
    begin
        storage := System.Null_Address;
        if frames.length >= frames.capacity then
            result := Frame_Tracking_Full;
            return;
        elsif proctab(frameOwner).quota.maxFrames /= 0 and then
          frames.length >= proctab(frameOwner).quota.maxFrames then
            result := Frame_Quota_Full;
            return;
        elsif Virtmem.tableWalk (To_Integer (mapTo), addrtab(proc.pgTable)) /= 0 then
            result := Mapping_Already_Present;
            return;
        end if;
        Acquire (newFrame, outcome);
        result := Page_Allocation_Result (outcome);
        if result = Page_Added then
            storage := Virtmem.P2Va (newFrame);
        end if;
    end tryAddPage;

    ---------------------------------------------------------------------------
    -- create
    -- Writes directly to proctab(pid) to avoid 12KB stack allocation.
    ---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    -- allocGuardedKernelStack
    -- Two contiguous pages: an unmapped guard (lower) and the kernel stack.
    ---------------------------------------------------------------------------
    procedure allocGuardedKernelStack (tid : ThreadID; ok : out Boolean) is
        function toKStackPtr is new Ada.Unchecked_Conversion
            (System.Address, ProcessKernelStackPtr);
        baseVirt : System.Address;
        guarded : Boolean;
    begin
        ok := False;
        BuddyAllocator.alloc (1, baseVirt);
        if baseVirt = BuddyAllocator.NO_BLOCK_AVAILABLE then
            return;
        end if;
        Mem_mgr.tryCreateGuardPage (Virtmem.V2P (baseVirt), guarded);
        if not guarded then
            BuddyAllocator.free (1, baseVirt);
            return;
        end if;
        threadtab (tid).guardPage   := Virtmem.V2P (baseVirt);
        threadtab (tid).kernelStack :=
            toKStackPtr (baseVirt + Virtmem.PAGE_SIZE);
        threadtab (tid).kernelStack.canary := KSTACK_CANARY;
        threadtab (tid).kernelStackTop :=
            threadtab (tid).kernelStack.all'Address + ProcessKernelStack'Size / 8;
        ok := True;
    end allocGuardedKernelStack;

    -- Release a kernel stack from allocGuardedKernelStack.
    procedure freeGuardedKernelStack (tid : ThreadID) is
    begin
        if threadtab (tid).guardPage /= 0 then
            Mem_mgr.removeGuardPage (threadtab (tid).guardPage);
            BuddyAllocator.free (1, Virtmem.P2Va (threadtab (tid).guardPage));
            threadtab (tid).guardPage := 0;
        end if;
        threadtab (tid).kernelStack := null;
        threadtab (tid).kernelStackTop := System.Null_Address;
        threadtab (tid).context := System.Null_Address;
        threadtab (tid).fpu := System.Null_Address;
    end freeGuardedKernelStack;

    ---------------------------------------------------------------------------
    -- initializeUserEntry
    -- Build a user thread's first kernel stack: a clean FPU image, and an
    -- interrupt frame so the first dispatch enters user mode at entry with
    -- the given stack and first argument (RDI) through start and IRETQ.
    ---------------------------------------------------------------------------
    procedure initializeUserEntry (tid      : ThreadID;
                                   entryPoint : System.Address;
                                   userRSP  : System.Address;
                                   argument : Unsigned_64) is
    begin
        initializeFPUState (threadtab (tid).kernelStack.fpuarea);
        threadtab (tid).fpu := threadtab (tid).kernelStack.fpuarea'Address;
        threadtab (tid).kernelStack.filler := (others => 0);

        threadtab (tid).kernelStack.interruptFrame := (
                interruptNumber => 0,
                rip             => entryPoint,
                rsp             => userRSP,
                rdi             => argument,
                rflags          => x86.FLAGS_INTERRUPT,
                cs              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_CODE) or 3,
                ss              => Segment.GDTOffset'Enum_Rep(Segment.GDT_OFFSET_USER_DATA) or 3,
                others          => 0);

        threadtab (tid).kernelStack.returnAddress := interruptReturn'Address;
        threadtab (tid).kernelStack.context := (rip => start'Address, others => 0);

        threadtab (tid).context := threadtab (tid).kernelStack.context'Address;
    end initializeUserEntry;

    function create (procStart    : in System.Address;
                     ppid         : in ProcessID;
                     name         : in ProcessName;
                     priority     : in ProcessPriority;
                     procStack    : in System.Address;
                     stackSize    : in UserStackSize;
                     imageFrames  : in Natural;
                     requestedPID : in ProcessID := NO_PROCESS) return ProcessID

    is
        pid : ProcessID;
        reserved : Boolean;
        storage : System.Address;
        allocation : Page_Allocation_Result;

        procedure zeroize is new Virtmem.zeroize (Virtmem.P4);
        frameCapacity : constant Natural := ELF_Admission.Frame_Capacity
          (imageFrames, Positive (stackSize / Virtmem.PAGE_SIZE), INITIAL_HEAP_FRAME_HEADROOM);
    begin
        if frameCapacity = 0 then return NO_PROCESS; end if;
        if To_Integer (procStack) > To_Integer (PROCESS_STACK_TOP_VIRT) or else
          To_Integer (procStack) < Integer_Address (stackSize) or else
          To_Integer (procStack) mod Virtmem.PAGE_SIZE /= 0 or else
          stackSize mod Virtmem.PAGE_SIZE /= 0 then
            return NO_PROCESS;
        end if;
        if requestedPID /= NO_PROCESS then
            PIDTracker.tryAllocSpecificPID (requestedPID, reserved);
            if not reserved then return NO_PROCESS; end if;
            pid := requestedPID;
        else
            PIDTracker.allocPID (pid);
        end if;

        -- sanity checks
        if pid = 0 then
            return NO_PROCESS;
        end if;

        -- Clear the proctab entry before populating fields. The entry may
        -- contain stale data from a previously killed process. Preserve the
        -- capability generation counter so recycled PIDs don't reset to
        -- INITIAL_GENERATION (which would let stale caps pass gen checks).
        declare
            ignore   : System.Address;
        begin

            -- The table reset both records at allocation; reset again in
            -- case this PID's records were reserved without it.
            declare
                mainThread : constant ThreadID := proctab(pid).mainThread;
            begin
                resetProcessRecord (proctab(pid).E.all);
                proctab(pid).mainThread := mainThread;
                resetThreadRecord (threadOf(pid).E.all);
                threadOf(pid).process := pid;
            end;
            proctab(pid).requestSequence := IPC_Request_Ids.Initial_Sequence;
            threadOf (pid).lifetime := Process_Lifetime.Initial_State;
            threadOf (pid).execution := (others => <>);
            threadOf (pid).savedTurn := Scheduling_Turns.Empty;
            threadOf (pid).turnCounters := [others => 0];
            threadOf (pid).shadow := Scheduling_Shadow.Empty_Reservation;
            proctab(pid).admitted := False;
            -- Grant generations are namespaced by this life (the PID's
            -- ledger generation), so references to an earlier process
            -- with this PID never match (docs/threads.md).
            for slot in GrantID loop
                proctab(pid).grants(slot).generation :=
                  Memory_Grants.Life_Base (Memory_Grants.Process_Generation
                    (Process_Table.Generation_Of (pid)));
                proctab(pid).grants(slot).reusable := True;
            end loop;
        end;

        proctab(pid).pid          := pid;
        proctab(pid).ppid         := ppid;
        proctab(pid).threadCount  := 1;
        proctab(pid).svpid        := ppid;
        if ppid /= NO_PROCESS then
            proctab(pid).parentGeneration := generationOf (ppid);
        end if;
        proctab(pid).name         := name;
        threadOf (pid).mode         := USER;
        threadOf (pid).state        := SUSPENDED;
        threadOf (pid).priority     := priority;
        threadOf (pid).latency      :=
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
        declare
            stacked : Boolean;
        begin
            allocGuardedKernelStack (mainThreadOf (pid), stacked);
            if not stacked then
                discardUnpublished (pid);
                return NO_PROCESS;
            end if;
        end;

        FrameLists.create
            (proctab(pid).frames, frameCapacity);

        initializeUserEntry (mainThreadOf (pid), procStart,
                             proctab(pid).stackTop, 0);

        -- Set up the send/recv queues and the address space.
        proctab(pid).pgTable := pid;

        mailtab(pid).recvQueue := (
            lock => <>,
            head => NO_THREAD,
            tail => NO_THREAD
        );

        mailtab(pid).sendQueue := (
            lock => <>,
            head => NO_THREAD,
            tail => NO_THREAD
        );

        mailtab(pid).notifyQueue := (
            lock => <>,
            head => NO_THREAD,
            tail => NO_THREAD
        );

        Spinlocks.enterCriticalSection (mailtab(pid).lock);
        mailtab(pid).closed := True;
        mailtab(pid).ring := (others => <>);
        mailtab(pid).nextReceiveLane := Queued_Messages;
        Spinlocks.exitCriticalSection (mailtab(pid).lock);

        -- Grant initial capabilities for well-known services
        Capabilities.Operations.grantInitialCaps (
            table => proctab(pid).caps,
            pid   => Unsigned_64(pid),
            gen   => generationOf (pid));

        zeroize (addrtab(pid));
        Mem_mgr.mapKernelMemIntoProcess (addrtab(pid));
        -- Add a page for the process' stack
        tryAddPage (proctab(pid), procStack - Virtmem.PAGE_SIZE, storage, allocation);
        if allocation /= Page_Added then
            discardUnpublished (pid);
            return NO_PROCESS;
        end if;
        proctab(pid).numStackFrames := 1;

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

    -- An idle thread gives way when another CPU has work it could take. Idle
    -- CPUs find aged work on their timer opportunities (at most every
    -- millisecond); a fresh wakeup is never stealable, so it is not kicked.
    function idleWithStealableWork (tid : ThreadID; cpu : Natural) return Boolean is
      (Build.Work_Stealing and then threadtab (tid).priority < 0 and then
       stealableWorkElsewhere (cpu));

    procedure serviceReschedule is
        cpuData : PerCPUData.PerCPUData with Import, Volatile,
          Address => PerCPUData.getPerCPUDataAddr;
    begin
        if not cpuData.needReschedule then return; end if;
        Spinlocks.enterCriticalSection (lock);
        cpuData.needReschedule := False;
        if Build.OneShot_Scheduling and then Build.Wakeup_Scheduling and then
          cpuData.currentThread /= NO_THREAD and then
          Queues.hasAwakenedPeer (cpuReadyLists(cpuData.cpuNum),
            threadtab (cpuData.currentThread).priority)
        then
            Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
        end if;
        if cpuData.currentThread /= NO_THREAD and then
           threadtab (cpuData.currentThread).state = RUNNING and then
           (Queues.hasReadyPeer (cpuReadyLists(cpuData.cpuNum),
              threadtab (cpuData.currentThread).priority, Queues.Strictly_Higher) or else
            idleWithStealableWork (cpuData.currentThread, cpuData.cpuNum))
        then
            cpuAccounting(cpuData.cpuNum).Reason := Higher_Priority;
            Scheduler.enter;
        end if;
        Spinlocks.exitCriticalSection (lock);
    end serviceReschedule;

    procedure serviceTimerPreemption is
        PID : constant ProcessID := PerCPUData.getCurrentPID;
        me : constant ThreadID := PerCPUData.getCurrentThread;
        CPU : constant Natural := PerCPUData.getCPUNumber;
    begin
        if PID = NO_PROCESS then return; end if;
        Spinlocks.enterCriticalSection (lock);
        Scheduling_Turns.Count (threadtab (me).turnCounters, Scheduling_Turns.Timer_Opportunity);
        accountBoundary (PerCPUData.getCurrentThread, PerCPUData.getCurrentThread, Accounting_Checkpoint);
        if Queues.hasReadyPeer
          (cpuReadyLists(CPU), threadtab (me).priority, Queues.Strictly_Higher) or else
           idleWithStealableWork (PerCPUData.getCurrentThread, CPU)
        then
            cpuAccounting(CPU).Reason := Higher_Priority;
            Scheduler.enter;
        elsif Queues.hasReadyPeer (cpuReadyLists(CPU), threadtab (me).priority) and then
          (Scheduling_Turns.Remaining (cpuAccounting(CPU).Turn) = 0 or else
           (Build.Wakeup_Scheduling and then
            Queues.hasAwakenedPeer (cpuReadyLists(CPU), threadtab (me).priority)))
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
    procedure ready (tid : ThreadID)
    is
        ret : ThreadID;
        targetCPU : constant Natural := threadtab (tid).cpu;
        current : constant ThreadID := PerCPUData.getCurrentThread;
    begin
        if threadtab (tid).state = INVALID or else
           Process_Lifetime.Closing (threadtab (tid).lifetime)
        then
            return; -- A queued notification cannot restart a retiring task.
        end if;
        threadtab (tid).readyTSC := x86.rdtsc;
        threadtab (tid).queuedTSC := threadtab (tid).readyTSC;
        threadtab (tid).readiness := Awakened;
        Trace.Emit (Trace.EVENT_READY, Unsigned_64(tid), Unsigned_64(targetCPU));
        threadtab (tid).state := READY;
        Queues.insert (cpuReadyLists(targetCPU), tid,
                       threadtab (tid).priority, ret);

        if ret /= tid then
            raise ProcessException with "Process.ready: Error adding pid to ready list.";
        end if;

        if Build.OneShot_Scheduling and then Build.Wakeup_Scheduling and then
          targetCPU = PerCPUData.getCPUNumber and then current /= NO_THREAD and then
          threadtab (tid).priority >= threadtab (current).priority
        then
            Scheduler_Alarm.Request_Earlier (Scheduler_Timing.Wakeup_Microseconds);
        end if;

        -- If the newly readied process has higher priority than the
        -- currently running one, request preemption at interrupt return.
        -- Only meaningful if targeting THIS CPU.
        if targetCPU = PerCPUData.getCPUNumber and then
           current /= NO_THREAD and then
           threadtab (tid).priority > threadtab (current).priority
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


    function generationOf (pid : ProcessID) return Capabilities.Generation is
      (if pid = NO_PROCESS then 0
       else Capabilities.Generation (Process_Table.Generation_Of (pid)) +
            Capabilities.INITIAL_GENERATION);

    function threadGenerationOf (tid : ThreadID) return Capabilities.Generation is
      (if tid = NO_THREAD then 0
       else Capabilities.Generation (Thread_Table.Generation_Of (Natural (tid))) +
            Capabilities.INITIAL_GENERATION);

    ---------------------------------------------------------------------------
    -- Process table adapter
    ---------------------------------------------------------------------------
    procedure lockProcessTable is
    begin
        Spinlocks.enterCriticalSection (tableLock);
    end lockProcessTable;

    procedure unlockProcessTable is
    begin
        Spinlocks.exitCriticalSection (tableLock);
    end unlockProcessTable;

    procedure allocTablePage (Page_Bytes : Natural; Addr : out System.Address) is
    begin
        BuddyAllocator.alloc
          (BuddyAllocator.getOrder (Storage_Count (Page_Bytes)), Addr);
    end allocTablePage;

    procedure freeTablePage (Page_Bytes : Natural; Addr : System.Address) is
    begin
        BuddyAllocator.free
          (BuddyAllocator.getOrder (Storage_Count (Page_Bytes)), Addr);
    end freeTablePage;

    -- The creation-time zero state (see create), without any generation:
    -- generations live in the table's ledger.
    procedure resetProcessRecord (P : in out Process) is
        ignore : System.Address;
    begin
        ignore := Util.memset (P'Address, 0, Process'Size / 8);
        P.requestSequence := IPC_Request_Ids.Initial_Sequence;
        P.admitted := False;
        P.mainThread := NO_THREAD;
        -- A zeroed lock would read as held by CPU 0.
        Spinlocks.Initialize (P.addressSpaceLock, addressSpaceLockName'Access);
    end resetProcessRecord;

    procedure lockAddressSpace (pid : ProcessID) is
    begin
        Spinlocks.enterCriticalSection (proctab(pid).addressSpaceLock);
    end lockAddressSpace;

    procedure unlockAddressSpace (pid : ProcessID) is
    begin
        Spinlocks.exitCriticalSection (proctab(pid).addressSpaceLock);
    end unlockAddressSpace;

    procedure resetThreadRecord (T : in out Thread) is
        ignore : System.Address;
    begin
        ignore := Util.memset (T'Address, 0, Thread'Size / 8);
        T.lifetime := Process_Lifetime.Initial_State;
        T.execution := (others => <>);
        T.savedTurn := Scheduling_Turns.Empty;
        T.turnCounters := [others => 0];
        T.shadow := Scheduling_Shadow.Empty_Reservation;
        T.state := INVALID;
        T.process := NO_PROCESS;
    end resetThreadRecord;

    procedure lockThreadTable is
    begin
        Spinlocks.enterCriticalSection (threadTableLock);
    end lockThreadTable;

    procedure unlockThreadTable is
    begin
        Spinlocks.exitCriticalSection (threadTableLock);
    end unlockThreadTable;

    procedure reclaimTablePages is
    begin
        Process_Table.Reclaim (cpuOnline);
        Thread_Table.Reclaim (cpuOnline);
    end reclaimTablePages;

    ---------------------------------------------------------------------------
    -- stealableWorkElsewhere
    ---------------------------------------------------------------------------
    function stealableWorkElsewhere (cpu : Natural) return Boolean is
    begin
        for C in cpuReadyLists'Range loop
            if C /= cpu and then Queues.hasStealable (cpuReadyLists(C)) then
                return True;
            end if;
        end loop;
        return False;
    end stealableWorkElsewhere;

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
        threadOf (pid).latency :=
            (class    => class,
             periodUs => periodUs,
             budgetUs => budgetUs,
             flags    => flags);
    end setLatencyContract;


    ---------------------------------------------------------------------------
    -- suspend
    ---------------------------------------------------------------------------
    procedure suspend
    is
        me : constant ThreadID := PerCPUData.getCurrentThread;
    begin
        -- println ("Process.suspend: acquiring proctab lock");
        Spinlocks.enterCriticalSection (lock);

        -- Begin suspension and reschedule.
        threadtab (me).state := SUSPENDED;
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

        if Process_Lifetime.Closing (threadOf (pid).lifetime) then
            Spinlocks.exitCriticalSection (lock);
            return;
        end if;
        if threadOf (pid).state /= SUSPENDED then
            raise ProcessException with "Process.resume: Attempting to resume non-suspended process.";
        end if;

        ready (mainThreadOf (pid));

        -- println ("Process.resume: releasing proctab lock");
        Spinlocks.exitCriticalSection (lock);
    end resume;

    ---------------------------------------------------------------------------
    -- notify
    ---------------------------------------------------------------------------
    procedure notify (tid : ThreadID)
    is
        ignore : ProcessID;
    begin
        Spinlocks.enterCriticalSection (lock);

        if Process_Lifetime.Closing (threadtab (tid).lifetime) then
            Spinlocks.exitCriticalSection (lock);
            return;
        end if;
        if threadtab (tid).state = WAITINGFOREVENT or else
           threadtab (tid).state = WAITINGFORREPLY or else
           threadtab (tid).state = WAITINGFORCOMPLETION or else
           threadtab (tid).state = RECEIVING
        then
            ready (tid);
        elsif threadtab (tid).state = READY or else
              threadtab (tid).state = RUNNING
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
        me : constant ThreadID := PerCPUData.getCurrentThread;
        ignore : ThreadID;
    begin
        -- Publish the blocked state and hand off the running context under
        -- Process.lock. A remote wakeup cannot enqueue this task before its
        -- context has been saved by the scheduler.
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        threadtab (me).state := SLEEPING;

        Queues.insertDeltaNoLock (q            => sleepList,
                                  pid          => me,
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
        allocation : Page_Allocation_Result;

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
                       stackSize   => INIT_PROCESS_STACK_SIZE,
                       imageFrames => 1);

        if pid = NO_PROCESS then
            raise ProcessException with "Insufficient memory for bootstrap process";
        end if;

        -- add page to process, copy the init image to it
        tryAddPage (proc    => proctab(pid),
                 mapTo   => To_Address(0),
                 storage => alignedStart,
                 result  => allocation,
                 flags   => Virtmem.PG_USERCODE);
        if allocation /= Page_Added then
            discardUnpublished (pid);
            raise ProcessException with "Insufficient memory for bootstrap image";
        end if;

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
        p4addr := addrtab(pid)'Address;

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
        if proctab(pid).admitted and then threadOf (pid).mode = USER and then
           threadOf (pid).state /= INVALID and then
           (expectedGeneration = 0 or else
            expectedGeneration = generationOf (pid))
        then
            mailtab(pid).closed := True;
            -- A fault in any thread ends the process: stop every thread.
            declare
                t : ThreadID := mainThreadOf (pid);
            begin
                while t /= NO_THREAD loop
                    Process_Lifetime.Request_Stop (threadtab (t).lifetime);
                    threadtab (t).receiveDeadlineActive := False;
                    t := threadtab (t).nextSibling;
                end loop;
            end;
            wakeReaper;
            IPI.broadcastReschedule;
            accepted := True;
        end if;
        Spinlocks.exitCriticalSection (lock);
        Spinlocks.exitCriticalSection (mailtab(pid).lock);
        return accepted;
    end killProcess;

    -- Caller holds Process.lock. Every thread of p has stopped and awaits
    -- retirement (a process is reaped only when none of its threads can
    -- still be on a CPU using its address space).
    function allThreadsReapable (p : ProcessID) return Boolean is
        t : ThreadID := mainThreadOf (p);
    begin
        if t = NO_THREAD then
            return False;
        end if;
        while t /= NO_THREAD loop
            if not Process_Lifetime.Can_Reap (threadtab (t).lifetime) then
                return False;
            end if;
            t := threadtab (t).nextSibling;
        end loop;
        return True;
    end allThreadsReapable;

    -- Caller holds Process.lock; t can be reaped. Take it off every
    -- scheduler list. INVALID rejects administrative operations.
    procedure claimThread (t : ThreadID) is
        claimed : Boolean;
    begin
        Process_Lifetime.Claim_Reap (threadtab (t).lifetime, claimed);
        if not claimed then
            raise ProcessException with "Retirement claim lost under process lock";
        end if;
        threadtab (t).state := INVALID;
        Queues.detach (cpuReadyLists(threadtab (t).cpu), t);
        Queues.detach (sleepList, t, Queues.Delta_Queue);
    end claimThread;

    -- An exited thread of a live process: free it alone.
    procedure reclaimThread (t : ThreadID) is
        finished : Boolean;
    begin
        Futex.cancelWait (t);
        freeGuardedKernelStack (t);
        Spinlocks.enterCriticalSection (lock);
        Process_Lifetime.Finish_Reap (threadtab (t).lifetime, finished);
        if not finished then
            raise ProcessException with "Thread reclamation without retirement claim";
        end if;
        Thread_Table.Release (Natural (t));
        extraThreads := extraThreads - 1;
        Spinlocks.exitCriticalSection (lock);
    end reclaimThread;

    procedure retirementWorker is
        victim : ProcessID;
        victimThread : ThreadID;
    begin
        loop
            -- Free table pages emptied by earlier retirements whose grace
            -- period has passed (outside Process.lock; the table lock only
            -- nests the buddy allocator's lock).
            reclaimTablePages;
            victim := NO_PROCESS;
            victimThread := NO_THREAD;
            Spinlocks.enterCriticalSection (lock);
            for p in ProctabRange loop
                if proctab(p).admitted and then allThreadsReapable (p) then
                    -- Admission was closed before this claim; PID storage
                    -- remains reserved.
                    declare
                        t : ThreadID := mainThreadOf (p);
                    begin
                        while t /= NO_THREAD loop
                            claimThread (t);
                            t := threadtab (t).nextSibling;
                        end loop;
                    end;
                    victim := p;
                    exit;
                end if;
            end loop;
            if victim = NO_PROCESS then
                -- Threads that called THREAD_EXIT in still-live processes.
                for n in 1 .. Thread_Table.High_Water loop
                    if threadtab (ThreadID (n)).exiting and then
                       Process_Lifetime.Can_Reap (threadtab (ThreadID (n)).lifetime)
                    then
                        victimThread := ThreadID (n);
                        claimThread (victimThread);
                        -- Unlink from its process (never the main thread).
                        declare
                            p : constant ProcessID := processOf (victimThread);
                            t : ThreadID := mainThreadOf (p);
                        begin
                            while t /= NO_THREAD loop
                                if threadtab (t).nextSibling = victimThread then
                                    threadtab (t).nextSibling :=
                                      threadtab (victimThread).nextSibling;
                                    exit;
                                end if;
                                t := threadtab (t).nextSibling;
                            end loop;
                            proctab(p).threadCount := proctab(p).threadCount - 1;
                        end;
                        exit;
                    end if;
                end loop;
            end if;
            if victimThread /= NO_THREAD then
                Spinlocks.exitCriticalSection (lock);
                reclaimThread (victimThread);
                yield;
            elsif victim = NO_PROCESS then
                -- No polling and no lost wakeup: request/CPU-stop uses this
                -- same process lock to ready the suspended worker.
                threadOf (ReaperPID).state := SUSPENDED;
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
        declare
            saturated : Boolean;
        begin
            Process_Table.Invalidate (pid, saturated);
            pidReusable := not saturated;
        end;

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

        if threadOf (pid).mode = USER then

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

        -- Every thread: withdraw futex waits, then free its kernel stack
        -- (remapping the guard page so the buddy allocator can reuse it).
        declare
            t : ThreadID := mainThreadOf (pid);
        begin
            while t /= NO_THREAD loop
                Futex.cancelWait (t);
                freeGuardedKernelStack (t);
                t := threadtab (t).nextSibling;
            end loop;
        end;

        -- Retire before either immediate or grant-deferred PID publication.
        -- No access to proctab(pid) is allowed after publishing the PID free.
        -- Threads other than the main thread are released now; the main
        -- thread goes with the PID.
        Spinlocks.enterCriticalSection (lock);
        declare
            main : constant ThreadID := mainThreadOf (pid);
            t : ThreadID := main;
            following : ThreadID;
        begin
            while t /= NO_THREAD loop
                following := threadtab (t).nextSibling;
                Process_Lifetime.Finish_Reap (threadtab (t).lifetime, finished);
                if not finished then
                    raise ProcessException with "Reclamation without exclusive retirement claim";
                end if;
                if t /= main then
                    Thread_Table.Release (Natural (t));
                    extraThreads := extraThreads - 1;
                end if;
                t := following;
            end loop;
            threadtab (main).nextSibling := NO_THREAD;
            proctab(pid).threadCount := 0;
        end;
        proctab(pid).admitted := False;
        if grantDeferred then
            IPC.finishGrantProtectedTeardown (pid);
        elsif pidReusable then
            PIDTracker.freePID (pid, invalidated => True);
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
    -- createThread
    ---------------------------------------------------------------------------
    procedure createThread (entryPoint, userStack : System.Address;
                            argument, fsBase, clearTidAddress : Unsigned_64;
                            tid : out ThreadID)
    is
        USER_LIMIT : constant Unsigned_64 := 16#0000_8000_0000_0000#;
        me   : constant ThreadID := PerCPUData.getCurrentThread;
        pid  : constant ProcessID := processOf (me);
        main : ThreadID;
        n    : Natural;
        ok   : Boolean;

        procedure unreserve is
        begin
            Spinlocks.enterCriticalSection (lock);
            proctab(pid).threadCount := proctab(pid).threadCount - 1;
            extraThreads := extraThreads - 1;
            Spinlocks.exitCriticalSection (lock);
        end unreserve;
    begin
        tid := NO_THREAD;
        if pid = NO_PROCESS or else threadtab (me).mode /= USER or else
           To_Integer (entryPoint) = 0 or else
           Unsigned_64 (To_Integer (entryPoint)) >= USER_LIMIT or else
           To_Integer (userStack) = 0 or else
           Unsigned_64 (To_Integer (userStack)) > USER_LIMIT or else
           clearTidAddress mod 4 /= 0 or else clearTidAddress >= USER_LIMIT or else
           fsBase >= USER_LIMIT
        then
            return;
        end if;

        -- Reserve quota. The caller executes, so its process cannot be
        -- reaped during this call; it may be killed, which is rechecked.
        Spinlocks.enterCriticalSection (lock);
        if Process_Lifetime.Closing (threadtab (me).lifetime) or else
           proctab(pid).threadCount >= MAX_THREADS_PER_PROCESS or else
           extraThreads >= MAX_EXTRA_THREADS
        then
            Spinlocks.exitCriticalSection (lock);
            return;
        end if;
        proctab(pid).threadCount := proctab(pid).threadCount + 1;
        extraThreads := extraThreads + 1;
        Spinlocks.exitCriticalSection (lock);

        Thread_Table.Allocate (n);
        if n = 0 then
            unreserve;
            return;
        end if;
        tid := ThreadID (n);
        allocGuardedKernelStack (tid, ok);
        if not ok then
            Thread_Table.Release (n);
            unreserve;
            tid := NO_THREAD;
            return;
        end if;

        main := mainThreadOf (pid);
        threadtab (tid).process := pid;
        threadtab (tid).mode := USER;
        threadtab (tid).priority := threadtab (main).priority;
        threadtab (tid).latency := threadtab (main).latency;
        -- Start on this CPU; an idle CPU may steal it once it has aged.
        threadtab (tid).cpu := PerCPUData.getCPUNumber;
        threadtab (tid).pinned := False;
        threadtab (tid).fsBase := fsBase;
        threadtab (tid).clearTidAddress := clearTidAddress;
        threadtab (tid).state := SUSPENDED;
        initializeUserEntry (tid, entryPoint, userStack, argument);

        -- Publish under the lock a kill takes to stop every thread: either
        -- the kill sees this thread, or this sees the kill.
        Spinlocks.enterCriticalSection (lock);
        if Process_Lifetime.Closing (threadtab (me).lifetime) then
            Spinlocks.exitCriticalSection (lock);
            freeGuardedKernelStack (tid);
            Thread_Table.Release (n);
            unreserve;
            tid := NO_THREAD;
            return;
        end if;
        threadtab (tid).nextSibling := threadtab (main).nextSibling;
        threadtab (main).nextSibling := tid;
        ready (tid);
        Spinlocks.exitCriticalSection (lock);
    end createThread;

    ---------------------------------------------------------------------------
    -- exitThread
    ---------------------------------------------------------------------------
    procedure exitThread is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        pid : constant ProcessID := processOf (me);
        addr : constant Unsigned_64 := threadtab (me).clearTidAddress;
        ok : Boolean;
        ignore : Unsigned_64;
    begin
        if me = mainThreadOf (pid) then
            kill (pid);
        end if;

        -- Join support: clear the word, then wake whoever waits on it.
        if addr /= 0 then
            User_Memory.Store_Word32 (pid, addr, 0, ok);
            if ok then
                ignore := Futex.wakeFor (pid, addr, Unsigned_64'Last);
            end if;
        end if;
        IPC.retireThread (me);

        Spinlocks.enterCriticalSection (lock);
        threadtab (me).exiting := True;
        Process_Lifetime.Request_Stop (threadtab (me).lifetime);
        -- Leaving the CPU makes this thread reapable and wakes the reaper.
        Scheduler.enter;
        raise ProcessException with "Exited thread resumed";
    end exitThread;

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
    procedure kernelUserFault (pid : ProcessID; addr : System.Address;
                               handled : out Boolean)
    is
        ignore : System.Address;
        result : Page_Allocation_Result;
        use type Page_Admission.Decision;
        page : constant Integer_Address := To_Integer (addr) and Virtmem.PAGE_MASK;
    begin
        handled := False;
        if pid = NO_PROCESS or else threadOf (pid).mode /= USER or else
           Unsigned_64 (To_Integer (addr)) >= 16#0000_8000_0000_0000# or else
           proctab(pid).pgTable = NO_PROCESS or else
           -- This CPU already changing the address space: a kernel bug.
           Spinlocks.ownedBy (proctab(pid).addressSpaceLock,
                              Locks.CPU_ID (PerCPUData.getCPUNumber))
        then
            return;
        end if;
        lockAddressSpace (pid);
        if Virtmem.tableWalk (page, addrtab(proctab(pid).pgTable)) /= 0 then
            handled := True;
        elsif Page_Admission.Check
          (Unsigned_64 (To_Integer (proctab(pid).stackBottom)),
           Unsigned_64 (To_Integer (proctab(pid).stackTop)),
           Unsigned_64 (To_Integer (proctab(pid).heapStart)),
           Unsigned_64 (To_Integer (proctab(pid).heapEnd)),
           Unsigned_64 (To_Integer (addr)), proctab(pid).frames.length,
           proctab(pid).frames.capacity, Natural (proctab(pid).quota.maxFrames))
          = Page_Admission.Admitted
        then
            tryAddPage (proc => Proctab(pid), mapTo => To_Address (page),
                        storage => ignore, result => result);
            handled := result = Page_Added;
        end if;
        unlockAddressSpace (pid);
    end kernelUserFault;

    procedure pageFault (pid : ProcessID; addr : System.Address)
    is
        ignore : System.Address;
        result : Page_Allocation_Result := Page_Added;
        use type Page_Admission.Decision;
        admission : Page_Admission.Decision;
        page : constant Integer_Address := To_Integer (addr) and Virtmem.PAGE_MASK;
    begin
        -- Another thread of this process may be faulting on, or growing
        -- the heap over, the same page. Decide and map under the lock.
        lockAddressSpace (pid);
        admission := Page_Admission.Check
          (Unsigned_64 (To_Integer (proctab(pid).stackBottom)),
           Unsigned_64 (To_Integer (proctab(pid).stackTop)),
           Unsigned_64 (To_Integer (proctab(pid).heapStart)),
           Unsigned_64 (To_Integer (proctab(pid).heapEnd)),
           Unsigned_64 (To_Integer (addr)), proctab(pid).frames.length,
           proctab(pid).frames.capacity, Natural (proctab(pid).quota.maxFrames));
        if admission = Page_Admission.Admitted and then
           Virtmem.tableWalk (page, addrtab(proctab(pid).pgTable)) /= 0
        then
            -- A sibling thread mapped it first.
            unlockAddressSpace (pid);
            return;
        end if;
        if admission = Page_Admission.Admitted then
            tryAddPage (proc => Proctab(pid),
                        mapTo => To_Address (page),
                        storage => ignore, result => result);
        end if;
        unlockAddressSpace (pid);

        if admission = Page_Admission.Admitted then
            if result /= Page_Added then
                IPC.notifySupervisor
                  (pid => pid, faultLabel => IPC_Labels.EVENT_PROCESS_FAULT,
                   detail0 => 14, detail1 => Unsigned_64 (To_Integer (addr)),
                   detail2 => Unsigned_64 (Page_Allocation_Result'Pos (result)));
                kill (pid);
            end if;
        else
            -- @TODO use a heuristic here to figure out if this was a stack
            -- overflow, or heap over/underflow and signal the process either way.
            -- (something like distance to stackBottom < distance to heapEnd = stack overflow)
            print ("Process: Illegal memory access at "); print (addr);
            print (" rip "); print (lastFaultRIP);
            -- The words at the user stack pointer: usually return addresses.
            declare
                type Words is array (0 .. 7) of Unsigned_64;
                stack : Words := (others => 0);
                copied : Boolean;
            begin
                User_Memory.Copy (pid, Unsigned_64 (To_Integer (lastFaultRSP)),
                                  stack'Address, stack'Size / 8, copied);
                if copied then
                    print (" stack");
                    for w of stack loop
                        print (" "); print (To_Address (Integer_Address (w)));
                    end loop;
                end if;
            end;
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
    procedure directSwitch (fromT : ThreadID; toT : ThreadID)
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        doSwitch : declare
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            -- The IPC fast path bypasses Scheduler.enter, so it must perform
            -- the same eager state transition explicitly.
            if threadtab (fromT).mode = USER then
                saveUserCPUState (fromT);
            end if;

            -- Update per-CPU state (what scheduler normally does)
            cpuData.currentThread  := toT;
            cpuData.savedKernelRSP := threadtab (toT).kernelStackTop;
            cpuData.tss.rsp0       := threadtab (toT).kernelStackTop;

            -- Switch address space if target is user process
            if threadtab (toT).mode = USER then
                switchAddressSpace (processOf (toT));
                restoreUserCPUState (toT);
            end if;

            threadtab (toT).state := RUNNING;
            threadtab (toT).readiness := Rescheduled;

            -- The lock is transferred with the stack. No reaper can observe
            -- Leave_CPU until asm_switch_to has stopped using fromT's stack.
            accountBoundary (fromT, toT, IPC_Handoff);
            noteContextStopped (fromT);
            noteContextStarted (toT);

            -- asm_switch_to saves fromT's RSP and loads toT's RSP.
            -- Target resumes in yield() which releases Process.lock.
            switch (threadtab (fromT).context'Address,
                    threadtab (toT).context);

            -- Resumed: Process.lock is held (by whoever switched back)
        end doSwitch;
    end directSwitch;

    ---------------------------------------------------------------------------
    -- saveUserCPUState
    ---------------------------------------------------------------------------
    procedure saveUserCPUState (tid : ThreadID)
    is
        perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
    begin
        if threadtab (tid).mode = USER then
            x86.fxsave (threadtab (tid).fpu);
            threadtab (tid).fsBase := x86.rdfsbase;
            clearLoadedState : declare
                cpuData : PerCPUData.PerCPUData with
                    Import, Volatile, Address => perCPUAddr;
            begin
                cpuData.fpuOwner := NO_PROCESS;
            end clearLoadedState;
        end if;
    end saveUserCPUState;

    ---------------------------------------------------------------------------
    -- restoreUserCPUState
    ---------------------------------------------------------------------------
    procedure restoreUserCPUState (tid : ThreadID)
    is
    begin
        if threadtab (tid).mode = USER then
            -- FXRSTOR64 raises #NM while CR0.TS is set, so clear TS before
            -- restoring the process' always-valid initial/saved state image.
            enableFPU;
            x86.fxrstor (threadtab (tid).fpu);
            x86.wrfsbase (threadtab (tid).fsBase);
            -- KERNEL_GS_BASE is swapped in as the user GS base on return to
            -- ring 3. User GS is not supported; zero it so a value written by
            -- the previous process (WRGSBASE) cannot leak into this one.
            x86.wrmsr (x86.MSRs.KERNEL_GS_BASE, 0);
        end if;
    end restoreUserCPUState;

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
    -- PID allocation is the process table's (Id_Ledger, proved).
    package body PIDTracker is
        -- Give pid its main thread. The main thread takes the same number as
        -- its process when that is free (readable diagnostics); otherwise,
        -- because another process's thread holds it, any free thread ID.
        -- Nothing may rely on the numbers matching.
        procedure attachMainThread (pid : ProcessID; ok : out Boolean) is
            tid : Natural := Natural (pid);
        begin
            Thread_Table.Allocate_Specific (tid, ok);
            if not ok then
                Thread_Table.Allocate (tid);
                ok := tid /= 0;
            end if;
            if ok then
                proctab(pid).mainThread := ThreadID (tid);
                threadtab(ThreadID (tid)).process := pid;
            end if;
        end attachMainThread;

        procedure allocPID (pid : out ProcessID) is
            ok : Boolean;
        begin
            Process_Table.Allocate (pid);
            if pid /= NO_PROCESS then
                attachMainThread (pid, ok);
                if not ok then
                    Process_Table.Release (pid);
                    pid := NO_PROCESS;
                end if;
            end if;
        end allocPID;

        procedure tryAllocSpecificPID (pid : ProcessID; success : out Boolean) is
        begin
            if pid = NO_PROCESS then
                success := False;
                return;
            end if;
            Process_Table.Allocate_Specific (pid, success);
            if success then
                attachMainThread (pid, success);
                if not success then
                    Process_Table.Release (pid);
                end if;
            end if;
        end tryAllocSpecificPID;

        procedure allocSpecificPID (pid : in ProcessID) is
            success : Boolean;
        begin
            tryAllocSpecificPID (pid, success);
            if not success then
                raise ProcessException with "Attempted to use specific PID already in use";
            end if;
        end allocSpecificPID;

        procedure freePID (pid : in ProcessID; invalidated : Boolean := False) is
            tid : ThreadID;
        begin
            if pid /= NO_PROCESS then
                tid := proctab(pid).mainThread;
                if tid /= NO_THREAD then
                    Thread_Table.Release (Natural (tid));
                end if;
                Process_Table.Release (pid, Advance => not invalidated);
            end if;
        end freePID;
    end PIDTracker;

end Process;
