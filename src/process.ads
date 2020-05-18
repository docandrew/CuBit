-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
-------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with Spinlock;
with Stackframe;
with Util;
with Virtmem;

Pragma Elaborate_All (Spinlock);
Pragma Elaborate_All (Virtmem);

package Process with
    SPARK_Mode => On
is
    subtype ProcessName is String (1..16);

    -- Process ID is just an index into the process table
    subtype ProcessID is Natural range 0..255;
    
    NO_PROCESS : constant ProcessID := 0;

    -- Limit a process to 256GiB of memory space. Later we'll add
    --  ASLR, and make the process' stack top some random negative offset
    --  from here.
    PROCESS_STACK_TOP_VIRT : constant Integer_Address := 
        16#0000_7FFF_FFFF_FFFF#;

    PROCESS_INITIAL_STACK_SIZE: constant Unsigned_64 := Virtmem.PAGE_SIZE;
    
    --subtype ThreadID is Unsigned_64 range 0..MAX_TID;

    -- TODO: rethink how we want to identify kernel tasks vs processes.

    subtype ProcessPriority is Integer range -1..100;
    type ProcessState is (INVALID, READY, RUNNING, SLEEPING, WAITING, SUSPENDED);

    -- Wait channels are just a pointer to some resource that a process is
    -- waiting on.
    --@TODO add a wait message a la BSD
    type WaitChannel is new Unsigned_64;

    --type ProcessMode is (KERNEL, USER);

    type ExitCode is new Natural;

    ---------------------------------------------------------------------------
    -- Saved state of the process (registers) during context switch
    --  Caller-saved registers are already saved and on the stack, pushed there
    --  by the scheduler in the call to switch();
    ---------------------------------------------------------------------------
    type SavedState is
    record
        r15 : Unsigned_64 := 0;
        r14 : Unsigned_64 := 0;
        r13 : Unsigned_64 := 0;
        r12 : Unsigned_64 := 0;
        r11 : Unsigned_64 := 0;
        rbx : Unsigned_64 := 0;
        rbp : Unsigned_64 := 0;
        rip : Unsigned_64 := 0;
        -- rflags : Unsigned_64; ?
        -- TODO: Add space for XSAVE state (when FP is used), or just
        -- use a separate memory area for FP state.
    end record with Size => 64 * 8;

    for SavedState use
    record
        r15 at 0  range 0..63;
        r14 at 8  range 0..63;
        r13 at 16 range 0..63;
        r12 at 24 range 0..63;
        r11 at 32 range 0..63;
        rbx at 40 range 0..63;
        rbp at 48 range 0..63;
        rip at 56 range 0..63;
    end record;

    ---------------------------------------------------------------------------
    -- The ProcessKernelStack is a single frame of memory in the Kernel's
    --  address space. Our process will use the kernel stack during system 
    --  calls, and when it is being scheduled. This structure represents the
    --  _initial_ state of the kernel stack. We have to set up the fields here
    --  to look like we have previously interrupted and context switched to the
    --  process, so when we start the process for the first time it will
    --  "switch back" to the process.
    --
    -- We need the interrupt frame at the top addresses of this record, so we
    -- use filler at the "bottom." The alternative is blitting bytes and work
    -- backwards from the top of the stack, but Ada doesn't make that easy.
    --
    -- CAUTION:
    -- Pay attention to GNAT error messages about unused bytes here if the size
    -- of any of the structures in the initial kernel stack are changed.
    -- If there are unused bytes, adjust the size of PKStackFiller accordingly.
    --
    ---------------------------------------------------------------------------
    type PKStackFiller is array (1..3840) of Unsigned_8;
    pragma Pack (PKStackFiller);

    KSTACK_CANARY : constant Unsigned_64 := 16#1BAD_CA11_D37EC7ED#;

    type ProcessKernelStack is
    record
        canary          : Unsigned_64 := KSTACK_CANARY;
        filler          : PKStackFiller;
        context         : SavedState;       -- used in switch
        returnAddress   : System.Address;   -- when switch returns the first 
                                            -- time, it returns here
        interruptFrame  : stackframe.InterruptStackFrame;
    end record with Size => virtmem.FRAME_SIZE * 8;

    ---------------------------------------------------------------------------
    -- MemoryDescriptor : contains virtual address boundaries of each of the
    --  sections in a process' address space.
    --
    -- Lightweight Processes under a given Heavyweight Process will have all 
    --  fields the same except the stackTop and stackBottom.
    ---------------------------------------------------------------------------
    -- type MemoryDescriptor is
    -- record
    --     owner : ProcessID;
    --     pgTable : System.Address;    -- ptr to top-level page table

    --     -- Stack used by this process while in kernel mode.
    --     -- (during context switches, interrupts, syscalls)
    --     kernelStack : System.Address; -- the kernel stack base

    --     stackTop    : System.Address; -- top of stack
    --     stackBottom : System.Address; -- limit of allowable stack
    --     brk         : System.Address; -- top of the heap
    --     startBrk    : System.Address; -- bottom of the heap
    --     --endBSS      : System.Address; -- end of BSS segment
    --     --startBSS    : System.Address; -- start of BSS segment
    --     --endData     : System.Address; -- end of data segment
    --     --startData   : System.Address; -- start of data segment
    --     endText     : System.Address; -- end of text segment
    --     startText   : System.Address; -- start of text segment
    -- end record;

    ---------------------------------------------------------------------------
    -- Entry in the process table.
    -- TODO: unify the address type used here
    ---------------------------------------------------------------------------
    type Process is
    record
        pid                 : ProcessID;        -- Index into the proctab
        ppid                : ProcessID;        -- If this is a thread, ppid will be
                                                --  the spawning process
        name                : ProcessName;
        state               : ProcessState;

        priority            : ProcessPriority;
        dynPriority         : ProcessPriority;                  
        
        pgTable             : Virtmem.P4;       -- top-level page table for this process
        
        context             : System.Address;   -- Pointer to the saved state

        kernelStackBottom   : Integer_Address;   -- Pointer to process' kernel stack
        kernelStackTop      : Integer_Address;

        stackTop            : Integer_Address;
        stackBottom         : System.Address;   -- limit of allowable stack
        
        brk                 : System.Address;   -- limit of allowable heap
        startBrk            : System.Address;
    end record;

    -- Lock for protecting the proctab
    lock : Spinlock.spinlock;

    type ProctabType is array (1..ProcessID'Last) of Process;
    proctab : ProctabType;

    ---------------------------------------------------------------------------
    -- create:
    --
    -- Creates a new process in the READY state. It does NOT add it to the
    -- proctab.
    --
    -- @param imageStart - the physical address (should be page-aligned) where
    --  the process has been loaded into physical memory
    -- @param imageSize - the size of the process' image
    -- @param procStart - the virtual process address where execution should
    --  start.
    -- @param ppid - parent PID
    -- @param name - process name
    -- @return : new process with a unique PID. If the PID of the returned
    --  process is 0, the process is invalid, perhaps due to PID exhaustion or
    --  a failure to allocate memory for page tables.
    --
    -- @TODO: add an error code so it's apparent why process creation failed.
    ---------------------------------------------------------------------------
    function create(imageStart  : in Virtmem.PhysAddress;
                    imageSize   : in Unsigned_64;
                    procStart   : in Virtmem.VirtAddress;
                    ppid        : in ProcessID;
                    name        : in ProcessName;
                    priority    : in ProcessPriority;
                    procStack   : in Virtmem.VirtAddress) return Process;

    ---------------------------------------------------------------------------
    -- yield
    -- Yield this process' execution back to the scheduler.
    -- Called by the running process itself after an interrupt.
    ---------------------------------------------------------------------------
    procedure yield;


    ---------------------------------------------------------------------------
    -- wait
    -- Pause execution of this process while waiting for some resource. We'll
    -- associate the resource with a spinlock to provide a mutex. This will
    -- ExitCriticalSection and begin waiting for the scheduler to wake us back
    -- up, and EnterCriticalSection back when we're done waiting.
    ---------------------------------------------------------------------------    
    procedure wait(channel : in WaitChannel; resourceLock : in out Spinlock.spinlock);

    ---------------------------------------------------------------------------
    -- start
    -- This procedure is set as the return address for new processes. A new 
    --  process first scheduling will context switch here.
    -- This procedure releases the lock previously set by Scheduler.schedule
    ---------------------------------------------------------------------------
    procedure start with
        Pre => spinlock.isLocked(lock),
        Post => not spinlock.isLocked(lock);

    ---------------------------------------------------------------------------
    -- switch:
    -- Changes the current process context. When the interrupt handler returns,
    --  the new process will be running.
    --
    -- TODO: make separate type for a context address
    ---------------------------------------------------------------------------
    procedure switch(oldProc : in System.Address; newProc : in System.Address)
        with Import => True, Convention => C, External_Name => "asm_switch_to";

    ---------------------------------------------------------------------------
    -- createFirstProcess:
    -- Since we can't call a syscall from kernel mode, we need to create a stub
    -- for the first user process (init) here and it can perform the syscall to
    -- load the actual init binary and start running it. See init.asm. 
    ---------------------------------------------------------------------------
    procedure createFirstProcess;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    -- Given the address of a process' P4 table, make that one the currently
    -- active table, changing the virtual memory address space in use.
    ---------------------------------------------------------------------------
    procedure switchAddressSpace(processP4 : in System.Address);

private
    ---------------------------------------------------------------------------
    -- Ring 3 entry point in interrupt.asm. Processes will initially set
    -- their return address here, so when we "return" to the process, they'll 
    -- clean up the "interrupt" and begin executing in the process context
    -- upon return from interrupt.
    ---------------------------------------------------------------------------
    interruptReturn : Util.Symbol 
        with Import, Convention => C, External_Name => "interruptReturn";

    ---------------------------------------------------------------------------
    -- PIDTracker
    -- Package w/ bitmap and locks to safely allocate and free PIDs
    -- In the pidMap, "True" means available, "False" means not available.
    ---------------------------------------------------------------------------
    package PIDTracker with
        Abstract_State => PIDTrackerState
    is
        -----------------------------------------------------------------------
        -- allocPID: find a free PID, mark it as in use
        --  out param pid - new PID, 0 if none free.
        -- Protected with pidLock
        -----------------------------------------------------------------------
        procedure allocPID(pid : out ProcessID) with
            Global => (In_Out => PIDTrackerState);

        -----------------------------------------------------------------------
        -- freePID: mark PID as free in bitmap. Not locked.
        -----------------------------------------------------------------------
        procedure freePID(pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState);

    private

        --subtype PIDBlock is Natural range 0..(MAX_PID / 64);
        --subtype PIDOffset is Natural range 0..63;

        type PIDBitmapType is array (ProcessID) of Boolean
            with Pack;

        pidMap : PIDBitmapType := (0 => False, others => True)
            with Part_Of => PIDTrackerState;
        
        pidLock : spinlock.Spinlock
            with Part_Of => PIDTrackerState;

        function findFreePID return ProcessID with
            Global => (Input => PIDTrackerState);

        procedure markUsed(pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState);

        procedure markFree(pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState),
            Pre => pid /= 0;

        --function getBlock(pid : in ProcessID) return PIDBlock;
        --function getOffset(pid : in ProcessID) return PIDOffset;
    end PIDTracker;

end Process;