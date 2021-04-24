-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
--
-- We use slightly different terminology here from other OSes. A process that
-- is "sleeping" is waiting purely for a time-based wakeup. "Waiting" means a
-- resource the process needs is busy.
-------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with Descriptors;
with Devices;
with LinkedList;
with Spinlocks;
with Stackframe;
with StoragePools;
with Util;
with Virtmem;

-- Pragma Elaborate_All (Spinlocks);
Pragma Elaborate_All (Virtmem);
Pragma Elaborate_All (Descriptors);
Pragma Elaborate_All (Devices);

package Process with
    SPARK_Mode => On
is
    ProcessException : exception;

    subtype ProcessName is String (1..16);

    -- Process ID is just an index into the process table
    subtype ProcessID is Natural range 0..255;
    
    NO_PROCESS : constant ProcessID := 0;

    -- Limit a user-mode process to 256GiB of memory space. Later we'll add
    --  ASLR, and make the process' stack top some random negative offset
    --  from here.
    -- @TODO I think since RSP is decremented first when pushing, this needs to be 8000_0000_0000.
    PROCESS_STACK_TOP_VIRT     : constant Integer_Address := 16#0000_7FFF_FFFF_FFFF#;
    -- PROCESS_INITIAL_STACK_SIZE : constant Unsigned_64 := Virtmem.PAGE_SIZE;
    MAX_STACK_FRAMES           : constant := 256;   -- 1 MiB stack.
    
    subtype ProcessPriority is Integer range -1..100;
    type ProcessState is (INVALID, READY, RUNNING, SLEEPING, WAITING, RECEIVING, SUSPENDED);

    -- Wait channels are just a pointer to some resource that a process is
    -- waiting on.
    --@TODO add a wait message a la BSD
    subtype WaitChannel is System.Address;
    NO_CHANNEL : constant WaitChannel := Null_Address;

    type ProcessMode is (KERNEL, USER);

    type ExitCode is new Natural;

    ---------------------------------------------------------------------------
    -- Process mailbox for low-level IPC
    ---------------------------------------------------------------------------
    type Mailbox is record
        hasMsg  : Boolean     := False;
        message : Unsigned_64 := Unsigned_64'Last;
        sender  : ProcessID   := NO_PROCESS;        -- necessary?
    end record;

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
        rip : System.Address := To_Address(0);
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
    -- The ProcessKernelStack is a page of memory in the Kernel's
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
    -- @CAUTION
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
        interruptFrame  : Stackframe.InterruptStackFrame;
    end record with Size => virtmem.FRAME_SIZE * 8;

    type ProcessKernelStackPtr is access ProcessKernelStack;
    for ProcessKernelStackPtr'Simple_Storage_Pool use StoragePools.pool;

    -- Keep a list of the pages used by this process.
    package ProcessPageList is new LinkedList (System.Address);
    
    -- type UserStackPtr is access UserStack;
    -- for UserStackPtr'Simple_Storage_Pool use StoragePools.pool;

    ---------------------------------------------------------------------------
    -- Entry in the process table.
    -- @TODO unify the address type used here 
    -- @TODO consider making the pgTable entry an access type, may improve
    -- performance and take up less space in the proctab for unused slots.
    --
    -- @field pid             - Process ID of this process
    -- @field ppid            - Parent Process ID
    -- @field name            - short name of this process
    -- @field state           - state of the process
    -- @field mode            - KERNEL or USER mode
    -- @field priority        - user specified priority for this 
    -- @field dynPriority     - dynamic priority used to adjust time-slices
    -- @field pgTable         - top-level page table for this process' address space.
    -- @field context         - pointer to process' saved state
    -- @field kernelStack     - pointer to the process' kernel-mode stack
    -- @field frames          - list of physical frames this process has allocated
    -- @field numStackFrames  - number of stack frames this process is using.
    -- @field stackTop        - top of process' stack (lower-half address)
    -- @field brk             - top of gap/heap space
    -- @field numHeapFrames   - number of frames used for process' heap
    -- @field mail            - Process' IPC mailbox
    -- @field channel         - resource process may be waiting on
    -- @field openDescriptors - list of descriptors for kernel resources
    -- @field workingDirectory - what pwd would tell you
    ---------------------------------------------------------------------------
    type Process is
    record
        pid                 : ProcessID;        -- Index into the proctab
        ppid                : ProcessID;        -- If this is a thread, ppid will be
                                                --  the spawning process
        name                : ProcessName;
        state               : ProcessState;
        mode                : ProcessMode;

        priority            : ProcessPriority;
        dynPriority         : ProcessPriority;
        
        pgTable             : Virtmem.P4;       -- top-level page table for this process
        
        context             : System.Address;   -- Pointer to the saved state

        kernelStack         : ProcessKernelStackPtr;
        -- kernelStackSize     : Storage_Count := ProcessKernelStack'Size / 8;

        -- kernelStackBottom   : Integer_Address;  -- Pointer to process' kernel stack
        -- kernelStackTop      : Integer_Address;
        frames              : ProcessPageList.List;
        numStackFrames      : Natural := 0;

        -- The top of the process' stack address (lower-half)
        stackTop            : System.Address;

        -- The top of the process' kernel stack (higher-half)
        kernelStackTop      : System.Address;
        -- stackBottom         : Integer_Address;  -- limit of allowable stack
        -- stackBottomPhys     : Integer_Address;
        
        brk                 : System.Address;   -- limit of allowable heap
        numHeapFrames       : Natural := 0;

        -- For low-level IPC
        mail                : Mailbox;

        channel             : WaitChannel;

        openDescriptors     : Descriptors.DescriptorArray;

        workingDirectory    : Unsigned_64;          --@TODO make this VFS Inode
        -- workingDevice       : Devices.DeviceID;     --@TODO make this drive letter
    end record;

    -- Lock for protecting the proctab
    lockname : aliased String := "Proctab";
    lock : Spinlocks.spinlock := (name => lockname'Access, others => <>);

    type ProctabType is array (1..ProcessID'Last) of aliased Process;
    proctab : ProctabType;

    -- WIP: proctab replacement
    type ProcPtr is access all Process;
    for ProcPtr'Simple_Storage_Pool use StoragePools.pool;
    -- package ProcList is new LinkedList (ProcPtr, Process.print);
    -- allProcs : ProcList.List;

    ---------------------------------------------------------------------------
    -- startKernelThread
    --
    -- Starts a CuBit procedure in its own thread. Note, the PID must be
    -- explicitly specified here for kernel tasks, so this can clobber other
    -- processes if you're not careful.
    ---------------------------------------------------------------------------
    procedure startKernelThread (procStart  : in System.Address;
                                 name       : in ProcessName;
                                 pid        : in ProcessID;
                                 priority   : in ProcessPriority);

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
    -- @param procStack - the virutal process address for the top of its
    --  stack (i.e. what the user code sees)
    -- @return : new process with a unique PID. If the PID of the returned
    --  process is 0, the process is invalid, perhaps due to PID exhaustion or
    --  a failure to allocate memory for page tables.
    --
    -- @TODO: add an error code so it's apparent why process creation failed.
    ---------------------------------------------------------------------------
    function create (imageStart  : in Virtmem.PhysAddress;
                     imageSize   : in Unsigned_64;
                     procStart   : in System.Address;
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
    -- up when someone or something else calls goAhead on the channel.
    ---------------------------------------------------------------------------    
    procedure wait (channel : in WaitChannel; resourceLock : in out Spinlocks.spinlock);

    ---------------------------------------------------------------------------
    -- goAhead
    -- Set any processes waiting on the specified channel to READY.
    -- The opposite of "wait".
    ---------------------------------------------------------------------------
    procedure goAhead (channel : in WaitChannel);

    ---------------------------------------------------------------------------
    -- start
    -- This procedure is set as the return address for new processes. A new 
    --  process first scheduling will context switch here.
    -- This procedure releases the lock previously set by Scheduler.schedule
    ---------------------------------------------------------------------------
    procedure start with
        Pre => Spinlocks.isLocked(lock),
        Post => not Spinlocks.isLocked(lock);

    ---------------------------------------------------------------------------
    -- switch:
    -- Changes the current process context. When the interrupt handler returns,
    --  the new process will be running.
    --
    -- TODO: make separate type for a context address
    ---------------------------------------------------------------------------
    procedure switch (oldProc : in System.Address; newProc : in System.Address)
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
    procedure switchAddressSpace (processP4 : in System.Address);

    ---------------------------------------------------------------------------
    -- kill
    -- End this process
    -- @TODO ensure freeing all resources.
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID);

    ---------------------------------------------------------------------------
    -- send
    -- Put a message in a process' mailbox.
    ---------------------------------------------------------------------------
    procedure send (dest : ProcessID; msg : Unsigned_64);

    ---------------------------------------------------------------------------
    -- receive
    -- Receive a message from one's mailbox. Block if no message available.
    ---------------------------------------------------------------------------
    function receive return Unsigned_64;

    ---------------------------------------------------------------------------
    -- receiveNB
    -- Receive a message from one's mailbox, return Unsigned_64'Last if no
    -- message available.
    ---------------------------------------------------------------------------
    function receiveNB return Unsigned_64;

    ---------------------------------------------------------------------------
    -- getRunningProcess
    ---------------------------------------------------------------------------
    function getRunningProcess return ProcPtr;

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
        procedure allocPID (pid : out ProcessID) with
            Global => (In_Out => PIDTrackerState);

        -----------------------------------------------------------------------
        -- allocSpecificPID: mark a specific PID as in use
        --  @param in pid - new PID
        -- Will throw exception if given PID already in use.
        -- Protected with pidLock
        -----------------------------------------------------------------------
        procedure allocSpecificPID (pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState);

        -----------------------------------------------------------------------
        -- freePID: mark PID as free in bitmap. Not locked.
        -----------------------------------------------------------------------
        procedure freePID (pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState);

    private

        trackerLockName : aliased String := "PID Tracker Lock"
            with Part_Of => PIDTrackerState;
        --subtype PIDBlock is Natural range 0..(MAX_PID / 64);
        --subtype PIDOffset is Natural range 0..63;

        type PIDBitmapType is array (ProcessID) of Boolean
            with Pack;

        pidMap : PIDBitmapType := (0 => False, others => True)
            with Part_Of => PIDTrackerState;
        
        pidLock : Spinlocks.Spinlock := (name => trackerLockName'Access, others => <>)
            with Part_Of => PIDTrackerState;

        function findFreePID return ProcessID with
            Global => (Input => PIDTrackerState);

        procedure markUsed (pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState);

        procedure markFree (pid : in ProcessID) with
            Global => (In_Out => PIDTrackerState),
            Pre => pid /= 0;

        --function getBlock(pid : in ProcessID) return PIDBlock;
        --function getOffset(pid : in ProcessID) return PIDOffset;
    end PIDTracker;

end Process;