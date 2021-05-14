-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CuBitOS Processes
--
-- We use slightly different terminology here from other OSes. A process that
-- is "sleeping" is waiting purely for a time-based wakeup. "Waiting" means a
-- resource the process needs is busy.
--
-- The term process and thread will be synonomous in CuBit once shared address
-- spaces and per-thread stacks are implemented (@TODO)
--
-- The main data structure here is an array of processes, the Proctab or
-- "process table". Every process can be on at most one of several lists - the
-- READY list for processes that are runnable, the SLEEPING list for processes
-- that are waiting for a time delay, or a per-process list for threads waiting
-- to send a message to that process, or threads waiting to receive messages
-- destined for that process.
--
-- SUSPENDED processes are not a part of any list. INVALID processes are not
-- active on any list either.
--
-- These lists are woven throughout the proctab itself, using the next and prev
-- fields of each Process object. By starting at one of the list heads and
-- utilizing these fields, the individual lists can be traversed.
--
-- The proctab itself is protected by a lock (Process.lock) which ensures the
-- proctab can be manipulated by only a single thread at a time.
-------------------------------------------------------------------------------
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

with Descriptors;
with LinkedLists;
limited with Process.Queues;
with Spinlocks;
with Stackframe;
with StoragePools;
with TextIO;
with Time;
with Util;
with Virtmem;

Pragma Elaborate_All (Virtmem);
Pragma Elaborate_All (Descriptors);

package Process with
    SPARK_Mode => On
is
    pool : StoragePools.StoragePool;

    ProcessException : exception;

    subtype ProcessName is String(1..16);

    -- Process ID is just an index into the process table, address table and
    -- mail table.
    subtype ProcessID is Natural range 0..255;
    
    NO_PROCESS : constant ProcessID := 0;

    -- Limit a user-mode process to 256GiB of memory space. Later we'll add
    --  ASLR, and make the process' stack top some random negative offset
    --  from here.
    PROCESS_STACK_TOP_VIRT : constant System.Address := To_Address (16#0000_8000_0000_0000#);
    STACK_SIZE             : constant Storage_Count := 16#1_000_000#;

    MAX_STACK_FRAMES       : constant := STACK_SIZE / Virtmem.FRAME_SIZE;   -- 1 MiB stack
    MAX_HEAP_FRAMES        : constant := STACK_SIZE / Virtmem.FRAME_SIZE;   -- 1 MiB heap

    BAD_HEAP_ADDRESS       : constant System.Address := To_Address (16#DEAD_DEAD_DEAD_DEAD#);
    
    -- Secondary stack starts at the bottom of the stack and eats upwards into it.
    --
    -- @TODO this is fine for ZFP user runtime for now, but eventually we'll
    -- get rid of this in favor of the real GNAT runtime.
    SECONDARY_STACK_START  : constant System.Address := PROCESS_STACK_TOP_VIRT - STACK_SIZE;

    subtype ProcessPriority is Integer range -1..100;

    type ProcessState is (
        INVALID,                    -- Entry in proctab does not refer to process
        READY,                      -- Process can be made active
        RUNNING,                    -- Active process
        SLEEPING,                   -- Waiting for time to elapse
        WAITING,                    -- Waiting on resource @TODO replace this
        WAITINGFOREVENT,            -- Waiting on an event
        SENDING,                    -- Queued for message delivery
        RECEIVING,                  -- Queued for message receipt
        WAITINGFORREPLY,            -- Receiver got message, sender waiting for reply
        SUSPENDED                   -- Suspended until a resume call.
    );

    -- Wait channels are just a pointer to some resource that a process is
    -- waiting on.
    --@TODO replace this with IPC primitives
    subtype WaitChannel is System.Address;
    NO_CHANNEL : constant WaitChannel := Null_Address;

    type ProcessMode is (KERNEL, USER);

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

    type FPUState is array (1..512) of Unsigned_8 with Convention => C;

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
    type PKStackFiller is array (1..3328) of Unsigned_8;
    pragma Pack (PKStackFiller);

    KSTACK_CANARY : constant Unsigned_64 := 16#1BAD_CA11_D37EC7ED#;

    ---------------------------------------------------------------------------
    -- @field fpuarea - 512 bytes reserved for FXSAVE (only if this process
    --  uses FPU instructions.)
    -- @field canary - @TODO will be used to detect kernel stack overflow
    ---------------------------------------------------------------------------
    type ProcessKernelStack is
    record
        fpuarea         : FPUState;
        canary          : Unsigned_64 := KSTACK_CANARY;
        filler          : PKStackFiller;
        context         : SavedState;       -- used in switch
        returnAddress   : System.Address;   -- when switch returns the first 
                                            -- time, it returns here
        interruptFrame  : Stackframe.InterruptStackFrame;
    end record with Size => virtmem.FRAME_SIZE * 8;

    type ProcessKernelStackPtr is access ProcessKernelStack;
    for ProcessKernelStackPtr'Simple_Storage_Pool use pool;

    -- Keep a list of the frames used by this process.
    package FrameLists is new LinkedLists (Virtmem.PhysAddress, TextIO.print);

    ---------------------------------------------------------------------------
    -- Process Queues. See package Process.Queues for use and more information.
    -- Head/Tail of each of the various lists a process can be on
    ---------------------------------------------------------------------------
    type ProcQueue is
    record
        lock : Spinlocks.Spinlock;
        head : ProcessID;
        tail : ProcessID;
    end record;

    -- @TODO move to a per-CPU ready list.
    readyListLockName : aliased String := "Ready List";
    readyList : ProcQueue := (
        lock => (name => readyListLockName'Access, others => <>),
        head => NO_PROCESS,
        tail => NO_PROCESS
    );

    sleepListLockName : aliased String := "Sleep List";
    sleepList : ProcQueue := (
        lock => (name => sleepListLockName'Access, others => <>),
        head => NO_PROCESS,
        tail => NO_PROCESS
    );
    
    ---------------------------------------------------------------------------
    -- Process mailboxes for low-level IPC
    -- @field sendQueue       - List of blocked senders waiting to send
    --                          this mailbox a message.
    -- @field recvQueue       - List of the blocked receivers waiting
    --                          to receive a message destined for this mailbox
    ---------------------------------------------------------------------------
    type Mailbox is record
        lock        : Spinlocks.spinlock := (name => null, others => <>);

        hasMsg      : Boolean      := False;
        message     : Unsigned_64  := Unsigned_64'Last;
        sender      : ProcessID    := NO_PROCESS;

        hasEvent    : Boolean      := False;
        event       : Unsigned_64  := Unsigned_64'Last;
        eventSender : ProcessID    := NO_PROCESS;

        sendQueue   : ProcQueue;
        recvQueue   : ProcQueue;
    end record;

    ---------------------------------------------------------------------------
    -- Entry in the process table.
    --
    -- @field next            - Process ID of the next process on whatever list
    --                          this process belongs to.
    -- @field prev            - Process ID of the previous process on whatever
    --                          list this process belongs to.
    -- @field queueKey        - For keeping elements in a queue in sorted order.
    --                          This will be the process' priority for
    --                          the ready list, or the delay for the sleep list.
    --                          If this is a thread on another process' sendQueue
    --                          or recvQueue, the queueKey will be that process' PID.
    -- @field isThread        - True if this process is a child thread of some
    --                          other parent process, sharing the parent's 
    --                          address space, descriptors and 
    -- @field pid             - Process ID of this process
    -- @field ppid            - Parent Process ID
    -- @field name            - short name of this process
    -- @field state           - state of the process
    -- @field mode            - KERNEL or USER mode
    -- @field priority        - user specified priority for this 
    -- @field pgTable         - index to top-level page table for this process' address space.
    -- @field context         - pointer to process' saved state
    -- @field kernelStack     - pointer to the process' kernel-mode stack
    -- @field frames          - list of physical frames this process has allocated
    -- @field numStackFrames  - number of stack frames this process is using.
    -- @field stackTop        - top of process' stack (lower-half address)
    -- @field brk             - top of gap/heap space
    -- @field numHeapFrames   - number of frames used for process' heap
    -- These next two fields need to be set by the process loader.
    -- @field iend            - end of the process' image (.text, .bss, .data)
    -- @field istart          - start of the process' image
    -- @field mail            - Index into the process' IPC mailbox
    -- @field reply           - Return value of a send() call stored here
    -- @field channel         - resource process may be waiting on
    -- @field openDescriptors - list of descriptors for kernel resources
    -- @field workingDirectory - what pwd would tell you
    -- @field fpu             - Set to address of the kernel stack FPU save area
    --                          if this process uses FPU/MMX/SSE, null otherwise.
    ---------------------------------------------------------------------------
    type Process is
    record
        next                : ProcessID := NO_PROCESS;
        prev                : ProcessID := NO_PROCESS;

        queueKey            : Integer;

        isThread            : Boolean := False;
        pid                 : ProcessID;        -- Index into the proctab
        ppid                : ProcessID;        -- Parent process ID

        name                : ProcessName;
        state               : ProcessState := SUSPENDED;
        mode                : ProcessMode;

        priority            : ProcessPriority;
        
        pgTable             : ProcessID;        -- Index into addrtab
        
        context             : System.Address;   -- Pointer to the saved state

        kernelStack         : ProcessKernelStackPtr;

        frames              : FrameLists.List;
        numStackFrames      : Natural := 0;

        -- The top of the process' kernel stack (higher-half)
        kernelStackTop      : System.Address;

        -- The top of the process' stack address (lower-half)
        stackTop            : System.Address;

        -- The bottom of the process' stack
        stackBottom         : System.Address;
        
        -- Heap
        heapEnd             : System.Address;
        heapStart           : System.Address;

        -- Start and end of process' image (.text, .data, .bss).
        iend                : System.Address := To_Address (0);
        istart              : System.Address := To_Address (16#FFFF_FFFF_FFFF_FFFF#);

        -- For low-level IPC
        mail                : ProcessID;
        reply               : Unsigned_64;

        channel             : WaitChannel;

        openDescriptors     : Descriptors.DescriptorArray;

        workingDirectory    : Unsigned_64;          --@TODO make this VFS Inode
        -- workingDevice       : Devices.DeviceID;     --@TODO make this drive letter
        fpu                 : System.Address := System.Null_Address;
    end record;

    -- Lock for protecting the proctab
    lockname : aliased String := "Proctab";
    lock : Spinlocks.Spinlock := (name => lockname'Access, others => <>);

    ---------------------------------------------------------------------------
    -- Proctab. Array of Process entries and master list of active processes in
    -- CuBit.
    ---------------------------------------------------------------------------
    type ProctabType is array (1..ProcessID'Last) of Process;
    proctab : ProctabType;

    ---------------------------------------------------------------------------
    -- Addrtab. Array of Address spaces. Individual processes will have an index
    -- into this object. Child threads of a parent process will all index to
    -- the same entry here.
    ---------------------------------------------------------------------------
    type AddrtabType is array (1..ProcessID'Last) of Virtmem.P4;
    addrtab : AddrtabType;

    ---------------------------------------------------------------------------
    -- Mailtab. Array of mailboxes. Processes will have an index into this
    -- object. Child threads of a parent process will all index to the same
    -- entry here.
    ---------------------------------------------------------------------------
    type MailtabType is array (1..ProcessID'Last) of Mailbox;
    mailtab : MailtabType;

    -- WIP: proctab replacement
    type ProcPtr is access all Process;
    -- for ProcPtr'Simple_Storage_Pool use pool;
    -- package ProcLists is new LinkedLists (ProcPtr, Process.print);
    -- allProcs : ProcLists.List;

    ---------------------------------------------------------------------------
    -- setup
    -- Allocate storage used for the FrameList package
    ---------------------------------------------------------------------------
    procedure setup with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- addToProctab
    -- Given a process object, add it to the process table.
    ---------------------------------------------------------------------------
    procedure addToProctab (proc : in Process) with SPARK_Mode => On;

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
                                 priority   : in ProcessPriority)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- addPage
    -- 
    -- Map a page into this process' address space at the specified virtual
    -- address with the given flags. This procedure will allocate memory for
    -- the page, which can be accessed via the storage param if bytes need to
    -- be copied into it, zeroed out, etc. If the process given is a child thread,
    -- the memory will be mapped into the parent process' memory space.
    --
    -- @param Process
    -- @param mapTo - Process virtual address that this page should be mapped to
    -- @param storage - Linear-mapped address of the underlying storage allocated
    --  for this page.
    -- @param flags - Page Table flags for this mapping.
    ---------------------------------------------------------------------------
    procedure addPage (proc    : in out Process;
                       mapTo   : in System.Address;
                       storage : out System.Address;
                       flags   : in Unsigned_64 := Virtmem.PG_USERDATA)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- create:
    --
    -- Creates a new process or thread in the SUSPENDED state. This function
    -- does NOT add the process to the proctab.
    --
    -- @param procStart - the virtual process address where execution should
    --  start.
    -- @param ppid - parent PID
    -- @param name - process name
    -- @param procStack - the virtual process address for the top of its
    --  stack (i.e. what the user code sees)
    -- @param thread - If true, this process will share an address space with
    --  the parent process (given via ppid). Ensure procStack is set up
    --  appropriately.
    -- @return : new process with a unique PID. If the PID of the returned
    --  process is 0, the process is invalid, perhaps due to PID exhaustion or
    --  a failure to allocate memory for page tables.
    --
    -- @TODO add an error code so it's apparent why process creation failed.
    ---------------------------------------------------------------------------
    function create (procStart   : in System.Address;
                     ppid        : in ProcessID;
                     name        : in ProcessName;
                     priority    : in ProcessPriority;
                     procStack   : in System.Address;
                     thread      : in Boolean := False) return Process
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- yield
    -- Yield this process' execution back to the scheduler.
    -- Called by the running process itself after an interrupt.
    ---------------------------------------------------------------------------
    procedure yield with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- ready
    -- Move a process into the ready list and change its state to READY
    ---------------------------------------------------------------------------
    procedure ready (pid : ProcessID) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- wait
    -- Pause execution of this process while waiting for some resource. We'll
    -- associate the resource with a spinlock to provide a mutex. This will
    -- ExitCriticalSection and begin waiting for the scheduler to wake us back
    -- up when someone or something else calls goAhead on the channel.
    -- @TODO replace this in favor of using IPC for resource synchronization
    ---------------------------------------------------------------------------    
    procedure wait (channel : in WaitChannel; resourceLock : in out Spinlocks.spinlock)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- goAhead
    -- Set any processes waiting on the specified channel to READY.
    -- The opposite of "wait".
    ---------------------------------------------------------------------------
    procedure goAhead (channel : in WaitChannel) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- suspend
    -- Place the current PID in a SUSPENDED state and reschedule.
    ---------------------------------------------------------------------------
    procedure suspend with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- resume
    -- Move the given PID from SUSPENDED to READY state
    ---------------------------------------------------------------------------
    procedure resume (pid : ProcessID) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- inform
    -- Move the given PID from WAITINGFOREVENT to READY state
    ---------------------------------------------------------------------------
    procedure inform (pid : ProcessID) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- sleep
    -- Put the running process to sleep for the specified duration.
    -- @NOTE delta queue uses signed integer for the ms delay, so we're limited
    -- by how long the delay can be here.
    ---------------------------------------------------------------------------
    procedure sleep (us : Time.Duration) with SPARK_Mode => On,
        Pre => (us <= 2147483647 * Time.Milliseconds);

    ---------------------------------------------------------------------------
    -- start
    -- This procedure is set as the return address for new processes. A new 
    --  process first scheduling will context switch here.
    -- This procedure releases the lock previously set by Scheduler.schedule
    ---------------------------------------------------------------------------
    procedure start with
        SPARK_Mode => On,
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
    procedure createFirstProcess
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- switchAddressSpace
    -- Given a process ID, make the corresponding entry in the addrtab the
    -- currently active table, changing the virtual memory address space in use.
    ---------------------------------------------------------------------------
    procedure switchAddressSpace (pid : in ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- getParent
    -- Given a process ID, return the parent process. If the process ID given
    -- is a thread, then return the owning process' ID.
    ---------------------------------------------------------------------------
    function getParent (pid : in ProcessID) return ProcessID
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- kill
    -- End this process
    -- @TODO ensure freeing all resources.
    ---------------------------------------------------------------------------
    procedure kill (pid : in ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- getRunningProcess
    ---------------------------------------------------------------------------
    -- function getRunningProcess return ProcPtr
    --     with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- pageFault
    -- When a page fault occurs, this procedure determines whether this is due
    -- simply to a page that hasn't been demand-mapped yet, or an actual
    -- violation.
    ---------------------------------------------------------------------------
    procedure pageFault (pid : ProcessID; addr : System.Address)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- enableFPU
    -- Turn on FPU state saving/restoring for this process.
    ---------------------------------------------------------------------------
    procedure enableFPU with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- saveFPUState
    -- If this process has previously used FPU/MMX/SSE instructions, this 
    -- procedure will save the FPU state and then disable FPU/MMX/SSE.
    --
    -- If this process does not use the FPU, it's a no-op.
    ---------------------------------------------------------------------------
    procedure saveFPUState (pid : ProcessID) with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- restoreFPUState
    -- If this process has previously used FPU/MMX/SSE instructions, this
    -- procedure will restore the FPU state. If not, it's a no-op.
    ---------------------------------------------------------------------------
    procedure restoreFPUState (pid : ProcessID) with SPARK_Mode => On;

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
        SPARK_Mode => On,
        Abstract_State => PIDTrackerState
    is
        -----------------------------------------------------------------------
        -- allocPID: find a free PID, mark it as in use
        --  out param pid - new PID, 0 if none free.
        -- Protected with pidLock
        -----------------------------------------------------------------------
        procedure allocPID (pid : out ProcessID) with
            SPARK_Mode => On,
            Global => (In_Out => PIDTrackerState);

        -----------------------------------------------------------------------
        -- allocSpecificPID: mark a specific PID as in use
        --  @param in pid - new PID
        -- Will throw exception if given PID already in use.
        -- Protected with pidLock
        -----------------------------------------------------------------------
        procedure allocSpecificPID (pid : in ProcessID) with
            SPARK_Mode => On,
            Global => (In_Out => PIDTrackerState);

        -----------------------------------------------------------------------
        -- freePID: mark PID as free in bitmap. Not locked.
        -----------------------------------------------------------------------
        procedure freePID (pid : in ProcessID) with
            SPARK_Mode => On,
            Global => (In_Out => PIDTrackerState);

    private

        trackerLockName : aliased String := "PID Tracker Lock"
            with Part_Of => PIDTrackerState;
        --subtype PIDBlock is Natural range 0..(MAX_PID / 64);
        --subtype PIDOffset is Natural range 0..63;

        type PIDBitmapType is array (ProcessID) of Boolean
            with Pack;

        -- Keep some PIDs reserved for the kernel to use for tasks with specific PIDs
        pidMap : PIDBitmapType := (0 => False, others => True)
            with Part_Of => PIDTrackerState;
        
        pidLock : Spinlocks.Spinlock := (name => trackerLockName'Access, others => <>)
            with Part_Of => PIDTrackerState;

        function findFreePID return ProcessID with
            SPARK_Mode => On,
            Global => (Input => PIDTrackerState);

        procedure markUsed (pid : in ProcessID) with
            SPARK_Mode => On,
            Global => (In_Out => PIDTrackerState);

        procedure markFree (pid : in ProcessID) with
            SPARK_Mode => On,
            Global => (In_Out => PIDTrackerState),
            Pre => pid /= 0;

        --function getBlock(pid : in ProcessID) return PIDBlock;
        --function getOffset(pid : in ProcessID) return PIDOffset;
    end PIDTracker;

end Process;