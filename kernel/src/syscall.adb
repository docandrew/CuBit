-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with PerCpuData;
with Process;
with Syscall_Census;
with Modules;
with Process_Identities;
with Process.Futex;
with Process.Owned_Memory;
with Process_Pool;
with Owned_Reservation_Policy;
with Owned_Memory_ABI;
with Process.IPC;
with Process.User_Memory;
with Process.Map_Probe;
with Debug_Register_Probe;
with Clock_Stall_Probe;
with Capabilities;
with Process_Capabilities;
with Spinlocks;
with Process_Launch;
with Syscall.IPC;
with Syscall.Memory;
with Syscall.Processes;
with Syscall.Admin;
with Syscall.Waits;
with TextIO; use TextIO;
with Time;
with Trace;
with Util;
with x86;
with Build;
with Boot_Timeline;
with Boot_Timeline_Layout;
with Wait_ABI;
with Wait_Policy;
with Futex_ABI;
with Latency_Contract_ABI;
with Thread_ABI;
with Thread_Slots;


package body Syscall is

    pragma Compile_Time_Error
        (SyscallNumber'Pos (SyscallNumber'Last) >= Syscall_Census.Calls,
         "the system call census needs an index per call");

    -- The most an sbrk prepares before its lock (64 MiB of frames).
    Sbrk_Prepare_Pages : constant := 16384;

    ---------------------------------------------------------------------------
    -- exit
    ---------------------------------------------------------------------------
    procedure exitp (currentPID : in Process.ProcessID;
                     requestedCode : in Unsigned_64) with SPARK_Mode => Off
    is
    begin
        Process.recordExit
          (currentPID, Process_Launch.To_Exit_Code (requestedCode));
        Process.kill (currentPID);
    end exitp;

    ---------------------------------------------------------------------------
    -- write
    ---------------------------------------------------------------------------
    function write (fd    : in Descriptors.DescriptorNum;
                    buf   : in System.Address;
                    count : in Unsigned_64) return Unsigned_64 with SPARK_Mode => Off
    is
        use Descriptors;    -- for '=' comparison
        Chunk_Size : constant := 256;
        bytesWritten : Unsigned_64 := 0;
        chunk : String (1 .. Chunk_Size);
        length : Natural;
        copied : Boolean;
    begin
        -- for testing
        if fd = Descriptors.STDOUT then
            -- Copy each chunk from user memory first, so a fault on the user
            -- buffer never happens under TextIO's output lock, then print the
            -- chunk as one string: other CPUs' output cannot interleave in it.
            -- buf is a user pointer: read through Process.User_Memory, so
            -- only the caller's own memory can be printed.
            while bytesWritten < count loop
                length := Natural (Unsigned_64'Min (count - bytesWritten, Chunk_Size));
                Process.User_Memory.Copy
                  (PerCPUData.getCurrentPID,
                   Unsigned_64 (To_Integer (buf)) + bytesWritten,
                   chunk'Address, Storage_Count (length), copied);
                exit when not copied;
                declare
                    probed : Boolean := False;
                begin
                    if Build.Kernel_Map_Probe then
                        Process.Map_Probe.Handle_Request
                          (PerCPUData.getCurrentPID, chunk (1 .. length), probed);
                    end if;
                    if Build.Debug_Register_Probe and then not probed then
                        Debug_Register_Probe.Handle_Request
                          (PerCPUData.getCurrentPID, chunk (1 .. length), probed);
                    end if;
                    if Build.Clock_Stall_Probe and then not probed then
                        Clock_Stall_Probe.Handle_Request
                          (PerCPUData.getCurrentPID, chunk (1 .. length), probed);
                    end if;
                    -- A process's text, not a kernel line: it stays out of
                    -- the kernel log ring (LOG-004).
                    if not probed then
                        printUser (chunk (1 .. length));
                    end if;
                end;
                bytesWritten := bytesWritten + Unsigned_64 (length);
            end loop;
        end if;

        return bytesWritten;
    end write;

    ---------------------------------------------------------------------------
    -- decodeSyscall
    --
    -- The hardware ABI accepts an arbitrary 64-bit value from userspace. Do
    -- not apply Enum_Val or an unchecked conversion to that untrusted value:
    -- only explicitly represented operations enter the typed dispatcher.
    ---------------------------------------------------------------------------
    procedure decodeSyscall (raw    : in Unsigned_64;
                             number : out SyscallNumber;
                             valid  : out Boolean) with SPARK_Mode => On
    is
    begin
        valid := True;
        case raw is
            when 0    => number := SYSCALL_EXIT;
            when 7    => number := SYSCALL_KILL;
            when 8    => number := SYSCALL_SBRK;
            when 146  => number := SYSCALL_RESERVE_OWNED_REGION;
            when 147  => number := SYSCALL_CHANGE_OWNED_RANGE;
            when 148  => number := SYSCALL_RELEASE_OWNED_REGION;
            when 126  => number := SYSCALL_INSTALL_LAUNCH_ARGUMENTS;
            when 127  => number := SYSCALL_SEND_CONTROL;
            when 118  => number := SYSCALL_YIELD;
            when 12   => number := SYSCALL_WRITE;
            when 18   => number := SYSCALL_REPLY;
            when 19   => number := SYSCALL_SEND_EVENT;
            when 29   => number := SYSCALL_MAPFB;
            when 30   => number := SYSCALL_INP8;
            when 31   => number := SYSCALL_OUTP8;
            when 32   => number := SYSCALL_INP16;
            when 33   => number := SYSCALL_OUTP16;
            when 36   => number := SYSCALL_INP32;
            when 37   => number := SYSCALL_OUTP32;
            when 129  => number := SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY;
            when 128  => number := SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY;
            when 130  => number := SYSCALL_ALLOCATE_DEVICE_VECTORS;
            when 131  => number := SYSCALL_RELEASE_DEVICE_VECTORS;
            when 132  => number := SYSCALL_READ_DEVICE_VECTOR;
            when 135  => number := SYSCALL_PUBLISH;
            when 137  => number := SYSCALL_CLOSE_HANDLE;
            when 138  => number := SYSCALL_INSTALL_CAPABILITY;
            when 139  => number := SYSCALL_REPLY_WITH_CAPABILITY;
            when 140  => number := SYSCALL_CALL_OFFERING_CAPABILITY;
            when 142  => number := SYSCALL_REPLY_SAVED_WITH_CAPABILITY;
            when 143  => number := SYSCALL_DERIVE_CAPABILITY;
            when Wait_ABI.System_Call_Number => number := SYSCALL_WAIT;
            when Notification_ABI.Wait_System_Call => number := SYSCALL_WAIT_ONE;
            when Notification_ABI.Wake_System_Call => number := SYSCALL_WAKE;
            when 42   => number := SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY;
            when 48   => number := SYSCALL_REPLY_WAIT;
            when 51   => number := SYSCALL_MOVE_REPLY_CAPABILITY;
            when 52   =>
                number := SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY;
            when 60   => number := SYSCALL_SPAWN;
            when 70   => number := SYSCALL_MAP_DEVICE;
            when 73   => number := SYSCALL_RESUME;
            when 74   => number := SYSCALL_ALLOC_DMA;
            when 75   => number := SYSCALL_ENABLE_IRQ;
            when 76   => number := SYSCALL_MAP_INTO;
            when 81   => number := SYSCALL_SET_LATENCY_CONTRACT;
            when 84   => number := SYSCALL_INSPECT_CAPABILITY;
            when 90   => number := SYSCALL_THREAD_CREATE;
            when 91   => number := SYSCALL_THREAD_EXIT;
            when 93   => number := SYSCALL_FUTEX_WAKE;
            when 102  =>
                number :=
                    SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID;
            when 103  => number := SYSCALL_REVOKE_SHARED_MEMORY_GRANT;
            when 106  =>
                number :=
                    SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY;
            when 108  =>
                number :=
                    SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION;
            when 109  => number := SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT;
            when 110  =>
                number := SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE;
            when 111  =>
                number := SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION;
            when 112  =>
                number := SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY;
            when 122 => number := SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY;
            when 2000 => number := SYSCALL_REGISTER_DRIVER;
            when others =>
                number := SYSCALL_EXIT;
                valid := False;
        end case;
    end decodeSyscall;


    -- The CAP_SCHEDULING a handle names in pid's table (a snapshot taken
    -- under its mailbox lock), or the null capability.
    function schedulingAuthority (pid : Process.ProcessID; handle : Unsigned_64)
                                  return Capabilities.Capability
      with SPARK_Mode => Off
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.tableLock (pid));
        found := Process_Capabilities.Find
          (Process.proctab(pid).caps, handle, Capabilities.CAP_SCHEDULING,
           Capabilities.NO_RIGHTS);
        Spinlocks.exitCriticalSection (Process.tableLock (pid));
        return (if found.OK then found.Cap else Capabilities.NULL_CAPABILITY);
    end schedulingAuthority;

    ---------------------------------------------------------------------------
    -- syscallHandler — thin dispatcher
    -- We get parameters passed here using the SysV ABI
    ---------------------------------------------------------------------------
    function syscallHandler (arg0,   -- rdi
                             arg1,   -- rsi
                             arg2,   -- rdx
                             arg3,   -- rcx
                             arg4,   -- r8
                             arg5,   -- r9
                             syscallNumRaw : in Unsigned_64;  -- first on stack
                             arg6 : in Unsigned_64)           -- second on stack
                             return Unsigned_64
    is
        -- The calling thread, read once: it is the same thread throughout
        -- the call, but not on the same CPU. An interrupt window (spawn,
        -- sbrk, owned-memory changes: Interrupt_Windows) or a block may
        -- move it to another CPU, so no per-CPU data may be read through an
        -- overlay taken at entry: after a move, that CPU runs another
        -- thread (an sbrk locked one process's address space and unlocked
        -- another's, and the kernel stopped on "Spinlock release without
        -- CPU ownership").
        caller : constant Process.ThreadID := PerCPUData.getCurrentThread;

        retval     : Unsigned_64 := 0;
        traceActive : constant Boolean := Trace.IsEnabled;
        startTSC   : Unsigned_64 := 0;
        syscallNum : SyscallNumber;
        validSyscall : Boolean;
        -- Boot timeline (docs/boot-timeline.md): a blocking call's kind and
        -- start, while the boot window records.
        waitKind   : Boot_Timeline_Layout.Wait_Kind := Boot_Timeline_Layout.No_Wait;
        waitStart  : Unsigned_64 := 0;
        -- Captured before dispatch: a blocked thread may resume on another
        -- CPU.
        waitPID    : Process.ProcessID := Process.NO_PROCESS;
    begin
        -- The thread's own line in its process's thread-count pages
        -- (Readiness_Publication; only this thread writes it).
        declare
            calls : Unsigned_64 with Import, Volatile,
                Address => Process.threadtab (caller).callCounter;
        begin
            calls := calls + 1;
        end;
        if traceActive then
            startTSC := x86.rdtsc;
            Trace.Emit (Trace.EVENT_SYSCALL_ENTER, syscallNumRaw, arg0);
        end if;

        decodeSyscall (syscallNumRaw, syscallNum, validSyscall);
        if Build.Syscall_Census and then validSyscall then
            Syscall_Census.Note (Process.processOf (caller),
                                 SyscallNumber'Pos (syscallNum), syscallNumRaw);
        end if;
        if not validSyscall then
            print ("Unknown syscall: "); printd (syscallNumRaw);
            print (" from PID: "); println (Process.processOf (caller));
            if traceActive then
                Trace.ObserveDuration (Trace.EVENT_SYSCALL_TIME,
                                       x86.rdtsc - startTSC);
            end if;
            Process.serviceReschedule;
            Process.checkTermination;
            return 0;
        end if;

        if Boot_Timeline.Recording then
            case syscallNum is
                -- Entering a receive: the first one after registration is
                -- the process's Ready (its request loop).
                when SYSCALL_REPLY_WAIT =>
                    Boot_Timeline.Note_Event
                      (Natural (Process.processOf (caller)),
                       Boot_Timeline.Received);
                    waitKind := Boot_Timeline_Layout.Receive;
                -- Entering a receive: the first one after registration is
                -- the process's Ready (its request loop).
                when SYSCALL_WAIT =>
                    case Wait_Policy.Decide (arg0, arg1, arg3, arg4) is
                        when Wait_Policy.Messages_Forever | Wait_Policy.Messages_Until
                           | Wait_Policy.Ready | Wait_Policy.Ready_Word =>
                            Boot_Timeline.Note_Event
                              (Natural (Process.processOf (caller)),
                               Boot_Timeline.Received);
                            waitKind := Boot_Timeline_Layout.Receive;
                        when Wait_Policy.Timer =>
                            waitKind := Boot_Timeline_Layout.Sleep;
                        when Wait_Policy.Completions_Wait =>
                            waitKind := Boot_Timeline_Layout.Completion;
                        when Wait_Policy.Futex =>
                            waitKind := Boot_Timeline_Layout.Futex;
                        when others =>
                            null;
                    end case;
                when SYSCALL_WAIT_ONE =>
                    waitKind := Boot_Timeline_Layout.Futex;
                when others =>
                    null;
            end case;
            if Boot_Timeline_Layout."/=" (waitKind, Boot_Timeline_Layout.No_Wait) then
                waitPID := Process.processOf (caller);
                waitStart := Boot_Timeline.Now;
            end if;
        end if;

        case syscallNum is
            when SYSCALL_EXIT =>
                exitp (Process.processOf (caller), arg0);

            when SYSCALL_KILL =>
                Admin.handleKill (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_WRITE =>
                retval := write (
                    fd    => Descriptors.DescriptorNum(arg0),
                    buf   => Util.numToAddr(arg1),
                    count => arg2);

            -- arg0 entry, arg1 stack pointer, arg2 argument (RDI), arg3 FS
            -- base, arg4 word cleared and futex-woken at exit (0: none).
            -- Returns the thread ID and its thread-count slot (also its
            -- RSI at entry) as Thread_ABI.Created, or Thread_ABI.Refused.
            when SYSCALL_THREAD_CREATE =>
                declare
                    tid : Process.ThreadID;
                    slot : Thread_Slots.Slot;
                begin
                    Process.createThread
                      (entryPoint      => Util.numToAddr (arg0),
                       userStack       => Util.numToAddr (arg1),
                       argument        => arg2,
                       fsBase          => arg3,
                       clearTidAddress => arg4,
                       tid             => tid,
                       slot            => slot);
                    retval := (if Process."=" (tid, Process.NO_THREAD)
                               then Thread_ABI.Refused
                               else Thread_ABI.Created (Unsigned_64 (tid), slot));
                end;

            when SYSCALL_THREAD_EXIT =>
                Process.exitThread;

            -- arg0 the word (an address, a shared word, or a peer's grant),
            -- arg1 maximum waiters to wake, arg2 its space, arg3 a sync hint
            -- (Futex_ABI, IPC-010).
            when SYSCALL_FUTEX_WAKE =>
                retval := Process.Futex.wakeSpace
                  (arg0, arg1, arg2,
                   sync   => Futex_ABI.Has_Hint (arg3, Futex_ABI.Sync_Hint),
                   urgent => Futex_ABI.Has_Hint (arg3, Futex_ABI.Urgent_Hint));

            when SYSCALL_SBRK =>
                -- The new pages' frames are made clean first, with no lock
                -- held and interrupt windows between batches (as for an
                -- owned-memory commit); what is left under the lock below
                -- is mapping. At most Sbrk_Prepare_Pages: a request can name
                -- any size, and Heap_Admission decides under the lock.
                if arg0 > 0 then
                    Process_Pool.Prepare
                      (Natural (Unsigned_64'Min (arg0 / Owned_Reservation_Policy.Page_Bytes + 1,
                                               Sbrk_Prepare_Pages)));
                end if;
                -- Serialized against sibling threads' faults and sbrk.
                Process.lockAddressSpace (Process.processOf (caller));
                Memory.handleSbrk (
                    Process.processOf (caller), arg0, retval);
                Process.unlockAddressSpace (Process.processOf (caller));

            -- Owned memory (Process.Owned_Memory, ABI-002): three calls,
            -- no flags. Reserve: arg0 page-aligned bytes, arg1 the base's
            -- alignment (0: a page); the base, or a refusal word
            -- (Owned_Memory_ABI, MEM-007: Too_Large above the kernel's
            -- largest region, Owned_Reservation_Policy).
            when SYSCALL_RESERVE_OWNED_REGION =>
                declare
                    Why : Owned_Memory_ABI.Refusal;
                begin
                    Process.Owned_Memory.Reserve
                      (Process.processOf (caller), arg0, arg1, retval, Why);
                    if retval = 0 then
                        retval := Owned_Memory_ABI.Refusal_Word (Why);
                    end if;
                end;

            -- arg0 address, arg1 bytes (a page range of one region), arg2
            -- the operation (Owned_Reservation_Policy.Range_Operation:
            -- 0 commit, 1 decommit, 2 inaccessible, 3 read-only, 4
            -- read/write). Done, or a refusal word (Owned_Memory_ABI).
            when SYSCALL_CHANGE_OWNED_RANGE =>
                declare
                    Operation : Owned_Reservation_Policy.Range_Operation;
                    Known, Changed : Boolean := False;
                    Why : Owned_Memory_ABI.Refusal := Owned_Memory_ABI.Invalid_Operation;
                begin
                    Owned_Reservation_Policy.Decode (arg2, Operation, Known);
                    if Known then
                        Process.Owned_Memory.Change_Range
                          (Process.processOf (caller), arg0, arg1,
                           Operation, Changed, Why);
                    end if;
                    retval := (if Changed then Owned_Memory_ABI.Done
                               else Owned_Memory_ABI.Refusal_Word (Why));
                end;

            when SYSCALL_RELEASE_OWNED_REGION =>
                -- The caller's own memory: an owned region (exact base and
                -- capacity), or else its read-only view of boot-image pages
                -- (Modules.Release_View), whose pages go back to the buddy
                -- allocator with their last mapping. One release call,
                -- whatever backs the range, until the boot-module handle
                -- takes the view's release (BOOT-013).
                declare
                    Released : Boolean;
                    Why : Owned_Memory_ABI.Refusal;
                begin
                    Process.Owned_Memory.Release
                      (Process.processOf (caller), arg0, arg1, Released, Why);
                    if not Released and then Owned_Memory_ABI."=" (Why, Owned_Memory_ABI.No_Region) then
                        Modules.Release_View
                          (Process.processOf (caller), arg0, arg1, Released);
                    end if;
                    retval := (if Released then Owned_Memory_ABI.Done
                               else Owned_Memory_ABI.Refusal_Word (Why));
                end;

            -- Give the CPU to any other ready thread; the caller is re-queued
            -- behind its peers and runs again when next picked.
            when SYSCALL_YIELD =>
                Process.yield;
                retval := 0;

            when SYSCALL_MAPFB =>
                Memory.handleMapFB (
                    Process.processOf (caller), arg0, retval);

            when SYSCALL_REPLY =>
                IPC.handleReply (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY =>
                IPC.handleReplyCap (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_SEND_EVENT =>
                IPC.handleSendEvent (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID =>
                IPC.handleGrant (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, retval);

            when SYSCALL_REVOKE_SHARED_MEMORY_GRANT =>
                IPC.handleRevoke (
                    Process.processOf (caller), arg0, retval);

            when SYSCALL_GET_OWNED_SHARED_MEMORY_GRANT_GENERATION =>
                IPC.handleGetOwnedGrantGeneration (arg0, retval);

            when SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT =>
                IPC.handleAcquireGrant
                  (arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_REVOKE_SHARED_MEMORY_GRANT_REFERENCE =>
                IPC.handleRevokeGrantReference (arg0, arg1, retval);

            when SYSCALL_RETURN_SHARED_MEMORY_GRANT_ACQUISITION =>
                IPC.handleReturnGrant (arg0, arg1, retval);

            when SYSCALL_ACQUIRE_SHARED_MEMORY_GRANT_VIA_CAPABILITY =>
                IPC.handleAcquireGrantViaCap
                  (Process.processOf (caller),
                   arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY =>
                IPC.handleDeriveGrantViaCap
                  (Process.processOf (caller),
                   arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_PUBLISH =>
                Admin.handlePublish (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_REGISTER_DRIVER =>
                Admin.handleRegisterDriver (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_INP8 | SYSCALL_OUTP8 |
                 SYSCALL_INP16 | SYSCALL_OUTP16 |
                 SYSCALL_INP32 | SYSCALL_OUTP32 =>
                Admin.handlePortIO (
                    Process.processOf (caller), syscallNum,
                    arg0, arg1, arg2, retval);

            when SYSCALL_MOVE_REPLY_CAPABILITY =>
                Admin.handleSaveReplyCap (
                    Process.processOf (caller), retval);

            when SYSCALL_CLOSE_HANDLE =>
                Admin.handleCloseHandle (
                    Process.processOf (caller), arg0, retval);

            when SYSCALL_INSTALL_CAPABILITY =>
                Admin.handleInstallCap (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_DERIVE_CAPABILITY =>
                Admin.handleDeriveCap (
                    Process.processOf (caller), arg0, retval);

            -- arg0 authority handle, arg1 operation, arg2 table, arg3
            -- offset, arg4 destination, arg5 capacity, arg6 reply record.
            -- The one wait (Wait_ABI): arg0 the Sources word, arg1 the
            -- deadline, arg2 .. arg4 the source's parameters.
            when SYSCALL_WAIT =>
                IPC.handleWait (arg0, arg1, arg2, arg3, arg4, arg5, retval);

            -- IPC revision 2 (Notification_ABI): arg0 the kind, arg1 the
            -- object, then the expected value and deadline, or the count.
            when SYSCALL_WAIT_ONE =>
                Waits.handleWait (arg0, arg1, arg2, arg3, retval);

            when SYSCALL_WAKE =>
                Waits.handleWake (arg0, arg1, arg2, retval);

            when SYSCALL_REPLY_WITH_CAPABILITY =>
                IPC.handleReplyWithCap (
                    Process.processOf (caller), arg0, arg1, arg2, retval);

            when SYSCALL_CALL_OFFERING_CAPABILITY =>
                Admin.handleCapCallOffering (
                    Process.processOf (caller), arg0, arg1, arg2, arg3, retval);

            when SYSCALL_REPLY_SAVED_WITH_CAPABILITY =>
                IPC.handleReplySavedWithCap (
                    Process.processOf (caller), arg0, arg1, arg2, retval);

            when SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapSend (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapCall (
                    Process.processOf (caller), arg0, arg1, arg2, retval);

            when SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapSubmit (
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_REPLY_WAIT =>
                Admin.handleReplyWait (arg0, arg1, retval);

            when SYSCALL_SPAWN =>
                Processes.handleSpawn (
                    Process.processOf (caller),
                    arg0, arg1, arg2, retval);

            when SYSCALL_MAP_DEVICE =>
                Memory.handleMapDevice (
                    Process.processOf (caller), arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_INSPECT_CAPABILITY =>
                Admin.handleInspectCap (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_RESUME =>
                Admin.handleResume (
                    Process.processOf (caller), arg0, arg1, arg2, arg3, retval);

            when SYSCALL_SEND_CONTROL =>
                Admin.handleSendControl (
                    Process.processOf (caller),
                    arg0, arg1, arg2, retval);

            when SYSCALL_INSTALL_LAUNCH_ARGUMENTS =>
                Admin.handleInstallLaunchArguments (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_ALLOC_DMA =>
                Memory.handleAllocDma (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_ENABLE_IRQ =>
                Admin.handleEnableIrq (
                    Process.processOf (caller), arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_ALLOCATE_DEVICE_VECTORS =>
                Admin.handleAllocateDeviceVectors (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_RELEASE_DEVICE_VECTORS =>
                Admin.handleReleaseDeviceVectors (
                    Process.processOf (caller), arg0, arg1, retval);

            when SYSCALL_READ_DEVICE_VECTOR =>
                Admin.handleReadDeviceVector (
                    Process.processOf (caller), arg0, arg1, arg2, retval);

            when SYSCALL_MAP_INTO =>
                Memory.handleMapInto (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_SET_LATENCY_CONTRACT =>
                -- The calling thread's scheduler contract, validated at the
                -- syscall boundary.
                if arg0 > Unsigned_64 (Process.LatencyClass'Pos (
                   Process.LatencyClass'Last)) or else
                   arg1 > Unsigned_64 (Unsigned_32'Last) or else
                   arg2 > Unsigned_64 (Unsigned_32'Last) or else
                   arg3 > Unsigned_64 (Unsigned_32'Last)
                then
                    retval := Unsigned_64'Last;
                elsif arg1 /= 0 and then arg2 > arg1 then
                    retval := Unsigned_64'Last;
                elsif not Latency_Contract_ABI.Defined (arg3) then
                    -- A flag this kernel does not know.
                    retval := Unsigned_64'Last;
                else
                    -- REALTIME and INTERACTIVE are enforced: admitted only
                    -- with a covering CAP_SCHEDULING and within the
                    -- system's real-time share (Process.reserveRealtime);
                    -- an INTERACTIVE budget pays for the urgent turns its
                    -- holder lends (docs/scheduler.md §4). The other
                    -- classes need no authority and give up a reservation.
                    declare
                        use type Process.LatencyClass;
                        class : constant Process.LatencyClass :=
                          Process.LatencyClass'Val (Natural (arg0));
                        granted : Boolean := True;
                    begin
                        if class in Process.LATENCY_REALTIME | Process.LATENCY_INTERACTIVE then
                            -- arg4 names the CAP_SCHEDULING relied on.
                            Process.reserveRealtime
                              (caller,
                               authority => schedulingAuthority
                                 (Process.processOf (caller), arg4),
                               budgetUs => Unsigned_32 (arg2),
                               periodUs => Unsigned_32 (arg1),
                               granted  => granted);
                        else
                            Process.releaseReservation (caller);
                        end if;
                        if granted then
                            Process.setLatencyContract (
                                tid      => caller,
                                class    => class,
                                periodUs => Unsigned_32 (arg1),
                                budgetUs => Unsigned_32 (arg2),
                                flags    => Unsigned_32 (arg3));
                            retval := 0;
                        else
                            retval := Unsigned_64'Last;
                        end if;
                    end;
                end if;

            when SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY =>
                IPC.handleGrantViaCap (
                    Process.processOf (caller),
                    arg0, arg1, arg2, arg3, retval);

        end case;

        if Boot_Timeline_Layout."/=" (waitKind, Boot_Timeline_Layout.No_Wait) then
            declare
                pid : constant Process.ProcessID := waitPID;
            begin
                Boot_Timeline.Note_Wait
                  (Slot    => Natural (pid),
                   Waiter  => Process_Identities.To_Word (Process.identityOf (pid)),
                   Kind    => waitKind,
                   Label   => 0,
                   Target  => Boot_Timeline_Layout.No_Identity,
                   Started => waitStart);
            end;
        end if;
        if traceActive then
            Trace.ObserveDuration (Trace.EVENT_SYSCALL_TIME,
                                   x86.rdtsc - startTSC);
        end if;
        Process.serviceReschedule;
        Process.checkTermination;
        if Trace.IsEnabled then
            Trace.Emit (Trace.EVENT_SYSCALL_RETURN, syscallNumRaw, 0);
        end if;
        return retval;
    end syscallHandler;


end Syscall;
