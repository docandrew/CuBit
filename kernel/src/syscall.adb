-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with PerCpuData;
with Process;
with Modules;
with Process_Identities;
with Process.Futex;
with Process.Owned_Memory;
with Owned_Reservation_Policy;
with Process.IPC;
with Process.User_Memory;
with Capabilities;
with Process_Capabilities;
with Spinlocks;
with Process_Launch;
with Syscall.IPC;
with Syscall.Admin;
with TextIO; use TextIO;
with Time;
with Trace;
with Util;
with x86;
with Build;
with Boot_Timeline;
with Boot_Timeline_Layout;
with Firmware_Snapshot_Export;
with Wait_ABI;
with Wait_Policy;
with Futex_ABI;


package body Syscall is

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
                -- A process's text, not a kernel line: it stays out of
                -- the kernel log ring (LOG-004).
                printUser (chunk (1 .. length));
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
            when 6    => number := SYSCALL_GETPID;
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
            when 27   => number := SYSCALL_GETTIME;
            when 114  => number := SYSCALL_READ_MONOTONIC_MICROSECONDS;
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
            when Boot_Timeline_Layout.System_Call_Number =>
                number := SYSCALL_BOOT_TIMELINE;
            when 134  => number := SYSCALL_READ_STATISTICS;
            when 135  => number := SYSCALL_PUBLISH;
            when 137  => number := SYSCALL_CLOSE_HANDLE;
            when 138  => number := SYSCALL_INSTALL_CAPABILITY;
            when 139  => number := SYSCALL_REPLY_WITH_CAPABILITY;
            when 140  => number := SYSCALL_CALL_OFFERING_CAPABILITY;
            when 142  => number := SYSCALL_REPLY_SAVED_WITH_CAPABILITY;
            when 143  => number := SYSCALL_DERIVE_CAPABILITY;
            when Firmware_Snapshot_ABI.System_Call_Number =>
                number := SYSCALL_FIRMWARE_SNAPSHOT;
            when Wait_ABI.System_Call_Number => number := SYSCALL_WAIT;
            when 42   => number := SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY;
            when 48   => number := SYSCALL_REPLY_WAIT;
            when 51   => number := SYSCALL_MOVE_REPLY_CAPABILITY;
            when 52   =>
                number := SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY;
            when 60   => number := SYSCALL_SPAWN;
            when 70   => number := SYSCALL_MAP_DEVICE;
            when 71   => number := SYSCALL_PROCLIST;
            when 73   => number := SYSCALL_RESUME;
            when 74   => number := SYSCALL_ALLOC_DMA;
            when 75   => number := SYSCALL_ENABLE_IRQ;
            when 76   => number := SYSCALL_MAP_INTO;
            when 78   => number := SYSCALL_SET_CPU;
            when 81   => number := SYSCALL_SET_LATENCY_CONTRACT;
            when 82   => number := SYSCALL_TRACE_RESET;
            when 83   => number := SYSCALL_TRACE_SUMMARY;
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
        Spinlocks.enterCriticalSection (Process.mailtab(pid).lock);
        found := Process_Capabilities.Find
          (Process.proctab(pid).caps, handle, Capabilities.CAP_SCHEDULING,
           Capabilities.NO_RIGHTS);
        Spinlocks.exitCriticalSection (Process.mailtab(pid).lock);
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
        percpu : PerCPUData.PerCPUData with
            Import, Volatile, Address => PerCPUData.getPerCPUDataAddr;

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
        -- CPU, where percpu no longer describes it.
        waitPID    : Process.ProcessID := Process.NO_PROCESS;
    begin
        Process.threadtab (percpu.currentThread).systemCalls :=
          Process.threadtab (percpu.currentThread).systemCalls + 1;
        if traceActive then
            startTSC := x86.rdtsc;
            -- Tight compute controls issue GETTIME repeatedly. Keep their
            -- timer/dispatch events without letting clock reads evict the
            -- bounded latency trace before the delayed receiver can freeze it.
            if not Build.Test_Latency_Trace or else
              syscallNumRaw /= SyscallNumber'Enum_Rep (SYSCALL_GETTIME)
            then
                Trace.Emit (Trace.EVENT_SYSCALL_ENTER, syscallNumRaw, arg0);
            end if;
        end if;

        decodeSyscall (syscallNumRaw, syscallNum, validSyscall);
        if not validSyscall then
            print ("Unknown syscall: "); printd (syscallNumRaw);
            print (" from PID: "); println (Process.processOf (percpu.currentThread));
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
                      (Natural (Process.processOf (percpu.currentThread)),
                       Boot_Timeline.Received);
                    waitKind := Boot_Timeline_Layout.Receive;
                -- Entering a receive: the first one after registration is
                -- the process's Ready (its request loop).
                when SYSCALL_WAIT =>
                    case Wait_Policy.Decide (arg0, arg1, arg3, arg4) is
                        when Wait_Policy.Messages_Forever | Wait_Policy.Messages_Until
                           | Wait_Policy.Ready | Wait_Policy.Ready_Word =>
                            Boot_Timeline.Note_Event
                              (Natural (Process.processOf (percpu.currentThread)),
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
                when others =>
                    null;
            end case;
            if Boot_Timeline_Layout."/=" (waitKind, Boot_Timeline_Layout.No_Wait) then
                waitPID := Process.processOf (percpu.currentThread);
                waitStart := Boot_Timeline.Now;
            end if;
        end if;

        case syscallNum is
            when SYSCALL_EXIT =>
                exitp (Process.processOf (percpu.currentThread), arg0);

            when SYSCALL_BOOT_TIMELINE =>
                Admin.handleBootTimeline
                  (Process.processOf (percpu.currentThread), arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_KILL =>
                Admin.handleKill (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_GETPID =>
                retval := Process_Identities.To_Word (Process.identityOf (Process.processOf (percpu.currentThread)));

            when SYSCALL_WRITE =>
                retval := write (
                    fd    => Descriptors.DescriptorNum(arg0),
                    buf   => Util.numToAddr(arg1),
                    count => arg2);

            -- arg0 entry, arg1 stack pointer, arg2 argument (RDI), arg3 FS
            -- base, arg4 word cleared and futex-woken at exit (0: none).
            -- Returns the thread ID, or Unsigned_64'Last.
            when SYSCALL_THREAD_CREATE =>
                declare
                    tid : Process.ThreadID;
                begin
                    Process.createThread
                      (entryPoint      => Util.numToAddr (arg0),
                       userStack       => Util.numToAddr (arg1),
                       argument        => arg2,
                       fsBase          => arg3,
                       clearTidAddress => arg4,
                       tid             => tid);
                    retval := (if Process."=" (tid, Process.NO_THREAD)
                               then Unsigned_64'Last else Unsigned_64 (tid));
                end;

            when SYSCALL_THREAD_EXIT =>
                Process.exitThread;

            -- arg0 the word (an address, a shared word, or a peer's grant),
            -- arg1 maximum waiters to wake, arg2 its space, arg3 a sync hint
            -- (Futex_ABI, IPC-010).
            when SYSCALL_FUTEX_WAKE =>
                retval := Process.Futex.wakeSpace
                  (arg0, arg1, arg2, sync => arg3 = Futex_ABI.Sync_Hint);

            when SYSCALL_SBRK =>
                -- Serialized against sibling threads' faults and sbrk.
                Process.lockAddressSpace (Process.processOf (percpu.currentThread));
                IPC.handleSbrk (
                    Process.processOf (percpu.currentThread), arg0, retval);
                Process.unlockAddressSpace (Process.processOf (percpu.currentThread));

            -- Owned memory (Process.Owned_Memory, ABI-002): three calls,
            -- no flags. Reserve: arg0 page-aligned bytes (up to 2 GiB),
            -- arg1 the base's alignment (0: a page); the base, or zero.
            when SYSCALL_RESERVE_OWNED_REGION =>
                Process.Owned_Memory.Reserve
                  (Process.processOf (percpu.currentThread), arg0, arg1, retval);

            -- arg0 address, arg1 bytes (a page range of one region), arg2
            -- the operation (Owned_Reservation_Policy.Range_Operation:
            -- 0 commit, 1 decommit, 2 inaccessible, 3 read-only, 4
            -- read/write). Zero, or Last when refused.
            when SYSCALL_CHANGE_OWNED_RANGE =>
                declare
                    Operation : Owned_Reservation_Policy.Range_Operation;
                    Known, Changed : Boolean := False;
                begin
                    Owned_Reservation_Policy.Decode (arg2, Operation, Known);
                    if Known then
                        Process.Owned_Memory.Change_Range
                          (Process.processOf (percpu.currentThread), arg0, arg1,
                           Operation, Changed);
                    end if;
                    retval := (if Changed then 0 else Unsigned_64'Last);
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
                begin
                    Process.Owned_Memory.Release
                      (Process.processOf (percpu.currentThread), arg0, arg1, Released);
                    if not Released then
                        Modules.Release_View
                          (Process.processOf (percpu.currentThread), arg0, arg1, Released);
                    end if;
                    retval := (if Released then 0 else Unsigned_64'Last);
                end;

            when SYSCALL_GETTIME =>
                retval := Time.msTicks;

            when SYSCALL_READ_MONOTONIC_MICROSECONDS =>
                declare
                    Available : Boolean;
                begin
                    Time.Read_Monotonic (retval, Available);
                    if not Available then retval := Unsigned_64'Last; end if;
                end;

            -- Give the CPU to any other ready thread; the caller is re-queued
            -- behind its peers and runs again when next picked.
            when SYSCALL_YIELD =>
                Process.yield;
                retval := 0;

            when SYSCALL_MAPFB =>
                IPC.handleMapFB (
                    Process.processOf (percpu.currentThread), arg0, retval);

            when SYSCALL_REPLY =>
                IPC.handleReply (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_REPLY_AND_CONSUME_REPLY_CAPABILITY =>
                IPC.handleReplyCap (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_SEND_EVENT =>
                IPC.handleSendEvent (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_CREATE_SHARED_MEMORY_GRANT_FOR_PROCESS_ID =>
                IPC.handleGrant (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, retval);

            when SYSCALL_REVOKE_SHARED_MEMORY_GRANT =>
                IPC.handleRevoke (
                    Process.processOf (percpu.currentThread), arg0, retval);

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
                  (Process.processOf (percpu.currentThread),
                   arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_DERIVE_SHARED_MEMORY_GRANT_VIA_CAPABILITY =>
                IPC.handleDeriveGrantViaCap
                  (Process.processOf (percpu.currentThread),
                   arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_READ_STATISTICS =>
                IPC.handleReadStatistics (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_PUBLISH =>
                Admin.handlePublish (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_REGISTER_DRIVER =>
                Admin.handleRegisterDriver (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_INP8 | SYSCALL_OUTP8 |
                 SYSCALL_INP16 | SYSCALL_OUTP16 |
                 SYSCALL_INP32 | SYSCALL_OUTP32 =>
                Admin.handlePortIO (
                    Process.processOf (percpu.currentThread), syscallNum,
                    arg0, arg1, arg2, retval);

            when SYSCALL_MOVE_REPLY_CAPABILITY =>
                Admin.handleSaveReplyCap (
                    Process.processOf (percpu.currentThread), retval);

            when SYSCALL_CLOSE_HANDLE =>
                Admin.handleCloseHandle (
                    Process.processOf (percpu.currentThread), arg0, retval);

            when SYSCALL_INSTALL_CAPABILITY =>
                Admin.handleInstallCap (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_DERIVE_CAPABILITY =>
                Admin.handleDeriveCap (
                    Process.processOf (percpu.currentThread), arg0, retval);

            -- arg0 authority handle, arg1 operation, arg2 table, arg3
            -- offset, arg4 destination, arg5 capacity, arg6 reply record.
            -- The one wait (Wait_ABI): arg0 the Sources word, arg1 the
            -- deadline, arg2 .. arg4 the source's parameters.
            when SYSCALL_WAIT =>
                IPC.handleWait (arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_FIRMWARE_SNAPSHOT =>
                Firmware_Snapshot_Export.Handle
                  (Process.processOf (percpu.currentThread),
                   arg0, (Op => arg1, Table => arg2, Offset => arg3,
                          Destination => arg4, Capacity => arg5), arg6, retval);

            when SYSCALL_REPLY_WITH_CAPABILITY =>
                IPC.handleReplyWithCap (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_CALL_OFFERING_CAPABILITY =>
                Admin.handleCapCallOffering (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, arg3, retval);

            when SYSCALL_REPLY_SAVED_WITH_CAPABILITY =>
                IPC.handleReplySavedWithCap (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_SEND_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapSend (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapCall (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_SUBMIT_VIA_ENDPOINT_CAPABILITY =>
                Admin.handleCapSubmit (
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_REPLY_WAIT =>
                Admin.handleReplyWait (arg0, arg1, retval);

            when SYSCALL_SPAWN =>
                IPC.handleSpawn (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, retval);

            when SYSCALL_MAP_DEVICE =>
                IPC.handleMapDevice (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_PROCLIST =>
                Admin.handleProclist (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_INSPECT_CAPABILITY =>
                Admin.handleInspectCap (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_RESUME =>
                Admin.handleResume (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, arg3, retval);

            when SYSCALL_SEND_CONTROL =>
                Admin.handleSendControl (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, retval);

            when SYSCALL_INSTALL_LAUNCH_ARGUMENTS =>
                Admin.handleInstallLaunchArguments (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_ALLOC_DMA =>
                IPC.handleAllocDma (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_ENABLE_IRQ =>
                Admin.handleEnableIrq (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_ALLOCATE_DEVICE_VECTORS =>
                Admin.handleAllocateDeviceVectors (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_RELEASE_DEVICE_VECTORS =>
                Admin.handleReleaseDeviceVectors (
                    Process.processOf (percpu.currentThread), arg0, arg1, retval);

            when SYSCALL_READ_DEVICE_VECTOR =>
                Admin.handleReadDeviceVector (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_MAP_INTO =>
                IPC.handleMapInto (
                    Process.processOf (percpu.currentThread),
                    arg0, arg1, arg2, arg3, arg4, arg5, arg6, retval);

            when SYSCALL_SET_CPU =>
                Admin.handleSetCpu (
                    Process.processOf (percpu.currentThread), arg0, arg1, arg2, retval);

            when SYSCALL_SET_LATENCY_CONTRACT =>
                -- Process-local scheduler contract, validated at the syscall
                -- boundary.
                if arg0 > Unsigned_64 (Process.LatencyClass'Pos (
                   Process.LatencyClass'Last)) or else
                   arg1 > Unsigned_64 (Unsigned_32'Last) or else
                   arg2 > Unsigned_64 (Unsigned_32'Last) or else
                   arg3 > Unsigned_64 (Unsigned_32'Last)
                then
                    retval := Unsigned_64'Last;
                elsif arg1 /= 0 and then arg2 > arg1 then
                    retval := Unsigned_64'Last;
                else
                    -- REALTIME is enforced: admitted only with a covering
                    -- CAP_SCHEDULING and within the system's real-time
                    -- share (Process.reserveRealtime). Other classes are
                    -- recorded as advisory hints.
                    declare
                        use type Process.LatencyClass;
                        class : constant Process.LatencyClass :=
                          Process.LatencyClass'Val (Natural (arg0));
                        granted : Boolean := True;
                    begin
                        if class = Process.LATENCY_REALTIME then
                            -- arg4 names the CAP_SCHEDULING relied on.
                            Process.reserveRealtime
                              (Process.processOf (percpu.currentThread),
                               authority => schedulingAuthority
                                 (Process.processOf (percpu.currentThread), arg4),
                               budgetUs => Unsigned_32 (arg2),
                               periodUs => Unsigned_32 (arg1),
                               granted  => granted);
                        end if;
                        if granted then
                            Process.setLatencyContract (
                                pid      => Process.processOf (percpu.currentThread),
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

            when SYSCALL_TRACE_RESET =>
                Trace.Reset;
                retval := 0;

            when SYSCALL_TRACE_SUMMARY =>
                if arg0 = Trace.Control_Operation'Enum_Rep (Trace.Summary) then
                    Trace.PrintSummary;
                    Process.printOwnAccounting;
                elsif Build.Test_Latency_Trace then
                    case arg0 is
                        when Trace.Control_Operation'Enum_Rep (Trace.Start_Local) => Trace.StartLocal;
                        when Trace.Control_Operation'Enum_Rep (Trace.Freeze_Local) => Trace.FreezeLocal;
                        when Trace.Control_Operation'Enum_Rep (Trace.Dump_Local) => Trace.DumpLocal;
                        when others => retval := Unsigned_64'Last;
                    end case;
                else
                    retval := Unsigned_64'Last;
                end if;

            when SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY =>
                IPC.handleGrantViaCap (
                    Process.processOf (percpu.currentThread),
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
        if Trace.IsEnabled and then
          (not Build.Test_Latency_Trace or else
           syscallNumRaw /= SyscallNumber'Enum_Rep (SYSCALL_GETTIME))
        then
            Trace.Emit (Trace.EVENT_SYSCALL_RETURN, syscallNumRaw, 0);
        end if;
        return retval;
    end syscallHandler;


end Syscall;
