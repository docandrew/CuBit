-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

with Capabilities;
with Descriptors;
with PerCpuData;
with Process;
with Process.IPC;
with Syscall.IPC;
with Syscall.Admin;
with TextIO; use TextIO;
with Time;
with Trace;
with Util;
with x86;

use type Process.MessageTag;

package body Syscall is

    ---------------------------------------------------------------------------
    -- exit
    ---------------------------------------------------------------------------
    procedure exitp (currentPID : in Process.ProcessID) with SPARK_Mode => On
    is
    begin
        Process.kill (currentPID);
    end exitp;

    ---------------------------------------------------------------------------
    -- write
    ---------------------------------------------------------------------------
    function write (fd    : in Descriptors.DescriptorNum;
                    buf   : in System.Address;
                    count : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
        use Descriptors;    -- for '=' comparison
        bytesWritten : Unsigned_64 := 0;
        idx : Storage_Offset := 0;
    begin
        -- for testing
        if fd = Descriptors.STDOUT then
            x86.stac;
            for i in 1 .. count loop
                nextByte: declare
                    c : Character with Import, Address => buf + idx;
                begin
                    print (c);
                    bytesWritten := bytesWritten + 1;
                    idx := idx + 1;
                end nextByte;
            end loop;
            x86.clac;
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
            when 12   => number := SYSCALL_WRITE;
            when 15   => number := SYSCALL_INFO;
            when 17   => number := SYSCALL_RECEIVE;
            when 18   => number := SYSCALL_REPLY;
            when 19   => number := SYSCALL_SEND_EVENT;
            when 20   => number := SYSCALL_RECEIVE_EVENT;
            when 22   => number := SYSCALL_POLL_ANY_IPC;
            when 23   => number := SYSCALL_SUBMIT;
            when 24   => number := SYSCALL_WAIT_COMPLETION;
            when 25   => number := SYSCALL_POLL_COMPLETION;
            when 26   => number := SYSCALL_RECEIVE_EVENT_NB;
            when 27   => number := SYSCALL_GETTIME;
            when 28   => number := SYSCALL_SLEEP;
            when 29   => number := SYSCALL_MAPFB;
            when 30   => number := SYSCALL_INP8;
            when 31   => number := SYSCALL_OUTP8;
            when 32   => number := SYSCALL_INP16;
            when 33   => number := SYSCALL_OUTP16;
            when 36   => number := SYSCALL_INP32;
            when 37   => number := SYSCALL_OUTP32;
            when 40   => number := SYSCALL_CAP_SEND;
            when 41   => number := SYSCALL_CAP_CALL;
            when 42   => number := SYSCALL_CAP_SUBMIT;
            when 43   => number := SYSCALL_NOTIFY;
            when 48   => number := SYSCALL_REPLY_WAIT;
            when 50   => number := SYSCALL_VIRT_TO_PHYS;
            when 51   => number := SYSCALL_SAVE_REPLY_CAP;
            when 52   => number := SYSCALL_REPLY_CAP;
            when 60   => number := SYSCALL_SPAWN;
            when 70   => number := SYSCALL_MAP_DEVICE;
            when 71   => number := SYSCALL_PROCLIST;
            when 72   => number := SYSCALL_MINT_CAP;
            when 73   => number := SYSCALL_RESUME;
            when 74   => number := SYSCALL_ALLOC_DMA;
            when 75   => number := SYSCALL_ENABLE_IRQ;
            when 76   => number := SYSCALL_MAP_INTO;
            when 77   => number := SYSCALL_SET_SYSINFO;
            when 78   => number := SYSCALL_SET_CPU;
            when 80   => number := SYSCALL_POLL_SERVICE_REQUEST;
            when 81   => number := SYSCALL_SET_LATENCY_CONTRACT;
            when 82   => number := SYSCALL_TRACE_RESET;
            when 83   => number := SYSCALL_TRACE_SUMMARY;
            when 84   => number := SYSCALL_INSPECT_CAP;
            when 102  => number := SYSCALL_GRANT;
            when 103  => number := SYSCALL_REVOKE;
            when 106  => number := SYSCALL_GRANT_VIA_CAP;
            when 107  => number := SYSCALL_SET_WELL_KNOWN;
            when 2000 => number := SYSCALL_REGISTER_DRIVER;
            when others =>
                number := SYSCALL_EXIT;
                valid := False;
        end case;
    end decodeSyscall;


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
                             syscallNumRaw : in Unsigned_64)   -- first on stack
                             return Unsigned_64
    is
        percpu : PerCPUData.PerCPUData with
            Import, Volatile, Address => PerCPUData.getPerCPUDataAddr;

        retval     : Unsigned_64 := 0;
        traceActive : constant Boolean := Trace.IsEnabled;
        startTSC   : Unsigned_64 := 0;
        syscallNum : SyscallNumber;
        validSyscall : Boolean;
    begin
        if traceActive then
            startTSC := x86.rdtsc;
            Trace.Emit (Trace.EVENT_SYSCALL_ENTER, syscallNumRaw, arg0);
        end if;

        decodeSyscall (syscallNumRaw, syscallNum, validSyscall);
        if not validSyscall then
            print ("Unknown syscall: "); printd (syscallNumRaw);
            print (" from PID: "); println (percpu.currentPID);
            if traceActive then
                Trace.ObserveDuration (Trace.EVENT_SYSCALL_TIME,
                                       x86.rdtsc - startTSC);
            end if;
            return 0;
        end if;

        case syscallNum is
            when SYSCALL_EXIT =>
                exitp (percpu.currentPID);

            when SYSCALL_KILL =>
                Admin.handleKill (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_GETPID =>
                retval := Unsigned_64 (percpu.currentPID);

            when SYSCALL_WRITE =>
                retval := write (
                    fd    => Descriptors.DescriptorNum(arg0),
                    buf   => Util.numToAddr(arg1),
                    count => arg2);

            when SYSCALL_SBRK =>
                IPC.handleSbrk (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_GETTIME =>
                retval := Time.msTicks;

            when SYSCALL_SLEEP =>
                if arg0 > 0 and arg0 <= 2147483647 then
                    Process.sleep (
                        Time.Duration(arg0) * Time.Milliseconds);
                end if;
                retval := 0;

            when SYSCALL_MAPFB =>
                IPC.handleMapFB (
                    percpu.currentPID, retval);

            when SYSCALL_RECEIVE =>
                IPC.handleReceive (arg0, retval);

            when SYSCALL_REPLY =>
                IPC.handleReply (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_REPLY_CAP =>
                IPC.handleReplyCap (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_RECEIVE_EVENT =>
                declare
                    function tagToU64 is new Ada.Unchecked_Conversion
                        (Process.MessageTag, Unsigned_64);
                    eventMsg : constant Process.Message :=
                        Process.IPC.receiveEvent;
                begin
                    retval := tagToU64 (eventMsg.tag);
                end;

            when SYSCALL_RECEIVE_EVENT_NB =>
                IPC.handleReceiveEventNB (arg0, retval);

            when SYSCALL_SEND_EVENT =>
                IPC.handleSendEvent (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_POLL_ANY_IPC =>
                IPC.handlePollAnyIpc (arg0, retval);

            when SYSCALL_POLL_SERVICE_REQUEST =>
                IPC.handlePollServiceRequest (arg0, retval);

            when SYSCALL_SUBMIT =>
                IPC.handleSubmit (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_WAIT_COMPLETION =>
                IPC.handleWaitCompletion (
                    arg0, arg1, arg2, retval);

            when SYSCALL_POLL_COMPLETION =>
                IPC.handlePollCompletion (arg0, retval);

            when SYSCALL_GRANT =>
                IPC.handleGrant (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, retval);

            when SYSCALL_REVOKE =>
                IPC.handleRevoke (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_INFO =>
                IPC.handleInfo (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_REGISTER_DRIVER =>
                Admin.handleRegisterDriver (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_INP8 | SYSCALL_OUTP8 |
                 SYSCALL_INP16 | SYSCALL_OUTP16 |
                 SYSCALL_INP32 | SYSCALL_OUTP32 =>
                Admin.handlePortIO (
                    percpu.currentPID, syscallNum,
                    arg0, arg1, retval);

            when SYSCALL_VIRT_TO_PHYS =>
                Admin.handleVirtToPhys (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_SAVE_REPLY_CAP =>
                Admin.handleSaveReplyCap (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_CAP_SEND =>
                Admin.handleCapSend (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_CAP_CALL =>
                Admin.handleCapCall (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_CAP_SUBMIT =>
                Admin.handleCapSubmit (
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_NOTIFY =>
                if arg0 >
                   Unsigned_64(Capabilities.CapabilitySlot'Last)
                then
                    retval := 0;
                else
                    if Process.IPC.capNotify (
                        capSlot => Capabilities.CapabilitySlot(arg0))
                    then
                        retval := 1;
                    else
                        retval := 0;
                    end if;
                end if;

            when SYSCALL_REPLY_WAIT =>
                Admin.handleReplyWait (arg0, arg1, retval);

            when SYSCALL_SPAWN =>
                IPC.handleSpawn (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_MAP_DEVICE =>
                IPC.handleMapDevice (
                    percpu.currentPID, arg0, arg1, arg2, retval);

            when SYSCALL_PROCLIST =>
                Admin.handleProclist (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_INSPECT_CAP =>
                Admin.handleInspectCap (
                    percpu.currentPID, arg0, arg1, arg2, retval);

            when SYSCALL_MINT_CAP =>
                Admin.handleMintCap (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, arg5, retval);

            when SYSCALL_RESUME =>
                Admin.handleResume (
                    percpu.currentPID, arg0, retval);

            when SYSCALL_ALLOC_DMA =>
                IPC.handleAllocDma (
                    percpu.currentPID, arg0, arg1, arg2, retval);

            when SYSCALL_ENABLE_IRQ =>
                Admin.handleEnableIrq (
                    percpu.currentPID, arg0, arg1, arg2, retval);

            when SYSCALL_MAP_INTO =>
                IPC.handleMapInto (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_SET_SYSINFO =>
                Admin.handleSetSysinfo (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_SET_CPU =>
                Admin.handleSetCpu (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_SET_LATENCY_CONTRACT =>
                -- Advisory process-local scheduler contract. We validate the
                -- ABI at the syscall boundary, but deliberately do not grant
                -- more CPU yet. Admission control and hard enforcement need a
                -- capability-governed policy pass so realtime cannot become a
                -- denial-of-service footgun.
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
                    Process.setLatencyContract (
                        pid      => percpu.currentPID,
                        class    => Process.LatencyClass'Val (Natural (arg0)),
                        periodUs => Unsigned_32 (arg1),
                        budgetUs => Unsigned_32 (arg2),
                        flags    => Unsigned_32 (arg3));
                    retval := 0;
                end if;

            when SYSCALL_TRACE_RESET =>
                Trace.Reset;
                retval := 0;

            when SYSCALL_TRACE_SUMMARY =>
                Trace.PrintSummary;
                retval := 0;

            when SYSCALL_GRANT_VIA_CAP =>
                IPC.handleGrantViaCap (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, retval);

            when SYSCALL_SET_WELL_KNOWN =>
                Admin.handleSetWellKnown (
                    percpu.currentPID, arg0, arg1, retval);

        end case;

        if traceActive then
            Trace.ObserveDuration (Trace.EVENT_SYSCALL_TIME,
                                   x86.rdtsc - startTSC);
        end if;
        return retval;
    end syscallHandler;


end Syscall;
