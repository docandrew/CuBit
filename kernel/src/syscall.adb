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
    -- read
    -- @TODO
    ---------------------------------------------------------------------------
    function read (fd    : in Descriptors.DescriptorNum;
                   buf   : in System.Address;
                   count : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end read;

    ---------------------------------------------------------------------------
    -- close
    -- @TODO
    ---------------------------------------------------------------------------
    function close (fd : in Descriptors.DescriptorNum) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end close;

    ---------------------------------------------------------------------------
    -- execve
    -- @TODO
    ---------------------------------------------------------------------------
    function execve (exename   : in System.Address;
                     args      : in System.Address;
                     env       : in System.Address) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end execve;

    ---------------------------------------------------------------------------
    -- fork
    -- @Note Debatable whether CuBit will support this.
    ---------------------------------------------------------------------------


    ---------------------------------------------------------------------------
    -- open
    -- @TODO
    ---------------------------------------------------------------------------
    function open (filenameLen : in Unsigned_64;
                   filename    : in System.Address;
                   flags       : in Unsigned_64;
                   mode        : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        --return Filesystem.VFS.Paths.open (filenameLen, filename, flags, mode);
        return 0;
    end open;

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
    -- syscallHandler — thin dispatcher
    -- We get parameters passed here using the SysV ABI
    ---------------------------------------------------------------------------
    function syscallHandler (arg0,   -- rdi
                             arg1,   -- rsi
                             arg2,   -- rdx
                             arg3,   -- rcx
                             arg4,   -- r8
                             arg5,   -- r9
                             syscallNum : in Unsigned_64)   -- first on stack
                             return Unsigned_64
    is
        percpu : PerCPUData.PerCPUData with
            Import, Volatile, Address => PerCPUData.getPerCPUDataAddr;

        retval : Unsigned_64 := 0;
    begin
        case syscallNum is
            when SYSCALL_EXIT =>
                exitp (percpu.currentPID);

            when SYSCALL_READ =>
                retval := read (
                    fd    => Descriptors.DescriptorNum(arg0),
                    buf   => Util.numToAddr(arg1),
                    count => arg2);

            when SYSCALL_CLOSE =>
                retval := close (
                    fd => Descriptors.DescriptorNum(arg0));

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

            when SYSCALL_OPEN =>
                retval := open (
                    filenameLen => arg0,
                    filename    => Util.numToAddr(arg1),
                    flags       => arg2,
                    mode        => arg3);

            when SYSCALL_RECEIVE =>
                IPC.handleReceive (arg0, retval);

            when SYSCALL_SEND =>
                IPC.handleSend (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, arg5,
                    retval);

            when SYSCALL_REPLY =>
                IPC.handleReply (
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

            when SYSCALL_CALL =>
                IPC.handleCall (
                    percpu.currentPID, arg0, arg1, retval);

            when SYSCALL_RECEIVE_NB =>
                IPC.handleReceiveNB (arg0, retval);

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
                 SYSCALL_INPS16 | SYSCALL_OUTPS16 |
                 SYSCALL_INP32 | SYSCALL_OUTP32 =>
                Admin.handlePortIO (
                    percpu.currentPID, syscallNum,
                    arg0, arg1, arg2, retval);

            when SYSCALL_VIRT_TO_PHYS =>
                Admin.handleVirtToPhys (
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

            when SYSCALL_NOTIFY_WAIT =>
                retval := Process.IPC.notifyWait;

            when SYSCALL_NOTIFY_POLL =>
                retval := Process.IPC.notifyPoll;

            when SYSCALL_BIND_NOTIFICATION =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    declare
                        function toErr is new Ada.Unchecked_Conversion
                            (Long_Integer, Unsigned_64);
                    begin
                        retval := toErr (-1);
                    end;
                else
                    Process.IPC.bindNotification (
                        notifPID => Process.ProcessID(arg0));
                    retval := 1;
                end if;

            when SYSCALL_UNBIND_NOTIFICATION =>
                Process.IPC.unbindNotification;
                retval := 1;

            when SYSCALL_REPLY_WAIT =>
                Admin.handleReplyWait (arg0, arg1, retval);

            when SYSCALL_CONTROLACCESS =>
                Admin.handleControlAccess (
                    percpu.currentPID,
                    arg0, arg1, arg2, arg3, arg4, retval);

            when SYSCALL_GETTICKET =>
                Admin.handleGetTicket (
                    percpu.currentPID, arg0, arg1, retval);

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

            when SYSCALL_SET_SUPERVISOR =>
                Admin.handleSetSupervisor (
                    percpu.currentPID, arg0, arg1, retval);

            when others =>
                print ("Syscall: "); printd (syscallNum);
                print (" from PID: ");
                println (percpu.currentPID);
                println (" with args: ");
                print ("  "); println (arg0);
                print ("  "); println (arg1);
                print ("  "); println (arg2);
                print ("  "); println (arg3);
                print ("  "); println (arg4);
                print ("  "); println (arg5);
        end case;

        return retval;
    end syscallHandler;


end Syscall;
