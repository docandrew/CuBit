-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall privileged/management handlers: capabilities, port I/O,
-- process management, system configuration.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;

package Syscall.Admin with
    SPARK_Mode => Off
is

    procedure handleRegisterDriver (callerPID : Process.ProcessID;
                                    arg0      : Unsigned_64;
                                    retval    : out Unsigned_64);

    procedure handlePortIO (callerPID  : Process.ProcessID;
                            syscallNum : SyscallNumber;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleCapSend (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5, arg6 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleCapCall (callerPID  : Process.ProcessID;
                             arg0, arg1, arg2 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleCapSubmit (arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleReplyWait (arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleBootTimeline (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval    : out Unsigned_64);

    procedure handleProclist (callerPID  : Process.ProcessID;
                              arg0, arg1 : Unsigned_64;
                              retval     : out Unsigned_64);

    procedure handleInspectCap (callerPID  : Process.ProcessID;
                                arg0, arg1, arg2 : Unsigned_64;
                                retval     : out Unsigned_64);

    procedure handleMintCap (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5 : Unsigned_64;
                             retval : out Unsigned_64;
                             boundRecipient : Boolean := False);

    procedure handleDelegateEndpoint
      (callerPID : Process.ProcessID;
       recipient, sourceSlot, destinationSlot, rights, tag, reserved : Unsigned_64;
       retval : out Unsigned_64);

    -- Device vectors (docs/msi-interrupts.md). Each needs the caller's
    -- vector authority: a CAP_IRQ naming the whole device range
    -- (Device_Vector_ABI), RIGHT_GRANT to allocate or release, RIGHT_READ
    -- to read.
    --
    -- SYSCALL_ALLOCATE_DEVICE_VECTORS: arg0 = owner identity, arg1 = count,
    -- arg2 = the owner's empty slot that receives CAP_IRQ (first vector,
    -- count), arg3 = the CPU that receives them. The caller also needs a
    -- capability-space grant for the owner. Returns
    -- Device_Vector_ABI.Allocation_Word, or -1.
    procedure handleAllocateDeviceVectors
      (callerPID              : Process.ProcessID;
       arg0, arg1, arg2, arg3 : Unsigned_64;
       retval                 : out Unsigned_64);

    -- SYSCALL_RELEASE_DEVICE_VECTORS: arg0 = a block's first vector, after
    -- the caller has masked it at the device. Withdraws the owner's CAP_IRQ
    -- if the owner still lives. Returns 0, or -1.
    procedure handleReleaseDeviceVectors
      (callerPID : Process.ProcessID;
       arg0      : Unsigned_64;
       retval    : out Unsigned_64);

    -- SYSCALL_READ_DEVICE_VECTOR: arg0 = vector, arg1 =
    -- Device_Vector_ABI.Read_Field. Returns the field, or -1.
    procedure handleReadDeviceVector
      (callerPID  : Process.ProcessID;
       arg0, arg1 : Unsigned_64;
       retval     : out Unsigned_64);

    procedure handleResume (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64);

    -- SYSCALL_INSTALL_LAUNCH_ARGUMENTS (docs/process-arguments.md):
    -- arg0 = target PID, arg1 = source address in the caller, arg2 = length.
    -- The caller needs CAP_PROCESS with RIGHT_EXECUTE for the target, which
    -- must be suspended and never resumed, without arguments yet. Copies the
    -- bytes into fresh pages mapped read-only/NX in the target at
    -- Process_Launch.Arguments_Base and starts its main thread with RDI =
    -- length. Returns 0, or -1 with nothing installed except pages that stay
    -- owned by the target (procmgr then discards the child).
    procedure handleInstallLaunchArguments
       (callerPID        : Process.ProcessID;
        arg0, arg1, arg2 : Unsigned_64;
        retval           : out Unsigned_64);

    -- SYSCALL_ENABLE_IRQ: a legacy IOAPIC/PIC line (vectors PS2KEYBOARD ..
    -- IDE2) only; message-signaled devices take allocated device vectors.
    -- arg0 = vector, arg1 = owner identity, arg2 = route (Line_Route).
    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handlePublish (callerPID  : Process.ProcessID;
                             arg0, arg1 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   arg0      : Unsigned_64;
                                   retval    : out Unsigned_64);

    -- RESTORE_REPLY_CAPABILITY: arg0 = a slot holding a reply capability
    -- saved by MOVE_REPLY_CAPABILITY; it becomes the calling thread's
    -- current reply authority again. 1 on success, 0 if the slot holds no
    -- reply capability or the thread already has a current one.
    procedure handleRestoreReplyCap (callerPID : Process.ProcessID;
                                     arg0      : Unsigned_64;
                                     retval    : out Unsigned_64);

    procedure handleKill (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64);

    -- A control message (docs/data-plane.md): arg0 = the target's identity
    -- (as OP_LAUNCH and EVENT_CHILD_EXIT report it), arg1 = its kind
    -- (IPC_Labels.Control_Kind). The caller must be the target's parent (the
    -- process that launched it, the same incarnation), or hold the process
    -- capability kill needs (RIGHT_WRITE). The target gets EVENT_CONTROL
    -- (kind, sender identity); retval 0, Busy, or an error when the kind is
    -- unknown, the target absent or stale, or the caller unauthorized.
    procedure handleSendControl (callerPID : Process.ProcessID;
                                 arg0, arg1 : Unsigned_64;
                                 retval     : out Unsigned_64);

end Syscall.Admin;
