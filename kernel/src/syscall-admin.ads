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

    -- REGISTER_DRIVER: arg0 = the role, arg1 = a notification handle with
    -- RIGHT_WRITE covering it.
    procedure handleRegisterDriver (callerPID : Process.ProcessID;
                                    arg0, arg1 : Unsigned_64;
                                    retval    : out Unsigned_64);

    -- Port I/O: arg0 = port, arg1 = value (OUTP), arg2 = the CAP_IOPORT
    -- handle covering the access.
    procedure handlePortIO (callerPID  : Process.ProcessID;
                            syscallNum : SyscallNumber;
                            arg0, arg1, arg2 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleCapSend (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5, arg6 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleCapCall (callerPID  : Process.ProcessID;
                             arg0, arg1, arg2 : Unsigned_64;
                             retval     : out Unsigned_64);

    -- CALL_OFFERING_CAPABILITY: arg0 = endpoint handle, arg1 = message
    -- address (the reply comes back there), arg2 = address of a
    -- Capability_ABI.Derivation (the offer), arg3 = deadline. The reply tag,
    -- or NULL_TAG when the endpoint or the derivation is refused.
    procedure handleCapCallOffering (callerPID : Process.ProcessID;
                                     arg0, arg1, arg2, arg3 : Unsigned_64;
                                     retval : out Unsigned_64);

    procedure handleCapSubmit (arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleReplyWait (arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64);

    -- BOOT_TIMELINE: arg4 = the authority handle (Read: a process
    -- capability with RIGHT_READ; Mark: the owner role's notification).
    procedure handleBootTimeline (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
                                  retval    : out Unsigned_64);

    -- PROCLIST: arg0 = buffer, arg1 = its size, arg2 = a process handle
    -- with RIGHT_READ.
    procedure handleProclist (callerPID  : Process.ProcessID;
                              arg0, arg1, arg2 : Unsigned_64;
                              retval     : out Unsigned_64);

    -- INSPECT_CAPABILITY: arg0 = a handle in the caller's table, arg1 =
    -- address of a Capability_ABI.Inspection. 0, or a refusal code.
    procedure handleInspectCap (callerPID  : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval     : out Unsigned_64);

    -- INSTALL_CAPABILITY (CAP-002): arg0 = a CSPACE handle, arg1 = address
    -- of a Capability_ABI.Derivation. Returns the handle word in the
    -- CSPACE's process, or a refusal code.
    procedure handleInstallCap (callerPID : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval : out Unsigned_64);

    -- DERIVE_CAPABILITY (CAP-002): arg0 = address of a
    -- Capability_ABI.Derivation of one of the caller's own capabilities.
    -- Places the (never wider) copy in the caller's own table: a badged or
    -- narrowed handle to keep or pass on. The handle word, or a refusal.
    procedure handleDeriveCap (callerPID : Process.ProcessID;
                               arg0      : Unsigned_64;
                               retval    : out Unsigned_64);

    -- CLOSE_HANDLE: arg0 = a handle in the caller's table. 0, or a refusal.
    procedure handleCloseHandle (callerPID : Process.ProcessID;
                                 arg0      : Unsigned_64;
                                 retval    : out Unsigned_64);

    -- Device vectors (docs/msi-interrupts.md). Each names the caller's
    -- vector authority by handle: a CAP_IRQ naming the whole device range
    -- (Device_Vector_ABI), RIGHT_GRANT to allocate or release, RIGHT_READ
    -- to read.
    --
    -- SYSCALL_ALLOCATE_DEVICE_VECTORS: arg0 = owner identity, arg1 = count,
    -- arg2 = the caller's CSPACE handle for the owner (which must not have
    -- started), arg3 = the CPU that receives them, arg4 = the vector
    -- authority handle, arg5 = address of a Capability_ABI.Vector_Allocation
    -- (the Device_Vector_ABI allocation word and the owner's new CAP_IRQ
    -- handle). Returns 0, or a refusal code.
    --
    -- RELEASE_DEVICE_VECTORS: arg1 = the vector authority handle; the
    -- owner's CAP_IRQ for the block is revoked. READ_DEVICE_VECTOR: arg2.
    procedure handleAllocateDeviceVectors
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval    : out Unsigned_64);

    -- SYSCALL_RELEASE_DEVICE_VECTORS: arg0 = a block's first vector, after
    -- the caller has masked it at the device. Withdraws the owner's CAP_IRQ
    -- if the owner still lives. Returns 0, or -1.
    procedure handleReleaseDeviceVectors
      (callerPID : Process.ProcessID;
       arg0, arg1 : Unsigned_64;
       retval    : out Unsigned_64);

    -- SYSCALL_READ_DEVICE_VECTOR: arg0 = vector, arg1 =
    -- Device_Vector_ABI.Read_Field. Returns the field, or -1.
    procedure handleReadDeviceVector
      (callerPID  : Process.ProcessID;
       arg0, arg1, arg2 : Unsigned_64;
       retval     : out Unsigned_64);

    -- RESUME: arg0 = target identity, arg1 = a process handle with
    -- RIGHT_EXECUTE for it, arg2 = most frames (0: unlimited), arg3 = CPU
    -- quota (low 32 bits, µs) and period (high 32 bits; 0: unlimited).
    procedure handleResume (callerPID : Process.ProcessID;
                            arg0, arg1, arg2, arg3 : Unsigned_64;
                            retval    : out Unsigned_64);

    -- SYSCALL_INSTALL_LAUNCH_ARGUMENTS (docs/process-arguments.md):
    -- arg0 = target PID, arg1 = source address in the caller, arg2 = length.
    -- The caller needs CAP_PROCESS with RIGHT_EXECUTE for the target, which
    -- must be suspended and never resumed, without arguments yet. Copies the
    -- bytes into fresh pages mapped read-only/NX in the target at
    -- Process_Launch.Arguments_Base and starts its main thread with RDI =
    -- length. Returns 0, or -1 with nothing installed except pages that stay
    -- owned by the target (procmgr then discards the child).
    -- arg3 = a process handle with RIGHT_EXECUTE for the target.
    procedure handleInstallLaunchArguments
       (callerPID        : Process.ProcessID;
        arg0, arg1, arg2, arg3 : Unsigned_64;
        retval           : out Unsigned_64);

    -- SYSCALL_ENABLE_IRQ: a legacy IOAPIC/PIC line (vectors PS2KEYBOARD ..
    -- IDE2) only; message-signaled devices take allocated device vectors.
    -- arg0 = vector, arg1 = owner identity, arg2 = route (Line_Route).
    -- arg3 = a process handle with RIGHT_GRANT for the owner, arg4 = a
    -- CAP_IRQ handle (RIGHT_GRANT) whose range holds the line.
    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handlePublish (callerPID  : Process.ProcessID;
                             arg0, arg1 : Unsigned_64;
                             retval     : out Unsigned_64);

    -- arg2 = a process handle with RIGHT_GRANT for the target.
    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1, arg2 : Unsigned_64;
                            retval     : out Unsigned_64);

    -- MOVE_REPLY_CAPABILITY: the calling thread's current reply authority
    -- becomes a kernel-placed CAP_REPLY handle (returned), or a refusal.
    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   retval    : out Unsigned_64);

    -- KILL: arg0 = target identity, arg1 = a process handle with
    -- RIGHT_WRITE for it.
    procedure handleKill (callerPID : Process.ProcessID;
                          arg0, arg1 : Unsigned_64;
                          retval    : out Unsigned_64);

    -- A control message (docs/data-plane.md): arg0 = the target's identity
    -- (as OP_LAUNCH and EVENT_CHILD_EXIT report it), arg1 = its kind
    -- (IPC_Labels.Control_Kind). The caller must be the target's parent (the
    -- process that launched it, the same incarnation), or hold the process
    -- capability kill needs (RIGHT_WRITE, named by arg2). The target gets EVENT_CONTROL
    -- (kind, sender identity); retval 0, Busy, or an error when the kind is
    -- unknown, the target absent or stale, or the caller unauthorized.
    -- arg2: a process handle with RIGHT_WRITE (unless the caller is the
    -- target's launcher).
    procedure handleSendControl (callerPID : Process.ProcessID;
                                 arg0, arg1, arg2 : Unsigned_64;
                                 retval     : out Unsigned_64);

end Syscall.Admin;
