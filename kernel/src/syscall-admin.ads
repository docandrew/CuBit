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

    procedure handleVirtToPhys (callerPID : Process.ProcessID;
                                arg0      : Unsigned_64;
                                retval    : out Unsigned_64);

    procedure handleCapSend (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleCapCall (callerPID  : Process.ProcessID;
                             arg0, arg1 : Unsigned_64;
                             retval     : out Unsigned_64);

    procedure handleCapSubmit (arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleReplyWait (arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64);

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

    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleSetSysinfo (callerPID  : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval     : out Unsigned_64);

    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   arg0      : Unsigned_64;
                                   retval    : out Unsigned_64);

    procedure handleKill (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64);

    -- A control message (docs/data-plane.md): arg0 = target PID, arg1 = its
    -- kind (IPC_Labels.Control_Kind), arg2 = the target's generation (its
    -- incarnation, as OP_LAUNCH and EVENT_CHILD_EXIT report it). The caller must be the target's
    -- parent (the process that launched it, the same incarnation), or hold
    -- the process capability kill needs (RIGHT_WRITE). The target gets EVENT_CONTROL
    -- (kind, sender) in its event lane; retval 0, or an error when the kind
    -- is unknown, the target absent, the caller unauthorized, or the
    -- target's event lane full.
    procedure handleSendControl (callerPID : Process.ProcessID;
                                 arg0, arg1, arg2 : Unsigned_64;
                                 retval     : out Unsigned_64);

    procedure handleSetWellKnown (callerPID : Process.ProcessID;
                                   arg0, arg1 : Unsigned_64;
                                   retval     : out Unsigned_64);

end Syscall.Admin;
