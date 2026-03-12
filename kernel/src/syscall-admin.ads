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
                            arg0, arg1, arg2 : Unsigned_64;
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
                               arg4, arg5 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleReplyWait (arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleControlAccess (callerPID : Process.ProcessID;
                                   arg0, arg1, arg2, arg3,
                                   arg4 : Unsigned_64;
                                   retval : out Unsigned_64);

    procedure handleGetTicket (callerPID  : Process.ProcessID;
                               arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleProclist (callerPID  : Process.ProcessID;
                              arg0, arg1 : Unsigned_64;
                              retval     : out Unsigned_64);

    procedure handleMintCap (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5 : Unsigned_64;
                             retval : out Unsigned_64);

    procedure handleResume (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64);

    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleSetSysinfo (callerPID  : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval     : out Unsigned_64);

    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleSetSupervisor (callerPID  : Process.ProcessID;
                                   arg0, arg1 : Unsigned_64;
                                   retval     : out Unsigned_64);

    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   arg0      : Unsigned_64;
                                   retval    : out Unsigned_64);

end Syscall.Admin;
