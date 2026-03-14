-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall IPC, memory, and process creation handlers.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;

package Syscall.IPC with
    SPARK_Mode => Off
is

    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64);

    procedure handleMapDevice (callerPID : Process.ProcessID;
                               arg0, arg1, arg2 : Unsigned_64;
                               retval    : out Unsigned_64);

    procedure handleAllocDma (callerPID : Process.ProcessID;
                              arg0, arg1, arg2 : Unsigned_64;
                              retval    : out Unsigned_64);

    procedure handleMapInto (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4 : Unsigned_64;
                             retval    : out Unsigned_64);

    procedure handleSbrk (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64);

    procedure handleMapFB (callerPID : Process.ProcessID;
                           retval    : out Unsigned_64);

    procedure handleReceive (arg0   : Unsigned_64;
                             retval : out Unsigned_64);

    procedure handleSend (callerPID : Process.ProcessID;
                          arg0, arg1, arg2, arg3,
                          arg4, arg5 : Unsigned_64;
                          retval     : out Unsigned_64);

    procedure handleReply (arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64);

    procedure handleReceiveEventNB (arg0   : Unsigned_64;
                                    retval : out Unsigned_64);

    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handleCall (callerPID  : Process.ProcessID;
                          arg0, arg1 : Unsigned_64;
                          retval     : out Unsigned_64);

    procedure handleReceiveNB (arg0   : Unsigned_64;
                               retval : out Unsigned_64);

    procedure handleSubmit (arg0, arg1, arg2, arg3,
                            arg4, arg5 : Unsigned_64;
                            retval     : out Unsigned_64);

    procedure handleWaitCompletion (arg0, arg1, arg2 : Unsigned_64;
                                    retval : out Unsigned_64);

    procedure handlePollCompletion (arg0   : Unsigned_64;
                                    retval : out Unsigned_64);

    procedure handleGrant (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3 : Unsigned_64;
                           retval : out Unsigned_64);

    procedure handleRevoke (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64);

    procedure handleInfo (callerPID  : Process.ProcessID;
                          arg0, arg1 : Unsigned_64;
                          retval     : out Unsigned_64);

    procedure handleGrantViaCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval : out Unsigned_64);

end Syscall.IPC;
