-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall IPC handlers: replies, events, waits and memory grants.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

with Process;

package Syscall.IPC with
    SPARK_Mode => Off
is

    procedure handleGetOwnedGrantGeneration
      (arg0   : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleAcquireGrant
      (arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleAcquireGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleReturnGrant
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleRevokeGrantReference
      (arg0, arg1 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleReply (arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64);

    procedure handleReplyCap (arg0, arg1, arg2, arg3,
                              arg4, arg5 : Unsigned_64;
                              retval     : out Unsigned_64);

    -- REPLY_WITH_CAPABILITY: arg0 = requester identity, arg1 = reply
    -- message address, arg2 = Capability_ABI.Derivation address. Returns the
    -- handle word in the requester's table, or a refusal code.
    procedure handleReplyWithCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2 : Unsigned_64;
                                  retval : out Unsigned_64);

    -- REPLY_SAVED_WITH_CAPABILITY: arg0 = saved reply handle, arg1 = reply
    -- message address, arg2 = Capability_ABI.Derivation address.
    procedure handleReplySavedWithCap (callerPID : Process.ProcessID;
                                       arg0, arg1, arg2 : Unsigned_64;
                                       retval : out Unsigned_64);

    -- SEND_EVENT: arg6 = the authority handle (a notification with
    -- RIGHT_READ covering a role the destination is registered for, or an
    -- endpoint with RIGHT_WRITE to it).
    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64);

    -- WAIT (ABI-002 slice 2, Wait_ABI): arg0 the Sources word, arg1 the
    -- deadline, arg2 .. arg4 the source's parameters A, B, C.
    procedure handleWait (sources, deadline, a, b, c, wakeWord : Unsigned_64;
                          retval : out Unsigned_64);

    procedure handleGrant (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3 : Unsigned_64;
                           retval : out Unsigned_64);

    procedure handleRevoke (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64);

    procedure handleDeriveGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleGrantViaCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval : out Unsigned_64);

end Syscall.IPC;
