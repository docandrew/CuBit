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

    -- SPAWN (CAP-002): arg0 = spawn authority (a CAP_PROCESS handle with
    -- RIGHT_EXECUTE), arg1 = address of a Capability_ABI.Spawn_Request,
    -- arg2 = address of a Capability_ABI.Spawn_Result. Creates the process
    -- suspended. Returns 0, or a refusal code.
    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0, arg1, arg2 : Unsigned_64;
                           retval     : out Unsigned_64);

    -- MAP_DEVICE: arg4 = the CAP_DEVICE_MEM handle covering the pages.
    procedure handleMapDevice (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
                               retval    : out Unsigned_64);

    -- ALLOC_DMA: arg5 = a process handle with RIGHT_GRANT for the target.
    procedure handleAllocDma (callerPID : Process.ProcessID;
                              arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
                              retval    : out Unsigned_64);

    -- MAP_INTO: arg5 = a process handle with RIGHT_GRANT for the target,
    -- arg6 = the CAP_DEVICE_MEM handle (device kind only).
    procedure handleMapInto (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5, arg6 : Unsigned_64;
                             retval    : out Unsigned_64);

    procedure handleSbrk (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64);

    -- MAPFB: arg0 = the CAP_DEVICE_MEM handle covering the framebuffer.
    procedure handleMapFB (callerPID : Process.ProcessID;
                           arg0      : Unsigned_64;
                           retval    : out Unsigned_64);

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

    procedure handleReceive (arg0   : Unsigned_64;
                             retval : out Unsigned_64);

    procedure handleReceiveUntil (arg0, arg1 : Unsigned_64;
                                  retval     : out Unsigned_64);

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

    procedure handleReceiveEventNB (arg0, arg1 : Unsigned_64;
                                    retval : out Unsigned_64);

    -- SEND_EVENT: arg6 = the authority handle (a notification with
    -- RIGHT_READ covering a role the destination is registered for, or an
    -- endpoint with RIGHT_WRITE to it).
    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64);

    procedure handlePollServiceRequest (arg0   : Unsigned_64;
                                        retval : out Unsigned_64);

    procedure handlePollAnyIpc (arg0   : Unsigned_64;
                                retval : out Unsigned_64);

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

    procedure handleReadStatistics (callerPID  : Process.ProcessID;
                                    arg0, arg1 : Unsigned_64;
                                    retval     : out Unsigned_64);

    procedure handleDeriveGrantViaCap
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval : out Unsigned_64);

    procedure handleGrantViaCap (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3 : Unsigned_64;
                                  retval : out Unsigned_64);

end Syscall.IPC;
