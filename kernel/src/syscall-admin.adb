-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall privileged/management handler implementations: capabilities,
-- port I/O, process management, system configuration.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with Capabilities;
with Capabilities.IRQ;
with Config;
with Capabilities.Operations;
with InterruptNumbers;
with IPC_Labels;
with Kernel_Controls;
with Interrupts;
with PerCpuData;
with PerCPUData;
with Process;
with Memory_Accounting;
with Process.IPC;
with Process.User_Memory;
with Process_Launch;
with Process_Identities;
with User_Buffer_Copy;
with Spinlocks;
with Process_Lifetime;
with Sysinfo;
with TextIO; use TextIO;
with Time;
with Util;
with Virtmem;
with x86;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.Admin is

    function toErr is
        new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
    reterr : constant Unsigned_64 := toErr (-1);
    -- Not accepted now, but may be later (the caller keeps its request).
    retbusy : constant Unsigned_64 := toErr (-2);

    function tagToU64 is new Ada.Unchecked_Conversion
        (Process.MessageTag, Unsigned_64);
    function u64ToTag is new Ada.Unchecked_Conversion
        (Unsigned_64, Process.MessageTag);
    function priToU16 is new Ada.Unchecked_Conversion
        (Integer_16, Unsigned_16);

    ---------------------------------------------------------------------------
    -- hasCapProcessFor - check if caller has CAP_PROCESS with a given right
    -- targeting a specific PID (ref=0 is wildcard, otherwise gen must match).
    ---------------------------------------------------------------------------
    function hasCapProcessFor (callerPID : Process.ProcessID;
                               targetPID : Process.ProcessID;
                               right     : Capabilities.CapabilityRight)
                               return Boolean
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
    begin
        for slot in Capabilities.CapabilitySlot loop
            cap := Process.proctab(callerPID).caps(slot);
            if cap.capType = Capabilities.CAP_PROCESS and then
               cap.rights(right) and then
               (cap.object.ref = 0 or
                (cap.gen = Process.generationOf (targetPID)
                 and then cap.object.ref = Unsigned_64 (targetPID)))
            then
                return True;
            end if;
        end loop;
        return False;
    end hasCapProcessFor;

    ---------------------------------------------------------------------------
    -- hasCspaceGrantFor
    --
    -- Installing authority into another process is a capability-space
    -- administration operation. It must never be inferred from ordinary
    -- CAP_PROCESS control. A scoped CAP_CSPACE is generation-bound to its
    -- target; ref=0 is the explicit bootstrap policy root.
    ---------------------------------------------------------------------------
    function hasCspaceGrantFor
      (callerPID : Process.ProcessID;
       targetPID : Process.ProcessID) return Boolean
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
    begin
        for slot in Capabilities.CapabilitySlot loop
            cap := Process.proctab(callerPID).caps(slot);
            if cap.capType = Capabilities.CAP_CSPACE and then
               cap.rights(Capabilities.RIGHT_GRANT) and then
               (cap.object.ref = 0 or else
                (cap.object.ref = Unsigned_64 (targetPID) and then
                 cap.gen = Process.generationOf (targetPID)))
            then
                return True;
            end if;
        end loop;
        return False;
    end hasCspaceGrantFor;

    ---------------------------------------------------------------------------
    -- canDelegateCspace
    --
    -- A CAP_CSPACE may itself be delegated, but never with a wider target
    -- scope or additional rights. This is the non-amplification rule for the
    -- policy-root capability rather than a special case in userspace policy.
    ---------------------------------------------------------------------------
    function canDelegateCspace
      (callerPID : Process.ProcessID;
       targetPID : Process.ProcessID;
       newRef    : Unsigned_64;
       newRights : Capabilities.CapabilityRights) return Boolean
    is
        use type Capabilities.CapabilityType;
        cap : Capabilities.Capability;
    begin
        for slot in Capabilities.CapabilitySlot loop
            cap := Process.proctab(callerPID).caps(slot);
            if cap.capType = Capabilities.CAP_CSPACE and then
               cap.rights(Capabilities.RIGHT_GRANT) and then
               (cap.object.ref = 0 or else
                (cap.object.ref = Unsigned_64 (targetPID) and then
                 cap.gen = Process.generationOf (targetPID))) and then
               Capabilities.isSubsetOf (newRights, cap.rights) and then
               (cap.object.ref = 0 or else newRef = cap.object.ref)
            then
                return True;
            end if;
        end loop;
        return False;
    end canDelegateCspace;


    ---------------------------------------------------------------------------
    -- handleRegisterDriver
    ---------------------------------------------------------------------------
    procedure handleRegisterDriver (callerPID : Process.ProcessID;
                                    arg0      : Unsigned_64;
                                    retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        hasCap : Boolean := False;
    begin
        retval := reterr;

        if arg0 > Unsigned_64 (Sysinfo.DriverID'Last) then
            return;
        end if;

        -- Kernel-mode threads are exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_NOTIFICATION and then
                   Process.proctab(callerPID).caps(slot).object.ref =
                   arg0 and then
                   Process.proctab(callerPID).caps(slot).rights(
                       Capabilities.RIGHT_WRITE)
                then
                    hasCap := True;
                    exit;
                end if;
            end loop;
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_REGISTER_DRIVER)),
                arg0, 0);
            return;
        end if;

        retval := Sysinfo.registerDriver (
            pid    => callerPID,
            driver => Sysinfo.DriverID(arg0));
        -- The registrant's identity, not its slot (KERN-003).
        if retval = Unsigned_64 (callerPID) then
            retval := Process_Identities.To_Word (Process.identityOf (callerPID));
        end if;
    end handleRegisterDriver;

    ---------------------------------------------------------------------------
    -- handlePortIO
    ---------------------------------------------------------------------------
    procedure handlePortIO (callerPID  : Process.ProcessID;
                            syscallNum : SyscallNumber;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is

        capAllowed : Boolean;

        procedure logDenied (port : Unsigned_64;
                             size : Unsigned_64;
                             writeAccess : Boolean) is
        begin
            print ("PORTIO: denied pid=");
            print (Unsigned_16 (callerPID));
            print (" syscall=");
            print (Unsigned_64 (SyscallNumber'Enum_Rep (syscallNum)));
            print (" port=");
            print (port and 16#FFFF#);
            print (" size=");
            print (size);
            print (" write=");
            println (writeAccess);
        end logDenied;
    begin
        case syscallNum is
            when SYSCALL_INP8 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 1, False, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 1, False);
                    retval := reterr;
                else
                    declare
                        val : Unsigned_8;
                    begin
                        x86.in8 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end;
                end if;

            when SYSCALL_OUTP8 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 1, True, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 1, True);
                    retval := reterr;
                else
                    x86.out8 (x86.IOPort(arg0 and 16#FFFF#),
                              Unsigned_8(arg1 and 16#FF#));
                    retval := 0;
                end if;

            when SYSCALL_INP16 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 2, False, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 2, False);
                    retval := reterr;
                else
                    declare
                        val : Unsigned_16;
                    begin
                        x86.in16 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end;
                end if;

            when SYSCALL_OUTP16 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 2, True, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 2, True);
                    retval := reterr;
                else
                    x86.out16 (x86.IOPort(arg0 and 16#FFFF#),
                               Unsigned_16(arg1 and 16#FFFF#));
                    retval := 0;
                end if;

            when SYSCALL_INP32 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 4, False, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 4, False);
                    retval := reterr;
                else
                    declare
                        val : Unsigned_32;
                    begin
                        x86.in32 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end;
                end if;

            when SYSCALL_OUTP32 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 4, True, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 4, True);
                    retval := reterr;
                else
                    x86.out32 (x86.IOPort(arg0 and 16#FFFF#),
                               Unsigned_32(arg1 and 16#FFFF_FFFF#));
                    retval := 0;
                end if;

            when others =>
                retval := reterr;
        end case;
    end handlePortIO;

    ---------------------------------------------------------------------------
    -- handleVirtToPhys
    ---------------------------------------------------------------------------
    procedure handleVirtToPhys (callerPID : Process.ProcessID;
                                arg0      : Unsigned_64;
                                retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        phys   : Virtmem.PhysAddress;
        hasCap : Boolean := False;
    begin
        -- Kernel-mode threads exempt
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_DEVICE_MEM
                then
                    hasCap := True;
                    exit;
                end if;
            end loop;
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_VIRT_TO_PHYS)),
                arg0, 0);
            retval := reterr;
        else
            phys := Virtmem.tableWalk (
                Virtmem.VirtAddress(arg0),
                Process.addrtab(callerPID), Allow_Big => True);
            if phys = 0 then
                retval := reterr;
            else
                retval := Unsigned_64(phys) + (arg0 and 16#FFF#);
            end if;
        end if;
    end handleVirtToPhys;

    ---------------------------------------------------------------------------
    -- handleCapSend
    ---------------------------------------------------------------------------
    procedure handleCapSend (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5, arg6 : Unsigned_64;
                             retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is




        sendMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => (arg2, arg3, arg4, arg5));
        replyTag : Process.MessageTag;
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := reterr;
        else
            -- arg6: the deadline (absolute monotonic ms; Unsigned_64'Last is
            -- for ever, as the caller chose: docs/ipc-fastpath.md).
            replyTag := Process.IPC.capSend (
                capSlot => Capabilities.CapabilitySlot(arg0),
                msg     => sendMsg,
                deadlineMs => arg6);
            Process.threadtab (PerCPUData.getCurrentThread).replyMsg :=
                Process.NULL_MESSAGE;
            retval := tagToU64 (replyTag);
        end if;
    end handleCapSend;

    ---------------------------------------------------------------------------
    -- handleCapCall
    ---------------------------------------------------------------------------
    procedure handleCapCall (callerPID  : Process.ProcessID;
                             arg0, arg1, arg2 : Unsigned_64;
                             retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        localMsg : Process.Message;
        replyTag : Process.MessageTag;
        ok       : Boolean;
    begin
        -- arg1 is a user pointer: read and written through
        -- Process.User_Memory, never dereferenced; the reply's destination
        -- is checked before the call is made.
        if arg0 > Unsigned_64 (Capabilities.CapabilitySlot'Last) or else
           not Process.User_Memory.Message_Writable (callerPID, arg1)
        then
            retval := reterr;
            return;
        end if;
        Process.User_Memory.Read_Message (callerPID, arg1, localMsg, ok);
        if not ok then
            retval := reterr;
            return;
        end if;
        -- arg2: the deadline (absolute monotonic ms; Unsigned_64'Last is
        -- for ever, as the caller chose: docs/ipc-fastpath.md).
        replyTag := Process.IPC.capCall
          (capSlot => Capabilities.CapabilitySlot (arg0), msg => localMsg,
           deadlineMs => arg2);
        Process.User_Memory.Write_Message
          (callerPID, arg1, Process.threadtab (PerCPUData.getCurrentThread).replyMsg, ok);
        Process.threadtab (PerCPUData.getCurrentThread).replyMsg := Process.NULL_MESSAGE;
        retval := (if ok then tagToU64 (replyTag) else reterr);
    end handleCapCall;

    ---------------------------------------------------------------------------
    -- handleCapSubmit
    ---------------------------------------------------------------------------
    procedure handleCapSubmit (arg0, arg1, arg2, arg3,
                               arg4, arg5, arg6 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is


        submitMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            authorityTag => 0,
            words    => [arg2, arg3, arg4, arg5]);
        ok : Boolean;
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := 0;
        else
            ok := Process.IPC.capSubmit (
                capSlot => Capabilities.CapabilitySlot(arg0),
                msg     => submitMsg,
                token   => arg6);
            if ok then
                retval := 1;
            else
                retval := 0;
            end if;
        end if;
    end handleCapSubmit;

    ---------------------------------------------------------------------------
    -- handleReplyWait
    ---------------------------------------------------------------------------
    procedure handleReplyWait (arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        caller   : constant Process.ProcessID := PerCPUData.getCurrentPID;
        from     : Process.ProcessID;
        localMsg : Process.Message;
        recvMsg  : Process.Message;
        ok       : Boolean;
    begin
        -- arg1 is a user pointer (the reply, then the next request):
        -- checked, never dereferenced.
        if not Process.User_Memory.Message_Writable (caller, arg1) then
            retval := reterr;
            return;
        end if;
        Process.User_Memory.Read_Message (caller, arg1, localMsg, ok);
        if not ok then
            retval := reterr;
            return;
        end if;
        Process.IPC.replyWait
          (replyTo => Process.processOfIdentity (Process_Identities.From_Word (arg0)), replyMsg => localMsg,
           from => from, msg => recvMsg);
        Process.User_Memory.Write_Message (caller, arg1, recvMsg, ok);
        retval := (if ok then Process_Identities.To_Word (Process.identityOf (from)) else reterr);
    end handleReplyWait;

    ---------------------------------------------------------------------------
    -- handleProclist
    ---------------------------------------------------------------------------
    procedure handleProclist (callerPID  : Process.ProcessID;
                              arg0, arg1 : Unsigned_64;
                              retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        hasCap     : Boolean := False;
        bufSize    : constant Unsigned_64 := arg1;
        ENTRY_SIZE : constant := 32;
        maxEntries : Unsigned_64;
        count      : Unsigned_64 := 0;
    begin
        -- Check CAP_PROCESS with RIGHT_READ (no target PID)
        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_READ)
            then
                hasCap := True;
                exit;
            end if;
        end loop;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (SYSCALL_PROCLIST)),
                arg0, arg1);
            retval := reterr;
            return;
        end if;

        if bufSize < ENTRY_SIZE then
            retval := 0;
            return;
        end if;

        maxEntries := bufSize / ENTRY_SIZE;

        -- Each entry is built here and copied to the caller's buffer
        -- (arg0, a user pointer) through Process.User_Memory.
        for i in Process.ProctabRange loop
            exit when count >= maxEntries;

            if Process.threadOf (i).state /= Process.INVALID then
                declare
                    -- The process by identity (KERN-003), then its state.
                    type Proc_Entry is record
                        identity : Unsigned_64;
                        state    : Unsigned_8;
                        cpu      : Unsigned_8;
                        priority : Unsigned_16;
                        frames   : Unsigned_32;
                        name     : String (1 .. 16);
                    end record;
                    for Proc_Entry use record
                        identity at 0 range 0 .. 63;
                        state    at 8 range 0 .. 7;
                        cpu      at 9 range 0 .. 7;
                        priority at 10 range 0 .. 15;
                        frames   at 12 range 0 .. 31;
                        name     at 16 range 0 .. 127;
                    end record;
                    for Proc_Entry'Size use ENTRY_SIZE * 8;
                    item : aliased constant Proc_Entry :=
                      (identity => Process_Identities.To_Word (Process.identityOf (i)),
                       state    => Process.ProcessState'Pos (Process.threadOf (i).state),
                       cpu      => Unsigned_8 (Process.threadOf (i).cpu),
                       priority => priToU16 (Integer_16 (Process.threadOf (i).priority)),
                       frames   => Unsigned_32 (Process.proctab(i).frames.length),
                       name     => Process.proctab(i).name);
                    copied : Boolean;
                begin
                    Process.User_Memory.Copy_To_User
                      (callerPID, arg0 + count * ENTRY_SIZE, item'Address, ENTRY_SIZE, copied);
                    if not copied then
                        retval := reterr;
                        return;
                    end if;
                    count := count + 1;
                end;
            end if;
        end loop;

        retval := count;
    end handleProclist;

    ---------------------------------------------------------------------------
    -- handleInspectCap
    ---------------------------------------------------------------------------
    procedure handleInspectCap (callerPID  : Process.ProcessID;
                                arg0, arg1, arg2 : Unsigned_64;
                                retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;

        targetPID : Process.ProcessID;
        slot      : Capabilities.CapabilitySlot;
        cap       : Capabilities.Capability;
        -- arg2 is a user pointer: six words, built here and copied out
        -- through Process.User_Memory.
        Inspection_Words : constant := 6;
        type Inspection is array (1 .. Inspection_Words) of Unsigned_64;
        result    : aliased Inspection := (others => 0);
        copied    : Boolean;
        rights    : Unsigned_64 := 0;
    begin
        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg0));
        if targetPID = Process.NO_PROCESS or else
           arg1 > Unsigned_64 (Capabilities.CapabilitySlot'Last) or else
           arg2 = 0
        then
            retval := reterr;
            return;
        end if;

        slot := Capabilities.CapabilitySlot (arg1);

        Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
        if not Process.proctab(targetPID).admitted or else
           Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime)
        then
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
            retval := reterr;
            return;
        elsif not hasCapProcessFor (callerPID, targetPID,
                                    Capabilities.RIGHT_READ)
        then
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_INSPECT_CAPABILITY)),
                arg0, arg1);
            retval := reterr;
            return;
        end if;

        cap := Process.proctab(targetPID).caps(slot);
        Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        if cap.rights(Capabilities.RIGHT_READ) then
            rights := rights or 1;
        end if;
        if cap.rights(Capabilities.RIGHT_WRITE) then
            rights := rights or 2;
        end if;
        if cap.rights(Capabilities.RIGHT_EXECUTE) then
            rights := rights or 4;
        end if;
        if cap.rights(Capabilities.RIGHT_GRANT) then
            rights := rights or 8;
        end if;
        if cap.rights(Capabilities.RIGHT_REVOKE) then
            rights := rights or 16;
        end if;

        result := (Unsigned_64 (Capabilities.CapabilityType'Pos (cap.capType)),
                   rights, cap.authorityTag,
                   -- Its process by identity (the slot and generation it
                   -- was made for), not the bare slot.
                   (if cap.capType in Capabilities.CAP_ENDPOINT |
                                      Capabilities.CAP_PROCESS |
                                      Capabilities.CAP_CSPACE
                       and then cap.object.ref /= 0
                       and then cap.object.ref <= Unsigned_64 (Process.ProcessID'Last)
                    then Process_Identities.To_Word (Process_Identities.Encode
                           (Process_Identities.Slot (cap.object.ref),
                            Process_Identities.Generation (cap.gen)))
                    else cap.object.ref),
                   cap.object.param, Unsigned_64 (cap.gen));
        Process.User_Memory.Copy_To_User
          (callerPID, arg2, result'Address, result'Size / 8, copied);
        if not copied then
            retval := reterr;
            return;
        end if;

        retval := 1;
    end handleInspectCap;

    ---------------------------------------------------------------------------
    -- handleMintCap
    ---------------------------------------------------------------------------
    procedure handleMintCap (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5 : Unsigned_64;
                             retval : out Unsigned_64;
                             boundRecipient : Boolean := False) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID  : Process.ProcessID;
        capTypePos : Natural;
        newCap     : Capabilities.Capability;
        newRights  : Capabilities.CapabilityRights;
        targetSlot : Capabilities.CapabilitySlot;
        expectedGeneration : Capabilities.Generation;
    begin
        -- The recipient by identity (KERN-003): its generation is part of it.
        Process.resolveIdentity (Process_Identities.From_Word (arg0), targetPID, expectedGeneration);
        if targetPID = Process.NO_PROCESS or else
           (boundRecipient and then arg4 > 31)
        then
            println ("POLICY_MINT_CAPABILITY: invalid target identity");
            retval := reterr;
            return;
        end if;

        declare
            procedure performLocked is
            begin
                -- Recipient generation is checked under the same mailbox
                -- lock as installation. Root CSPACE authority cannot bypass
                -- a stale recipient identity.
                if expectedGeneration /= Process.generationOf (targetPID)
                then
                    retval := reterr;
                    return;
                end if;
                if not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime) then
                    retval := reterr;
                    return;
                end if;

                if not hasCspaceGrantFor (callerPID, targetPID)
                then
                    println
                      ("POLICY_MINT_CAPABILITY: denied, no capability-space grant");
                    retval := reterr;
                    return;
                elsif arg5 >
                      Unsigned_64 (Capabilities.CapabilitySlot'Last)
                then
                    println ("POLICY_MINT_CAPABILITY: invalid slot");
                    retval := reterr;
                    return;
                elsif arg1 >
                      Unsigned_64 (Capabilities.CapabilityType'Pos (
                          Capabilities.CapabilityType'Last))
                then
                    println ("POLICY_MINT_CAPABILITY: invalid capability type");
                    retval := reterr;
                    return;
                elsif not Capabilities.isPolicyMintable
                  (Capabilities.CapabilityType'Val (Natural (arg1)))
                then
                    println
                      ("POLICY_MINT_CAPABILITY: capability type cannot be minted");
                    retval := reterr;
                    return;
                end if;

                targetSlot := Capabilities.CapabilitySlot (arg5);
                if boundRecipient and then
                   Process.proctab(targetPID).caps(targetSlot).capType /=
                     Capabilities.CAP_NULL
                then
                    retval := reterr;
                    return;
                end if;
                capTypePos := Natural (arg1);

                -- Build rights from bitmask
                newRights := (
                    Capabilities.RIGHT_READ    => (arg4 and 1) /= 0,
                    Capabilities.RIGHT_WRITE   => (arg4 and 2) /= 0,
                    Capabilities.RIGHT_EXECUTE => (arg4 and 4) /= 0,
                    Capabilities.RIGHT_GRANT   => (arg4 and 8) /= 0,
                    Capabilities.RIGHT_REVOKE  => (arg4 and 16) /= 0);

                -- Process-referencing capabilities are generation-bound to the
                -- referenced object, not to the process receiving the capability.
                -- Reject nonexistent references so a capability cannot spring into
                -- validity later when that PID is first allocated or recycled.
                declare
                    use type Capabilities.CapabilityType;
                    capGen : Capabilities.Generation;
                    ct     : constant Capabilities.CapabilityType :=
                        Capabilities.CapabilityType'Val (capTypePos);
                    objectPID : Process.ProcessID := Process.NO_PROCESS;
                    -- The object as stored: a process-referencing
                    -- capability keeps its slot (and generation in gen).
                    objectRef : Unsigned_64 := arg2;
                begin
                    -- arg2 names a process by identity for these types. Zero
                    -- is the wildcard; a stale identity must not become it.
                    if ct in Capabilities.CAP_ENDPOINT | Capabilities.CAP_PROCESS |
                             Capabilities.CAP_CSPACE and then arg2 /= 0
                    then
                        objectPID := Process.processOfIdentity (Process_Identities.From_Word (arg2));
                        if objectPID = Process.NO_PROCESS then
                            println
                              ("POLICY_MINT_CAPABILITY: referenced process not valid");
                            retval := reterr;
                            return;
                        end if;
                        objectRef := Unsigned_64 (objectPID);
                    end if;

                    if ct = Capabilities.CAP_CSPACE and then
                       not canDelegateCspace
                         (callerPID, targetPID, objectRef, newRights)
                    then
                        println
                          ("POLICY_MINT_CAPABILITY: CSPACE delegation would " &
                           "amplify authority");
                        retval := reterr;
                        return;
                    end if;

                    if ct = Capabilities.CAP_ENDPOINT or else
                       ((ct = Capabilities.CAP_PROCESS or else
                         ct = Capabilities.CAP_CSPACE) and then arg2 /= 0)
                    then
                        -- The referenced process by identity; a stale one
                        -- names no one.
                        Process.resolveIdentity (Process_Identities.From_Word (arg2), objectPID, capGen);
                        if objectPID = Process.NO_PROCESS or else
                           Process.threadOf (objectPID).state = Process.INVALID
                        then
                            println
                              ("POLICY_MINT_CAPABILITY: referenced process not valid");
                            retval := reterr;
                            return;
                        end if;
                    else
                        capGen := Capabilities.INITIAL_GENERATION;
                    end if;

                    newCap := (
                        capType  => ct,
                        rights   => newRights,
                        --  Endpoint object.param is its explicit authority
                        --  context when supplied by the capability-space
                        --  policy holder. Zero retains the sender-ID tag.
                        authorityTag =>
                          (if ct = Capabilities.CAP_ENDPOINT and arg3 /= 0
                           then arg3 else Process_Identities.To_Word (Process.identityOf (targetPID))),
                        object   => (ref   => objectRef,
                                     param => arg3),
                        gen      => capGen);
                end;

                Capabilities.Operations.insertCapAt (
                    table => Process.proctab(targetPID).caps,
                    slot  => targetSlot,
                    cap   => newCap);

                retval := 0;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleMintCap;

    ---------------------------------------------------------------------------
    -- handleResume
    ---------------------------------------------------------------------------
    procedure handleDelegateEndpoint
      (callerPID : Process.ProcessID;
       recipient, sourceSlot, destinationSlot, rights, tag, reserved : Unsigned_64;
       retval : out Unsigned_64) with SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        -- The recipient by identity (KERN-003): slot and generation.
        generation : Capabilities.Generation;
        targetPID, firstPID, secondPID : Process.ProcessID;
        procedure performLocked is
            parent : constant Capabilities.Capability :=
              Process.proctab(callerPID).caps(Capabilities.CapabilitySlot(sourceSlot));
            requested : constant Capabilities.CapabilityRights :=
              (Capabilities.RIGHT_READ => (rights and 1) /= 0,
               Capabilities.RIGHT_WRITE => (rights and 2) /= 0,
               Capabilities.RIGHT_EXECUTE => (rights and 4) /= 0,
               Capabilities.RIGHT_GRANT => (rights and 8) /= 0,
               Capabilities.RIGHT_REVOKE => (rights and 16) /= 0);
        begin
            if not Process.proctab(targetPID).admitted or else
               Process_Lifetime.Closing (Process.threadOf(targetPID).lifetime) or else
               generation /= Process.generationOf(targetPID) or else
               not hasCspaceGrantFor(callerPID, targetPID) or else
               parent.capType /= Capabilities.CAP_ENDPOINT or else
               not parent.rights(Capabilities.RIGHT_GRANT) or else
               parent.gen = 0 or else parent.object.ref = 0 or else
               not Capabilities.isSubsetOf(requested, parent.rights) or else
               Process.proctab(targetPID).caps
                 (Capabilities.CapabilitySlot(destinationSlot)).capType /=
                   Capabilities.CAP_NULL
            then
                return;
            end if;
            -- Preserve the exact source object and generation. Never resolve
            -- its PID again: a stale endpoint remains stale, not redirected.
            Capabilities.Operations.insertCapAt
              (Process.proctab(targetPID).caps,
               Capabilities.CapabilitySlot(destinationSlot),
               Capabilities.mint(parent, tag, requested));
            retval := 0;
        end performLocked;
    begin
        retval := reterr;
        Process.resolveIdentity (Process_Identities.From_Word (recipient), targetPID, generation);
        if targetPID = Process.NO_PROCESS or else
           reserved /= 0 or else rights > 31 or else
           sourceSlot > Unsigned_64(Capabilities.CapabilitySlot'Last) or else
           destinationSlot > Unsigned_64(Capabilities.CapabilitySlot'Last)
        then
            return;
        end if;
        firstPID := Process.ProcessID'Min(callerPID, targetPID);
        secondPID := Process.ProcessID'Max(callerPID, targetPID);
        -- Follow the IPC two-mailbox ascending-PID order. Source snapshot,
        -- CSPACE authorization and destination installation are one operation.
        Spinlocks.enterCriticalSection(Process.mailtab(firstPID).lock);
        if secondPID /= firstPID then
            Spinlocks.enterCriticalSection(Process.mailtab(secondPID).lock);
        end if;
        performLocked;
        if secondPID /= firstPID then
            Spinlocks.exitCriticalSection(Process.mailtab(secondPID).lock);
        end if;
        Spinlocks.exitCriticalSection(Process.mailtab(firstPID).lock);
    end handleDelegateEndpoint;

    procedure handleResume (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
    begin
        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg0));
        if targetPID = Process.NO_PROCESS then
            println ("RESUME: invalid target identity");
            retval := reterr;
            return;
        end if;

        declare
            procedure performLocked is
                candidateQuota : Process.ResourceQuota := Process.proctab(targetPID).quota;
                quotaAccepted : Boolean;
            begin
                if not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime) then
                    retval := reterr;
                    return;
                end if;

                if not hasCapProcessFor (callerPID, targetPID,
                                         Capabilities.RIGHT_EXECUTE)
                then
                    println ("RESUME: denied, no RIGHT_EXECUTE");
                    retval := reterr;
                elsif Process.threadOf (targetPID).state /= Process.SUSPENDED then
                    println ("RESUME: target not suspended");
                    retval := reterr;
                else
                    -- Scan child's cap table for CAP_RESOURCE and populate quota
                    for slot in Capabilities.CapabilitySlot loop
                        if Process.proctab(targetPID).caps(slot).capType =
                           Capabilities.CAP_RESOURCE
                        then
                            declare
                                cap : Capabilities.Capability renames
                                    Process.proctab(targetPID).caps(slot);
                                q   : Process.ResourceQuota renames
                                    candidateQuota;
                            begin
                                q.maxFrames := cap.object.ref;
                                q.cpuQuotaUs :=
                                    Unsigned_32 (cap.object.param and 16#FFFF_FFFF#);
                                q.cpuPeriodUs :=
                                    Unsigned_32 (Shift_Right (cap.object.param, 32));
                                q.cpuUsedTicks    := 0;
                                q.periodStartTick := Time.msTicks;
                            end;
                            exit;
                        end if;
                    end loop;

                    -- Launch arguments can no longer be installed.
                    Memory_Accounting.Adopt (Process.proctab(targetPID).memoryAccount,
                      candidateQuota.maxFrames, quotaAccepted);
                    if not quotaAccepted then
                        println ("RESUME: memory quota below existing charge");
                        retval := reterr;
                        return;
                    end if;
                    Process.proctab(targetPID).quota := candidateQuota;
                    Process.proctab(targetPID).launch := Process_Launch.Started;
                    Process.resume (targetPID);
                    print ("RESUME: resumed PID ");
                    println (Integer (targetPID));
                    retval := 0;
                end if;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleResume;

    ---------------------------------------------------------------------------
    -- handleInstallLaunchArguments
    ---------------------------------------------------------------------------
    procedure handleInstallLaunchArguments
       (callerPID        : Process.ProcessID;
        arg0, arg1, arg2 : Unsigned_64;
        retval           : out Unsigned_64) with
        SPARK_Mode => Off   -- page-table and process-table integration
    is
        use type Process.ProcessState;
        use type Process.Page_Allocation_Result;
        use type Process_Launch.Launch_Phase;
        targetPID : Process.ProcessID;
    begin
        retval := reterr;
        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg0));
        if targetPID = Process.NO_PROCESS then
            println ("LAUNCH-ARGS: invalid target identity");
            return;
        elsif arg2 not in Process_Launch.Argument_Bytes or else
              not User_Buffer_Copy.Valid_Range (arg1, arg2)
        then
            println ("LAUNCH-ARGS: invalid source range");
            return;
        end if;

        declare
            procedure performLocked is
                pages : constant Process_Launch.Argument_Pages :=
                    Process_Launch.Page_Count (arg2);
                frames : Process.FrameLists.List renames
                    Process.proctab(targetPID).frames;
                storage : System.Address;
                result : Process.Page_Allocation_Result;
                copied : Boolean;
                offset, count : Unsigned_64;
            begin
                if not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime)
                then
                    return;
                elsif not hasCapProcessFor (callerPID, targetPID,
                                            Capabilities.RIGHT_EXECUTE)
                then
                    println ("LAUNCH-ARGS: denied, no RIGHT_EXECUTE");
                    return;
                elsif Process.threadOf (targetPID).state /= Process.SUSPENDED or else
                      Process.proctab(targetPID).launch /= Process_Launch.Unstarted
                then
                    println ("LAUNCH-ARGS: target already started or has arguments");
                    return;
                end if;

                Process.lockAddressSpace (targetPID);
                -- The frame list's capacity bounds tracked frames; nodes are
                -- supplied on demand (as for heap growth).
                if frames.capacity > Natural'Last - pages then
                    Process.unlockAddressSpace (targetPID);
                    return;
                end if;
                frames.capacity := frames.capacity + pages;
                for page in 0 .. pages - 1 loop
                    Process.tryAddPage
                      (proc    => Process.proctab(targetPID),
                       mapTo   => To_Address
                                    (Integer_Address (Process_Launch.Arguments_Base) +
                                     Integer_Address (page) *
                                       Process_Launch.Page_Bytes),
                       storage => storage,
                       result  => result,
                       flags   => Virtmem.PG_USERDATARO);
                    if result /= Process.Page_Added then
                        Process.unlockAddressSpace (targetPID);
                        println ("LAUNCH-ARGS: page allocation rejected");
                        return;
                    end if;
                    -- Fresh frames are zeroed; copy this page's share.
                    offset := Unsigned_64 (page) * Process_Launch.Page_Bytes;
                    count := Unsigned_64'Min (arg2 - offset,
                                              Process_Launch.Page_Bytes);
                    Process.User_Memory.Copy
                      (callerPID, arg1 + offset, storage,
                       Storage_Count (count), copied);
                    if not copied then
                        Process.unlockAddressSpace (targetPID);
                        println ("LAUNCH-ARGS: source unreadable");
                        return;
                    end if;
                end loop;
                Process.unlockAddressSpace (targetPID);

                -- First dispatch enters user mode with RDI = length.
                Process.threadOf (targetPID).kernelStack.interruptFrame.rdi := arg2;
                Process.proctab(targetPID).launch :=
                    Process_Launch.Arguments_Installed;
                retval := 0;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleInstallLaunchArguments;

    ---------------------------------------------------------------------------
    -- handleKill
    ---------------------------------------------------------------------------
    procedure handleKill (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Process.ProcessState;

        targetPID : Process.ProcessID;
        generation : Capabilities.Generation;
    begin
        Process.resolveIdentity (Process_Identities.From_Word (arg0), targetPID, generation);
        if targetPID = Process.NO_PROCESS then
            println ("KILL: invalid target identity");
            retval := reterr;
            return;
        end if;

        if Process.threadOf (targetPID).state = Process.INVALID then
            println ("KILL: target not active");
            retval := reterr;
            return;
        end if;

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_WRITE)
        then
            println ("KILL: denied, no RIGHT_WRITE");
            retval := reterr;
            return;
        end if;

        if targetPID = callerPID then
            -- Self-kill: use kill which enters scheduler (never returns)
            Process.kill (targetPID);
            -- unreachable
            retval := 0;
        else
            -- Kill another process: terminate and continue
            if Process.killProcess (targetPID, generation) then
                print ("KILL: stop requested for PID ");
                println (Integer (targetPID));
                retval := 0;
            else
                retval := reterr;
            end if;
        end if;
    end handleKill;

    procedure handleSendControl (callerPID : Process.ProcessID;
                                 arg0, arg1 : Unsigned_64;
                                 retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Process.ProcessState;
        use type Kernel_Controls.Send_Result;
        targetPID : Process.ProcessID;
        kind : IPC_Labels.Control_Kind := IPC_Labels.Control_Kind'First;
        known : Boolean := False;
        result : Kernel_Controls.Send_Result;
    begin
        retval := reterr;
        for candidate in IPC_Labels.Control_Kind loop
            if arg1 = IPC_Labels.Control_Kind'Enum_Rep (candidate) then
                kind := candidate;
                known := True;
            end if;
        end loop;
        if not known then
            return;
        end if;
        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg0));
        if targetPID = Process.NO_PROCESS
          or else Process.threadOf (targetPID).state = Process.INVALID
        then
            return;
        end if;
        -- Its launcher (the kernel's parent, this incarnation of it), or a
        -- holder of the process capability kill needs.
        if not ((Process.proctab (targetPID).ppid = callerPID
                 and then Process.proctab (targetPID).parentGeneration =
                            Process.generationOf (callerPID))
                or else hasCapProcessFor (callerPID, targetPID, Capabilities.RIGHT_WRITE))
        then
            return;
        end if;
        -- Kept until the target reads it (docs/ipc-delivery.md); busy when
        -- its sender slots hold others' unread messages: try again later.
        Process.IPC.sendControl
          (targetPID, Process.generationOf (targetPID), callerPID, kind, result);
        if result = Kernel_Controls.Accepted then
            retval := 0;
        elsif result = Kernel_Controls.Busy then
            retval := retbusy;
        end if;
    end handleSendControl;

    ---------------------------------------------------------------------------
    -- handleSetWellKnown
    -- arg0 = role (ServiceRole), arg1 = PID to register
    ---------------------------------------------------------------------------
    procedure handleSetWellKnown (callerPID : Process.ProcessID;
                                   arg0, arg1 : Unsigned_64;
                                   retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Process.ProcessState;

        targetPID : Process.ProcessID;
    begin
        if arg0 > Unsigned_64 (Config.ServiceRole'Last) then
            println ("SET_WELL_KNOWN: invalid role");
            retval := reterr;
            return;
        end if;

        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg1));
        if targetPID = Process.NO_PROCESS then
            println ("SET_WELL_KNOWN: invalid identity");
            retval := reterr;
            return;
        end if;

        declare
            procedure performLocked is
            begin
                if not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime) then
                    retval := reterr;
                    return;
                end if;

                if not hasCapProcessFor (callerPID, targetPID,
                                         Capabilities.RIGHT_GRANT)
                then
                    println ("SET_WELL_KNOWN: denied, no RIGHT_GRANT");
                    retval := reterr;
                    return;
                end if;

                Config.wellKnownServices (Config.ServiceRole (arg0)) :=
                    (pid => Natural (targetPID),
                     gen => Process.generationOf (targetPID));

                print ("SET_WELL_KNOWN: role ");
                print (Integer (arg0));
                print (" => PID ");
                println (Integer (targetPID));
                retval := 0;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleSetWellKnown;

    ---------------------------------------------------------------------------
    -- handleEnableIrq
    ---------------------------------------------------------------------------
    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        irqOk  : Boolean;
        targetPID : Process.ProcessID;
    begin
        -- Validate args before cap check (arg1 is the owner PID)
        -- Only vectors with installed device-interrupt stubs may be assigned.
        -- Never turn a missing IDT entry (or an exception/IPI vector) into a
        -- device route, even for a process-management authority holder.
        if arg0 not in Unsigned_64 (InterruptNumbers.PS2KEYBOARD) ..
          Unsigned_64 (InterruptNumbers.DEVICE_MSI_LAST)
        then
            println ("ENABLE_IRQ: unsupported device vector");
            retval := reterr;
            return;
        end if;

        targetPID := Process.processOfIdentity (Process_Identities.From_Word (arg1));
        if targetPID = Process.NO_PROCESS then
            println ("ENABLE_IRQ: invalid owner identity");
            retval := reterr;
            return;
        end if;
        Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
        if not Process.proctab(targetPID).admitted or else
           Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime)
        then
            retval := reterr;
        else
            if not hasCapProcessFor (callerPID,
                                     targetPID,
                                     Capabilities.RIGHT_GRANT)
            then
                println ("ENABLE_IRQ: denied, no RIGHT_GRANT");
                retval := reterr;
            else
                --  The resolved slot: delivery finds owners by slot.
                Capabilities.IRQ.registerIRQ (
                    vector => Natural (arg0),
                    pid    => Unsigned_64 (targetPID),
                    status => irqOk);

                if irqOk then
                    --  MSI/MSI-X sources target an IDT vector directly and must
                    --  not unmask the numerically corresponding IOAPIC input.
                    if (arg2 and 16#400#) = 0 then
                        Interrupts.enableDeviceIRQ (
                            InterruptNumbers.x86Interrupt (arg0),
                            Unsigned_32 (arg2 and 16#FF#),
                            levelTriggered => (arg2 and 16#100#) /= 0,
                            activeLow      => (arg2 and 16#200#) /= 0);
                    end if;
                    retval := 0;
                else
                    println ("ENABLE_IRQ: shared subscriber set full");
                    retval := reterr;
                end if;
            end if;

        end if;
        Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
    end handleEnableIrq;

    ---------------------------------------------------------------------------
    -- handleSetSysinfo
    ---------------------------------------------------------------------------
    procedure handleSetSysinfo (callerPID  : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        --  Who may set what: the wall-clock offset only the registered
        --  clock service; device configuration only the registered device
        --  manager. (Any write-capable CAP_PROCESS used to suffice, and every
        --  process holds one for itself.)
        owner : constant Sysinfo.DriverID :=
            (if arg0 = Sysinfo.WALL_CLOCK_OFFSET then Sysinfo.DRIVER_CLOCK
             else Sysinfo.DRIVER_DEVMGR);
        hasCap : constant Boolean :=
            Process.threadOf (callerPID).mode = Process.KERNEL or else
            Sysinfo.getInfo (Sysinfo.REGISTERED_DRIVER, Unsigned_64 (owner)) =
                Unsigned_64 (callerPID);
    begin
        if not hasCap then
            println ("SET_SYSINFO: denied, caller is not the registered owner");
            retval := reterr;
        elsif Sysinfo.setInfo (arg0, arg1) then
            retval := 0;
        else
            println ("SET_SYSINFO: unknown queryID");
            retval := reterr;
        end if;
    end handleSetSysinfo;

    ---------------------------------------------------------------------------
    -- handleSetCpu
    ---------------------------------------------------------------------------
    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1 : Unsigned_64;
                            retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : constant Process.ProcessID := Process.processOfIdentity (Process_Identities.From_Word (arg0));
    begin
        if targetPID = Process.NO_PROCESS then
            retval := reterr;
            return;
        elsif arg1 >= Unsigned_64 (acpi.numCPUs) then
            println ("SET_CPU: CPU number out of range");
            retval := reterr;
            return;
        end if;

        declare
            procedure performLocked is
            begin
                if not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime) then
                    retval := reterr;
                    return;
                end if;

                if not hasCapProcessFor (callerPID, targetPID,
                                         Capabilities.RIGHT_GRANT)
                then
                    println ("SET_CPU: denied, no RIGHT_GRANT");
                    retval := reterr;
                elsif Process.threadOf (targetPID).state /= Process.SUSPENDED or else
                      Process_Lifetime.Executing (Process.threadOf (targetPID).lifetime) then
                    println ("SET_CPU: target must be stopped and suspended");
                    retval := reterr;
                else
                    --  Explicit placement is a hard affinity: work stealing
                    --  never moves this process.
                    Process.threadOf (targetPID).cpu := Natural (arg1);
                    Process.threadOf (targetPID).pinned := True;
                    retval := 0;
                end if;
            end performLocked;
        begin
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
    end handleSetCpu;

    ---------------------------------------------------------------------------
    -- handleSaveReplyCap
    -- Move the calling thread's CAP_REPLY to the specified process slot.
    -- Used by servers that need to defer replies (e.g. netstack).
    ---------------------------------------------------------------------------
    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   arg0      : Unsigned_64;
                                   retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;

        destSlot : Capabilities.CapabilitySlot;
        moved    : Boolean;
    begin
        -- Validate destination slot
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := 0;
            return;
        end if;

        destSlot := Capabilities.CapabilitySlot(arg0);

        -- Cannot save to slot 63 itself
        if destSlot = Capabilities.REPLY_CAP_SLOT then
            retval := 0;
            return;
        end if;

        -- The same lock serializes authorized cspace edits and inspection.
        -- The reply capability comes from the calling thread.
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        Capabilities.Operations.moveReplyCapFrom
          (source => Process.threadtab (PerCPUData.getCurrentThread).replyCap,
           table  => Process.proctab(callerPID).caps,
           dest   => destSlot,
           moved  => moved);

        if not moved then
            Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
            retval := 0;
            return;
        end if;

        Process.proctab(callerPID).deferredReplyCaps :=
            Process.proctab(callerPID).deferredReplyCaps or
            Shift_Left (Unsigned_64'(1), destSlot);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        retval := 1;
    end handleSaveReplyCap;

end Syscall.Admin;
