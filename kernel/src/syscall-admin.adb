-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall privileged/management handler implementations: capabilities,
-- port I/O, process management, system configuration.
-------------------------------------------------------------------------------
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
with Interrupts;
with PerCpuData;
with Process;
with Process.IPC;
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
                (cap.gen = Process.proctab(targetPID).capGeneration
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
                 cap.gen = Process.proctab(targetPID).capGeneration))
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
                 cap.gen = Process.proctab(targetPID).capGeneration)) and then
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
        if Process.proctab(callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_NOTIFICATION and then
                   Process.proctab(callerPID).caps(slot).object.ref =
                   arg0
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
        if Process.proctab(callerPID).mode = Process.KERNEL then
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
                Process.addrtab(callerPID));
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
                             arg4, arg5 : Unsigned_64;
                             retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is




        sendMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            capBadge => 0,
            words    => (arg2, arg3, arg4, arg5));
        replyTag : Process.MessageTag;
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := reterr;
        else
            replyTag := Process.IPC.capSend (
                capSlot => Capabilities.CapabilitySlot(arg0),
                msg     => sendMsg);
            Process.proctab(callerPID).replyMsg :=
                Process.NULL_MESSAGE;
            retval := tagToU64 (replyTag);
        end if;
    end handleCapSend;

    ---------------------------------------------------------------------------
    -- handleCapCall
    ---------------------------------------------------------------------------
    procedure handleCapCall (callerPID  : Process.ProcessID;
                             arg0, arg1 : Unsigned_64;
                             retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Process.MessageTag;

        userMsg  : Process.Message
            with Import, Address => Util.numToAddr(arg1);
        replyTag : Process.MessageTag;
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := reterr;
        else
            -- Copy from user memory before blocking IPC call.
            -- Context switch during capCall clears EFLAGS.AC (SMAP).
            declare
                localMsg : Process.Message;
            begin
                x86.stac;
                localMsg := userMsg;
                x86.clac;

                replyTag := Process.IPC.capCall (
                    capSlot => Capabilities.CapabilitySlot(arg0),
                    msg     => localMsg);
            end;

            x86.stac;
            userMsg := Process.proctab(callerPID).replyMsg;
            x86.clac;
            Process.proctab(callerPID).replyMsg :=
                Process.NULL_MESSAGE;
            retval := tagToU64 (replyTag);
        end if;
    end handleCapCall;

    ---------------------------------------------------------------------------
    -- handleCapSubmit
    ---------------------------------------------------------------------------
    procedure handleCapSubmit (arg0, arg1, arg2, arg3,
                               arg4, arg5 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is


        submitMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            capBadge => 0,
            words    => (arg2, arg3, arg4, 0));
        ok : Boolean;
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := 0;
        else
            ok := Process.IPC.capSubmit (
                capSlot => Capabilities.CapabilitySlot(arg0),
                msg     => submitMsg,
                token   => arg5);
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
        -- GNAT bug: Import overlay + regular Process.Message in the same
        -- declarative region causes compiler stack overflow. Use a nested
        -- declare block for localMsg/recvMsg to work around this.
        userMsg : Process.Message
            with Import, Address => Util.numToAddr(arg1);
        from    : Process.ProcessID;
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
        else
            -- Copy from user memory before blocking IPC call.
            -- Context switch during replyWait clears EFLAGS.AC (SMAP).
            declare
                localMsg : Process.Message;
                recvMsg  : Process.Message;
            begin
                x86.stac;
                localMsg := userMsg;
                x86.clac;

                Process.IPC.replyWait (
                    replyTo  => Process.ProcessID(arg0),
                    replyMsg => localMsg,
                    from     => from,
                    msg      => recvMsg);

                x86.stac;
                userMsg := recvMsg;
                x86.clac;
            end;
            retval := Unsigned_64(from);
        end if;
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
        bufAddr    : constant System.Address := Util.numToAddr (arg0);
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

        x86.stac;
        for i in Process.proctab'Range loop
            exit when count >= maxEntries;

            if Process.proctab(i).state /= Process.INVALID then
                declare
                    offset : constant Storage_Offset :=
                        Storage_Offset (count * ENTRY_SIZE);
                    entryAddr : constant System.Address :=
                        bufAddr + offset;

                    pidVal : Unsigned_16 with
                        Import, Address => entryAddr;
                    stateVal : Unsigned_8 with
                        Import, Address => entryAddr + 2;
                    cpuVal : Unsigned_8 with
                        Import, Address => entryAddr + 3;
                    priVal : Unsigned_16 with
                        Import, Address => entryAddr + 4;
                    padVal : Unsigned_16 with
                        Import, Address => entryAddr + 6;
                    nameField : String (1 .. 16) with
                        Import, Address => entryAddr + 8;
                    framesVal : Unsigned_32 with
                        Import, Address => entryAddr + 24;
                    reservedVal : Unsigned_32 with
                        Import, Address => entryAddr + 28;
                begin
                    pidVal := Unsigned_16 (
                        Process.proctab(i).pid);
                    stateVal := Process.ProcessState'Pos (
                        Process.proctab(i).state);
                    cpuVal := Unsigned_8 (
                        Process.proctab(i).cpu);
                    priVal := priToU16 (Integer_16 (
                        Process.proctab(i).priority));
                    padVal := 0;
                    nameField :=
                        Process.proctab(i).name;
                    framesVal := Unsigned_32 (
                        Process.proctab(i).frames.length);
                    reservedVal := 0;
                    count := count + 1;
                end;
            end if;
        end loop;
        x86.clac;

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
        outAddr   : constant System.Address := Util.numToAddr (arg2);
        typeVal   : Unsigned_64 with Import, Address => outAddr;
        rightsVal : Unsigned_64 with Import, Address => outAddr + 8;
        badgeVal  : Unsigned_64 with Import, Address => outAddr + 16;
        refVal    : Unsigned_64 with Import, Address => outAddr + 24;
        paramVal  : Unsigned_64 with Import, Address => outAddr + 32;
        genVal    : Unsigned_64 with Import, Address => outAddr + 40;
        rights    : Unsigned_64 := 0;
    begin
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or else
           arg0 = 0 or else
           arg1 > Unsigned_64 (Capabilities.CapabilitySlot'Last) or else
           arg2 = 0
        then
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);
        slot := Capabilities.CapabilitySlot (arg1);

        if Process.proctab(targetPID).state = Process.INVALID then
            retval := reterr;
            return;
        elsif not hasCapProcessFor (callerPID, targetPID,
                                    Capabilities.RIGHT_READ)
        then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                Unsigned_64 (SyscallNumber'Enum_Rep (
                    SYSCALL_INSPECT_CAP)),
                arg0, arg1);
            retval := reterr;
            return;
        end if;

        cap := Process.proctab(targetPID).caps(slot);
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

        x86.stac;
        typeVal := Unsigned_64 (Capabilities.CapabilityType'Pos (cap.capType));
        rightsVal := rights;
        badgeVal := cap.capBadge;
        refVal := cap.object.ref;
        paramVal := cap.object.param;
        genVal := Unsigned_64 (cap.gen);
        x86.clac;

        retval := 1;
    end handleInspectCap;

    ---------------------------------------------------------------------------
    -- handleMintCap
    ---------------------------------------------------------------------------
    procedure handleMintCap (callerPID : Process.ProcessID;
                             arg0, arg1, arg2, arg3,
                             arg4, arg5 : Unsigned_64;
                             retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID  : Process.ProcessID;
        capTypePos : Natural;
        newCap     : Capabilities.Capability;
        newRights  : Capabilities.CapabilityRights;
        targetSlot : Capabilities.CapabilitySlot;
    begin
        -- Validate target PID range
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or
           arg0 = 0
        then
            println ("MINT_CAP: invalid target PID");
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);

        if not hasCspaceGrantFor (callerPID, targetPID)
        then
            println ("MINT_CAP: denied, no capability-space grant");
            retval := reterr;
            return;
        elsif Process.proctab(targetPID).state = Process.INVALID then
            println ("MINT_CAP: target not valid");
            retval := reterr;
            return;
        elsif arg5 >
              Unsigned_64 (Capabilities.CapabilitySlot'Last)
        then
            println ("MINT_CAP: invalid slot");
            retval := reterr;
            return;
        elsif arg1 >
              Unsigned_64 (Capabilities.CapabilityType'Pos (
                  Capabilities.CapabilityType'Last))
        then
            println ("MINT_CAP: invalid cap type");
            retval := reterr;
            return;
        elsif not Capabilities.isOrdinarilyDerivable
          (Capabilities.CapabilityType'Val (Natural (arg1)))
        then
            println ("MINT_CAP: capability type cannot be minted");
            retval := reterr;
            return;
        end if;

        targetSlot := Capabilities.CapabilitySlot (arg5);
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
            objectPID : Process.ProcessID;
        begin
            if ct = Capabilities.CAP_CSPACE and then
               not canDelegateCspace
                 (callerPID, targetPID, arg2, newRights)
            then
                println ("MINT_CAP: CSPACE delegation would amplify authority");
                retval := reterr;
                return;
            end if;

            if ct = Capabilities.CAP_ENDPOINT or else
               ((ct = Capabilities.CAP_PROCESS or else
                 ct = Capabilities.CAP_CSPACE) and then arg2 /= 0)
            then
                if arg2 > Unsigned_64 (Process.ProcessID'Last) or else
                   arg2 = 0
                then
                    println ("MINT_CAP: invalid referenced PID");
                    retval := reterr;
                    return;
                end if;

                objectPID := Process.ProcessID (arg2);
                if Process.proctab(objectPID).state = Process.INVALID then
                    println ("MINT_CAP: referenced process not valid");
                    retval := reterr;
                    return;
                end if;
                capGen := Process.proctab(objectPID).capGeneration;
            else
                capGen := Capabilities.INITIAL_GENERATION;
            end if;

            newCap := (
                capType  => ct,
                rights   => newRights,
                capBadge => Unsigned_64 (targetPID),
                object   => (ref   => arg2,
                             param => arg3),
                gen      => capGen);
        end;

        Capabilities.Operations.insertCapAt (
            table => Process.proctab(targetPID).caps,
            slot  => targetSlot,
            cap   => newCap);

        retval := 0;
    end handleMintCap;

    ---------------------------------------------------------------------------
    -- handleResume
    ---------------------------------------------------------------------------
    procedure handleResume (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
    begin
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or
           arg0 = 0
        then
            println ("RESUME: invalid target PID");
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_EXECUTE)
        then
            println ("RESUME: denied, no RIGHT_EXECUTE");
            retval := reterr;
        elsif Process.proctab(targetPID).state /= Process.SUSPENDED then
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
                            Process.proctab(targetPID).quota;
                    begin
                        q.maxFrames :=
                            Natural (cap.object.ref);
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

            Process.resume (targetPID);
            print ("RESUME: resumed PID ");
            println (Integer (targetPID));
            retval := 0;
        end if;
    end handleResume;

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
    begin
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or
           arg0 = 0
        then
            println ("KILL: invalid target PID");
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);

        if Process.proctab(targetPID).state = Process.INVALID then
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
            Process.killProcess (targetPID);
            print ("KILL: terminated PID ");
            println (Integer (targetPID));
            retval := 0;
        end if;
    end handleKill;

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

        if arg1 > Unsigned_64 (Process.ProcessID'Last) or
           arg1 = 0
        then
            println ("SET_WELL_KNOWN: invalid PID");
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg1);

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_GRANT)
        then
            println ("SET_WELL_KNOWN: denied, no RIGHT_GRANT");
            retval := reterr;
            return;
        end if;

        Config.wellKnownServices (Config.ServiceRole (arg0)) :=
            (pid => Natural (targetPID),
             gen => Process.proctab(targetPID).capGeneration);

        print ("SET_WELL_KNOWN: role ");
        print (Integer (arg0));
        print (" => PID ");
        println (Integer (targetPID));
        retval := 0;
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
    begin
        -- Validate args before cap check (arg1 is the owner PID)
        if arg0 > 255 then
            println ("ENABLE_IRQ: invalid vector");
            retval := reterr;
            return;
        elsif arg1 > Unsigned_64 (Process.ProcessID'Last) or
              arg1 = 0
        then
            println ("ENABLE_IRQ: invalid owner PID");
            retval := reterr;
            return;
        end if;

        if not hasCapProcessFor (callerPID,
                                 Process.ProcessID (arg1),
                                 Capabilities.RIGHT_GRANT)
        then
            println ("ENABLE_IRQ: denied, no RIGHT_GRANT");
            retval := reterr;
        else
            Capabilities.IRQ.registerIRQ (
                vector => Natural (arg0),
                pid    => arg1,
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


        hasCap : Boolean := False;
    begin
        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_WRITE)
            then
                hasCap := True;
                exit;
            end if;
        end loop;

        if not hasCap then
            println ("SET_SYSINFO: denied, no RIGHT_WRITE");
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


        targetPID : Process.ProcessID;
    begin
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or
           arg0 = 0
        then
            retval := reterr;
            return;
        elsif arg1 >= Unsigned_64 (acpi.numCPUs) then
            println ("SET_CPU: CPU number out of range");
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_GRANT)
        then
            println ("SET_CPU: denied, no RIGHT_GRANT");
            retval := reterr;
        elsif Process.proctab(targetPID).state = Process.INVALID then
            println ("SET_CPU: target not valid");
            retval := reterr;
        else
            Process.proctab(targetPID).cpu := Natural (arg1);
            retval := 0;
        end if;
    end handleSetCpu;

    ---------------------------------------------------------------------------
    -- handleSaveReplyCap
    -- Move CAP_REPLY from slot 63 to the specified destination slot.
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

        Capabilities.Operations.moveReplyCap
          (table => Process.proctab(callerPID).caps,
           dest  => destSlot,
           moved => moved);

        if not moved then
            retval := 0;
            return;
        end if;

        Process.proctab(callerPID).deferredReplyCaps :=
            Process.proctab(callerPID).deferredReplyCaps or
            Shift_Left (Unsigned_64'(1), destSlot);
        retval := 1;
    end handleSaveReplyCap;

end Syscall.Admin;
