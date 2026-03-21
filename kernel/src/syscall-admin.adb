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
    function u64ToRights is new Ada.Unchecked_Conversion
        (Unsigned_8, Capabilities.CapabilityRights);
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
                SYSCALL_REGISTER_DRIVER,
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
                            arg0, arg1, arg2 : Unsigned_64;
                            retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is

        capAllowed : Boolean;
    begin
        case syscallNum is
            when SYSCALL_INP8 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 1, False, capAllowed);
                if not capAllowed then
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
                    retval := reterr;
                else
                    x86.out16 (x86.IOPort(arg0 and 16#FFFF#),
                               Unsigned_16(arg1 and 16#FFFF#));
                    retval := 0;
                end if;

            when SYSCALL_INPS16 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, Unsigned_64(arg2) * 2, False,
                    capAllowed);
                if not capAllowed then
                    retval := reterr;
                else
                    x86.stac;
                    x86.ins16 (x86.IOPort(arg0 and 16#FFFF#),
                               Util.numToAddr(arg1),
                               Unsigned_32(arg2));
                    x86.clac;
                    retval := 0;
                end if;

            when SYSCALL_OUTPS16 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, Unsigned_64(arg2) * 2, True,
                    capAllowed);
                if not capAllowed then
                    retval := reterr;
                else
                    x86.stac;
                    x86.outs16 (x86.IOPort(arg0 and 16#FFFF#),
                                Util.numToAddr(arg1),
                                Unsigned_32(arg2));
                    x86.clac;
                    retval := 0;
                end if;

            when SYSCALL_INP32 =>
                Capabilities.Operations.checkPortAccess (
                    Process.proctab(callerPID).caps,
                    arg0 and 16#FFFF#, 4, False, capAllowed);
                if not capAllowed then
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
                SYSCALL_VIRT_TO_PHYS,
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
    -- handleControlAccess
    ---------------------------------------------------------------------------
    procedure handleControlAccess (callerPID : Process.ProcessID;
                                   arg0, arg1, arg2, arg3,
                                   arg4 : Unsigned_64;
                                   retval : out Unsigned_64) with
        SPARK_Mode => Off
    is

        subOp    : constant Unsigned_64 := arg0;
        opStatus : Capabilities.Operations.OperationStatus;
        slot     : Capabilities.CapabilitySlot;
    begin
        case subOp is
            -- INSERT disabled: capability bypass vulnerability.
            when 1 =>
                println ("CONTROLACCESS_INSERT: denied (removed)");
                retval := reterr;

            -- DERIVE: arg1=source_slot, arg2=new_rights_bitmask,
            --         arg3=dest_slot (0=auto)
            when CONTROLACCESS_DERIVE =>
                declare
                    use type Capabilities.CapabilityType;
                    srcCap    : Capabilities.Capability;
                    newCap    : Capabilities.Capability;
                    newRights : Capabilities.CapabilityRights;
                begin
                    if arg1 >
                       Unsigned_64(Capabilities.CapabilitySlot'Last)
                    then
                        retval := reterr;
                    else
                        Capabilities.Operations.lookupCap (
                            table  => Process.proctab(callerPID).caps,
                            slot   =>
                                Capabilities.CapabilitySlot(arg1),
                            cap    => srcCap,
                            status => opStatus);

                        if opStatus /=
                           Capabilities.Operations.OP_OK
                        then
                            retval := reterr;
                        elsif srcCap.capType = Capabilities.CAP_REPLY
                        then
                            -- CAP_REPLY cannot be derived
                            retval := reterr;
                        else
                            newRights := u64ToRights(
                                Unsigned_8(arg2 and 16#FF#));

                            if not Capabilities.isSubsetOf (
                                newRights, srcCap.rights)
                            then
                                retval := reterr;
                            else
                                newCap := Capabilities.derive (
                                    srcCap, newRights);

                                if arg3 /= 0 and then
                                   arg3 <= Unsigned_64(
                                       Capabilities.CapabilitySlot'Last)
                                then
                                    Capabilities.Operations.insertCapAt (
                                        table =>
                                            Process.proctab(
                                                callerPID).caps,
                                        slot  =>
                                            Capabilities.CapabilitySlot(
                                                arg3),
                                        cap   => newCap);
                                    retval := arg3;
                                else
                                    Capabilities.Operations.insertCap (
                                        table  =>
                                            Process.proctab(
                                                callerPID).caps,
                                        cap    => newCap,
                                        slot   => slot,
                                        status => opStatus);
                                    if opStatus =
                                       Capabilities.Operations.OP_OK
                                    then
                                        retval := Unsigned_64(slot);
                                    else
                                        retval := reterr;
                                    end if;
                                end if;
                            end if;
                        end if;
                    end if;
                end;

            -- MINT: arg1=source_slot, arg2=new_badge,
            --       arg3=rights_bitmask, arg4=dest_slot (0=auto)
            when CONTROLACCESS_MINT =>
                declare
                    use type Capabilities.CapabilityType;
                    srcCap    : Capabilities.Capability;
                    newCap    : Capabilities.Capability;
                    newRights : Capabilities.CapabilityRights;
                begin
                    if arg1 >
                       Unsigned_64(Capabilities.CapabilitySlot'Last)
                    then
                        retval := reterr;
                    else
                        Capabilities.Operations.lookupCap (
                            table  => Process.proctab(callerPID).caps,
                            slot   =>
                                Capabilities.CapabilitySlot(arg1),
                            cap    => srcCap,
                            status => opStatus);

                        if opStatus /=
                           Capabilities.Operations.OP_OK
                        then
                            retval := reterr;
                        elsif srcCap.capType = Capabilities.CAP_REPLY
                        then
                            -- CAP_REPLY cannot be minted
                            retval := reterr;
                        else
                            newRights := u64ToRights(
                                Unsigned_8(arg3 and 16#FF#));

                            if not Capabilities.isSubsetOf (
                                newRights, srcCap.rights)
                            then
                                retval := reterr;
                            else
                                newCap := Capabilities.mint (
                                    srcCap, arg2, newRights);

                                if arg4 /= 0 and then
                                   arg4 <= Unsigned_64(
                                       Capabilities.CapabilitySlot'Last)
                                then
                                    Capabilities.Operations.insertCapAt (
                                        table =>
                                            Process.proctab(
                                                callerPID).caps,
                                        slot  =>
                                            Capabilities.CapabilitySlot(
                                                arg4),
                                        cap   => newCap);
                                    retval := arg4;
                                else
                                    Capabilities.Operations.insertCap (
                                        table  =>
                                            Process.proctab(
                                                callerPID).caps,
                                        cap    => newCap,
                                        slot   => slot,
                                        status => opStatus);
                                    if opStatus =
                                       Capabilities.Operations.OP_OK
                                    then
                                        retval := Unsigned_64(slot);
                                    else
                                        retval := reterr;
                                    end if;
                                end if;
                            end if;
                        end if;
                    end if;
                end;

            -- REMOVE: arg1=slot
            when CONTROLACCESS_REMOVE =>
                if arg1 >
                   Unsigned_64(Capabilities.CapabilitySlot'Last)
                then
                    retval := reterr;
                else
                    Capabilities.Operations.removeCap (
                        table  => Process.proctab(callerPID).caps,
                        slot   => Capabilities.CapabilitySlot(arg1),
                        status => opStatus);
                    if opStatus = Capabilities.Operations.OP_OK then
                        retval := Unsigned_64(arg1);
                    else
                        retval := reterr;
                    end if;
                end if;

            -- REVOKE: arg1=slot (nullify capability)
            when CONTROLACCESS_REVOKE =>
                if arg1 >
                   Unsigned_64(Capabilities.CapabilitySlot'Last)
                then
                    retval := reterr;
                else
                    Capabilities.Operations.removeCap (
                        table  => Process.proctab(callerPID).caps,
                        slot   => Capabilities.CapabilitySlot(arg1),
                        status => opStatus);
                    if opStatus = Capabilities.Operations.OP_OK then
                        retval := Unsigned_64(arg1);
                    else
                        retval := reterr;
                    end if;
                end if;

            -- REVOKE_ALL: bump generation counter (requires CAP_PROCESS/REVOKE)
            when CONTROLACCESS_REVOKE_ALL =>
                declare
                    use type Capabilities.CapabilityType;
                    hasRevoke : Boolean := False;
                begin
                    for s in Capabilities.CapabilitySlot loop
                        if Process.proctab(callerPID).caps(s).capType =
                           Capabilities.CAP_PROCESS and then
                           Process.proctab(callerPID).caps(s).rights(
                               Capabilities.RIGHT_REVOKE)
                        then
                            hasRevoke := True;
                            exit;
                        end if;
                    end loop;

                    if not hasRevoke then
                        println ("REVOKE_ALL: denied, no CAP_PROCESS/REVOKE");
                        retval := reterr;
                    elsif Process.proctab(callerPID).capGeneration <
                          Capabilities.Generation'Last
                    then
                        Process.proctab(callerPID).capGeneration :=
                            Process.proctab(callerPID).capGeneration + 1;
                        retval := Unsigned_64(
                            Process.proctab(callerPID).capGeneration);
                    else
                        retval := reterr;
                    end if;
                end;

            when others =>
                retval := reterr;
        end case;
    end handleControlAccess;

    ---------------------------------------------------------------------------
    -- handleGetTicket
    ---------------------------------------------------------------------------
    procedure handleGetTicket (callerPID  : Process.ProcessID;
                               arg0, arg1 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        cap      : Capabilities.Capability;
        opStatus : Capabilities.Operations.OperationStatus;
        userCap  : Capabilities.Capability with
            Import, Address => Util.numToAddr(arg1);
    begin
        if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
            retval := 0;
        else
            Capabilities.Operations.lookupCap (
                table  => Process.proctab(callerPID).caps,
                slot   => Capabilities.CapabilitySlot(arg0),
                cap    => cap,
                status => opStatus);

            if opStatus = Capabilities.Operations.OP_OK then
                cap.gen := 0;
                x86.stac;
                userCap := cap;
                x86.clac;
                retval := 1;
            else
                retval := 0;
            end if;
        end if;
    end handleGetTicket;

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
                SYSCALL_PROCLIST,
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

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_GRANT)
        then
            println ("MINT_CAP: denied, no RIGHT_GRANT");
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
        elsif arg1 = Unsigned_64 (Capabilities.CapabilityType'Pos (
                  Capabilities.CAP_REPLY))
        then
            println ("MINT_CAP: CAP_REPLY cannot be minted");
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

        -- For endpoint caps, gen must match the destination process's
        -- capGeneration (arg2 = destPID), not the holder's. The gen
        -- check in capCall/capSend compares cap.gen against the
        -- destination's capGeneration to detect stale references.
        declare
            use type Capabilities.CapabilityType;
            capGen : Capabilities.Generation;
            ct     : constant Capabilities.CapabilityType :=
                Capabilities.CapabilityType'Val (capTypePos);
        begin
            if ct = Capabilities.CAP_ENDPOINT and then
               arg2 <= Unsigned_64(Process.ProcessID'Last) and then
               arg2 > 0
            then
                capGen := Process.proctab(
                    Process.ProcessID(arg2)).capGeneration;
            else
                capGen := Process.proctab(targetPID).capGeneration;
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
            Interrupts.enableDeviceIRQ (
                InterruptNumbers.x86Interrupt (arg0),
                Unsigned_32 (arg2));

            Capabilities.IRQ.registerIRQ (
                vector => Natural (arg0),
                pid    => arg1,
                status => irqOk);

            if irqOk then
                retval := 0;
            else
                println ("ENABLE_IRQ: IRQ already owned");
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
    -- handleSetSupervisor
    ---------------------------------------------------------------------------
    procedure handleSetSupervisor (callerPID  : Process.ProcessID;
                                   arg0, arg1 : Unsigned_64;
                                   retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
        newSvPID  : Process.ProcessID;
    begin
        if arg0 > Unsigned_64 (Process.ProcessID'Last) or
           arg0 = 0
        then
            retval := reterr;
            return;
        elsif arg1 > Unsigned_64 (Process.ProcessID'Last) then
            retval := reterr;
            return;
        end if;

        targetPID := Process.ProcessID (arg0);
        newSvPID  := Process.ProcessID (arg1);

        if not hasCapProcessFor (callerPID, targetPID,
                                 Capabilities.RIGHT_GRANT)
        then
            retval := reterr;
        elsif Process.proctab(targetPID).state = Process.INVALID then
            println ("SET_SUPERVISOR: target invalid");
            retval := reterr;
        else
            Process.proctab(targetPID).svpid := newSvPID;
            retval := 0;
        end if;
    end handleSetSupervisor;
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
        cap      : Capabilities.Capability;
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

        -- Slot 63 must actually hold a CAP_REPLY
        cap := Process.proctab(callerPID).caps(
            Capabilities.REPLY_CAP_SLOT);
        if cap.capType /= Capabilities.CAP_REPLY then
            retval := 0;
            return;
        end if;

        -- Move: copy to dest, clear slot 63, set bitmap bit
        Process.proctab(callerPID).caps(destSlot) := cap;
        Process.proctab(callerPID).caps(Capabilities.REPLY_CAP_SLOT) :=
            Capabilities.NULL_CAPABILITY;
        Process.proctab(callerPID).deferredReplyCaps :=
            Process.proctab(callerPID).deferredReplyCaps or
            Shift_Left (Unsigned_64'(1), destSlot);
        retval := 1;
    end handleSaveReplyCap;

end Syscall.Admin;
