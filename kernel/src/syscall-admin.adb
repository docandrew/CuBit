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
with Boot_Timeline;
with Boot_Timeline_Layout;
with Capabilities;
with Capabilities.IRQ;
with Config;
with Device_Interrupts;
with Device_Memory_Diagnostics;
with Device_Vector_ABI;
with Capabilities.Operations;
with Capability_ABI;
with Capability_Install;
with Process_Capabilities;
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
with Boot_Output;
with Publication_Pages;
with Service_Registry;
with System_Publications;
with TextIO; use TextIO;
with Time;
with Util;
with Virtmem;
with x86;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.Admin is
    use type Process_Capabilities.Tables.Slot_Base;

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
    -- findOwn
    -- The capability a handle names in the caller's own table, if it is of
    -- Kind and holds Rights (CAP-002: authority is always named by handle;
    -- nothing searches a table for "a capability of this type"). A snapshot
    -- taken under the caller's mailbox lock, which serializes edits of its
    -- table; objects are generation-checked by the caller as before.
    ---------------------------------------------------------------------------
    function findOwn (callerPID : Process.ProcessID;
                      handle    : Unsigned_64;
                      kind      : Capabilities.CapabilityType;
                      rights    : Capabilities.CapabilityRights)
                      return Process_Capabilities.Found
    is
        found : Process_Capabilities.Found;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find
          (Process.proctab(callerPID).caps, handle, kind, rights);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        return found;
    end findOwn;

    function onlyRight (right : Capabilities.CapabilityRight)
                        return Capabilities.CapabilityRights
    is
        rights : Capabilities.CapabilityRights := Capabilities.NO_RIGHTS;
    begin
        rights (right) := True;
        return rights;
    end onlyRight;

    -- A notification capability names service roles ref .. ref + param - 1.
    function coversRole (cap : Capabilities.Capability; role : Unsigned_64)
                         return Boolean is
        (cap.object.param > 0 and then role >= cap.object.ref
         and then role - cap.object.ref < cap.object.param);

    ---------------------------------------------------------------------------
    -- holdsProcessAuthority
    -- The caller's handle names a CAP_PROCESS with Right over targetPID: one
    -- naming that process (in its current incarnation), or the wildcard
    -- (ref = 0, the device manager's boot root and procmgr's copy of it).
    ---------------------------------------------------------------------------
    function holdsProcessAuthority (callerPID : Process.ProcessID;
                                    handle    : Unsigned_64;
                                    targetPID : Process.ProcessID;
                                    right     : Capabilities.CapabilityRight)
                                    return Boolean
    is
        found : constant Process_Capabilities.Found :=
          findOwn (callerPID, handle, Capabilities.CAP_PROCESS, onlyRight (right));
    begin
        return found.OK and then
          (found.Cap.object.ref = 0 or else
           (found.Cap.object.ref = Unsigned_64 (targetPID) and then
            found.Cap.gen = Process.generationOf (targetPID)));
    end holdsProcessAuthority;


    ---------------------------------------------------------------------------
    -- handleRegisterDriver
    ---------------------------------------------------------------------------
    procedure handleRegisterDriver (callerPID : Process.ProcessID;
                                    arg0, arg1 : Unsigned_64;
                                    retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        hasCap : Boolean := False;
    begin
        retval := reterr;

        if arg0 > Service_Registry.DriverID'Last then
            return;
        end if;

        -- Kernel-mode threads are exempt; others name a notification
        -- capability (RIGHT_WRITE) covering the role (arg1).
        if Process.threadOf (callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            declare
                found : constant Process_Capabilities.Found :=
                  findOwn (callerPID, arg1, Capabilities.CAP_NOTIFICATION,
                           onlyRight (Capabilities.RIGHT_WRITE));
            begin
                hasCap := found.OK and then coversRole (found.Cap, arg0);
            end;
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

        -- The log service turns the kernel's device-memory refusals into
        -- records (Device_Memory_Publication); only it reads that page.
        if arg0 = Service_Registry.DRIVER_LOGSTORE then
            declare
                mapped : Boolean;
            begin
                Device_Memory_Diagnostics.Map_Into (callerPID, mapped);
                if not mapped then
                    println ("REGISTER_DRIVER: logstore refused, diagnostics page unmapped");
                    return;
                end if;
            end;
        end if;
        Service_Registry.Register (pid => callerPID, driver => arg0);
        -- The registrant's identity, not its slot (KERN-003).
        retval := Process_Identities.To_Word (Process.identityOf (callerPID));
        Boot_Timeline.Note_Event (Natural (callerPID), Boot_Timeline.Registered);
    end handleRegisterDriver;

    ---------------------------------------------------------------------------
    -- handleBootTimeline (docs/boot-timeline.md)
    ---------------------------------------------------------------------------
    procedure handleBootTimeline (callerPID : Process.ProcessID;
                                  arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
                                  retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        package BT renames Boot_Timeline_Layout;

        --  The driver role whose registration authority may mark each
        --  milestone: the service that owns the event.
        Owner : constant array (BT.Milestone) of Service_Registry.DriverID :=
          [BT.Startup_Profile_Started  => Service_Registry.DRIVER_PROCMGR,
           BT.Startup_Profile_Launched => Service_Registry.DRIVER_PROCMGR,
           BT.Display_Leased           => Service_Registry.DRIVER_DISPLAY,
           BT.First_Frame_Presented    => Service_Registry.DRIVER_DESKTOP,
           BT.First_Input_Accepted     => Service_Registry.DRIVER_DESKTOP];

        -- arg4 names the authority: a process capability with RIGHT_READ to
        -- read, the milestone owner's notification (RIGHT_WRITE) to mark.
        function Holds (Kind : Capabilities.CapabilityType;
                        Right : Capabilities.CapabilityRight;
                        Object : Unsigned_64;
                        Any_Object : Boolean) return Boolean is
            found : constant Process_Capabilities.Found :=
              findOwn (callerPID, arg4, Kind, onlyRight (Right));
        begin
            return found.OK and then
              (Any_Object or else coversRole (found.Cap, Object));
        end Holds;
    begin
        retval := reterr;
        if arg0 = BT.Operation'Enum_Rep (BT.Read) then
            if not Holds (Capabilities.CAP_PROCESS, Capabilities.RIGHT_READ, 0, True) then
                Process.IPC.notifySupervisor
                  (callerPID, IPC_Labels.EVENT_CAP_FAULT,
                   Unsigned_64 (SyscallNumber'Enum_Rep (SYSCALL_BOOT_TIMELINE)), arg0, arg1);
                return;
            end if;
            declare
                Chunk  : aliased Storage_Array (1 .. BT.Read_Chunk_Bytes);
                Copied : Unsigned_64;
                ok     : Boolean;
            begin
                Boot_Timeline.Read (Chunk'Address, arg3, arg2, Copied);
                if Copied = 0 then
                    retval := 0;
                    return;
                end if;
                Process.User_Memory.Copy_To_User
                  (callerPID, arg1, Chunk'Address, Storage_Count (Copied), ok);
                if ok then
                    retval := Copied;
                end if;
            end;
        elsif arg0 = BT.Operation'Enum_Rep (BT.Mark) then
            for Item in BT.Milestone loop
                if arg1 = BT.Milestone'Enum_Rep (Item) then
                    if Holds (Capabilities.CAP_NOTIFICATION, Capabilities.RIGHT_WRITE,
                              Unsigned_64 (Owner (Item)), False)
                    then
                        Boot_Timeline.Mark_Milestone
                          (Item, Process_Identities.To_Word (Process.identityOf (callerPID)));
                        retval := 0;
                    end if;
                    return;
                end if;
            end loop;
        end if;
    end handleBootTimeline;

    ---------------------------------------------------------------------------
    -- handlePortIO
    ---------------------------------------------------------------------------
    procedure handlePortIO (callerPID  : Process.ProcessID;
                            syscallNum : SyscallNumber;
                            arg0, arg1, arg2 : Unsigned_64;
                            retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        capAllowed : Boolean;
        -- arg2 names the CAP_IOPORT this access relies on.
        held : Process_Capabilities.Found;

        procedure checkPortAccess (port, accessSize : Unsigned_64;
                                   forWrite : Boolean; allowed : out Boolean) is
        begin
            allowed := held.OK and then held.Cap.capType = Capabilities.CAP_IOPORT
              and then port >= held.Cap.object.ref
              and then port - held.Cap.object.ref <= held.Cap.object.param
              and then accessSize <= held.Cap.object.param - (port - held.Cap.object.ref)
              and then (if forWrite then held.Cap.rights (Capabilities.RIGHT_WRITE)
                        else held.Cap.rights (Capabilities.RIGHT_READ));
        end checkPortAccess;

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
        held := findOwn (callerPID, arg2, Capabilities.CAP_IOPORT, Capabilities.NO_RIGHTS);
        case syscallNum is
            when SYSCALL_INP8 =>
                checkPortAccess (arg0 and 16#FFFF#, 1, False, capAllowed);
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
                checkPortAccess (arg0 and 16#FFFF#, 1, True, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 1, True);
                    retval := reterr;
                else
                    x86.out8 (x86.IOPort(arg0 and 16#FFFF#),
                              Unsigned_8(arg1 and 16#FF#));
                    retval := 0;
                end if;

            when SYSCALL_INP16 =>
                checkPortAccess (arg0 and 16#FFFF#, 2, False, capAllowed);
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
                checkPortAccess (arg0 and 16#FFFF#, 2, True, capAllowed);
                if not capAllowed then
                    logDenied (arg0, 2, True);
                    retval := reterr;
                else
                    x86.out16 (x86.IOPort(arg0 and 16#FFFF#),
                               Unsigned_16(arg1 and 16#FFFF#));
                    retval := 0;
                end if;

            when SYSCALL_INP32 =>
                checkPortAccess (arg0 and 16#FFFF#, 4, False, capAllowed);
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
                checkPortAccess (arg0 and 16#FFFF#, 4, True, capAllowed);
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
        pragma Unreferenced (callerPID);
        -- arg0: the endpoint handle. arg6: the deadline (absolute monotonic
        -- ms; Unsigned_64'Last is for ever, as the caller chose:
        -- docs/ipc-fastpath.md).
        replyTag := Process.IPC.capSend (
            handle  => arg0,
            msg     => sendMsg,
            deadlineMs => arg6);
        Process.threadtab (PerCPUData.getCurrentThread).replyMsg :=
            Process.NULL_MESSAGE;
        retval := tagToU64 (replyTag);
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
        if not Process.User_Memory.Message_Writable (callerPID, arg1) then
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
        declare
            Recording : constant Boolean := Boot_Timeline.Recording;
            Started   : constant Unsigned_64 :=
              (if Recording then Boot_Timeline.Now else 0);
            -- The callee as the capability named it when the call began.
            Named     : constant Process_Capabilities.Found :=
              (if Recording
               then findOwn (callerPID, arg0, Capabilities.CAP_ENDPOINT,
                             Capabilities.NO_RIGHTS)
               else (OK => False, Why => Capability_ABI.Invalid_Handle));
            Callee    : constant Unsigned_64 :=
              (if Named.OK then Named.Cap.object.ref else 0);
            Target    : constant Unsigned_64 :=
              (if Recording and then Callee in 1 .. Unsigned_64 (Process.ProctabRange'Last)
               then Process_Identities.To_Word (Process.identityOf (Process.ProcessID (Callee)))
               else Boot_Timeline_Layout.No_Identity);
        begin
            replyTag := Process.IPC.capCall
              (handle => arg0, msg => localMsg, deadlineMs => arg2);
            if Recording then
                Boot_Timeline.Note_Wait
                  (Slot    => Natural (callerPID),
                   Waiter  => Process_Identities.To_Word (Process.identityOf (callerPID)),
                   Kind    => Boot_Timeline_Layout.Call,
                   Label   => localMsg.tag.label,
                   Target  => Target,
                   Started => Started);
            end if;
        end;
        Process.User_Memory.Write_Message
          (callerPID, arg1, Process.threadtab (PerCPUData.getCurrentThread).replyMsg, ok);
        Process.threadtab (PerCPUData.getCurrentThread).replyMsg := Process.NULL_MESSAGE;
        retval := (if ok then tagToU64 (replyTag) else reterr);
    end handleCapCall;

    ---------------------------------------------------------------------------
    -- handleCapCallOffering
    ---------------------------------------------------------------------------
    procedure handleCapCallOffering (callerPID : Process.ProcessID;
                                     arg0, arg1, arg2, arg3 : Unsigned_64;
                                     retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        localMsg   : Process.Message;
        replyTag   : Process.MessageTag;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        source     : Process_Capabilities.Found;
        offer      : Capability_Install.Outcome;
        ok         : Boolean;
    begin
        retval := tagToU64 (Process.NULL_TAG);
        if not Process.User_Memory.Message_Writable (callerPID, arg1) then
            return;
        end if;
        Process.User_Memory.Read_Message (callerPID, arg1, localMsg, ok);
        if not ok then
            return;
        end if;
        Process.User_Memory.Copy
          (callerPID, arg2, derivation'Address, Capability_ABI.Derivation_Bytes, ok);
        if not ok or else not Capability_Install.Well_Formed (derivation) then
            return;
        end if;
        -- The offer is a narrowed copy of the caller's own capability (it
        -- must hold RIGHT_GRANT): nothing is created.
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        source := Process_Capabilities.Find_Any
          (Process.proctab(callerPID).caps, derivation (Capability_ABI.Source));
        if source.OK then
            offer := Capability_Install.Derive
              (source.Cap, Capability_Install.Decode (derivation));
        end if;
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        if not source.OK or else not offer.Accepted then
            return;
        end if;
        replyTag := Process.IPC.capCallOffering
          (handle => arg0, msg => localMsg, deadlineMs => arg3, offer => offer.Copy);
        Process.User_Memory.Write_Message
          (callerPID, arg1, Process.threadtab (PerCPUData.getCurrentThread).replyMsg, ok);
        Process.threadtab (PerCPUData.getCurrentThread).replyMsg := Process.NULL_MESSAGE;
        retval := (if ok then tagToU64 (replyTag) else reterr);
    end handleCapCallOffering;

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
        ok := Process.IPC.capSubmit (
            handle  => arg0,
            msg     => submitMsg,
            token   => arg6);
        retval := (if ok then 1 else 0);
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
                              arg0, arg1, arg2 : Unsigned_64;
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
        -- arg2 names a CAP_PROCESS with RIGHT_READ (any target).
        hasCap := findOwn (callerPID, arg2, Capabilities.CAP_PROCESS,
                           onlyRight (Capabilities.RIGHT_READ)).OK;

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
                                arg0, arg1 : Unsigned_64;
                                retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        found     : Process_Capabilities.Found;
        cap       : Capabilities.Capability;
        -- arg1 is a user pointer: a Capability_ABI.Inspection, built here
        -- and copied out through Process.User_Memory.
        result    : aliased Capability_ABI.Inspection := [others => 0];
        copied    : Boolean;
    begin
        -- arg0: a handle in the caller's own table. Another process's
        -- entries are never read.
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        found := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, arg0);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        if not found.OK then
            retval := Capability_ABI.Refusal_Word (found.Why);
            return;
        end if;
        cap := found.Cap;
        result :=
          [Capability_ABI.Capability_Kind =>
             Unsigned_64 (Capabilities.CapabilityType'Pos (cap.capType)),
           Capability_ABI.Rights_Mask   => Capability_Install.Mask_Of (cap.rights),
           Capability_ABI.Authority_Tag => cap.authorityTag,
           -- Its process by identity (the slot and generation it was made
           -- for), not the bare slot.
           Capability_ABI.Object =>
             (if cap.capType in Capabilities.CAP_ENDPOINT |
                                Capabilities.CAP_PROCESS |
                                Capabilities.CAP_CSPACE
                 and then cap.object.ref /= 0
                 and then cap.object.ref <= Unsigned_64 (Process.ProcessID'Last)
              then Process_Identities.To_Word (Process_Identities.Encode
                     (Process_Identities.Slot (cap.object.ref),
                      Process_Identities.Generation (cap.gen)))
              else cap.object.ref),
           Capability_ABI.Parameter  => cap.object.param,
           Capability_ABI.Generation => Unsigned_64 (cap.gen)];
        Process.User_Memory.Copy_To_User
          (callerPID, arg1, result'Address, result'Size / 8, copied);
        -- 0 on success, like CLOSE_HANDLE: every nonzero word is a refusal
        -- code (1 is Invalid_Handle), so success can never look like one.
        retval := (if copied then 0
                   else Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument));
    end handleInspectCap;

    ---------------------------------------------------------------------------
    -- handleInstallCap (INSTALL_CAPABILITY, CAP-002)
    -- arg0: a CSPACE handle (SPAWN gave it, one per child); arg1: address of
    -- a Capability_ABI.Derivation naming one of the caller's capabilities.
    -- Places a narrowed copy in the CSPACE's process, which must still be
    -- the incarnation it names and must not have started running. Returns
    -- the handle word in that process's table, or a refusal code. Nothing
    -- else about another process's table can be read or changed.
    ---------------------------------------------------------------------------
    procedure handleInstallCap (callerPID : Process.ProcessID;
                                arg0, arg1 : Unsigned_64;
                                retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Process_Launch.Launch_Phase;
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        copied     : Boolean;
        cspace     : Process_Capabilities.Found;
        source     : Process_Capabilities.Found;
        targetPID  : Process.ProcessID;
        firstPID, secondPID : Process.ProcessID;
        placed     : Process_Capabilities.Placement.Place_Result;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Copy
          (callerPID, arg1, derivation'Address, Capability_ABI.Derivation_Bytes, copied);
        if not copied or else not Capability_Install.Well_Formed (derivation) then
            return;
        end if;
        cspace := findOwn (callerPID, arg0, Capabilities.CAP_CSPACE,
                           onlyRight (Capabilities.RIGHT_GRANT));
        if not cspace.OK then
            retval := Capability_ABI.Refusal_Word (cspace.Why);
            return;
        elsif cspace.Cap.object.ref not in 1 .. Unsigned_64 (Process.ProctabRange'Last) then
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Target_Gone);
            return;
        end if;
        targetPID := Process.ProcessID (cspace.Cap.object.ref);
        firstPID := Process.ProcessID'Min (callerPID, targetPID);
        secondPID := Process.ProcessID'Max (callerPID, targetPID);
        -- The IPC two-mailbox ascending-PID order. The CSPACE, the source and
        -- the target's table are read and changed as one operation.
        Spinlocks.enterCriticalSection (Process.mailtab(firstPID).lock);
        if secondPID /= firstPID then
            Spinlocks.enterCriticalSection (Process.mailtab(secondPID).lock);
        end if;
        cspace := Process_Capabilities.Find_Any (Process.proctab(callerPID).caps, arg0);
        source := Process_Capabilities.Find_Any
          (Process.proctab(callerPID).caps, derivation (Capability_ABI.Source));
        if not cspace.OK then
            retval := Capability_ABI.Refusal_Word (cspace.Why);
        elsif not source.OK then
            retval := Capability_ABI.Refusal_Word (source.Why);
        elsif not Process.proctab(targetPID).admitted or else
              Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime)
        then
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Target_Gone);
        else
            Process_Capabilities.Placement.Place
              (Target   => Process.proctab(targetPID).caps,
               Admitted => Capability_Install.Cspace_Admits
                             (Cspace            => cspace.Cap,
                              Target            => Unsigned_64 (targetPID),
                              Target_Generation => Process.generationOf (targetPID),
                              Target_Started    =>
                                Process.proctab(targetPID).launch = Process_Launch.Started),
               Source   => source.Cap,
               Item     => Capability_Install.Decode (derivation),
               Result   => placed);
            retval := Process_Capabilities.Result_Word (placed);
        end if;
        if secondPID /= firstPID then
            Spinlocks.exitCriticalSection (Process.mailtab(secondPID).lock);
        end if;
        Spinlocks.exitCriticalSection (Process.mailtab(firstPID).lock);
    end handleInstallCap;

    ---------------------------------------------------------------------------
    -- handleDeriveCap (DERIVE_CAPABILITY): the caller derives a copy of one
    -- of its own capabilities into its own table. The same proved placement
    -- as INSTALL_CAPABILITY (never wider than the source, Set_Tag only on an
    -- untagged endpoint, existing entries untouched); no CSPACE is involved
    -- because nothing leaves the caller.
    ---------------------------------------------------------------------------
    procedure handleDeriveCap (callerPID : Process.ProcessID;
                               arg0      : Unsigned_64;
                               retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        derivation : aliased Capability_ABI.Derivation := [others => 0];
        copied     : Boolean;
        source     : Process_Capabilities.Found;
        placed     : Process_Capabilities.Placement.Place_Result;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        Process.User_Memory.Copy
          (callerPID, arg0, derivation'Address, Capability_ABI.Derivation_Bytes, copied);
        if not copied or else not Capability_Install.Well_Formed (derivation) then
            return;
        end if;
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        source := Process_Capabilities.Find_Any
          (Process.proctab(callerPID).caps, derivation (Capability_ABI.Source));
        if not source.OK then
            retval := Capability_ABI.Refusal_Word (source.Why);
        else
            Process_Capabilities.Placement.Place
              (Target   => Process.proctab(callerPID).caps,
               Admitted => (Admitted => True),
               Source   => source.Cap,
               Item     => Capability_Install.Decode (derivation),
               Result   => placed);
            retval := Process_Capabilities.Result_Word (placed);
        end if;
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
    end handleDeriveCap;

    ---------------------------------------------------------------------------
    -- handleCloseHandle (CLOSE_HANDLE): give up one of the caller's own
    -- entries. 0, or a refusal code (the handle was not current).
    ---------------------------------------------------------------------------
    procedure handleCloseHandle (callerPID : Process.ProcessID;
                                 arg0      : Unsigned_64;
                                 retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        removed : Capabilities.Capability;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        Process_Capabilities.Close (Process.proctab(callerPID).caps, arg0, removed, retval);
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
        -- A saved reply closed unanswered: its caller learns now, as when a
        -- server exits holding it.
        if retval = 0 and then removed.capType = Capabilities.CAP_REPLY then
            Process.IPC.failReply (removed);
        end if;
    end handleCloseHandle;

    procedure handleResume (callerPID : Process.ProcessID;
                            arg0, arg1, arg2, arg3 : Unsigned_64;
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

                if not holdsProcessAuthority (callerPID, arg1, targetPID,
                                              Capabilities.RIGHT_EXECUTE)
                then
                    println ("RESUME: denied, no RIGHT_EXECUTE");
                    retval := reterr;
                elsif Process.threadOf (targetPID).state /= Process.SUSPENDED then
                    println ("RESUME: target not suspended");
                    retval := reterr;
                else
                    -- The quota is the launcher's explicit choice (arg2 =
                    -- most frames, 0 unlimited; arg3 = CPU quota in the low
                    -- 32 bits and its period in the high 32, 0 unlimited).
                    candidateQuota.maxFrames := arg2;
                    candidateQuota.cpuQuotaUs := Unsigned_32 (arg3 and 16#FFFF_FFFF#);
                    candidateQuota.cpuPeriodUs := Unsigned_32 (Shift_Right (arg3, 32));
                    candidateQuota.cpuUsedTicks := 0;
                    candidateQuota.periodStartTick := Time.msTicks;

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
        arg0, arg1, arg2, arg3 : Unsigned_64;
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
                elsif not holdsProcessAuthority (callerPID, arg3, targetPID,
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
                          arg0, arg1 : Unsigned_64;
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

        if not holdsProcessAuthority (callerPID, arg1, targetPID,
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
                                 arg0, arg1, arg2 : Unsigned_64;
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
                or else holdsProcessAuthority (callerPID, arg2, targetPID,
                                               Capabilities.RIGHT_WRITE))
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
    -- handleEnableIrq
    ---------------------------------------------------------------------------
    procedure handleEnableIrq (callerPID  : Process.ProcessID;
                               arg0, arg1, arg2, arg3, arg4 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;

        -- The route word: target CPU in the low byte, then trigger and
        -- polarity for the IOAPIC redirection entry.
        ROUTE_CPU_MASK     : constant Unsigned_64 := 16#00FF#;
        ROUTE_LEVEL        : constant Unsigned_64 := 16#0100#;
        ROUTE_ACTIVE_LOW   : constant Unsigned_64 := 16#0200#;
        ROUTE_DEFINED_BITS : constant Unsigned_64 :=
          ROUTE_CPU_MASK or ROUTE_LEVEL or ROUTE_ACTIVE_LOW;

        irqOk  : Boolean;
        targetPID : Process.ProcessID;

        -- The caller's handle names a CAP_IRQ (RIGHT_GRANT: it may route the
        -- line to someone) whose range holds the vector: the device
        -- manager's legacy-line root or a copy of it.
        function legacyLineHeld (handle, vector : Unsigned_64) return Boolean is
            found : constant Process_Capabilities.Found :=
              findOwn (callerPID, handle, Capabilities.CAP_IRQ,
                       onlyRight (Capabilities.RIGHT_GRANT));
        begin
            return found.OK and then found.Cap.object.param > 0
              and then vector >= found.Cap.object.ref
              and then vector - found.Cap.object.ref < found.Cap.object.param;
        end legacyLineHeld;
    begin
        -- Validate args before cap check (arg1 is the owner PID)
        -- Only legacy lines with installed device-interrupt stubs may be
        -- routed. Never turn a missing IDT entry (or an exception/IPI
        -- vector) into a device route, even for a process-management
        -- authority holder; device vectors are allocated, never enabled.
        if arg0 not in Unsigned_64 (InterruptNumbers.PS2KEYBOARD) ..
          Unsigned_64 (InterruptNumbers.IDE2)
        then
            println ("ENABLE_IRQ: not a legacy line vector");
            retval := reterr;
            return;
        elsif (arg2 and not ROUTE_DEFINED_BITS) /= 0 then
            println ("ENABLE_IRQ: undefined route bits");
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
            if not holdsProcessAuthority (callerPID, arg3, targetPID,
                                          Capabilities.RIGHT_GRANT)
            then
                println ("ENABLE_IRQ: denied, no RIGHT_GRANT");
                retval := reterr;
            elsif not legacyLineHeld (arg4, arg0) then
                println ("ENABLE_IRQ: denied, no interrupt capability for the line");
                retval := reterr;
            else
                --  The resolved slot: delivery finds owners by slot.
                Capabilities.IRQ.registerIRQ (
                    vector => Natural (arg0),
                    pid    => Unsigned_64 (targetPID),
                    status => irqOk);

                if irqOk then
                    Interrupts.enableDeviceIRQ (
                        InterruptNumbers.x86Interrupt (arg0),
                        Unsigned_32 (arg2 and ROUTE_CPU_MASK),
                        levelTriggered => (arg2 and ROUTE_LEVEL) /= 0,
                        activeLow      => (arg2 and ROUTE_ACTIVE_LOW) /= 0);
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
    -- hasVectorAuthority
    -- The caller's handle names a CAP_IRQ over the whole device range with
    -- the given right: the device manager's vector authority (Modules).
    ---------------------------------------------------------------------------
    function hasVectorAuthority (callerPID : Process.ProcessID;
                                 handle    : Unsigned_64;
                                 right     : Capabilities.CapabilityRight)
                                 return Boolean
    is
        found : constant Process_Capabilities.Found :=
          findOwn (callerPID, handle, Capabilities.CAP_IRQ, onlyRight (right));
    begin
        return found.OK and then
          found.Cap.object.ref = Device_Vector_ABI.First_Vector and then
          found.Cap.object.param = Device_Vector_ABI.Vector_Count;
    end hasVectorAuthority;

    ---------------------------------------------------------------------------
    -- handleAllocateDeviceVectors
    ---------------------------------------------------------------------------
    procedure handleAllocateDeviceVectors
      (callerPID : Process.ProcessID;
       arg0, arg1, arg2, arg3, arg4, arg5 : Unsigned_64;
       retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Device_Interrupts.Allocation_Outcome;
        use type Process_Launch.Launch_Phase;
        targetPID : Process.ProcessID;
        expectedGeneration : Capabilities.Generation;
        cspace : Process_Capabilities.Found;
        result : aliased Capability_ABI.Vector_Allocation := [others => 0];
        copied : Boolean;
    begin
        retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        if not hasVectorAuthority (callerPID, arg4, Capabilities.RIGHT_GRANT) then
            println ("ALLOCATE_DEVICE_VECTORS: denied, no vector authority");
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Not_Permitted);
            return;
        elsif arg1 not in 1 .. Device_Vector_ABI.Max_Block_Vectors then
            println ("ALLOCATE_DEVICE_VECTORS: invalid count");
            return;
        elsif arg3 > Unsigned_64 (Natural'Last) then
            println ("ALLOCATE_DEVICE_VECTORS: invalid CPU");
            return;
        elsif not Process.User_Memory.Writable_Range
                    (callerPID, arg5, Capability_ABI.Vector_Allocation_Bytes)
        then
            return;
        end if;

        Process.resolveIdentity (Process_Identities.From_Word (arg0),
                                 targetPID, expectedGeneration);
        if targetPID = Process.NO_PROCESS then
            println ("ALLOCATE_DEVICE_VECTORS: invalid owner identity");
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Target_Gone);
            return;
        end if;

        declare
            procedure performLocked is
                head : Device_Interrupts.Vector;
                destination : Device_Vector_ABI.Destination_ID;
                outcome : Device_Interrupts.Allocation_Outcome;
                count : constant Device_Interrupts.Block_Size :=
                  Device_Interrupts.Block_Size (arg1);
                admitted : Capability_Install.Admission;
                word : Unsigned_64;
            begin
                if expectedGeneration /= Process.generationOf (targetPID) or else
                   not Process.proctab(targetPID).admitted or else
                   Process_Lifetime.Closing (Process.threadOf (targetPID).lifetime)
                then
                    retval := Capability_ABI.Refusal_Word (Capability_ABI.Target_Gone);
                    return;
                end if;
                -- The owner's CAP_IRQ goes into its table through the
                -- CSPACE the caller holds for it (arg2), before it runs.
                admitted := Capability_Install.Cspace_Admits
                  (Cspace            => cspace.Cap,
                   Target            => Unsigned_64 (targetPID),
                   Target_Generation => Process.generationOf (targetPID),
                   Target_Started    =>
                     Process.proctab(targetPID).launch = Process_Launch.Started);
                if not admitted.Admitted then
                    println ("ALLOCATE_DEVICE_VECTORS: denied by the owner's CSPACE");
                    retval := Capability_ABI.Refusal_Word (admitted.Why);
                    return;
                end if;

                Device_Interrupts.allocate
                  (targetPID, count, Natural (arg3), head, destination, outcome);
                case outcome is
                    when Device_Interrupts.Allocated =>
                        -- The owner's interrupt capability: first vector and
                        -- count, receive-only. Delivery follows the table,
                        -- the capability names what was granted.
                        Process_Capabilities.Insert
                          (Process.proctab(targetPID).caps,
                           (capType  => Capabilities.CAP_IRQ,
                            rights   => Capabilities.READ_ONLY,
                            authorityTag => Process_Identities.To_Word
                              (Process.identityOf (targetPID)),
                            object   => (ref   => Unsigned_64 (head),
                                         param => Unsigned_64 (count)),
                            gen      => Capabilities.INITIAL_GENERATION),
                           word);
                        if not Capability_ABI.Is_Handle_Word (word) then
                            -- No room in the owner's table: give the
                            -- vectors back rather than deliver unnamed.
                            declare
                                released : Boolean;
                                ignoreCount : Device_Interrupts.Block_Size;
                                ignoreOwner : Unsigned_64;
                            begin
                                Device_Interrupts.release
                                  (head, ignoreCount, ignoreOwner, released);
                            end;
                            retval := word;
                            return;
                        end if;
                        result := [Capability_ABI.Allocation =>
                                     Device_Vector_ABI.Allocation_Word
                                       (head, count, destination),
                                   Capability_ABI.Owner_Handle => word];
                        retval := 0;
                    when Device_Interrupts.Vectors_Exhausted =>
                        println ("ALLOCATE_DEVICE_VECTORS: device vectors exhausted");
                        retval := Capability_ABI.Refusal_Word (Capability_ABI.Out_Of_Memory);
                    when Device_Interrupts.Invalid_CPU =>
                        println ("ALLOCATE_DEVICE_VECTORS: CPU not running");
                end case;
            end performLocked;
        begin
            cspace := findOwn (callerPID, arg2, Capabilities.CAP_CSPACE,
                               onlyRight (Capabilities.RIGHT_GRANT));
            if not cspace.OK then
                retval := Capability_ABI.Refusal_Word (cspace.Why);
                return;
            end if;
            Spinlocks.enterCriticalSection (Process.mailtab(targetPID).lock);
            performLocked;
            Spinlocks.exitCriticalSection (Process.mailtab(targetPID).lock);
        end;
        if retval = 0 then
            Process.User_Memory.Copy_To_User
              (callerPID, arg5, result'Address, Capability_ABI.Vector_Allocation_Bytes, copied);
            if not copied then
                retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
            end if;
        end if;
    end handleAllocateDeviceVectors;

    ---------------------------------------------------------------------------
    -- handleReleaseDeviceVectors
    ---------------------------------------------------------------------------
    procedure handleReleaseDeviceVectors
      (callerPID : Process.ProcessID;
       arg0, arg1 : Unsigned_64;
       retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        count : Device_Interrupts.Block_Size;
        owner : Unsigned_64;
        released : Boolean;
        ownerPID : Process.ProcessID;
    begin
        retval := reterr;
        if not hasVectorAuthority (callerPID, arg1, Capabilities.RIGHT_GRANT) then
            println ("RELEASE_DEVICE_VECTORS: denied, no vector authority");
            return;
        elsif arg0 not in Device_Vector_ABI.First_Vector .. Device_Vector_ABI.Last_Vector then
            return;
        end if;

        Device_Interrupts.release
          (Device_Interrupts.Vector (arg0), count, owner, released);
        if not released then
            println ("RELEASE_DEVICE_VECTORS: not the first vector of a block");
            return;
        end if;

        -- The owner may still live (setup failed, or a device was taken
        -- away): withdraw the capability that named the block.
        ownerPID := Process.processOfIdentity (Process_Identities.From_Word (owner));
        if ownerPID /= Process.NO_PROCESS then
            Spinlocks.enterCriticalSection (Process.mailtab(ownerPID).lock);
            -- Revocation from outside: the owner's handle reports Revoked.
            for slot in 0 .. Process_Capabilities.Tables.High_Water
                               (Process.proctab(ownerPID).caps) - 1
            loop
                if Process_Capabilities.Tables.Is_Live_At (Process.proctab(ownerPID).caps, slot)
                then
                    declare
                        cap : constant Capabilities.Capability :=
                          Process_Capabilities.Tables.Item_At (Process.proctab(ownerPID).caps, slot);
                    begin
                        if cap.capType = Capabilities.CAP_IRQ and then
                           cap.object.ref = arg0 and then
                           cap.object.param = Unsigned_64 (count)
                        then
                            Process_Capabilities.Revoke_At (Process.proctab(ownerPID).caps, slot);
                        end if;
                    end;
                end if;
            end loop;
            Spinlocks.exitCriticalSection (Process.mailtab(ownerPID).lock);
        end if;
        retval := 0;
    end handleReleaseDeviceVectors;

    ---------------------------------------------------------------------------
    -- handleReadDeviceVector
    ---------------------------------------------------------------------------
    procedure handleReadDeviceVector
      (callerPID  : Process.ProcessID;
       arg0, arg1, arg2 : Unsigned_64;
       retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use Device_Vector_ABI;
        v : Device_Interrupts.Vector;
    begin
        retval := reterr;
        if not hasVectorAuthority (callerPID, arg2, Capabilities.RIGHT_READ) or else
           arg0 not in First_Vector .. Last_Vector
        then
            return;
        end if;
        v := Device_Interrupts.Vector (arg0);
        if arg1 = Read_Field'Enum_Rep (Interrupt_Count) then
            retval := Device_Interrupts.interruptCount (v);
        elsif arg1 = Read_Field'Enum_Rep (Owner_Identity) then
            retval := Device_Interrupts.ownerIdentity (v);
        elsif arg1 = Read_Field'Enum_Rep (Binding_State) then
            retval := Vector_Binding'Enum_Rep (Device_Interrupts.binding (v));
        end if;
    end handleReadDeviceVector;

    ---------------------------------------------------------------------------
    -- handlePublish (SYSCALL_PUBLISH, KERN-002 writer)
    -- arg0 = System_Publications.Published_Value, arg1 = the word. Each value
    -- has one publisher: the wall-clock offset the registered clock service,
    -- the boot display handoff the registered device manager.
    ---------------------------------------------------------------------------
    procedure handlePublish (callerPID  : Process.ProcessID;
                             arg0, arg1 : Unsigned_64;
                             retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        package SP renames System_Publications;
        function Registered_As (Owner : Service_Registry.DriverID) return Boolean is
          (Process.threadOf (callerPID).mode = Process.KERNEL or else
           Service_Registry.Registered (Owner) = callerPID);
    begin
        retval := reterr;
        if arg0 = SP.Published_Value'Enum_Rep (SP.Wall_Clock_Offset) then
            if not Registered_As (Service_Registry.DRIVER_CLOCK) then
                println ("PUBLISH: denied, caller is not the registered clock");
                return;
            end if;
            Publication_Pages.Set_Wall_Clock (arg1);
            retval := 0;
        elsif arg0 = SP.Published_Value'Enum_Rep (SP.Boot_Display_Native) then
            if not Registered_As (Service_Registry.DRIVER_DEVMGR) then
                println ("PUBLISH: denied, caller is not the registered device manager");
                return;
            elsif arg1 > 1 then
                return;
            end if;
            --  The boot-adapter handoff precedes starting its native driver:
            --  retire the firmware renderer before that driver can reset or
            --  reprogram the display, not afterward.
            if arg1 = 1 then
                Boot_Output.Retire;
            end if;
            Publication_Pages.Set_Boot_Display_Native (arg1 = 1);
            retval := 0;
        else
            println ("PUBLISH: unknown value");
        end if;
    end handlePublish;

    ---------------------------------------------------------------------------
    -- handleSetCpu
    ---------------------------------------------------------------------------
    procedure handleSetCpu (callerPID  : Process.ProcessID;
                            arg0, arg1, arg2 : Unsigned_64;
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

                if not holdsProcessAuthority (callerPID, arg2, targetPID,
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
    -- handleSaveReplyCap (MOVE_REPLY_CAPABILITY)
    -- Move the calling thread's current reply authority into the process's
    -- table, where the kernel chooses: servers that answer later (netstack,
    -- the filesystem, intel-gpu) keep the returned handle and answer with
    -- REPLY_AND_CONSUME_REPLY_CAPABILITY. Returns the handle word, or a
    -- refusal code (no current reply: Invalid_Argument; the table's limit,
    -- capacity or memory); on refusal the thread keeps its reply authority.
    ---------------------------------------------------------------------------
    procedure handleSaveReplyCap (callerPID : Process.ProcessID;
                                   retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;
        me : constant Process.ThreadID := PerCPUData.getCurrentThread;
    begin
        Spinlocks.enterCriticalSection (Process.mailtab(callerPID).lock);
        if Process.threadtab (me).replyCap.capType /= Capabilities.CAP_REPLY then
            retval := Capability_ABI.Refusal_Word (Capability_ABI.Invalid_Argument);
        else
            Process_Capabilities.Insert
              (Process.proctab(callerPID).caps, Process.threadtab (me).replyCap, retval);
            if Capability_ABI.Is_Handle_Word (retval) then
                Process.threadtab (me).replyCap := Capabilities.NULL_CAPABILITY;
            end if;
        end if;
        Spinlocks.exitCriticalSection (Process.mailtab(callerPID).lock);
    end handleSaveReplyCap;

end Syscall.Admin;
