-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
--
-- Syscall IPC, memory, and process creation handler implementations.
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with acpi;
with BuddyAllocator;
with Capabilities;
with Capabilities.IRQ;
with Capabilities.Operations;
with InterruptNumbers;
with IPC_Labels;
with ELF;
with Interrupts;
with PerCpuData;
with Process;
with Process.IPC;
with Process.Loader;
with Sysinfo;
with TextIO; use TextIO;
with Util;
with Video.VGA;
with Virtmem;
with x86;

use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall.IPC is

    function toErr is
        new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
    reterr : constant Unsigned_64 := toErr (-1);

    function tagToU64 is new Ada.Unchecked_Conversion
        (Process.MessageTag, Unsigned_64);
    function u64ToTag is new Ada.Unchecked_Conversion
        (Unsigned_64, Process.MessageTag);
    function u64ToRights is new Ada.Unchecked_Conversion
        (Unsigned_8, Capabilities.CapabilityRights);

    procedure handleSpawn (callerPID : Process.ProcessID;
                           arg0      : Unsigned_64;
                           arg1      : Unsigned_64;
                           arg2      : Unsigned_64;
                           arg3      : Unsigned_64;
                           arg4      : Unsigned_64;
                           arg5      : Unsigned_64;
                           retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- Import overlay
    is
        use type ELF.SegmentType;
        use type Capabilities.CapabilityType;


        hasCap   : Boolean := False;
        elfAddr  : constant System.Address := Util.numToAddr (arg0);
        elfSize  : constant Storage_Count := Storage_Count (arg1);
        priority : Process.ProcessPriority;
        elfHeader : ELF.ELFFileHeader with Import, Address => elfAddr;
        defaultName : aliased constant String := "spawned" & ASCII.NUL;
        nameAddr : System.Address;
        newPID   : Process.ProcessID;
        reqPID   : Process.ProcessID := Process.NO_PROCESS;
    begin
        retval := reterr;

        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_EXECUTE)
            then
                hasCap := True;
                exit;
            end if;
        end loop;

        if not hasCap then
            println ("SPAWN: denied, no CAP_PROCESS/EXECUTE");
            return;
        elsif elfSize < 64 then
            println ("SPAWN: ELF too small");
            return;
        end if;

        -- Enable user memory access for ELF reading (SMAP)
        x86.stac;

        if not Process.Loader.isValidELF (elfHeader) then
            x86.clac;
            println ("SPAWN: invalid ELF header");
            return;
        end if;

        if arg2 > 10 then
            priority := 5;
        elsif arg2 = 0 then
            priority := 1;
        else
            priority := Process.ProcessPriority (arg2);
        end if;

        if arg4 > 0 and arg4 <= Unsigned_64 (Process.ProcessID'Last) then
            reqPID := Process.ProcessID (arg4);
        end if;

        -- arg3 = name pointer (0 = default "spawned")
        if arg3 /= 0 then
            nameAddr := Util.numToAddr (arg3);
        else
            nameAddr := defaultName'Address;
        end if;

        newPID := Process.Loader.load (
            elfHeader    => elfHeader,
            objStart     => elfAddr,
            size         => elfSize,
            strAddr      => nameAddr,
            requestedPID => reqPID,
            priority     => priority,
            ppid         => (if arg5 <= Unsigned_64 (Process.ProcessID'Last)
                              then Process.ProcessID (arg5)
                              else callerPID));

        x86.clac;

        if newPID = Process.NO_PROCESS then
            println ("SPAWN: load failed");
            return;
        end if;

        -- Always spawn suspended; caller resumes after granting caps
        print ("SPAWN: created suspended PID ");
        println (Integer (newPID));
        retval := Unsigned_64 (newPID);
    end handleSpawn;

    ---------------------------------------------------------------------------
    -- handleMapDevice
    -- Extracted to its own procedure to keep mapPage instantiation off
    -- syscallHandler's stack frame.
    ---------------------------------------------------------------------------
    procedure handleMapDevice (callerPID : Process.ProcessID;
                               arg0      : Unsigned_64;
                               arg1      : Unsigned_64;
                               arg2      : Unsigned_64;
                               retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is

        procedure mapPageInst is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);

        physAddr : constant Virtmem.PhysAddress :=
            Virtmem.PhysAddress (arg0);
        virtAddr : constant Integer_Address := Integer_Address (arg1);
        numPages : constant Unsigned_64 := arg2;
        capAllowed : Boolean;
        ok : Boolean := True;
    begin
        retval := reterr;

        if numPages = 0 or numPages > 1024 then
            return;
        end if;

        Capabilities.Operations.checkDeviceMemAccess (
            table   => Process.proctab(callerPID).caps,
            base    => arg0,
            size    => numPages * Unsigned_64 (Virtmem.PAGE_SIZE),
            allowed => capAllowed);

        if not capAllowed then
            println ("MAP_DEVICE: denied, no CAP_DEVICE_MEM");
            return;
        end if;

        for i in 0 .. numPages - 1 loop
            declare
                pageOk : Boolean;
            begin
                mapPageInst (
                    phys    => physAddr +
                        Virtmem.PhysAddress (
                            i * Unsigned_64 (Virtmem.PAGE_SIZE)),
                    virt    => virtAddr +
                        Integer_Address (
                            i * Unsigned_64 (Virtmem.PAGE_SIZE)),
                    flags   => Virtmem.PG_USERIO,
                    myP4    => Process.addrtab(callerPID),
                    success => pageOk);
                if not pageOk then
                    ok := False;
                end if;
            end;
        end loop;

        if ok then
            retval := 0;
        end if;
    end handleMapDevice;

    ---------------------------------------------------------------------------
    -- handleAllocDma
    -- Extracted to its own procedure to avoid inflating syscallHandler's
    -- stack frame with mapPage generic instantiation + locals.
    ---------------------------------------------------------------------------
    procedure handleAllocDma (callerPID : Process.ProcessID;
                              arg0      : Unsigned_64;
                              arg1      : Unsigned_64;
                              arg2      : Unsigned_64;
                              retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
        hasCap    : Boolean := False;
        order     : BuddyAllocator.Order;
        dmaAddr   : System.Address;
        dmaPhys   : Virtmem.PhysAddress;
        virtBase  : Virtmem.VirtAddress;
        ok        : Boolean;

        procedure mapPage is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);
    begin
        retval := reterr;

        if arg0 > Unsigned_64 (Process.ProcessID'Last) or arg0 = 0 then
            return;
        elsif arg1 >= Unsigned_64 (BuddyAllocator.Order'Last) then
            println ("ALLOC_DMA: order too large");
            return;
        end if;

        targetPID := Process.ProcessID (arg0);
        order := BuddyAllocator.Order (arg1);
        virtBase := Virtmem.VirtAddress (arg2);

        -- Check CAP_PROCESS with RIGHT_GRANT matching target (ref=0 wildcard)
        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_GRANT) and then
               (Process.proctab(callerPID).caps(slot).object.ref = 0 or
                (Process.proctab(callerPID).caps(slot).gen =
                     Process.proctab(targetPID).capGeneration and then
                 Process.proctab(callerPID).caps(slot).object.ref =
                     Unsigned_64 (targetPID)))
            then
                hasCap := True;
                exit;
            end if;
        end loop;

        if not hasCap then
            println ("ALLOC_DMA: denied, no RIGHT_GRANT");
            return;
        elsif Process.proctab(targetPID).state = Process.INVALID then
            println ("ALLOC_DMA: target not valid");
            return;
        end if;

        BuddyAllocator.alloc (order, dmaAddr);

        if System."=" (dmaAddr, BuddyAllocator.NO_BLOCK_AVAILABLE) then
            println ("ALLOC_DMA: alloc failed");
            return;
        end if;

        dmaPhys := Virtmem.V2P (dmaAddr);
        declare
            numPages : constant Natural := 2 ** Natural (order);
        begin
            for i in 0 .. numPages - 1 loop
                mapPage (
                    phys    => dmaPhys +
                        Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                    virt    => virtBase +
                        Virtmem.VirtAddress (i * Virtmem.PAGE_SIZE),
                    flags   => Virtmem.PG_USERDATA,
                    myP4    => Process.addrtab (targetPID),
                    success => ok);

                if not ok then
                    print ("ALLOC_DMA: map fail pg ");
                    println (i);
                    return;
                end if;
            end loop;

            --  Track DMA allocation for cleanup on kill()
            trackDMA : for d in Process.DMAAllocArray'Range loop
                if not Process.proctab(targetPID).dmaAllocs(d).active then
                    Process.proctab(targetPID).dmaAllocs(d) :=
                        (active   => True,
                         physAddr => dmaPhys,
                         order    => order);
                    exit trackDMA;
                end if;
            end loop trackDMA;

            retval := Unsigned_64 (dmaPhys);
        end;
    end handleAllocDma;

    ---------------------------------------------------------------------------
    -- handleMapInto
    -- Extracted to its own procedure to avoid inflating syscallHandler's
    -- stack frame with mapPage generic instantiation + locals.
    ---------------------------------------------------------------------------
    procedure handleMapInto (callerPID : Process.ProcessID;
                             arg0      : Unsigned_64;
                             arg1      : Unsigned_64;
                             arg2      : Unsigned_64;
                             arg3      : Unsigned_64;
                             arg4      : Unsigned_64;
                             retval    : out Unsigned_64) with
        SPARK_Mode => Off   -- generic instantiation
    is
        use type Capabilities.CapabilityType;
        use type Process.ProcessState;


        targetPID : Process.ProcessID;
        hasCap    : Boolean := False;
        ok        : Boolean;
        pgFlags   : Unsigned_64;

        procedure mapPage is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);
    begin
        retval := reterr;

        if arg0 > Unsigned_64 (Process.ProcessID'Last) or arg0 = 0 then
            return;
        elsif arg3 > 1024 then
            println ("MAP_INTO: too many pages");
            return;
        end if;

        targetPID := Process.ProcessID (arg0);

        -- Check CAP_PROCESS with RIGHT_GRANT matching target (ref=0 wildcard)
        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_GRANT) and then
               (Process.proctab(callerPID).caps(slot).object.ref = 0 or
                (Process.proctab(callerPID).caps(slot).gen =
                     Process.proctab(targetPID).capGeneration and then
                 Process.proctab(callerPID).caps(slot).object.ref =
                     Unsigned_64 (targetPID)))
            then
                hasCap := True;
                exit;
            end if;
        end loop;

        if not hasCap then
            println ("MAP_INTO: denied, no RIGHT_GRANT");
            return;
        elsif Process.proctab(targetPID).state = Process.INVALID then
            println ("MAP_INTO: target not valid");
            return;
        end if;

        case arg4 is
            when 0 => pgFlags := Virtmem.PG_USERDATA;
            when 1 => pgFlags := Virtmem.PG_USERDATARO;
            when 2 => pgFlags := Virtmem.PG_USERIO;
            when others => pgFlags := Virtmem.PG_USERDATA;
        end case;

        ok := True;
        for i in 0 .. Natural (arg3) - 1 loop
            mapPage (
                phys    => Virtmem.PhysAddress (arg1) +
                    Virtmem.PhysAddress (i * Virtmem.PAGE_SIZE),
                virt    => Virtmem.VirtAddress (arg2) +
                    Virtmem.VirtAddress (i * Virtmem.PAGE_SIZE),
                flags   => pgFlags,
                myP4    => Process.addrtab (targetPID),
                success => ok);

            if not ok then
                print ("MAP_INTO: map fail page ");
                println (i);
                return;
            end if;
        end loop;

        retval := 0;
    end handleMapInto;

    ---------------------------------------------------------------------------
    -- handleSbrk
    ---------------------------------------------------------------------------
    procedure handleSbrk (callerPID : Process.ProcessID;
                          arg0      : Unsigned_64;
                          retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
        function toNum is new Ada.Unchecked_Conversion
            (System.Address, Unsigned_64);

        increment : constant Storage_Count := Storage_Count(arg0);
        oldEnd    : constant System.Address :=
            Process.proctab(callerPID).heapEnd;
        newEnd    : System.Address;
        storage   : System.Address;
        curPage   : System.Address;
        newPage   : System.Address;
    begin
        if increment = 0 then
            retval := toNum (oldEnd);
        else
            newEnd := oldEnd + increment;

            curPage := To_Address (
                (To_Integer(oldEnd) +
                 Integer_Address(Virtmem.PAGE_SIZE - 1)) and
                Integer_Address(Virtmem.PAGE_MASK));

            newPage := To_Address (
                (To_Integer(newEnd) +
                 Integer_Address(Virtmem.PAGE_SIZE - 1)) and
                Integer_Address(Virtmem.PAGE_MASK));

            while To_Integer(curPage) < To_Integer(newPage) loop
                Process.addPage (
                    proc    => Process.proctab(callerPID),
                    mapTo   => curPage,
                    storage => storage,
                    flags   => Virtmem.PG_USERDATA);
                curPage := curPage + Virtmem.PAGE_SIZE;
            end loop;

            Process.proctab(callerPID).heapEnd := newEnd;
            retval := toNum (oldEnd);
        end if;
    end handleSbrk;

    ---------------------------------------------------------------------------
    -- handleMapFB
    ---------------------------------------------------------------------------
    procedure handleMapFB (callerPID : Process.ProcessID;
                           retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is

        procedure mapPageInst is new Virtmem.mapPage
            (BuddyAllocator.allocFrame);

        FB_USER_BASE : constant Integer_Address :=
            16#0000_6000_0000_0000#;

        fbSize   : constant Storage_Count := Video.VGA.framebufferSize;
        numPages : constant Storage_Count :=
            (fbSize + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;
        fbPhys   : constant Virtmem.PhysAddress :=
            Virtmem.PhysAddress(
                To_Integer(Video.VGA.framebufferAddr) -
                Virtmem.LINEAR_BASE);

        ok : Boolean := True;
        capAllowed : Boolean;
    begin
        Capabilities.Operations.checkDeviceMemAccess (
            table   => Process.proctab(callerPID).caps,
            base    => 0,
            size    => Unsigned_64(fbSize),
            allowed => capAllowed);

        if not capAllowed then
            retval := reterr;
        else
            for i in 0 .. numPages - 1 loop
                declare
                    pageOk : Boolean;
                begin
                    mapPageInst (
                        phys    => fbPhys +
                            Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                        virt    => FB_USER_BASE +
                            Integer_Address(i * Virtmem.PAGE_SIZE),
                        flags   => Virtmem.PG_USERIO_WC,
                        myP4    => Process.addrtab(callerPID),
                        success => pageOk);
                    if not pageOk then
                        ok := False;
                    end if;
                end;
            end loop;

            if ok then
                retval := Unsigned_64(FB_USER_BASE);
            else
                retval := reterr;
            end if;
        end if;
    end handleMapFB;

    ---------------------------------------------------------------------------
    -- handleReceive
    ---------------------------------------------------------------------------
    procedure handleReceive (arg0   : Unsigned_64;
                             retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        from    : Process.ProcessID;
        recvMsg : Process.Message;
        userMsg : Process.Message with
            Import, Address => Util.numToAddr(arg0);
    begin
        Process.IPC.receive (from, recvMsg);
        x86.stac;
        userMsg := recvMsg;
        x86.clac;
        retval := Unsigned_64(from);
    end handleReceive;

    ---------------------------------------------------------------------------
    -- handleSend
    ---------------------------------------------------------------------------
    procedure handleSend (callerPID : Process.ProcessID;
                          arg0, arg1, arg2, arg3,
                          arg4, arg5 : Unsigned_64;
                          retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is




        sendMsg  : constant Process.Message := (
            tag      => u64ToTag (arg1),
            capBadge => 0,
            words    => (arg2, arg3, arg4, arg5));
        replyTag : Process.MessageTag;
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
        else
            replyTag := Process.IPC.send (
                dest => Process.ProcessID(arg0),
                msg  => sendMsg);
            Process.proctab(callerPID).replyMsg := Process.NULL_MESSAGE;
            retval := tagToU64 (replyTag);
        end if;
    end handleSend;

    ---------------------------------------------------------------------------
    -- handleReply
    ---------------------------------------------------------------------------
    procedure handleReply (arg0, arg1, arg2, arg3,
                           arg4, arg5 : Unsigned_64;
                           retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is



        replyMsg : constant Process.Message := (
            tag      => u64ToTag (arg1),
            capBadge => 0,
            words    => (arg2, arg3, arg4, arg5));
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
        else
            retval := Process.IPC.reply (
                replyTo => Process.ProcessID(arg0),
                msg     => replyMsg);
        end if;
    end handleReply;

    ---------------------------------------------------------------------------
    -- handleReceiveEventNB
    ---------------------------------------------------------------------------
    procedure handleReceiveEventNB (arg0   : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        eventMsg : Process.Message;
        found    : Boolean;
        userMsg  : Process.Message with
            Import, Address => Util.numToAddr(arg0);
    begin
        Process.IPC.receiveEventNB (eventMsg, found);
        if found then
            x86.stac;
            userMsg := eventMsg;
            x86.clac;
            retval := 1;
        else
            retval := 0;
        end if;
    end handleReceiveEventNB;

    ---------------------------------------------------------------------------
    -- handleSendEvent
    ---------------------------------------------------------------------------
    procedure handleSendEvent (callerPID : Process.ProcessID;
                               arg0, arg1, arg2, arg3,
                               arg4, arg5 : Unsigned_64;
                               retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;




        destPID : Process.ProcessID;
        hasCap  : Boolean := False;
        eventMsg : Process.Message;
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
            return;
        end if;

        destPID := Process.ProcessID (arg0);
        eventMsg := (
            tag      => u64ToTag (arg1),
            capBadge => 0,
            words    => (arg2, arg3, arg4, arg5));

        -- Kernel-mode threads are exempt
        if Process.proctab(callerPID).mode = Process.KERNEL then
            hasCap := True;
        end if;

        -- CAP_IRQ holders are hardware drivers that
        -- forward events to consumers by definition.
        if not hasCap then
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_IRQ
                then
                    hasCap := True;
                    exit;
                end if;
            end loop;
        end if;

        if not hasCap then
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_ENDPOINT and then
                   Process.proctab(callerPID).caps(slot).object.ref =
                   Unsigned_64 (destPID) and then
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
                SYSCALL_SEND_EVENT,
                arg0, 0);
            retval := reterr;
        else
            Process.IPC.sendEvent (
                dest => destPID,
                msg  => eventMsg);
            retval := 1;
        end if;
    end handleSendEvent;

    ---------------------------------------------------------------------------
    -- handleCall
    ---------------------------------------------------------------------------
    procedure handleCall (callerPID : Process.ProcessID;
                          arg0, arg1 : Unsigned_64;
                          retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is



        userMsg  : Process.Message
            with Import, Address => Util.numToAddr(arg1);
        replyTag : Process.MessageTag;
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
        else
            -- Copy from user memory before blocking IPC call.
            -- Context switch during send clears EFLAGS.AC (SMAP).
            declare
                localMsg : Process.Message;
            begin
                x86.stac;
                localMsg := userMsg;
                x86.clac;

                replyTag := Process.IPC.send (
                    dest => Process.ProcessID(arg0),
                    msg  => localMsg);
            end;

            x86.stac;
            userMsg := Process.proctab(callerPID).replyMsg;
            x86.clac;
            Process.proctab(callerPID).replyMsg :=
                Process.NULL_MESSAGE;
            retval := tagToU64 (replyTag);
        end if;
    end handleCall;

    ---------------------------------------------------------------------------
    -- handleReceiveNB
    ---------------------------------------------------------------------------
    procedure handleReceiveNB (arg0   : Unsigned_64;
                               retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        from    : Process.ProcessID;
        recvMsg : Process.Message;
        found   : Boolean;
        userMsg : Process.Message with
            Import, Address => Util.numToAddr(arg0);
    begin
        Process.IPC.receiveNB (from, recvMsg, found);
        if found then
            x86.stac;
            userMsg := recvMsg;
            x86.clac;
            retval := Unsigned_64(from);
        else
            retval := 0;
        end if;
    end handleReceiveNB;

    ---------------------------------------------------------------------------
    -- handleSubmit
    ---------------------------------------------------------------------------
    procedure handleSubmit (arg0, arg1, arg2, arg3,
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
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := 0;
        else
            ok := Process.IPC.submit (
                dest  => Process.ProcessID(arg0),
                msg   => submitMsg,
                token => arg5);
            if ok then
                retval := 1;
            else
                retval := 0;
            end if;
        end if;
    end handleSubmit;

    ---------------------------------------------------------------------------
    -- handleWaitCompletion
    ---------------------------------------------------------------------------
    procedure handleWaitCompletion (arg0, arg1, arg2 : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        numReturned : Natural;
        userBuf : Process.CompletionRing with
            Import, Address => Util.numToAddr(arg0);
        effectiveMax : Natural;
        effectiveMin : Natural;
    begin
        if arg1 > Unsigned_64(Process.COMPLETION_QUEUE_SIZE) then
            effectiveMax := Process.COMPLETION_QUEUE_SIZE;
        else
            effectiveMax := Natural(arg1);
        end if;

        if arg2 > Unsigned_64(effectiveMax) then
            effectiveMin := effectiveMax;
        else
            effectiveMin := Natural(arg2);
        end if;

        -- waitCompletion handles STAC/CLAC internally since it may block
        Process.IPC.waitCompletion (
            entries     => userBuf,
            maxEntries  => effectiveMax,
            minWait     => effectiveMin,
            numReturned => numReturned);

        retval := Unsigned_64(numReturned);
    end handleWaitCompletion;

    ---------------------------------------------------------------------------
    -- handlePollCompletion
    ---------------------------------------------------------------------------
    procedure handlePollCompletion (arg0   : Unsigned_64;
                                    retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        found      : Boolean;
        localEntry : Process.CompletionEntry;
        userEntry  : Process.CompletionEntry with
            Import, Address => Util.numToAddr(arg0);
    begin
        Process.IPC.pollCompletion (localEntry, found);
        if found then
            x86.stac;
            userEntry := localEntry;
            x86.clac;
            retval := 1;
        else
            retval := 0;
        end if;
    end handlePollCompletion;

    ---------------------------------------------------------------------------
    -- handleGrant
    ---------------------------------------------------------------------------
    procedure handleGrant (callerPID : Process.ProcessID;
                           arg0, arg1, arg2, arg3 : Unsigned_64;
                           retval : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        granteePID : Process.ProcessID;
        hasCap : Boolean := False;
        gid : Process.GrantID;
        perm : Process.GrantPermission;
        ok : Boolean;
    begin
        if arg0 > Unsigned_64(Process.ProcessID'Last) then
            retval := reterr;
            return;
        end if;

        granteePID := Process.ProcessID (arg0);

        -- Kernel-mode threads exempt
        if Process.proctab(callerPID).mode = Process.KERNEL then
            hasCap := True;
        else
            -- Forward: caller has endpoint to grantee
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_ENDPOINT and then
                   Process.proctab(callerPID).caps(slot).object.ref =
                   Unsigned_64 (granteePID)
                then
                    hasCap := True;
                    exit;
                end if;
            end loop;
        end if;

        -- Reverse: grantee has endpoint to caller
        -- with RIGHT_GRANT (explicit opt-in to
        -- receiving grants from that service).
        if not hasCap then
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(granteePID).caps(slot).capType =
                   Capabilities.CAP_ENDPOINT and then
                   Process.proctab(granteePID).caps(slot).object.ref =
                   Unsigned_64 (callerPID) and then
                   Process.proctab(granteePID).caps(slot).rights(
                       Capabilities.RIGHT_GRANT)
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
                SYSCALL_GRANT,
                arg0, arg1);
            retval := reterr;
        else
            if arg3 = 1 then
                perm := Process.GRANT_READWRITE;
            else
                perm := Process.GRANT_READ;
            end if;

            Process.IPC.createGrant (
                grantee   => granteePID,
                localAddr => Util.numToAddr(arg1),
                numPages  => Natural(arg2),
                perm      => perm,
                id        => gid,
                success   => ok);

            if ok then
                retval := Unsigned_64(gid);
            else
                retval := reterr;
            end if;
        end if;
    end handleGrant;

    ---------------------------------------------------------------------------
    -- handleRevoke
    ---------------------------------------------------------------------------
    procedure handleRevoke (callerPID : Process.ProcessID;
                            arg0      : Unsigned_64;
                            retval    : out Unsigned_64) with
        SPARK_Mode => Off
    is
    begin
        if arg0 > Unsigned_64(Process.GrantID'Last) then
            retval := 0;
        elsif Process.proctab(callerPID).grants(
                  Process.GrantID (arg0)).granterPID /= callerPID
        then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                SYSCALL_REVOKE,
                arg0, 0);
            retval := 0;
        else
            Process.IPC.revokeGrant (id => Process.GrantID (arg0));
            retval := 1;
        end if;
    end handleRevoke;

    ---------------------------------------------------------------------------
    -- handleInfo
    ---------------------------------------------------------------------------
    procedure handleInfo (callerPID  : Process.ProcessID;
                          arg0, arg1 : Unsigned_64;
                          retval     : out Unsigned_64) with
        SPARK_Mode => Off
    is
        use type Capabilities.CapabilityType;


        isPublic     : Boolean := False;
        isDeviceInfo : Boolean := False;
        hasCap       : Boolean := False;
    begin
        case arg0 is
            when Sysinfo.FB_WIDTH | Sysinfo.FB_HEIGHT |
                 Sysinfo.FB_PITCH | Sysinfo.FB_BPP |
                 Sysinfo.NUM_CPUS |
                 Sysinfo.REGISTERED_DRIVER =>
                isPublic := True;
            when Sysinfo.NVME_BAR0 | Sysinfo.NVME_DMA_PHYS |
                 Sysinfo.HDA_BAR0 | Sysinfo.HDA_DMA_PHYS |
                 Sysinfo.NET_IOBASE |
                 Sysinfo.MAGIC_RAMDISK_ADDRESS =>
                isDeviceInfo := True;
            when others =>
                null;
        end case;

        if isPublic then
            retval := Sysinfo.getInfo (arg0, arg1);
            return;
        end if;

        -- Kernel-mode threads exempt
        if Process.proctab(callerPID).mode = Process.KERNEL then
            hasCap := True;
        elsif isDeviceInfo then
            -- Physical addresses require CAP_DEVICE_MEM
            for slot in Capabilities.CapabilitySlot loop
                if Process.proctab(callerPID).caps(slot).capType =
                   Capabilities.CAP_DEVICE_MEM
                then
                    hasCap := True;
                    exit;
                end if;
            end loop;
        else
            -- Other non-public queries require CAP_PROCESS+READ
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
        end if;

        if not hasCap then
            Process.IPC.notifySupervisor (
                callerPID,
                IPC_Labels.EVENT_CAP_FAULT,
                SYSCALL_INFO,
                arg0, arg1);
            retval := reterr;
            return;
        end if;

        retval := Sysinfo.getInfo (arg0, arg1);
    end handleInfo;

end Syscall.IPC;
