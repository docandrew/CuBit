-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
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
with Mem_mgr;
with PerCpuData;
with Process;
with Process.IPC;
with Process.Loader;
with Sysinfo;
with TextIO; use TextIO;
with Time;
with Util;
with Video.VGA;
with Virtmem;
with x86;

-- Bring operators into scope for syscall dispatch
use type Process.MessageTag;
use type Process.ProcessMode;
use type Capabilities.Operations.OperationStatus;

package body Syscall is

    ---------------------------------------------------------------------------
    -- exit
    ---------------------------------------------------------------------------
    procedure exitp (currentPID : in Process.ProcessID) with SPARK_Mode => On
    is
    begin
        Process.kill (currentPID);
    end exitp;

    ---------------------------------------------------------------------------
    -- read
    -- @TODO
    ---------------------------------------------------------------------------
    function read (fd    : in Descriptors.DescriptorNum;
                   buf   : in System.Address;
                   count : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end read;

    ---------------------------------------------------------------------------
    -- close
    -- @TODO
    ---------------------------------------------------------------------------
    function close (fd : in Descriptors.DescriptorNum) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end close;

    ---------------------------------------------------------------------------
    -- execve
    -- @TODO
    ---------------------------------------------------------------------------
    function execve (exename   : in System.Address;
                     args      : in System.Address;
                     env       : in System.Address) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        return 0;
    end execve;

    ---------------------------------------------------------------------------
    -- fork
    -- @Note Debatable whether CuBit will support this.
    ---------------------------------------------------------------------------


    ---------------------------------------------------------------------------
    -- open
    -- @TODO
    ---------------------------------------------------------------------------
    function open (filenameLen : in Unsigned_64;
                   filename    : in System.Address;
                   flags       : in Unsigned_64;
                   mode        : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
    begin
        --return Filesystem.VFS.Paths.open (filenameLen, filename, flags, mode);
        return 0;
    end open;

    ---------------------------------------------------------------------------
    -- write
    ---------------------------------------------------------------------------
    function write (fd    : in Descriptors.DescriptorNum;
                    buf   : in System.Address;
                    count : in Unsigned_64) return Unsigned_64 with SPARK_Mode => On
    is
        use Descriptors;    -- for '=' comparison
        bytesWritten : Unsigned_64 := 0;
        idx : Storage_Offset := 0;
    begin
        -- for testing
        if fd = Descriptors.STDOUT then
            for i in 1 .. count loop
                nextByte: declare
                    c : Character with Import, Address => buf + idx;
                begin
                    print (c);
                    bytesWritten := bytesWritten + 1;
                    idx := idx + 1;
                end nextByte;
            end loop;
        end if;

        return bytesWritten;
    end write;

    ---------------------------------------------------------------------------
    -- handleSpawn
    -- Extracted to its own procedure to keep Process.Loader.load's deep
    -- call chain off syscallHandler's stack frame.
    ---------------------------------------------------------------------------
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

        function toErr is
            new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
        reterr : constant Unsigned_64 := toErr (-1);

        hasCap   : Boolean := False;
        elfAddr  : constant System.Address := Util.numToAddr (arg0);
        elfSize  : constant Storage_Count := Storage_Count (arg1);
        priority : Process.ProcessPriority;
        elfHeader : ELF.ELFFileHeader with Import, Address => elfAddr;
        spawnName : aliased constant String := "spawned" & ASCII.NUL;
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
        elsif not Process.Loader.isValidELF (elfHeader) then
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

        newPID := Process.Loader.load (
            elfHeader    => elfHeader,
            objStart     => elfAddr,
            size         => elfSize,
            strAddr      => spawnName'Address,
            requestedPID => reqPID,
            priority     => priority,
            ppid         => Process.ProcessID (arg5 and 16#FF#));

        if newPID = Process.NO_PROCESS then
            println ("SPAWN: load failed");
            return;
        end if;

        if (arg3 and 1) = 0 then
            Process.resume (newPID);
            print ("SPAWN: started PID ");
        else
            print ("SPAWN: created suspended PID ");
        end if;
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
        function toErr is
            new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
        reterr : constant Unsigned_64 := toErr (-1);

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

        function toErr is
            new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
        reterr : constant Unsigned_64 := toErr (-1);

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

        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_GRANT)
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

        function toErr is
            new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
        reterr : constant Unsigned_64 := toErr (-1);

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

        for slot in Capabilities.CapabilitySlot loop
            if Process.proctab(callerPID).caps(slot).capType =
               Capabilities.CAP_PROCESS and then
               Process.proctab(callerPID).caps(slot).rights(
                   Capabilities.RIGHT_GRANT)
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
    -- We get parameters passed here using the SysV ABI
    ---------------------------------------------------------------------------
    function syscallHandler (arg0,   -- rdi
                             arg1,   -- rsi
                             arg2,   -- rdx
                             arg3,   -- rcx
                             arg4,   -- r8
                             arg5,   -- r9
                             syscallNum : in Unsigned_64)   -- first on stack
                             return Unsigned_64
    is
        oldCR3 : Integer_Address;

        percpu : PerCPUData.PerCPUData with
            Import, Volatile, Address => PerCPUData.getPerCPUDataAddr;

        retval  : Unsigned_64 := 0;
        retval2 : Unsigned_64 := 0;

        function toErr is new Ada.Unchecked_Conversion (Long_Integer, Unsigned_64);
        reterr : constant Unsigned_64 := toErr(-1);
    begin
        --oldCR3 := x86.getCR3;
        -- Dispatch the syscall
        case syscallNum is
            when SYSCALL_EXIT =>
                exitp (percpu.currentPID);

            when SYSCALL_READ =>
                retval := read (fd      => Descriptors.DescriptorNum(arg0),
                                buf     => Util.numToAddr(arg1),
                                count   => arg2);
            
            when SYSCALL_CLOSE =>
                retval := close (fd     => Descriptors.DescriptorNum(arg0));

            when SYSCALL_WRITE =>
                retval := write (fd     => Descriptors.DescriptorNum(arg0),
                                 buf    => Util.numToAddr(arg1),
                                 count  => arg2);

            when SYSCALL_SBRK =>
                sbrkHandler : declare
                    function toNum is new Ada.Unchecked_Conversion (System.Address, Unsigned_64);

                    pid       : constant Process.ProcessID := percpu.currentPID;
                    increment : constant Storage_Count := Storage_Count(arg0);
                    oldEnd    : constant System.Address := Process.proctab(pid).heapEnd;
                    newEnd    : System.Address;
                    storage   : System.Address;
                    curPage   : System.Address;
                    newPage   : System.Address;
                begin
                    if increment = 0 then
                        -- Just return current heap end
                        retval := toNum (oldEnd);
                    else
                        newEnd := oldEnd + increment;

                        -- Allocate pages for any new pages in the expanded range
                        -- Current top page (page containing last byte of current heap)
                        curPage := To_Address (
                            (To_Integer(oldEnd) + Integer_Address(Virtmem.PAGE_SIZE - 1)) and
                            Integer_Address(Virtmem.PAGE_MASK));

                        -- New top page boundary
                        newPage := To_Address (
                            (To_Integer(newEnd) + Integer_Address(Virtmem.PAGE_SIZE - 1)) and
                            Integer_Address(Virtmem.PAGE_MASK));

                        -- Allocate any new pages needed
                        while To_Integer(curPage) < To_Integer(newPage) loop
                            Process.addPage (
                                proc    => Process.proctab(pid),
                                mapTo   => curPage,
                                storage => storage,
                                flags   => Virtmem.PG_USERDATA);
                            curPage := curPage + Virtmem.PAGE_SIZE;
                        end loop;

                        Process.proctab(pid).heapEnd := newEnd;
                        retval := toNum (oldEnd);
                    end if;
                end sbrkHandler;

            -- GETTIME: returns millisecond tick count
            when SYSCALL_GETTIME =>
                retval := Time.msTicks;

            -- SLEEP: arg0 = milliseconds to sleep
            when SYSCALL_SLEEP =>
                if arg0 > 0 and arg0 <= 2147483647 then
                    Process.sleep (Time.Duration(arg0) * Time.Milliseconds);
                end if;
                retval := 0;

            -- MAPFB: Map framebuffer into calling process at 0x6000_0000_0000
            -- Returns: virtual address of mapped framebuffer (or -1 on error)
            when SYSCALL_MAPFB =>
                mapFBHandler : declare
                    procedure mapPageInst is new Virtmem.mapPage (BuddyAllocator.allocFrame);
                    function toNum is new Ada.Unchecked_Conversion (System.Address, Unsigned_64);

                    FB_USER_BASE : constant Integer_Address := 16#0000_6000_0000_0000#;

                    pid      : constant Process.ProcessID := percpu.currentPID;
                    fbSize   : constant Storage_Count := Video.VGA.framebufferSize;
                    numPages : constant Storage_Count :=
                        (fbSize + Virtmem.PAGE_SIZE - 1) / Virtmem.PAGE_SIZE;

                    -- Get the physical address by subtracting LINEAR_BASE from
                    -- the kernel's linear-mapped framebuffer address
                    fbPhys   : constant Virtmem.PhysAddress :=
                        Virtmem.PhysAddress(
                            To_Integer(Video.VGA.framebufferAddr) -
                            Virtmem.LINEAR_BASE);

                    ok : Boolean := True;
                    capAllowed : Boolean;
                begin
                    -- Check CAP_DEVICE_MEM capability
                    Capabilities.Operations.checkDeviceMemAccess (
                        table   => Process.proctab(pid).caps,
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
                                    phys    => fbPhys + Virtmem.PhysAddress(i * Virtmem.PAGE_SIZE),
                                    virt    => FB_USER_BASE + Integer_Address(i * Virtmem.PAGE_SIZE),
                                    flags   => Virtmem.PG_USERIO_WC,
                                    myP4    => Process.addrtab(pid),
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
                end mapFBHandler;

            when SYSCALL_OPEN =>
                retval := open (filenameLen => arg0,
                                filename    => Util.numToAddr(arg1),
                                flags       => arg2,
                                mode        => arg3);

            -- IPC
            -- RECEIVE: RDI=pointer to Message struct (kernel writes full message)
            --          Returns: RAX=sender_pid
            when SYSCALL_RECEIVE =>
                receiveHandler : declare
                    from    : Process.ProcessID;
                    recvMsg : Process.Message;
                    -- User provides pointer to their Message buffer in arg0 (RDI)
                    userMsg : Process.Message with Import, Address => Util.numToAddr(arg0);
                begin
                    Process.IPC.receive (from, recvMsg);
                    userMsg := recvMsg;
                    retval := Unsigned_64(from);
                end receiveHandler;

            -- SEND: RDI=dest_pid, RSI=tag, RDX=w0, RCX=w1, R8=w2, R9=w3
            --       Returns: RAX=reply_tag (as Unsigned_64)
            when SYSCALL_SEND =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    sendHandler : declare
                        function tagToU64 is new Ada.Unchecked_Conversion
                            (Process.MessageTag, Unsigned_64);
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        pid : constant Process.ProcessID := PerCPUData.getCurrentPID;
                        sendMsg  : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, arg5));
                        replyTag : Process.MessageTag;
                    begin
                        replyTag := Process.IPC.send (
                            dest => Process.ProcessID(arg0),
                            msg  => sendMsg);
                        -- Clear reply message (SEND doesn't expose reply words)
                        Process.proctab(pid).replyMsg := Process.NULL_MESSAGE;
                        retval := tagToU64 (replyTag);
                    end sendHandler;
                end if;

            -- REPLY: RDI=dest_pid, RSI=tag, RDX=w0, RCX=w1, R8=w2, R9=w3
            --        Returns: RAX=status
            when SYSCALL_REPLY =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    replyHandler : declare
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        replyMsg : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, arg5));
                    begin
                        retval := Process.IPC.reply (
                            replyTo => Process.ProcessID(arg0),
                            msg     => replyMsg);
                    end replyHandler;
                end if;

            when SYSCALL_RECEIVE_EVENT =>
                receiveEventHandler : declare
                    function tagToU64 is new Ada.Unchecked_Conversion
                        (Process.MessageTag, Unsigned_64);
                    eventMsg : constant Process.Message := Process.IPC.receiveEvent;
                begin
                    retval := tagToU64 (eventMsg.tag);
                end receiveEventHandler;

            -- RECEIVE_EVENT_NB: non-blocking event receive
            -- RDI=pointer to Message struct
            -- Returns: RAX=1 if event found, 0 if not
            when SYSCALL_RECEIVE_EVENT_NB =>
                receiveEventNBHandler : declare
                    eventMsg : Process.Message;
                    found    : Boolean;
                    userMsg  : Process.Message with
                        Import, Address => Util.numToAddr(arg0);
                begin
                    Process.IPC.receiveEventNB (eventMsg, found);
                    if found then
                        userMsg := eventMsg;
                        retval := 1;
                    else
                        retval := 0;
                    end if;
                end receiveEventNBHandler;

            when SYSCALL_SEND_EVENT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    sendEventHandler : declare
                        use type Capabilities.CapabilityType;
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        callerPID : constant Process.ProcessID :=
                            percpu.currentPID;
                        destPID : constant Process.ProcessID :=
                            Process.ProcessID (arg0);
                        hasCap  : Boolean := False;
                        eventMsg : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, arg5));
                    begin
                        -- Kernel-mode threads are exempt
                        if Process.proctab(callerPID).mode =
                           Process.KERNEL
                        then
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
                    end sendEventHandler;
                end if;

            -- CALL: pointer-based send+receive
            -- RDI=dest_pid, RSI=pointer to Message struct (in/out)
            -- On entry: reads message from *RSI
            -- On return: writes reply message to *RSI
            -- Returns: RAX=reply_tag (as Unsigned_64)
            when SYSCALL_CALL =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    callHandler : declare
                        function tagToU64 is new Ada.Unchecked_Conversion
                            (Process.MessageTag, Unsigned_64);

                        userMsg  : Process.Message
                            with Import, Address => Util.numToAddr(arg1);
                        replyTag : Process.MessageTag;
                    begin
                        replyTag := Process.IPC.send (
                            dest => Process.ProcessID(arg0),
                            msg  => userMsg);
                        -- Write full reply (tag + words) back to user buffer
                        userMsg := Process.proctab(PerCPUData.getCurrentPID).replyMsg;
                        Process.proctab(PerCPUData.getCurrentPID).replyMsg := Process.NULL_MESSAGE;
                        retval := tagToU64 (replyTag);
                    end callHandler;
                end if;

            -- RECEIVE_NB: non-blocking receive
            -- RDI=pointer to Message struct
            -- Returns: RAX=sender_pid (0 if no message)
            when SYSCALL_RECEIVE_NB =>
                receiveNBHandler : declare
                    from    : Process.ProcessID;
                    recvMsg : Process.Message;
                    found   : Boolean;
                    userMsg : Process.Message with Import, Address => Util.numToAddr(arg0);
                begin
                    Process.IPC.receiveNB (from, recvMsg, found);
                    if found then
                        userMsg := recvMsg;
                        retval := Unsigned_64(from);
                    else
                        retval := 0;
                    end if;
                end receiveNBHandler;
            
            -- SUBMIT: async non-blocking send
            -- RDI=dest_pid, RSI=tag, RDX=w0, RCX=w1, R8=w2, R9=token
            -- Returns: RAX=1 success, 0 failure
            -- Note: sacrifices w3 to pass token in R9 (3 data words per async msg)
            when SYSCALL_SUBMIT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := 0;
                else
                    submitHandler : declare
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        submitMsg : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, 0));
                        ok : Boolean;
                    begin
                        ok := Process.IPC.submit (
                            dest  => Process.ProcessID(arg0),
                            msg   => submitMsg,
                            token => arg5);
                        if ok then
                            retval := 1;
                        else
                            retval := 0;
                        end if;
                    end submitHandler;
                end if;

            -- WAIT_COMPLETION: block until completions available
            -- RDI=user_buf_ptr, RSI=max, RDX=min
            -- Returns: RAX=count
            when SYSCALL_WAIT_COMPLETION =>
                waitCompletionHandler : declare
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

                    -- Write directly to user buffer to avoid 4KB stack copy
                    Process.IPC.waitCompletion (
                        entries     => userBuf,
                        maxEntries  => effectiveMax,
                        minWait     => effectiveMin,
                        numReturned => numReturned);

                    retval := Unsigned_64(numReturned);
                end waitCompletionHandler;

            -- POLL_COMPLETION: non-blocking single completion check
            -- RDI=user_entry_ptr
            -- Returns: RAX=1 if found, 0 if not
            when SYSCALL_POLL_COMPLETION =>
                pollCompletionHandler : declare
                    found : Boolean;
                    userEntry : Process.CompletionEntry with
                        Import, Address => Util.numToAddr(arg0);
                begin
                    Process.IPC.pollCompletion (userEntry, found);
                    if found then
                        retval := 1;
                    else
                        retval := 0;
                    end if;
                end pollCompletionHandler;

            -- GRANT: create shared memory grant
            -- RDI=grantee_pid, RSI=local_addr, RDX=num_pages, RCX=permission
            -- Returns: RAX=grant_id (16#FFFF_FFFF_FFFF_FFFF# on error)
            -- Requires: CAP_ENDPOINT to grantee PID
            when SYSCALL_GRANT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    grantHandler : declare
                        use type Capabilities.CapabilityType;
                        callerPID : constant Process.ProcessID :=
                            percpu.currentPID;
                        granteePID : constant Process.ProcessID :=
                            Process.ProcessID (arg0);
                        hasCap : Boolean := False;
                        gid : Process.GrantID;
                        perm : Process.GrantPermission;
                        ok : Boolean;
                    begin
                        -- Kernel-mode threads exempt
                        if Process.proctab(callerPID).mode =
                           Process.KERNEL
                        then
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
                    end grantHandler;
                end if;

            -- REVOKE: revoke shared memory grant
            -- RDI=grant_id
            -- Returns: RAX=1 success, 0 failure
            -- Only the original granter can revoke.
            when SYSCALL_REVOKE =>
                if arg0 > Unsigned_64(Process.GrantID'Last) then
                    retval := 0;
                else
                    revokeHandler : declare
                        callerPID : constant Process.ProcessID :=
                            percpu.currentPID;
                        gid : constant Process.GrantID :=
                            Process.GrantID (arg0);
                    begin
                        if Process.proctab(callerPID).grants(gid).granterPID /=
                           callerPID
                        then
                            Process.IPC.notifySupervisor (
                                callerPID,
                                IPC_Labels.EVENT_CAP_FAULT,
                                SYSCALL_REVOKE,
                                arg0, 0);
                            retval := 0;
                        else
                            Process.IPC.revokeGrant (id => gid);
                            retval := 1;
                        end if;
                    end revokeHandler;
                end if;

            when SYSCALL_INFO =>
                -- Public queries (no cap needed): FB dims, NUM_CPUS
                -- Private queries: require CAP_PROCESS + RIGHT_READ
                sysinfoHandler : declare
                    use type Capabilities.CapabilityType;
                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    isPublic  : Boolean := False;
                    hasCap    : Boolean := False;
                begin
                    case arg0 is
                        when Sysinfo.FB_WIDTH | Sysinfo.FB_HEIGHT |
                             Sysinfo.FB_PITCH | Sysinfo.FB_BPP |
                             Sysinfo.NUM_CPUS |
                             Sysinfo.REGISTERED_DRIVER =>
                            isPublic := True;
                        when others =>
                            isPublic := False;
                    end case;

                    if isPublic then
                        return Sysinfo.getInfo (arg0, arg1);
                    end if;

                    -- Kernel-mode threads exempt
                    if Process.proctab(callerPID).mode =
                       Process.KERNEL
                    then
                        hasCap := True;
                    else
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
                        return reterr;
                    end if;

                    return Sysinfo.getInfo (arg0, arg1);
                end sysinfoHandler;

            when SYSCALL_REGISTER_DRIVER =>
                --  Gate ALL driver IDs on CAP_NOTIFICATION with matching ref
                registerDriverCapCheck : declare
                    use type Capabilities.CapabilityType;
                    hasCap : Boolean := False;
                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                begin
                    if arg0 > Unsigned_64 (Sysinfo.DriverID'Last) then
                        return reterr;
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
                        return reterr;
                    end if;
                end registerDriverCapCheck;
                return Sysinfo.registerDriver (pid    => PerCPUData.getCurrentPID,
                                               driver => Sysinfo.DriverID(arg0));

            -- Port I/O syscalls for userspace drivers
            -- Gated by CAP_IOPORT capability check.
            -- RDI=port
            -- Returns: RAX=value (for INB/INW), -1 if denied
            when SYSCALL_INP8 =>
                inbHandler : declare
                    val : Unsigned_8;
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 1, False, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.in8 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end if;
                end inbHandler;

            -- RDI=port, RSI=value
            when SYSCALL_OUTP8 =>
                outbHandler : declare
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 1, True, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.out8 (x86.IOPort(arg0 and 16#FFFF#),
                                  Unsigned_8(arg1 and 16#FF#));
                        retval := 0;
                    end if;
                end outbHandler;

            when SYSCALL_INP16 =>
                inwHandler : declare
                    val : Unsigned_16;
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 2, False, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.in16 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end if;
                end inwHandler;

            -- RDI=port, RSI=value
            when SYSCALL_OUTP16 =>
                outwHandler : declare
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 2, True, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.out16 (x86.IOPort(arg0 and 16#FFFF#),
                                   Unsigned_16(arg1 and 16#FFFF#));
                        retval := 0;
                    end if;
                end outwHandler;

            -- Bulk port I/O: RDI=port, RSI=user_buffer_addr, RDX=word_count
            when SYSCALL_INPS16 =>
                ins16Handler : declare
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, Unsigned_64(arg2) * 2, False,
                        capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.ins16 (x86.IOPort(arg0 and 16#FFFF#),
                                   Util.numToAddr(arg1),
                                   Unsigned_32(arg2));
                        retval := 0;
                    end if;
                end ins16Handler;

            when SYSCALL_OUTPS16 =>
                outs16Handler : declare
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, Unsigned_64(arg2) * 2, True,
                        capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.outs16 (x86.IOPort(arg0 and 16#FFFF#),
                                    Util.numToAddr(arg1),
                                    Unsigned_32(arg2));
                        retval := 0;
                    end if;
                end outs16Handler;

            -- 32-bit port I/O: RDI=port
            when SYSCALL_INP32 =>
                inlHandler : declare
                    val : Unsigned_32;
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 4, False, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.in32 (x86.IOPort(arg0 and 16#FFFF#), val);
                        retval := Unsigned_64(val);
                    end if;
                end inlHandler;

            -- 32-bit port write: RDI=port, RSI=value
            when SYSCALL_OUTP32 =>
                outlHandler : declare
                    capAllowed : Boolean;
                begin
                    Capabilities.Operations.checkPortAccess (
                        Process.proctab(percpu.currentPID).caps,
                        arg0 and 16#FFFF#, 4, True, capAllowed);
                    if not capAllowed then
                        retval := reterr;
                    else
                        x86.out32 (x86.IOPort(arg0 and 16#FFFF#),
                                   Unsigned_32(arg1 and 16#FFFF_FFFF#));
                        retval := 0;
                    end if;
                end outlHandler;

            -- VIRT_TO_PHYS: RDI=virtual address
            -- Returns: physical address, or -1 if unmapped
            -- Requires: CAP_DEVICE_MEM (only DMA drivers need phys addrs)
            when SYSCALL_VIRT_TO_PHYS =>
                vtpHandler : declare
                    use type Capabilities.CapabilityType;
                    pid  : constant Process.ProcessID :=
                        percpu.currentPID;
                    phys : Virtmem.PhysAddress;
                    hasCap : Boolean := False;
                begin
                    -- Kernel-mode threads exempt
                    if Process.proctab(pid).mode = Process.KERNEL then
                        hasCap := True;
                    else
                        for slot in Capabilities.CapabilitySlot loop
                            if Process.proctab(pid).caps(slot).capType =
                               Capabilities.CAP_DEVICE_MEM
                            then
                                hasCap := True;
                                exit;
                            end if;
                        end loop;
                    end if;

                    if not hasCap then
                        Process.IPC.notifySupervisor (
                            pid,
                            IPC_Labels.EVENT_CAP_FAULT,
                            SYSCALL_VIRT_TO_PHYS,
                            arg0, 0);
                        retval := reterr;
                    else
                        phys := Virtmem.tableWalk (
                            Virtmem.VirtAddress(arg0),
                            Process.addrtab(pid));
                        if phys = 0 then
                            retval := reterr;
                        else
                            retval := Unsigned_64(phys) +
                                (arg0 and 16#FFF#);
                        end if;
                    end if;
                end vtpHandler;

            -- CAP_SEND: RDI=cap_slot, RSI=tag, RDX=w0, RCX=w1, R8=w2, R9=w3
            -- Returns: RAX=reply_tag
            when SYSCALL_CAP_SEND =>
                if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                    retval := reterr;
                else
                    capSendHandler : declare
                        function tagToU64 is new Ada.Unchecked_Conversion
                            (Process.MessageTag, Unsigned_64);
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        pid : constant Process.ProcessID := PerCPUData.getCurrentPID;
                        sendMsg : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, arg5));
                        replyTag : Process.MessageTag;
                    begin
                        replyTag := Process.IPC.capSend (
                            capSlot => Capabilities.CapabilitySlot(arg0),
                            msg     => sendMsg);
                        Process.proctab(pid).replyMsg := Process.NULL_MESSAGE;
                        retval := tagToU64 (replyTag);
                    end capSendHandler;
                end if;

            -- CAP_CALL: RDI=cap_slot, RSI=pointer to Message struct (in/out)
            -- Returns: RAX=reply_tag
            when SYSCALL_CAP_CALL =>
                if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                    retval := reterr;
                else
                    capCallHandler : declare
                        function tagToU64 is new Ada.Unchecked_Conversion
                            (Process.MessageTag, Unsigned_64);

                        userMsg  : Process.Message
                            with Import, Address => Util.numToAddr(arg1);
                        replyTag : Process.MessageTag;
                    begin
                        replyTag := Process.IPC.capCall (
                            capSlot => Capabilities.CapabilitySlot(arg0),
                            msg     => userMsg);
                        userMsg := Process.proctab(PerCPUData.getCurrentPID).replyMsg;
                        Process.proctab(PerCPUData.getCurrentPID).replyMsg :=
                            Process.NULL_MESSAGE;
                        retval := tagToU64 (replyTag);
                    end capCallHandler;
                end if;

            -- CAP_SUBMIT: RDI=cap_slot, RSI=tag, RDX=w0, RCX=w1, R8=w2, R9=token
            -- Returns: RAX=1 success, 0 failure
            when SYSCALL_CAP_SUBMIT =>
                if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                    retval := 0;
                else
                    capSubmitHandler : declare
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        submitMsg : constant Process.Message := (
                            tag      => u64ToTag (arg1),
                            capBadge => 0,
                            words    => (arg2, arg3, arg4, 0));
                        ok : Boolean;
                    begin
                        ok := Process.IPC.capSubmit (
                            capSlot => Capabilities.CapabilitySlot(arg0),
                            msg     => submitMsg,
                            token   => arg5);
                        if ok then
                            retval := 1;
                        else
                            retval := 0;
                        end if;
                    end capSubmitHandler;
                end if;

            -- NOTIFY: signal a notification capability
            -- RDI=cap_slot
            -- Returns: RAX=1 success, 0 failure
            when SYSCALL_NOTIFY =>
                if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                    retval := 0;
                else
                    if Process.IPC.capNotify (
                        capSlot => Capabilities.CapabilitySlot(arg0))
                    then
                        retval := 1;
                    else
                        retval := 0;
                    end if;
                end if;

            -- NOTIFY_WAIT: block until notification word is non-zero
            -- Returns: RAX=notification word value
            when SYSCALL_NOTIFY_WAIT =>
                retval := Process.IPC.notifyWait;

            -- NOTIFY_POLL: non-blocking notification check
            -- Returns: RAX=notification word value (0 if none)
            when SYSCALL_NOTIFY_POLL =>
                retval := Process.IPC.notifyPoll;

            -- BIND_NOTIFICATION: bind a notification to the calling process
            -- RDI=notification PID
            -- Returns: RAX=1
            when SYSCALL_BIND_NOTIFICATION =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    Process.IPC.bindNotification (
                        notifPID => Process.ProcessID(arg0));
                    retval := 1;
                end if;

            -- UNBIND_NOTIFICATION: remove notification binding
            -- Returns: RAX=1
            when SYSCALL_UNBIND_NOTIFICATION =>
                Process.IPC.unbindNotification;
                retval := 1;

            -- REPLY_WAIT: atomic reply+receive (seL4 ReplyRecv)
            -- RDI=replyTo PID, RSI=pointer to Message struct (in: reply, out: received)
            -- Returns: RAX=sender PID of next received message
            when SYSCALL_REPLY_WAIT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    replyWaitHandler : declare
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        userMsg  : Process.Message
                            with Import, Address => Util.numToAddr(arg1);
                        replyMsg : constant Process.Message := userMsg;
                        from     : Process.ProcessID;
                        recvMsg  : Process.Message;
                    begin
                        Process.IPC.replyWait (
                            replyTo  => Process.ProcessID(arg0),
                            replyMsg => replyMsg,
                            from     => from,
                            msg      => recvMsg);
                        userMsg := recvMsg;
                        retval := Unsigned_64(from);
                    end replyWaitHandler;
                end if;

            -- CONTROLACCESS: capability table manipulation
            -- RDI=sub-op, RSI..R9=sub-op-specific arguments
            -- Returns: RAX=slot index on success, -1 on error
            when SYSCALL_CONTROLACCESS =>
                controlAccessHandler : declare
                    pid : constant Process.ProcessID := percpu.currentPID;
                    subOp : constant Unsigned_64 := arg0;
                    opStatus : Capabilities.Operations.OperationStatus;
                    slot : Capabilities.CapabilitySlot;
                begin
                    case subOp is
                        -- INSERT: arg1=capType, arg2=rights_bitmask,
                        --         arg3=ref, arg4=param
                        when CONTROLACCESS_INSERT =>
                            insertHandler : declare
                                function u64ToRights is new Ada.Unchecked_Conversion
                                    (Unsigned_8, Capabilities.CapabilityRights);

                                capTypeVal : Capabilities.CapabilityType;
                                newCap : Capabilities.Capability;
                            begin
                                if arg1 > Capabilities.CapabilityType'Pos(
                                    Capabilities.CapabilityType'Last) then
                                    retval := reterr;
                                else
                                    capTypeVal := Capabilities.CapabilityType'Val(
                                        Natural(arg1));
                                    newCap := (
                                        capType  => capTypeVal,
                                        rights   => u64ToRights(Unsigned_8(arg2 and 16#FF#)),
                                        capBadge => Capabilities.NO_BADGE,
                                        object   => (ref => arg3, param => arg4),
                                        gen      => Capabilities.INITIAL_GENERATION);
                                    Capabilities.Operations.insertCap (
                                        table  => Process.proctab(pid).caps,
                                        cap    => newCap,
                                        slot   => slot,
                                        status => opStatus);
                                    if opStatus = Capabilities.Operations.OP_OK then
                                        retval := Unsigned_64(slot);
                                    else
                                        retval := reterr;
                                    end if;
                                end if;
                            end insertHandler;

                        -- DERIVE: arg1=source_slot, arg2=new_rights_bitmask,
                        --         arg3=dest_slot (0=auto)
                        when CONTROLACCESS_DERIVE =>
                            deriveHandler : declare
                                function u64ToRights is new Ada.Unchecked_Conversion
                                    (Unsigned_8, Capabilities.CapabilityRights);

                                srcCap  : Capabilities.Capability;
                                newCap  : Capabilities.Capability;
                                newRights : Capabilities.CapabilityRights;
                            begin
                                if arg1 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                    retval := reterr;
                                else
                                    Capabilities.Operations.lookupCap (
                                        table  => Process.proctab(pid).caps,
                                        slot   => Capabilities.CapabilitySlot(arg1),
                                        cap    => srcCap,
                                        status => opStatus);

                                    if opStatus /= Capabilities.Operations.OP_OK then
                                        retval := reterr;
                                    else
                                        newRights := u64ToRights(Unsigned_8(arg2 and 16#FF#));

                                        if not Capabilities.isSubsetOf (newRights, srcCap.rights) then
                                            retval := reterr;
                                        else
                                            newCap := Capabilities.derive (srcCap, newRights);

                                            if arg3 /= 0 and then
                                               arg3 <= Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                                Capabilities.Operations.insertCapAt (
                                                    table => Process.proctab(pid).caps,
                                                    slot  => Capabilities.CapabilitySlot(arg3),
                                                    cap   => newCap);
                                                retval := arg3;
                                            else
                                                Capabilities.Operations.insertCap (
                                                    table  => Process.proctab(pid).caps,
                                                    cap    => newCap,
                                                    slot   => slot,
                                                    status => opStatus);
                                                if opStatus = Capabilities.Operations.OP_OK then
                                                    retval := Unsigned_64(slot);
                                                else
                                                    retval := reterr;
                                                end if;
                                            end if;
                                        end if;
                                    end if;
                                end if;
                            end deriveHandler;

                        -- MINT: arg1=source_slot, arg2=new_badge,
                        --       arg3=rights_bitmask, arg4=dest_slot (0=auto)
                        when CONTROLACCESS_MINT =>
                            mintHandler : declare
                                function u64ToRights is new Ada.Unchecked_Conversion
                                    (Unsigned_8, Capabilities.CapabilityRights);

                                srcCap    : Capabilities.Capability;
                                newCap    : Capabilities.Capability;
                                newRights : Capabilities.CapabilityRights;
                            begin
                                if arg1 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                    retval := reterr;
                                else
                                    Capabilities.Operations.lookupCap (
                                        table  => Process.proctab(pid).caps,
                                        slot   => Capabilities.CapabilitySlot(arg1),
                                        cap    => srcCap,
                                        status => opStatus);

                                    if opStatus /= Capabilities.Operations.OP_OK then
                                        retval := reterr;
                                    else
                                        newRights := u64ToRights(Unsigned_8(arg3 and 16#FF#));

                                        if not Capabilities.isSubsetOf (newRights, srcCap.rights) then
                                            retval := reterr;
                                        else
                                            newCap := Capabilities.mint (srcCap, arg2, newRights);

                                            if arg4 /= 0 and then
                                               arg4 <= Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                                Capabilities.Operations.insertCapAt (
                                                    table => Process.proctab(pid).caps,
                                                    slot  => Capabilities.CapabilitySlot(arg4),
                                                    cap   => newCap);
                                                retval := arg4;
                                            else
                                                Capabilities.Operations.insertCap (
                                                    table  => Process.proctab(pid).caps,
                                                    cap    => newCap,
                                                    slot   => slot,
                                                    status => opStatus);
                                                if opStatus = Capabilities.Operations.OP_OK then
                                                    retval := Unsigned_64(slot);
                                                else
                                                    retval := reterr;
                                                end if;
                                            end if;
                                        end if;
                                    end if;
                                end if;
                            end mintHandler;

                        -- REMOVE: arg1=slot
                        when CONTROLACCESS_REMOVE =>
                            if arg1 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                retval := reterr;
                            else
                                Capabilities.Operations.removeCap (
                                    table  => Process.proctab(pid).caps,
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
                            if arg1 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                                retval := reterr;
                            else
                                Capabilities.Operations.removeCap (
                                    table  => Process.proctab(pid).caps,
                                    slot   => Capabilities.CapabilitySlot(arg1),
                                    status => opStatus);
                                if opStatus = Capabilities.Operations.OP_OK then
                                    retval := Unsigned_64(arg1);
                                else
                                    retval := reterr;
                                end if;
                            end if;

                        -- REVOKE_ALL: bump this process' generation counter,
                        -- invalidating all caps held by other processes that
                        -- reference this process.
                        when CONTROLACCESS_REVOKE_ALL =>
                            if Process.proctab(pid).capGeneration <
                               Capabilities.Generation'Last
                            then
                                Process.proctab(pid).capGeneration :=
                                    Process.proctab(pid).capGeneration + 1;
                                retval := Unsigned_64(
                                    Process.proctab(pid).capGeneration);
                            else
                                retval := reterr;
                            end if;

                        when others =>
                            retval := reterr;
                    end case;
                end controlAccessHandler;

            -- GETTICKET: read capability from slot
            -- RDI=slot, RSI=pointer to user-space Capability buffer
            -- Returns: RAX=1 on success, 0 on error
            when SYSCALL_GETTICKET =>
                getTicketHandler : declare
                    pid : constant Process.ProcessID := percpu.currentPID;
                    cap : Capabilities.Capability;
                    opStatus : Capabilities.Operations.OperationStatus;
                    userCap : Capabilities.Capability with
                        Import, Address => Util.numToAddr(arg1);
                begin
                    if arg0 > Unsigned_64(Capabilities.CapabilitySlot'Last) then
                        retval := 0;
                    else
                        Capabilities.Operations.lookupCap (
                            table  => Process.proctab(pid).caps,
                            slot   => Capabilities.CapabilitySlot(arg0),
                            cap    => cap,
                            status => opStatus);

                        if opStatus = Capabilities.Operations.OP_OK then
                            userCap := cap;
                            retval := 1;
                        else
                            retval := 0;
                        end if;
                    end if;
                end getTicketHandler;

            -- SPAWN: create a new process from an ELF image in caller's memory
            -- RDI=virtual address of ELF image, RSI=size in bytes, RDX=priority
            -- Returns: RAX=new PID, or -1 on error
            -- Requires: CAP_PROCESS with RIGHT_EXECUTE
            -- SPAWN: create a new process from an ELF binary
            -- arg0 = ELF address, arg1 = ELF size, arg2 = priority,
            -- arg3 = flags (bit 0 = SPAWN_SUSPENDED),
            -- arg4 = requested PID (0 = auto-assign)
            -- Returns: new PID, or -1 on error
            -- Requires: CAP_PROCESS with RIGHT_EXECUTE
            when SYSCALL_SPAWN =>
                handleSpawn (percpu.currentPID,
                             arg0, arg1, arg2, arg3, arg4, arg5, retval);

            -- MAP_DEVICE: map device MMIO into calling process' address space
            -- arg0 = physical address, arg1 = virtual address, arg2 = number of pages
            -- Returns: 0 on success, -1 on failure
            -- Requires: CAP_DEVICE_MEM covering [physAddr, physAddr + numPages*4096)
            when SYSCALL_MAP_DEVICE =>
                handleMapDevice (percpu.currentPID,
                                 arg0, arg1, arg2, retval);

            -- PROCLIST: write process table entries into user buffer
            -- arg0 = buffer address, arg1 = buffer size in bytes
            -- Returns: number of entries written
            -- Requires: CAP_PROCESS with RIGHT_READ
            when SYSCALL_PROCLIST =>
                proclistCapCheck : declare
                    use type Capabilities.CapabilityType;
                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    hasCap : Boolean := False;
                begin
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
                    end if;
                end proclistCapCheck;

                if retval /= reterr then
                proclistHandler : declare
                    use type Process.ProcessState;

                    function priToU16 is new Ada.Unchecked_Conversion
                        (Integer_16, Unsigned_16);

                    bufAddr : constant System.Address := Util.numToAddr (arg0);
                    bufSize : constant Unsigned_64 := arg1;
                    ENTRY_SIZE : constant := 32;
                    maxEntries : Unsigned_64;
                    count : Unsigned_64 := 0;
                begin
                    if bufSize < ENTRY_SIZE then
                        retval := 0;
                    else
                        maxEntries := bufSize / ENTRY_SIZE;

                        for i in Process.proctab'Range loop
                            exit when count >= maxEntries;

                            if Process.proctab(i).state /= Process.INVALID then
                                declare
                                    offset : constant Storage_Offset :=
                                        Storage_Offset (count * ENTRY_SIZE);
                                    entryAddr : constant System.Address :=
                                        bufAddr + offset;

                                    -- PID (2 bytes)
                                    pidVal : Unsigned_16 with
                                        Import, Address => entryAddr;
                                    -- State (1 byte)
                                    stateVal : Unsigned_8 with
                                        Import, Address => entryAddr + 2;
                                    -- CPU (1 byte)
                                    cpuVal : Unsigned_8 with
                                        Import, Address => entryAddr + 3;
                                    -- Priority (2 bytes, signed stored as unsigned)
                                    priVal : Unsigned_16 with
                                        Import, Address => entryAddr + 4;
                                    -- Pad (2 bytes at offset 6)
                                    padVal : Unsigned_16 with
                                        Import, Address => entryAddr + 6;
                                    -- Name (16 bytes at offset 8)
                                    nameField : String (1 .. 16) with
                                        Import, Address => entryAddr + 8;
                                    -- Reserved (8 bytes at offset 24)
                                    reservedField : Unsigned_64 with
                                        Import, Address => entryAddr + 24;
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
                                    reservedField := 0;
                                    count := count + 1;
                                end;
                            end if;
                        end loop;

                        retval := count;
                    end if;
                end proclistHandler;
                end if;

            -- MINT_CAP: insert a capability into a target process' cap table
            -- arg0 = target PID, arg1 = cap type (CapabilityType'Pos),
            -- arg2 = object ref, arg3 = object param,
            -- arg4 = rights bitmask (bit0=R,1=W,2=X,3=GRANT,4=REVOKE),
            -- arg5 = slot number
            -- Requires: CAP_PROCESS with RIGHT_GRANT for target PID
            when SYSCALL_MINT_CAP =>
                mintCapHandler : declare
                    use type Capabilities.CapabilityType;
                    use type Process.ProcessState;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    targetPID : Process.ProcessID;
                    hasCap    : Boolean := False;
                    capTypePos : Natural;
                    newCap    : Capabilities.Capability;
                    newRights : Capabilities.CapabilityRights;
                    targetSlot : Capabilities.CapabilitySlot;
                begin
                    -- Validate target PID range
                    if arg0 > Unsigned_64 (Process.ProcessID'Last) or
                       arg0 = 0
                    then
                        println ("MINT_CAP: invalid target PID");
                        retval := reterr;
                    else
                        targetPID := Process.ProcessID (arg0);

                        -- Check caller has CAP_PROCESS with RIGHT_GRANT
                        for slot in Capabilities.CapabilitySlot loop
                            if Process.proctab(callerPID).caps(slot).capType =
                               Capabilities.CAP_PROCESS and then
                               Process.proctab(callerPID).caps(slot).rights(
                                   Capabilities.RIGHT_GRANT)
                            then
                                hasCap := True;
                                exit;
                            end if;
                        end loop;

                        if not hasCap then
                            println ("MINT_CAP: denied, no RIGHT_GRANT");
                            retval := reterr;
                        elsif Process.proctab(targetPID).state =
                              Process.INVALID
                        then
                            println ("MINT_CAP: target not valid");
                            retval := reterr;
                        elsif arg5 >
                              Unsigned_64 (Capabilities.CapabilitySlot'Last)
                        then
                            println ("MINT_CAP: invalid slot");
                            retval := reterr;
                        elsif arg1 >
                              Unsigned_64 (Capabilities.CapabilityType'Pos (
                                  Capabilities.CapabilityType'Last))
                        then
                            println ("MINT_CAP: invalid cap type");
                            retval := reterr;
                        else
                            targetSlot :=
                                Capabilities.CapabilitySlot (arg5);
                            capTypePos := Natural (arg1);

                            -- Build rights from bitmask
                            newRights := (
                                Capabilities.RIGHT_READ    =>
                                    (arg4 and 1) /= 0,
                                Capabilities.RIGHT_WRITE   =>
                                    (arg4 and 2) /= 0,
                                Capabilities.RIGHT_EXECUTE =>
                                    (arg4 and 4) /= 0,
                                Capabilities.RIGHT_GRANT   =>
                                    (arg4 and 8) /= 0,
                                Capabilities.RIGHT_REVOKE  =>
                                    (arg4 and 16) /= 0);

                            -- Build capability.
                            -- For endpoint caps, badge = target PID so the
                            -- server can identify the caller on capCall.
                            newCap := (
                                capType  => Capabilities.CapabilityType'Val (
                                    capTypePos),
                                rights   => newRights,
                                capBadge => Unsigned_64 (targetPID),
                                object   => (ref   => arg2,
                                             param => arg3),
                                gen      => Capabilities.INITIAL_GENERATION);

                            Capabilities.Operations.insertCapAt (
                                table => Process.proctab(targetPID).caps,
                                slot  => targetSlot,
                                cap   => newCap);

                            retval := 0;
                        end if;
                    end if;
                end mintCapHandler;

            -- RESUME: resume a suspended process
            -- arg0 = target PID
            -- Requires: CAP_PROCESS with RIGHT_EXECUTE for target
            when SYSCALL_RESUME =>
                resumeHandler : declare
                    use type Capabilities.CapabilityType;
                    use type Process.ProcessState;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    targetPID : Process.ProcessID;
                    hasCap    : Boolean := False;
                begin
                    if arg0 > Unsigned_64 (Process.ProcessID'Last) or
                       arg0 = 0
                    then
                        println ("RESUME: invalid target PID");
                        retval := reterr;
                    else
                        targetPID := Process.ProcessID (arg0);

                        -- Check caller has CAP_PROCESS with RIGHT_EXECUTE
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
                            println ("RESUME: denied, no RIGHT_EXECUTE");
                            retval := reterr;
                        elsif Process.proctab(targetPID).state /=
                              Process.SUSPENDED
                        then
                            println ("RESUME: target not suspended");
                            retval := reterr;
                        else
                            Process.resume (targetPID);
                            print ("RESUME: resumed PID ");
                            println (Integer (targetPID));
                            retval := 0;
                        end if;
                    end if;
                end resumeHandler;

            -- ALLOC_DMA: allocate contiguous physical pages, map into target
            -- arg0 = targetPID, arg1 = order, arg2 = virtBase
            -- Returns: physical address, or -1 on error
            -- Requires: CAP_PROCESS with RIGHT_GRANT
            when SYSCALL_ALLOC_DMA =>
                handleAllocDma (percpu.currentPID,
                                arg0, arg1, arg2, retval);

            -- ENABLE_IRQ: enable IOAPIC routing and register IRQ owner
            -- arg0 = vector, arg1 = ownerPID, arg2 = target CPU
            -- Returns: 0 on success, -1 on error
            -- Requires: CAP_PROCESS with RIGHT_GRANT
            when SYSCALL_ENABLE_IRQ =>
                enableIrqHandler : declare
                    use type Capabilities.CapabilityType;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    hasCap    : Boolean := False;
                    irqOk     : Boolean;
                begin
                    for slot in Capabilities.CapabilitySlot loop
                        if Process.proctab(callerPID).caps(slot).capType =
                           Capabilities.CAP_PROCESS and then
                           Process.proctab(callerPID).caps(slot).rights(
                               Capabilities.RIGHT_GRANT)
                        then
                            hasCap := True;
                            exit;
                        end if;
                    end loop;

                    if not hasCap then
                        println ("ENABLE_IRQ: denied, no RIGHT_GRANT");
                        retval := reterr;
                    elsif arg0 > 255 then
                        println ("ENABLE_IRQ: invalid vector");
                        retval := reterr;
                    elsif arg1 > Unsigned_64 (Process.ProcessID'Last) or
                          arg1 = 0
                    then
                        println ("ENABLE_IRQ: invalid owner PID");
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
                end enableIrqHandler;

            -- MAP_INTO: map physical pages into a target process
            -- arg0 = targetPID, arg1 = physical address,
            -- arg2 = virtual address, arg3 = number of pages,
            -- arg4 = flags (0=USERDATA, 1=USERDATARO, 2=USERIO)
            -- Returns: 0 on success, -1 on error
            -- Requires: CAP_PROCESS with RIGHT_GRANT
            when SYSCALL_MAP_INTO =>
                handleMapInto (percpu.currentPID,
                               arg0, arg1, arg2, arg3, arg4, retval);

            -- SET_SYSINFO: set a sysinfo value from userspace
            -- arg0 = queryID, arg1 = value
            -- Returns: 0 on success, -1 on error
            -- Requires: CAP_PROCESS with RIGHT_WRITE
            when SYSCALL_SET_SYSINFO =>
                setSysinfoHandler : declare
                    use type Capabilities.CapabilityType;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    hasCap    : Boolean := False;
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
                end setSysinfoHandler;

            -- SET_CPU: set CPU affinity for a process
            -- arg0 = targetPID, arg1 = CPU number
            -- Returns: 0 on success, -1 on error
            -- Requires: CAP_PROCESS with RIGHT_GRANT
            when SYSCALL_SET_CPU =>
                setCpuHandler : declare
                    use type Capabilities.CapabilityType;
                    use type Process.ProcessState;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    targetPID : Process.ProcessID;
                    hasCap    : Boolean := False;
                begin
                    if arg0 > Unsigned_64 (Process.ProcessID'Last) or
                       arg0 = 0
                    then
                        retval := reterr;
                    elsif arg1 >= Unsigned_64 (acpi.numCPUs) then
                        println ("SET_CPU: CPU number out of range");
                        retval := reterr;
                    else
                        targetPID := Process.ProcessID (arg0);

                        for slot in Capabilities.CapabilitySlot loop
                            if Process.proctab(callerPID).caps(slot).capType =
                               Capabilities.CAP_PROCESS and then
                               Process.proctab(callerPID).caps(slot).rights(
                                   Capabilities.RIGHT_GRANT)
                            then
                                hasCap := True;
                                exit;
                            end if;
                        end loop;

                        if not hasCap then
                            println ("SET_CPU: denied, no RIGHT_GRANT");
                            retval := reterr;
                        elsif Process.proctab(targetPID).state =
                              Process.INVALID
                        then
                            println ("SET_CPU: target not valid");
                            retval := reterr;
                        else
                            Process.proctab(targetPID).cpu :=
                                Natural (arg1);
                            retval := 0;
                        end if;
                    end if;
                end setCpuHandler;

            -- SET_SUPERVISOR: reassign supervisor for a process
            -- arg0 = target PID, arg1 = new supervisor PID
            -- Returns: 0 on success, -1 on error
            -- Requires: CAP_PROCESS with RIGHT_GRANT
            when SYSCALL_SET_SUPERVISOR =>
                setSupervisorHandler : declare
                    use type Capabilities.CapabilityType;
                    use type Process.ProcessState;

                    callerPID : constant Process.ProcessID :=
                        percpu.currentPID;
                    targetPID : Process.ProcessID;
                    newSvPID  : Process.ProcessID;
                    hasCap    : Boolean := False;
                begin
                    if arg0 > Unsigned_64 (Process.ProcessID'Last) or
                       arg0 = 0
                    then
                        retval := reterr;
                    elsif arg1 > Unsigned_64 (Process.ProcessID'Last) then
                        retval := reterr;
                    else
                        targetPID := Process.ProcessID (arg0);
                        newSvPID  := Process.ProcessID (arg1);

                        for slot in Capabilities.CapabilitySlot loop
                            if Process.proctab(callerPID).caps(slot).capType =
                               Capabilities.CAP_PROCESS and then
                               Process.proctab(callerPID).caps(slot).rights(
                                   Capabilities.RIGHT_GRANT)
                            then
                                hasCap := True;
                                exit;
                            end if;
                        end loop;

                        if not hasCap then
                            null;  -- notifySupervisor below
                            retval := reterr;
                        elsif Process.proctab(targetPID).state =
                              Process.INVALID
                        then
                            println ("SET_SUPERVISOR: target invalid");
                            retval := reterr;
                        else
                            Process.proctab(targetPID).svpid := newSvPID;
                            retval := 0;
                        end if;
                    end if;
                end setSupervisorHandler;

            when others =>
                print ("Syscall: "); printd (syscallNum);
                print (" from PID: "); println (percpu.currentPID);
                println (" with args: ");
                print ("  "); println (arg0);
                print ("  "); println (arg1);
                print ("  "); println (arg2);
                print ("  "); println (arg3);
                print ("  "); println (arg4);
                print ("  "); println (arg5);
        end case;

        return retval;
    end syscallHandler;


end Syscall;
