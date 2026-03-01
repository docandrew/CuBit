-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with BuddyAllocator;
with Devices;
with Filesystem.VFS.Paths;
with Mem_mgr;
with PerCpuData;
with Process;
with Process.IPC;
with Sysinfo;
with TextIO; use TextIO;
with Time;
with Util;
with Video.VGA;
with Virtmem;
with x86;

-- Bring MessageTag operators into scope for syscall dispatch
use type Process.MessageTag;

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
            Import, Address => PerCPUData.getPerCPUDataAddr;

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
                begin
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
                            tag   => u64ToTag (arg1),
                            words => (arg2, arg3, arg4, arg5));
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
                            tag   => u64ToTag (arg1),
                            words => (arg2, arg3, arg4, arg5));
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

            when SYSCALL_SEND_EVENT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    sendEventHandler : declare
                        function u64ToTag is new Ada.Unchecked_Conversion
                            (Unsigned_64, Process.MessageTag);

                        eventMsg : constant Process.Message := (
                            tag   => u64ToTag (arg1),
                            words => (arg2, arg3, arg4, arg5));
                    begin
                        Process.IPC.sendEvent (
                            dest => Process.ProcessID(arg0),
                            msg  => eventMsg);
                        retval := 1;
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
                            tag   => u64ToTag (arg1),
                            words => (arg2, arg3, arg4, 0));
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
                    entries : Process.CompletionRing;
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

                    Process.IPC.waitCompletion (
                        entries     => entries,
                        maxEntries  => effectiveMax,
                        minWait     => effectiveMin,
                        numReturned => numReturned);

                    -- Copy results to user buffer
                    for i in 0 .. numReturned - 1 loop
                        userBuf(i) := entries(i);
                    end loop;

                    retval := Unsigned_64(numReturned);
                end waitCompletionHandler;

            -- POLL_COMPLETION: non-blocking single completion check
            -- RDI=user_entry_ptr
            -- Returns: RAX=1 if found, 0 if not
            when SYSCALL_POLL_COMPLETION =>
                pollCompletionHandler : declare
                    pollResult : Process.CompletionEntry;
                    found : Boolean;
                    userEntry : Process.CompletionEntry with
                        Import, Address => Util.numToAddr(arg0);
                begin
                    Process.IPC.pollCompletion (pollResult, found);
                    if found then
                        userEntry := pollResult;
                        retval := 1;
                    else
                        retval := 0;
                    end if;
                end pollCompletionHandler;

            -- GRANT: create shared memory grant
            -- RDI=grantee_pid, RSI=local_addr, RDX=num_pages, RCX=permission
            -- Returns: RAX=grant_id (16#FFFF_FFFF_FFFF_FFFF# on error)
            when SYSCALL_GRANT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    grantHandler : declare
                        gid : Process.GrantID;
                        perm : Process.GrantPermission;
                        ok : Boolean;
                    begin
                        if arg3 = 1 then
                            perm := Process.GRANT_READWRITE;
                        else
                            perm := Process.GRANT_READ;
                        end if;

                        Process.IPC.createGrant (
                            grantee   => Process.ProcessID(arg0),
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
                    end grantHandler;
                end if;

            -- REVOKE: revoke shared memory grant
            -- RDI=grant_id
            -- Returns: RAX=1 success, 0 failure
            when SYSCALL_REVOKE =>
                if arg0 > Unsigned_64(Process.GrantID'Last) then
                    retval := 0;
                else
                    Process.IPC.revokeGrant (id => Process.GrantID(arg0));
                    retval := 1;
                end if;

            when SYSCALL_INFO =>
                return Sysinfo.getInfo (query  => arg0,
                                        detail => arg1);

            when SYSCALL_REGISTER_DRIVER =>
                return Sysinfo.registerDriver (pid    => PerCPUData.getCurrentPID,
                                               driver => Sysinfo.DriverID(arg0));

            -- Port I/O syscalls for userspace drivers
            -- TODO: Add IOPB permission checks per-process
            -- RDI=port
            -- Returns: RAX=value (for INB/INW)
            when SYSCALL_INB =>
                inbHandler : declare
                    val : Unsigned_8;
                begin
                    x86.in8 (x86.IOPort(arg0 and 16#FFFF#), val);
                    retval := Unsigned_64(val);
                end inbHandler;

            -- RDI=port, RSI=value
            when SYSCALL_OUTB =>
                x86.out8 (x86.IOPort(arg0 and 16#FFFF#), Unsigned_8(arg1 and 16#FF#));
                retval := 0;

            when SYSCALL_INW =>
                inwHandler : declare
                    val : Unsigned_16;
                begin
                    x86.in16 (x86.IOPort(arg0 and 16#FFFF#), val);
                    retval := Unsigned_64(val);
                end inwHandler;

            -- RDI=port, RSI=value
            when SYSCALL_OUTW =>
                x86.out16 (x86.IOPort(arg0 and 16#FFFF#), Unsigned_16(arg1 and 16#FFFF#));
                retval := 0;

            -- Bulk port I/O: RDI=port, RSI=user_buffer_addr, RDX=word_count
            when SYSCALL_INS16 =>
                x86.ins16 (x86.IOPort(arg0 and 16#FFFF#),
                           Util.numToAddr(arg1),
                           Unsigned_32(arg2));
                retval := 0;

            when SYSCALL_OUTS16 =>
                x86.outs16 (x86.IOPort(arg0 and 16#FFFF#),
                            Util.numToAddr(arg1),
                            Unsigned_32(arg2));
                retval := 0;

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
