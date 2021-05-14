-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;
with Filesystem.VFS.Paths;
with Mem_mgr;
with PerCpuData;
with Process;
with Process.IPC;
with Sysinfo;
with TextIO; use TextIO;
with Util;
with Virtmem;

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
        -- print ("SYSCALL: "); print (syscallNum); print (" from pid: "); println (PerCPUData.getCurrentPID);
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

            when SYSCALL_OPEN =>
                retval := open (filenameLen => arg0,
                                filename    => Util.numToAddr(arg1),
                                flags       => arg2,
                                mode        => arg3);

            -- IPC
            when SYSCALL_RECEIVE =>
                declare
                    from : Process.ProcessID with Import, Address => Util.numToAddr(arg1);
                begin
                    retval := Process.IPC.receive (from);
                end;

            when SYSCALL_SEND =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    retval := Process.IPC.send (dest => Process.ProcessID(arg0),
                                                msg  => arg1);
                end if;

            when SYSCALL_REPLY =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    retval := Process.IPC.reply (replyTo  => Process.ProcessID(arg0),
                                                 msg      => arg1);
                end if;

            when SYSCALL_RECEIVE_EVENT =>
                retval := Process.IPC.receiveEvent;

            when SYSCALL_SEND_EVENT =>
                if arg0 > Unsigned_64(Process.ProcessID'Last) then
                    retval := reterr;
                else
                    Process.IPC.sendEvent (dest => Process.ProcessID(arg0),
                                           msg  => arg1);
                    retval := 1;
                end if;
            
            when SYSCALL_INFO =>
                return Sysinfo.getInfo (query  => arg0,
                                        detail => arg1);

            when SYSCALL_REGISTER_DRIVER =>
                return Sysinfo.registerDriver (pid    => PerCPUData.getCurrentPID,
                                               driver => Sysinfo.DriverID(arg0));

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
