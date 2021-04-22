-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with System;
with System.Storage_Elements; use System.Storage_Elements;

with Devices;
with Mem_mgr;
with PerCpuData;
with Process;
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
                   count : in Unsigned_64) return Long_Integer with SPARK_Mode => On
    is
    begin
        return 0;
    end read;

    ---------------------------------------------------------------------------
    -- close
    -- @TODO
    ---------------------------------------------------------------------------
    function close (fd : in Descriptors.DescriptorNum) return Long_Integer with SPARK_Mode => On
    is
    begin
        return -1;
    end close;

    ---------------------------------------------------------------------------
    -- execve
    -- @TODO
    ---------------------------------------------------------------------------
    function execve (exename   : in System.Address;
                     args      : in System.Address;
                     env       : in System.Address) return Long_Integer with SPARK_Mode => On
    is
    begin
        return -1;
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
                   mode        : in Unsigned_64) return Long_Integer with SPARK_Mode => On
    is
        type PathType is (ABSOLUTE, RELATIVE);

        drive    : Devices.DriveLetter := Devices.NODRIVE;
        device   : Devices.DeviceID;
        path     : String(1..Natural(filenameLen)) with Address => filename;
        kind     : PathType;
        idx      : Natural := 1;
        nextPos  : Natural := 1;
    begin
        print ("Open file: ");
        printz (filename);
        print (" mode: "); printdln (mode);

        if filenameLen = 0 then
            return -1;
        end if;

        if filenameLen = 1 then
            -- has to be a single-char filename. get it.
            return -1;
        end if;

        -- Are we referencing a drive?
        if path(2) = ':' then
            if path(1) in 'A'..'Z' then
                drive := Devices.DriveLetter'Val (Character'Pos (path(1)) - Character'Pos ('A'));
                print (" drive: "); println (Integer(Devices.DriveLetter'Pos (drive)));
                print (" device ID: "); 
                kind := ABSOLUTE;
            else
                return -1;
            end if;
        end if;

        -- find first '/' if it exists
      
        -- parse file path
        -- see if we're using a drive letter:full path or a relative path.

        -- Walk the directory tree and find the requested file
        -- @TODO cache the directory info to avoid future walks

        -- @TODO
        
        return -1;
    end open;

    ---------------------------------------------------------------------------
    -- write
    ---------------------------------------------------------------------------
    function write (fd    : in Descriptors.DescriptorNum;
                    buf   : in System.Address;
                    count : in Unsigned_64) return Long_Integer with SPARK_Mode => On
    is
        use Descriptors;    -- for '=' comparison
        bytesWritten : Long_Integer := 0;
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
                             return Long_Integer
    is
        oldCR3 : Integer_Address;

        percpu : PerCPUData.PerCPUData with
            Import, Address => PerCPUData.getPerCPUDataAddr;

        retval : Long_Integer := -1;
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

            when SYSCALL_OPEN =>
                retval := open (filenameLen => arg0,
                                filename    => Util.numToAddr(arg1),
                                flags       => arg2,
                                mode        => arg3);

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
