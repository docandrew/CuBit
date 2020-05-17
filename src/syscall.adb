-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2020 Jon Andrew
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with Mem_mgr;
with PerCpuData;
with Serial;
with Textmode; use Textmode;
with Virtmem;
with x86;

package body Syscall is

    ---------------------------------------------------------------------------
    -- We get parameters passed here using the SysV ABI
    ---------------------------------------------------------------------------
    function syscallHandler(arg0,   -- rdi
                            arg1,   -- rsi
                            arg2,   -- rdx
                            arg3,   -- rcx
                            arg4,   -- r8
                            arg5,   -- r9
                            syscallNum : in Unsigned_64)   -- first on stack
                            return Unsigned_64
    is
        oldCR3 : Integer_Address;

        --syscallNum  : constant Unsigned_64 := frame.interruptNumber;
        --arg0        : constant Unsigned_64 := frame.rdi;
        --arg1        : constant Unsigned_64 := frame.rsi;
        --arg2        : constant Unsigned_64 := frame.rdx;
        --arg3        : constant Unsigned_64 := frame.r10;
        --arg4        : constant Unsigned_64 := frame.r8;
        --arg5        : constant Unsigned_64 := frame.r9;

        percpu : PerCPUData.PerCPUData with
            Import, Address => PerCPUData.getPerCPUDataAddr;
    begin
        oldCR3 := x86.getCR3;
        Mem_mgr.switchAddressSpace;
        
        print("Syscall: "); printd(syscallNum);
        print(" from PID: "); println(percpu.runningPID);
        println(" with args: ");
        print("  "); println(arg0);
        print("  "); println(arg1);
        print("  "); println(arg2);
        print("  "); println(arg3);
        print("  "); println(arg4);
        print("  "); println(arg5);

        Virtmem.setActiveP4(oldCR3);
        -- Serial.send(Serial.COM1, 'A');
        -- return from the syscall
        return 16#C4FFE17E#;
    end syscallHandler;

end Syscall;