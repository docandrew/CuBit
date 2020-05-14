-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Per-CPU Data Region
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;

with Textmode; use Textmode;
with Util;
with Virtmem;
with x86;

package body PerCPUData with
    SPARK_Mode => On
is
    function toSegmentDescriptor is 
        new Ada.Unchecked_Conversion (Unsigned_64, Segment.Descriptor);
    
    procedure setupPerCPUData(cpuNum            : in Natural;
                              cpuData           : in out PerCPUData;
                              cpuDataAddr       : in System.Address;
                              gdtAddr           : in System.Address;
                              gdtPointerAddr    : in System.Address;
                              tssAddr           : in System.Address)
    with
        SPARK_Mode => On
    is
        use segment;

        -- On syscall entry, CPU will clear RFLAGS for each bit set here:
        -- TF, DF, IF, IOPL, AC and NT
        SYSCALL_FLAG_MASK : constant Unsigned_64 := 16#47600#;
    begin
        cpuData.cpuNum := cpuNum;
        cpuData.gdt(GDT_SEGMENT_NULL) := toSegmentDescriptor(0);
        
        cpuData.gdt(GDT_SEGMENT_KERNEL_CODE) := (
            typeField =>        Segment.CODE_EXECUTE_ONLY,
            systemSegment =>    True,
            dpl =>              x86.DPL_KERNEL,
            present =>          True,
            longmode =>         True,
            others => <>
        );
        
        cpuData.gdt(GDT_SEGMENT_KERNEL_DATA) := (
            typeField =>        Segment.DATA_READ_WRITE,
            systemSegment =>    True,
            dpl =>              x86.DPL_KERNEL,
            present =>          True,
            others => <>
        );

        cpuData.gdt(GDT_SEGMENT_USER_CODE) := (
            typeField =>        Segment.CODE_EXECUTE_ONLY,
            systemSegment =>    True,
            dpl =>              x86.DPL_USER,
            present =>          True,
            longmode =>         True,
            others => <>
        );

        cpuData.gdt(GDT_SEGMENT_USER_DATA) := (
            typeField =>        Segment.DATA_READ_WRITE,
            systemSegment =>    True,
            dpl =>              x86.DPL_USER,
            present =>          True,
            others => <>
        );

        -- The TSS is a little funny. Since we use an array of generic
        -- Descriptors in the GDT, we have to divide the 128-bit TSS into
        -- 2 pseudo-descriptors and bit-wrangle them in.
        setupTSS : declare
            tssAddrNum : constant Unsigned_64 := Util.addrToNum(tssAddr);
            highBits   : constant Unsigned_64 := Shift_Right(tssAddrNum, 32);
            lowBits    : Segment.Descriptor;
        begin
            -- not sure if this is true for 64-bit TSS, but in 32-bit TSS,
            -- 0x67 is the minimum limit
            if Segment.TaskSwitchSegment'Size > 16#67# then
                lowBits.limit_0_15 := Segment.TaskSwitchSegment'Size / 8;
            else
                lowBits.limit_0_15 := 16#67#;
            end if;

            lowBits.base_0_15   := 
                Unsigned_16(tssAddrNum and 16#FFFF#);
            lowBits.base_16_23  := 
                Unsigned_8(Shift_Right(tssAddrNum, 16) and 16#FF#);
            lowBits.base_24_31  :=
                Unsigned_8(Shift_Right(tssAddrNum, 24) and 16#FF#);
            
            -- This just happens to have the same type (9) as 64-bit TSS
            lowBits.typeField   := Segment.CODE_EXECUTE_ONLY_ACCESSED;

            lowBits.dpl         := x86.DPL_USER;
            lowBits.present     := True;
            
            cpuData.gdt(GDT_SEGMENT_TSS_0)  := lowBits;
            cpuData.gdt(GDT_SEGMENT_TSS_1)  := toSegmentDescriptor(highBits);
        end setupTSS;

        cpuData.gdt(GDT_SEGMENT_UNUSED) := toSegmentDescriptor(0);
        
        cpuData.gdtPointer.base     := Util.addrToNum(gdtAddr);
        cpuData.gdtPointer.limit    := (cpuData'Size / 8) - 1;

        -- Set this GDT active for this CPU
        x86.lgdt(gdtPointerAddr);

        -- Install the KERNEL_GS_BASE and GS_BASE MSRs. These will get swapgs'd
        -- when the first process to run on this CPU is started. See
        -- interruptReturn in interrupt_handlers.asm
        x86.wrmsr(x86.MSRs.GS_BASE, Util.addrToNum(cpuDataAddr));
        x86.wrmsr(x86.MSRs.KERNEL_GS_BASE, 16#1337d00d#);

        -- Install the TSS
        x86.ltr(GDTOffset'Enum_Rep(GDT_OFFSET_TSS));

        -- Install the user & kernel segments for syscalls
        makeStar : declare
            -- The first part of this is the SYSRET CS and SS. The SYSRET
            -- instruction adds 16 to the selector in bits 48-63 to get
            -- the CS selector, and adds 8 to it to get the SS selector. (low
            -- 2 bits are for CPL 3 that SYSRET returns to, so this is why
            -- we use the GDT_OFFSET_KERNEL_DATA selector for the SYSRET
            -- CS and SS portion of the STAR MSR
            starVal : constant Unsigned_64 :=
                Shift_Left(GDTOffset'Enum_Rep(GDT_OFFSET_KERNEL_DATA) or 3, 48) 
                or
                Shift_Left(GDTOffset'Enum_Rep(GDT_OFFSET_KERNEL_CODE), 32);
        begin            
            --print("loading STAR with"); println(starVal);
            x86.wrmsr(x86.MSRs.STAR, starVal);
        end makeStar;

        x86.wrmsr(x86.MSRs.LSTAR, Util.addrToNum(syscallEntryPoint'Address));

        -- Compatibility-mode not supported (CSTAR), but we'll load it anyway
        x86.wrmsr(x86.MSRs.CSTAR, Util.addrToNum(syscallEntryPoint'Address));

        x86.wrmsr(x86.MSRs.FMASK, SYSCALL_FLAG_MASK);

    end setupPerCPUData;

    -- Other OSes use gs as a segment to get specific values, here we just want
    -- the address from GS that we can use to instantiate the PerCPUData struct
    -- in our SPARK code.
    function getPerCPUDataAddr return System.Address with
        SPARK_Mode => On
    is
        ret : System.Address;
    begin

        Asm("rdgsbase %0",
            Volatile => True,
            Outputs => System.Address'Asm_Output("=r", ret));

        return ret;

    end getPerCPUDataAddr;

    -- We statically allocate per-CPU stacks in boot.asm, and can use simple
    -- arithmetic to determine the secondary stack address given the CPU num.
    function getSecondaryStack return SS_Stack_Ptr with
        SPARK_Mode => Off   -- Unchecked_Conversion
    is
        function toPtr is new Ada.Unchecked_Conversion(Integer_Address, SS_Stack_Ptr);
        
        perCPUAddr      : constant System.Address := getPerCPUDataAddr;
        totalStackSize  : Unsigned_64;
        perCPUStackSize : Unsigned_64;
        cpuSecStack     : Integer_Address;
    begin

        if perCPUAddr = System.Null_Address then
            raise SecondaryStackNotAvailable 
                with "Attempted secondary stack use before per-CPU data is initialized";
        end if;

        getPerCPU : declare
            perCPU : PerCPUData
                with Import, Address => getPerCPUDataAddr;
        begin
            totalStackSize := Unsigned_64(Virtmem.STACK_TOP - Virtmem.STACK_BOTTOM);

            --print(" total Stack Size: "); println(totalStackSize);

            perCPUStackSize := totalStackSize / Config.MAX_CPUS;
            
            --print(" perCPUStackSize: "); println(perCPUStackSize);

            cpuSecStack := 
                Virtmem.STACK_TOP -
                Integer_Address(Unsigned_64(perCPU.cpuNum + 1) * perCPUStackSize);
        
                --print(" Secondary stack: "); print(cpuSecStack); print(" for CPU "); println(perCPU.cpuNum);
        end getPerCPU;

        return toPtr(cpuSecStack);
    end getSecondaryStack;

end PerCPUData;