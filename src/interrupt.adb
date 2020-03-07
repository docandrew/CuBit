-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @description x86-64 interrupt handler routines and interrupt vector setup.
-------------------------------------------------------------------------------
with System.Storage_Elements; use System.Storage_Elements;

with Config;
with InterruptNumbers; use InterruptNumbers;
with Lapic;
with Mem_mgr;
with Pic;
with PerCpuData;
with Process;
--with util;
with Time;
--with x86;

package body Interrupt with
    SPARK_Mode => On,
    Refined_State => (InterruptServiceRoutines => (
            isr0, isr1, isr2, isr3, isr4, isr5, isr6, isr7, isr8, isr9, 
            isr10, isr11, isr12, isr13, isr14, isr16, isr17, isr18, isr19, 
            isr32, isr33, isr34, isr35, isr36, isr37, isr38, isr39, isr40,
            isr41, isr42, isr43, isr44, isr45, isr46, isr47,
            isr127, isr128, isr255),
            SetupState => (intController, lapicAddr),
            IDTState => (idt, idtp))
is

    -- The IDT itself, shared by all CPUs
    idt : IDTType;

    -- Pointer structure for our IDT
    idtp : IDTPointer;

    procedure printRegs(frame : not null access constant stackframe.InterruptStackFrame) is
    begin
        println;
        --print("Exception:  "); println(Integer(frame.interruptNumber));
        print("error code: "); println(frame.errorCode);
        print("rip:    "); println(frame.rip);
        print("rflags: "); println(frame.rflags);
        print("rsp:    "); println(frame.rsp);
        print("cs:     "); println(frame.cs);
        print("ss:     "); println(frame.ss);
        
        print("rax:    "); println(frame.rax);
        print("rbx:    "); println(frame.rbx);
        print("rcx:    "); println(frame.rcx);
        print("rdx:    "); println(frame.rdx);
        print("rsi:    "); println(frame.rsi);
        print("rdi:    "); println(frame.rdi);
        print("r8:     "); println(frame.r8);
        print("r9:     "); println(frame.r9);
        print("r10:    "); println(frame.r10);
        print("r11:    "); println(frame.r11);
        print("r12:    "); println(frame.r12);
        print("r13:    "); println(frame.r13);
        print("r14:    "); println(frame.r14);
        print("r15:    "); println(frame.r15);
    end printRegs;

    ---------------------------------------------------------------------------
    -- eoi - handle end of interrupt, depending on the interrupt source. If
    --  the interrupt controller has not been set up yet, this should not be
    --  called, and will cause a kernel panic if it is.
    ---------------------------------------------------------------------------
    procedure eoi(num : x86Interrupt) with
        SPARK_Mode => On
    is
        NoInterruptController : Exception;
        package myLapic is new lapic(lapicAddr);
    begin
        case intController is
            when LEGACY_PIC =>
                -- for PIC, no action necessary except for hardware IRQs
                if num in TIMER..IDE2 then
                    pic.finishIRQ(num);
                end if;
            when APIC =>
                --print("APIC EOI"); println(Unsigned_32(num));
                myLapic.finishIRQ;
            when X2APIC =>
                null;
            when NONE =>
                println("FATAL: end-of-interrupt called with no interrupt controller.");
                raise NoInterruptController;
        end case;
    end eoi;

    ---------------------------------------------------------------------------
    -- Interrupt Handler - called from interrupt_handlers.asm. Note that this
    --  function needs to know what (PIC, APIC, x2APIC) is generating these
    --  interrupts.
    ---------------------------------------------------------------------------
    procedure interruptHandler(frame : not null access constant stackframe.InterruptStackFrame)
        with SPARK_Mode => On
    is      
        interruptNumber : constant x86Interrupt := x86Interrupt(frame.interruptNumber);
        oldCR3 : Integer_Address;
    begin
        -- Save the old address space
        oldCR3 := x86.getCR3;
        --print("Saving old address space: "); println(oldCR3);

        -- Switch page tables to kernel. If this interrupt happened during user-mode, the TSS
        -- should ensure that the process' kernel stack is being used.
        Mem_mgr.switchAddressSpace;
        --print("In kernel address space: "); println(x86.getCR3);

        case interruptNumber is
            when NO_MATH_COPROCESSOR =>
                -- will want to set that this process uses FP,
                -- during context switch if this flag is set, then
                -- we'll need to change CR0 appropriately and make
                -- sure that FXSAVE, FXRESTORE are used during context
                -- switches for FP processes.
                println("Floating point co-processor used, not enabled");
            when PAGE_FAULT =>
                print("Page Fault at ");
                println(frame.rip);
                handlePageFault(frame.errorCode);
                --eoi(PAGE_FAULT);
                x86.halt;
            when TIMER =>
                -- possible overflow in geologic time scales.
                time.msTicks := time.msTicks + 1;

                if time.msTicks mod config.TIME_SLICE = 0 then
                    -- must finish IRQ first since the yield
                    -- will eventually return to interruptReturn in
                    -- interrupt.asm, not here.
                    --print(".");
                    eoi(TIMER);
                    --pic.finishIRQ(TIMER);

                    if(scheduler.getCurrentPID /= 0 and scheduler.getCurrentPID /= 1) then
                        Process.yield;
                    end if;
                else
                    eoi(TIMER);
                    --pic.finishIRQ(TIMER);
                end if;
            when PS2KEYBOARD =>
                keyboard.readKey;
                eoi(PS2KEYBOARD);
                --pic.finishIRQ(interruptNumber);
            when INVALID .. IDE2 =>
                print("IRQ: "); 
                println(Integer(interruptNumber));
                eoi(interruptNumber);
                --finishIRQ(interruptNumber);
            when SPURIOUS =>
                println("Spurious Interrupt");
            when KERNEL_PANIC =>
                println("KERNEL PANIC!");
                printRegs(frame);
                x86.halt;
            when SYSCALL =>
                print("SYSCALL");
            when others =>
                print("EXCEPTION: "); 
                println(Integer(interruptNumber));
                printRegs(frame);
                x86.halt;   -- never iretq, so interrupts should still be disabled.
        end case;

        -- if we return from this interrupt, put page tables back the way they were.
        -- TODO: check cs to see if we were in user code
        Virtmem.setActiveP4(oldCR3);
    end interruptHandler;

    procedure handlePageFault(err : in Unsigned_64) with
        SPARK_Mode => On
    is
        faultAddr : constant Unsigned_64 := x86.getCR2;

        --currPID         : constant Process.ProcessID := 
        --    PerCPUData.getPerCPUDataAddr... etc.
        --check process memory limits
        
        -- what caused the page fault
        present         : constant Boolean := util.isBitSet(err, 0);
        write           : constant Boolean := util.isBitSet(err, 1);
        userMode        : constant Boolean := util.isBitSet(err, 2);
        reservedWrite   : constant Boolean := util.isBitSet(err, 3);
        nxeViolation    : constant Boolean := util.isBitSet(err, 4);
    begin
        -- handle NXE violations separately.
        if nxeViolation then
            if userMode then
                -- if it was a user process, then kill it.
                println("NXE violation in user process!");
            else
                -- if it was the kernel, we goofed up.
                println("NXE violation in kernel!");
                x86.panic;
            end if;
        end if;

        -- decision tree based on flags set in the error code.
        case present is
            when True =>
                case write is
                    when True =>
                        case userMode is
                            when True =>
                                -- user page-protection wr violation. kill it.
                                print("User page-protection write violation: ");
                                println(faultAddr);
                            when False =>
                                -- kernel page-protection wr violation. we goofed.
                                print("Kernel page-protection write violation: ");
                                println(faultAddr);
                        end case;
                    when False =>
                        case userMode is
                            when True =>
                                -- user page-protection rd violation. kill it.
                                print("User page-protection read violation: ");
                                println(faultAddr);
                            when False =>
                                -- kernel page-protection rd violation. we goofed.
                                print("Kernel page-protection read violation: ");
                                println(faultAddr);
                        end case;
                end case;
            when False =>
                case write is
                    when True =>
                        case userMode is
                            when True =>
                                -- user wrote non-present page. see if it's in their
                                -- allocated range and page in if it is. If it's not,
                                -- then may be a stack overflow or OoM.
                                print("User non-present page write: ");
                                println(faultAddr);
                            when False =>
                                -- kernel wrote non-present page. see if it's something
                                -- that we should have, page it in if it is.
                                print("Kernel non-present page write: ");
                                println(faultAddr);
                        end case;
                    when False =>
                        case userMode is
                            when True =>
                                -- user read non-present page. see if it's in their
                                -- allocated range and page in if it is. If it's not,
                                -- may be a stack overflow or OoM.
                                print("User non-present page read: ");
                                println(faultAddr);
                            when False =>
                                -- kernel read non-present page. see if it's something
                                -- that we should have. see if it's something that we should
                                -- have, page it in if it is.
                                print("Kernel non-present page read: ");
                                println(faultAddr);
                        end case;                    
                end case;
        end case;

    end handlePageFault;

    ---------------------------------------------------------------------------
    -- Install ISRs, load the IDT holding them all.
    ---------------------------------------------------------------------------
    procedure setupIDT with
        SPARK_Mode => Off   -- due to 'Address
    is
    begin
        createIDT;
        idtp := calculateIDTP(idt'Address);
        validIDT := True;
    end setupIDT;

    ---------------------------------------------------------------------------
    -- Wrapper for lidt
    ---------------------------------------------------------------------------
    procedure loadIDT with
        SPARK_Mode => Off   -- due to 'Address
    is
    begin
        x86.lidt(idtp'Address);
    end loadIDT;

    ---------------------------------------------------------------------------
    -- Set interrupt controller used
    ---------------------------------------------------------------------------
    procedure setInterruptController(cont : in InterruptController) with
        SPARK_Mode => On
    is
    begin
        intController := cont;
    end setInterruptController;

    ---------------------------------------------------------------------------
    -- Set LAPIC base address
    ---------------------------------------------------------------------------        
    procedure setLAPICBaseAddress(lapicBase : in virtmem.PhysAddress) with
        SPARK_Mode => On
    is
        use System.Storage_Elements;
    begin
        lapicAddr := To_Address(virtmem.P2V(lapicBase));
    end setLAPICBaseAddress;

    ---------------------------------------------------------------------------
    -- Compose the IDTPointer containing our IDTEntry size and the address to
    -- the IDT itself.
    ---------------------------------------------------------------------------
    function calculateIDTP(idtPtr : in System.Address) return IDTPointer 
        with SPARK_Mode => On
    is
        myidtp : IDTPointer;
    begin
        -- 'Size gives bits, so divide by 8 for bytes of entire IDT structure
        myidtp.size := Unsigned_16((IDTEntry'Size * 256 / 8) - 1);
        myidtp.base := util.addrToNum(idtPtr);
        return myidtp;
    end calculateIDTP;

    ---------------------------------------------------------------------------
    -- Create a single IDT entry
    -- Params:
    --  handler: address of ISR
    --  gdtSelector: GDT selector for our kernel
    --  dpl: descriptor privilege level (Ring 0-3) for interrupt
    ---------------------------------------------------------------------------
    function createIDTEntry(handler : in Unsigned_64;
                            isTrap : in Boolean;
                            gdtSelector : in segment.GDTOffset;
                            dpl : in x86.PrivilegeLevel) return IDTEntry
        with SPARK_Mode => On
    is
        newidt : IDTEntry;
    begin
        newidt.offset1 := Unsigned_16(handler and 16#FFFF#);
        newidt.selector := gdtSelector;
        newidt.istIndex := 0;

        newidt.istrap := isTrap;
        newidt.dpl := dpl;
        newidt.present := True;

        newidt.offset2 := Unsigned_16(Shift_Right(handler, 16) and 16#FFFF#);
        newidt.offset3 := Unsigned_32(Shift_Right(handler, 32) and 16#FFFF_FFFF#);

        return newidt;
    end createIDTEntry;

    ---------------------------------------------------------------------------
    -- Creates IDT Entries for each interrupt handler and puts them in our IDT
    -- Note: We have our GDT kernel segment selector at offset 8 (see boot.asm)
    ---------------------------------------------------------------------------
    procedure createIDT 
        with SPARK_Mode => Off
    is
        use segment;
        use x86;
    begin
        -- Exceptions
        --textmode.print("createIDT ISR0 Address: ");
        --textmode.println(isr0'Address);
        idt(0)  := createIDTEntry(addrToNum(isr0'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(1)  := createIDTEntry(addrToNum(isr1'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(2)  := createIDTEntry(addrToNum(isr2'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(3)  := createIDTEntry(addrToNum(isr3'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(4)  := createIDTEntry(addrToNum(isr4'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(5)  := createIDTEntry(addrToNum(isr5'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(6)  := createIDTEntry(addrToNum(isr6'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(7)  := createIDTEntry(addrToNum(isr7'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(8)  := createIDTEntry(addrToNum(isr8'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(9)  := createIDTEntry(addrToNum(isr9'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(10) := createIDTEntry(addrToNum(isr10'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(11) := createIDTEntry(addrToNum(isr11'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(12) := createIDTEntry(addrToNum(isr12'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(13) := createIDTEntry(addrToNum(isr13'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(14) := createIDTEntry(addrToNum(isr14'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        idt(16) := createIDTEntry(addrToNum(isr16'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(17) := createIDTEntry(addrToNum(isr17'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(18) := createIDTEntry(addrToNum(isr18'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(19) := createIDTEntry(addrToNum(isr19'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- IRQs
        idt(32) := createIDTEntry(addrToNum(isr32'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(33) := createIDTEntry(addrToNum(isr33'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(34) := createIDTEntry(addrToNum(isr34'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(35) := createIDTEntry(addrToNum(isr35'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(36) := createIDTEntry(addrToNum(isr36'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(37) := createIDTEntry(addrToNum(isr37'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(38) := createIDTEntry(addrToNum(isr38'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(39) := createIDTEntry(addrToNum(isr39'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(40) := createIDTEntry(addrToNum(isr40'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(41) := createIDTEntry(addrToNum(isr41'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(42) := createIDTEntry(addrToNum(isr42'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(43) := createIDTEntry(addrToNum(isr43'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(44) := createIDTEntry(addrToNum(isr44'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(45) := createIDTEntry(addrToNum(isr45'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(46) := createIDTEntry(addrToNum(isr46'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(47) := createIDTEntry(addrToNum(isr47'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- Kernel Panic - don't want interrupts to happen here, because we're crashed.
        idt(127) := createIDTEntry(addrToNum(isr127'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- Syscall
        idt(128) := createIDTEntry(addrToNum(isr128'Address), True, GDT_OFFSET_KERNEL_CODE, DPL_USER);

        -- Spurious Vector
        idt(255) := createIDTEntry(addrToNum(isr255'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
    end createIDT;

end interrupt;