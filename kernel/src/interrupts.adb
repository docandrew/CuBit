-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @description x86-64 interrupt handler routines and interrupt vector setup.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

with Capabilities.IRQ;
with Config;
with ioapic;
with IPC_Labels;
with InterruptNumbers; use InterruptNumbers;
with Lapic;
with Mem_mgr;
with Pic;
with PerCpuData;
with Process;
with Process.IPC;
with Serial;
with TextIO; use TextIO;
with Time;
with Virtmem;

package body Interrupts with
    SPARK_Mode => On
    -- Refined_State => (InterruptServiceRoutines => (
    --         isr0, isr1, isr2, isr3, isr4, isr5, isr6, isr7, isr8, isr9, 
    --         isr10, isr11, isr12, isr13, isr14, isr16, isr17, isr18, isr19, 
    --         isr32, isr33, isr34, isr35, isr36, isr37, isr38, isr39, isr40,
    --         isr41, isr42, isr43, isr44, isr45, isr46, isr47,
    --         isr127, isr128, isr255),
    --         IDTState => (idt, idtp))
is

    -- The IDT itself, shared by all CPUs
    idt : IDTType;

    -- Pointer structure for our IDT
    idtp : IDTPointer;

    ---------------------------------------------------------------------------
    -- printRegs
    ---------------------------------------------------------------------------
    procedure printRegs(frame : not null access constant Stackframe.InterruptStackFrame) is
    begin
        println;
        --print("Exception:  "); println(Integer(frame.interruptNumber));
        print ("error code: "); println (frame.errorCode);
        print ("rip:    ");     println (frame.rip);
        print ("rflags: ");     println (frame.rflags);
        print ("rsp:    ");     println (frame.rsp);
        print ("rbp:    ");     println (frame.rbp);
        print ("cs:     ");     println (frame.cs);
        print ("ss:     ");     println (frame.ss);
        
        print ("rax:    ");     println (frame.rax);
        print ("rbx:    ");     println (frame.rbx);
        print ("rcx:    ");     println (frame.rcx);
        print ("rdx:    ");     println (frame.rdx);
        print ("rsi:    ");     println (frame.rsi);
        print ("rdi:    ");     println (frame.rdi);
        print ("r8:     ");     println (frame.r8);
        print ("r9:     ");     println (frame.r9);
        print ("r10:    ");     println (frame.r10);
        print ("r11:    ");     println (frame.r11);
        print ("r12:    ");     println (frame.r12);
        print ("r13:    ");     println (frame.r13);
        print ("r14:    ");     println (frame.r14);
        print ("r15:    ");     println (frame.r15);
    end printRegs;

    ---------------------------------------------------------------------------
    -- eoi - handle end of interrupt, depending on the interrupt source. If
    --  the interrupt controller has not been set up yet, this should not be
    --  called, and will cause a kernel panic if it is.
    ---------------------------------------------------------------------------
    procedure eoi (num : x86Interrupt) with
        SPARK_Mode => On
    is
        NoInterruptController : Exception;
        package myLapic is new lapic(lapicAddr);
    begin
        case intController is
            when LEGACY_PIC =>
                -- for PIC, no action necessary except for hardware IRQs
                if num in TIMER..IDE2 then
                    pic.finishIRQ (num);
                end if;
            when APIC =>
                myLapic.finishIRQ;
                -- When LAPIC is active but no IOAPIC (virtual wire mode),
                -- PIC-sourced IRQs also need PIC EOI to allow the next
                -- interrupt on that line.
                if num in TIMER..IDE2 then
                    pic.finishIRQ (num);
                end if;
            when X2APIC =>
                null;
            when NONE =>
                println("FATAL: end-of-interrupt called with no interrupt controller.");
                raise NoInterruptController;
        end case;
    end eoi;

    ---------------------------------------------------------------------------
    -- dispatchDeviceIRQ
    --
    -- Legacy PCI INTx is level-triggered and shareable. Deliver the event to
    -- every process that was explicitly registered for this vector; each
    -- driver is responsible for checking its own device status before
    -- acknowledging it.
    ---------------------------------------------------------------------------
    procedure dispatchDeviceIRQ (vector : x86Interrupt) with
        SPARK_Mode => On
    is
        dest : Interfaces.Unsigned_64;
    begin
        for index in Capabilities.IRQ.IRQOwnerIndex loop
            dest := Capabilities.IRQ.getOwner (vector, index);
            if dest > 0 and then
               dest <= Interfaces.Unsigned_64 (Process.ProcessID'Last)
            then
                Process.IPC.sendEvent
                  (Process.ProcessID (dest),
                   (tag      => (label  => 1,
                                  length => 0,
                                  flags  => 0,
                                  badge  => 0),
                    capBadge => 0,
                    words    => (others => 0)));
            end if;
        end loop;
    end dispatchDeviceIRQ;

    ---------------------------------------------------------------------------
    -- Interrupt Handler - called from interrupt_handlers.asm. Note that this
    --  function needs to know what (PIC, APIC, x2APIC) is generating these
    --  interrupts.
    ---------------------------------------------------------------------------
    procedure interruptHandler (frame : not null access constant Stackframe.InterruptStackFrame)
        with SPARK_Mode => On
    is
        KernelFPUException : Exception;

        interruptNumber : constant x86Interrupt := x86Interrupt(frame.interruptNumber);
        oldCR3 : Integer_Address;
    begin
        -- Save the old address space
        oldCR3 := x86.getCR3;
        --print("Saving old address space: "); println(oldCR3);

        -- Switch page tables to kernel. If this interrupt happened during user-mode, the TSS
        -- should ensure that the process' kernel stack is being used.
        -- @TODO not sure this is necessary since kernel is mapped in each process.
        Mem_mgr.switchAddressSpace;
        --print("In kernel address space: "); println(x86.getCR3);

        case interruptNumber is
            when NMI =>
                -- NMI handler: lock-free, writes directly to serial port.
                -- Do NOT acquire any spinlocks here - NMI can fire while
                -- a lock is held, leading to instant deadlock.
                handleNMI : declare
                    perCPUAddr : constant System.Address := PerCPUData.getPerCPUDataAddr;
                    cpuData : PerCPUData.PerCPUData with
                        Import, Volatile, Address => perCPUAddr;
                    nmiMsg : constant String := "NMI on CPU " & ASCII.LF;
                begin
                    -- Record NMI state per-CPU
                    cpuData.nmiCount := cpuData.nmiCount + 1;

                    if cpuData.nmiInProgress then
                        -- Nested NMI - just count and return
                        return;
                    end if;

                    cpuData.nmiInProgress := True;

                    -- Log directly to serial port (bypasses TextIO lock)
                    for C of nmiMsg loop
                        Serial.send (Config.serialMirrorPort, C);
                    end loop;

                    cpuData.nmiInProgress := False;
                end handleNMI;
                -- NMI runs on IST 1, returns via iretq - no EOI needed
                return;

            when DOUBLE_FAULT =>
                -- Double fault runs on IST 2. Unrecoverable.
                println ("DOUBLE FAULT!");
                printRegs (frame);
                x86.halt;

            when MACHINE_CHECK =>
                -- Machine check runs on IST 3. Unrecoverable.
                println ("MACHINE CHECK EXCEPTION!");
                printRegs (frame);
                x86.halt;

            when NO_MATH_COPROCESSOR =>
                -- FP/SIMD state is restored eagerly before entering userspace,
                -- and kernel code is compiled without FP/SIMD. Reaching #NM
                -- therefore means the isolation invariant was violated; never
                -- recover by exposing whatever register state is still live.
                raise KernelFPUException
                    with "Unexpected #NM under eager FPU switching";

            when PAGE_FAULT =>
                print ("Page Fault at ");
                print (frame.rip);
                print (" PID ");
                println (PerCPUData.getCurrentPID);
                handlePageFault (frame.errorCode);

            when TIMER =>
                -- must finish IRQ first since any yield that happens
                -- will eventually return to interruptReturn in
                -- interrupt.asm, not here.
                eoi (TIMER);
                Time.clockTick;
                
            when PS2KEYBOARD =>
                -- println ("PS2 Interrupt");
                eoi (PS2KEYBOARD);
                dispatchDeviceIRQ (PS2KEYBOARD);

            when IDE1 =>
                eoi (IDE1);
                dispatchDeviceIRQ (IDE1);

            when IDE2 =>
                eoi (IDE2);
                dispatchDeviceIRQ (IDE2);

            when DEVICE_MSI_FIRST =>
                --  Dedicated PCI message-signaled vector. It is outside
                --  the legacy PIC range so no physical IRQ line aliases it.
                eoi (interruptNumber);
                dispatchDeviceIRQ (interruptNumber);

            when INVALID .. COPROCESSOR =>
                eoi (interruptNumber);
                dispatchDeviceIRQ (interruptNumber);

            when RESCHEDULE =>
                eoi (RESCHEDULE);
                -- IPI from another CPU: set needReschedule so the
                -- checkPreempt block at interrupt return yields.
                -- Also flush TLB if a grant revocation requires it.
                reschedIPI : declare
                    perCPUAddr2 : constant System.Address :=
                        PerCPUData.getPerCPUDataAddr;
                    cpuData2 : PerCPUData.PerCPUData with
                        Import, Volatile, Address => perCPUAddr2;
                    myCPU : constant Natural := cpuData2.cpuNum;
                begin
                    cpuData2.needReschedule := True;
                    if Process.tlbFlushPending(myCPU) then
                        Process.tlbFlushPending(myCPU) := False;
                        Virtmem.flushTLB;
                    end if;
                end reschedIPI;

            when SPURIOUS =>
                println ("Spurious Interrupt");

            when KERNEL_PANIC =>
                println ("KERNEL PANIC!");
                printRegs (frame);
                x86.halt;

            when SYSCALL =>
                print ("SYSCALL");

            when others =>
                print ("EXCEPTION "); print (Integer(interruptNumber));
                print (" at RIP "); print (frame.rip);
                print (" PID "); println (PerCPUData.getCurrentPID);
                if Util.isBitSet (frame.cs, 0) or Util.isBitSet (frame.cs, 1) then
                    -- User-mode exception: notify supervisor, then kill
                    print ("  Killing user process "); println (PerCPUData.getCurrentPID);
                    printRegs (frame);
                    Process.IPC.notifySupervisor (
                        pid        => PerCPUData.getCurrentPID,
                        faultLabel => IPC_Labels.EVENT_PROCESS_FAULT,
                        detail0    => Interfaces.Unsigned_64 (interruptNumber),
                        detail1    => Util.addrToNum (frame.rip),
                        detail2    => Interfaces.Unsigned_64 (frame.errorCode));
                    Process.kill (PerCPUData.getCurrentPID);
                else
                    -- Kernel exception: halt
                    printRegs (frame);
                    x86.halt;
                end if;
        end case;

        -- If a higher-priority process was readied during this interrupt
        -- (e.g. keyboard IRQ woke the keyboard service), preempt now.
        -- Guard: only yield when a real process is running. If the timer
        -- fires during the scheduler context (currentPID = NO_PROCESS),
        -- skip — the scheduler will pick the highest-priority process.
        checkPreempt : declare
            perCPUAddr : constant System.Address :=
                PerCPUData.getPerCPUDataAddr;
            cpuData : PerCPUData.PerCPUData with
                Import, Volatile, Address => perCPUAddr;
        begin
            if cpuData.needReschedule then
                cpuData.needReschedule := False;
                if cpuData.currentPID /= Process.NO_PROCESS then
                    Process.yield;
                end if;
            end if;
        end checkPreempt;

        -- if we return from this interrupt, put page tables back the way they were.
        -- TODO: check cs to see if we were in user code?
        Virtmem.setActiveP4 (oldCR3);
    end interruptHandler;

    ---------------------------------------------------------------------------
    -- handlePageFault
    ---------------------------------------------------------------------------
    procedure handlePageFault (err : in Unsigned_64) with
        SPARK_Mode => On
    is
        faultAddr : constant System.Address := Util.numToAddr (x86.getCR2);

        pid : constant Process.ProcessID := PerCPUData.getCurrentPID;
        
        PageFaultException : exception;
        NXEException       : exception;

        -- what caused the page fault
        present         : constant Boolean := Util.isBitSet (err, 0);
        write           : constant Boolean := Util.isBitSet (err, 1);
        userMode        : constant Boolean := Util.isBitSet (err, 2);
        reservedWrite   : constant Boolean := Util.isBitSet (err, 3);
        nxeViolation    : constant Boolean := Util.isBitSet (err, 4);
    begin
        -- handle NXE violations separately.
        if nxeViolation then
            if userMode then
                -- if it was a user process, then kill it.
                println("NXE violation in user process!");
            else
                -- if it was the kernel, we goofed up.
                println("NXE violation in kernel!");
                raise NXEException with "NXE violated in the kernel.";
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
                                print ("User page-protection write violation: ");
                                println (faultAddr);
                                Process.kill (pid);
                            when False =>
                                -- kernel page-protection wr violation. we goofed.
                                print ("Kernel page-protection write violation: ");
                                println (faultAddr);
                                raise PageFaultException with "Kernel tried to write non-writable page";
                        end case;
                    when False =>
                        case userMode is
                            when True =>
                                -- user page-protection rd violation. kill it.
                                print ("User page-protection read violation: ");
                                println (faultAddr);
                                Process.kill (pid);
                            when False =>
                                -- kernel page-protection rd violation. we goofed.
                                print ("Kernel page-protection read violation: ");
                                println (faultAddr);
                                raise PageFaultException with "Kernel tried to read non-readable page";
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
                                print ("User non-present page write: ");
                                println (faultAddr);
                                Process.pageFault (pid, faultAddr);
                            when False =>
                                -- kernel wrote non-present page. see if it's something
                                -- that we should have, page it in if it is.
                                print ("Kernel non-present page write: ");
                                println (faultAddr);
                                raise PageFaultException;
                        end case;
                    when False =>
                        case userMode is
                            when True =>
                                -- user read non-present page. see if it's in their
                                -- allocated range and page in if it is. If it's not,
                                -- may be a stack overflow or OoM.
                                print ("User non-present page read: ");
                                println (faultAddr);
                                Process.pageFault (pid, faultAddr);
                            when False =>
                                -- kernel read non-present page. see if it's something
                                -- that we should have. see if it's something that we should
                                -- have, page it in if it is.
                                print ("Kernel non-present page read: ");
                                println (faultAddr);
                                raise PageFaultException with "Kernel read non-present page.";
                        end case;                    
                end case;
        end case;

    end handlePageFault;

    ---------------------------------------------------------------------------
    -- 
    ---------------------------------------------------------------------------
    procedure setup is
    begin
        setupIDT;
        loadIDT;
    end setup;

    ---------------------------------------------------------------------------
    -- Install ISRs, load the IDT holding them all.
    ---------------------------------------------------------------------------
    procedure setupIDT with
        SPARK_Mode => Off   -- due to 'Address
    is
    begin
        createIDT;
        idtp := calculateIDTP (idt'Address);
        validIDT := True;
    end setupIDT;

    ---------------------------------------------------------------------------
    -- Wrapper for lidt
    ---------------------------------------------------------------------------
    procedure loadIDT with
        SPARK_Mode => Off   -- due to 'Address
    is
    begin
        x86.lidt (idtp'Address);
    end loadIDT;

    ---------------------------------------------------------------------------
    -- Set interrupt controller used
    ---------------------------------------------------------------------------
    procedure setInterruptController (cont : in InterruptController) with
        SPARK_Mode => On
    is
    begin
        intController := cont;
    end setInterruptController;

    ---------------------------------------------------------------------------
    -- Set LAPIC base address
    ---------------------------------------------------------------------------        
    procedure setLAPICBaseAddress (lapicBase : in virtmem.PhysAddress) with
        SPARK_Mode => On
    is
    begin
        lapicAddr := To_Address (virtmem.P2V(lapicBase));
    end setLAPICBaseAddress;

    ---------------------------------------------------------------------------
    -- Compose the IDTPointer containing our IDTEntry size and the address to
    -- the IDT itself.
    ---------------------------------------------------------------------------
    function calculateIDTP (idtPtr : in System.Address) return IDTPointer 
        with SPARK_Mode => On
    is
        myidtp : IDTPointer;
    begin
        -- 'Size gives bits, so divide by 8 for bytes of entire IDT structure
        myidtp.size := Unsigned_16((IDTEntry'Size * 256 / 8) - 1);
        myidtp.base := Util.addrToNum (idtPtr);
        return myidtp;
    end calculateIDTP;

    ---------------------------------------------------------------------------
    -- Create a single IDT entry
    -- Params:
    --  handler: address of ISR
    --  gdtSelector: GDT selector for our kernel
    --  dpl: descriptor privilege level (Ring 0-3) for interrupt
    ---------------------------------------------------------------------------
    function createIDTEntry (handler     : in Unsigned_64;
                             isTrap      : in Boolean;
                             gdtSelector : in segment.GDTOffset;
                             dpl         : in x86.PrivilegeLevel;
                             ist         : in Integer := 0) return IDTEntry
        with SPARK_Mode => On
    is
        newidt : IDTEntry;
    begin
        newidt.offset1  := Unsigned_16(handler and 16#FFFF#);
        newidt.selector := gdtSelector;
        newidt.istIndex := ist;

        newidt.istrap   := isTrap;
        newidt.dpl      := dpl;
        newidt.present  := True;

        newidt.offset2  := Unsigned_16(Shift_Right(handler, 16) and 16#FFFF#);
        newidt.offset3  := Unsigned_32(Shift_Right(handler, 32) and 16#FFFF_FFFF#);

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
        -- NMI uses IST 1: dedicated stack so NMI during lock-hold doesn't corrupt kernel stack
        idt(2)  := createIDTEntry(addrToNum(isr2'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL, ist => 1);
        idt(3)  := createIDTEntry(addrToNum(isr3'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(4)  := createIDTEntry(addrToNum(isr4'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(5)  := createIDTEntry(addrToNum(isr5'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(6)  := createIDTEntry(addrToNum(isr6'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(7)  := createIDTEntry(addrToNum(isr7'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        -- Double Fault uses IST 2: must have its own stack to handle stack overflow
        idt(8)  := createIDTEntry(addrToNum(isr8'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL, ist => 2);
        idt(9)  := createIDTEntry(addrToNum(isr9'Address),  False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(10) := createIDTEntry(addrToNum(isr10'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(11) := createIDTEntry(addrToNum(isr11'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(12) := createIDTEntry(addrToNum(isr12'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(13) := createIDTEntry(addrToNum(isr13'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(14) := createIDTEntry(addrToNum(isr14'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        idt(16) := createIDTEntry(addrToNum(isr16'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        idt(17) := createIDTEntry(addrToNum(isr17'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
        -- Machine Check uses IST 3: hardware failure must not corrupt existing stack
        idt(18) := createIDTEntry(addrToNum(isr18'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL, ist => 3);
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
        idt(DEVICE_MSI_FIRST) := createIDTEntry(addrToNum(isr48'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- Kernel Panic - don't want interrupts to happen here, because we're crashed.
        idt(127) := createIDTEntry(addrToNum(isr127'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- Syscall (old school, we use the actual syscall/sysret instructions in CuBit)
        idt(128) := createIDTEntry(addrToNum(isr128'Address), True, GDT_OFFSET_KERNEL_CODE, DPL_USER);

        -- Reschedule IPI
        idt(249) := createIDTEntry(addrToNum(isr249'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);

        -- Spurious Vector
        idt(255) := createIDTEntry(addrToNum(isr255'Address), False, GDT_OFFSET_KERNEL_CODE, DPL_KERNEL);
    end createIDT;

    ---------------------------------------------------------------------------
    -- IOAPIC base address for enableDeviceIRQ
    ---------------------------------------------------------------------------
    ioapicAddr : System.Address := System.Null_Address;

    ---------------------------------------------------------------------------
    -- setIOAPICBaseAddress
    ---------------------------------------------------------------------------
    procedure setIOAPICBaseAddress (addr : in System.Address) with
        SPARK_Mode => On
    is
    begin
        ioapicAddr := addr;
    end setIOAPICBaseAddress;

    ---------------------------------------------------------------------------
    -- Shared LAPIC timer interval (BSP-calibrated, used by APs)
    ---------------------------------------------------------------------------
    lapicTimerInterval : Unsigned_32 := 0;

    function getLAPICTimerInterval return Unsigned_32 is
    begin
        return lapicTimerInterval;
    end getLAPICTimerInterval;

    procedure setLAPICTimerInterval (interval : in Unsigned_32) is
    begin
        lapicTimerInterval := interval;
    end setLAPICTimerInterval;

    ---------------------------------------------------------------------------
    -- enableDeviceIRQ
    --  If IOAPIC is available, route via IOAPIC.  Otherwise fall back to
    --  legacy 8259 PIC unmask.
    ---------------------------------------------------------------------------
    procedure enableDeviceIRQ
      (vector         : in InterruptNumbers.x86Interrupt;
       cpu            : in Unsigned_32;
       levelTriggered : in Boolean := False;
       activeLow      : in Boolean := False) with
        SPARK_Mode => Off   -- generic instantiation
    is
    begin
        if System."/=" (ioapicAddr, System.Null_Address) then
            enableIOAPIC : declare
                package myIOAPIC is new ioapic (ioapicAddr);
            begin
                myIOAPIC.enableIRQ
                  (vector, cpu, levelTriggered, activeLow);
            end enableIOAPIC;
        else
            --  No IOAPIC; unmask on legacy PIC instead
            Pic.enableIRQ (vector);
        end if;
    end enableDeviceIRQ;

end Interrupts;
