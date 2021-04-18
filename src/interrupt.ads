-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary
-- x86-64 interrupt handler routine and machine-dependent interrupt-related
-- functions.
--
-- @description
-- For interrupt setup, first the interrupt vector/descriptor table (IDT) must
-- be created with setupIDT. This can be done before the interrupt controller
-- itself is set up, for exception handling. It must then be loaded with 
-- loadIDT, once per CPU.
--
-- Then, the interrupt controller type can be provided using
-- setInterruptController. Interrupt handlers will use this for
-- end-of-interrupt cleanup.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with Process;
-- with Scheduler;
with Segment;
with Stackframe;
with Textmode; use Textmode;
with Util; use Util;
with Virtmem;
with x86;

package Interrupt with
    SPARK_Mode => On,
    Abstract_State => (InterruptServiceRoutines, SetupState, IDTState)
is

    ---------------------------------------------------------------------------
    -- InterruptController
    ---------------------------------------------------------------------------
    type InterruptController is (NONE, LEGACY_PIC, APIC, X2APIC);

    ---------------------------------------------------------------------------
    -- initialized : True once setupIDT is complete (validIDT = True) and
    --  setInterruptController has been called.
    ---------------------------------------------------------------------------
    initialized                 : Boolean := False with Ghost;
    validIDT                    : Boolean := False with Ghost;

    ---------------------------------------------------------------------------
    -- Struct describing x86-64 Interrupt Descriptor Table entry
    -- @field offset1 - handler address bits 0..15
    -- @field selector - code segment selector in GDT or LDT
    -- @field istIndex - 0: don't switch stacks, 1-7: switch to nth stack in
    --  IST when called
    -- @field isTrap - if 0/False, interrupts disabled in this handler (0 =
    --  interrupt gate). For syscalls, we still want interrupts to happen.
    -- @field res2 - must be all ones
    -- @field res3 - must be all zeroes
    -- @field dpl - Descriptor privilege level (ring 0 or ring 3)
    -- @field present - True if this entry is valid
    -- @field offset2 - handler address bits 16..31
    -- @field offset3 - handler address bits 32..63
    -- @field zero
    ---------------------------------------------------------------------------
    type IDTEntry is
    record
        offset1     : Unsigned_16           := 0;
        selector    : segment.GDTOffset     := segment.GDT_OFFSET_NULL;
        istIndex    : Integer range 0..7    := 0;
        res1        : Integer range 0..31   := 0;
        isTrap      : Boolean               := False;
        res2        : Integer range 0..7    := 7;
        res3        : Integer range 0..1    := 0;
        dpl         : x86.PrivilegeLevel    := x86.DPL_KERNEL;
        present     : Boolean               := False;
        offset2     : Unsigned_16           := 0;
        offset3     : Unsigned_32           := 0;
        zero        : Unsigned_32           := 0;
    end record with Size => 8 * 16;
    
    for IDTEntry use
    record
        offset1     at 0 range 0 .. 15;
        selector    at 2 range 0 .. 15;
        istIndex    at 4 range 0 .. 2;
        res1        at 4 range 3 .. 7;
        isTrap      at 5 range 0 .. 0;
        res2        at 5 range 1 .. 3;
        res3        at 5 range 4 .. 4;
        dpl         at 5 range 5 .. 6;
        present     at 5 range 7 .. 7; 
        -- more offsets
        offset2     at 6 range 0 .. 15;
        offset3     at 8 range 0 .. 31;
        zero        at 12 range 0 .. 31;
    end record;

    ---------------------------------------------------------------------------
    -- IDT pointer uses a special format containing size and the address
    ---------------------------------------------------------------------------
    type IDTPointer is
    record
        size : Unsigned_16 := 0;
        base : Unsigned_64 := 0;
    end record with Size => 8 * 10;             -- 10-byte structure

    for IDTPointer use
    record
        size    at 0 range 0 .. 15;
        base    at 2 range 0 .. 63;
    end record;

    ---------------------------------------------------------------------------
    -- Our Interrupt Descriptor Table (IDT)
    ---------------------------------------------------------------------------
    type IDTType is array (Integer range 0..255) of IDTEntry 
        with Size => 16 * 8 * 256, Convention => C;

    ---------------------------------------------------------------------------
    -- Interrupt handler. Called from interrupt.asm.
    ---------------------------------------------------------------------------
    procedure interruptHandler(frame : not null access constant stackframe.InterruptStackFrame)
    with 
        Global => (Proof_In => validIDT,
                   In_Out => (Textmode.ScreenState,
                              Textmode.cursor,
                            --   Keyboard.caps,
                            --   Keyboard.shifted,
                              Process.lock)),
        Pre => validIDT,
        Export => True, 
        --Convention => C, 
        External_Name => "interruptHandler";

    ---------------------------------------------------------------------------
    -- setupIDT
    --
    -- Install ISRs for all processors
    ---------------------------------------------------------------------------
    procedure setupIDT with
        Global => (Output => IDTState),
        Post => validIDT;

    ---------------------------------------------------------------------------
    -- loadIDT
    -- Installs the IDT for _this_ processor only. Must be called by each CPU
    -- on the system.
    ---------------------------------------------------------------------------
    procedure loadIDT with
        Pre => validIDT;

    ---------------------------------------------------------------------------
    -- setInterruptController
    --
    -- Set the interrupt controller used to generate interrupts for this
    --  hardware.
    -- @param cont - see type InterruptController
    ---------------------------------------------------------------------------
    procedure setInterruptController(cont : in InterruptController) with
        Pre => validIDT,
        Post => initialized;

    ---------------------------------------------------------------------------
    -- setLAPICBaseAddress
    --
    -- If LAPIC is being used as the interrupt controller, this is used to set
    --  the LAPIC base address for generic lapic package instantiation.
    -- @param lapicBase - physical base address of the LAPIC
    ---------------------------------------------------------------------------
    procedure setLAPICBaseAddress(lapicBase : in virtmem.PhysAddress) with
        Global => (Output => SetupState);

private
    -- Interrupt Controller
    intController : InterruptController := NONE with Part_Of => SetupState;
    lapicAddr : System.Address := System.Null_Address with Part_Of => SetupState;
    
    -- Clock ticks moved to time.ads
    --ticks : Unsigned_64 := 0 with Part_Of => TimerState;

    ---------------------------------------------------------------------------
    -- Handler for Page Faults
    -- @param err - error code as set by the underlying CPU exception.
    ---------------------------------------------------------------------------
    procedure handlePageFault(err : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- Calculate the IDTPointer type from our idt address
    ---------------------------------------------------------------------------
    function calculateIDTP(idtPtr : in System.Address) return IDTPointer;

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
                            dpl : in x86.PrivilegeLevel) return IDTEntry;

    ---------------------------------------------------------------------------
    -- Put our interrupt vectors into the IDT array
    ---------------------------------------------------------------------------
    procedure createIDT;

    ---------------------------------------------------------------------------
    -- Interrupt service routines found in interrupt_handlers.asm
    ---------------------------------------------------------------------------
    isr0    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr1    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr2    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr3    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr4    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr5    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr6    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr7    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr8    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr9    : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr10   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr11   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr12   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr13   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr14   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

    isr16   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr17   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr18   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr19   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

    -- Hardware IRQs
    isr32   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr33   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr34   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr35   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr36   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr37   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr38   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr39   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr40   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr41   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr42   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr43   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr44   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr45   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr46   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;
    isr47   : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

    -- CuBit-specific
    -- Kernel Panic interrupt
    isr127  : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

    -- Syscall software interrupt
    isr128  : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

    -- Spurious Vector
    isr255  : Symbol with Import, Convention => C, Part_Of => InterruptServiceRoutines;

end interrupt;