-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- @summary Intel 8259A Programmable Interrupt Controller setup and handler
--  routines.
--
-- This interrupt controller is used in legacy platforms with no multi-core
-- CPU support. This must still be set-up even if not used, since it will
-- continue to generate timer interrupts that get routed to the wrong place
-- (causing what looks like a double-fault) if not re-mapped/disabled.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
--with System;

with InterruptNumbers; use InterruptNumbers;
--with segment;
--with util; use util;
with x86; use x86;

package pic with
    Abstract_State => IRQMaskState,
    SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- IOPorts for the 8259A PIC
    ---------------------------------------------------------------------------
    -- Master IRQ controller (IRQ 0-7, ISR 32-39)
    PIC1Command : constant IOPort := 16#20#;
    PIC1Data    : constant IOPort := 16#21#;
    -- Slave IRQ controller (IRQ 8-15, ISR 40-47)
    PIC2Command : constant IOPort := 16#A0#;
    PIC2Data    : constant IOPort := 16#A1#;

    ---------------------------------------------------------------------------
    -- Command to finish an IRQ
    ---------------------------------------------------------------------------
    IRQ_END     : constant := 16#20#;

    ---------------------------------------------------------------------------
    -- initialized : True once setupPIC completes. Ghost var used only
    --  for contract checks at proof-time.
    ---------------------------------------------------------------------------
    initialized : Boolean := False with Ghost;

    ---------------------------------------------------------------------------
    -- getIRQMask:
    --  Return the IRQ mask currently in use.
    ---------------------------------------------------------------------------
    function getIRQMask return Unsigned_16 with
        Global => (Input => IRQMaskState),
        Depends => (getIRQMask'Result => IRQMaskState);

    ---------------------------------------------------------------------------
    -- setupPIC:
    --  Remaps the PIC's interrupts to the proper IRQ range (>32) and masks
    --  them all by default (except Slave IRQ).
    ---------------------------------------------------------------------------
    procedure setupPIC with
        Global => (In_Out => (initialized),
                   Output => (IRQMaskState, x86.IOPortState)),
        Pre => (not initialized),
        Post => (initialized and getIRQMask = 16#FFFB#);

    ---------------------------------------------------------------------------
    -- setIRQMask:
    --  Sends the given mask to our PIC to enable IRQs for those bits cleared
    --  in the mask, and disable IRQs for those bits set in the mask. This also
    --  updates our global irqMask var.
    --
    --  In the PIC, we use IOPorts to turn on and off IRQs, so this function
    --  has a global effect on the I/O Port
    ---------------------------------------------------------------------------
    procedure setIRQMask(mask : Unsigned_16) with
        Global => (Output => (x86.IOPortState, IRQMaskState)),
        Depends => (IRQMaskState => mask, x86.IOPortState => mask),
        Post => (getIRQMask = mask);

    ---------------------------------------------------------------------------
    -- disable:
    --  Convenience function for disabling all interrupts from this device.
    ---------------------------------------------------------------------------
    procedure disable with
        Global => (Output => (x86.IOPortSTate, IRQMaskState)),
        Depends => (IRQMaskState => null, x86.IOPortState => null),
        Post => (getIRQMask = 16#FFFF#);

    ---------------------------------------------------------------------------
    -- enableIRQ:
    --  Convenience function for enabling a single HW IRQ. Clears the bit for
    --  that IRQ and updates our irqMask var.
    ---------------------------------------------------------------------------
    procedure enableIRQ(int : in x86Interrupt) with
        Pre => (int in TIMER..IDE2);

    procedure disableIRQ(int : in x86Interrupt) with
        Pre => (int in TIMER..IDE2);

    ---------------------------------------------------------------------------
    -- Tell interrupt controller that we're done servicing a HW IRQ
    --
    -- Affects x86 I/O port state (see setIRQMask)
    ---------------------------------------------------------------------------
    procedure finishIRQ(irqnum : in x86Interrupt) with
        Global => (Proof_In => initialized,
                   Output => x86.IOPortState),
        Pre => (irqnum >= TIMER and then 
                irqnum <= IDE2 and then
                initialized);

private

    ---------------------------------------------------------------------------
    -- Initialize 8259A interrupt controller.
    --
    -- Affects x86 I/O port state (see setIRQMask)
    ---------------------------------------------------------------------------
    procedure initIRQ with
        Global => (Output => (x86.IOPortState, IRQMaskState)),
        Post => (getIRQMask = 16#FFFB#);

end pic;
    