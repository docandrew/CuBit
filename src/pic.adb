-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Intel 8259A Programmable Interrupt Controller setup and handler routines
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;

--with Config;
--with Textmode;
--with Util; use Util;

package body pic with
    Refined_State => (IRQMaskState => irqMask),
    SPARK_Mode => On
is

    -- IRQ mask. Start with all disabled except slave PIC (Invalid one above)
    irqMask : Unsigned_16;

    function getIRQMask return Unsigned_16 with
        SPARK_Mode => On,
        Refined_Global => (Input => irqMask),
        Refined_Depends => (getIRQMask'Result => irqMask),
        Refined_Post => (getIRQMask'Result = irqMask)
    is
    begin
        return irqMask;
    end getIRQMask;

    ---------------------------------------------------------------------------
    -- Install ISRs, lidt, init H/W IRQs
    ---------------------------------------------------------------------------
    procedure setupPIC
        with SPARK_Mode => On
    is
    begin
        --initIDT;
        initIRQ;
        initialized := True;        -- ghost assignment
    end setupPIC;

    ---------------------------------------------------------------------------
    -- Initialize 8259A interrupt controller
    ---------------------------------------------------------------------------
    procedure initIRQ with 
        SPARK_Mode => On,
        Refined_Global => (Output => (irqMask, x86.IOPortState))
    is
    begin
        -- mask all HW interrupts initially.
        out8(PIC1Data, 16#FF#);
        out8(PIC2Data, 16#FF#);

        out8(PIC1Command,   16#11#);
	    out8(PIC2Command,   16#11#);
        out8(PIC1Data,      16#20#);
        out8(PIC2Data,      16#28#);
        out8(PIC1Data,      16#04#);
        out8(PIC2Data,      16#02#);
        out8(PIC1Data,      16#01#);
        out8(PIC2Data,      16#01#);
        out8(PIC1Data,      16#00#);
        out8(PIC2Data,      16#00#);

        -- unmask the slave PIC IRQ 
        setIRQMask(16#FFFB#);
    end initIRQ;

    ---------------------------------------------------------------------------
    -- Tell slave/master PIC that the interrupt is finished.
    ---------------------------------------------------------------------------
    procedure finishIRQ(irqnum : in x86Interrupt)
    with 
        SPARK_Mode => On
    is
    begin
        if irqnum >= 40 then
            -- tell slave controller interrupt is done
            out8(PIC2Command, IRQ_END);
        end if;

        --tell master controller interrupt is done
        out8(PIC1Command, IRQ_END);
    end finishIRQ;

    ---------------------------------------------------------------------------
    -- send the mask to our PIC and update our global irqMask.
    ---------------------------------------------------------------------------
    procedure setIRQMask(mask : Unsigned_16)
    with
        SPARK_Mode => On,
        Refined_Global => (Output => (irqMask, x86.IOPortState)),
        Refined_Depends => (irqMask => mask, x86.IOPortState => mask),
        Refined_Post => (irqMask = mask)
    is
    begin
        out8(PIC1Data, Unsigned_8(16#FF# and mask));
        out8(PIC2Data, Unsigned_8(Shift_Right(mask, 8)));
        irqMask := mask;
    end setIRQMask;

    ---------------------------------------------------------------------------
    -- Mask everything, including slave PIC
    ---------------------------------------------------------------------------
    procedure disable
    with
        SPARK_Mode => On,
        Refined_Global => (Output => (irqMask, x86.IOPortState)),
        Refined_Depends => (irqMask => null, x86.IOPortState => null),
        Refined_Post => (irqMask = 16#FFFF#)
    is
    begin
        setIRQMask(16#FFFF#);
    end disable;

    ---------------------------------------------------------------------------
    -- clear bit for IRQ we want.
    ---------------------------------------------------------------------------   
    procedure enableIRQ(int : in x86Interrupt)
    with
        SPARK_Mode => On
    is
        mask : constant Unsigned_16 :=
            irqMask and (not Shift_Left(1, (int - 32)));
    begin
        setIRQMask(mask);
    end enableIRQ;

    ---------------------------------------------------------------------------
    -- set bit for IRQ we want.
    ---------------------------------------------------------------------------
    procedure disableIRQ(int : in x86Interrupt)
    with
        SPARK_Mode => On
    is
        mask : constant Unsigned_16 :=
            irqMask or Shift_Left(1, (int - 32));
    begin
        setIRQMask(mask);
    end disableIRQ;
        
end pic;
    