-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- x86-64 interrupt vector numbers
-------------------------------------------------------------------------------
package InterruptNumbers with
    SPARK_Mode => On
is
    subtype x86Interrupt is Natural range 0..255;

    -- Software Exceptions
    DIVIDE_BY_ZERO              : constant x86Interrupt := 0;
    DEBUG                       : constant x86Interrupt := 1;
    NMI                         : constant x86Interrupt := 2;
    BREAKPOINT                  : constant x86Interrupt := 3;
    OVERFLOW                    : constant x86Interrupt := 4;
    BOUND_EXCEED                : constant x86Interrupt := 5;
    INVALID_OPCODE              : constant x86Interrupt := 6;
    NO_MATH_COPROCESSOR         : constant x86Interrupt := 7;
    DOUBLE_FAULT                : constant x86Interrupt := 8;
    SEGMENT_OVERRUN             : constant x86Interrupt := 9;
    INVALID_TSS                 : constant x86Interrupt := 10;
    SEGMENT_NOT_PRESENT         : constant x86Interrupt := 11;
    STACK_SEGMENT_FAULT         : constant x86Interrupt := 12;
    GENERAL_PROTECTION_FAULT    : constant x86Interrupt := 13;
    PAGE_FAULT                  : constant x86Interrupt := 14;
    RESERVED                    : constant x86Interrupt := 15;
    FP_ERROR                    : constant x86Interrupt := 16;
    ALIGNMENT_CHECK             : constant x86Interrupt := 17;
    MACHINE_CHECK               : constant x86Interrupt := 18;
    SIMD_FP_EXCEPTION           : constant x86Interrupt := 19;
    VIRTUALIZATION_EXCEPTION    : constant x86Interrupt := 20;

    -- Hardware IRQs
    TIMER                       : constant x86Interrupt := 32;
    PS2KEYBOARD                 : constant x86Interrupt := 33;
    INVALID                     : constant x86Interrupt := 34;
    COM2                        : constant x86Interrupt := 35;
    COM1                        : constant x86Interrupt := 36;
    LPT2                        : constant x86Interrupt := 37;
    FLOPPY                      : constant x86Interrupt := 38;
    LPT1                        : constant x86Interrupt := 39;
    RTC                         : constant x86Interrupt := 40;
    ACPI                        : constant x86Interrupt := 41;
    PERIPHERAL1                 : constant x86Interrupt := 42;
    PERIPHERAL2                 : constant x86Interrupt := 43;
    PS2MOUSE                    : constant x86Interrupt := 44;
    COPROCESSOR                 : constant x86Interrupt := 45;
    IDE1                        : constant x86Interrupt := 46;  -- technically "ATA1"
    IDE2                        : constant x86Interrupt := 47;

    -- CubitOS defined
    KERNEL_PANIC                : constant x86Interrupt := 127;

    -- User interrupt. Use same number as Linux for ease of porting sw
    SYSCALL                     : constant x86Interrupt := 128;

    -- Spurious Vector
    SPURIOUS                    : constant x86Interrupt := 255;
end InterruptNumbers;