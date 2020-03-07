-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- PIT - 8253/8254 Programmable Interval Timer.
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with x86;

package timer_pit
    with SPARK_Mode => On
is

    ---------------------------------------------------------------------------
    -- 8253 Timer ports
    ---------------------------------------------------------------------------
    Timer0  	        : constant x86.IOPort := 16#40#;
    Timer1 	            : constant x86.IOPort := 16#41#;
    Timer2 	            : constant x86.IOPort := 16#42#;
    TimerModeReg        : constant x86.IOPort := 16#43#;

    ---------------------------------------------------------------------------
    -- Types used in the timer command register
    ---------------------------------------------------------------------------
    subtype TimerChannel is Natural range 0..3;
    TIMER_CHANNEL_0     : constant TimerChannel := 0;
    TIMER_CHANNEL_1     : constant TimerChannel := 1;
    TIMER_CHANNEL_2     : constant TimerChannel := 2;
    READ_BACK           : constant TimerChannel := 3; -- only on 8254 chip

    subtype TimerAccessMode is Natural range 0..3;
    LATCH_COUNT_VALUE   : constant TimerAccessMode := 0;
    ACCESS_LOW_BYTE     : constant TimerAccessMode := 1;
    ACCESS_HIGH_BYTE    : constant TimerAccessMode := 2;
    ACCESS_WORD         : constant TimerAccessMode := 3;

    subtype TimerOperatingMode is Natural range 0..7;
    MODE_0_INT_TERM_CT  : constant TimerOperatingMode := 0;
    MODE_1_HW_ONE_SHOT  : constant TimerOperatingMode := 1;
    MODE_2_RATE_GEN     : constant TimerOperatingMode := 2;
    MODE_3_SQ_WAVE_GEN  : constant TimerOperatingMode := 3;
    MODE_4_SW_STROBE    : constant TimerOperatingMode := 4;
    MODE_5_HW_STROBE    : constant TimerOperatingMode := 5;
    MODE_2_RATE_GEN2    : constant TimerOperatingMode := 6;
    MODE_3_SQ_WAVE_GEN2 : constant TimerOperatingMode := 7;

    subtype TimerBinaryOrBCD is Natural range 0..1;
    TIMER_BINARY        : constant TimerBinaryOrBCD := 0;
    TIMER_BCD           : constant TimerBinaryOrBCD := 1;

    ---------------------------------------------------------------------------
    -- Format for the timer mode/command register
    ---------------------------------------------------------------------------
    type TimerCommand is
    record
        bcdbin : TimerBinaryOrBCD;
        mode : TimerOperatingMode;
        acc : TimerAccessMode;
        channel : TimerChannel;
    end record with Size => 8;

    for TimerCommand use
    record
        bcdbin  at 0 range 0 .. 0; 
        mode    at 0 range 1 .. 3;
        acc     at 0 range 4 .. 5;
        channel at 0 range 6 .. 7;
    end record;

    -- Conversion from TimerCommand record to a port-outputtable word.
    function toByte is new Ada.Unchecked_Conversion(TimerCommand, Unsigned_8);

    ---------------------------------------------------------------------------
    -- Initial PIT setup. Uses a 1ms default timer. (See MS_TIMER)
    ---------------------------------------------------------------------------
    procedure setupPIT;

    ---------------------------------------------------------------------------
    -- Enable the PIT in periodic mode by unmasking its interrupt on the PIC
    -- Note: does NOT enable CPU interrupts (sti)
    ---------------------------------------------------------------------------
    procedure enable;

    ---------------------------------------------------------------------------
    -- Disable the PIT by masking its interrupt on the PIC
    -- Note: does NOT disable CPU interrupts (cli)
    ---------------------------------------------------------------------------
    procedure disable;

end timer_pit;