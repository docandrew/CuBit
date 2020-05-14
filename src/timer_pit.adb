-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- PIT - Programmable Interval Timer - Intel 8253
--
-- For multiprocessor systems, this has been superceded by the APIC high-
--  precision timer.
-------------------------------------------------------------------------------
--with textmode; use textmode;
with InterruptNumbers;
with pic;

package body timer_pit
    with SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- Default PIT setting with 1ms ticks.
    ---------------------------------------------------------------------------
    MS_TIMER : TimerCommand := 
        (bcdbin => TIMER_BINARY, 
         mode => MODE_2_RATE_GEN,
         acc => ACCESS_WORD,
         channel => TIMER_CHANNEL_0);

    -- 1.193 Mhz / 1193 for 1ms interrupt rate
    ms_interval : Unsigned_16 := 1193;

    -- Needed to reset the timer in one-shot mode (PCs are weird)
    PCSpeakerPort : constant x86.IOPort := 16#61#;

    -- Set up for rate-generator, 1ms ticks
    procedure setupPIT is
    begin
        --set timer 0 to 16-bit counter, rate generator,
        --counter running in binary
        x86.out8(TimerModeReg, toByte(MS_TIMER));
        x86.out8(Timer0, Unsigned_8(16#FF# and ms_interval));
        x86.out8(Timer0, Unsigned_8(Shift_Right(ms_interval, 8)));
    end setupPIT;

    -- Unmask timer interrupt
    procedure enable is
    begin
        pic.enableIRQ(InterruptNumbers.TIMER);
    end enable;

    -- Mask timer interrupt
    procedure disable is
    begin
        pic.disableIRQ(InterruptNumbers.TIMER);
    end disable;

end timer_pit;