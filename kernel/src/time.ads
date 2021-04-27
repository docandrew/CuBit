-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package time with
    SPARK_Mode => On
is
    subtype POSIXTime is Unsigned_32;

    -- Each Duration unit is a single microsecond
    subtype Duration is Unsigned_64;
    Microseconds            : constant Duration := 1;
    Milliseconds            : constant Duration := 1000 * Microseconds;
    Seconds                 : constant Duration := 1000 * Milliseconds;
    Minutes                 : constant Duration := 60 * Seconds;
    Hours                   : constant Duration := 60 * Minutes;

    subtype TSCTicks        is Unsigned_64;

    ---------------------------------------------------------------------------
    -- msTicks is a running count, updated by the interruptHandler.
    ---------------------------------------------------------------------------
    msTicks             : Unsigned_64 := 0;

    ---------------------------------------------------------------------------
    -- TSC ticks per time duration
    ---------------------------------------------------------------------------
    tscPerDuration      : TSCTicks;

    tscCalibrated       : Boolean := False with Ghost;

    ---------------------------------------------------------------------------
    -- bootCalibrationSleep
    -- This will wait in a loop until msTicks (updated by the interruptHandler)
    -- reaches the number desired. This function is intended for use early in 
    -- the boot process to calibrate other timing sources.
    --
    -- IMPORTANT: A timer must be active and PIC _interrupts enabled_ for this
    --  to work.
    -- 
    -- @param ms - number of milliseconds to sleep
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep(ms : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- calibrateTSC
    -- Use bootCalibrationSleep to determine the time interval between
    -- successive TSC ticks.
    ---------------------------------------------------------------------------
    procedure calibrateTSC with
        Post => tscCalibrated;

    ---------------------------------------------------------------------------
    -- sleep
    -- Perform a busy wait for a certain duration of time to elapse, as 
    -- measured by the CPU Time-Stamp Counter. Note that very small durations 
    -- < 100uS may be inaccurate depending on the resolution of the underlying
    -- clock.
    --
    -- @param d - duration to sleep
    ---------------------------------------------------------------------------
    procedure sleep(d : in Duration) with
        Pre => tscCalibrated;

end time;