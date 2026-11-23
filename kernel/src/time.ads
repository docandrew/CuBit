-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Boot_Timer_Rates;

package Time with
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

    -- High-resolution monotonic backend; independent of UTC and msTicks.
    -- False means unavailable. Do not infer physical accuracy from units.
    procedure Read_Monotonic (Microseconds : out Unsigned_64;
                             Success : out Boolean) with SPARK_Mode => Off;

    -- The clock user space reads (CuBit.Monotonic, CuBit.Published_Clock):
    -- the clock publication's conversion when user space has it
    -- (Clock_Page.Shared), else msTicks, the page's millisecond word, in
    -- microseconds. Timer waits (Wait_ABI.Timer) take their absolute
    -- instants on this clock. In-kernel busy waits keep Read_Monotonic: its
    -- HPET fallback advances with interrupts off, msTicks does not.
    procedure Read_User_Monotonic (Microseconds : out Unsigned_64;
                                   Success : out Boolean) with SPARK_Mode => Off;

    ---------------------------------------------------------------------------
    -- msTicks is a running count, updated by the interruptHandler.
    ---------------------------------------------------------------------------
    msTicks             : Unsigned_64 := 0 with Volatile;

    ---------------------------------------------------------------------------
    -- TSC ticks per time duration
    ---------------------------------------------------------------------------
    tscPerDuration      : TSCTicks := 0;
    tscFrequencyHz     : Boot_Timer_Rates.Frequency := 0;
    -- BSP only, before interrupts/APs. False leaves calibration unchanged.
    function Try_CPU_TSC return Boolean with SPARK_Mode => Off, No_Inline;

    tscCalibrated       : Boolean := False with Ghost;
    clockFault          : Boolean := False with Atomic;

    ---------------------------------------------------------------------------
    -- bootCalibrationSleep
    -- This will wait in a loop until msTicks (updated by the interruptHandler)
    -- reaches the number desired. This function is intended for use early in 
    -- the boot process to calibrate other timing sources.
    --
    -- IMPORTANT: A timer must be active and PIC _interrupts enabled_ for this
    --  to work.
    --  Missing progress fails with a bounded diagnostic rather than hanging.
    -- 
    -- @param ms - number of milliseconds to sleep
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep (ms : in Unsigned_64);

    ---------------------------------------------------------------------------
    -- calibrateTSC
    -- Bounded PIT fallback, only when Try_CPU_TSC could not establish a rate.
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
    procedure sleep (d : in Duration) with
        Pre => tscCalibrated;

    ---------------------------------------------------------------------------
    -- clockTick
    -- At timer interrupt intervals, this procedure expires due sleepers,
    -- keeps the system's time when this CPU may (Timekeeping_Turns), and
    -- offers ready peers a scheduling
    -- opportunity on turn exhaustion. Elapsed time comes from the calibrated
    -- TSC, not counting 250-us LAPIC interrupts, which may be delayed/coalesced.
    -- The wake-aware experiment can offer an earlier FIFO rotation when newly
    -- awakened work waits, without granting a priority boost.
    ---------------------------------------------------------------------------
    procedure clockTick with SPARK_Mode => On;

    -- Timer interrupts CPU 0 took, due or not (Interrupts, CPU 0 only): the
    -- silence report tells an undelivered timer from one set far ahead.
    Boot_CPU_Timer_Interrupts : Unsigned_64 := 0 with Atomic;

    -- How long CPU 0's timer has run no clock work while another CPU kept
    -- the time (Timekeeping_Turns.Silence_Report_Ms or more), else 0.
    function Boot_CPU_Silence_Ms return Unsigned_64 with SPARK_Mode => Off;

    -- BSP boot only, interrupts disabled, after masking the calibration PIT
    -- and before starting APs. Public time/sleep units remain milliseconds.
    procedure enableSchedulingClock;

end Time;
