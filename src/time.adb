-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
with cpuid;
with x86;

package body time with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- bootCalibrationSleep - busy wait until tick difference matches up
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep(ms : in Unsigned_64)
        with SPARK_Mode => On
    is
        startTicks : Unsigned_64 := msTicks;
    begin
        while msTicks < startTicks + ms loop
            null;
        end loop;
    end bootCalibrationSleep;

    ---------------------------------------------------------------------------
    -- We get a tick every 1ms, so check TSC difference between ticks. I think
    -- 100ms should give us a decent average.
    --
    -- Use rdtscp to get stronger serialization guarantees, not sure if it
    -- matters.
    --
    -- TODO: need to find whether TSC is invariant or not.
    ---------------------------------------------------------------------------
    procedure calibrateTSC
        with SPARK_Mode => On
    is
        samplems    : Unsigned_64 := 100;
        startTicks  : Unsigned_64 := msTicks;
        startTSC    : TSCTicks := x86.rdtsc;
        endTSC      : TSCTicks;
        tscPerMilli : Unsigned_64;
    begin
        while msTicks < startTicks + samplems loop
            null;
        end loop;

        endTSC := x86.rdtsc;

        tscPerMilli         := (endTSC - startTSC) / samplems;
        tscPerDuration      := tscPerMilli / 1_000;
        tscCalibrated := True;
    end calibrateTSC;

    -- function tscToMilliseconds(tsc : TSCTicks) return Milliseconds is 
    --     (tsc / tscPerMillisecond) with Inline;

    -- function tscToMicroseconds(tsc : TSCTicks) return Microseconds is
    --     (tsc / tscPerMicrosecond) with Inline;

    ---------------------------------------------------------------------------
    -- busy sleeps
    ---------------------------------------------------------------------------
    -- procedure sleep(ms : in Milliseconds) is
    --     startTicks  : TSCTicks := x86.rdtsc;
    --     endTicks    : TSCTicks := startTicks + (tscPerMillisecond * ms);
    -- begin
    --     while x86.rdtsc < endTicks loop
    --         null;
    --     end loop;
    -- end sleep;

    procedure sleep(d : in Duration) is
        startTicks  : TSCTicks := x86.rdtsc;
        endTicks    : TSCTicks := startTicks + (tscPerDuration * d);
    begin
        while x86.rdtsc < endTicks loop
            null;
        end loop;
    end sleep;

end time;