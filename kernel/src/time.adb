-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
with Config;
with cpuid;
with PerCPUData;
with Process;
with Process.Queues;
with x86;

package body Time with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- bootCalibrationSleep - busy wait until tick difference matches up
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep (ms : in Unsigned_64)
        with SPARK_Mode => On
    is
        startTicks : constant Unsigned_64 := msTicks;
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
        samplems    : constant Unsigned_64 := 100;
        startTicks  : constant Unsigned_64 := msTicks;
        startTSC    : constant TSCTicks := x86.rdtsc;
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

    procedure sleep (d : in Duration) is
        startTicks  : constant TSCTicks := x86.rdtsc;
        endTicks    : constant TSCTicks := startTicks + (tscPerDuration * d);
    begin
        while x86.rdtsc < endTicks loop
            null;
        end loop;
    end sleep;

    ---------------------------------------------------------------------------
    -- clockTick
    ---------------------------------------------------------------------------
    procedure clockTick with SPARK_Mode => On
    is
    begin
        -- possible overflow in geologic time scales.
        Time.msTicks := Time.msTicks + 1;

        -- Adjust sleep list, wake any processes that need it
        Process.Queues.clockTick;

        -- If we're at the quantum, yield the current process.
        if Time.msTicks mod Config.TIME_SLICE = 0 and 
            PerCPUData.getCurrentPID /= Process.NO_PROCESS then
                Process.yield;
        end if;
    end clockTick;

end Time;
