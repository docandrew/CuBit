-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Config;
with cpuid;
with PerCPUData;
with Process;
with Process.IPC;
with Process.Queues;
with Scheduler_Timing;
with x86;

package body Time with
    SPARK_Mode => On
is
    ---------------------------------------------------------------------------
    -- bootCalibrationSleep - busy wait until tick difference matches up
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep (ms : in Unsigned_64)
        with SPARK_Mode => Off -- asynchronous hardware tick polling
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
        with SPARK_Mode => Off -- asynchronous hardware tick/TSC sampling
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
    -- Per-CPU timer handler. Only BSP (CPU 0) manages global time and sleep
    -- list. All CPUs divide the timer into clock and scheduling opportunities.
    ---------------------------------------------------------------------------
    schedulingClock : Boolean := False;
    cpuTickPhase : array (0 .. Config.MAX_SMP_CPUS - 1) of
      Scheduler_Timing.Tick_Phase := [others => <>];

    procedure enableSchedulingClock is
    begin
        schedulingClock := True;
    end enableSchedulingClock;

    procedure clockTick with SPARK_Mode => Off -- live process queues and CPU state
    is
        cpuNum    : constant Natural := PerCPUData.getCPUNumber;
        currentPID : constant Process.ProcessID := PerCPUData.getCurrentPID;
        millisecond : Boolean := True;
        quantum : Boolean := False;
    begin
        if schedulingClock then
            Scheduler_Timing.Advance
              (cpuTickPhase (cpuNum), millisecond, quantum);
        end if;
        -- Only BSP handles global timekeeping and sleep list
        if cpuNum = 0 and then millisecond then
            Time.msTicks := Time.msTicks + 1;
            Process.Queues.clockTick;
            Process.IPC.expireReceiveDeadlines (Time.msTicks);
        end if;

        -- Existing coarse CPU quota accounting remains at one-millisecond
        -- resolution; the faster timer IRQ must not charge it twice.
        -- This legacy yield-on-exhaustion mechanism is NOT a reservation.
        if currentPID /= Process.NO_PROCESS and then millisecond then
            checkQuota : declare
                q : Process.ResourceQuota renames
                    Process.proctab(currentPID).quota;
            begin
                if q.cpuQuotaUs > 0 then
                    -- Check for new period (each tick = 1ms = 1000us)
                    if q.cpuPeriodUs > 0 and then
                       (Time.msTicks - q.periodStartTick) >=
                       Unsigned_64 (q.cpuPeriodUs / 1000)
                    then
                        q.cpuUsedTicks    := 0;
                        q.periodStartTick := Time.msTicks;
                    end if;

                    q.cpuUsedTicks := q.cpuUsedTicks + 1;

                    -- Exceeded quota for this period? Force yield.
                    if q.cpuUsedTicks >=
                       Natural (q.cpuQuotaUs / 1000)
                    then
                        Process.yield;
                        return;
                    end if;
                end if;
            end checkQuota;
        end if;

        -- FIFO rotation among equal-priority peers every 1.5 ms. No syscall,
        -- block/wake or direct IPC handoff resets this CPU-owned opportunity.
        -- Avoid a context switch when only lower-priority/idle work is ready.
        -- Latency hints remain advisory; this creates no priority authority.
        if quantum and then currentPID /= Process.NO_PROCESS and then
           Process.Queues.hasReadyPeer
             (Process.cpuReadyLists(cpuNum), Process.proctab(currentPID).priority)
        then
            Process.yield;
        end if;
    end clockTick;

end Time;
