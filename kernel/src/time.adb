-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Config;
with PerCPUData;
with Process;
with Process.IPC;
with Process.Queues;
with Scheduler_Timing;
with TextIO;
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
    cpuClock : array (0 .. Config.MAX_SMP_CPUS - 1) of
      Scheduler_Timing.Clock_State;
    referenceEpoch : Unsigned_64 := 0;

    procedure enableSchedulingClock with SPARK_Mode => Off -- boot TSC anchor
    is
        Stamp : constant Unsigned_64 := x86.readOrderedTSC;
    begin
        if tscPerDuration = 0 or else
          tscPerDuration > Unsigned_64 (Scheduler_Timing.Tick_Count'Last) / 1000
        then
            raise Program_Error with "Missing reference TSC calibration";
        end if;
        referenceEpoch := Stamp;
        for CPU in cpuClock'Range loop
            cpuClock(CPU) := Scheduler_Timing.Start
              (0, Scheduler_Timing.Tick_Rate (tscPerDuration * 1000));
        end loop;
        schedulingClock := True;
    end enableSchedulingClock;

    procedure clockTick with SPARK_Mode => Off -- live process queues and CPU state
    is
        cpuNum    : constant Natural := PerCPUData.getCPUNumber;
        currentPID : constant Process.ProcessID := PerCPUData.getCurrentPID;
        elapsed : Unsigned_64 := 1;
        valid : Boolean;
        clockDelta : Scheduler_Timing.Tick_Count;
    begin
        if schedulingClock then
            readClock : declare
                stamp : constant Unsigned_64 := x86.readOrderedTSC - referenceEpoch;
            begin
                elapsed := 0;
                valid := stamp <= Unsigned_64 (Scheduler_Timing.Tick_Count'Last);
                if valid then
                    Scheduler_Timing.Advance
                      (cpuClock(cpuNum), Scheduler_Timing.Tick_Count (stamp), clockDelta, valid);
                    elapsed := Unsigned_64 (clockDelta);
                end if;
                if not valid and then not clockFault then
                    clockFault := True;
                    TextIO.println ("CLOCK: FAIL reference counter outside monotonic epoch");
                end if;
            end readClock;
        end if;
        -- Only BSP handles global timekeeping and sleep list
        if cpuNum = 0 and then elapsed > 0 then
            Time.msTicks := Time.msTicks + elapsed;
            Process.Queues.clockTick (elapsed);
            Process.IPC.expireReceiveDeadlines (Time.msTicks);
        end if;

        -- Existing coarse CPU quota accounting remains at one-millisecond
        -- resolution; the faster timer IRQ must not charge it twice.
        -- This legacy yield-on-exhaustion mechanism is NOT a reservation.
        if currentPID /= Process.NO_PROCESS and then elapsed > 0 then
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

                    q.cpuUsedTicks := Natural (Unsigned_64'Min
                      (Unsigned_64 (Natural'Last), Process.Accounting.Saturating_Add
                         (Unsigned_64 (q.cpuUsedTicks), elapsed)));

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

        -- Rotation uses actual charged execution, not the wall-clock divider.
        -- Unfinished turns survive higher-priority interruptions; direct IPC
        -- transfers the existing CPU turn without replenishing it.
        if schedulingClock then
            Process.serviceTimerPreemption;
        end if;
    end clockTick;

end Time;
