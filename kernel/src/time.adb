-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- General functions and data structures for time-keeping.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with Config;
with CPUID;
with Boot_Timer_Diagnostics;
with PerCPUData;
with Process;
with Process.IPC;
with Process.Stuck_Calls;
with Process.Wait_Timers;
with VM_Objects;
with Process.Queues;
with Scheduler_Timing;
with Scheduler_Alarm;
with Virtual_Deadlines;
with TextIO;
with x86;
with Platform_Monotonic;
with Clock_Page;
with Clock_Publication;
with Spinlocks;
with Timekeeping_Turns;
with Msr_Safe;

package body Time with
    SPARK_Mode => On
is
    procedure Read_Monotonic (Microseconds : out Unsigned_64;
                             Success : out Boolean) with SPARK_Mode => Off is
        Nanoseconds : Unsigned_64;
    begin
        -- One epoch with the clock publication and msTicks when user space
        -- has the page; the HPET's own epoch otherwise.
        if Clock_Page.Shared then
            Clock_Page.Read (x86.readOrderedTSC, Nanoseconds, Success);
            Microseconds := Clock_Publication.Microseconds (Nanoseconds);
        else
            Platform_Monotonic.Read (Microseconds, Success);
        end if;
    end Read_Monotonic;

    procedure Read_User_Monotonic (Microseconds : out Unsigned_64;
                                   Success : out Boolean) with SPARK_Mode => Off is
        Ticks : constant Unsigned_64 := msTicks;
    begin
        if Clock_Page.Shared then
            Read_Monotonic (Microseconds, Success);
        elsif Ticks > Unsigned_64'Last / Milliseconds then
            Microseconds := 0;
            Success := False;
        else
            Microseconds := Ticks * Milliseconds;
            Success := True;
        end if;
    end Read_User_Monotonic;

    function Try_CPU_TSC return Boolean with SPARK_Mode => Off is
        Rate : Boot_Timer_Rates.Frequency;
    begin
        -- The direct-frequency path requires Intel's architectural leaf and
        -- an invariant TSC. Other CPUs keep the bounded reference fallback.
        if CPUID.cpuVendor /= "GenuineIntel" or else not CPUID.hasInvariantTSC
          or else CPUID.getMaxStandardFunction < 16#15#
        then
            return False;
        end if;
        Rate := Boot_Timer_Rates.From_CPUID
          (CPUID.tscRatioDenominator, CPUID.tscRatioNumerator, CPUID.crystalClockHz);
        if Rate = 0 then return False; end if;
        tscFrequencyHz := Rate;
        tscPerDuration := Rate / 1_000_000;
        tscCalibrated := True;
        return True;
    end Try_CPU_TSC;

    ---------------------------------------------------------------------------
    -- bootCalibrationSleep - busy wait until tick difference matches up
    ---------------------------------------------------------------------------
    procedure bootCalibrationSleep (ms : in Unsigned_64)
        with SPARK_Mode => Off -- asynchronous hardware tick polling
    is
    begin
        Boot_Timer_Diagnostics.Wait_For_Ticks (ms);
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
        startTSC    : constant TSCTicks := x86.rdtsc;
        endTSC      : TSCTicks;
        Rate : Unsigned_64;
    begin
        bootCalibrationSleep (samplems);

        endTSC := x86.rdtsc;

        Rate := (endTSC - startTSC) / samplems * 1000;
        if Rate not in Boot_Timer_Rates.Valid_Frequency then
            raise Program_Error with "PIT reference produced an invalid TSC rate";
        end if;
        tscFrequencyHz := Rate;
        tscPerDuration := Rate / 1_000_000;
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
    -- Per-CPU timer handler. Any CPU expires due sleepers; the system's time
    -- and its deadlines have one keeper at a time (Timekeeping_Turns). All
    -- CPUs divide the timer into clock and scheduling opportunities.
    ---------------------------------------------------------------------------
    schedulingClock : Boolean := False;
    -- Held by the CPU keeping time on this interrupt; another CPU's timer
    -- that finds it held skips the work, which the holder is doing.
    timekeeper : Spinlocks.Spinlock;
    -- msTicks when CPU 0's timer last ran its clock work, and whether
    -- another CPU has reported it missing since.
    bootCPULastRun : Unsigned_64 := 0 with Atomic;
    bootCPUSilenceReported : Boolean := False with Atomic;
    -- Scheduling has begun on CPU 0 (its timer has interrupted a thread).
    -- Boot runs long stretches with interrupts masked on CPU 0 before
    -- that, which are no stall: silences and jumps are reported only after.
    watching : Boolean := False with Atomic;
    -- The clock page refused a keeper's counter (reported once).
    conversionRefusedReported : Boolean := False with Atomic;

    -- MSR_SMI_COUNT (Intel, Silvermont and later): SMIs this CPU took. A
    -- jump of the time with SMIs is the machine in SMM; without, some other
    -- whole-machine stall. Read by each keeper tick on its own CPU (one
    -- RDMSR a millisecond), through Msr_Safe: a CPU without it refuses once
    -- and it is not read again.
    MSR_SMI_COUNT : constant Unsigned_32 := 16#34#;
    type SMI_Reading is (Untried, Counting, Absent);
    smiState : SMI_Reading := Untried with Atomic;
    smiLast : array (0 .. Config.MAX_SMP_CPUS - 1) of Unsigned_64 := [others => 0];

    -- SMIs on CPU since its previous keeper tick (0 without the MSR).
    function smiDelta (CPU : Natural) return Unsigned_64 with SPARK_Mode => Off is
        Value : Unsigned_64;
        Ok : Boolean;
        Previous : Unsigned_64;
    begin
        if smiState = Absent or else CPU not in smiLast'Range then
            return 0;
        elsif smiState = Untried and then CPUID.cpuVendor /= "GenuineIntel" then
            smiState := Absent;
            TextIO.println ("CLOCK: no SMI count (MSR_SMI_COUNT is Intel's)");
            return 0;
        end if;
        Msr_Safe.Read (MSR_SMI_COUNT, Value, Ok);
        if not Ok then
            smiState := Absent;
            TextIO.println ("CLOCK: no SMI count (MSR_SMI_COUNT refused)");
            return 0;
        end if;
        if smiState = Untried then
            smiState := Counting;
            TextIO.println ("CLOCK: SMI count at the first keeper tick" & Unsigned_64'Image (Value) &
                            " (CPU" & Natural'Image (CPU) & ")");
            smiLast (CPU) := Value;
        end if;
        Previous := smiLast (CPU);
        smiLast (CPU) := Value;
        return (if Value >= Previous then Value - Previous else 0);
    end smiDelta;
    -- The thread CPU 0's timer interrupted at that last run.
    bootCPULastThread : Process.ThreadID := Process.NO_THREAD with Atomic;
    -- Boot_CPU_Timer_Interrupts at that last run.
    bootCPULastInterrupts : Unsigned_64 := 0 with Atomic;
    -- CPU 0's SMI count as its last keeper tick read it.
    bootCPULastSMIs : Unsigned_64 := 0 with Atomic;

    -- "thread T of name (rip R)" for the silence report: unlocked reads of a
    -- record that may be torn or gone, for diagnosis only.
    function threadText (T : Process.ThreadID) return String
      with SPARK_Mode => Off
    is
        Hex : constant String := "0123456789ABCDEF";
        Digits16 : String (1 .. 16);
        RIP : Unsigned_64;
        PID : Process.ProcessID;
        Last : Natural := 0;
        use type Process.ThreadID;
    begin
        if T = Process.NO_THREAD or else not Process.Thread_Table.Present (Natural (T)) then
            return "no thread";
        end if;
        PID := Process.threadtab (T).process;
        RIP := Process.threadtab (T).sampledRIP;
        for I in Digits16'Range loop
            Digits16 (I) := Hex (Natural (Shift_Right (RIP, (16 - I) * 4) and 16#F#) + 1);
        end loop;
        if PID = Process.NO_PROCESS then
            return "thread" & Process.ThreadID'Image (T) & " (rip " & Digits16 & ")";
        end if;
        declare
            Name : constant Process.ProcessName := Process.proctab (PID).name;
        begin
            for I in Name'Range loop
                exit when Name (I) = ASCII.NUL;
                if Name (I) /= ' ' then Last := I; end if;
            end loop;
            return "thread" & Process.ThreadID'Image (T) & " of " & Name (Name'First .. Last) &
                   " (user rip " & Digits16 & ")";
        end;
    end threadText;

    function Boot_CPU_Silence_Ms return Unsigned_64 with SPARK_Mode => Off is
        Last : constant Unsigned_64 := bootCPULastRun;
        Now : constant Unsigned_64 := msTicks;
    begin
        return (if Timekeeping_Turns.Silent (Last, Now)
                then Timekeeping_Turns.Silence (Last, Now) else 0);
    end Boot_CPU_Silence_Ms;

    -- The system's time and the deadlines measured on it, by the keeper of
    -- this interrupt (timekeeper held). msTicks is the clock publication's
    -- time at Counter, so a reader of the page is never behind it
    -- (docs/fast-clock.md); without a conversion, CPU 0's Elapsed ticks.
    procedure keepTime (CPU : Natural; Counter, Elapsed : Unsigned_64)
      with SPARK_Mode => Off -- live process queues
    is
        previousMs : constant Unsigned_64 := Time.msTicks;
        publishedNs : Unsigned_64;
        valid : Boolean;
        armed : Boolean;
        alarmAt : Unsigned_64;
        smis : constant Unsigned_64 := smiDelta (CPU);
    begin
        if schedulingClock and then Clock_Page.Converting then
            Clock_Page.Read (Counter, publishedNs, valid);
            Time.msTicks := Timekeeping_Turns.Next
              (previousMs,
               (if valid then Clock_Publication.Milliseconds (publishedNs) else 0),
               valid);
            if not valid and then not conversionRefusedReported then
                -- msTicks holds while the page cannot convert this counter.
                conversionRefusedReported := True;
                TextIO.println ("CLOCK: FAIL the clock page cannot convert CPU" &
                  Natural'Image (CPU) & "'s counter" & Unsigned_64'Image (Counter) &
                  "; msTicks holds at" & Unsigned_64'Image (previousMs));
                Process.Stuck_Calls.noteEvidence ("CLOCK C: the clock page refused CPU" & Natural'Image (CPU) &
                  "'s counter; msTicks held at" & Unsigned_64'Image (previousMs));
            end if;
        elsif CPU = Timekeeping_Turns.Boot_CPU then
            Time.msTicks := Timekeeping_Turns.Advanced (previousMs, Elapsed);
        end if;
        if watching and then Timekeeping_Turns.Silent (previousMs, Time.msTicks) then
            -- No keeper ran for that long: every CPU's timer was stopped
            -- (the machine in SMM, say), or CPU 0's without a shared clock.
            TextIO.println ("CLOCK: the system's time jumped" &
              Unsigned_64'Image (Time.msTicks - previousMs) & " ms at one tick of CPU" &
              Natural'Image (CPU) & "; no CPU kept time from" & Unsigned_64'Image (previousMs) &
              " ms; SMIs during the jump:" &
              (if smiState = Counting then Unsigned_64'Image (smis) else " unknown"));
            Process.Stuck_Calls.noteEvidence ("CLOCK B: time jumped" &
              Unsigned_64'Image (Time.msTicks - previousMs) & " ms at" &
              Unsigned_64'Image (previousMs) & " ms, no CPU ran; SMIs during the jump:" &
              (if smiState = Counting then Unsigned_64'Image (smis) else " unknown"));
        end if;
        if Time.msTicks /= previousMs then
            if watching and then CPU /= Timekeeping_Turns.Boot_CPU and then not bootCPUSilenceReported
              and then Timekeeping_Turns.Silent (bootCPULastRun, Time.msTicks)
            then
                bootCPUSilenceReported := True;
                -- What CPU 0's timer was set to (a lost or unarmed timer,
                -- or one set far ahead) and whether it was running a thread.
                Scheduler_Alarm.Peek (Timekeeping_Turns.Boot_CPU, armed, alarmAt);
                TextIO.println ("CLOCK: CPU 0 ran no timer work for" &
                  Unsigned_64'Image (Timekeeping_Turns.Silence (bootCPULastRun, Time.msTicks)) &
                  " ms; CPU" & Natural'Image (CPU) & " keeps the system's time; CPU 0 " &
                  (if Process.cpuRunningKey (Timekeeping_Turns.Boot_CPU) = Virtual_Deadlines.Idle_Key
                   then "idle" else "running a thread") & ", last interrupted " &
                  threadText (bootCPULastThread) & ";" &
                  Unsigned_64'Image (Boot_CPU_Timer_Interrupts - bootCPULastInterrupts) &
                  " timer interrupts since, its alarm " &
                  (if not armed then "unarmed"
                   elsif tscPerDuration = 0 then "armed"
                   elsif alarmAt <= Counter then
                     "due" & Unsigned_64'Image ((Counter - alarmAt) / tscPerDuration / Milliseconds) & " ms ago"
                   else
                     "due in" & Unsigned_64'Image ((alarmAt - Counter) / tscPerDuration / Milliseconds) & " ms") &
                  "; SMIs on CPU" & Natural'Image (CPU) & " since its last keeper tick:" &
                  (if smiState = Counting then Unsigned_64'Image (smis) else " unknown"));
                Process.Stuck_Calls.noteEvidence ("CLOCK A: CPU 0 silent from" &
                  Unsigned_64'Image (bootCPULastRun) & " ms, " &
                  (if Process.cpuRunningKey (Timekeeping_Turns.Boot_CPU) = Virtual_Deadlines.Idle_Key
                   then "idle" else "busy") & ", alarm " &
                  (if not armed then "unarmed"
                   elsif alarmAt <= Counter then "past due" else "ahead") & "," &
                  Unsigned_64'Image (Boot_CPU_Timer_Interrupts - bootCPULastInterrupts) &
                  " timer irqs, last " & threadText (bootCPULastThread));
            end if;
            Clock_Page.Note_Milliseconds (Time.msTicks);
            Process.IPC.expireReceiveDeadlines (Time.msTicks);
            if Time.msTicks / Process.Stuck_Calls.Stuck_Report_Period_Ms /=
               previousMs / Process.Stuck_Calls.Stuck_Report_Period_Ms
            then
                Process.Stuck_Calls.reportStuckCalls (Time.msTicks);
            end if;
            VM_Objects.Expire_Halts (Time.msTicks);
        end if;
    end keepTime;
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
        -- msTicks continues from here on the clock publication's conversion.
        Clock_Page.Publish
          (Frequency         => tscFrequencyHz,
           Base_Ticks        => Stamp,
           Base_Milliseconds => msTicks,
           Invariant         => CPUID.hasInvariantTSC);
        for CPU in cpuClock'Range loop
            cpuClock(CPU) := Scheduler_Timing.Start
              (0, Scheduler_Timing.Tick_Rate (tscFrequencyHz / 1000));
        end loop;
        schedulingClock := True;
    end enableSchedulingClock;

    ---------------------------------------------------------------------------
    -- Image 39 (laptop diagnostics only): does every CPU read one clock?
    -- Each CPU logs IA32_TSC_ADJUST once at its first tick, and how far its
    -- own TSC's millisecond (through the clock page) is from msTicks; CPU 0
    -- prints each CPU's lag range every 10 s. Unlocked, racy by design.
    ---------------------------------------------------------------------------
    IA32_TSC_ADJUST : constant Unsigned_32 := 16#3B#;
    TSC_Report_Period_Ms : constant := 10_000;
    type Lag_Array is array (0 .. Config.MAX_SMP_CPUS - 1) of Integer_64;
    tscSeen : array (0 .. Config.MAX_SMP_CPUS - 1) of Boolean := [others => False];
    lagMin : Lag_Array := [others => Integer_64'Last];
    lagMax : Lag_Array := [others => Integer_64'First];
    lagSamples : array (0 .. Config.MAX_SMP_CPUS - 1) of Unsigned_64 := [others => 0];
    tscLastReport : Unsigned_64 := 0;

    function To_Signed is new Ada.Unchecked_Conversion (Unsigned_64, Integer_64);

    procedure noteTSC (CPU : Natural; Counter : Unsigned_64) with SPARK_Mode => Off is
        Adjust : Unsigned_64 := 0;
        Ok : Boolean := False;
        Ns : Unsigned_64;
        Valid : Boolean;
        Lag : Integer_64;
        Now : constant Unsigned_64 := Time.msTicks;
    begin
        if CPU not in tscSeen'Range then
            return;
        end if;
        if not tscSeen (CPU) then
            tscSeen (CPU) := True;
            Msr_Safe.Read (IA32_TSC_ADJUST, Adjust, Ok);
            TextIO.println ("TSC: CPU" & Natural'Image (CPU) & " first tick: TSC_ADJUST " &
              (if Ok then Integer_64'Image (To_Signed (Adjust)) else " unreadable") &
              ", TSC" & Unsigned_64'Image (Counter) & ", msTicks" & Unsigned_64'Image (Now));
        end if;
        if not Clock_Page.Converting then
            return;
        end if;
        Clock_Page.Read (Counter, Ns, Valid);
        if Valid then
            Lag := Integer_64 (Now) - Integer_64 (Clock_Publication.Milliseconds (Ns));
            if Lag < lagMin (CPU) then lagMin (CPU) := Lag; end if;
            if Lag > lagMax (CPU) then lagMax (CPU) := Lag; end if;
            lagSamples (CPU) := lagSamples (CPU) + 1;
        end if;
        if CPU = Timekeeping_Turns.Boot_CPU and then Now >= tscLastReport + TSC_Report_Period_Ms then
            tscLastReport := Now;
            for C in lagSamples'Range loop
                if lagSamples (C) > 0 then
                    TextIO.println ("TSC: CPU" & Natural'Image (C) & " msTicks minus its own clock:" &
                      Integer_64'Image (lagMin (C)) & " .." & Integer_64'Image (lagMax (C)) &
                      " ms over" & Unsigned_64'Image (lagSamples (C)) & " ticks");
                    lagMin (C) := Integer_64'Last;
                    lagMax (C) := Integer_64'First;
                    lagSamples (C) := 0;
                end if;
            end loop;
        end if;
    end noteTSC;

    procedure clockTick with SPARK_Mode => Off -- live process queues and CPU state
    is
        cpuNum    : constant Natural := PerCPUData.getCPUNumber;
        currentPID : constant Process.ProcessID := PerCPUData.getCurrentPID;
        elapsed : Unsigned_64 := 1;
        valid : Boolean;
        clockDelta : Scheduler_Timing.Tick_Count;
        counter : Unsigned_64 := 0;
        publishedNs : Unsigned_64;
        due, kept : Boolean;
    begin
        if schedulingClock then
            counter := x86.readOrderedTSC;
            noteTSC (cpuNum, counter);
            readClock : declare
                stamp : constant Unsigned_64 := counter - referenceEpoch;
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
        -- This CPU's deadlines (sleeps and one-object Waits, IPC revision 2):
        -- each CPU's own heap, its alarm armed for the earliest.
        Process.Wait_Timers.Expire (cpuNum, x86.rdtsc);
        -- The system's time and its deadlines: CPU 0's timer, or any CPU's
        -- where all CPUs read one clock, so a CPU 0 that takes no timer
        -- interrupt (masked, busy, or its timer lost) stops neither.
        -- Another CPU first checks, without the lock, that the published
        -- millisecond is past msTicks: one keeper per millisecond.
        if Timekeeping_Turns.May_Keep
             (cpuNum, schedulingClock and then Clock_Page.Shared)
        then
            due := cpuNum = Timekeeping_Turns.Boot_CPU;
            if not due then
                Clock_Page.Read (counter, publishedNs, valid);
                due := valid and then
                  Clock_Publication.Milliseconds (publishedNs) > Time.msTicks;
            end if;
            if due then
                Spinlocks.tryEnterCriticalSection (timekeeper, kept);
                if kept then
                    keepTime (cpuNum, counter, elapsed);
                    Spinlocks.exitCriticalSection (timekeeper);
                end if;
            end if;
        end if;
        if cpuNum = Timekeeping_Turns.Boot_CPU then
            if bootCPUSilenceReported then
                bootCPUSilenceReported := False;
                declare
                    Value : Unsigned_64 := 0;
                    Ok : Boolean := False;
                begin
                    if smiState = Counting then
                        Msr_Safe.Read (MSR_SMI_COUNT, Value, Ok);
                    end if;
                    TextIO.println ("CLOCK: CPU 0 runs its timer work again after" &
                      Unsigned_64'Image (Timekeeping_Turns.Silence (bootCPULastRun, Time.msTicks)) &
                      " ms;" & Unsigned_64'Image (Boot_CPU_Timer_Interrupts - bootCPULastInterrupts) &
                      " timer interrupts meanwhile, now in " & threadText (PerCPUData.getCurrentThread) &
                      "; SMIs on CPU 0 meanwhile:" &
                      (if Ok and then Value >= bootCPULastSMIs
                       then Unsigned_64'Image (Value - bootCPULastSMIs) else " unknown"));
                    Process.Stuck_Calls.noteEvidence ("CLOCK A: CPU 0 back after" &
                      Unsigned_64'Image (Timekeeping_Turns.Silence (bootCPULastRun, Time.msTicks)) &
                      " ms," & Unsigned_64'Image (Boot_CPU_Timer_Interrupts - bootCPULastInterrupts) &
                      " timer irqs; SMIs" &
                      (if Ok and then Value >= bootCPULastSMIs
                       then Unsigned_64'Image (Value - bootCPULastSMIs) else " unknown"));
                end;
            end if;
            bootCPULastRun := Time.msTicks;
            bootCPULastThread := PerCPUData.getCurrentThread;
            if Process."/=" (bootCPULastThread, Process.NO_THREAD) then
                watching := True;
            end if;
            bootCPULastInterrupts := Boot_CPU_Timer_Interrupts;
            bootCPULastSMIs := smiLast (Timekeeping_Turns.Boot_CPU);
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
