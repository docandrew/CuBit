-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- Low-overhead kernel trace ring.
-------------------------------------------------------------------------------
with Config;
with PerCPUData;
with TextIO; use TextIO;
with x86;

package body Trace with
    SPARK_Mode => Off
is
    EVENTS_PER_CPU : constant Natural := 512;

    subtype CPUIndex is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
    subtype RingIndex is Natural range 0 .. EVENTS_PER_CPU - 1;
    subtype BucketIndex is Natural range 0 .. 11;

    type TraceEvent is record
        tsc   : Unsigned_64 := 0;
        pid   : Unsigned_64 := 0;
        event : EventKind := 0;
        arg0  : Unsigned_64 := 0;
        arg1  : Unsigned_64 := 0;
    end record;

    type TraceRing is array (RingIndex) of TraceEvent;
    type TraceRings is array (CPUIndex) of TraceRing;
    type TraceHeads is array (CPUIndex) of RingIndex;
    type EventCounters is array (CPUIndex, EventKind) of Unsigned_64;
    type TotalCounters is array (EventKind) of Unsigned_64;
    type Histograms is array (CPUIndex, EventKind, BucketIndex) of Unsigned_64;
    type TotalHistogram is array (BucketIndex) of Unsigned_64;

    enabled : Boolean := False with Volatile;
    rings   : TraceRings;
    heads   : TraceHeads := (others => 0);
    counts  : EventCounters := (others => (others => 0));
    hists   : Histograms := (others => (others => (others => 0)));

    bucketLimits : constant array (BucketIndex) of Unsigned_64 :=
        (1_000,
         2_000,
         5_000,
         10_000,
         20_000,
         50_000,
         100_000,
         200_000,
         500_000,
         1_000_000,
         2_000_000,
         Unsigned_64'Last);

    function CurrentCPU return CPUIndex is
        cpu : constant Natural := PerCPUData.getCPUNumber;
    begin
        if cpu in CPUIndex then
            return cpu;
        end if;

        return 0;
    end CurrentCPU;

    function EventName (event : EventKind) return String is
    begin
        case event is
            when EVENT_SYSCALL_ENTER =>
                return "syscall_enter";
            when EVENT_SCHEDULE_RUN =>
                return "schedule_run";
            when EVENT_SCHEDULE_STOP =>
                return "schedule_stop";
            when EVENT_SYSCALL_TIME =>
                return "syscall_tsc";
            when EVENT_RUN_TIME =>
                return "run_tsc";
            when EVENT_READY_LATENCY =>
                return "ready_latency_tsc";
            when others =>
                return "unknown";
        end case;
    end EventName;

    function BucketFor (ticks : Unsigned_64) return BucketIndex is
    begin
        for b in BucketIndex loop
            if ticks <= bucketLimits(b) then
                return b;
            end if;
        end loop;

        return BucketIndex'Last;
    end BucketFor;

    procedure Reset is
    begin
        enabled := False;

        for cpu in CPUIndex loop
            heads(cpu) := 0;
            for event in EventKind loop
                counts(cpu, event) := 0;
                for bucket in BucketIndex loop
                    hists(cpu, event, bucket) := 0;
                end loop;
            end loop;
        end loop;

        enabled := True;
    end Reset;

    procedure Disable is
    begin
        enabled := False;
    end Disable;

    procedure Emit
        (event : in EventKind;
         arg0  : in Unsigned_64 := 0;
         arg1  : in Unsigned_64 := 0)
    is
        cpu  : CPUIndex;
        head : RingIndex;
    begin
        if not enabled then
            return;
        end if;

        cpu := CurrentCPU;
        head := heads(cpu);

        rings(cpu)(head) :=
            (tsc   => x86.rdtsc,
             pid   => Unsigned_64 (PerCPUData.getCurrentPID),
             event => event,
             arg0  => arg0,
             arg1  => arg1);

        counts(cpu, event) := counts(cpu, event) + 1;

        if head = RingIndex'Last then
            heads(cpu) := RingIndex'First;
        else
            heads(cpu) := head + 1;
        end if;
    end Emit;

    procedure ObserveDuration
        (event : in EventKind;
         ticks : in Unsigned_64)
    is
        cpu : CPUIndex;
        bucket : BucketIndex;
    begin
        if not enabled then
            return;
        end if;

        cpu := CurrentCPU;
        bucket := BucketFor (ticks);
        hists(cpu, event, bucket) := hists(cpu, event, bucket) + 1;
    end ObserveDuration;

    procedure PrintSummary is
        totals : TotalCounters := (others => 0);
        totalEvents : Unsigned_64 := 0;
    begin
        Disable;

        for cpu in CPUIndex loop
            for event in EventKind loop
                totals(event) := totals(event) + counts(cpu, event);
            end loop;
        end loop;

        println ("TRACE: summary begin");
        for event in EventKind loop
            if totals(event) /= 0 then
                print ("TRACE: event=");
                print (EventName (event));
                print (" count=");
                printdln (totals(event));
                totalEvents := totalEvents + totals(event);
            end if;
        end loop;

        for event in EventKind loop
            declare
                hist : TotalHistogram := (others => 0);
                histTotal : Unsigned_64 := 0;
            begin
                for cpu in CPUIndex loop
                    for bucket in BucketIndex loop
                        hist(bucket) := hist(bucket) + hists(cpu, event, bucket);
                    end loop;
                end loop;

                for bucket in BucketIndex loop
                    histTotal := histTotal + hist(bucket);
                end loop;

                if histTotal /= 0 then
                    for bucket in BucketIndex loop
                        if hist(bucket) /= 0 then
                            print ("TRACE: hist=");
                            print (EventName (event));
                            if bucket = BucketIndex'Last then
                                print (" gt_tsc=");
                                printd (bucketLimits(bucket - 1));
                            else
                                print (" le_tsc=");
                                printd (bucketLimits(bucket));
                            end if;
                            print (" count=");
                            printdln (hist(bucket));
                        end if;
                    end loop;
                end if;
            end;
        end loop;

        for cpu in CPUIndex loop
            declare
                cpuEvents : Unsigned_64 := 0;
            begin
                for event in EventKind loop
                    cpuEvents := cpuEvents + counts(cpu, event);
                end loop;

                if cpuEvents /= 0 then
                    print ("TRACE: cpu=");
                    printd (Unsigned_64 (cpu));
                    print (" events=");
                    printd (cpuEvents);
                    print (" head=");
                    printd (Unsigned_64 (heads(cpu)));

                    declare
                        samples : Unsigned_64 := cpuEvents;
                        firstIdx : RingIndex;
                        lastIdx  : RingIndex;
                        span     : Unsigned_64 := 0;
                    begin
                        if samples > Unsigned_64 (EVENTS_PER_CPU) then
                            samples := Unsigned_64 (EVENTS_PER_CPU);
                        end if;

                        firstIdx := RingIndex
                            ((Natural (heads(cpu)) + EVENTS_PER_CPU -
                              Natural (samples)) mod EVENTS_PER_CPU);

                        if heads(cpu) = RingIndex'First then
                            lastIdx := RingIndex'Last;
                        else
                            lastIdx := heads(cpu) - 1;
                        end if;

                        if rings(cpu)(lastIdx).tsc >= rings(cpu)(firstIdx).tsc then
                            span := rings(cpu)(lastIdx).tsc -
                                    rings(cpu)(firstIdx).tsc;
                        end if;

                        print (" span_tsc=");
                        printdln (span);
                    end;
                end if;
            end;
        end loop;

        print ("TRACE: total=");
        printdln (totalEvents);
        println ("TRACE: summary end");
    end PrintSummary;
end Trace;
