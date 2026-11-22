with Interfaces; use Interfaces;
with System.Machine_Code; use System.Machine_Code;
with CuBit.Timing_Histograms;
with PerCPUData;
with Spinlocks;
with TextIO;
with Time;
with TLB_Shootdown;
with x86;

package body Spinlock_Benchmark is
    package Hist renames CuBit.Timing_Histograms;
    subtype CPU_Index is Natural range 0 .. 3;
    type Workload is (Private_Lock, Shared_Lock);
    type Measurement is (Throughput, Acquisition_Latency);
    Iterations : constant := 50_000;
    Repetitions : constant := 3;

    -- The small kernel runtime does not preserve arbitrary enumeration image
    -- tables. Keep the wire/report labels explicit, not ordinal magic numbers.
    function Name (Mode : Workload) return String is
      (case Mode is when Private_Lock => "PRIVATE_LOCK", when Shared_Lock => "SHARED_LOCK");
    function Name (Measure : Measurement) return String is
      (case Measure is when Throughput => "THROUGHPUT",
                       when Acquisition_Latency => "ACQUISITION_LATENCY");

    -- Separate cache lines for private locks, payloads and per-CPU results.
    -- Never Atomic: these ordinary stores must be published by the lock.
    type Protected_Data is limited record
        Counter : Unsigned_64 := 0;
        Mirror : Unsigned_64 := not Unsigned_64'(0);
    end record with Alignment => 64;
    type Padded_Lock is record
        Lock : Spinlocks.Spinlock;
    end record with Alignment => 64;
    type Result_Record is record
        Started, Finished : Unsigned_64 := 0;
        Errors : Natural := 0;
        Samples : Hist.Histogram;
    end record with Alignment => 64;
    type Signal_Record is record
        Value : Natural := 0 with Atomic;
    end record with Alignment => 64;
    Shared : Padded_Lock;
    Shared_Data : Protected_Data;
    Private_Locks : array (CPU_Index) of Padded_Lock;
    Private_Data : array (CPU_Index) of Protected_Data;
    Results : array (CPU_Index) of Result_Record;
    Ready, Done : array (CPU_Index) of Signal_Record;
    Go : Natural := 0 with Atomic;
    Consumed : Natural := 0 with Atomic;

    procedure Poll (CPU : CPU_Index) is
    begin
        TLB_Shootdown.Service (CPU);
        Asm ("pause", Volatile => True, Clobber => "memory");
    end Poll;

    procedure Finish (Passed : Boolean) with No_Return is
    begin
        TextIO.println (if Passed then "LOCK-BENCH: PASS" else "LOCK-BENCH: FAIL");
        x86.out32 (16#F4#, (if Passed then 16#10# else 16#11#));
        loop
            x86.halt;
        end loop;
    end Finish;

    procedure Exercise
      (CPU : CPU_Index; S : in out Spinlocks.Spinlock;
       Data : in out Protected_Data; Measure : Measurement)
    is
        Before, Acquired : Unsigned_64 := 0;
    begin
        Results(CPU).Started := x86.readOrderedTSC;
        for I in 1 .. Iterations loop
            if Measure = Acquisition_Latency then
                Before := x86.readOrderedTSC;
            end if;
            Spinlocks.enterCriticalSection (S);
            if Measure = Acquisition_Latency then
                Acquired := x86.readOrderedTSC;
            end if;
            if Data.Mirror /= not Data.Counter or else
               not Spinlocks.ownedBy (S, CPU) or else PerCPUData.numCLI /= 2
            then
                Results(CPU).Errors := Results(CPU).Errors + 1;
            end if;
            Data.Counter := Data.Counter + 1;
            Data.Mirror := not Data.Counter;
            Spinlocks.exitCriticalSection (S);
            if PerCPUData.numCLI /= 1 then
                Results(CPU).Errors := Results(CPU).Errors + 1;
            end if;
            if Measure = Acquisition_Latency then
                if Acquired < Before then
                    Results(CPU).Errors := Results(CPU).Errors + 1;
                else
                    Hist.Add (Results(CPU).Samples, Acquired - Before);
                end if;
            end if;
        end loop;
        Results(CPU).Finished := x86.readOrderedTSC;
    end Exercise;

    procedure Run (CPU : Natural; Participants : Natural) is
        Phase : Natural := 0;
        Passed : Boolean := True;
        First, Last : Unsigned_64;
    begin
        if Participants /= 4 or else CPU > 3 then
            Finish (False);
        end if;
        -- Isolate the primitive from timer/scheduler work. Production push/pop
        -- still execute, nested inside this exclusion. This is not an IRQ or
        -- application latency measurement. Barriers still service shootdowns.
        x86.cli;
        PerCPUData.pushCLI;
        if CPU = 0 then
            TextIO.println ("LOCK-BENCH: cpus=4 ticks_per_us=" &
                            Unsigned_64'Image (Time.tscPerDuration));
        end if;
        for Repeat in 1 .. Repetitions loop
            for Mode in Workload loop
                for Measure in Measurement loop
                    Phase := Phase + 1;
                    if CPU = 0 then
                        Shared_Data.Counter := 0;
                        Shared_Data.Mirror := not Unsigned_64'(0);
                    end if;
                    Private_Data(CPU).Counter := 0;
                    Private_Data(CPU).Mirror := not Unsigned_64'(0);
                    Results(CPU) := (others => <>);
                    Ready(CPU).Value := Phase;
                    if CPU = 0 then
                        for Other in CPU_Index loop
                            while Ready(Other).Value < Phase loop Poll (CPU); end loop;
                        end loop;
                        Go := Phase;
                    else
                        while Go < Phase loop Poll (CPU); end loop;
                    end if;
                    if Mode = Shared_Lock then
                        Exercise (CPU, Shared.Lock, Shared_Data, Measure);
                    else
                        Exercise (CPU, Private_Locks(CPU).Lock, Private_Data(CPU), Measure);
                    end if;
                    Done(CPU).Value := Phase;
                    -- Do not overwrite results or private data until CPU 0
                    -- acknowledges their consumption by releasing this phase.
                    if CPU = 0 then
                        for Other in CPU_Index loop
                            while Done(Other).Value < Phase loop Poll (CPU); end loop;
                        end loop;
                        First := Unsigned_64'Last;
                        Last := 0;
                        for Other in CPU_Index loop
                            First := Unsigned_64'Min (First, Results(Other).Started);
                            Last := Unsigned_64'Max (Last, Results(Other).Finished);
                            Passed := Passed and Results(Other).Errors = 0;
                            if Mode = Private_Lock then
                                Passed := Passed and Private_Data(Other).Counter = Iterations;
                            end if;
                            if Measure = Acquisition_Latency then
                                Passed := Passed and Hist.Count (Results(Other).Samples) = Iterations;
                                TextIO.println
                                  ("LOCK-SAMPLE: phase=" & Phase'Image & " cpu=" & Other'Image &
                                   " count=" & Hist.Count (Results(Other).Samples)'Image &
                                   " p50=" & Hist.Quantile_Upper (Results(Other).Samples, 50)'Image &
                                   " p99=" & Hist.Quantile_Upper (Results(Other).Samples, 99)'Image &
                                   " max=" & Hist.Maximum (Results(Other).Samples)'Image);
                            end if;
                        end loop;
                        if Mode = Shared_Lock then
                            Passed := Passed and Shared_Data.Counter = 4 * Iterations;
                        end if;
                        Passed := Passed and Last >= First;
                        TextIO.println
                          ("LOCK-ROUND: phase=" & Phase'Image & " repeat=" & Repeat'Image &
                           " mode=" & Name (Mode) & " measurement=" & Name (Measure) &
                           " operations=200000 elapsed_ticks=" & Unsigned_64'Image (Last - First) &
                           " valid=" & Boolean'Image (Passed));
                        Consumed := Phase;
                    else
                        while Consumed < Phase loop Poll (CPU); end loop;
                    end if;
                end loop;
            end loop;
        end loop;
        if CPU = 0 then
            Finish (Passed);
        end if;
        loop Poll (CPU); end loop;
    end Run;
end Spinlock_Benchmark;
