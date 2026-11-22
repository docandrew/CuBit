with System.Machine_Code; use System.Machine_Code;
with Interfaces; use Interfaces;
with CPUID;
with TextIO; use TextIO;
with Time;
with x86;

package body CPU_Performance_Report with SPARK_Mode => Off is
    -- Four dependent adds per iteration: four cycles of latency each turn
    -- on every x86 core since the P6 (the loop counter runs alongside).
    Adds_Per_Iteration : constant := 4;
    -- About 20 ms at 1 GHz: long against TSC jitter, short for boot.
    Iterations         : constant := 5_000_000;
    Hz_Per_MHz         : constant := 1_000_000;
    procedure Spin (Count : Unsigned_64) is
        N   : Unsigned_64 := Count;
        Acc : Unsigned_64 := 0;
    begin
        Asm ("1:" & ASCII.LF &
             "add $1, %1" & ASCII.LF &
             "add $1, %1" & ASCII.LF &
             "add $1, %1" & ASCII.LF &
             "add $1, %1" & ASCII.LF &
             "dec %0" & ASCII.LF &
             "jnz 1b",
             Outputs => (Unsigned_64'Asm_Output ("+r", N),
                         Unsigned_64'Asm_Output ("+r", Acc)),
             Clobber => "cc",
             Volatile => True);
    end Spin;

    procedure Report is
        Start_TSC, End_TSC : Unsigned_64;
        TSC_Hz  : constant Unsigned_64 := Unsigned_64 (Time.tscFrequencyHz);
        Elapsed : Unsigned_64;
    begin
        -- No MSR is read here: the N150 (image 27) raised #GP on one of
        -- them although CPUID listed the feature (some HWP MSRs fault until
        -- HWP is enabled). MSR reads return behind a #GP-recovering reader.
        Start_TSC := x86.rdtsc;
        Spin (Iterations);
        End_TSC := x86.rdtsc;
        Elapsed := End_TSC - Start_TSC;

        print ("cpu-perf: tsc-mhz=");
        printd (TSC_Hz / Hz_Per_MHz);
        print (" effective-mhz=");
        if Elapsed = 0 or else TSC_Hz = 0 then
            print ("unknown");
        else
            -- cycles * TSC_Hz / ticks, scaled to MHz without overflow:
            -- 2e7 cycles * (TSC_Hz / 1e6) stays far below 2 ** 64.
            printd (Unsigned_64 (Adds_Per_Iteration * Iterations) *
                    (TSC_Hz / Hz_Per_MHz) / Elapsed);
        end if;
        print (" turbo=");
        print ((if CPUID.leaf6eax.hasTurboBoost then "yes" else "no"));
        print (" hwp-supported=");
        print ((if CPUID.leaf6eax.hasHWP then "yes" else "no"));
        print (" hwp-epp=");
        print ((if CPUID.leaf6eax.hasHWPEnergyPrefs then "yes" else "no"));
        print (" aperf-mperf=");
        print ((if CPUID.leaf6ecx.hasHWPerfFeedback then "yes" else "no"));
        println;
    end Report;
end CPU_Performance_Report;
