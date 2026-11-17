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
    -- HWP ratios and IA32_PERF_STATUS ratios count 100 MHz bus units.
    MHz_Per_Ratio      : constant := 100;

    function Field (Value : Unsigned_64; Shift : Natural) return Unsigned_64 is
       (Shift_Right (Value, Shift) and 16#FF#);

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
        Has_Feedback : constant Boolean := CPUID.leaf6ecx.hasHWPerfFeedback;
        Start_TSC, End_TSC : Unsigned_64;
        Start_A, Start_M, End_A, End_M : Unsigned_64 := 0;
        TSC_Hz  : constant Unsigned_64 := Unsigned_64 (Time.tscFrequencyHz);
        Elapsed : Unsigned_64;
    begin
        if Has_Feedback then
            Start_M := x86.rdmsr (x86.MSRs.MPERF);
            Start_A := x86.rdmsr (x86.MSRs.APERF);
        end if;
        Start_TSC := x86.rdtsc;
        Spin (Iterations);
        End_TSC := x86.rdtsc;
        if Has_Feedback then
            End_A := x86.rdmsr (x86.MSRs.APERF);
            End_M := x86.rdmsr (x86.MSRs.MPERF);
        end if;
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
        print (" aperf/mperf-pct=");
        if Has_Feedback and then End_M > Start_M then
            printd ((End_A - Start_A) * 100 / (End_M - Start_M));
        else
            print ("none");
        end if;
        print (" turbo=");
        print ((if CPUID.leaf6eax.hasTurboBoost then "yes" else "no"));
        if CPUID.leaf1ecx.hasEST then
            print (" perf-status-mhz=");
            printd (Field (x86.rdmsr (x86.MSRs.PERF_STATUS), 8) * MHz_Per_Ratio);
        end if;
        if CPUID.leaf6eax.hasHWP then
            declare
                Enabled : constant Unsigned_64 := x86.rdmsr (x86.MSRs.PM_ENABLE) and 1;
                Caps    : constant Unsigned_64 := x86.rdmsr (x86.MSRs.HWP_CAPABILITIES);
                Request : constant Unsigned_64 := x86.rdmsr (x86.MSRs.HWP_REQUEST);
            begin
                print (" hwp=");
                print ((if Enabled = 1 then "enabled" else "disabled"));
                print (" highest-mhz=");    printd (Field (Caps, 0) * MHz_Per_Ratio);
                print (" guaranteed-mhz="); printd (Field (Caps, 8) * MHz_Per_Ratio);
                print (" efficient-mhz=");  printd (Field (Caps, 16) * MHz_Per_Ratio);
                print (" lowest-mhz=");     printd (Field (Caps, 24) * MHz_Per_Ratio);
                print (" request-min=");    printd (Field (Request, 0));
                print (" max=");            printd (Field (Request, 8));
                print (" desired=");        printd (Field (Request, 16));
                print (" epp=");            printd (Field (Request, 24));
            end;
        else
            print (" hwp=absent");
        end if;
        println;
    end Report;
end CPU_Performance_Report;
