-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- One clock on every CPU (tsc_sync.ads).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with System.Machine_Code; use System.Machine_Code;
with Build;
with Clock_Page;
with CPUID;
with Msr_Safe;
with TextIO;
with Time;
with TSC_Sync_Rules;
with x86;

package body TSC_Sync with
    SPARK_Mode => Off
is
    package R renames TSC_Sync_Rules;
    use type R.Adjust_Value;
    use type R.Verdict;
    use type R.Step;

    IA32_TIME_STAMP_COUNTER : constant Unsigned_32 := 16#10#;
    IA32_TSC_ADJUST : constant Unsigned_32 := 16#3B#;

    -- How long each CPU of a pair reads the counter in one check (Linux
    -- checks a CPU of an already running package for 2 ms).
    Check_Ms : constant R.Wait_Milliseconds := 2;
    -- How long a CPU waits for the other at a meeting point, and for its
    -- turn (the other CPU's one counter read).
    Meet_Limit_Ms : constant R.Wait_Milliseconds := 100;
    Turn_Limit_Ms : constant R.Wait_Milliseconds := 10;
    -- A second bound on every spin, whatever the counter does.
    Maximum_Spins : constant := 100_000_000;
    -- CPUs at a meeting point: the boot CPU and the one being checked.
    Pair : constant Unsigned_32 := 2;

    -- Test only (Build.Test_TSC_Skew, headless --test tsc-skew): this CPU's
    -- counter moves this far ahead before its TSC_ADJUST step, about the
    -- N150's 1.4e9-tick disagreement (image 37).
    Skew_Probe_CPU : constant := 1;
    Skew_Probe_Ticks : constant Unsigned_64 := 2 ** 30;

    -- The two CPUs of a check.
    type Side is (Boot_Side, Secondary_Side);
    Other : constant array (Side) of Side :=
      [Boot_Side => Secondary_Side, Secondary_Side => Boot_Side];

    -- Shared by the boot CPU and the secondary CPU being checked: the spin
    -- word whose turn it is, the last counter value read, and who is done.
    Turn : Side := Secondary_Side with Atomic;
    Last : Unsigned_64 := 0 with Atomic;
    Finished : array (Side) of Boolean := [others => False] with Atomic_Components;
    Arrived : aliased Unsigned_32 := 0 with Volatile;
    Departed : aliased Unsigned_32 := 0 with Volatile;
    -- The check the secondary CPU posted (only it writes this).
    Request : Unsigned_32 := 0 with Atomic;
    -- What the boot CPU saw, written before it departs.
    Boot_Seen : R.Observation := R.No_Observation with Volatile;
    Boot_Completed : Boolean := False with Atomic;

    -- The boot CPU's: the last check it joined.
    Served : Unsigned_32 := 0;

    -- Set by the boot CPU before any secondary CPU starts.
    Has_Adjust : Boolean := False;
    Boot_Adjust : R.Adjust_Value := R.Boot_Value;

    -- Secondary CPUs start one at a time: the outcome so far.
    Shared : Boolean := True;
    Agreed : Natural := 0;
    Disagreed : Natural := 0;
    Largest : Unsigned_64 := 0;

    procedure Pause is
    begin
        Asm ("pause", Volatile => True);
    end Pause;

    -- One CPU's arrival at a meeting point (a locked increment).
    procedure Increment (Address : System.Address) is
        Word : Unsigned_32 with Import, Volatile, Address => Address;
    begin
        Asm ("lock incl %0",
             Outputs => Unsigned_32'Asm_Output ("+m", Word),
             Volatile => True, Clobber => "cc,memory");
    end Increment;

    -- Arrive at a meeting point and wait for the other CPU there.
    procedure Meet (Address : System.Address; Met : out Boolean) is
        Count : Unsigned_32 with Import, Volatile, Address => Address;
        Start : constant Unsigned_64 := x86.rdtsc;
        Limit : constant Unsigned_64 := R.Limit_Ticks (Time.tscFrequencyHz, Meet_Limit_Ms);
    begin
        Met := False;
        Increment (Address);
        for Spin in 1 .. Maximum_Spins loop
            if Count >= Pair then
                Met := True;
                return;
            end if;
            exit when R.Expired (Start, x86.rdtsc, Limit);
            Pause;
        end loop;
    end Meet;

    -- Take turns with the other CPU for Check_Ms: on this CPU's turn, read
    -- the last value the other CPU read, read the counter, publish it, and
    -- hand the turn over. Strict turns: every sample follows one of the
    -- other CPU's. Ends at this CPU's time limit, or when the other CPU is
    -- done; a turn that does not come within Turn_Limit_Ms is incomplete.
    procedure Sample (Me : Side; Seen : out R.Observation; Completed : out Boolean) is
        Start : constant Unsigned_64 := x86.readOrderedTSC;
        Limit : constant Unsigned_64 := R.Limit_Ticks (Time.tscFrequencyHz, Check_Ms);
        Turn_Limit : constant Unsigned_64 := R.Limit_Ticks (Time.tscFrequencyHz, Turn_Limit_Ms);
        Previous, Now, Waiting_Since : Unsigned_64;
        Mine : Boolean;
    begin
        Seen := R.No_Observation;
        Completed := True;
        Turns : while Seen.Rounds < R.Maximum_Rounds loop
            Mine := False;
            Waiting_Since := x86.rdtsc;
            for Spin in 1 .. Maximum_Spins loop
                if Turn = Me then
                    Mine := True;
                    exit;
                end if;
                exit Turns when Finished (Other (Me));
                if R.Expired (Waiting_Since, x86.rdtsc, Turn_Limit) then
                    Completed := False;
                    exit Turns;
                end if;
                Pause;
            end loop;
            if not Mine then
                Completed := False;
                exit Turns;
            end if;
            Previous := Last;
            Now := x86.readOrderedTSC;
            Last := Now;
            Turn := Other (Me);
            Seen := R.Observed (Seen, Previous, Now);
            exit Turns when R.Expired (Start, Now, Limit);
        end loop Turns;
        Finished (Me) := True;
    end Sample;

    -- With a leading space either way ("TSC_ADJUST -5", "TSC_ADJUST 0").
    function Image (Value : R.Adjust_Value) return String is
        ((if Value < 0 then " " else "") & Long_Long_Integer'Image (Long_Long_Integer (Value)));

    function Verdict_Text (V : R.Verdict) return String is
        (case V is
            when R.Agree => "no warp",
            when R.Disagree => "warp",
            when R.Incomplete => "incomplete");

    procedure Start_Boot_CPU is
        Raw : Unsigned_64;
        Ok : Boolean;
        Current : R.Adjust_Value;
        Plan : R.Adjust_Plan;
    begin
        Has_Adjust := CPUID.leaf7ebx.hasTSCAdjust;
        if not Has_Adjust then
            TextIO.println ("TSC: no TSC_ADJUST; each CPU's counter is checked against CPU 0's");
            return;
        end if;
        Msr_Safe.Read (IA32_TSC_ADJUST, Raw, Ok);
        if not Ok then
            Has_Adjust := False;
            TextIO.println ("TSC: CPU 0 refused its TSC_ADJUST read; each CPU's counter is checked against CPU 0's");
            return;
        end if;
        Current := R.To_Adjust (Raw);
        Plan := R.Boot_Plan (Current);
        Boot_Adjust := Current;
        if not Plan.Write then
            TextIO.println ("TSC: CPU 0 TSC_ADJUST" & Image (Current));
        elsif Msr_Safe.Write (IA32_TSC_ADJUST, R.To_Raw (Plan.Value)) then
            Boot_Adjust := Plan.Value;
            TextIO.println ("TSC: CPU 0 TSC_ADJUST" & Image (Current) & " ->" & Image (Plan.Value) &
              " (firmware left it nonzero)");
        else
            TextIO.println ("TSC: CPU 0 refused TSC_ADJUST" & Image (Plan.Value) &
              "; it stays" & Image (Current));
        end if;
    end Start_Boot_CPU;

    procedure Serve_Boot_CPU is
        Posted : constant Unsigned_32 := Request;
        Seen : R.Observation := R.No_Observation;
        Met_First, Ignored_Departure : Boolean;
        Sampled : Boolean := False;
    begin
        if Posted = Served then
            return;
        end if;
        Served := Posted;
        Meet (Arrived'Address, Met_First);
        if Met_First then
            Sample (Boot_Side, Seen, Sampled);
        end if;
        Boot_Seen := Seen;
        Boot_Completed := Met_First and then Sampled;
        -- The secondary CPU judges the check; the boot CPU only waits.
        Meet (Departed'Address, Ignored_Departure);
    end Serve_Boot_CPU;

    -- One check, from the secondary CPU: post it, meet the boot CPU, take
    -- turns, meet again, collect what the boot CPU saw.
    procedure Check (Seen, Boot : out R.Observation; Completed : out Boolean) is
        Met_First, Met_Last : Boolean;
        Sampled : Boolean := False;
    begin
        Seen := R.No_Observation;
        Last := 0;
        Turn := Secondary_Side;
        Finished (Boot_Side) := False;
        Finished (Secondary_Side) := False;
        Arrived := 0;
        Departed := 0;
        Boot_Completed := False;
        Boot_Seen := R.No_Observation;
        Request := Request + 1;
        Meet (Arrived'Address, Met_First);
        if Met_First then
            Sample (Secondary_Side, Seen, Sampled);
        end if;
        Meet (Departed'Address, Met_Last);
        Boot := Boot_Seen;
        Completed := Met_First and then Sampled and then Met_Last and then Boot_Completed;
    end Check;

    procedure Start_Secondary (CPU : Natural) is
        Name : constant String := "TSC: CPU" & Natural'Image (CPU);
        Raw : Unsigned_64;
        Ok : Boolean;
        Adjustable : Boolean := Has_Adjust;
        Current : R.Adjust_Value := R.Boot_Value;
        Plan : R.Adjust_Plan;
        Updated : R.Adjust_Value;
        Seen, Boot : R.Observation := R.No_Observation;
        Completed : Boolean;
        Result : R.Verdict := R.Incomplete;
        Next : R.Step := R.Done_Disagree;
        Warp : Unsigned_64 := 0;
    begin
        if Build.Test_TSC_Skew and then CPU = Skew_Probe_CPU then
            TextIO.println ("TSC-PROBE: CPU" & Natural'Image (CPU) & "'s counter moved" &
              Unsigned_64'Image (Skew_Probe_Ticks) & " ticks ahead: " &
              (if Msr_Safe.Write (IA32_TIME_STAMP_COUNTER, x86.readOrderedTSC + Skew_Probe_Ticks)
               then "written" else "refused"));
        end if;
        if Adjustable then
            Msr_Safe.Read (IA32_TSC_ADJUST, Raw, Ok);
            if not Ok then
                Adjustable := False;
                TextIO.println (Name & " refused its TSC_ADJUST read; checked without it");
            else
                Current := R.To_Adjust (Raw);
                Plan := R.Secondary_Plan (Current, Boot_Adjust);
                if Plan.Write then
                    if Msr_Safe.Write (IA32_TSC_ADJUST, R.To_Raw (Plan.Value)) then
                        TextIO.println (Name & " TSC_ADJUST" & Image (Current) & " ->" &
                          Image (Plan.Value) & " (CPU 0's)");
                        Current := Plan.Value;
                    else
                        Adjustable := False;
                        TextIO.println (Name & " refused TSC_ADJUST" & Image (Plan.Value) &
                          "; it stays" & Image (Current));
                    end if;
                end if;
            end if;
        end if;

        for Number in R.Check_Number loop
            Check (Seen, Boot, Completed);
            Result := R.Verdict_Of (Completed, Boot, Seen);
            Next := R.Next_Step (Number, Result, Adjustable, Boot, Seen);
            Warp := R.Largest_Warp (Boot, Seen);
            TextIO.println (Name & " check" & Positive'Image (Number) & ": " & Verdict_Text (Result) &
              "; behind CPU 0 by up to" & Unsigned_64'Image (Seen.Max_Warp) &
              " ticks, ahead by up to" & Unsigned_64'Image (Boot.Max_Warp) & " ticks;" &
              Natural'Image (Seen.Rounds) & " and" & Natural'Image (Boot.Rounds) & " samples");
            case Next is
                when R.Compensate =>
                    Updated := R.Compensated (Current, Boot, Seen);
                    if Msr_Safe.Write (IA32_TSC_ADJUST, R.To_Raw (Updated)) then
                        TextIO.println (Name & " TSC_ADJUST" & Image (Current) & " ->" &
                          Image (Updated) & " to compensate a" & Unsigned_64'Image (Warp) & "-tick warp");
                        Current := Updated;
                    else
                        TextIO.println (Name & " refused TSC_ADJUST" & Image (Updated));
                        Next := R.Done_Disagree;
                        exit;
                    end if;
                when R.Check_Again =>
                    null;
                when R.Done_Agree | R.Done_Disagree =>
                    exit;
            end case;
        end loop;
        if Next not in R.Done_Agree | R.Done_Disagree then
            Next := R.Done_Disagree;
        end if;

        Shared := R.Still_Shared (Shared, Next);
        if Next = R.Done_Agree then
            Agreed := Agreed + 1;
            TextIO.println (Name & " agrees with CPU 0: max warp" & Unsigned_64'Image (Warp) & " ticks");
        else
            Disagreed := Disagreed + 1;
            Largest := Unsigned_64'Max (Largest, Warp);
            -- Before this CPU's timer starts: it never keeps time on the
            -- shared clock it disagrees with.
            Clock_Page.Withdraw_Sharing;
            if Result = R.Incomplete then
                TextIO.println (Name & "'s warp check did not complete; the clock is CPU 0's alone");
            else
                TextIO.println ("TSC: CPUs disagree by" & Unsigned_64'Image (Warp) &
                  " ticks after TSC_ADJUST; the clock is CPU 0's alone");
            end if;
        end if;
    end Start_Secondary;

    procedure Report (Secondary_CPUs : Natural) is
    begin
        TextIO.println ("TSC:" & Natural'Image (Agreed) & " of" & Natural'Image (Secondary_CPUs) &
          " secondary CPUs agree with CPU 0" &
          (if Disagreed > 0 then "," & Natural'Image (Disagreed) & " disagree by up to" &
             Unsigned_64'Image (Largest) & " ticks" else "") &
          "; the clock is " &
          (if Clock_Page.Shared then "shared"
           elsif Shared and then Disagreed = 0 then "CPU 0's alone (no invariant TSC)"
           else "CPU 0's alone"));
    end Report;
end TSC_Sync;
