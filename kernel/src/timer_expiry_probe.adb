pragma Ada_2022;
with Interfaces; use Interfaces;
with Config;
with Build;
with Deadline_Ownership;
with Interrupts;
with Lapic;
with PerCPUData;
with Scheduler_Timing;
with TextIO;
with Time;
with x86;

-- Hardware/interrupt adapter. Only Deadline_Ownership is SPARK-verified.
package body Timer_Expiry_Probe is
   subtype CPU_Number is Natural range 0 .. Config.MAX_SMP_CPUS - 1;
   type Generation is (No_Generation, Original, Replacement);
   type Identity is record
      CPU : CPU_Number;
      Life : Generation;
   end record;
   -- Synthetic identities for a boot-only test; no live process is admitted.
   package Owner is new Deadline_Ownership (Identity, (0, No_Generation));
   use type Owner.Outcome;
   type Scenario is (Normal_Expiry, Replaced_Timer, Cancelled_Timer, Replaced_Owner);
   Samples_Per_Scenario : constant := 128;
   Limits_Us : constant array (Natural range 0 .. 10) of Unsigned_64 :=
     [1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1_024];
   type Histogram is array (Limits_Us'Range) of Natural;
   type CPU_State is record
      Slot : Owner.State;
      Current : Identity := (0, No_Generation);
      Kind : Scenario := Normal_Expiry;
      Expired, Cancelled, Stale, Early_Vectors, Failures : Natural := 0;
      Max_Late : Unsigned_64 := 0;
      Buckets : Histogram := [others => 0];
   end record with Alignment => 64;
   States : array (CPU_Number) of CPU_State;
   Enabled, Completed : array (CPU_Number) of Boolean := [others => False]
     with Volatile_Components;
   Probe_Failure : exception;

   subtype Probe_Delay_Us is Positive range 1 .. 200;
   function Countdown (Microseconds : Probe_Delay_Us) return Unsigned_32 is
      Count : constant Unsigned_64 :=
        (Unsigned_64 (Interrupts.getLAPICTimerInterval) * Unsigned_64 (Microseconds) + 999) / 1_000;
   begin
      return Unsigned_32 (Unsigned_64'Max (1, Count));
   end Countdown;

   procedure Record_Lateness (S : in out CPU_State; Late : Unsigned_64) is
   begin
      S.Max_Late := Unsigned_64'Max (S.Max_Late, Late);
      for I in Limits_Us'Range loop
         if Late <= Limits_Us (I) * Time.tscPerDuration then
            S.Buckets (I) := S.Buckets (I) + 1;
            exit;
         end if;
      end loop;
   end Record_Lateness;

   function Handle_Interrupt return Boolean is
      CPU : constant CPU_Number := PerCPUData.getCPUNumber;
      S : CPU_State renames States (CPU);
      package HW is new Lapic (Interrupts.lapicAddr);
      Now : Unsigned_64;
      R : Owner.Outcome;
      Late : Unsigned_64;
   begin
      if not Enabled (CPU) then return False; end if;
      if Completed (CPU) then return True; end if;
      Now := x86.readOrderedTSC;
      Owner.Poll (S.Slot, Now, S.Current, R, Late);
      case R is
         when Owner.Nothing_Due =>
            if Owner.View (S.Slot).Active then
               S.Early_Vectors := S.Early_Vectors + 1;
               -- TSC/APIC calibration and a replaced timer can give an early
               -- IRQ. Re-arm the remaining interval; never deliver early.
               HW.armTimer (Countdown (Probe_Delay_Us
                 ((Owner.View (S.Slot).Deadline - Now + Time.tscPerDuration - 1) /
                  Time.tscPerDuration)));
               return True;
            elsif S.Kind = Cancelled_Timer then
               S.Cancelled := S.Cancelled + 1;
            else
               S.Failures := S.Failures + 1;
            end if;
         when Owner.Expired =>
            if S.Kind in Normal_Expiry | Replaced_Timer then
               S.Expired := S.Expired + 1;
               Record_Lateness (S, Late);
            else
               S.Failures := S.Failures + 1;
            end if;
         when Owner.Stale_Owner =>
            if S.Kind = Replaced_Owner then
               S.Stale := S.Stale + 1;
            else
               S.Failures := S.Failures + 1;
            end if;
      end case;
      Completed (CPU) := True;
      return True;
   end Handle_Interrupt;

   procedure Run is
      CPU : constant CPU_Number := PerCPUData.getCPUNumber;
      S : CPU_State renames States (CPU);
      package HW is new Lapic (Interrupts.lapicAddr);
      First, Second : Owner.Ticket;
      OK : Boolean;
      Start, Timeout : Unsigned_64;
      Cumulative : Natural := 0;
      P99_Us : Unsigned_64;
   begin
      -- Diagnostics require a plausible completed boot calibration. Runtime
      -- assertions stay disabled in the kernel; test failures are explicit.
      if Time.tscPerDuration not in 1 .. 1_000_000 or else
         Interrupts.getLAPICTimerInterval = 0 or else
         PerCPUData.getCurrentPID /= 0
      then
         raise Probe_Failure with "Deadline probe requires calibrated, pre-scheduler CPU";
      end if;
      HW.selectOneShotTimer;
      Enabled (CPU) := True;
      for Kind in Scenario loop
         for Sample in 1 .. Samples_Per_Scenario loop
            S.Kind := Kind;
            S.Current := (CPU, Original);
            Completed (CPU) := False;
            Start := x86.readOrderedTSC;
            Timeout := Start + 1_000_000 * Time.tscPerDuration;
            Owner.Arm (S.Slot, S.Current, Start + 200 * Time.tscPerDuration, First, OK);
            if not OK then raise Probe_Failure with "Deadline arm rejected"; end if;
            case Kind is
               when Normal_Expiry => HW.armTimer (Countdown (200));
               when Replaced_Timer =>
                  -- Leave the old 50-us hardware countdown running after
                  -- replacing its software owner/deadline with a 200-us one.
                  HW.armTimer (Countdown (50));
                  S.Current := (CPU, Replacement);
                  Owner.Arm (S.Slot, S.Current, Start + 200 * Time.tscPerDuration, Second, OK);
                  if not OK then raise Probe_Failure with "Replacement rejected"; end if;
                  Owner.Cancel (S.Slot, (CPU, Original), First, OK);
                  if OK then raise Probe_Failure with "Stale cancel accepted"; end if;
               when Cancelled_Timer =>
                  HW.armTimer (Countdown (200));
                  Owner.Cancel (S.Slot, S.Current, First, OK);
                  if not OK then raise Probe_Failure with "Cancel rejected"; end if;
               when Replaced_Owner =>
                  HW.armTimer (Countdown (200));
                  S.Current := (CPU, Replacement);
            end case;
            x86.sti;
            while not Completed (CPU) and then x86.readOrderedTSC < Timeout loop
               null;
            end loop;
            x86.cli;
            if not Completed (CPU) then
               raise Probe_Failure with "Deadline timer interrupt timed out";
            end if;
         end loop;
      end loop;
      Enabled (CPU) := False;
      if Build.OneShot_Scheduling then
         HW.selectOneShotTimer;
         HW.armTimer (Unsigned_32'Max (1, Interrupts.getLAPICTimerInterval /
           Scheduler_Timing.OneShot_Ticks_Per_Millisecond));
      else
         HW.restorePeriodicTimer (Unsigned_32'Max
           (1, Interrupts.getLAPICTimerInterval / Scheduler_Timing.Ticks_Per_Millisecond));
      end if;
      P99_Us := (S.Max_Late + Time.tscPerDuration - 1) / Time.tscPerDuration;
      for I in Limits_Us'Range loop
         Cumulative := Cumulative + S.Buckets (I);
         if Cumulative * 100 >= S.Expired * 99 then
            P99_Us := Limits_Us (I);
            exit;
         end if;
      end loop;
      TextIO.println ("DEADLINE-TIMER: cpu=" & CPU'Image &
        " requested_us=200 expired=" & S.Expired'Image &
        " cancelled=" & S.Cancelled'Image & " stale=" & S.Stale'Image &
        " early_vectors=" & S.Early_Vectors'Image & " failures=" & S.Failures'Image &
        " p99_late_le_us=" & P99_Us'Image & " max_late_ticks=" & S.Max_Late'Image &
        " ticks_per_us=" & Time.tscPerDuration'Image);
      if S.Expired /= 2 * Samples_Per_Scenario or else
         S.Cancelled /= Samples_Per_Scenario or else S.Stale /= Samples_Per_Scenario or else
         S.Early_Vectors = 0 or else S.Failures /= 0
      then
         raise Probe_Failure with "Deadline timer ownership test failed";
      end if;
   end Run;
end Timer_Expiry_Probe;
