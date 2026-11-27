pragma Ada_2022;
with Build;
with Config;
with PerCPUData;
with Scheduler_Alarm;
with Wait_Deadlines;
with x86;
with Process.Notifications;
with Process.Private_Futex;
with Process.Futex;

package body Process.Wait_Timers is

    package WD renames Wait_Deadlines;
    use type WD.Thread_Ref;

    pragma Compile_Time_Error
      (Natural (WD.Thread_Ref'Last) /= Natural (ThreadID'Last),
       "Wait_Deadlines.Thread_Ref must cover ThreadID");

    subtype CPU_Index is Natural range 0 .. Config.MAX_SMP_CPUS - 1;

    heaps     : array (CPU_Index) of WD.Heap;
    heapLocks : array (CPU_Index) of Spinlocks.Spinlock;
    -- The earliest key of each heap, for the timer's lock-free look.
    earliest  : array (CPU_Index) of Unsigned_64 := [others => Unsigned_64'Last]
      with Atomic_Components;

    function ticksOf (us : Unsigned_64) return Unsigned_64 is
      (if Time.tscPerDuration = 0 then 1
       elsif us > Unsigned_64'Last / Time.tscPerDuration then Unsigned_64'Last
       else Time.tscPerDuration * us);

    function later (tsc, delta_ticks : Unsigned_64) return Unsigned_64 is
      (if delta_ticks > Unsigned_64'Last - 1 - tsc then Unsigned_64'Last - 1
       else tsc + delta_ticks);

    procedure Deadline_Of (deadlineUs : Unsigned_64; tsc : out Unsigned_64; passed : out Boolean) is
        nowUs : Unsigned_64;
        ok : Boolean;
        stamp : constant Unsigned_64 := x86.rdtsc;
    begin
        Time.Read_User_Monotonic (nowUs, ok);
        passed := ok and then deadlineUs <= nowUs;
        tsc := (if passed then stamp
                elsif not ok then later (stamp, ticksOf (1_000))
                else later (stamp, ticksOf (deadlineUs - nowUs)));
    end Deadline_Of;

    procedure Deadline_Of_Ms (deadlineMs : Unsigned_64; tsc : out Unsigned_64; passed : out Boolean) is
        nowMs : constant Unsigned_64 := Time.msTicks;
        stamp : constant Unsigned_64 := x86.rdtsc;
    begin
        passed := deadlineMs <= nowMs;
        tsc := (if passed then stamp
                elsif deadlineMs - nowMs > Unsigned_64'Last / 1_000 then Unsigned_64'Last - 1
                else later (stamp, ticksOf ((deadlineMs - nowMs) * 1_000)));
    end Deadline_Of_Ms;

    -- Caller holds heapLocks (cpu), on cpu: the alarm for its earliest
    -- deadline, when that is within the tick.
    procedure armFor (cpu : CPU_Index) is
        key : constant Unsigned_64 := WD.Earliest_Key (heaps (cpu));
        now : Unsigned_64;
    begin
        earliest (cpu) := key;
        if not Build.OneShot_Scheduling or else key = Unsigned_64'Last
          or else Time.tscPerDuration = 0
        then
            return;
        end if;
        now := x86.rdtsc;
        if key <= now then
            Scheduler_Alarm.Request_Earlier (1);
        elsif (key - now) / Time.tscPerDuration < 1_000 then
            Scheduler_Alarm.Request_Earlier
              (Scheduler_Alarm.Delay_Microseconds
                 (Unsigned_64'Max (1, (key - now + Time.tscPerDuration - 1) / Time.tscPerDuration)));
        end if;
    end armFor;

    procedure Arm (me : ThreadID; tsc : Unsigned_64) is
        cpu : constant CPU_Index := PerCPUData.getCPUNumber;
    begin
        Spinlocks.enterCriticalSection (heapLocks (cpu));
        threadtab (me).oneWaitCPU := cpu;
        threadtab (me).oneWaitTimed := True;
        threadtab (me).oneWaitDeadlineMs :=
          (if Time.tscPerDuration = 0 or else tsc <= x86.rdtsc then Time.msTicks
           else Time.msTicks + (tsc - x86.rdtsc) / Time.tscPerDuration / 1_000);
        WD.Insert (heaps (cpu), WD.Thread_Ref (me), tsc);
        if WD.Earliest (heaps (cpu)) = WD.Thread_Ref (me) then
            armFor (cpu);
        end if;
        Spinlocks.exitCriticalSection (heapLocks (cpu));
    end Arm;

    procedure Disarm (me : ThreadID) is
        cpu : constant CPU_Index := threadtab (me).oneWaitCPU;
    begin
        if not threadtab (me).oneWaitTimed then
            return;
        end if;
        Spinlocks.enterCriticalSection (heapLocks (cpu));
        if WD.Contains (heaps (cpu), WD.Thread_Ref (me)) then
            WD.Remove (heaps (cpu), WD.Thread_Ref (me));
            earliest (cpu) := WD.Earliest_Key (heaps (cpu));
        end if;
        threadtab (me).oneWaitTimed := False;
        Spinlocks.exitCriticalSection (heapLocks (cpu));
    end Disarm;

    procedure Finish (t : ThreadID; how : One_Wait_Status; bits : Unsigned_64 := 0;
                      sync : Boolean := False) is
    begin
        threadtab (t).oneWaitBits := bits;
        threadtab (t).oneWaitStatus := how;
        Spinlocks.enterCriticalSection (lock);
        if threadtab (t).state = FUTEXWAITING then
            if sync then
                readySync (t);
            else
                ready (t);
            end if;
        end if;
        Spinlocks.exitCriticalSection (lock);
    end Finish;

    -- Caller holds heapLocks of t's heap; t was just taken from it.
    procedure timeOut (t : ThreadID) is
    begin
        case threadtab (t).oneWaitKind is
            when Notification_Wait => Notifications.Time_Out (t);
            when Private_Word_Wait => Private_Futex.Time_Out (t);
            when Shared_Word_Wait  => Futex.Time_Out (t);
            when No_One_Wait       => null;
        end case;
    end timeOut;

    procedure Expire (cpu : Natural; now : Unsigned_64) is
        t : WD.Thread_Ref;
    begin
        if cpu not in CPU_Index or else earliest (cpu) > now then
            return;
        end if;
        Spinlocks.enterCriticalSection (heapLocks (cpu));
        for n in 1 .. Expire_Chunk loop
            WD.Pop_Due (heaps (cpu), now, t);
            exit when t = WD.No_Thread;
            timeOut (ThreadID (t));
        end loop;
        -- More due than one chunk: the alarm comes back at once for them.
        armFor (cpu);
        Spinlocks.exitCriticalSection (heapLocks (cpu));
    end Expire;

    procedure Cancel (t : ThreadID) is
        cpu : constant CPU_Index := threadtab (t).oneWaitCPU;
    begin
        if threadtab (t).oneWaitTimed then
            Spinlocks.enterCriticalSection (heapLocks (cpu));
            if WD.Contains (heaps (cpu), WD.Thread_Ref (t)) then
                WD.Remove (heaps (cpu), WD.Thread_Ref (t));
                earliest (cpu) := WD.Earliest_Key (heaps (cpu));
            end if;
            threadtab (t).oneWaitTimed := False;
            Spinlocks.exitCriticalSection (heapLocks (cpu));
        end if;
        case threadtab (t).oneWaitKind is
            when Notification_Wait => Notifications.Cancel (t);
            when Private_Word_Wait => Private_Futex.Cancel (t);
            -- The reaper's Futex.cancelWait has withdrawn a shared wait.
            when Shared_Word_Wait | No_One_Wait => null;
        end case;
        Notifications.Unbind_Thread_Of (t);
    end Cancel;

end Process.Wait_Timers;
