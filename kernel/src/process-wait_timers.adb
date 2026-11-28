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
with Scheduler;

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
    -- This CPU's alarm for deadline key, when that is within the tick
    -- (the next regular timer opportunity is at most a millisecond away,
    -- and its tick arms again). Interrupts disabled.
    procedure alarmFor (key : Unsigned_64) is
        now : Unsigned_64;
    begin
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
    end alarmFor;

    -- Caller holds heapLocks (cpu), on cpu: note its earliest deadline and
    -- arm the alarm for it.
    procedure armFor (cpu : CPU_Index) is
    begin
        earliest (cpu) := WD.Earliest_Key (heaps (cpu));
        alarmFor (earliest (cpu));
    end armFor;

    procedure Arm (me : ThreadID; tsc : Unsigned_64) is
        cpu : CPU_Index;
    begin
        -- Interrupts off before the CPU is read: the thread stays on it, so
        -- the heap is this CPU's and the alarm armed below is its own.
        PerCPUData.pushCLI;
        cpu := PerCPUData.getCPUNumber;
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
        PerCPUData.popCLI;
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

    procedure Sleep (tsc : Unsigned_64) is
        me : constant ThreadID := PerCPUData.getCurrentThread;
    begin
        threadtab (me).oneWaitKind := Sleep_Wait;
        threadtab (me).oneWaitStatus := Wait_Pending;
        Arm (me, tsc);
        -- Published and handed off under Process.lock: the timeout (which
        -- needs it) finds SLEEPING, or Wait_Timed_Out already set.
        Spinlocks.enterCriticalSection (lock);
        if threadtab (me).oneWaitStatus = Wait_Pending then
            threadtab (me).state := SLEEPING;
            Scheduler.enter;
        end if;
        Spinlocks.exitCriticalSection (lock);
        Disarm (me);
        threadtab (me).oneWaitKind := No_One_Wait;
    end Sleep;

    -- Caller holds Process.lock.
    procedure endSleep (t : ThreadID; how : One_Wait_Status; woken : out Boolean) is
    begin
        woken := threadtab (t).oneWaitKind = Sleep_Wait
          and then threadtab (t).oneWaitStatus = Wait_Pending;
        if woken then
            threadtab (t).oneWaitStatus := how;
            if threadtab (t).state = SLEEPING then
                ready (t);
            end if;
        end if;
    end endSleep;

    procedure Wake_Sleeper (t : ThreadID; woken : out Boolean) is
    begin
        Spinlocks.enterCriticalSection (lock);
        endSleep (t, Wait_Woken, woken);
        Spinlocks.exitCriticalSection (lock);
    end Wake_Sleeper;

    -- Caller holds heapLocks of t's heap; t was just taken from it.
    procedure timeOut (t : ThreadID) is
    begin
        case threadtab (t).oneWaitKind is
            when Notification_Wait => Notifications.Time_Out (t);
            when Private_Word_Wait => Private_Futex.Time_Out (t);
            when Shared_Word_Wait  => Futex.Time_Out (t);
            when Sleep_Wait        =>
                declare
                    ignore : Boolean;
                begin
                    Spinlocks.enterCriticalSection (lock);
                    endSleep (t, Wait_Timed_Out, ignore);
                    Spinlocks.exitCriticalSection (lock);
                end;
            when No_One_Wait       => null;
        end case;
    end timeOut;

    -- Caller holds heapLocks (c): time out c's due waits, one chunk.
    procedure expireOn (c : CPU_Index; now : Unsigned_64) is
        t : WD.Thread_Ref;
    begin
        for n in 1 .. Expire_Chunk loop
            WD.Pop_Due (heaps (c), now, t);
            exit when t = WD.No_Thread;
            timeOut (ThreadID (t));
        end loop;
        earliest (c) := WD.Earliest_Key (heaps (c));
    end expireOn;

    procedure Expire (cpu : Natural; now : Unsigned_64) is
        taken : Boolean;
        grace : constant Unsigned_64 := ticksOf (Steal_After_Us);
    begin
        if cpu not in CPU_Index then
            return;
        end if;
        if earliest (cpu) <= now then
            Spinlocks.enterCriticalSection (heapLocks (cpu));
            expireOn (cpu, now);
            -- More due than one chunk: the alarm comes back at once for them.
            armFor (cpu);
            Spinlocks.exitCriticalSection (heapLocks (cpu));
        else
            -- A deadline that was more than a tick away when it joined:
            -- each tick arms for it once it is within one (the timer's
            -- interrupt, interrupts disabled).
            alarmFor (earliest (cpu));
        end if;
        -- Another CPU whose timer has not run its deadlines (masked, in
        -- SMM, its timer lost): this one times them out, late by at most
        -- the grace, as the old global lists did for any CPU's tick.
        for c in CPU_Index loop
            if c /= cpu and then earliest (c) < Unsigned_64'Last - grace
              and then earliest (c) + grace <= now
            then
                Spinlocks.tryEnterCriticalSection (heapLocks (c), taken);
                if taken then
                    expireOn (c, now);
                    Spinlocks.exitCriticalSection (heapLocks (c));
                end if;
            end if;
        end loop;
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
            when Shared_Word_Wait | Sleep_Wait | No_One_Wait => null;
        end case;
        Notifications.Unbind_Thread_Of (t);
    end Cancel;

end Process.Wait_Timers;
