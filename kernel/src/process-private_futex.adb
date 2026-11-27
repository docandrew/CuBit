pragma Ada_2022;
with Notification_ABI;
with PerCPUData;
with Readiness_Publication;
with User_Page_Walk;
with Process.IPC;
with Process.User_Memory;
with Process.Wait_Timers;

package body Process.Private_Futex is

    package PF renames Private_Futexes;
    package NA renames Notification_ABI;

    pragma Compile_Time_Error
      (Natural (PF.Slot'Last) /= Natural (Readiness_Publication.Thread_Slot'Last),
       "Private_Futexes.Slot must be a thread slot");
    pragma Compile_Time_Error
      (Natural (PF.Thread_Ref'Last) /= Natural (ThreadID'Last),
       "Private_Futexes.Thread_Ref must cover ThreadID");

    function slotOf (t : ThreadID) return PF.Slot is (PF.Slot (threadtab (t).countSlot));

    function wordOk (address : Unsigned_64) return Boolean is
      (address /= 0 and then address mod 4 = 0 and then address < User_Page_Walk.User_Limit);

    -- Caller holds pid's privateFutexLock; t was just taken from its table.
    procedure finish (t : ThreadID; how : One_Wait_Status; sync : Boolean := False) is
    begin
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
    end finish;

    function wait (address, expected : Unsigned_64; timed, passed : Boolean;
                   tsc : Unsigned_64; handOff : Boolean := False) return Unsigned_64
    is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        pid : constant ProcessID := processOf (me);
        s   : PF.Slot;
        value : Unsigned_32;
        ok : Boolean;
        result : Unsigned_64;
        slept : Boolean := False;
    begin
        if pid = NO_PROCESS or else not wordOk (address) or else expected > NA.Word_Value_Last then
            return NA.Bad_Argument;
        end if;
        s := slotOf (me);
        threadtab (me).oneWaitKind := Private_Word_Wait;
        threadtab (me).oneWaitObject := Unsigned_32 (s);
        threadtab (me).oneWaitStatus := Wait_Pending;
        if timed and then not passed then
            Wait_Timers.Arm (me, tsc);
        end if;

        Spinlocks.enterCriticalSection (proctab (pid).privateFutexLock);
        User_Memory.Load_Word32 (pid, address, value, ok);
        if not ok then
            result := NA.Bad_Argument;
        elsif Unsigned_64 (value) /= expected then
            result := NA.Retry;
        elsif passed or else threadtab (me).oneWaitStatus = Wait_Timed_Out then
            result := NA.Timed_Out;
        elsif proctab (pid).privateFutexes.Next_Ticket = Unsigned_64'Last
          or else proctab (pid).privateFutexes.Used (s)
        then
            -- 2**64 waits, or a slot not this thread's (never: a live
            -- thread owns its slot and waits on one word at a time).
            result := NA.Retry;
        else
            PF.Enqueue (proctab (pid).privateFutexes, s, address, PF.Thread_Ref (me));
            -- Published before the lock is released: a waker, which needs
            -- the lock, always finds this state.
            threadtab (me).state := FUTEXWAITING;
            slept := True;
            result := NA.Woken;
        end if;
        Spinlocks.exitCriticalSection (proctab (pid).privateFutexLock);

        if slept then
            if handOff then
                yieldToSyncWakee;
            else
                yield;
            end if;
            result := (case threadtab (me).oneWaitStatus is
                         when Wait_Timed_Out => NA.Timed_Out,
                         when others         => NA.Woken);
        end if;
        if timed and then not passed then
            Wait_Timers.Disarm (me);
        end if;
        threadtab (me).oneWaitKind := No_One_Wait;
        return result;
    end wait;

    function wake (pid : ProcessID; address, count : Unsigned_64; sync : Boolean := False)
      return Unsigned_64
    is
        taken : PF.Batch;
        n : PF.Batch_Count;
        woken : Unsigned_64 := 0;
        everyone : constant Boolean := count >= Unsigned_64 (PF.Slot'Last) + 1;
        want : PF.Batch_Count;
    begin
        if pid = NO_PROCESS or else not wordOk (address) then
            return NA.Bad_Argument;
        end if;
        while woken < count loop
            want := (if count - woken >= PF.Chunk then PF.Chunk else PF.Batch_Count (count - woken));
            Spinlocks.enterCriticalSection (proctab (pid).privateFutexLock);
            PF.Take_Batch (proctab (pid).privateFutexes, address, want, everyone, taken, n);
            for i in 1 .. n loop
                finish (ThreadID (taken (i)), Wait_Woken,
                        sync => sync and then woken = 0 and then i = 1);
            end loop;
            -- The batch's threads are readied under the lock, so the reaper
            -- (Cancel takes it) never frees one still being finished here.
            Spinlocks.exitCriticalSection (proctab (pid).privateFutexLock);
            woken := woken + Unsigned_64 (n);
            exit when n < want;
        end loop;
        -- The word may be the process's wake word (IPC-010 option B).
        if woken < count and then IPC.noteWakeWord (pid, address, sync => sync and then woken = 0) then
            woken := woken + 1;
        end if;
        return woken;
    end wake;

    procedure Time_Out (t : ThreadID) is
        pid : constant ProcessID := processOf (t);
        removed : Boolean;
    begin
        if pid = NO_PROCESS then
            return;
        end if;
        Spinlocks.enterCriticalSection (proctab (pid).privateFutexLock);
        if threadtab (t).oneWaitStatus = Wait_Pending then
            PF.Remove (proctab (pid).privateFutexes, PF.Slot (threadtab (t).oneWaitObject),
                       PF.Thread_Ref (t), removed);
            -- Not yet enqueued (removed False): the thread sees this under
            -- the lock and does not block.
            finish (t, Wait_Timed_Out);
        end if;
        Spinlocks.exitCriticalSection (proctab (pid).privateFutexLock);
    end Time_Out;

    procedure Cancel (t : ThreadID) is
        pid : constant ProcessID := processOf (t);
        removed : Boolean;
    begin
        if pid = NO_PROCESS then
            return;
        end if;
        Spinlocks.enterCriticalSection (proctab (pid).privateFutexLock);
        PF.Remove (proctab (pid).privateFutexes, PF.Slot (threadtab (t).oneWaitObject),
                   PF.Thread_Ref (t), removed);
        threadtab (t).oneWaitStatus := Wait_Ended;
        threadtab (t).oneWaitKind := No_One_Wait;
        Spinlocks.exitCriticalSection (proctab (pid).privateFutexLock);
    end Cancel;

end Process.Private_Futex;
