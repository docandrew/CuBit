with PerCPUData;
with Process.User_Memory;
with User_Page_Walk;
with Futex_Queues;
with System.Machine_Code;

package body Process.Futex is
    use Futex_Keys;

    -- A futex waiter is a thread.
    pragma Compile_Time_Error
      (Futex_Keys.Max_Waiter /= Natural (ThreadID'Last),
       "Futex_Keys.Waiter_Id must cover ThreadID");

    -- Hashed buckets of 32 slots, and one overflow set with a slot for every
    -- possible thread: a thread waits on one futex at a time, so the
    -- overflow set can always take a waiter and FUTEX_WAIT never fails for
    -- lack of space (one process cannot crowd others out of a bucket).
    package Small is new Futex_Queues (Capacity => 32);
    package Large is new Futex_Queues (Capacity => Futex_Keys.Max_Waiter);

    buckets     : array (Bucket_Index) of Small.Bucket :=
      (others => Small.Empty_Bucket);
    bucketLocks : array (Bucket_Index) of Spinlocks.Spinlock;
    overflow    : Large.Bucket := Large.Empty_Bucket;
    overflowLock : Spinlocks.Spinlock;
    -- Waiters in the overflow set: wakes skip it while zero. Written under
    -- overflowLock, read under a bucket lock as a hint that is exact for
    -- that bucket's keys (their overflow entries are added under it).
    overflowCount : Natural := 0 with Atomic;

    -- Waits with a deadline, per bucket (overflow waiters count in their
    -- key's bucket): lets the tick skip idle buckets.
    deadlines   : array (Bucket_Index) of Natural := (others => 0)
      with Atomic_Components;

    -- Tickets for FIFO order, drawn under the lock of the structure the
    -- waiter joins; a key's waiters are always enqueued under its bucket
    -- lock, so their tickets follow their arrival order.
    ticketCounter : Unsigned_64 := 0 with Volatile;

    function nextTicket return Unsigned_64 is
        use System.Machine_Code;
        value : Unsigned_64 := 1;
    begin
        Asm ("lock xaddq %0, %1",
             Outputs => (Unsigned_64'Asm_Output ("+r", value),
                         Unsigned_64'Asm_Output ("+m", ticketCounter)),
             Clobber => "memory, cc", Volatile => True);
        return value;
    end nextTicket;

    function keyOf (pid : ProcessID; address : Unsigned_64) return Key is
      (Owner => Unsigned_32 (pid), Address => address);

    -- Caller holds bucketLocks (b); t was just removed from its structure.
    -- The bucket lock is held until the thread is readied, so cancelWait
    -- (which takes it) never frees a thread still being finished here.
    procedure finishWait (t : ThreadID; b : Bucket_Index; timedOut : Boolean) is
    begin
        if threadtab (t).futexDeadlineActive then
            threadtab (t).futexDeadlineActive := False;
            deadlines (b) := deadlines (b) - 1;
        end if;
        threadtab (t).futexTimedOut := timedOut;
        threadtab (t).futexWaiting := False;
        Spinlocks.enterCriticalSection (lock);
        if threadtab (t).state = FUTEXWAITING then
            ready (t);
        end if;
        Spinlocks.exitCriticalSection (lock);
    end finishWait;

    -- Caller holds bucketLocks (b). Remove t from wherever it waits.
    procedure removeWaiter (t : ThreadID; removed : out Boolean) is
        b : constant Bucket_Index := threadtab (t).futexBucket;
    begin
        if threadtab (t).futexInOverflow then
            Spinlocks.enterCriticalSection (overflowLock);
            Large.Remove_At (overflow, Large.Slot_Index (threadtab (t).futexSlot),
                             Waiter_Id (t), removed);
            if removed then
                overflowCount := overflowCount - 1;
            end if;
            Spinlocks.exitCriticalSection (overflowLock);
        else
            Small.Remove_At (buckets (b), Small.Slot_Index (threadtab (t).futexSlot),
                             Waiter_Id (t), removed);
        end if;
    end removeWaiter;

    function wait (address, expected, deadlineMs : Unsigned_64)
      return Unsigned_64
    is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        pid : constant ProcessID := processOf (me);
        k   : constant Key := keyOf (pid, address);
        b   : constant Bucket_Index := Bucket_Of (k);
        value : Unsigned_32;
        ok : Boolean;
        ticket : Unsigned_64;
        smallSlot : Small.Slot_Index;
        largeSlot : Large.Slot_Index;
    begin
        if pid = NO_PROCESS or else address mod 4 /= 0 or else
           address >= User_Page_Walk.User_Limit or else
           expected > Unsigned_64 (Unsigned_32'Last)
        then
            return FUTEX_FAULT;
        end if;

        Spinlocks.enterCriticalSection (bucketLocks (b));
        User_Memory.Load_Word32 (pid, address, value, ok);
        if not ok then
            Spinlocks.exitCriticalSection (bucketLocks (b));
            return FUTEX_FAULT;
        elsif Unsigned_64 (value) /= expected then
            Spinlocks.exitCriticalSection (bucketLocks (b));
            return FUTEX_RETRY;
        elsif deadlineMs /= FOREVER and then Time.msTicks >= deadlineMs then
            Spinlocks.exitCriticalSection (bucketLocks (b));
            return FUTEX_TIMED_OUT;
        end if;

        ticket := nextTicket;
        Small.Enqueue (buckets (b), Waiter_Id (me), k, ticket, smallSlot, ok);
        if ok then
            threadtab (me).futexInOverflow := False;
            threadtab (me).futexSlot := Natural (smallSlot);
        else
            -- Bucket full: the overflow set always has room.
            Spinlocks.enterCriticalSection (overflowLock);
            ticket := nextTicket;
            Large.Enqueue (overflow, Waiter_Id (me), k, ticket, largeSlot, ok);
            if not ok then
                raise ProcessException with "Futex overflow set full";
            end if;
            overflowCount := overflowCount + 1;
            Spinlocks.exitCriticalSection (overflowLock);
            threadtab (me).futexInOverflow := True;
            threadtab (me).futexSlot := Natural (largeSlot);
        end if;
        threadtab (me).futexBucket := b;
        threadtab (me).futexTimedOut := False;
        if deadlineMs /= FOREVER then
            threadtab (me).futexDeadlineMs := deadlineMs;
            threadtab (me).futexDeadlineActive := True;
            deadlines (b) := deadlines (b) + 1;
        end if;
        threadtab (me).futexWaiting := True;
        -- Published before the bucket lock is released: a waker, which
        -- needs that lock, always finds this state.
        threadtab (me).state := FUTEXWAITING;
        Spinlocks.exitCriticalSection (bucketLocks (b));

        yield;

        return (if threadtab (me).futexTimedOut then FUTEX_TIMED_OUT
                else FUTEX_WOKEN);
    end wait;

    -- Caller holds bucketLocks (Bucket_Of (k)). Remove k's oldest waiter
    -- across the bucket and the overflow set, by ticket.
    procedure wakeOldest (k : Key; w : out Waiter_Id) is
        b : constant Bucket_Index := Bucket_Of (k);
        inSmall, inLarge : Boolean := False;
        smallSlot : Small.Slot_Index;
        largeSlot : Large.Slot_Index;
        from : Small.Slot_Index;
        fromLarge : Large.Slot_Index;
    begin
        w := No_Waiter;
        Small.Find_Oldest (buckets (b), k, inSmall, smallSlot);
        if overflowCount > 0 then
            Spinlocks.enterCriticalSection (overflowLock);
            Large.Find_Oldest (overflow, k, inLarge, largeSlot);
            if inLarge and then
               (not inSmall or else
                overflow.S (largeSlot).Ticket < buckets (b).S (smallSlot).Ticket)
            then
                Large.Wake_One (overflow, k, w, fromLarge);
                overflowCount := overflowCount - 1;
                Spinlocks.exitCriticalSection (overflowLock);
                return;
            end if;
            Spinlocks.exitCriticalSection (overflowLock);
        end if;
        if inSmall then
            Small.Wake_One (buckets (b), k, w, from);
        end if;
    end wakeOldest;

    function wakeFor (pid : ProcessID; address, count : Unsigned_64)
      return Unsigned_64
    is
        k : constant Key := keyOf (pid, address);
        b : constant Bucket_Index := Bucket_Of (k);
        woken : Unsigned_64 := 0;
        w : Waiter_Id;
    begin
        if pid = NO_PROCESS or else address mod 4 /= 0 then
            return 0;
        end if;
        Spinlocks.enterCriticalSection (bucketLocks (b));
        while woken < count loop
            wakeOldest (k, w);
            exit when w = No_Waiter;
            finishWait (ThreadID (w), b, timedOut => False);
            woken := woken + 1;
        end loop;
        Spinlocks.exitCriticalSection (bucketLocks (b));
        return woken;
    end wakeFor;

    function wake (address, count : Unsigned_64) return Unsigned_64 is
    begin
        return wakeFor (PerCPUData.getCurrentPID, address, count);
    end wake;

    -- Expired overflow waiters found by one pass (BSP tick only).
    expiredOverflow : array (1 .. Futex_Keys.Max_Waiter) of ThreadID;

    procedure expireDeadlines (nowMs : Unsigned_64) is
        t : ThreadID;
        removed : Boolean;
        anyTimed : Boolean := False;
        found : Natural := 0;
        b : Bucket_Index;
    begin
        for bi in Bucket_Index loop
            if deadlines (bi) > 0 then
                anyTimed := True;
                Spinlocks.enterCriticalSection (bucketLocks (bi));
                for s in Small.Slot_Index loop
                    if buckets (bi).S (s).Used then
                        t := ThreadID (buckets (bi).S (s).Waiter);
                        if threadtab (t).futexDeadlineActive and then
                           threadtab (t).futexDeadlineMs <= nowMs
                        then
                            removeWaiter (t, removed);
                            if removed then
                                finishWait (t, bi, timedOut => True);
                            end if;
                        end if;
                    end if;
                end loop;
                Spinlocks.exitCriticalSection (bucketLocks (bi));
            end if;
        end loop;

        -- Overflow waiters: one pass under overflowLock to find candidates,
        -- then each is removed under its bucket lock after a recheck (the
        -- bucket lock is taken before overflowLock, never after).
        if not anyTimed or else overflowCount = 0 then
            return;
        end if;
        Spinlocks.enterCriticalSection (overflowLock);
        for s in Large.Slot_Index loop
            if overflow.S (s).Used then
                t := ThreadID (overflow.S (s).Waiter);
                if threadtab (t).futexDeadlineActive and then
                   threadtab (t).futexDeadlineMs <= nowMs
                then
                    found := found + 1;
                    expiredOverflow (found) := t;
                end if;
            end if;
        end loop;
        Spinlocks.exitCriticalSection (overflowLock);
        for n in 1 .. found loop
            t := expiredOverflow (n);
            b := threadtab (t).futexBucket;
            Spinlocks.enterCriticalSection (bucketLocks (b));
            -- Still this wait? (It may have been woken, or the thread may
            -- have waited again elsewhere, since the pass.)
            if threadtab (t).futexBucket = b and then
               threadtab (t).futexWaiting and then
               threadtab (t).futexInOverflow and then
               threadtab (t).futexDeadlineActive and then
               threadtab (t).futexDeadlineMs <= nowMs
            then
                removeWaiter (t, removed);
                if removed then
                    finishWait (t, b, timedOut => True);
                end if;
            end if;
            Spinlocks.exitCriticalSection (bucketLocks (b));
        end loop;
    end expireDeadlines;

    procedure cancelWait (tid : ThreadID) is
        b : constant Bucket_Index := threadtab (tid).futexBucket;
        removed : Boolean;
    begin
        -- Decide under the bucket lock: a waker or the deadline tick may be
        -- finishing this thread's wait right now (finishWait holds the lock).
        -- The bucket index is the last one this thread waited on.
        Spinlocks.enterCriticalSection (bucketLocks (b));
        if threadtab (tid).futexWaiting then
            removeWaiter (tid, removed);
            if removed then
                threadtab (tid).futexWaiting := False;
                if threadtab (tid).futexDeadlineActive then
                    threadtab (tid).futexDeadlineActive := False;
                    deadlines (b) := deadlines (b) - 1;
                end if;
            end if;
        end if;
        Spinlocks.exitCriticalSection (bucketLocks (b));
    end cancelWait;
end Process.Futex;
