with PerCPUData;
with Process.IPC;
with Process.User_Memory;
with Futex_ABI;
with Futex_Queues;
with Memory_Grants;
with System.Machine_Code;
with Process.Wait_Timers;

package body Process.Futex is
    use Futex_Keys;

    -- A futex waiter is a thread.
    pragma Compile_Time_Error
      (Futex_Keys.Max_Waiter /= Natural (ThreadID'Last),
       "Futex_Keys.Waiter_Id must cover ThreadID");
    -- A shared word's slot field names every grant slot.
    pragma Compile_Time_Error
      (Memory_Grants.Maximum_Grants > Futex_ABI.Slot_Limit,
       "Futex_ABI.Slot_Bits must cover Memory_Grants.Global_Slot");

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

    -- How a waiter's wait ended.
    type Ending is (By_Wake, By_Timeout, By_Revocation);

    -- Caller holds t's bucket lock; t was just removed from its structure.
    -- The bucket lock is held until the thread is readied, so cancelWait
    -- (which takes it) never frees a thread still being finished here.
    -- sync: the waker is about to sleep (wake-plus-wait): place t on this
    -- CPU for a direct switch (readySync).
    procedure finishWait (t : ThreadID; how : Ending; sync : Boolean := False) is
    begin
        threadtab (t).oneWaitStatus := (case how is
                                          when By_Wake       => Wait_Woken,
                                          when By_Timeout    => Wait_Timed_Out,
                                          when By_Revocation => Wait_Ended);
        threadtab (t).futexTimedOut := how = By_Timeout;
        threadtab (t).futexRevoked := how = By_Revocation;
        threadtab (t).futexWaiting := False;
        Spinlocks.enterCriticalSection (lock);
        if threadtab (t).state = FUTEXWAITING then
            if sync then
                readySync (t);
            else
                ready (t);
            end if;
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

    -- The key and bucket of a shared word the calling process names, and
    -- where the word is in its address space, resolved under the bucket
    -- lock, which the caller then holds (locked). ok False: malformed, or
    -- not a party of a live grant.
    procedure lockWord (pid : ProcessID; word : Unsigned_64;
                        k : out Key; b : out Bucket_Index; address : out Unsigned_64;
                        owner : out ProcessID; ownerAddress : out Unsigned_64;
                        locked : out Boolean; ok : out Boolean) is
        generation : Unsigned_32;
    begin
        k := No_Key;
        b := Bucket_Index'First;
        address := 0;
        owner := NO_PROCESS;
        ownerAddress := 0;
        locked := False;
        ok := False;
        if pid = NO_PROCESS or else not Futex_ABI.Well_Formed (word) then
            return;
        end if;
        b := Grant_Bucket (Unsigned_32 (Futex_ABI.Slot_Of (word)));
        -- The grant is checked live under the bucket lock: its end's sweep
        -- needs this lock, so a waiter that saw it live is swept.
        Spinlocks.enterCriticalSection (bucketLocks (b));
        locked := True;
        IPC.resolveSharedWord (pid, Futex_ABI.Slot_Of (word), Futex_ABI.Offset_Of (word),
                               generation, address, owner, ownerAddress, ok);
        if ok then
            k := Shared_Key (Unsigned_32 (Futex_ABI.Slot_Of (word)), generation,
                             Futex_ABI.Offset_Of (word));
            pragma Assert (Bucket_Of (k) = b);
        end if;
    end lockWord;

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

    -- Caller holds bucketLocks (Bucket_Of (k)). Wake up to count of k's
    -- waiters, oldest first; the first placed on this CPU when sync.
    function wakeKeyLocked (k : Key; count : Unsigned_64; sync : Boolean)
      return Unsigned_64
    is
        woken : Unsigned_64 := 0;
        w : Waiter_Id;
    begin
        while woken < count loop
            wakeOldest (k, w);
            exit when w = No_Waiter;
            finishWait (ThreadID (w), By_Wake, sync => sync and then woken = 0);
            woken := woken + 1;
        end loop;
        return woken;
    end wakeKeyLocked;

    -- Wake up to count waiters of the shared word the calling process
    -- names. WAKE_REFUSED: malformed, or not a party of a live grant.
    function wakeWordOf (pid : ProcessID; word, count : Unsigned_64; sync : Boolean)
      return Unsigned_64
    is
        k : Key;
        b : Bucket_Index;
        address : Unsigned_64;
        owner : ProcessID;
        ownerAddress : Unsigned_64;
        locked, ok : Boolean;
        woken : Unsigned_64;
    begin
        lockWord (pid, word, k, b, address, owner, ownerAddress, locked, ok);
        if not ok then
            if locked then
                Spinlocks.exitCriticalSection (bucketLocks (b));
            end if;
            return WAKE_REFUSED;
        end if;
        woken := wakeKeyLocked (k, count, sync);
        Spinlocks.exitCriticalSection (bucketLocks (b));
        -- The word's owner may have named it as its wake word (IPC-010
        -- option B): its Ready wait and its receive wake too.
        if woken < count and then
           IPC.noteWakeWord (owner, ownerAddress, sync => sync and then woken = 0)
        then
            woken := woken + 1;
        end if;
        return woken;
    end wakeWordOf;

    -- The wait proper: the word at address holds expected, checked and
    -- enqueued under bucketLocks (b), which the caller holds and this
    -- releases. handOff: switch to this thread's sync wakee on sleeping.
    function waitLocked (pid : ProcessID; k : Key; b : Bucket_Index;
                         address, expected : Unsigned_64; passed : Boolean;
                         owner : ProcessID; handOff : Boolean) return Unsigned_64
    is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        value : Unsigned_32;
        ok : Boolean;
        ticket : Unsigned_64;
        smallSlot : Small.Slot_Index;
        largeSlot : Large.Slot_Index;
    begin
        -- Through this process's mapping, of the grant owner's frame.
        User_Memory.Load_Shared_Word32 (pid, owner, address, value, ok);
        if not ok then
            Spinlocks.exitCriticalSection (bucketLocks (b));
            return FUTEX_FAULT;
        elsif Unsigned_64 (value) /= expected then
            Spinlocks.exitCriticalSection (bucketLocks (b));
            return FUTEX_RETRY;
        elsif passed or else threadtab (me).oneWaitStatus = Wait_Timed_Out then
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
        threadtab (me).futexRevoked := False;
        threadtab (me).futexWaiting := True;
        -- Published before the bucket lock is released: a waker, which
        -- needs that lock, always finds this state.
        threadtab (me).state := FUTEXWAITING;
        Spinlocks.exitCriticalSection (bucketLocks (b));

        if handOff then
            yieldToSyncWakee;
        else
            yield;
        end if;

        return (if threadtab (me).futexRevoked then FUTEX_REVOKED
                elsif threadtab (me).futexTimedOut then FUTEX_TIMED_OUT
                else FUTEX_WOKEN);
    end waitLocked;

    function waitForm
      (shared     : Boolean;
       word       : Unsigned_64;
       expected   : Unsigned_64;
       deadlineMs : Unsigned_64;
       wakeFirst  : Boolean;
       wakeShared : Boolean;
       wakeWord   : Unsigned_64) return Unsigned_64
    is
        me  : constant ThreadID := PerCPUData.getCurrentThread;
        pid : constant ProcessID := processOf (me);
        k : Key;
        b : Bucket_Index;
        address : Unsigned_64;
        owner : ProcessID;
        ownerAddress : Unsigned_64;
        locked, ok : Boolean;
        result : Unsigned_64;
        timed : Boolean;
        passed : Boolean := False;
        tsc : Unsigned_64 := 0;
    begin
        -- Private words wait in their process's own table (Wait_One).
        if pid = NO_PROCESS or else not shared or else (wakeFirst and then not wakeShared)
          or else expected > Unsigned_64 (Unsigned_32'Last)
        then
            return FUTEX_FAULT;
        end if;
        if wakeFirst then
            -- The hand-off's wake: one waiter, placed here, never lost
            -- (an ordinary wake under its bucket lock), whatever the wait
            -- then answers.
            if wakeWordOf (pid, wakeWord, 1, sync => True) = WAKE_REFUSED then
                return FUTEX_FAULT;
            end if;
        end if;
        -- A deadline joins this CPU's heap first (Process.Wait_Timers:
        -- heap lock, then bucket lock); its expiry finds the bucket here.
        timed := deadlineMs /= FOREVER;
        if timed then
            Wait_Timers.Deadline_Of_Ms (deadlineMs, tsc, passed);
            timed := not passed;
        end if;
        threadtab (me).oneWaitKind := Shared_Word_Wait;
        threadtab (me).oneWaitStatus := Wait_Pending;
        if Futex_ABI.Well_Formed (word) then
            threadtab (me).futexBucket := Grant_Bucket (Unsigned_32 (Futex_ABI.Slot_Of (word)));
        end if;
        if timed then
            Wait_Timers.Arm (me, tsc);
        end if;
        lockWord (pid, word, k, b, address, owner, ownerAddress, locked, ok);
        if not ok then
            if locked then
                Spinlocks.exitCriticalSection (bucketLocks (b));
            end if;
            result := FUTEX_FAULT;
        else
            result := waitLocked (pid, k, b, address, expected, passed, owner,
                                  handOff => wakeFirst);
        end if;
        if timed then
            Wait_Timers.Disarm (me);
        end if;
        threadtab (me).oneWaitKind := No_One_Wait;
        -- A wait that did not sleep leaves the wakee, if any, to run when
        -- the scheduler finds it due, as after any readySync.
        threadtab (me).syncWakee := NO_THREAD;
        return result;
    end waitForm;

    function wakeSpace (word, count, space : Unsigned_64; sync : Boolean;
                        urgent : Boolean := False) return Unsigned_64 is
    begin
        if space = Futex_ABI.Shared_Space then
            return wakeWordOf (PerCPUData.getCurrentPID, word, count, sync => sync);
        elsif space = Futex_ABI.Peer_Space then
            return IPC.wakePeer (PerCPUData.getCurrentPID, word, sync, urgent);
        else
            return WAKE_REFUSED;
        end if;
    end wakeSpace;

    procedure endSharedLife (slot, generation : Unsigned_32) is
        b : constant Bucket_Index := Grant_Bucket (slot);
        w : Waiter_Id;
        from : Small.Slot_Index;
        fromLarge : Large.Slot_Index;
    begin
        Spinlocks.enterCriticalSection (bucketLocks (b));
        -- Every waiter of that life is in this bucket or the overflow set
        -- (Futex_Keys.Prove_Grant_One_Bucket); no other key is touched
        -- (Futex_Queues.Remove_Of_Life).
        loop
            Small.Remove_Of_Life (buckets (b), slot, generation, w, from);
            exit when w = No_Waiter;
            finishWait (ThreadID (w), By_Revocation);
        end loop;
        if overflowCount > 0 then
            loop
                Spinlocks.enterCriticalSection (overflowLock);
                Large.Remove_Of_Life (overflow, slot, generation, w, fromLarge);
                if w /= No_Waiter then
                    overflowCount := overflowCount - 1;
                end if;
                Spinlocks.exitCriticalSection (overflowLock);
                exit when w = No_Waiter;
                finishWait (ThreadID (w), By_Revocation);
            end loop;
        end if;
        Spinlocks.exitCriticalSection (bucketLocks (b));
    end endSharedLife;

    -- Shared overflow waiters of dead lives found by one pass.
    deadOverflow : array (1 .. Futex_Keys.Max_Waiter) of ThreadID;
    deadOverflowLock : Spinlocks.Spinlock;

    procedure endDeadSharedLives is
        t : ThreadID;
        removed : Boolean;
        k : Key;
        found : Natural := 0;
        b : Bucket_Index;
    begin
        for bi in Bucket_Index loop
            Spinlocks.enterCriticalSection (bucketLocks (bi));
            for s in Small.Slot_Index loop
                if buckets (bi).S (s).Used then
                    k := buckets (bi).S (s).K;
                    if not IPC.sharedLifeLive (k.Owner, k.Generation) then
                        t := ThreadID (buckets (bi).S (s).Waiter);
                        removeWaiter (t, removed);
                        if removed then
                            finishWait (t, By_Revocation);
                        end if;
                    end if;
                end if;
            end loop;
            Spinlocks.exitCriticalSection (bucketLocks (bi));
        end loop;

        if overflowCount = 0 then
            return;
        end if;
        -- Overflow waiters: candidates under overflowLock, each rechecked
        -- and removed under its bucket lock (taken before overflowLock).
        Spinlocks.enterCriticalSection (deadOverflowLock);
        Spinlocks.enterCriticalSection (overflowLock);
        for s in Large.Slot_Index loop
            if overflow.S (s).Used then
                found := found + 1;
                deadOverflow (found) := ThreadID (overflow.S (s).Waiter);
            end if;
        end loop;
        Spinlocks.exitCriticalSection (overflowLock);
        for n in 1 .. found loop
            t := deadOverflow (n);
            b := threadtab (t).futexBucket;
            Spinlocks.enterCriticalSection (bucketLocks (b));
            if threadtab (t).futexBucket = b and then threadtab (t).futexWaiting
              and then threadtab (t).futexInOverflow
            then
                Spinlocks.enterCriticalSection (overflowLock);
                k := overflow.S (Large.Slot_Index (threadtab (t).futexSlot)).K;
                Spinlocks.exitCriticalSection (overflowLock);
                if not IPC.sharedLifeLive (k.Owner, k.Generation)
                then
                    removeWaiter (t, removed);
                    if removed then
                        finishWait (t, By_Revocation);
                    end if;
                end if;
            end if;
            Spinlocks.exitCriticalSection (bucketLocks (b));
        end loop;
        Spinlocks.exitCriticalSection (deadOverflowLock);
    end endDeadSharedLives;

    procedure Time_Out (t : ThreadID) is
        b : constant Bucket_Index := threadtab (t).futexBucket;
        removed : Boolean;
    begin
        Spinlocks.enterCriticalSection (bucketLocks (b));
        if threadtab (t).oneWaitStatus = Wait_Pending then
            if threadtab (t).futexWaiting then
                removeWaiter (t, removed);
                if removed then
                    finishWait (t, By_Timeout);
                end if;
            else
                -- Not yet enqueued: it sees this under the lock.
                threadtab (t).oneWaitStatus := Wait_Timed_Out;
            end if;
        end if;
        Spinlocks.exitCriticalSection (bucketLocks (b));
    end Time_Out;

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
            end if;
        end if;
        Spinlocks.exitCriticalSection (bucketLocks (b));
    end cancelWait;
end Process.Futex;
