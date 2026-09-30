-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- CuBitOS Process Queues
-------------------------------------------------------------------------------
with Spinlocks;
with Virtual_Deadlines;
with Work_Stealing;
with x86;
with Time;
with TextIO; use TextIO;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
with Interfaces;
package body Process.Queues is

    function isInSleepQueue (pid : ThreadID) return Boolean;
    procedure popItemNoLock (q : in out ProcQueue; pid : ThreadID;
                            result : out ThreadID);

    procedure initQueue (q : in out ProcQueue; locknamePtr : Spinlocks.Lock_Name)

    is
    begin
        Spinlocks.Initialize (q.lock, locknamePtr);
        q.head := NO_THREAD;
        q.tail := NO_THREAD;
    end initQueue;

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean

    is
    begin
        return (q.head = NO_THREAD);
    end isEmpty;

    function headKey (q : in out ProcQueue) return Interfaces.Unsigned_64 is
        result : Interfaces.Unsigned_64;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        result := (if q.head = NO_THREAD then Virtual_Deadlines.Idle_Key
                   else threadtab (q.head).runKey);
        Spinlocks.exitCriticalSection (q.lock);
        return result;
    end headKey;

    -- The rule itself is Work_Stealing.Eligible (SPARK, proved): ordinary
    -- work, unpinned, not retiring, not still switching out on another CPU.
    -- When work moves is Virtual_Deadlines.Choose's decision, so no queueing
    -- age is required.
    function isStealable (pid : ThreadID) return Boolean is
      (Work_Stealing.Eligible
         ((Priority  => threadtab (pid).priority,
           Pinned    => threadtab (pid).pinned,
           Closing   => Process_Lifetime.Closing (threadtab (pid).lifetime),
           Executing => Process_Lifetime.Executing (threadtab (pid).lifetime),
           Queued_At => threadtab (pid).queuedTSC),
          Now                   => x86.rdtsc,
          Ticks_Per_Microsecond => Time.tscPerDuration,
          Age_Microseconds      => 0));

    function takeableKey (q : in out ProcQueue) return Interfaces.Unsigned_64 is
        Cursor : ThreadID;
        result : Interfaces.Unsigned_64 := Virtual_Deadlines.Idle_Key;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        Cursor := q.head;
        while Cursor /= NO_THREAD loop
            if isStealable (Cursor) then
                result := threadtab (Cursor).runKey;
                exit;
            end if;
            Cursor := threadtab (Cursor).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
        return result;
    end takeableKey;

    procedure takeFirst (q : in out ProcQueue; result : out ThreadID) is
        Cursor : ThreadID;
        Ignored : ThreadID;
    begin
        result := NO_THREAD;
        Spinlocks.enterCriticalSection (q.lock);
        Cursor := q.head;
        while Cursor /= NO_THREAD loop
            if isStealable (Cursor) then
                popItemNoLock (q, Cursor, Ignored);
                threadtab (Cursor).prev := NO_THREAD;
                threadtab (Cursor).next := NO_THREAD;
                result := Cursor;
                exit;
            end if;
            Cursor := threadtab (Cursor).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
    end takeFirst;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ThreadID)

    is
    begin
        -- Selection and removal must use the same lock acquisition.
        dequeue (q, result);
    end popFront;

    ---------------------------------------------------------------------------
    -- popBack
    ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ThreadID)

    is
    begin
        Spinlocks.enterCriticalSection (q.lock);
        if isEmpty(q) then
            result := NO_THREAD;
        else
            popItemNoLock (q, q.tail, result);
            threadtab (result).prev := NO_THREAD;
            threadtab (result).next := NO_THREAD;
        end if;
        Spinlocks.exitCriticalSection (q.lock);
    end popBack;

    ---------------------------------------------------------------------------
    -- popItemNoLock
    -- Remove an item without acquiring the lock. Internal
    -- functions in Process.Queue that already hold the lock should use this.
    --
    -- Public clients of the Process.Queues package use popItem which will hold
    -- the lock.
    ---------------------------------------------------------------------------
    procedure popItemNoLock (q : in out ProcQueue; pid : ThreadID;
        result : out ThreadID)

    is
        prev, next : ThreadID;
    begin

        next := threadtab (pid).next;
        prev := threadtab (pid).prev;

        -- Unlink this process from its current list
        if prev /= NO_THREAD then
            threadtab (prev).next := next;
        else
            -- first element in list
            q.head := next;
        end if;

        if next /= NO_THREAD then
            threadtab (next).prev := prev;
        else
            -- last element
            q.tail := prev;
        end if;

        result := pid;
    end popItemNoLock;

    ---------------------------------------------------------------------------
    -- popItem
    ---------------------------------------------------------------------------
    procedure popItem (q : in out ProcQueue; pid : ThreadID;
        result : out ThreadID)

    is
    begin
        Spinlocks.enterCriticalSection (q.lock);

        popItemNoLock (q, pid, result);

        Spinlocks.exitCriticalSection (q.lock);
    end popItem;

    procedure detach (q : in out ProcQueue; pid : ThreadID) is
        current, ignored : ThreadID;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        current := q.head;
        while current /= NO_THREAD loop
            if current = pid then
                popItemNoLock (q, pid, ignored);
                threadtab (pid).next := NO_THREAD;
                threadtab (pid).prev := NO_THREAD;
                exit;
            end if;
            current := threadtab (current).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
    end detach;

    ---------------------------------------------------------------------------
    -- enqueue - add to the back of the list while holding the list's lock
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ThreadID;
        result : out ThreadID)

    is
        prev : ThreadID;
    begin

        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            threadtab (pid).prev := NO_THREAD;
            threadtab (pid).next := NO_THREAD;
        else
            prev := q.tail;
            threadtab (pid).prev := prev;
            threadtab (pid).next := NO_THREAD;
            threadtab (prev).next := pid;
            q.tail := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end enqueue;

    ---------------------------------------------------------------------------
    -- dequeueNoLock
    ---------------------------------------------------------------------------
    procedure dequeueNoLock (q : in out ProcQueue; result : out ThreadID)

    is
        pid : ThreadID;
    begin

        if isEmpty (q) then
            result := NO_THREAD;
            return;
        end if;

        popItemNoLock (q, q.head, pid);

        threadtab (pid).prev := NO_THREAD;
        threadtab (pid).next := NO_THREAD;

        result := pid;
    end dequeueNoLock;

    ---------------------------------------------------------------------------
    -- dequeue - remove from front of the list while holding the list's lock
    ---------------------------------------------------------------------------
    procedure dequeuePreferring (q : in out ProcQueue; cpu : Natural;
                                 result : out ThreadID)
    is
        cursor : ThreadID;
        pid    : ThreadID;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        if isEmpty (q) then
            Spinlocks.exitCriticalSection (q.lock);
            result := NO_THREAD;
            return;
        end if;
        cursor := q.head;
        while cursor /= NO_THREAD and then threadtab (cursor).cpu /= cpu loop
            cursor := threadtab (cursor).next;
        end loop;
        if cursor = NO_THREAD then
            cursor := q.head;
        end if;
        popItemNoLock (q, cursor, pid);
        threadtab (pid).prev := NO_THREAD;
        threadtab (pid).next := NO_THREAD;
        Spinlocks.exitCriticalSection (q.lock);
        result := pid;
    end dequeuePreferring;

    procedure dequeue (q : in out ProcQueue; result : out ThreadID)

    is
        pid : ThreadID;
    begin

        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            Spinlocks.exitCriticalSection (q.lock);
            result := NO_THREAD;
            return;
        end if;

        popItemNoLock (q, q.head, pid);

        threadtab (pid).prev := NO_THREAD;
        threadtab (pid).next := NO_THREAD;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end dequeue;

    ---------------------------------------------------------------------------
    -- Insert in ascending key order, FIFO among equal keys.
    ---------------------------------------------------------------------------
    procedure insertByKey (q      : in out ProcQueue;
                           pid    : ThreadID;
                           key    : Interfaces.Unsigned_64;
                           result : out ThreadID)
    is
    begin
        Spinlocks.enterCriticalSection (q.lock);
        insertByKeyNoLock (q, pid, key, result);
        Spinlocks.exitCriticalSection (q.lock);
    end insertByKey;

    procedure insertByKeyNoLock (q      : in out ProcQueue;
                                 pid    : ThreadID;
                                 key    : Interfaces.Unsigned_64;
                                 result : out ThreadID)
    is
        use type Interfaces.Unsigned_64;
        curr : ThreadID;
    begin
        threadtab (pid).runKey := key;

        -- The first entry with a later key; the new one goes before it.
        curr := q.head;
        while curr /= NO_THREAD and then threadtab (curr).runKey <= key loop
            curr := threadtab (curr).next;
        end loop;

        if curr = NO_THREAD then
            -- Append at the tail.
            threadtab (pid).next := NO_THREAD;
            threadtab (pid).prev := q.tail;
            if q.tail /= NO_THREAD then
                threadtab (q.tail).next := pid;
            else
                q.head := pid;
            end if;
            q.tail := pid;
        else
            threadtab (pid).next := curr;
            threadtab (pid).prev := threadtab (curr).prev;
            if threadtab (curr).prev /= NO_THREAD then
                threadtab (threadtab (curr).prev).next := pid;
            else
                q.head := pid;
            end if;
            threadtab (curr).prev := pid;
        end if;
        result := pid;
    end insertByKeyNoLock;

    ---------------------------------------------------------------------------
    -- isInSleepQueue
    -- Check whether the process is actually linked in the sleep list.
    -- Must be called while holding sleepList.lock.
    ---------------------------------------------------------------------------
    function isInSleepQueue (pid : ThreadID) return Boolean

    is
        curr : ThreadID := sleepList.head;
    begin
        while curr /= NO_THREAD loop
            if curr = pid then
                return True;
            end if;
            curr := threadtab (curr).next;
        end loop;
        return False;
    end isInSleepQueue;

    ---------------------------------------------------------------------------
    -- wakeFromSleep
    -- Remove a specific process from the sleep list and ready it.
    -- Verifies the process is actually in the queue to avoid corruption
    -- from a race with Process.sleep.
    ---------------------------------------------------------------------------
    procedure wakeFromSleep (pid : ThreadID; woken : out Boolean)

    is
        ignore  : ThreadID;
    begin
        woken := False;
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        if threadtab (pid).state = SLEEPING and then
           isInSleepQueue (pid)
        then
            popItemNoLock (sleepList, pid, ignore);
            woken := True;
        end if;

        Spinlocks.exitCriticalSection (sleepList.lock);

        if woken then
            ready (pid);
        end if;
        Spinlocks.exitCriticalSection (lock);
    end wakeFromSleep;

    ---------------------------------------------------------------------------
    -- nextWake / expireSleepers
    ---------------------------------------------------------------------------
    function nextWake return Interfaces.Unsigned_64 is
        head : constant ThreadID := sleepList.head;
    begin
        return (if head = NO_THREAD then Virtual_Deadlines.Idle_Key
                else threadtab (head).runKey);
    end nextWake;

    function nextWakeOn (cpu : Natural; horizon : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
    is
        use type Interfaces.Unsigned_64;
        cursor : ThreadID;
        result : Interfaces.Unsigned_64 := Virtual_Deadlines.Idle_Key;
    begin
        Spinlocks.enterCriticalSection (sleepList.lock);
        cursor := sleepList.head;
        while cursor /= NO_THREAD and then threadtab (cursor).runKey <= horizon loop
            if threadtab (cursor).cpu = cpu then
                result := threadtab (cursor).runKey;
                exit;
            end if;
            cursor := threadtab (cursor).next;
        end loop;
        Spinlocks.exitCriticalSection (sleepList.lock);
        return result;
    end nextWakeOn;

    procedure expireSleepers (now : Interfaces.Unsigned_64) is
        use type Interfaces.Unsigned_64;
        due : ThreadID;
    begin
        Spinlocks.enterCriticalSection (lock);
        loop
            Spinlocks.enterCriticalSection (sleepList.lock);
            if not isEmpty (sleepList) and then
               threadtab (sleepList.head).runKey <= now
            then
                dequeueNoLock (sleepList, due);
            else
                due := NO_THREAD;
            end if;
            Spinlocks.exitCriticalSection (sleepList.lock);
            exit when due = NO_THREAD;
            ready (due);
        end loop;
        Spinlocks.exitCriticalSection (lock);
    end expireSleepers;


    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue)
    is
        curr : ThreadID := q.head;
    begin
        println ("Process.Queues: ");

        if isEmpty (q) then
            println (" * Empty.");
            return;
        end if;

        while curr /= NO_THREAD loop
            println ("* " & proctab(processOf (curr)).name & " key: " & threadtab (curr).queueKey'Image);
            curr := threadtab (curr).next;
        end loop;

    end print;

end Process.Queues;
