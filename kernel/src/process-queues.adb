-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- CuBitOS Process Queues
-------------------------------------------------------------------------------
with Spinlocks;
with Scheduler_Timing;
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

    function hasReadyPeer (q : in out ProcQueue; priority : Integer;
                          relation : Priority_Query := At_Least) return Boolean is
        result : Boolean;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        result := q.head /= NO_THREAD and then
          (if relation = Strictly_Higher then threadtab (q.head).queueKey > priority
           else threadtab (q.head).queueKey >= priority);
        Spinlocks.exitCriticalSection (q.lock);
        return result;
    end hasReadyPeer;

    function hasAwakenedPeer (q : in out ProcQueue; priority : Integer) return Boolean is
        Cursor : ThreadID;
        Found : Boolean := False;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        Cursor := q.head;
        while Cursor /= NO_THREAD and then threadtab (Cursor).queueKey >= priority loop
            if threadtab (Cursor).readiness = Awakened then
                Found := True;
                exit;
            end if;
            Cursor := threadtab (Cursor).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
        return Found;
    end hasAwakenedPeer;

    -- The rule itself is Work_Stealing.Eligible (SPARK, proved): ordinary
    -- work, unpinned, not retiring, not still switching out on another CPU,
    -- and queued at least Steal_Age_Microseconds. A freshly woken IPC partner
    -- therefore stays on its CPU.
    function isStealable (pid : ThreadID) return Boolean is
      (Work_Stealing.Eligible
         ((Priority  => threadtab (pid).queueKey,
           Pinned    => threadtab (pid).pinned,
           Closing   => Process_Lifetime.Closing (threadtab (pid).lifetime),
           Executing => Process_Lifetime.Executing (threadtab (pid).lifetime),
           Queued_At => threadtab (pid).queuedTSC),
          Now                   => x86.rdtsc,
          Ticks_Per_Microsecond => Time.tscPerDuration,
          Age_Microseconds      => Scheduler_Timing.Steal_Age_Microseconds));

    function hasStealable (q : in out ProcQueue) return Boolean is
        Cursor : ThreadID;
        Found : Boolean := False;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        Cursor := q.head;
        while Cursor /= NO_THREAD loop
            if isStealable (Cursor) then
                Found := True;
                exit;
            end if;
            Cursor := threadtab (Cursor).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
        return Found;
    end hasStealable;

    procedure stealFrom (q : in out ProcQueue; result : out ThreadID) is
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
    end stealFrom;

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

    procedure detach (q : in out ProcQueue; pid : ThreadID;
                      kind : Removal_Kind := Ordinary_Queue) is
        current, ignored, following : ThreadID;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        current := q.head;
        while current /= NO_THREAD loop
            if current = pid then
                following := threadtab (pid).next;
                if kind = Delta_Queue and then following /= NO_THREAD then
                    threadtab (following).queueKey :=
                        threadtab (following).queueKey + threadtab (pid).queueKey;
                end if;
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
    -- Insert in descending key order, FIFO among equal keys.
    ---------------------------------------------------------------------------
    procedure insert (q      : in out ProcQueue;
                      pid    : ThreadID;
                      key    : Integer;
                      result : out ThreadID;
                      placement : Equal_Placement := After_Peers)

    is
        curr : ThreadID;
        prev : ThreadID;
    begin
        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            -- empty list.
            q.head := pid;
            q.tail := pid;
            threadtab (pid).prev     := NO_THREAD;
            threadtab (pid).next     := NO_THREAD;
            threadtab (pid).queueKey := key;

            Spinlocks.exitCriticalSection (q.lock);
            result := pid;
            return;
        end if;

        -- Walk the list to find the right insertion point (descending key order).
        curr := q.head;

        loop
            exit when key > threadtab (curr).queueKey or else
              (placement = Resume_Turn and then key = threadtab (curr).queueKey) or else
              threadtab (curr).next = NO_THREAD;
            curr := threadtab (curr).next;
        end loop;

        if key > threadtab (curr).queueKey or else
          (placement = Resume_Turn and then key = threadtab (curr).queueKey)
        then
            -- Only an unfinished, higher-priority-preempted turn can resume
            -- ahead of equal peers. Ordinary rotation remains FIFO.
            prev                  := threadtab (curr).prev;
            threadtab (pid).next     := curr;
            threadtab (pid).prev     := prev;
            threadtab (pid).queueKey := key;
            threadtab (curr).prev    := pid;

            if prev /= NO_THREAD then
                threadtab (prev).next := pid;
            else
                q.head := pid;
            end if;
        else
            -- Append AFTER curr (new node has lowest priority, goes at tail)
            threadtab (pid).next     := NO_THREAD;
            threadtab (pid).prev     := curr;
            threadtab (pid).queueKey := key;
            threadtab (curr).next    := pid;
            q.tail                := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end insert;

    ---------------------------------------------------------------------------
    -- insertDelta
    ---------------------------------------------------------------------------
    procedure insertDelta (q            : in out ProcQueue;
                           pid          : ThreadID;
                           delayFromNow : Integer;
                           result       : out ThreadID)

    is
        -- accumDelay tracks the absolute wakeup time of all entries
        -- before the current insertion point.
        accumDelay : Integer := 0;

        prev, curr : ThreadID;
    begin
        Spinlocks.enterCriticalSection (q.lock);

        -- Initialize new node's links
        threadtab (pid).next := NO_THREAD;
        threadtab (pid).prev := NO_THREAD;

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            threadtab (pid).queueKey := delayFromNow;

            Spinlocks.exitCriticalSection (q.lock);
            result := pid;
            return;
        end if;

        curr := q.head;

        -- Walk the delta list.  accumDelay + curr.queueKey gives the
        -- absolute wakeup time of curr.  Insert before the first node
        -- whose absolute time exceeds our delay.
        loop
            if delayFromNow < accumDelay + threadtab (curr).queueKey then
                -- Insert before curr
                threadtab (pid).queueKey := delayFromNow - accumDelay;

                -- Reduce curr's delta (now relative to the new node)
                threadtab (curr).queueKey :=
                    threadtab (curr).queueKey - threadtab (pid).queueKey;

                prev := threadtab (curr).prev;
                threadtab (pid).next := curr;
                threadtab (pid).prev := prev;
                threadtab (curr).prev := pid;

                if prev /= NO_THREAD then
                    threadtab (prev).next := pid;
                else
                    q.head := pid;
                end if;

                Spinlocks.exitCriticalSection (q.lock);
                result := pid;
                return;
            end if;

            accumDelay := accumDelay + threadtab (curr).queueKey;

            exit when threadtab (curr).next = NO_THREAD;
            curr := threadtab (curr).next;
        end loop;

        -- Append at the tail (after curr)
        threadtab (pid).queueKey := delayFromNow - accumDelay;
        threadtab (pid).prev     := curr;
        threadtab (curr).next    := pid;
        q.tail                := pid;

        Spinlocks.exitCriticalSection (q.lock);
        result := pid;
    end insertDelta;

    ---------------------------------------------------------------------------
    -- wakeup -- caller holds Process.lock. Do not nest ready-list locks
    -- underneath the sleep-list lock; move each process between queues.
    ---------------------------------------------------------------------------
    procedure wakeup
    is
        wakePid : ThreadID;
    begin
        loop
            Spinlocks.enterCriticalSection (sleepList.lock);
            if not Queues.isEmpty (sleepList) and then
               threadtab (sleepList.head).queueKey <= 0
            then
                dequeueNoLock (sleepList, wakePid);
            else
                wakePid := NO_THREAD;
            end if;
            Spinlocks.exitCriticalSection (sleepList.lock);
            exit when wakePid = NO_THREAD;
            ready (wakePid);
        end loop;
    end wakeup;

    ---------------------------------------------------------------------------
    -- insertDeltaNoLock
    -- Same as insertDelta but caller must already hold q.lock.
    ---------------------------------------------------------------------------
    procedure insertDeltaNoLock (q            : in out ProcQueue;
                                 pid          : ThreadID;
                                 delayFromNow : Integer;
                                 result       : out ThreadID)

    is
        accumDelay : Integer := 0;
        prev, curr : ThreadID;
    begin
        threadtab (pid).next := NO_THREAD;
        threadtab (pid).prev := NO_THREAD;

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            threadtab (pid).queueKey := delayFromNow;
            result := pid;
            return;
        end if;

        curr := q.head;

        loop
            if delayFromNow < accumDelay + threadtab (curr).queueKey then
                threadtab (pid).queueKey := delayFromNow - accumDelay;
                threadtab (curr).queueKey :=
                    threadtab (curr).queueKey - threadtab (pid).queueKey;

                prev := threadtab (curr).prev;
                threadtab (pid).next := curr;
                threadtab (pid).prev := prev;
                threadtab (curr).prev := pid;

                if prev /= NO_THREAD then
                    threadtab (prev).next := pid;
                else
                    q.head := pid;
                end if;

                result := pid;
                return;
            end if;

            accumDelay := accumDelay + threadtab (curr).queueKey;

            exit when threadtab (curr).next = NO_THREAD;
            curr := threadtab (curr).next;
        end loop;

        threadtab (pid).queueKey := delayFromNow - accumDelay;
        threadtab (pid).prev     := curr;
        threadtab (curr).next    := pid;
        q.tail                := pid;

        result := pid;
    end insertDeltaNoLock;

    ---------------------------------------------------------------------------
    -- isInSleepQueue
    -- Check whether the process is actually linked in the sleep delta queue.
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
    -- Remove a specific process from the sleep delta queue and ready it.
    -- Adjusts the successor's delta to preserve remaining timings.
    -- Verifies the process is actually in the queue to avoid corruption
    -- from a race with Process.sleep.
    ---------------------------------------------------------------------------
    procedure wakeFromSleep (pid : ThreadID; woken : out Boolean)

    is
        ignore  : ThreadID;
        nextPID : ThreadID;
    begin
        woken := False;
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        if threadtab (pid).state = SLEEPING and then
           isInSleepQueue (pid)
        then
            nextPID := threadtab (pid).next;
            if nextPID /= NO_THREAD then
                threadtab (nextPID).queueKey :=
                    threadtab (nextPID).queueKey + threadtab (pid).queueKey;
            end if;
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
    -- clockTick
    ---------------------------------------------------------------------------
    procedure clockTick (elapsed : Interfaces.Unsigned_64 := 1)
    is
        use type Interfaces.Unsigned_64;
        remaining : Interfaces.Unsigned_64 := elapsed;
        cursor : ThreadID;
    begin
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        -- Work is bounded by queued sleepers, not missed timer ticks. Preserve
        -- the first future delta while marking every passed deadline due.
        cursor := sleepList.head;
        while cursor /= NO_THREAD and then remaining > 0 loop
            if threadtab (cursor).queueKey > 0 then
                if Interfaces.Unsigned_64 (threadtab (cursor).queueKey) <= remaining then
                    remaining := remaining - Interfaces.Unsigned_64 (threadtab (cursor).queueKey);
                    threadtab (cursor).queueKey := 0;
                else
                    threadtab (cursor).queueKey := threadtab (cursor).queueKey - Integer (remaining);
                    remaining := 0;
                end if;
            end if;
            cursor := threadtab (cursor).next;
        end loop;

        Spinlocks.exitCriticalSection (sleepList.lock);
        wakeup;
        Spinlocks.exitCriticalSection (lock);
    end clockTick;

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
