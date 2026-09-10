-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- CuBitOS Process Queues
-------------------------------------------------------------------------------
with Spinlocks;
with TextIO; use TextIO;

-- Ada implementation: custom storage, address overlays or live context state.
-- Only separately annotated SPARK policy/state routines carry proof obligations.
package body Process.Queues is

    function isInSleepQueue (pid : ProcessID) return Boolean;
    procedure popItemNoLock (q : in out ProcQueue; pid : ProcessID;
                            result : out ProcessID);

    procedure initQueue (q : in out ProcQueue; locknamePtr : Spinlocks.Lock_Name)

    is
    begin
        Spinlocks.Initialize (q.lock, locknamePtr);
        q.head := NO_PROCESS;
        q.tail := NO_PROCESS;
    end initQueue;

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean

    is
    begin
        return (q.head = NO_PROCESS);
    end isEmpty;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ProcessID)

    is
    begin
        -- Selection and removal must use the same lock acquisition.
        dequeue (q, result);
    end popFront;

    ---------------------------------------------------------------------------
    -- popBack
    ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ProcessID)

    is
    begin
        Spinlocks.enterCriticalSection (q.lock);
        if isEmpty(q) then
            result := NO_PROCESS;
        else
            popItemNoLock (q, q.tail, result);
            proctab(result).prev := NO_PROCESS;
            proctab(result).next := NO_PROCESS;
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
    procedure popItemNoLock (q : in out ProcQueue; pid : ProcessID;
        result : out ProcessID)

    is
        prev, next : ProcessID;
    begin

        next := proctab(pid).next;
        prev := proctab(pid).prev;

        -- Unlink this process from its current list
        if prev /= NO_PROCESS then
            proctab(prev).next := next;
        else
            -- first element in list
            q.head := next;
        end if;

        if next /= NO_PROCESS then
            proctab(next).prev := prev;
        else
            -- last element
            q.tail := prev;
        end if;

        result := pid;
    end popItemNoLock;

    ---------------------------------------------------------------------------
    -- popItem
    ---------------------------------------------------------------------------
    procedure popItem (q : in out ProcQueue; pid : ProcessID;
        result : out ProcessID)

    is
    begin
        Spinlocks.enterCriticalSection (q.lock);

        popItemNoLock (q, pid, result);

        Spinlocks.exitCriticalSection (q.lock);
    end popItem;

    procedure detach (q : in out ProcQueue; pid : ProcessID;
                      kind : Removal_Kind := Ordinary_Queue) is
        current, ignored, following : ProcessID;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        current := q.head;
        while current /= NO_PROCESS loop
            if current = pid then
                following := proctab(pid).next;
                if kind = Delta_Queue and then following /= NO_PROCESS then
                    proctab(following).queueKey :=
                        proctab(following).queueKey + proctab(pid).queueKey;
                end if;
                popItemNoLock (q, pid, ignored);
                proctab(pid).next := NO_PROCESS;
                proctab(pid).prev := NO_PROCESS;
                exit;
            end if;
            current := proctab(current).next;
        end loop;
        Spinlocks.exitCriticalSection (q.lock);
    end detach;

    ---------------------------------------------------------------------------
    -- enqueue - add to the back of the list while holding the list's lock
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ProcessID;
        result : out ProcessID)

    is
        prev : ProcessID;
    begin

        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            proctab(pid).prev := NO_PROCESS;
            proctab(pid).next := NO_PROCESS;
        else
            prev := q.tail;
            proctab(pid).prev := prev;
            proctab(pid).next := NO_PROCESS;
            proctab(prev).next := pid;
            q.tail := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end enqueue;

    ---------------------------------------------------------------------------
    -- dequeueNoLock
    ---------------------------------------------------------------------------
    procedure dequeueNoLock (q : in out ProcQueue; result : out ProcessID)

    is
        pid : ProcessID;
    begin

        if isEmpty (q) then
            result := NO_PROCESS;
            return;
        end if;

        popItemNoLock (q, q.head, pid);

        proctab(pid).prev := NO_PROCESS;
        proctab(pid).next := NO_PROCESS;

        result := pid;
    end dequeueNoLock;

    ---------------------------------------------------------------------------
    -- dequeue - remove from front of the list while holding the list's lock
    ---------------------------------------------------------------------------
    procedure dequeue (q : in out ProcQueue; result : out ProcessID)

    is
        pid : ProcessID;
    begin

        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            Spinlocks.exitCriticalSection (q.lock);
            result := NO_PROCESS;
            return;
        end if;

        popItemNoLock (q, q.head, pid);

        proctab(pid).prev := NO_PROCESS;
        proctab(pid).next := NO_PROCESS;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end dequeue;

    ---------------------------------------------------------------------------
    -- Insert in descending key order, FIFO among equal keys.
    ---------------------------------------------------------------------------
    procedure insert (q      : in out ProcQueue;
                      pid    : ProcessID;
                      key    : Integer;
                      result : out ProcessID)

    is
        curr : ProcessID;
        prev : ProcessID;
    begin
        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            -- empty list.
            q.head := pid;
            q.tail := pid;
            proctab(pid).prev     := NO_PROCESS;
            proctab(pid).next     := NO_PROCESS;
            proctab(pid).queueKey := key;

            Spinlocks.exitCriticalSection (q.lock);
            result := pid;
            return;
        end if;

        -- Walk the list to find the right insertion point (descending key order).
        curr := q.head;

        loop
            exit when key > proctab(curr).queueKey or proctab(curr).next = NO_PROCESS;
            curr := proctab(curr).next;
        end loop;

        if key > proctab(curr).queueKey then
            -- Insert BEFORE curr only for strictly higher priority. A task
            -- whose quantum expired must go behind already-ready peers.
            prev                  := proctab(curr).prev;
            proctab(pid).next     := curr;
            proctab(pid).prev     := prev;
            proctab(pid).queueKey := key;
            proctab(curr).prev    := pid;

            if prev /= NO_PROCESS then
                proctab(prev).next := pid;
            else
                q.head := pid;
            end if;
        else
            -- Append AFTER curr (new node has lowest priority, goes at tail)
            proctab(pid).next     := NO_PROCESS;
            proctab(pid).prev     := curr;
            proctab(pid).queueKey := key;
            proctab(curr).next    := pid;
            q.tail                := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end insert;

    ---------------------------------------------------------------------------
    -- insertDelta
    ---------------------------------------------------------------------------
    procedure insertDelta (q            : in out ProcQueue;
                           pid          : ProcessID;
                           delayFromNow : Integer;
                           result       : out ProcessID)

    is
        -- accumDelay tracks the absolute wakeup time of all entries
        -- before the current insertion point.
        accumDelay : Integer := 0;

        prev, curr : ProcessID;
    begin
        Spinlocks.enterCriticalSection (q.lock);

        -- Initialize new node's links
        proctab(pid).next := NO_PROCESS;
        proctab(pid).prev := NO_PROCESS;

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            proctab(pid).queueKey := delayFromNow;

            Spinlocks.exitCriticalSection (q.lock);
            result := pid;
            return;
        end if;

        curr := q.head;

        -- Walk the delta list.  accumDelay + curr.queueKey gives the
        -- absolute wakeup time of curr.  Insert before the first node
        -- whose absolute time exceeds our delay.
        loop
            if delayFromNow < accumDelay + proctab(curr).queueKey then
                -- Insert before curr
                proctab(pid).queueKey := delayFromNow - accumDelay;

                -- Reduce curr's delta (now relative to the new node)
                proctab(curr).queueKey :=
                    proctab(curr).queueKey - proctab(pid).queueKey;

                prev := proctab(curr).prev;
                proctab(pid).next := curr;
                proctab(pid).prev := prev;
                proctab(curr).prev := pid;

                if prev /= NO_PROCESS then
                    proctab(prev).next := pid;
                else
                    q.head := pid;
                end if;

                Spinlocks.exitCriticalSection (q.lock);
                result := pid;
                return;
            end if;

            accumDelay := accumDelay + proctab(curr).queueKey;

            exit when proctab(curr).next = NO_PROCESS;
            curr := proctab(curr).next;
        end loop;

        -- Append at the tail (after curr)
        proctab(pid).queueKey := delayFromNow - accumDelay;
        proctab(pid).prev     := curr;
        proctab(curr).next    := pid;
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
        wakePid : ProcessID;
    begin
        loop
            Spinlocks.enterCriticalSection (sleepList.lock);
            if not Queues.isEmpty (sleepList) and then
               proctab(sleepList.head).queueKey <= 0
            then
                dequeueNoLock (sleepList, wakePid);
            else
                wakePid := NO_PROCESS;
            end if;
            Spinlocks.exitCriticalSection (sleepList.lock);
            exit when wakePid = NO_PROCESS;
            ready (wakePid);
        end loop;
    end wakeup;

    ---------------------------------------------------------------------------
    -- insertDeltaNoLock
    -- Same as insertDelta but caller must already hold q.lock.
    ---------------------------------------------------------------------------
    procedure insertDeltaNoLock (q            : in out ProcQueue;
                                 pid          : ProcessID;
                                 delayFromNow : Integer;
                                 result       : out ProcessID)

    is
        accumDelay : Integer := 0;
        prev, curr : ProcessID;
    begin
        proctab(pid).next := NO_PROCESS;
        proctab(pid).prev := NO_PROCESS;

        if isEmpty (q) then
            q.head := pid;
            q.tail := pid;
            proctab(pid).queueKey := delayFromNow;
            result := pid;
            return;
        end if;

        curr := q.head;

        loop
            if delayFromNow < accumDelay + proctab(curr).queueKey then
                proctab(pid).queueKey := delayFromNow - accumDelay;
                proctab(curr).queueKey :=
                    proctab(curr).queueKey - proctab(pid).queueKey;

                prev := proctab(curr).prev;
                proctab(pid).next := curr;
                proctab(pid).prev := prev;
                proctab(curr).prev := pid;

                if prev /= NO_PROCESS then
                    proctab(prev).next := pid;
                else
                    q.head := pid;
                end if;

                result := pid;
                return;
            end if;

            accumDelay := accumDelay + proctab(curr).queueKey;

            exit when proctab(curr).next = NO_PROCESS;
            curr := proctab(curr).next;
        end loop;

        proctab(pid).queueKey := delayFromNow - accumDelay;
        proctab(pid).prev     := curr;
        proctab(curr).next    := pid;
        q.tail                := pid;

        result := pid;
    end insertDeltaNoLock;

    ---------------------------------------------------------------------------
    -- isInSleepQueue
    -- Check whether the process is actually linked in the sleep delta queue.
    -- Must be called while holding sleepList.lock.
    ---------------------------------------------------------------------------
    function isInSleepQueue (pid : ProcessID) return Boolean

    is
        curr : ProcessID := sleepList.head;
    begin
        while curr /= NO_PROCESS loop
            if curr = pid then
                return True;
            end if;
            curr := proctab(curr).next;
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
    procedure wakeFromSleep (pid : ProcessID; woken : out Boolean)

    is
        ignore  : ProcessID;
        nextPID : ProcessID;
    begin
        woken := False;
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        if proctab(pid).state = SLEEPING and then
           isInSleepQueue (pid)
        then
            nextPID := proctab(pid).next;
            if nextPID /= NO_PROCESS then
                proctab(nextPID).queueKey :=
                    proctab(nextPID).queueKey + proctab(pid).queueKey;
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
    procedure clockTick
    is
    begin
        Spinlocks.enterCriticalSection (lock);
        Spinlocks.enterCriticalSection (sleepList.lock);

        if not isEmpty (sleepList) and then
           proctab(sleepList.head).queueKey > 0
        then
            -- Zero-delay entries are already due; don't make them negative.
            proctab(sleepList.head).queueKey := proctab(sleepList.head).queueKey - 1;
        end if;

        Spinlocks.exitCriticalSection (sleepList.lock);
        wakeup;
        Spinlocks.exitCriticalSection (lock);
    end clockTick;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue)
    is
        curr : ProcessID := q.head;
    begin
        println ("Process.Queues: ");

        if isEmpty (q) then
            println (" * Empty.");
            return;
        end if;

        while curr /= NO_PROCESS loop
            println ("* " & proctab(curr).name & " key: " & proctab(curr).queueKey'Image);
            curr := proctab(curr).next;
        end loop;

    end print;

end Process.Queues;
