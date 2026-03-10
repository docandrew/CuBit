-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- CuBitOS Process Queues
-------------------------------------------------------------------------------
with Spinlocks;
with TextIO; use TextIO;

package body Process.Queues with
    SPARK_Mode => On
is

    procedure initQueue (q : in out ProcQueue; locknamePtr : access String)
        with SPARK_Mode => On
    is
    begin
        q.lock := (name => locknamePtr, others => <>);
        q.head := NO_PROCESS;
        q.tail := NO_PROCESS;
    end initQueue;

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean
        with SPARK_Mode => On
    is
    begin
        return (q.head = NO_PROCESS);
    end isEmpty;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On
    is
    begin
        if isEmpty (q) then
            result := NO_PROCESS;
            return;
        end if;

        popItem (q, q.head, result);
    end popFront;

    ---------------------------------------------------------------------------
    -- popBack
    ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On
    is
    begin
        if isEmpty(q) then
            result := NO_PROCESS;
            return;
        end if;

        popItem (q, q.tail, result);
    end popBack;

    ---------------------------------------------------------------------------
    -- popItemNoLock
    -- Remove an item from the queue _without_ holding the lock. Internal
    -- functions in Process.Queue that already hold the lock should use this.
    --
    -- Public clients of the Process.Queues package use popItem which will hold
    -- the lock.
    ---------------------------------------------------------------------------
    procedure popItemNoLock (q : in out ProcQueue; pid : ProcessID;
        result : out ProcessID)
        with SPARK_Mode => On
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
        with SPARK_Mode => On
    is
    begin
        Spinlocks.enterCriticalSection (q.lock);

        popItemNoLock (q, pid, result);

        Spinlocks.exitCriticalSection (q.lock);
    end popItem;

    ---------------------------------------------------------------------------
    -- enqueue - add to the back of the list while holding the list's lock
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ProcessID;
        result : out ProcessID)
        with SPARK_Mode => On
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
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        result := pid;
    end enqueue;

    ---------------------------------------------------------------------------
    -- dequeueNoLock
    ---------------------------------------------------------------------------
    procedure dequeueNoLock (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On
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
        with SPARK_Mode => On
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
    -- insert in descending key order
    ---------------------------------------------------------------------------
    procedure insert (q      : in out ProcQueue;
                      pid    : ProcessID;
                      key    : Integer;
                      result : out ProcessID)
        with SPARK_Mode => On
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
            exit when key >= proctab(curr).queueKey or proctab(curr).next = NO_PROCESS;
            curr := proctab(curr).next;
        end loop;

        if key >= proctab(curr).queueKey then
            -- Insert BEFORE curr (new node has higher or equal priority)
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
        with SPARK_Mode => On
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
    -- wakeup
    ---------------------------------------------------------------------------
    procedure wakeup with SPARK_Mode => On
    is
        wakePid : ProcessID;
    begin
        while not Queues.isEmpty (sleepList) and
            proctab(sleepList.head).queueKey <= 0 loop
            -- print ("Waking PID"); println (Integer(sleepList.head));
            dequeueNoLock (sleepList, wakePid);
            ready (wakePid);
        end loop;
    end wakeup;

    ---------------------------------------------------------------------------
    -- clockTick
    ---------------------------------------------------------------------------
    procedure clockTick with SPARK_Mode => On
    is
    begin
        Spinlocks.enterCriticalSection (sleepList.lock);

        if not isEmpty (sleepList) then
            -- decrement head of sleep list by 1 ms
            proctab(sleepList.head).queueKey := proctab(sleepList.head).queueKey - 1;

            if proctab(sleepList.head).queueKey <= 0 then
                -- wakeup all processes with this delay
                wakeup;
            end if;
        end if;

        Spinlocks.exitCriticalSection (sleepList.lock);
    end clockTick;

    ---------------------------------------------------------------------------
    -- print
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue) with SPARK_Mode => On
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
