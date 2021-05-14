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
    function popFront (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On
    is
    begin
        if isEmpty (q) then
            return NO_PROCESS;
        end if;

        return popItem (q, q.head);
    end popFront;

    ---------------------------------------------------------------------------
    -- popBack
    ---------------------------------------------------------------------------
    function popBack (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On
    is
    begin
        if isEmpty(q) then
            return NO_PROCESS;
        end if;

        return popItem (q, q.tail);
    end popBack;

    ---------------------------------------------------------------------------
    -- popItemNoLock
    -- Remove an item from the queue _without_ holding the lock. Internal
    -- functions in Process.Queue that already hold the lock should use this.
    --
    -- Public clients of the Process.Queues package use popItem which will hold
    -- the lock.
    ---------------------------------------------------------------------------
    function popItemNoLock (q : in out ProcQueue; pid : ProcessID) return ProcessID
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

        return pid;
    end popItemNoLock;

    ---------------------------------------------------------------------------
    -- popItem
    ---------------------------------------------------------------------------
    function popItem (q : in out ProcQueue; pid : ProcessID) return ProcessID
        with SPARK_Mode => On
    is
        ret : ProcessID;
    begin
        Spinlocks.enterCriticalSection (q.lock);
        
        ret := popItemNoLock (q, pid);
        
        Spinlocks.exitCriticalSection (q.lock);
        
        return ret;
    end popItem;

    ---------------------------------------------------------------------------
    -- enqueue - add to the back of the list while holding the list's lock
    ---------------------------------------------------------------------------
    function enqueue (q : in out ProcQueue; pid : ProcessID) return ProcessID
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
        
        return pid;
    end enqueue;

    ---------------------------------------------------------------------------
    -- dequeueNoLock
    ---------------------------------------------------------------------------
    function dequeueNoLock (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On
    is
        pid : ProcessID;
    begin
        
        if isEmpty (q) then
            return NO_PROCESS;
        end if;

        pid := popItemNoLock (q, q.head);

        proctab(pid).prev := NO_PROCESS;
        proctab(pid).next := NO_PROCESS;

        return pid;
    end dequeueNoLock;

    ---------------------------------------------------------------------------
    -- dequeue - remove from front of the list while holding the list's lock
    ---------------------------------------------------------------------------
    function dequeue (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On
    is
        pid : ProcessID;
    begin
        
        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            Spinlocks.exitCriticalSection (q.lock);
            return NO_PROCESS;
        end if;

        pid := popItemNoLock (q, q.head);

        proctab(pid).prev := NO_PROCESS;
        proctab(pid).next := NO_PROCESS;

        Spinlocks.exitCriticalSection (q.lock);
        
        return pid;
    end dequeue;

    ---------------------------------------------------------------------------
    -- insert in descending key order
    ---------------------------------------------------------------------------
    function insert (q   : in out ProcQueue;
                     pid : ProcessID;
                     key : Integer) return ProcessID
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
            proctab(pid).queueKey := key;

            Spinlocks.exitCriticalSection (q.lock);
            return pid;
        end if;

        curr := q.head;

        loop
            exit when key >= proctab(curr).queueKey or proctab(curr).next = NO_PROCESS;

            curr := proctab(curr).next;
        end loop;

        -- Insert between previous node and current node
        prev                  := proctab(curr).prev;
        proctab(pid).next     := curr;
        proctab(pid).prev     := prev;
        proctab(pid).queueKey := key;
        proctab(curr).prev    := pid;

        if prev /= NO_PROCESS then
            proctab(prev).next := pid;
        else
            -- adding to front of list
            q.head := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        return pid;
    end insert;

    ---------------------------------------------------------------------------
    -- insertDelta
    ---------------------------------------------------------------------------
    function insertDelta (q            : in out ProcQueue;
                          pid          : ProcessID;
                          delayFromNow : Integer) return ProcessID
        with SPARK_Mode => On
    is
        -- Need to track difference between desired delay and sum of delay of
        -- all previous processes on the sleep list.
        prevDelaySum : Integer := 0;

        prev, curr : ProcessID;
    begin
        Spinlocks.enterCriticalSection (q.lock);

        if isEmpty (q) then
            -- empty list.
            q.head := pid;
            q.tail := pid;
            proctab(pid).queueKey := delayFromNow;

            Spinlocks.exitCriticalSection (q.lock);
            return pid;
        end if;

        curr := q.head;

        -- Sum up previous delays. When our desired delay is less than the sum
        -- of the previous delays or we reach the end of the list, insert our
        -- process into the list.
        loop
            exit when delayFromNow < prevDelaySum or proctab(curr).next = NO_PROCESS;
            
            prevDelaySum := prevDelaySum + proctab(curr).queueKey;
            curr := proctab(curr).next;
        end loop;

        -- Insert between previous node and current node
        prev                  := proctab(curr).prev;
        proctab(pid).next     := curr;
        proctab(pid).prev     := prev;
        proctab(pid).queueKey := delayFromNow - prevDelaySum;
        proctab(curr).prev    := pid;

        if prev /= NO_PROCESS then
            proctab(prev).next := pid;
        else
            -- adding to front of list
            q.head := pid;
        end if;

        Spinlocks.exitCriticalSection (q.lock);

        return pid;
    end insertDelta;

    ---------------------------------------------------------------------------
    -- wakeup
    ---------------------------------------------------------------------------
    procedure wakeup with SPARK_Mode => On
    is
    begin
        while not Queues.isEmpty (sleepList) and
            proctab(sleepList.head).queueKey <= 0 loop
            -- print ("Waking PID"); println (Integer(sleepList.head));
            ready (Queues.dequeueNoLock (sleepList));
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
            -- print (sleepList);
            -- decrement head of sleep list by 1 ms
            proctab(sleepList.head).queueKey := proctab(sleepList.head).queueKey - 1;

            if proctab(sleepList.head).queueKey <= 0 then
                -- wakeup all processes with this delay
                wakeup;
            end if;
        else
            -- print (".");
            null;
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
