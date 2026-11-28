-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2021 Jon Andrew
--
-- @summary
-- CuBitOS Process Queues
--
-- @description
-- CuBit Process Queues are a linked list of processes, where the lists
-- themselves are woven through the proctab. Each process can be on at most one
-- list at a time. The list heads are separate ProcessQueue objects that point
-- to the first entry from the proctab in that list.
-------------------------------------------------------------------------------

with Interfaces;
package Process.Queues is

    procedure initQueue (q : in out ProcQueue; locknamePtr : Spinlocks.Lock_Name);

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean;

    ---------------------------------------------------------------------------
    -- Ready lists (docs/scheduler.md) are ordered by run key, earliest first,
    -- FIFO among equal keys. An empty list's head key is Idle_Key.
    ---------------------------------------------------------------------------
    function headKey (q : in out ProcQueue) return Interfaces.Unsigned_64;

    -- Another CPU may take an entry that is ordinary work (not an idle
    -- thread), unpinned, not being retired, and not still executing
    -- (switching out) on another CPU (Work_Stealing.Eligible).
    -- The key of the first such entry, or Idle_Key if there is none.
    function takeableKey (q : in out ProcQueue) return Interfaces.Unsigned_64;
    -- Remove the first such entry, or return NO_THREAD.
    procedure takeFirst (q : in out ProcQueue; result : out ThreadID);

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ThreadID);

    -- ---------------------------------------------------------------------------
    -- -- popBack
    -- ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ThreadID);

    -- ---------------------------------------------------------------------------
    -- -- popItem
    -- ---------------------------------------------------------------------------
    procedure popItem (q : in out ProcQueue; pid : ThreadID;
                       result : out ThreadID);
    -- Caller holds Process.lock; selecting/removing membership is one
    -- queue-locked operation.
    procedure detach (q : in out ProcQueue; pid : ThreadID);

    ---------------------------------------------------------------------------
    -- enqueue
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ThreadID;
                       result : out ThreadID);

    ---------------------------------------------------------------------------
    -- dequeue
    ---------------------------------------------------------------------------
    procedure dequeue (q : in out ProcQueue; result : out ThreadID);

    ---------------------------------------------------------------------------
    -- dequeuePreferring
    -- Remove the first entry whose home CPU is cpu, else the head (FIFO).
    -- Used to hand IPC work to a receiver thread on the sender's CPU, so the
    -- handoff can be direct; it changes no priority and grants nothing.
    ---------------------------------------------------------------------------
    procedure dequeuePreferring (q : in out ProcQueue; cpu : Natural;
                                 result : out ThreadID);

    ---------------------------------------------------------------------------
    -- insertByKey
    -- Inserts a ready thread in ascending key order; equal keys keep FIFO
    -- arrival order. The key is kept in the thread's runKey.
    ---------------------------------------------------------------------------
    procedure insertByKey (q      : in out ProcQueue;
                           pid    : ThreadID;
                           key    : Interfaces.Unsigned_64;
                           result : out ThreadID);
    -- The same; the caller holds q.lock (Process.sleepUntil publishes the
    -- sleeping state and the insertion together).
    procedure insertByKeyNoLock (q      : in out ProcQueue;
                                 pid    : ThreadID;
                                 key    : Interfaces.Unsigned_64;
                                 result : out ThreadID);


    ---------------------------------------------------------------------------
    -- wakeFromSleep
    -- A thread asleep in Process.sleep runs again now (woken); else nothing.
    -- Caller must not hold Process.lock.
    ---------------------------------------------------------------------------
    procedure wakeFromSleep (pid : ThreadID; woken : out Boolean);

    ---------------------------------------------------------------------------
    -- print
    -- Dump the list contents to TextIO
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue);

end Process.Queues;
