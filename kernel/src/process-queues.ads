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

package Process.Queues is

    procedure initQueue (q : in out ProcQueue; locknamePtr : Spinlocks.Lock_Name);

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean;

    -- Atomic readiness test for the priority-ordered run queue. No dequeue,
    -- preference boost or borrowed authority; the scheduler still selects.
    function hasReadyPeer (q : in out ProcQueue; priority : Integer) return Boolean;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ProcessID);

    -- ---------------------------------------------------------------------------
    -- -- popBack
    -- ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ProcessID);

    -- ---------------------------------------------------------------------------
    -- -- popItem
    -- ---------------------------------------------------------------------------
    procedure popItem (q : in out ProcQueue; pid : ProcessID;
                       result : out ProcessID);
    type Removal_Kind is (Ordinary_Queue, Delta_Queue);
    -- Caller holds Process.lock; selecting/removing membership is one
    -- queue-locked operation. Delta removal preserves successors' deadlines.
    procedure detach (q : in out ProcQueue; pid : ProcessID;
                      kind : Removal_Kind := Ordinary_Queue);

    ---------------------------------------------------------------------------
    -- enqueue
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ProcessID;
                       result : out ProcessID);

    ---------------------------------------------------------------------------
    -- dequeue
    ---------------------------------------------------------------------------
    procedure dequeue (q : in out ProcQueue; result : out ProcessID);

    ---------------------------------------------------------------------------
    -- insert
    -- Inserts in descending key order; equal keys retain FIFO arrival order.
    ---------------------------------------------------------------------------
    procedure insert (q      : in out ProcQueue;
                      pid    : ProcessID;
                      key    : Integer;
                      result : out ProcessID);

    ---------------------------------------------------------------------------
    -- insertDelta
    -- inserts into a given queue in descending key order, using delta queue
    -- math to ensure delay is delta from previous node.
    ---------------------------------------------------------------------------
    procedure insertDelta (q            : in out ProcQueue;
                           pid          : ProcessID;
                           delayFromNow : Integer;
                           result       : out ProcessID);

    ---------------------------------------------------------------------------
    -- insertDeltaNoLock
    -- Same as insertDelta but caller must already hold q.lock.
    -- Used by Process.sleep to atomically set state + insert.
    ---------------------------------------------------------------------------
    procedure insertDeltaNoLock (q            : in out ProcQueue;
                                 pid          : ProcessID;
                                 delayFromNow : Integer;
                                 result       : out ProcessID);

    ---------------------------------------------------------------------------
    -- wakeFromSleep
    -- Remove a specific process from the sleep delta queue and ready it.
    -- Adjusts the successor's delta to preserve remaining timings.
    -- Acquires Process.lock before sleepList.lock; caller must not hold either.
    ---------------------------------------------------------------------------
    procedure wakeFromSleep (pid : ProcessID; woken : out Boolean);

    ---------------------------------------------------------------------------
    -- clockTick
    -- Adjust the delta queue entries by the elapsed tick, wake up any sleeping
    -- processes whose delay has elapsed.
    -- Acquires Process.lock; timer caller must not already hold it.
    ---------------------------------------------------------------------------
    procedure clockTick;

    ---------------------------------------------------------------------------
    -- print
    -- Dump the list contents to TextIO
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue);

end Process.Queues;
