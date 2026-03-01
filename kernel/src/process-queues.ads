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

package Process.Queues with
    SPARK_Mode => On
is

    procedure initQueue (q : in out ProcQueue; locknamePtr : access String)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- isEmpty
    ---------------------------------------------------------------------------
    function isEmpty (q : ProcQueue) return Boolean
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- popFront
    ---------------------------------------------------------------------------
    procedure popFront (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On;

    -- ---------------------------------------------------------------------------
    -- -- popBack
    -- ---------------------------------------------------------------------------
    procedure popBack (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On;

    -- ---------------------------------------------------------------------------
    -- -- popItem
    -- ---------------------------------------------------------------------------
    procedure popItem (q : in out ProcQueue; pid : ProcessID;
                       result : out ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- enqueue
    ---------------------------------------------------------------------------
    procedure enqueue (q : in out ProcQueue; pid : ProcessID;
                       result : out ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- dequeue
    ---------------------------------------------------------------------------
    procedure dequeue (q : in out ProcQueue; result : out ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insert
    -- inserts into a given queue in descending key order
    ---------------------------------------------------------------------------
    procedure insert (q      : in out ProcQueue;
                      pid    : ProcessID;
                      key    : Integer;
                      result : out ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insertDelta
    -- inserts into a given queue in descending key order, using delta queue
    -- math to ensure delay is delta from previous node.
    ---------------------------------------------------------------------------
    procedure insertDelta (q            : in out ProcQueue;
                           pid          : ProcessID;
                           delayFromNow : Integer;
                           result       : out ProcessID)
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- clockTick
    -- Adjust the delta queue entries by the elapsed tick, wake up any sleeping
    -- processes whose delay has elapsed.
    ---------------------------------------------------------------------------
    procedure clockTick with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- print
    -- Dump the list contents to TextIO
    ---------------------------------------------------------------------------
    procedure print (q : ProcQueue) with SPARK_Mode => On;

end Process.Queues;
