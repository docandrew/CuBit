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
    function popFront (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On;

    -- ---------------------------------------------------------------------------
    -- -- popBack
    -- ---------------------------------------------------------------------------
    function popBack (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On;

    -- ---------------------------------------------------------------------------
    -- -- popItem
    -- ---------------------------------------------------------------------------
    function popItem (q : in out ProcQueue; pid : ProcessID) return ProcessID
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- enqueue
    ---------------------------------------------------------------------------
    function enqueue (q : in out ProcQueue; pid : ProcessID) return ProcessID
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- dequeue
    ---------------------------------------------------------------------------
    function dequeue (q : in out ProcQueue) return ProcessID
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insert
    -- inserts into a given queue in descending key order
    ---------------------------------------------------------------------------
    function insert (q   : in out ProcQueue;
                     pid : ProcessID;
                     key : Integer) return ProcessID
        with SPARK_Mode => On;

    ---------------------------------------------------------------------------
    -- insertDelta
    -- inserts into a given queue in descending key order, using delta queue
    -- math to ensure delay is delta from previous node.
    ---------------------------------------------------------------------------
    function insertDelta (q            : in out ProcQueue;
                          pid          : ProcessID;
                          delayFromNow : Integer) return ProcessID
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
