-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary A process's private futex table (IPC revision 2, S1): words in
-- the process's own memory, waited on only by its own threads.
--
-- @description
-- One entry per thread slot of the process (Thread_Slots: at most 128 live
-- threads, each owning one slot), since a thread waits on one word at a
-- time. An entry is the word's user address, the thread and a ticket; the
-- table keeps the fields in separate arrays, so a lookup reads only the
-- used bits and the words (compact entries, read and written field by
-- field in place, never as whole records). Tickets order the waiters of a
-- word: a wake takes the oldest first.
--
-- Bounds, whatever the process does: an enqueue and a removal are O(1); a
-- lookup visits the 128 entries once (no hashing, no secret: the table is
-- the process's own, so only its own threads can crowd it); a batch takes
-- at most Chunk waiters, so the kernel's hold of the table's lock (with
-- interrupts off) is at most Chunk lookups, and a larger wake releases
-- the lock between batches. The kernel checks the word and enqueues under
-- that same lock, and wakes under it, which is the discipline of the
-- proved protocol in tests/futex-queues (Futex_Protocol: no lost wake when
-- every changing store is followed by a wake). tests/private-futexes
-- proves this unit (level 2) and the batch bound.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Private_Futexes with SPARK_Mode => On is

    -- A thread slot of the process (Readiness_Publication.Thread_Slot).
    type Slot is range 0 .. 127;

    -- A kernel thread (Process.ThreadID), 0 for none.
    type Thread_Ref is range 0 .. 1023;
    No_Thread : constant Thread_Ref := 0;

    type Used_Set is array (Slot) of Boolean with Pack;
    type Word_Array is array (Slot) of Unsigned_64;
    type Ticket_Array is array (Slot) of Unsigned_64;
    type Thread_Array is array (Slot) of Thread_Ref;

    type Table is record
        Used        : Used_Set := [others => False];
        Word        : Word_Array := [others => 0];
        Ticket      : Ticket_Array := [others => 0];
        Thread      : Thread_Array := [others => No_Thread];
        Next_Ticket : Unsigned_64 := 0;
    end record;

    -- Waiters a batch takes under one hold of the table's lock.
    Chunk : constant := 16;
    subtype Batch_Count is Natural range 0 .. Chunk;
    subtype Batch_Index is Positive range 1 .. Chunk;
    type Batch is array (Batch_Index) of Thread_Ref;

    ---------------------------------------------------------------------------
    -- Invariant
    ---------------------------------------------------------------------------
    function Valid (T : Table) return Boolean is
      ((for all S in Slot =>
          (if T.Used (S) then T.Thread (S) /= No_Thread and then T.Ticket (S) < T.Next_Ticket))
       and then
       (for all S in Slot =>
          (for all U in Slot =>
             (if S /= U and then T.Used (S) and then T.Used (U) then
                T.Ticket (S) /= T.Ticket (U)))));

    function Has_Word (T : Table; W : Unsigned_64) return Boolean is
      (for some S in Slot => T.Used (S) and then T.Word (S) = W);

    -- S is W's oldest waiter.
    function Oldest (T : Table; W : Unsigned_64; S : Slot) return Boolean is
      (T.Used (S) and then T.Word (S) = W
       and then (for all U in Slot =>
                   (if T.Used (U) and then T.Word (U) = W then T.Ticket (S) <= T.Ticket (U))));

    -- Nothing but slot S changed.
    function Others_Same (A, B : Table; S : Slot) return Boolean is
      (for all U in Slot =>
         (if U /= S then
            A.Used (U) = B.Used (U) and then A.Word (U) = B.Word (U)
            and then A.Ticket (U) = B.Ticket (U) and then A.Thread (U) = B.Thread (U)));

    -- B's used entries are A's, minus some (a batch removed them).
    function Subset (B, A : Table) return Boolean is
      (for all U in Slot =>
         (if B.Used (U) then
            A.Used (U) and then B.Word (U) = A.Word (U)
            and then B.Ticket (U) = A.Ticket (U) and then B.Thread (U) = A.Thread (U)));

    ---------------------------------------------------------------------------
    -- Operations (the table's lock held)
    ---------------------------------------------------------------------------

    -- Thread, in its own slot S, waits on W; the newest waiter.
    procedure Enqueue (T : in out Table; S : Slot; W : Unsigned_64; Thread : Thread_Ref)
    with Pre  => Valid (T) and then not T.Used (S) and then Thread /= No_Thread
                 and then T.Next_Ticket < Unsigned_64'Last,
         Post => Valid (T) and then T.Used (S) and then T.Word (S) = W
                 and then T.Thread (S) = Thread and then Others_Same (T, T'Old, S)
                 and then T.Next_Ticket = T'Old.Next_Ticket + 1
                 and then (for all U in Slot =>
                             (if U /= S and then T.Used (U) then T.Ticket (U) < T.Ticket (S)));

    -- Thread leaves slot S (deadline, kill), if it still waits there.
    procedure Remove (T : in out Table; S : Slot; Thread : Thread_Ref; Removed : out Boolean)
    with Pre  => Valid (T) and then Thread /= No_Thread,
         Post => Valid (T) and then Removed = (T'Old.Used (S) and then T'Old.Thread (S) = Thread)
                 and then T.Next_Ticket = T'Old.Next_Ticket
                 and then (if Removed then not T.Used (S) and then Others_Same (T, T'Old, S)
                           else T = T'Old);

    -- W's oldest waiter, or Found False when none waits on W.
    procedure Find_Oldest (T : Table; W : Unsigned_64; Found : out Boolean; S : out Slot)
    with Pre  => Valid (T),
         Post => (if Found then Oldest (T, W, S) else not Has_Word (T, W));

    -- Take up to Max of W's waiters, oldest first (All_Of_Them: every
    -- waiter of W is being woken, so any order). Count of them are in
    -- Taken; fewer than Max only when no waiter of W is left.
    procedure Take_Batch (T : in out Table; W : Unsigned_64; Max : Batch_Count;
                          All_Of_Them : Boolean; Taken : out Batch; Count : out Batch_Count)
    with Pre  => Valid (T),
         Post => Valid (T) and then Count <= Max and then Subset (T, T'Old)
                 and then T.Next_Ticket = T'Old.Next_Ticket
                 and then (if Count < Max then not Has_Word (T, W))
                 -- Only W's waiters left, and each one taken was W's.
                 and then (for all U in Slot =>
                             (if T'Old.Used (U) and then not T.Used (U) then T'Old.Word (U) = W))
                 -- Each entry taken is reported, and each thread reported
                 -- was a waiter of an entry taken: the kernel wakes exactly
                 -- these.
                 and then (for all U in Slot =>
                             (if T'Old.Used (U) and then not T.Used (U) then
                                (for some I in 1 .. Count => Taken (I) = T'Old.Thread (U))))
                 and then (for all I in 1 .. Count =>
                             Taken (I) /= No_Thread
                             and then (for some U in Slot =>
                                         T'Old.Used (U) and then not T.Used (U)
                                         and then T'Old.Thread (U) = Taken (I)))
                 -- Oldest first: no waiter of W left is older than one taken.
                 and then (if not All_Of_Them then
                             (for all U in Slot =>
                                (for all V in Slot =>
                                   (if T.Used (U) and then T.Word (U) = W
                                       and then T'Old.Used (V) and then not T.Used (V)
                                    then T'Old.Ticket (V) < T.Ticket (U)))));

end Private_Futexes;
