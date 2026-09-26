-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Futex wait queues (docs/threads.md)
--
-- Pure SPARK data structure for the kernel's futex buckets. A futex key is a
-- (process, user address) pair: futexes are private to one address space.
-- Each bucket holds a bounded set of waiters. Every waiter carries a ticket
-- from its bucket's counter, so waking the lowest ticket with a key wakes
-- that key's oldest waiter (FIFO per key).
--
-- Generic in its capacity: the kernel uses small hashed buckets and one
-- overflow set large enough for every thread, so a wait is never refused
-- for lack of space. Tickets come from the caller (a counter the kernel
-- draws from under the structure's lock), so tickets are comparable across
-- a bucket and the overflow set.
--
-- The kernel serializes each bucket with its own spinlock; this package
-- has no concurrency of its own. Proved: every operation preserves
-- Well_Formed (distinct waiters, distinct tickets below Next_Ticket), wakes
-- only waiters of the requested key and the oldest of them first, removes
-- exactly the named waiter, and never disturbs any other slot.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Futex_Keys; use Futex_Keys;

generic
    Capacity : Positive;
package Futex_Queues with SPARK_Mode => On is

    subtype Slot_Index is Natural range 0 .. Capacity - 1;

    type Slot is record
        Used   : Boolean := False;
        Waiter : Waiter_Id := No_Waiter;
        K      : Key;
        Ticket : Unsigned_64 := 0;
    end record;

    type Slots is array (Slot_Index) of Slot;

    type Bucket is record
        S           : Slots;
        Next_Ticket : Unsigned_64 := 0;
    end record;

    Empty_Bucket : constant Bucket :=
      (S => (others => (Used => False, Waiter => No_Waiter,
                        K => (Owner => 0, Address => 0), Ticket => 0)),
       Next_Ticket => 0);

    ---------------------------------------------------------------------------
    -- Invariant
    ---------------------------------------------------------------------------
    function Well_Formed (B : Bucket) return Boolean is
      ((for all I in Slot_Index =>
          (if B.S (I).Used then
             B.S (I).Waiter /= No_Waiter and then
             B.S (I).Ticket < B.Next_Ticket))
       and then
       (for all I in Slot_Index =>
          (for all J in Slot_Index =>
             (if I /= J and then B.S (I).Used and then B.S (J).Used then
                B.S (I).Waiter /= B.S (J).Waiter and then
                B.S (I).Ticket /= B.S (J).Ticket))));

    function Contains (B : Bucket; W : Waiter_Id) return Boolean is
      (for some I in Slot_Index => B.S (I).Used and then B.S (I).Waiter = W);

    function Has_Key (B : Bucket; K : Key) return Boolean is
      (for some I in Slot_Index => B.S (I).Used and then B.S (I).K = K);

    function Full (B : Bucket) return Boolean is
      (for all I in Slot_Index => B.S (I).Used);

    -- Every slot other than I is unchanged.
    function Others_Unchanged (Updated, Original : Bucket; I : Slot_Index)
      return Boolean is
      (for all J in Slot_Index => (if J /= I then Updated.S (J) = Original.S (J)))
    with Ghost;

    -- Tickets only increase.
    function Tickets_Monotonic (Updated, Original : Bucket) return Boolean is
      (Updated.Next_Ticket >= Original.Next_Ticket)
    with Ghost;

    ---------------------------------------------------------------------------
    -- Operations
    ---------------------------------------------------------------------------

    -- Add W, waiting on K, with Ticket (newer than every ticket the bucket
    -- has issued). Fails, changing nothing, only if the bucket is full.
    procedure Enqueue (B    : in out Bucket;
                       W    : Waiter_Id;
                       K    : Key;
                       Ticket : Unsigned_64;
                       At_Slot : out Slot_Index;
                       Ok   : out Boolean)
    with
        Pre  => Well_Formed (B) and then W /= No_Waiter and then
                not Contains (B, W) and then Ticket >= B.Next_Ticket and then
                Ticket < Unsigned_64'Last,
        Post => Well_Formed (B) and then
                B.Next_Ticket = (if Ok then Ticket + 1
                                 else B'Old.Next_Ticket) and then
                (if Ok then B.S (At_Slot).Ticket = Ticket) and then
                (if Ok then
                   B.S (At_Slot).Used and then B.S (At_Slot).Waiter = W and then
                   B.S (At_Slot).K = K and then
                   not B'Old.S (At_Slot).Used and then
                   (for all I in Slot_Index =>
                      (if I /= At_Slot and then B.S (I).Used then
                         B.S (I).Ticket < B.S (At_Slot).Ticket)) and then
                   Others_Unchanged (B, B'Old, At_Slot)
                 else
                   B = B'Old and then Full (B'Old));

    -- Remove the waiter in At_Slot if it is W (timeout, kill). Removed is
    -- False, changing nothing, if that slot no longer holds W.
    procedure Remove_At (B       : in out Bucket;
                         At_Slot : Slot_Index;
                         W       : Waiter_Id;
                         Removed : out Boolean)
    with
        Pre  => Well_Formed (B) and then W /= No_Waiter,
        Post => Well_Formed (B) and then Tickets_Monotonic (B, B'Old) and then
                Removed = (B'Old.S (At_Slot).Used and then
                           B'Old.S (At_Slot).Waiter = W) and then
                (if Removed then
                   not B.S (At_Slot).Used and then not Contains (B, W) and then
                   Others_Unchanged (B, B'Old, At_Slot)
                 else B = B'Old);

    -- The oldest waiter on K: the used slot with key K and least ticket.
    function Oldest (B : Bucket; K : Key; I : Slot_Index) return Boolean is
      (B.S (I).Used and then B.S (I).K = K and then
       (for all J in Slot_Index =>
          (if B.S (J).Used and then B.S (J).K = K then
             B.S (I).Ticket <= B.S (J).Ticket)))
    with Ghost;

    -- K's oldest waiter, without removing it (to choose between a bucket
    -- and the overflow set by ticket).
    procedure Find_Oldest (B : Bucket; K : Key; Found : out Boolean;
                           At_Slot : out Slot_Index)
    with
        Pre  => Well_Formed (B),
        Post => (if Found then Oldest (B, K, At_Slot)
                 else not Has_Key (B, K));

    -- Remove and return K's oldest waiter, or No_Waiter if none waits on K.
    procedure Wake_One (B : in out Bucket; K : Key; W : out Waiter_Id;
                        From_Slot : out Slot_Index)
    with
        Pre  => Well_Formed (B),
        Post => Well_Formed (B) and then Tickets_Monotonic (B, B'Old) and then
                (if W = No_Waiter then
                   B = B'Old and then not Has_Key (B'Old, K)
                 else
                   Oldest (B'Old, K, From_Slot) and then
                   B'Old.S (From_Slot).Waiter = W and then
                   not B.S (From_Slot).Used and then
                   not Contains (B, W) and then
                   Others_Unchanged (B, B'Old, From_Slot));

    ---------------------------------------------------------------------------
    -- Proved lemmas
    ---------------------------------------------------------------------------

    -- Two waiters enqueued one after the other on the same key are woken in
    -- that order (FIFO), whatever else the bucket holds.
    procedure Prove_FIFO (B : Bucket; K : Key; W1, W2 : Waiter_Id)
    with
        Ghost,
        Global => null,
        Pre  => Well_Formed (B) and then W1 /= No_Waiter and then
                W2 /= No_Waiter and then W1 /= W2 and then
                not Contains (B, W1) and then not Contains (B, W2) and then
                B.Next_Ticket < Unsigned_64'Last - 1 and then
                not Has_Key (B, K);

    -- Waking a key never wakes a waiter on another key.
    procedure Prove_Key_Isolation (B : Bucket; K1, K2 : Key; W : Waiter_Id)
    with
        Ghost,
        Global => null,
        Pre  => Well_Formed (B) and then K1 /= K2 and then W /= No_Waiter and then
                not Contains (B, W) and then B.Next_Ticket < Unsigned_64'Last;

end Futex_Queues;
