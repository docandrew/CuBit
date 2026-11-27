-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary The deadlines of one CPU's waits (IPC revision 2, S1): a binary
-- min-heap of threads keyed by their deadline, with each thread's place.
--
-- @description
-- A thread that waits with a deadline (Wait on a notification or a private
-- futex word) joins the heap of the CPU it waits on; the CPU's timer looks
-- at the earliest only (Earliest), so a tick with nothing due costs one
-- load, whatever the number of waiters, and the timer is armed for exactly
-- that deadline. Insert, Remove and Pop are O(log n) swaps (at most 10 for
-- the 1023 threads the kernel has), never a scan: this replaces scanning
-- every thread each millisecond.
--
-- The kernel serializes each heap with that CPU's lock; this unit has no
-- concurrency of its own. Proved (tests/wait-deadlines, level 2): every
-- operation keeps the heap ordered and the places exact, Earliest is the
-- least deadline, and an operation changes no other thread's membership or
-- deadline.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Wait_Deadlines with SPARK_Mode => On is

    -- A kernel thread (Process.ThreadID), 0 for none.
    type Thread_Ref is range 0 .. 1023;
    No_Thread : constant Thread_Ref := 0;

    Capacity : constant := 1023;
    subtype Count_Type is Natural range 0 .. Capacity;
    subtype Index is Positive range 1 .. Capacity;

    type Key_Array is array (Index) of Unsigned_64;
    type Item_Array is array (Index) of Thread_Ref;
    type Place_Array is array (Thread_Ref) of Count_Type;  -- 0: not in the heap

    type Heap is record
        Size  : Count_Type := 0;
        Keys  : Key_Array := [others => 0];
        Items : Item_Array := [others => No_Thread];
        Place : Place_Array := [others => 0];
    end record;

    function Parent (I : Index) return Index is (if I = 1 then 1 else I / 2);

    function Ordered (H : Heap) return Boolean is
      (for all I in 2 .. H.Size => H.Keys (I / 2) <= H.Keys (I));

    function Placed (H : Heap) return Boolean is
      (H.Place (No_Thread) = 0
       and then (for all I in 1 .. H.Size =>
                   H.Items (I) /= No_Thread and then H.Place (H.Items (I)) = I)
       and then (for all T in Thread_Ref =>
                   H.Place (T) <= H.Size
                   and then (if H.Place (T) /= 0 then H.Items (H.Place (T)) = T)));

    function Valid (H : Heap) return Boolean is (Placed (H) and then Ordered (H));

    function Contains (H : Heap; T : Thread_Ref) return Boolean is (H.Place (T) /= 0);

    function Key_Of (H : Heap; T : Thread_Ref) return Unsigned_64 is
      (H.Keys (H.Place (T)))
    with Pre => Placed (H) and then Contains (H, T);

    -- Every thread but T keeps its membership and its deadline.
    function Others_Kept (A, B : Heap; T : Thread_Ref) return Boolean is
      (for all U in Thread_Ref =>
         (if U /= T then
            Contains (A, U) = Contains (B, U)
            and then (if Contains (B, U) then A.Keys (A.Place (U)) = B.Keys (B.Place (U)))))
    with Pre => Placed (A) and then Placed (B);

    procedure Insert (H : in out Heap; T : Thread_Ref; Key : Unsigned_64)
    with Pre  => Valid (H) and then T /= No_Thread and then not Contains (H, T)
                 and then H.Size < Capacity,
         Post => Valid (H) and then Contains (H, T) and then Key_Of (H, T) = Key
                 and then H.Size = H'Old.Size + 1 and then Others_Kept (H, H'Old, T);

    procedure Remove (H : in out Heap; T : Thread_Ref)
    with Pre  => Valid (H) and then T /= No_Thread and then Contains (H, T),
         Post => Valid (H) and then not Contains (H, T)
                 and then H.Size = H'Old.Size - 1 and then Others_Kept (H, H'Old, T);

    -- The thread with the least deadline (No_Thread when empty).
    function Earliest (H : Heap) return Thread_Ref
    with Pre  => Valid (H),
         Post => (if H.Size = 0 then Earliest'Result = No_Thread
                  else Contains (H, Earliest'Result)
                       and then (for all U in Thread_Ref =>
                                   (if Contains (H, U) then
                                      Key_Of (H, Earliest'Result) <= Key_Of (H, U))));

    function Earliest_Key (H : Heap) return Unsigned_64 is
      (if H.Size = 0 then Unsigned_64'Last else H.Keys (1))
    with Pre => Valid (H);

    -- Take the earliest thread if its deadline is not after Now.
    procedure Pop_Due (H : in out Heap; Now : Unsigned_64; T : out Thread_Ref)
    with Pre  => Valid (H),
         Post => Valid (H)
                 and then (if T = No_Thread then
                             H = H'Old
                             and then (for all U in Thread_Ref =>
                                         (if Contains (H, U) then Key_Of (H, U) > Now))
                           else Contains (H'Old, T) and then Key_Of (H'Old, T) <= Now
                                and then not Contains (H, T)
                                and then Others_Kept (H, H'Old, T));

end Wait_Deadlines;
