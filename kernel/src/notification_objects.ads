-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Notifications (IPC revision 2, S1): the state of one object and
-- the rules of every operation on it, in SPARK (tests/notifications).
--
-- @description
-- A notification is a word of pending bits and at most one waiting thread.
-- A Wake capability carries a badge fixed when it was minted; Wake ORs that
-- badge into the bits and the caller passes none. Wait takes every pending
-- bit and clears them, or blocks the one waiter. A second Wait while one
-- thread waits is refused (Busy): nothing is stolen.
--
-- Fan-in has exactly two levels, leaf -> root -> thread:
--   * a leaf is bound to one root, with the badge of a Wake capability of
--     that root; a signal of the leaf keeps its bits in the leaf and
--     forwards the root badge to the root (Forward);
--   * a root may be bound to one thread (the bound notification): a signal
--     with no waiter rings that thread (Ring_Bound) and the bits stay;
--   * a root never becomes a leaf (Is_Root is kept for the object's life),
--     a leaf never has leaves or a thread, and a signal of the second level
--     never forwards (Signal's Second). So a chain is at most two objects
--     long whatever the bindings, no cycle can form, and a Wake takes at
--     most two notification locks, one after the other.
--
-- The kernel holds each object's lock around these operations (two locks
-- in ascending identity order only for Bind_Leaf); the object's identity
-- and generation, and the owner's charge, are the kernel's
-- (Process.Notifications). The two-level rules over a whole table of
-- objects are proved in tests/notifications (Fan_In_Model).
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Notification_Objects with SPARK_Mode => On is

    -- A kernel thread (Process.ThreadID's range), 0 for none.
    type Thread_Ref is range 0 .. 1023;
    No_Thread : constant Thread_Ref := 0;

    -- An object of the kernel's table, 0 for none. A link to an object
    -- names its generation too: an ended object's links are stale.
    Max_Objects : constant := 32_768;
    type Object_Id is range 0 .. Max_Objects;
    No_Object : constant Object_Id := 0;

    type Link is record
        Id  : Object_Id := No_Object;
        Gen : Unsigned_32 := 0;
    end record;
    No_Link : constant Link := (Id => No_Object, Gen => 0);

    -- The process that pays for an object (the holder of its Wait right).
    type Owner_Ref is range 0 .. 255;
    No_Owner : constant Owner_Ref := 0;

    type Notification is record
        Live       : Boolean := False;
        Owner      : Owner_Ref := No_Owner;
        Bits       : Unsigned_64 := 0;
        -- The one thread blocked in Wait on this object.
        Waiter     : Thread_Ref := No_Thread;
        -- The thread this root is bound to (the bound notification).
        Bound      : Thread_Ref := No_Thread;
        -- A leaf's root, and the badge it signals the root with.
        Root       : Link := No_Link;
        Root_Badge : Unsigned_64 := 0;
        -- A leaf or a thread was bound to it: it can never be a leaf.
        Is_Root    : Boolean := False;
    end record;

    Empty : constant Notification :=
      (Live => False, Owner => No_Owner, Bits => 0, Waiter => No_Thread,
       Bound => No_Thread, Root => No_Link, Root_Badge => 0, Is_Root => False);

    function Is_Leaf (N : Notification) return Boolean is (N.Root.Id /= No_Object);

    -- Every object's own rules.
    function Valid (N : Notification) return Boolean is
      ((if Is_Leaf (N) then
          not N.Is_Root and then N.Waiter = No_Thread and then N.Bound = No_Thread
          and then N.Root_Badge /= 0
        else N.Root_Badge = 0)
       and then (if N.Bound /= No_Thread then N.Is_Root)
       and then (if not N.Live then N = Empty));

    ---------------------------------------------------------------------------
    -- Life
    ---------------------------------------------------------------------------
    procedure Create (N : out Notification; Owner : Owner_Ref)
    with Pre  => Owner /= No_Owner,
         Post => Valid (N) and then N.Live and then N.Owner = Owner
                 and then N.Bits = 0 and then N.Waiter = No_Thread
                 and then N.Bound = No_Thread and then not Is_Leaf (N)
                 and then not N.Is_Root;

    -- The Wait right closed (or its holder ended): the object ends. Waiter
    -- and Bound are the threads still attached, for the kernel to wake
    -- (Ended) and to unbind.
    procedure Finish (N : in out Notification; Waiter, Bound : out Thread_Ref)
    with Pre  => Valid (N),
         Post => N = Empty and then Waiter = N'Old.Waiter and then Bound = N'Old.Bound;

    ---------------------------------------------------------------------------
    -- Signal (Wake)
    ---------------------------------------------------------------------------
    -- First: the object a Wake capability names. Second: the root a leaf
    -- forwarded to; it never forwards again.
    type Level is (First, Second);

    type Action is
      (Nothing,       -- the bits wait in the object
       Wake_Waiter,   -- the waiter is woken with Taken (the bits now clear)
       Ring_Bound,    -- no waiter: the bound thread is told; the bits wait
       Forward);      -- a leaf: signal To with To_Badge (the bits wait here)

    type Signal_Result is record
        What     : Action := Nothing;
        Thread   : Thread_Ref := No_Thread;
        Taken    : Unsigned_64 := 0;
        To       : Link := No_Link;
        To_Badge : Unsigned_64 := 0;
    end record;

    procedure Signal (N : in out Notification; Badge : Unsigned_64; At_Level : Level;
                      Result : out Signal_Result)
    with Pre  => Valid (N) and then N.Live and then Badge /= 0,
         Post => Valid (N) and then N.Live
                 and then N.Owner = N'Old.Owner and then N.Bound = N'Old.Bound
                 and then N.Root = N'Old.Root and then N.Root_Badge = N'Old.Root_Badge
                 and then N.Is_Root = N'Old.Is_Root
                 and then
                 (if At_Level = First and then Is_Leaf (N'Old) then
                    Result.What = Forward and then Result.To = N'Old.Root
                    and then Result.To_Badge = N'Old.Root_Badge
                    and then N.Bits = (N'Old.Bits or Badge) and then N.Waiter = N'Old.Waiter
                  elsif N'Old.Waiter /= No_Thread then
                    Result.What = Wake_Waiter and then Result.Thread = N'Old.Waiter
                    and then Result.Taken = (N'Old.Bits or Badge)
                    and then N.Bits = 0 and then N.Waiter = No_Thread
                  elsif N'Old.Bound /= No_Thread and then not Is_Leaf (N'Old) then
                    Result.What = Ring_Bound and then Result.Thread = N'Old.Bound
                    and then N.Bits = (N'Old.Bits or Badge) and then N.Waiter = No_Thread
                  else
                    Result.What = Nothing
                    and then N.Bits = (N'Old.Bits or Badge) and then N.Waiter = N'Old.Waiter)
                 and then (if Result.What = Forward then At_Level = First)
                 and then (if Result.What /= Wake_Waiter then Result.Taken = 0);

    ---------------------------------------------------------------------------
    -- Wait
    ---------------------------------------------------------------------------
    type Wait_Outcome is
      (Got,       -- bits were pending: taken (cleared), no block
       Empty_Now, -- none pending and the caller will not block (deadline Now)
       Blocked,   -- the caller is the waiter now
       Busy);     -- another thread waits, or a bound leaf (its root is told)

    procedure Start_Wait (N : in out Notification; Me : Thread_Ref; Block : Boolean;
                          Outcome : out Wait_Outcome; Taken : out Unsigned_64)
    with Pre  => Valid (N) and then N.Live and then Me /= No_Thread,
         Post => Valid (N) and then N.Live
                 and then N.Owner = N'Old.Owner and then N.Bound = N'Old.Bound
                 and then N.Root = N'Old.Root and then N.Is_Root = N'Old.Is_Root
                 and then N.Root_Badge = N'Old.Root_Badge
                 and then
                 (if N'Old.Bits /= 0 then
                    Outcome = Got and then Taken = N'Old.Bits and then N.Bits = 0
                    and then N.Waiter = N'Old.Waiter
                  elsif not Block then
                    Outcome = Empty_Now and then Taken = 0 and then N = N'Old
                  elsif N'Old.Waiter /= No_Thread or else Is_Leaf (N'Old) then
                    Outcome = Busy and then Taken = 0 and then N = N'Old
                  else
                    Outcome = Blocked and then Taken = 0 and then N.Waiter = Me
                    and then N.Bits = 0);

    -- The waiter left without a signal (deadline, kill): if Me still waits.
    procedure Cancel_Wait (N : in out Notification; Me : Thread_Ref; Removed : out Boolean)
    with Pre  => Valid (N) and then Me /= No_Thread,
         Post => Valid (N) and then Removed = (N'Old.Waiter = Me)
                 and then (if Removed then N = (N'Old with delta Waiter => No_Thread)
                           else N = N'Old);

    ---------------------------------------------------------------------------
    -- Binding
    ---------------------------------------------------------------------------
    type Bind_Outcome is
      (Bound_Now,
       Not_Allowed,  -- a root as a leaf, a leaf as a root, a leaf to a thread,
                     -- an object to itself, a zero badge
       Already,      -- the leaf is bound, or the root has a thread
       Busy);        -- a thread waits on the would-be leaf

    -- Bind L (by its Wait right) to R (by a Wake right of R with Badge;
    -- R_Link names R). L's pending bits are forwarded at once (Told), so a
    -- wake racing the binding is not lost.
    procedure Bind_Leaf (L, R : in out Notification; L_Id : Object_Id; R_Link : Link;
                         Badge : Unsigned_64; Outcome : out Bind_Outcome;
                         Told : out Signal_Result)
    with Pre  => Valid (L) and then Valid (R) and then L.Live and then R.Live
                 and then R_Link.Id /= No_Object,
         Post => Valid (L) and then Valid (R) and then L.Live and then R.Live
                 and then L.Owner = L'Old.Owner and then R.Owner = R'Old.Owner
                 and then (Outcome = Bound_Now) =
                   (L_Id /= R_Link.Id and then Badge /= 0 and then not L'Old.Is_Root
                    and then not Is_Leaf (L'Old) and then L'Old.Waiter = No_Thread
                    and then not Is_Leaf (R'Old))
                 and then
                 (if Outcome = Bound_Now then
                    L.Root = R_Link and then L.Root_Badge = Badge and then R.Is_Root
                    and then L.Bits = L'Old.Bits and then L.Waiter = No_Thread
                    and then R.Bound = R'Old.Bound and then R.Root = R'Old.Root
                    and then
                    (if L'Old.Bits /= 0 then
                       Told.What in Wake_Waiter | Ring_Bound | Nothing
                       and then R.Bits = (if Told.What = Wake_Waiter then 0
                                          else (R'Old.Bits or Badge))
                     else Told.What = Nothing and then R.Bits = R'Old.Bits
                          and then R.Waiter = R'Old.Waiter)
                  else L = L'Old and then R = R'Old and then Told.What = Nothing);

    procedure Unbind_Leaf (L : in out Notification)
    with Pre  => Valid (L),
         Post => Valid (L) and then not Is_Leaf (L) and then L.Bits = L'Old.Bits
                 and then L.Live = L'Old.Live and then L.Owner = L'Old.Owner
                 and then L.Is_Root = L'Old.Is_Root and then L.Waiter = L'Old.Waiter
                 and then L.Bound = L'Old.Bound;

    -- Bind root R to thread T, which has no bound notification (the kernel
    -- checks T's side). Pending bits ring T at once (Told).
    procedure Bind_Thread (R : in out Notification; T : Thread_Ref;
                           Outcome : out Bind_Outcome; Told : out Signal_Result)
    with Pre  => Valid (R) and then R.Live and then T /= No_Thread,
         Post => Valid (R) and then R.Live and then R.Owner = R'Old.Owner
                 and then R.Bits = R'Old.Bits and then R.Waiter = R'Old.Waiter
                 and then (Outcome = Bound_Now) = (not Is_Leaf (R'Old) and then R'Old.Bound = No_Thread)
                 and then
                 (if Outcome = Bound_Now then
                    R.Bound = T and then R.Is_Root
                    and then (if R.Bits /= 0 and then R.Waiter = No_Thread
                              then Told.What = Ring_Bound and then Told.Thread = T
                              else Told.What = Nothing)
                  else R = R'Old and then Told.What = Nothing);

    procedure Unbind_Thread (R : in out Notification; T : Thread_Ref; Removed : out Boolean)
    with Pre  => Valid (R) and then T /= No_Thread,
         Post => Valid (R) and then Removed = (R'Old.Bound = T)
                 and then (if Removed then R = (R'Old with delta Bound => No_Thread)
                           else R = R'Old);

end Notification_Objects;
