-------------------------------------------------------------------------------
-- Futex_Protocol: a proved transition-system model of the kernel's futex
-- protocol for one futex word (docs/threads.md). Test-only; not kernel code.
--
-- FUTEX_WAIT runs as two steps under the bucket lock: Wait_Load (take the
-- lock, read the user word) and Wait_Commit (sleep only if the word read
-- equals the expected value, then release the lock). User threads may store
-- to the word at any time, including between those steps; the kernel never
-- locks user stores. FUTEX_WAKE takes the same bucket lock.
--
-- Pending models the obligation a waker takes on by changing the word: to
-- call FUTEX_WAKE afterwards. The invariant says every sleeper, and every
-- waiter between load and commit, either expects the word's current value
-- or is covered by a pending wake. Hence a sleeper never waits forever on a
-- stale value when every changing store is followed by a wake (no lost
-- wakeup), even though stores race with the kernel's load.
--
-- Shared futexes (IPC-010): the word is in a grant whose life can end at
-- any time (End, under the grant lock, not the bucket lock). Wait_Load
-- reads whether the life is still live under the bucket lock and refuses
-- a dead one; the end then sweeps the bucket (Sweep, under the bucket
-- lock), waking every sleeper as Revoked. The invariant adds: no thread
-- sleeps on an ended life unless the sweep is still to run, so after the
-- sweep nobody sleeps on it (Prove_No_Sleeper_After_Revoke), however the
-- end races a waiter between its load and its commit.
--
-- Wake-plus-wait (IPC-010): Wait's wake operand is an ordinary wake of
-- another word, then this word's Wait_Load and Wait_Commit; each step keeps
-- its word's invariant, so the pair loses no wake on either word
-- (Prove_Wake_Then_Wait).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Futex_Protocol with SPARK_Mode => On is

    type Thread is range 1 .. 3;
    type Phase is (Running, Loaded, Sleeping);

    type Thread_State is record
        P        : Phase := Running;
        Expected : Unsigned_32 := 0;   -- FUTEX_WAIT's expected value
        Seen     : Unsigned_32 := 0;   -- the word as loaded under the lock
        Live     : Boolean := True;    -- the life was live at the load
    end record;

    type Threads is array (Thread) of Thread_State;

    type State is record
        Value   : Unsigned_32 := 0;
        T       : Threads;
        Holder  : Boolean := False;    -- the bucket lock is held
        Owner   : Thread := Thread'First;
        Pending : Boolean := False;    -- a store changed the word since the last wake
        Ended   : Boolean := False;    -- the grant life ended (shared futexes)
        Sweep_Pending : Boolean := False;  -- its sweep has not run yet
    end record;

    function Lock_Consistent (S : State) return Boolean is
      ((for all I in Thread => (if S.T (I).P = Loaded then S.Holder and then S.Owner = I))
       and then (if S.Holder then S.T (S.Owner).P = Loaded));

    -- The protocol invariant.
    function Inv (S : State) return Boolean is
      (Lock_Consistent (S) and then
       (if S.Sweep_Pending then S.Ended) and then
       (for all I in Thread =>
          (case S.T (I).P is
             when Running  => True,
             when Loaded   => (S.T (I).Seen = S.Value or else S.Pending) and then
                              (if S.T (I).Live and then S.Ended then S.Sweep_Pending),
             when Sleeping => (S.T (I).Expected = S.Value or else S.Pending) and then
                              (if S.Ended then S.Sweep_Pending))));

    function Initial (V : Unsigned_32) return State is
      (Value => V, T => (others => (Running, 0, 0, True)), Holder => False,
       Owner => Thread'First, Pending => False, Ended => False, Sweep_Pending => False);

    procedure Prove_Initial (V : Unsigned_32)
    with Ghost, Global => null, Post => Inv (Initial (V));

    -- A running thread stores to the word (no lock).
    procedure Store (S : in out State; I : Thread; V : Unsigned_32)
    with
        Global => null,
        Pre  => Inv (S) and then S.T (I).P = Running,
        Post => Inv (S) and then S.Value = V and then
                S.T = S'Old.T and then S.Holder = S'Old.Holder and then
                S.Owner = S'Old.Owner and then
                (if V /= S'Old.Value then S.Pending else S.Pending = S'Old.Pending);

    -- FUTEX_WAIT, step 1: take the bucket lock and read the word.
    procedure Wait_Load (S : in out State; I : Thread; E : Unsigned_32)
    with
        Global => null,
        Pre  => Inv (S) and then S.T (I).P = Running and then not S.Holder,
        Post => Inv (S) and then S.T (I).P = Loaded and then
                S.T (I).Seen = S.Value and then S.T (I).Expected = E and then
                S.T (I).Live = not S.Ended and then
                S.Ended = S'Old.Ended and then S.Sweep_Pending = S'Old.Sweep_Pending and then
                S.Value = S'Old.Value and then S.Pending = S'Old.Pending and then
                S.Holder and then S.Owner = I and then
                (for all J in Thread => (if J /= I then S.T (J) = S'Old.T (J)));

    -- FUTEX_WAIT, step 2: sleep if the word read was the expected value,
    -- else return EAGAIN; release the lock either way.
    procedure Wait_Commit (S : in out State; I : Thread; Slept : out Boolean)
    with
        Global => null,
        Pre  => Inv (S) and then S.T (I).P = Loaded,
        Post => Inv (S) and then not S.Holder and then
                Slept = (S'Old.T (I).Seen = S'Old.T (I).Expected and then S'Old.T (I).Live) and then
                S.Ended = S'Old.Ended and then S.Sweep_Pending = S'Old.Sweep_Pending and then
                (if Slept then S.T (I).P = Sleeping else S.T (I).P = Running) and then
                S.T (I).Expected = S'Old.T (I).Expected and then
                S.Value = S'Old.Value and then S.Pending = S'Old.Pending and then
                (for all J in Thread => (if J /= I then S.T (J) = S'Old.T (J)));

    -- FUTEX_WAKE of every waiter: takes the bucket lock, so no waiter is
    -- between load and commit. Discharges the pending obligation.
    procedure Wake_All (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Holder,
        Post => Inv (S) and then not S.Pending and then not S.Holder and then
                S.Value = S'Old.Value and then
                S.Ended = S'Old.Ended and then S.Sweep_Pending = S'Old.Sweep_Pending and then
                (for all I in Thread => S.T (I).P /= Sleeping);

    -- FUTEX_WAKE of one waiter (a mutex unlock). The obligation remains
    -- while others sleep; the woken thread will act on the new value.
    procedure Wake_One (S : in out State; I : Thread)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Holder and then S.T (I).P = Sleeping,
        Post => Inv (S) and then S.T (I).P = Running and then
                not S.Holder and then S.Value = S'Old.Value and then
                S.Pending = S'Old.Pending and then
                S.Ended = S'Old.Ended and then S.Sweep_Pending = S'Old.Sweep_Pending and then
                (for all J in Thread => (if J /= I then S.T (J) = S'Old.T (J)));

    -- The grant life ends (revoke, give back, a party's death): marked
    -- under the grant lock only, so it races every futex step; its sweep
    -- is owed.
    procedure End_Life (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Ended,
        Post => Inv (S) and then S.Ended and then S.Sweep_Pending and then
                S.T = S'Old.T and then S.Value = S'Old.Value and then
                S.Pending = S'Old.Pending and then S.Holder = S'Old.Holder and then
                S.Owner = S'Old.Owner;

    -- The sweep: takes the bucket lock and wakes every sleeper (Revoked).
    procedure Sweep (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Holder and then S.Sweep_Pending,
        Post => Inv (S) and then not S.Sweep_Pending and then S.Ended and then
                not S.Holder and then S.Value = S'Old.Value and then
                S.Pending = S'Old.Pending and then
                (for all I in Thread => S.T (I).P /= Sleeping);

    -- No one sleeps on an ended life once its sweep has run, and no later
    -- wait can sleep on it (its load sees the end).
    procedure Prove_No_Sleeper_After_Revoke (S : State)
    with
        Ghost, Global => null,
        Pre  => Inv (S) and then S.Ended and then not S.Sweep_Pending,
        Post => (for all I in Thread => S.T (I).P /= Sleeping);

    -- The end racing a waiter between its load and its commit: the waiter
    -- sleeps on the stale life, and the sweep, which needs the lock it held,
    -- wakes it.
    procedure Prove_Revoke_Race (V : Unsigned_32)
    with Ghost, Global => null;

    -- Wake-plus-wait across two words: a hand-off wakes the peer's word,
    -- then waits on its own; the peer's store-and-wake racing either step
    -- is not lost.
    procedure Prove_Wake_Then_Wait (V0, V1 : Unsigned_32)
    with Ghost, Global => null, Pre => V0 /= V1;

    -- The theorem: with no wake pending, every sleeper expects the word's
    -- current value, so a store-then-wake discipline never strands one.
    procedure Prove_No_Stale_Sleeper (S : State)
    with
        Ghost, Global => null,
        Pre  => Inv (S) and then not S.Pending,
        Post => (for all I in Thread =>
                   (if S.T (I).P = Sleeping then S.T (I).Expected = S.Value));

    -- A store racing between load and commit is covered: the pending wake
    -- must take the lock, which the loaded waiter holds until it commits.
    procedure Prove_Racing_Store_Covered (V0, V1 : Unsigned_32)
    with
        Ghost, Global => null,
        Pre  => V0 /= V1;

end Futex_Protocol;
