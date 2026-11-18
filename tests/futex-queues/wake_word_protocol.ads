-------------------------------------------------------------------------------
-- Wake_Word_Protocol: a proved model of the Ready_Word wait (IPC-010
-- option B) for one process: its queues and its wake word, one waiter.
-- Test-only; not kernel code.
--
-- The waiter checks everything under the mailbox lock: whether queued work
-- (Activity) or a wake of its word (Flag) is pending, and whether the word
-- still holds the value it last saw. It sleeps only if none of that is so,
-- still under the lock (Wait_Load then Wait_Commit). Peers change the word
-- with no lock (Store) and then owe a futex wake of it (Owed), which the
-- kernel turns into Note_Wake under the mailbox lock: set Flag, wake the
-- waiter. Queued work (Enqueue) also arrives under the lock.
--
-- Invariant: a sleeping (or loaded) waiter sees no pending activity or
-- wake, and either its value is the word's or a wake is owed. Theorem
-- (Prove_No_Lost_Wake): with no wake owed, a sleeper's word still holds
-- its value and nothing is pending: whatever a peer did, it has woken it.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Wake_Word_Protocol with SPARK_Mode => On is

    type Phase is (Running, Loaded, Sleeping);

    type State is record
        Value    : Unsigned_32 := 0;    -- the wake word
        Activity : Boolean := False;    -- queued work the wait reports
        Flag     : Boolean := False;    -- a wake of the word, not yet taken
        Owed     : Boolean := False;    -- a store's futex wake not yet made
        P        : Phase := Running;
        Expected : Unsigned_32 := 0;    -- the value the waiter last saw
        Holder   : Boolean := False;    -- the waiter holds the mailbox lock
    end record;

    function Inv (S : State) return Boolean is
      ((S.P = Loaded) = S.Holder and then
       (if S.P in Loaded | Sleeping then
          not S.Activity and then not S.Flag and then
          (S.Expected = S.Value or else S.Owed)));

    function Initial (V : Unsigned_32) return State is
      (Value => V, Activity => False, Flag => False, Owed => False,
       P => Running, Expected => V, Holder => False);

    -- The waiter, step 1: under the lock, register and look. Returns
    -- (Back) at once when anything is pending or the word moved.
    procedure Wait_Load (S : in out State; E : Unsigned_32; Back : out Boolean)
    with
        Global => null,
        Pre  => Inv (S) and then S.P = Running,
        Post => Inv (S) and then
                Back = (S'Old.Activity or else S'Old.Flag or else S'Old.Value /= E) and then
                (if Back then S.P = Running and then not S.Flag
                 else S.P = Loaded and then S.Expected = E) and then
                S.Value = S'Old.Value and then S.Owed = S'Old.Owed;

    -- Step 2: sleep, releasing the lock.
    procedure Wait_Commit (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then S.P = Loaded,
        Post => Inv (S) and then S.P = Sleeping and then S.Value = S'Old.Value and then
                S.Owed = S'Old.Owed;

    -- A peer changes the word (no lock) and owes a wake of it.
    procedure Store (S : in out State; V : Unsigned_32)
    with
        Global => null,
        Pre  => Inv (S),
        Post => Inv (S) and then S.Value = V and then S.P = S'Old.P and then
                (if V /= S'Old.Value then S.Owed else S.Owed = S'Old.Owed);

    -- The futex wake of the word: under the mailbox lock (so never while
    -- the waiter is loaded), it sets the flag and wakes the waiter.
    procedure Note_Wake (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Holder,
        Post => Inv (S) and then not S.Owed and then S.Flag and then
                S.P = (if S'Old.P = Sleeping then Running else S'Old.P) and then
                S.Value = S'Old.Value;

    -- Queued work arrives under the lock and wakes the waiter.
    procedure Enqueue (S : in out State)
    with
        Global => null,
        Pre  => Inv (S) and then not S.Holder,
        Post => Inv (S) and then S.P /= Sleeping and then S.Value = S'Old.Value and then
                S.Owed = S'Old.Owed;

    procedure Prove_No_Lost_Wake (S : State)
    with
        Ghost, Global => null,
        Pre  => Inv (S) and then not S.Owed and then S.P = Sleeping,
        Post => S.Expected = S.Value and then not S.Activity and then not S.Flag;

    -- A peer's store and wake racing the waiter between its look and its
    -- sleep: the wake needs the lock the waiter holds, so it comes after
    -- the sleep and wakes it.
    procedure Prove_Racing_Wake (V0, V1 : Unsigned_32)
    with Ghost, Global => null, Pre => V0 /= V1;

end Wake_Word_Protocol;
