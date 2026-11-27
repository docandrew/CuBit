-------------------------------------------------------------------------------
-- Word_Protocol: a proved model of the private futex protocol for one word
-- over the kernel's own Private_Futexes table (test-only; not kernel code).
--
-- As tests/futex-queues' Futex_Protocol: Wait runs as two steps under the
-- table's lock, Wait_Load (take the lock, read the user word) and
-- Wait_Commit (enqueue in the table only if the word read is the expected
-- value, then release the lock). User stores race with both steps. Pending
-- is the obligation a store that changes the word takes on: to wake
-- afterwards. Wake takes the same lock and removes waiters from the real
-- table in batches of at most Private_Futexes.Chunk, releasing the lock
-- between batches.
--
-- Proved: every sleeper is exactly a used table entry for the word, and
-- expects the current value unless a wake is pending, so a store-then-wake
-- discipline strands no sleeper (Prove_No_Stale_Sleeper); a full wake
-- leaves no sleeper (Wake_All), whatever the batches; a wake of one takes
-- the oldest sleeper.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Private_Futexes; use Private_Futexes;

package Word_Protocol with SPARK_Mode => On is

    -- The word's user address; threads 1 .. 3 in slots 0 .. 2.
    The_Word : constant Unsigned_64 := 16#1000#;
    type Thread is range 1 .. 3;
    function Slot_Of (I : Thread) return Slot is (Slot (I) - 1);
    function Ref_Of (I : Thread) return Thread_Ref is (Thread_Ref (I));

    type Phase is (Running, Loaded, Sleeping);

    type Thread_State is record
        P        : Phase := Running;
        Expected : Unsigned_32 := 0;
        Seen     : Unsigned_32 := 0;
    end record;
    type Threads is array (Thread) of Thread_State;

    type State is record
        Value   : Unsigned_32 := 0;
        Tab     : Table;
        T       : Threads;
        Holder  : Boolean := False;
        Owner   : Thread := Thread'First;
        Pending : Boolean := False;
    end record;

    function In_Table (S : State; I : Thread) return Boolean is
      (S.Tab.Used (Slot_Of (I)) and then S.Tab.Word (Slot_Of (I)) = The_Word
       and then S.Tab.Thread (Slot_Of (I)) = Ref_Of (I));

    function Inv (S : State) return Boolean is
      (Valid (S.Tab)
       and then S.Tab.Next_Ticket < Unsigned_64'Last - 4
       and then (for all U in Slot => (if U > 2 then not S.Tab.Used (U)))
       and then (for all I in Thread => (S.T (I).P = Sleeping) = S.Tab.Used (Slot_Of (I)))
       and then (for all I in Thread => (if S.T (I).P = Sleeping then In_Table (S, I)))
       and then (for all I in Thread => (if S.T (I).P = Loaded then S.Holder and then S.Owner = I))
       and then (if S.Holder then S.T (S.Owner).P = Loaded)
       and then (for all I in Thread =>
                   (case S.T (I).P is
                      when Running  => True,
                      when Loaded   => S.T (I).Seen = S.Value or else S.Pending,
                      when Sleeping => S.T (I).Expected = S.Value or else S.Pending)));

    function Initial (V : Unsigned_32) return State is
      (Value => V, Tab => (Used => [others => False], Word => [others => 0],
                           Ticket => [others => 0], Thread => [others => No_Thread],
                           Next_Ticket => 0),
       T => [others => (Running, 0, 0)], Holder => False, Owner => Thread'First,
       Pending => False);

    procedure Prove_Initial (V : Unsigned_32)
    with Ghost, Global => null, Post => Inv (Initial (V));

    -- A running thread stores to the word (no lock).
    procedure Store (S : in out State; I : Thread; V : Unsigned_32)
    with Global => null,
         Pre  => Inv (S) and then S.T (I).P = Running,
         Post => Inv (S) and then S.Value = V
                 and then (if V /= S'Old.Value then S.Pending else S.Pending = S'Old.Pending);

    procedure Wait_Load (S : in out State; I : Thread; E : Unsigned_32)
    with Global => null,
         Pre  => Inv (S) and then S.T (I).P = Running and then not S.Holder,
         Post => Inv (S) and then S.T (I).P = Loaded and then S.Value = S'Old.Value
                 and then S.Pending = S'Old.Pending;

    -- Sleep (enqueue) only if the word read was the expected value; the
    -- ticket counter is far from its end (a run of 2**64 waits).
    procedure Wait_Commit (S : in out State; I : Thread; Slept : out Boolean)
    with Global => null,
         Pre  => Inv (S) and then S.T (I).P = Loaded
                 and then S.Tab.Next_Ticket < Unsigned_64'Last - 5,
         Post => Inv (S) and then not S.Holder
                 and then Slept = (S'Old.T (I).Seen = S'Old.T (I).Expected)
                 and then S.Value = S'Old.Value and then S.Pending = S'Old.Pending;

    -- Wake every waiter of the word, in batches under the lock.
    procedure Wake_All (S : in out State)
    with Global => null,
         Pre  => Inv (S) and then not S.Holder,
         Post => Inv (S) and then not S.Pending and then not S.Holder
                 and then S.Value = S'Old.Value
                 and then (for all I in Thread => S.T (I).P /= Sleeping);

    -- Wake one waiter (a mutex unlock): the oldest.
    procedure Wake_One (S : in out State; Woke : out Boolean)
    with Global => null,
         Pre  => Inv (S) and then not S.Holder,
         Post => Inv (S) and then not S.Holder and then S.Value = S'Old.Value
                 and then S.Pending = S'Old.Pending
                 and then Woke = (for some I in Thread => S'Old.T (I).P = Sleeping)
                 -- The one woken was the oldest sleeper.
                 and then (for all I in Thread =>
                             (if S'Old.T (I).P = Sleeping and then S.T (I).P = Running then
                                (for all J in Thread =>
                                   (if S.T (J).P = Sleeping then
                                      S'Old.Tab.Ticket (Slot_Of (I)) < S.Tab.Ticket (Slot_Of (J))))));

    -- No wake pending: every sleeper expects the word's current value.
    procedure Prove_No_Stale_Sleeper (S : State)
    with Ghost, Global => null,
         Pre  => Inv (S) and then not S.Pending,
         Post => (for all I in Thread =>
                    (if S.T (I).P = Sleeping then S.T (I).Expected = S.Value));

end Word_Protocol;
