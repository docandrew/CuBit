-------------------------------------------------------------------------------
-- Fan_In_Model: a proved model of a table of notifications (test-only; not
-- kernel code). It drives the kernel's own Notification_Objects operations
-- on an array of objects, each with a generation the kernel's table bumps
-- when the object ends (so a link to an ended object is stale), exactly as
-- Process.Notifications does.
--
-- Proved over every sequence of creates, ends, bindings, unbindings, waits
-- and wakes:
--   * Inv: every live link from a leaf names a root, and a root is never a
--     leaf (so the fan-in is leaf -> root -> thread, two levels);
--   * Prove_No_Chain: no leaf's root is a leaf, and no object is its own
--     root, so no chain longer than two and no cycle exists;
--   * revocation by generation: Revoke advances an object's generation, a
--     capability of an older generation (revoked, or of an ended object
--     whose slot was reused) authorizes nothing (Wake_With, Wait_With,
--     Bind_With change nothing), and generations never go back;
--   * a binding is only as good as the Wake capability that made it: a
--     leaf's link names that capability's generation, so after the root's
--     Revoke no leaf reaches it (Revoke), and a Wake of such a leaf unbinds
--     it, keeping its bits (Wake);
--   * Wake: the thread woken is exactly the expected one: the waiter of the
--     object signalled, or of its root when it is a leaf; otherwise the
--     bound thread is rung; every other object is unchanged; the second
--     step never forwards.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Notification_Objects; use Notification_Objects;

package Fan_In_Model with SPARK_Mode => On is

    subtype Id is Object_Id range 1 .. 6;
    type Objects is array (Id) of Notification;
    type Generations is array (Id) of Unsigned_32;

    -- G: each object's capability generation (Revoke and the end advance
    -- it), what leaf links name; L: its life (only the end advances it),
    -- what the kernel's thread binding names, so a revocation keeps the
    -- owner's bound thread (in O) but no leaf link made before it.
    type State is record
        O : Objects;
        G : Generations;
        L : Generations;
    end record;

    function Live_Link (S : State; L : Link) return Boolean is
      (L.Id in Id and then S.G (L.Id) = L.Gen and then S.O (L.Id).Live);

    -- A link names an object of the table; a link to an object that is not
    -- live is stale for good (its generation is no longer the object's),
    -- so a new object in that place is never reached through it.
    function Inv (S : State) return Boolean is
      ((for all I in Id => Valid (S.O (I)) and then S.L (I) <= S.G (I))
       and then
       (for all I in Id =>
          (if Is_Leaf (S.O (I)) then
             S.O (I).Root.Id in Id
             and then S.O (I).Root.Gen <= S.G (S.O (I).Root.Id)
             and then (S.O (S.O (I).Root.Id).Live
                       or else S.O (I).Root.Gen /= S.G (S.O (I).Root.Id))))
       and then
       (for all I in Id =>
          (if Is_Leaf (S.O (I)) and then Live_Link (S, S.O (I).Root) then
             S.O (S.O (I).Root.Id).Is_Root)));

    function Initial return State is
      (O => [others => Empty], G => [others => 1], L => [others => 1]);

    procedure Prove_Initial with Ghost, Global => null, Post => Inv (Initial);

    procedure Create_Object (S : in out State; I : Id; Owner : Owner_Ref)
    with Global => null,
         Pre  => Inv (S) and then not S.O (I).Live and then Owner /= No_Owner,
         Post => Inv (S) and then S.O (I).Live and then S.G = S'Old.G and then S.L = S'Old.L;

    -- The Wait right closed: the object ends; its generation advances.
    procedure End_Object (S : in out State; I : Id)
    with Global => null,
         Pre  => Inv (S) and then S.G (I) < Unsigned_32'Last and then S.L (I) < Unsigned_32'Last,
         Post => Inv (S) and then not S.O (I).Live and then S.G (I) = S'Old.G (I) + 1
                 and then S.L (I) = S'Old.L (I) + 1;

    -- Bind leaf L to root R with Badge (a current Wake right of R). The
    -- kernel refuses one object bound to itself before locking anything.
    procedure Bind (S : in out State; L, R : Id; Badge : Unsigned_64;
                    Outcome : out Bind_Outcome)
    with Global => null,
         Pre  => Inv (S) and then S.O (L).Live and then S.O (R).Live,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L
                 -- A binding made now reaches the root (until its Revoke).
                 and then (if Outcome = Bound_Now then L /= R
                           and then Is_Leaf (S.O (L)) and then Live_Link (S, S.O (L).Root)
                           and then S.O (L).Root.Id = R);

    procedure Unbind (S : in out State; L : Id)
    with Global => null,
         Pre  => Inv (S),
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L;

    procedure Bind_To_Thread (S : in out State; R : Id; T : Thread_Ref;
                              Outcome : out Bind_Outcome)
    with Global => null,
         Pre  => Inv (S) and then S.O (R).Live and then T /= No_Thread,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L;

    procedure Unbind_From_Thread (S : in out State; R : Id; T : Thread_Ref)
    with Global => null,
         Pre  => Inv (S) and then T /= No_Thread,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L;

    -- Wait on I (a leaf whose root ended is unbound first, as the kernel does).
    procedure Wait (S : in out State; I : Id; Me : Thread_Ref; Block : Boolean;
                    Outcome : out Wait_Outcome; Taken : out Unsigned_64)
    with Global => null,
         Pre  => Inv (S) and then S.O (I).Live and then Me /= No_Thread,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L;

    procedure Cancel (S : in out State; I : Id; Me : Thread_Ref)
    with Global => null,
         Pre  => Inv (S) and then Me /= No_Thread,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L;

    -- A capability of generation Gen authorizes an operation on I.
    function Authorizes (S : State; I : Id; Gen : Unsigned_32) return Boolean is
      (S.O (I).Live and then S.G (I) = Gen);

    -- Revoke (Right_Revoke): I's generation advances, I lives on; its
    -- waiter (holding the old Wait right) leaves, told Revoked.
    procedure Revoke (S : in out State; I : Id; Waiter : out Thread_Ref)
    with Global => null,
         Pre  => Inv (S) and then S.O (I).Live and then S.G (I) < Unsigned_32'Last,
         Post => Inv (S) and then S.O (I).Live and then S.G (I) = S'Old.G (I) + 1
                 and then Waiter = S'Old.O (I).Waiter and then S.O (I).Waiter = No_Thread
                 and then (for all J in Id => (if J /= I then S.O (J) = S'Old.O (J)
                                                 and then S.G (J) = S'Old.G (J)))
                 and then not Authorizes (S, I, S'Old.G (I))
                 -- Its life (its bound thread, in O) is kept; no leaf
                 -- bound with an old Wake right reaches it any more.
                 and then S.L = S'Old.L
                 and then (for all J in Id =>
                             (if Is_Leaf (S.O (J)) and then S.O (J).Root.Id = I then
                                not Live_Link (S, S.O (J).Root)));

    -- Bind_Leaf through a Wake right of R of generation Gen: nothing
    -- changes unless it authorizes (a revoked root's old right binds
    -- nothing: Revoked_Object).
    procedure Bind_With (S : in out State; L, R : Id; Gen : Unsigned_32; Badge : Unsigned_64;
                         Done : out Boolean; Outcome : out Bind_Outcome)
    with Global => null,
         Pre  => Inv (S) and then S.O (L).Live,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L
                 and then Done = Authorizes (S'Old, R, Gen)
                 and then (if not Done then S = S'Old);

    -- A Wait through a capability of generation Gen: nothing changes
    -- unless it authorizes.
    procedure Wait_With (S : in out State; I : Id; Gen : Unsigned_32; Me : Thread_Ref;
                         Block : Boolean; Done : out Boolean;
                         Outcome : out Wait_Outcome; Taken : out Unsigned_64)
    with Global => null,
         Pre  => Inv (S) and then Me /= No_Thread,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L and then Done = Authorizes (S'Old, I, Gen)
                 and then (if not Done then S = S'Old and then Taken = 0);

    -- The thread a Wake of I must reach, and how.
    function Leaf_Root (S : State; I : Id) return Boolean is
      (Is_Leaf (S.O (I)) and then Live_Link (S, S.O (I).Root));

    function Target (S : State; I : Id) return Id is
      (if Leaf_Root (S, I) then S.O (I).Root.Id else I)
    with Pre => Inv (S);

    -- A Wake of I through a capability with Badge: the kernel's two steps.
    -- Woken is the thread made runnable (with Taken), Rung the bound
    -- thread told.
    procedure Wake (S : in out State; I : Id; Badge : Unsigned_64;
                    Woken, Rung : out Thread_Ref; Taken : out Unsigned_64)
    with Global => null,
         Pre  => Inv (S) and then S.O (I).Live and then Badge /= 0,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L
                 -- Exactly the target's waiter, else its bound thread.
                 and then Woken = S'Old.O (Target (S'Old, I)).Waiter
                 and then (if Woken = No_Thread
                           then Rung = S'Old.O (Target (S'Old, I)).Bound
                           else Rung = No_Thread)
                 and then (if Woken /= No_Thread then S.O (Target (S'Old, I)).Waiter = No_Thread)
                 -- Nothing else moves: only I and its root change.
                 and then (for all J in Id =>
                             (if J /= I and then J /= Target (S'Old, I) then
                                S.O (J) = S'Old.O (J)))
                 -- Bits are never lost: they are pending in I, or taken
                 -- by the woken waiter.
                 and then (if Woken = No_Thread then (S.O (I).Bits and Badge) = Badge)
                 -- A leaf whose link is stale (its root revoked or ended)
                 -- is unbound: its bits wait in it, for its own Wait.
                 and then (if Is_Leaf (S'Old.O (I)) and then not Leaf_Root (S'Old, I) then
                             not Is_Leaf (S.O (I)));

    -- A Wake through a capability of generation Gen: nothing changes, and
    -- nobody wakes, unless it authorizes.
    procedure Wake_With (S : in out State; I : Id; Gen : Unsigned_32; Badge : Unsigned_64;
                         Done : out Boolean; Woken, Rung : out Thread_Ref; Taken : out Unsigned_64)
    with Global => null,
         Pre  => Inv (S) and then Badge /= 0,
         Post => Inv (S) and then S.G = S'Old.G and then S.L = S'Old.L and then Done = Authorizes (S'Old, I, Gen)
                 and then (if not Done then S = S'Old and then Woken = No_Thread
                           and then Rung = No_Thread and then Taken = 0);

    -- Generations only advance: a capability minted at an older one never
    -- authorizes again, whatever was created in the slot since.
    procedure Prove_Stale_Stays_Stale (S : State; I : Id; Gen : Unsigned_32)
    with Ghost, Global => null,
         Pre  => Gen < S.G (I),
         Post => not Authorizes (S, I, Gen);

    -- No chain longer than two, no cycle.
    procedure Prove_No_Chain (S : State; I : Id)
    with Ghost, Global => null,
         Pre  => Inv (S) and then Leaf_Root (S, I),
         Post => S.O (I).Root.Id /= I
                 and then not Is_Leaf (S.O (S.O (I).Root.Id))
                 and then not Leaf_Root (S, S.O (I).Root.Id);

end Fan_In_Model;
