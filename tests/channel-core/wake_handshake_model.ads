------------------------------------------------------------------------------
--  The needs-wake handshake of a channel side (CAP-002 step 3,
--  CuBit.Request_Channels' Arm and Ring_Once, the kernel's coalesced channel
--  wake), as a state machine over every interleaving of its steps. Proved
--  (level 2): no lost wakeup, that is, the consumer is never asleep with
--  data published, the producer done, and no wake pending.
--
--  Assumed, not proved: the code refines this model, and the stores and
--  loads it orders happen in this order (each side stores its word, then
--  mfence, then loads the other's: sequential consistency on x86-64 for
--  those accesses). Tested by tests/channel-core (the race the re-check
--  covers, injected) and the guest test.
------------------------------------------------------------------------------
pragma Ada_2022;

package Wake_Handshake_Model with SPARK_Mode is

   Max_Items : constant := 1_000;
   subtype Count is Natural range 0 .. Max_Items;

   --  Consumer: Running; Armed (stored its needs-wake word, not yet looked
   --  again); Asleep. Producer: Idle; Published (stored the item, not yet
   --  read the needs-wake word).
   type Consumer_Phase is (Running, Armed, Asleep);
   type Producer_Phase is (Idle, Published);

   type State is record
      Produced    : Count := 0;       --  the producer's index
      Consumed    : Count := 0;       --  the consumer's index
      Needs_Wake  : Boolean := False; --  the consumer's word
      Wake_Pending : Boolean := False; --  the kernel's coalesced wake
      Consumer    : Consumer_Phase := Running;
      Producer    : Producer_Phase := Idle;
   end record;

   function Invariant (S : State) return Boolean is
     (S.Consumed <= S.Produced
      --  Armed or asleep only with the word set.
      and then (if S.Consumer in Armed | Asleep then S.Needs_Wake)
      --  No lost wakeup.
      and then (if S.Consumer = Asleep and then S.Producer = Idle and then S.Produced > S.Consumed
                then S.Wake_Pending));

   type Action is
     (Produce,        --  P1: store the item (Produced + 1)
      Check_And_Wake, --  P2: read Needs_Wake; if set, send the wake (coalesced)
      Arm,            --  C1: store Needs_Wake
      Recheck,        --  C2: look again: data -> disarm and run, else sleep
      Wake_Up,        --  the kernel delivers the pending wake: run, disarm
      Consume);       --  the running consumer takes an item

   function Enabled (S : State; A : Action) return Boolean is
     (case A is
         when Produce        => S.Producer = Idle and then S.Produced < Max_Items,
         when Check_And_Wake => S.Producer = Published,
         when Arm            => S.Consumer = Running,
         when Recheck        => S.Consumer = Armed,
         when Wake_Up        => S.Consumer = Asleep and then S.Wake_Pending,
         when Consume        => S.Consumer = Running and then S.Consumed < S.Produced);

   procedure Step (S : in out State; A : Action)
   with Pre  => Invariant (S) and then Enabled (S, A),
        Post => Invariant (S);

end Wake_Handshake_Model;
