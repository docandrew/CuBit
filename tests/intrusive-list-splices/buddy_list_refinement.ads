with Buddy_Blocks;
with Intrusive_List_Splices;

-- Proof-only composition of the production ledger and link primitives.
-- IDs stand for admitted physical block heads; zero is the list sentinel.
-- This does not establish the physical adapter's address-to-ID correspondence.
package Buddy_List_Refinement with SPARK_Mode, Ghost is
   use type Buddy_Blocks.Descriptor;
   subtype ID is Natural;
   type References is array (ID range <>) of ID;
   type Descriptors is array (ID range <>) of Buddy_Blocks.Descriptor;

   function Shape (Next, Previous, Sequence, Rank : References;
                   Ledger : Descriptors) return Boolean is
     (Next'First = 0 and then Next'Last > 0
      and then Previous'First = 0 and then Previous'Last = Next'Last
      and then Sequence'First = 0 and then Sequence'Last = Next'Last
      and then Rank'First = 0 and then Rank'Last = Next'Last
      and then Ledger'First = 0 and then Ledger'Last = Next'Last);

   function Valid (Next, Previous, Sequence, Rank : References;
                   Ledger : Descriptors; Count : Natural;
                   Size : Buddy_Blocks.Order) return Boolean is
     (Shape (Next, Previous, Sequence, Rank, Ledger)
      and then Count <= Next'Last and then Sequence (0) = 0
      and then Rank (0) = 0
      and then Next (0) = (if Count = 0 then 0 else Sequence (1))
      and then Previous (0) = Sequence (Count)
      and then (for all I in 1 .. Next'Last =>
        Rank (I) <= Count
        and then Buddy_Blocks.Matches (Ledger (I), Buddy_Blocks.Listed, Size)
          = (Rank (I) > 0)
        and then (if Rank (I) > 0 then Sequence (Rank (I)) = I))
      and then (for all P in 1 .. Count =>
        Sequence (P) in 1 .. Next'Last
        and then Rank (Sequence (P)) = P
        and then Previous (Sequence (P)) = Sequence (P - 1)
        and then Next (Sequence (P)) =
          (if P = Count then 0 else Sequence (P + 1))));

   -- Establish the base case over a supplied ledger that has no blocks
   -- published at this order yet. Reserved/interior/live/other-order blocks
   -- need no initialized payload links to be excluded from this list.
   procedure Initialize_Empty
     (Next, Previous : in out References; Sequence, Rank : out References;
      Ledger : Descriptors; Count : out Natural; Size : Buddy_Blocks.Order) with
     Pre => Next'First = 0 and then Next'Last > 0
       and then Previous'First = 0 and then Previous'Last = Next'Last
       and then Sequence'First = 0 and then Sequence'Last = Next'Last
       and then Rank'First = 0 and then Rank'Last = Next'Last
       and then Ledger'First = 0 and then Ledger'Last = Next'Last
       and then (for all I in 1 .. Ledger'Last =>
         not Buddy_Blocks.Matches (Ledger (I), Buddy_Blocks.Listed, Size)),
     Post => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Count = 0
       and then (for all I in 1 .. Next'Last =>
         Next (I) = Next'Old (I) and then Previous (I) = Previous'Old (I));

   procedure Insert_Front
     (Next, Previous, Sequence, Rank : in out References;
      Ledger : in out Descriptors; Count : in out Natural;
      Size : Buddy_Blocks.Order; Item : ID) with
     Pre => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Count < Next'Last and then Item in 1 .. Next'Last
       and then Buddy_Blocks.Matches (Ledger (Item), Buddy_Blocks.Detached, Size),
     Post => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Count = Count'Old + 1 and then Sequence (1) = Item
       and then (for all I in 1 .. Next'Last =>
         (if I /= Item then Ledger (I) = Ledger'Old (I)))
       and then (for all P in 1 .. Count'Old => Sequence (P + 1) = Sequence'Old (P));

   -- Position is part of the Ghost witness, not a runtime traversal/index.
   procedure Remove_At
     (Next, Previous, Sequence, Rank : in out References;
      Ledger : in out Descriptors; Count : in out Natural;
      Size : Buddy_Blocks.Order; Position : Positive) with
     Pre => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Position <= Count,
     Post => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Count = Count'Old - 1
       and then Buddy_Blocks.Matches
         (Ledger (Sequence'Old (Position)), Buddy_Blocks.Detached, Size)
       and then (for all I in 1 .. Next'Last =>
         (if I /= Sequence'Old (Position) then Ledger (I) = Ledger'Old (I)))
       and then (for all P in 1 .. Count => Sequence (P) =
         (if P < Position then Sequence'Old (P) else Sequence'Old (P + 1)));

   procedure Prove_Membership
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      Item : ID) with
     Pre => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then Item in 1 .. Next'Last,
     Post => Buddy_Blocks.Matches (Ledger (Item), Buddy_Blocks.Listed, Size)
       = (for some P in 1 .. Count => Sequence (P) = Item);

   procedure Prove_Unique
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      P, Q : Positive) with
     Pre => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size)
       and then P <= Count and then Q <= Count and then P /= Q,
     Post => Sequence (P) /= Sequence (Q);

   -- Follow the actual Next edges, establishing that traversal returns to
   -- the sentinel after exactly Count non-sentinel nodes, not merely that
   -- the auxiliary sequence has Count entries.
   procedure Prove_Traversal
     (Next, Previous, Sequence, Rank : References;
      Ledger : Descriptors; Count : Natural; Size : Buddy_Blocks.Order;
      Traversed : out Natural; End_Node : out ID) with
     Pre => Valid (Next, Previous, Sequence, Rank, Ledger, Count, Size),
     Post => Traversed = Count and then End_Node = 0;
private
   package Splices is new Intrusive_List_Splices (ID);
end Buddy_List_Refinement;
