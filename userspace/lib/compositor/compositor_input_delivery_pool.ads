with Compositor_Input_Delivery;

-- A service-lifetime pool, deliberately independent of channel storage.
-- Closed channels do not own or reset this state. One pending loan per
-- authenticated owner/surface prevents a retry from consuming another slot.
-- Its owner sizes it (Maximum) with the surface table and grows it by
-- copying into a larger pool (Grow); a pending loan keeps its slot.
generic
   with package Delivery is new Compositor_Input_Delivery (<>);
package Compositor_Input_Delivery_Pool with SPARK_Mode is
   package W renames Delivery.W;
   package GR renames Delivery.GR;
   use type W.Word;
   use type GR.Reference;
   use type Delivery.Outcome;
   subtype Slot is Positive;
   type State (Maximum : Slot) is private;
   function Pending (S : State; I : Slot) return Boolean with Pre => I <= S.Maximum;
   function Owner (S : State; I : Slot) return W.Word with Pre => I <= S.Maximum;
   function Surface (S : State; I : Slot) return W.Word with Pre => I <= S.Maximum;
   function Reference (S : State; I : Slot) return GR.Reference with Pre => I <= S.Maximum;
   function Cursor (S : State) return Slot with Post => Cursor'Result <= S.Maximum;
   function Next (S : State; I : Slot) return Slot is (if I >= S.Maximum then 1 else I + 1)
     with Post => Next'Result <= S.Maximum;

   procedure Publish
     (S : in out State; Sender, Target : W.Identity;
      Grant : GR.Reference; Payload : W.Snapshot_Words;
      Result : out Delivery.Outcome)
   with Post => Cursor (S) = Cursor (S'Old) and then
     (for all I in 1 .. S.Maximum =>
       (if Pending (S'Old, I) then Pending (S, I) and then
          Owner (S, I) = Owner (S'Old, I) and then
          Surface (S, I) = Surface (S'Old, I) and then
          Reference (S, I) = Reference (S'Old, I))) and then
     (if (for some I in 1 .. S.Maximum => Pending (S'Old, I) and then
          Owner (S'Old, I) = Sender and then Surface (S'Old, I) = Target)
      then Result = Delivery.Busy and S = S'Old);

   -- Examine exactly one slot per maintenance tick, attempt at most one return,
   -- and advance regardless of success. A failing loan cannot starve another.
   procedure Poll (S : in out State)
   with Post => Cursor (S) = Next (S, Cursor (S'Old)) and then
     (for all I in 1 .. S.Maximum =>
        Owner (S, I) = Owner (S'Old, I) and then
        Surface (S, I) = Surface (S'Old, I) and then
        Reference (S, I) = Reference (S'Old, I) and then
        (if I /= Cursor (S'Old) then Pending (S, I) = Pending (S'Old, I))
        and then (if not Pending (S'Old, I) then not Pending (S, I)));

   -- A larger pool with the same loans, at the same slots, and the same
   -- cursor; the added slots hold none.
   procedure Grow (From : State; Into : out State)
   with Pre => Into.Maximum >= From.Maximum,
     Post => Cursor (Into) = Cursor (From) and then
       (for all I in 1 .. From.Maximum =>
          Pending (Into, I) = Pending (From, I) and then
          Owner (Into, I) = Owner (From, I) and then
          Surface (Into, I) = Surface (From, I) and then
          Reference (Into, I) = Reference (From, I)) and then
       (for all I in 1 .. Into.Maximum => (if I > From.Maximum then not Pending (Into, I)));
private
   type Entry_State is record
      Owner, Surface : W.Word := 0;
      Loan : Delivery.State;
   end record;
   type Entries is array (Slot range <>) of Entry_State;
   type State (Maximum : Slot) is record
      Items : Entries (1 .. Maximum);
      Current : Slot := 1;
   end record
     with Dynamic_Predicate => Current <= Maximum;
   function Pending (S : State; I : Slot) return Boolean is (Delivery.Pending (S.Items (I).Loan));
   function Owner (S : State; I : Slot) return W.Word is (S.Items (I).Owner);
   function Surface (S : State; I : Slot) return W.Word is (S.Items (I).Surface);
   function Reference (S : State; I : Slot) return GR.Reference is (Delivery.Reference (S.Items (I).Loan));
   function Cursor (S : State) return Slot is (S.Current);
end Compositor_Input_Delivery_Pool;
