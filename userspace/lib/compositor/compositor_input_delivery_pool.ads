with Compositor_Input_Delivery;

-- A fixed service-lifetime pool, deliberately independent of channel storage.
-- Closed channels do not own or reset this state. One pending loan per
-- authenticated owner/surface prevents a retry from consuming another slot.
generic
   Maximum : Positive;
   with package Delivery is new Compositor_Input_Delivery (<>);
package Compositor_Input_Delivery_Pool with SPARK_Mode is
   package W renames Delivery.W;
   package GR renames Delivery.GR;
   use type W.Word;
   use type GR.Reference;
   use type Delivery.Outcome;
   subtype Slot is Positive range 1 .. Maximum;
   type State is private;
   function Pending (S : State; I : Slot) return Boolean;
   function Owner (S : State; I : Slot) return W.Word;
   function Surface (S : State; I : Slot) return W.Word;
   function Reference (S : State; I : Slot) return GR.Reference;
   function Cursor (S : State) return Slot;
   function Next (I : Slot) return Slot is (if I = Slot'Last then Slot'First else I + 1);

   procedure Publish
     (S : in out State; Sender, Target : W.Identity;
      Grant : GR.Reference; Payload : W.Snapshot_Words;
      Result : out Delivery.Outcome)
   with Post => Cursor (S) = Cursor (S'Old) and then
     (for all I in Slot =>
       (if Pending (S'Old, I) then Pending (S, I) and then
          Owner (S, I) = Owner (S'Old, I) and then
          Surface (S, I) = Surface (S'Old, I) and then
          Reference (S, I) = Reference (S'Old, I))) and then
     (if (for some I in Slot => Pending (S'Old, I) and then
          Owner (S'Old, I) = Sender and then Surface (S'Old, I) = Target)
      then Result = Delivery.Busy and S = S'Old);

   -- Examine exactly one slot per maintenance tick, attempt at most one return,
   -- and advance regardless of success. A failing loan cannot starve another.
   procedure Poll (S : in out State)
   with Post => Cursor (S) = Next (Cursor (S'Old)) and then
     (for all I in Slot =>
        Owner (S, I) = Owner (S'Old, I) and then
        Surface (S, I) = Surface (S'Old, I) and then
        Reference (S, I) = Reference (S'Old, I) and then
        (if I /= Cursor (S'Old) then Pending (S, I) = Pending (S'Old, I))
        and then (if not Pending (S'Old, I) then not Pending (S, I)));
private
   type Entry_State is record
      Owner, Surface : W.Word := 0;
      Loan : Delivery.State;
   end record;
   type Entries is array (Slot) of Entry_State;
   type State is record
      Items : Entries;
      Current : Slot := Slot'First;
   end record;
   function Pending (S : State; I : Slot) return Boolean is (Delivery.Pending (S.Items (I).Loan));
   function Owner (S : State; I : Slot) return W.Word is (S.Items (I).Owner);
   function Surface (S : State; I : Slot) return W.Word is (S.Items (I).Surface);
   function Reference (S : State; I : Slot) return GR.Reference is (Delivery.Reference (S.Items (I).Loan));
   function Cursor (S : State) return Slot is (S.Current);
end Compositor_Input_Delivery_Pool;
