with Compositor_Damage;
with Compositor_Pool;
package Compositor_Repaint with SPARK_Mode, Pure is
   package D renames Compositor_Damage;
   use type D.State, D.Box;
   subtype Slot is Compositor_Pool.Live_Slot;
   type State is private;
   function Bounds (S : State) return D.Box;
   function Pending (S : State; B : Slot) return D.State;
   function Valid (S : State) return Boolean;
   function Open (Extent : D.Box) return State
     with Pre => D.Valid (Extent), Post => Valid (Open'Result) and
       Bounds (Open'Result) = Extent and
       (for all B in Slot => D.Covers (Pending (Open'Result, B), Extent));
   procedure Invalidate (S : in out State; Area : D.Box)
     with Pre => Valid (S) and D.Valid (Area) and D.Contains (Bounds (S), Area),
       Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
         (for all B in Slot => D.Covers (Pending (S, B), Area) and
           (for all I in 1 .. D.Count (Pending (S'Old, B)) =>
             D.Covers (Pending (S, B), D.Item (Pending (S'Old, B), I))));
   -- Call only for a pool-authorized writer, immediately before rendering.
   -- Clear at take, never at completion: later invalidations must survive.
   procedure Take (S : in out State; B : Slot; Areas : out D.State)
     with Pre => Valid (S), Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
       Areas = Pending (S'Old, B) and D.Count (Pending (S, B)) = 0 and
       (for all J in Slot => (if J /= B then Pending (S, J) = Pending (S'Old, J)));
   procedure Failed_Render (S : in out State; B : Slot)
     with Pre => Valid (S), Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
       D.Covers (Pending (S, B), Bounds (S)) and
       (for all J in Slot => (if J /= B then Pending (S, J) = Pending (S'Old, J)));
private
   type Queue_Array is array (Slot) of D.State;
   type State is record
      Extent : D.Box;
      Queues : Queue_Array;
   end record;
   function Bounds (S : State) return D.Box is (S.Extent);
   function Pending (S : State; B : Slot) return D.State is (S.Queues (B));
   function Queue_Valid (Q : D.State; Extent : D.Box) return Boolean is
     (D.Valid (Q) and then (if D.Count (Q) > 0 then D.Contains (Extent, D.Bounds (Q))));
   function Valid (S : State) return Boolean is
     (D.Valid (S.Extent) and (for all B in Slot => Queue_Valid (S.Queues (B), S.Extent)));
end Compositor_Repaint;
