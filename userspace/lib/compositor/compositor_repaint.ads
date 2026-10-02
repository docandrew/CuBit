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
   -- Dormant targets need no speculative maintenance. A guaranteed full
   -- redraw replaces all stale pixels; partial work must first repair them.
   function Preparation_Required
     (S : State; B : Slot; Work_Pending, Full_Repaint : Boolean) return Boolean
     with Pre => Valid (S),
       Post => Preparation_Required'Result =
         (Work_Pending and not Full_Repaint and D.Count (Pending (S, B)) > 0);
   -- Four outside strips plus the cursor intersection: fixed storage, no queue.
   -- Caller must redraw Upcoming before submitting this writer, or quarantine.
   subtype Repair_Index is Positive range 1 .. 5;
   type Repair_Plan is array (Repair_Index) of D.Box;
   function Inside (Area : D.Box; X, Y : Natural) return Boolean is
     (X >= Area.Left and X < Area.Right and Y >= Area.Top and Y < Area.Bottom);
   function Covers_Point (Plan : Repair_Plan; X, Y : Natural) return Boolean is
     (for some Area of Plan => Inside (Area, X, Y));
   function Before_Draw
     (Area, Upcoming, Cursor : D.Box; Drawing_Pending : Boolean) return Repair_Plan
     with Pre => D.Valid (Area),
       Post =>
         (for all R of Before_Draw'Result => (if D.Valid (R) then D.Contains (Area, R))) and
         (for all I in Repair_Index => (for all J in Repair_Index =>
           (if I /= J and D.Valid (Before_Draw'Result (I)) and D.Valid (Before_Draw'Result (J))
            then not D.Overlaps (Before_Draw'Result (I), Before_Draw'Result (J)))));
   -- Arbitrary-point proof avoids unbounded run-time quantified checks.
   procedure Coverage_Lemma
     (Area, Upcoming, Cursor : D.Box; Drawing_Pending : Boolean; X, Y : Natural)
     with Ghost, Pre => D.Valid (Area),
       Post => Covers_Point (Before_Draw (Area, Upcoming, Cursor, Drawing_Pending), X, Y) =
         (Inside (Area, X, Y) and
           (not Drawing_Pending or not Inside (Upcoming, X, Y) or Inside (Cursor, X, Y)));
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
   function Intersection (A, B : D.Box) return D.Box is
     (if D.Valid (A) and then D.Valid (B) and then D.Overlaps (A, B)
      then (Natural'Max (A.Left, B.Left), Natural'Max (A.Top, B.Top),
            Natural'Min (A.Right, B.Right), Natural'Min (A.Bottom, B.Bottom))
      else (0, 0, 0, 0));
   function Before_Draw
     (Area, Upcoming, Cursor : D.Box; Drawing_Pending : Boolean) return Repair_Plan is
     (declare K : constant D.Box := Intersection (Area, Upcoming); begin
        (if not Drawing_Pending or else not D.Valid (K)
         then [Area, others => (0, 0, 0, 0)]
         else [(Area.Left, Area.Top, Area.Right, K.Top),
               (Area.Left, K.Bottom, Area.Right, Area.Bottom),
               (Area.Left, K.Top, K.Left, K.Bottom),
               (K.Right, K.Top, Area.Right, K.Bottom),
               Intersection (K, Cursor)]));
   type Queue_Array is array (Slot) of D.State;
   type State is record
      Extent : D.Box;
      Queues : Queue_Array;
   end record;
   function Bounds (S : State) return D.Box is (S.Extent);
   function Pending (S : State; B : Slot) return D.State is (S.Queues (B));
   function Preparation_Required
     (S : State; B : Slot; Work_Pending, Full_Repaint : Boolean) return Boolean is
     (Work_Pending and not Full_Repaint and D.Count (S.Queues (B)) > 0);
   function Queue_Valid (Q : D.State; Extent : D.Box) return Boolean is
     (D.Valid (Q) and then (if D.Count (Q) > 0 then D.Contains (Extent, D.Bounds (Q))));
   function Valid (S : State) return Boolean is
     (D.Valid (S.Extent) and (for all B in Slot => Queue_Valid (S.Queues (B), S.Extent)));
end Compositor_Repaint;
