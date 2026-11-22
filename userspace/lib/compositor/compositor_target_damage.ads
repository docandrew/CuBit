with Compositor_Damage;
with Compositor_Pool;
-- Per-output, three-target repaint history. Changes mark all targets dirty;
-- changes arriving during a paint are separately retained for its next use.
-- No pixel copies, allocations, elapsed-time assumptions, or GPU calls.
package Compositor_Target_Damage with SPARK_Mode, Pure is
   package D renames Compositor_Damage;
   package P renames Compositor_Pool;
   use type D.State, D.Box, P.Slot;
   subtype Extent is Positive range 1 .. 65535;
   type State is private;
   function Valid (S : State) return Boolean;
   function Bounds (S : State) return D.Box;
   function Active (S : State) return P.Slot;
   function Faulted (S : State) return Boolean;
   function Initialized (S : State; Target : P.Live_Slot) return Boolean;
   function Pending (S : State; Target : P.Live_Slot) return D.State;
   function Painting (S : State) return D.State;
   function Later (S : State) return D.State;
   function Open (Width, Height : Extent) return State
     with Post => Valid (Open'Result) and Active (Open'Result) = 0 and not Faulted (Open'Result) and
       (for all I in P.Live_Slot => not Initialized (Open'Result, I)) and
       (for all I in P.Live_Slot => D.Covers (Pending (Open'Result, I), Bounds (Open'Result)));
   procedure Change (S : in out State; Region : D.Box)
     with Pre => Valid (S) and D.Valid (Region) and D.Contains (Bounds (S), Region),
       Post => Valid (S) and
         (for all I in P.Live_Slot => Initialized (S, I) = Initialized (S'Old, I)) and Active (S) = Active (S'Old) and Faulted (S) = Faulted (S'Old) and
         Bounds (S) = Bounds (S'Old) and Painting (S) = Painting (S'Old) and
         (for all I in P.Live_Slot => D.Covers (Pending (S, I), Region)) and
         (for all I in P.Live_Slot =>
            (for all J in 1 .. D.Count (Pending (S'Old, I)) =>
               D.Covers (Pending (S, I), D.Item (Pending (S'Old, I), J)))) and
         (if Active (S) /= 0 then D.Covers (Later (S), Region)) and
         (for all J in 1 .. D.Count (Later (S'Old)) =>
            D.Covers (Later (S), D.Item (Later (S'Old), J)));
   procedure Begin_Paint (S : in out State; Target : P.Live_Slot)
     with Pre => Valid (S) and not Faulted (S) and Active (S) = 0,
       Post => Valid (S) and
         (for all I in P.Live_Slot => Initialized (S, I) = Initialized (S'Old, I)) and not Faulted (S) and Active (S) = Target and
         Bounds (S) = Bounds (S'Old) and Painting (S) = Pending (S'Old, Target) and
         D.Count (Later (S)) = 0 and
         (for all I in P.Live_Slot => Pending (S, I) = Pending (S'Old, I));
   type Completion is (Completed, Cancelled, Unknown);
   procedure Finish (S : in out State; Result : Completion)
     with Pre => Valid (S) and not Faulted (S) and Active (S) /= 0,
       Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
         (for all I in P.Live_Slot => Initialized (S, I) =
            (Initialized (S'Old, I) or (Result = Completed and I = Active (S'Old)))) and
         (if Result = Unknown then Faulted (S) and Active (S) = Active (S'Old) and
            Painting (S) = Painting (S'Old) and Later (S) = Later (S'Old)
          else not Faulted (S) and Active (S) = 0 and D.Count (Painting (S)) = 0 and D.Count (Later (S)) = 0) and
         (for all I in P.Live_Slot => Pending (S, I) =
            (if Result = Completed and I = Active (S'Old) then Later (S'Old) else Pending (S'Old, I)));
private
   type Regions is array (P.Live_Slot) of D.State;
   type Initializations is array (P.Live_Slot) of Boolean;
   type State is record
      Limit : D.Box := (0, 0, 1, 1);
      Dirty : Regions;
      Plan, After_Paint : D.State;
      Writer : P.Slot := 0;
      Failed : Boolean := False;
      Warm : Initializations := (others => False);
   end record;
   function Initialized (S : State; Target : P.Live_Slot) return Boolean is (S.Warm (Target));
   function Bounds (S : State) return D.Box is (S.Limit);
   function Active (S : State) return P.Slot is (S.Writer);
   function Faulted (S : State) return Boolean is (S.Failed);
   function Pending (S : State; Target : P.Live_Slot) return D.State is (S.Dirty (Target));
   function Painting (S : State) return D.State is (S.Plan);
   function Later (S : State) return D.State is (S.After_Paint);
   function Within (R : D.State; B : D.Box) return Boolean is
     (if D.Count (R) > 0 then D.Contains (B, D.Bounds (R)));
   function Valid (S : State) return Boolean is
     (D.Valid (S.Limit) and
      (for all I in P.Live_Slot => D.Valid (S.Dirty (I))) and
      D.Valid (S.Plan) and D.Valid (S.After_Paint) and
      (for all I in P.Live_Slot => Within (S.Dirty (I), S.Limit)) and
      Within (S.Plan, S.Limit) and Within (S.After_Paint, S.Limit));
end Compositor_Target_Damage;
