--  Bounded sparse damage. Overlap or exhaustion collapses to the conservative
--  bounding box; separate regions never overlap and never lose dirty pixels.
package Compositor_Damage with SPARK_Mode, Pure is
   Capacity : constant := 8;
   subtype Length is Natural range 0 .. Capacity;
   subtype Index is Positive range 1 .. Capacity;
   type Box is record
      Left, Top, Right, Bottom : Natural := 0;
   end record;
   function Valid (R : Box) return Boolean is
     (R.Left < R.Right and R.Top < R.Bottom);
   function Contains (Outer, Inner : Box) return Boolean is
     (Outer.Left <= Inner.Left and Outer.Top <= Inner.Top and
      Outer.Right >= Inner.Right and Outer.Bottom >= Inner.Bottom);
   function Overlaps (A, B : Box) return Boolean is
     (A.Left < B.Right and B.Left < A.Right and
      A.Top < B.Bottom and B.Top < A.Bottom);
   function Envelope (A, B : Box) return Box is
     (Natural'Min (A.Left, B.Left), Natural'Min (A.Top, B.Top),
      Natural'Max (A.Right, B.Right), Natural'Max (A.Bottom, B.Bottom));
   type State is private;
   function Count (S : State) return Length;
   function Bounds (S : State) return Box;
   function Item (S : State; I : Index) return Box
     with Pre => I <= Count (S);
   function Valid (S : State) return Boolean;
   function Covers (S : State; R : Box) return Boolean;
   procedure Clear (S : out State)
     with Post => Valid (S) and Count (S) = 0;
   procedure Add (S : in out State; R : Box)
     with Pre => Valid (S) and Valid (R),
       Post => Valid (S) and Count (S) > 0 and Covers (S, R) and
         (for all I in 1 .. Count (S'Old) => Covers (S, Item (S'Old, I))) and
         Bounds (S) = (if Count (S'Old) = 0 then R
                       else Envelope (Bounds (S'Old), R)) and
         (if Count (S'Old) > 0 then Contains (Bounds (S), Bounds (S'Old)));
private
   type Boxes is array (Index) of Box;
   type State is record
      Used : Length := 0;
      Region : Boxes := (others => (others => 0));
      Extent : Box;
   end record;
   function Count (S : State) return Length is (S.Used);
   function Bounds (S : State) return Box is (S.Extent);
   function Item (S : State; I : Index) return Box is (S.Region (I));
   function Valid (S : State) return Boolean is
     ((if S.Used > 0 then Valid (S.Extent)) and then
      (for all I in 1 .. S.Used =>
         Valid (S.Region (I)) and Contains (S.Extent, S.Region (I))) and then
      (for all I in 1 .. S.Used =>
         (for all J in 1 .. S.Used =>
            (if I /= J then not Overlaps (S.Region (I), S.Region (J))))));
   function Covers (S : State; R : Box) return Boolean is
     (for some I in 1 .. S.Used => Contains (S.Region (I), R));
end Compositor_Damage;
