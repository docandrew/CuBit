-- Serialized producer repaint debt. This policy never grants write ownership:
-- obtain that separately from Client_Frame_Buffer before touching any pixels.
package Client_Frame_Damage with SPARK_Mode, Pure is
   subtype Coordinate is Natural range 0 .. 65_535;
   type Box is record
      Left, Top, Right, Bottom : Coordinate := 0;
   end record;
   Empty : constant Box := (others => 0);
   function Nonempty (R : Box) return Boolean is
     (R.Left < R.Right and R.Top < R.Bottom);
   function Contains (Outer, Inner : Box) return Boolean is
     (Inner = Empty or else
        (Outer.Left <= Inner.Left and Outer.Top <= Inner.Top and
         Outer.Right >= Inner.Right and Outer.Bottom >= Inner.Bottom));
   subtype Slot is Positive range 1 .. 2;
   type State is private;
   function Bounds (S : State) return Box;
   function Required (S : State; B : Slot) return Box;
   function Publication_Damage (S : State) return Box;
   function Valid (S : State) return Boolean;
   function Open (Extent : Box) return State
     with Pre => Nonempty (Extent), Post => Valid (Open'Result) and
       Bounds (Open'Result) = Extent and
       Publication_Damage (Open'Result) = Extent and
       (for all B in Slot => Required (Open'Result, B) = Extent);
   -- Call for every retained-state change, including while a buffer is busy.
   procedure Invalidate (S : in out State; Area : Box)
     with Pre => Valid (S) and Nonempty (Area) and Contains (Bounds (S), Area),
       Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
         Contains (Publication_Damage (S), Area) and
         Contains (Publication_Damage (S), Publication_Damage (S'Old)) and
         (for all B in Slot => Contains (Required (S, B), Area) and
            Contains (Required (S, B), Required (S'Old, B)));
   -- Only after successful publication of a buffer repainted from current
   -- retained state over Rendered. No intervening state changes are permitted.
   -- On render/publication failure leave debt intact. Repair is NOT additional
   -- scene damage: publish Publication_Damage, not Required.
   procedure Published (S : in out State; B : Slot; Rendered : Box)
     with Pre => Valid (S) and Nonempty (Rendered) and
       Contains (Bounds (S), Rendered) and Contains (Rendered, Required (S, B)),
       Post => Valid (S) and Bounds (S) = Bounds (S'Old) and
         Required (S, B) = Empty and Publication_Damage (S) = Empty and
         (for all J in Slot => (if J /= B then Required (S, J) = Required (S'Old, J)));
private
   type Debts is array (Slot) of Box;
   type State is record
      Extent : Box;
      Pending : Box;
      Debt : Debts;
   end record;
   function Bounds (S : State) return Box is (S.Extent);
   function Required (S : State; B : Slot) return Box is (S.Debt (B));
   function Publication_Damage (S : State) return Box is (S.Pending);
   function In_Bounds (R, Extent : Box) return Boolean is
     (R = Empty or else (Nonempty (R) and Contains (Extent, R)));
   function Valid (S : State) return Boolean is
     (Nonempty (S.Extent) and In_Bounds (S.Pending, S.Extent) and
      (for all B in Slot => In_Bounds (S.Debt (B), S.Extent) and
         Contains (S.Debt (B), S.Pending)));
end Client_Frame_Damage;
