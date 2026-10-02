package body Client_Frame_Damage with SPARK_Mode is
   function Merge (A, B : Box) return Box
     with Pre => (A = Empty or Nonempty (A)) and Nonempty (B),
       Post => Nonempty (Merge'Result) and Contains (Merge'Result, A) and
         Contains (Merge'Result, B) and
         Merge'Result = (if A = Empty then B else
           (Coordinate'Min (A.Left, B.Left), Coordinate'Min (A.Top, B.Top),
            Coordinate'Max (A.Right, B.Right), Coordinate'Max (A.Bottom, B.Bottom)));
   function Merge (A, B : Box) return Box is
     (if A = Empty then B else
        (Coordinate'Min (A.Left, B.Left), Coordinate'Min (A.Top, B.Top),
         Coordinate'Max (A.Right, B.Right), Coordinate'Max (A.Bottom, B.Bottom)));
   function Open (Extent : Box) return State is
     ((Extent, Extent, (others => Extent)));
   procedure Invalidate (S : in out State; Area : Box) is
   begin
      S.Pending := Merge (S.Pending, Area);
      S.Debt (1) := Merge (S.Debt (1), Area);
      S.Debt (2) := Merge (S.Debt (2), Area);
   end Invalidate;
   procedure Published (S : in out State; B : Slot; Rendered : Box) is
      pragma Unreferenced (Rendered);
   begin
      S.Debt (B) := Empty;
      S.Pending := Empty;
   end Published;
end Client_Frame_Damage;
