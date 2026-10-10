package body Client_Popup_Layout with SPARK_Mode is
   --  Start, length along one axis: Preferred where it fits, else Flipped,
   --  else pushed back inside.
   function Fit (Preferred, Flipped : Integer; Length : Coordinate; First : Coordinate; Span : Coordinate)
     return Coordinate
     with Pre => Length <= Span and then First <= MAXIMUM_COORDINATE - Span
                 and then Preferred in -MAXIMUM_COORDINATE .. 2 * MAXIMUM_COORDINATE
                 and then Flipped in -MAXIMUM_COORDINATE .. 2 * MAXIMUM_COORDINATE,
          Post => Fit'Result >= First and then Fit'Result - First + Length <= Span
   is
      Last_Start : constant Coordinate := First + (Span - Length);
   begin
      if Preferred >= First and then Preferred <= Last_Start then
         return Preferred;
      elsif Flipped >= First and then Flipped <= Last_Start then
         return Flipped;
      elsif Preferred < First then
         return First;
      else
         return Last_Start;
      end if;
   end Fit;

   function Place (Anchor_X, Anchor_Y : Coordinate; W, H : Coordinate; Area : Box) return Box is
      Width : constant Coordinate := Natural'Min (W, Area.W);
      Height : constant Coordinate := Natural'Min (H, Area.H);
   begin
      if Area.X > MAXIMUM_COORDINATE - Area.W or else Area.Y > MAXIMUM_COORDINATE - Area.H then
         return (Area.X, Area.Y, Natural'Min (W, Area.W), Natural'Min (H, Area.H));
      end if;
      return (Fit (Anchor_X, Anchor_X - Width, Width, Area.X, Area.W),
              Fit (Anchor_Y, Anchor_Y - Height, Height, Area.Y, Area.H), Width, Height);
   end Place;

   function Place_Beside (Parent : Box; Row_Y : Coordinate; W, H : Coordinate; Area : Box) return Box is
      Width : constant Coordinate := Natural'Min (W, Area.W);
      Height : constant Coordinate := Natural'Min (H, Area.H);
   begin
      if Area.X > MAXIMUM_COORDINATE - Area.W or else Area.Y > MAXIMUM_COORDINATE - Area.H then
         return (Area.X, Area.Y, Width, Height);
      end if;
      return (Fit (Parent.X + Parent.W, Parent.X - Width, Width, Area.X, Area.W),
              Fit (Row_Y, Row_Y - Height, Height, Area.Y, Area.H), Width, Height);
   end Place_Beside;

   function Next (Rows : Selectable_Rows; Count : Row_Count; From : Row_Count; Forward : Boolean) return Row_Count is
   begin
      if Forward then
         for R in From + 1 .. Count loop
            if Rows (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in From + 1 .. R => not Rows (Q));
         end loop;
         for R in 1 .. From loop
            if Rows (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in From + 1 .. Count => not Rows (Q));
            pragma Loop_Invariant (for all Q in 1 .. R => not Rows (Q));
         end loop;
      else
         for R in reverse 1 .. (if From = 0 then Count else From - 1) loop
            if Rows (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in R .. (if From = 0 then Count else From - 1) => not Rows (Q));
         end loop;
         for R in reverse (if From = 0 then Count + 1 else From) .. Count loop
            if Rows (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in 1 .. (if From = 0 then Count else From - 1) => not Rows (Q));
            pragma Loop_Invariant (for all Q in R .. Count => not Rows (Q));
         end loop;
      end if;
      return 0;
   end Next;
end Client_Popup_Layout;
