pragma Ada_2022;

package body Dash_Grid is
   use Dash_Model;

   function Sizes_Of (D : Dashboard) return Size_List is
      Result : Size_List := [others => <>];
   begin
      for P in 1 .. D.Panels_Used loop
         Result (P) := (D.Panels (P).Width, D.Panels (P).Height);
      end loop;
      return Result;
   end Sizes_Of;

   procedure Pack (Sizes : Size_List; Count : Panel_Count; Placed : out Placements; Rows : out Row_Count) is
      type Skyline is array (Column_Index) of Row_Count;
      Heights : Skyline := [others => 0];
   begin
      Placed := [others => <>];
      Rows := 0;
      for P in 1 .. Count loop
         declare
            W : constant Grid_Width := Sizes (P).Width;
            Best_Column : Column_Index := 0;
            Best_Row : Natural := Natural'Last;
         begin
            for Column in 0 .. Grid_Columns - W loop
               declare
                  Row : Row_Count := 0;
               begin
                  for K in Column .. Column + W - 1 loop
                     Row := Row_Count'Max (Row, Heights (K));
                  end loop;
                  if Row < Best_Row then
                     Best_Row := Row;
                     Best_Column := Column;
                  end if;
               end;
            end loop;
            Placed (P) := (Best_Column, Best_Row);
            for K in Best_Column .. Best_Column + W - 1 loop
               Heights (K) := Best_Row + Sizes (P).Height;
            end loop;
            Rows := Row_Count'Max (Rows, Best_Row + Sizes (P).Height);
         end;
      end loop;
   end Pack;

   procedure First_Free
     (D : Dashboard; Width : Grid_Width; Height : Grid_Height; At_Index : out Panel_Index; Where : out Placement)
   is
      Sizes : constant Size_List := Sizes_Of (D);
      Before : Placements;
      Rows : Row_Count;
      Found : Boolean := False;
   begin
      Pack (Sizes, D.Panels_Used, Before, Rows);
      At_Index := D.Panels_Used + 1;
      Where := (0, 0);
      for K in 1 .. D.Panels_Used + 1 loop
         declare
            Trial : Size_List := Sizes;
            After : Placements;
            Kept : Boolean := True;
         begin
            Trial (K + 1 .. D.Panels_Used + 1) := Sizes (K .. D.Panels_Used);
            Trial (K) := (Width, Height);
            Pack (Trial, D.Panels_Used + 1, After, Rows);
            for P in 1 .. D.Panels_Used loop
               if After ((if P < K then P else P + 1)) /= Before (P) then
                  Kept := False;
               end if;
            end loop;
            if Kept and then (not Found or else After (K).Row < Where.Row
                              or else (After (K).Row = Where.Row and then After (K).Column < Where.Column))
            then
               Found := True;
               At_Index := K;
               Where := After (K);
            end if;
         end;
      end loop;
   end First_Free;
end Dash_Grid;
