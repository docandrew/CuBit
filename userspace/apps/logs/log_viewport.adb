package body Log_Viewport with SPARK_Mode is
   procedure Fit (View : in out Viewport; Count : Row_Count; Rows : Page_Rows) is
   begin
      View.Count := Count;
      View.Rows := Rows;
      View.Selected := Natural'Min (View.Selected, Count);
      View.Top := Natural'Min (View.Top, Last_Top (Count, Rows));
   end Fit;

   procedure Place (View : in out Viewport; Count : Row_Count; Selected, Top : Row_Count) is
   begin
      View.Count := Count;
      View.Selected := Selected;
      View.Top := Natural'Min (Top, Last_Top (Count, View.Rows));
   end Place;

   procedure Scroll_To (View : in out Viewport; Top : Row_Count) is
      Target : constant Row_Count := Natural'Min (Top, Last_Top (View.Count, View.Rows));
   begin
      View.Follow := View.Follow and then Target = View.Top;
      View.Top := Target;
   end Scroll_To;

   procedure Scroll (View : in out Viewport; By : Row_Delta) is
   begin
      Scroll_To (View, Integer'Min (MAXIMUM_ROWS, Integer'Max (0, View.Top + By)));
   end Scroll;

   procedure Reveal (View : in out Viewport) is
   begin
      if View.Selected > 0 then
         if View.Selected <= View.Top then
            View.Top := View.Selected - 1;
         elsif View.Selected > View.Top + View.Rows then
            View.Top := View.Selected - View.Rows;
         end if;
      end if;
   end Reveal;

   procedure Move (View : in out Viewport; By : Row_Delta) is
   begin
      View.Follow := False;
      View.Selected := Natural'Max (1, Integer'Min (View.Count, View.Selected + By));
      Reveal (View);
   end Move;

   procedure Select_Row (View : in out Viewport; Row : Row_Index) is
   begin
      View.Follow := False;
      View.Selected := Row;
      Reveal (View);
   end Select_Row;

   procedure Show_Match (View : in out Viewport; Row : Row_Index) is
      --  Half a page above the match, where the rows allow.
      Above : constant Row_Count := (if Row - 1 > View.Rows / 2 then Row - 1 - View.Rows / 2 else 0);
   begin
      View.Follow := False;
      View.Selected := Row;
      if not Shows (View, Row) then
         View.Top := Natural'Min (Above, Last_Top (View.Count, View.Rows));
      end if;
   end Show_Match;

   procedure Follow_Newest (View : in out Viewport; Newest : Row_Count) is
   begin
      View.Follow := True;
      View.Selected := Newest;
      Reveal (View);
   end Follow_Newest;

   procedure Insert (View : in out Viewport; Row : Row_Index) is
   begin
      View.Count := View.Count + 1;
      if View.Selected >= Row then
         View.Selected := View.Selected + 1;
      end if;
      View.Top := Natural'Min ((if View.Top + 1 >= Row then View.Top + 1 else View.Top),
                               Last_Top (View.Count, View.Rows));
   end Insert;

   procedure Remove (View : in out Viewport; Row : Row_Index) is
   begin
      View.Count := View.Count - 1;
      if View.Selected >= Row then
         View.Selected := View.Selected - 1;
      end if;
      View.Top := Natural'Min ((if View.Top >= Row then View.Top - 1 else View.Top),
                               Last_Top (View.Count, View.Rows));
   end Remove;

   function Next_Match (Hits : Hit_Flags; Count : Row_Count; From : Row_Count; Forward : Boolean) return Row_Count
   is
   begin
      if Forward then
         for R in From + 1 .. Count loop
            if Hits (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in From + 1 .. R => not Hits (Q));
         end loop;
         for R in 1 .. From loop
            if Hits (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in From + 1 .. Count => not Hits (Q));
            pragma Loop_Invariant (for all Q in 1 .. R => not Hits (Q));
         end loop;
      else
         for R in reverse 1 .. From - 1 loop
            if Hits (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in R .. From - 1 => not Hits (Q));
         end loop;
         for R in reverse Natural'Max (1, From) .. Count loop
            if Hits (R) then
               return R;
            end if;
            pragma Loop_Invariant (for all Q in 1 .. From - 1 => not Hits (Q));
            pragma Loop_Invariant (for all Q in R .. Count => not Hits (Q));
         end loop;
      end if;
      return 0;
   end Next_Match;

   function Match_Total (Hits : Hit_Flags; Count : Row_Count) return Row_Count is
      Total : Row_Count := 0;
   begin
      for R in 1 .. Count loop
         if Hits (R) then
            Total := Total + 1;
         end if;
         pragma Loop_Invariant (Total <= R);
      end loop;
      return Total;
   end Match_Total;

   function Match_Rank (Hits : Hit_Flags; Row : Row_Index) return Row_Count is
      Rank : Row_Count := 0;
   begin
      for R in 1 .. Row loop
         if Hits (R) then
            Rank := Rank + 1;
         end if;
         pragma Loop_Invariant (Rank <= R and then (if Hits (R) then Rank > 0));
      end loop;
      return Rank;
   end Match_Rank;
end Log_Viewport;
