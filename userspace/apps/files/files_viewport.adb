package body Files_Viewport with SPARK_Mode is
   procedure Fit (View : in out Viewport; Rows : Page_Rows) is
   begin
      View.Rows := Rows;
      View.Top := Natural'Min (View.Top, Last_Top (View.Count, Rows));
   end Fit;

   procedure Reveal (View : in out Viewport) is
   begin
      if View.Cursor > 0 then
         if View.Cursor <= View.Top then
            View.Top := View.Cursor - 1;
         elsif View.Cursor - View.Top > View.Rows then
            View.Top := View.Cursor - View.Rows;
         end if;
      end if;
   end Reveal;

   procedure Place (View : in out Viewport; Count : Row_Count; Cursor : Row_Count) is
      --  Where the cursor was on screen, if it was.
      Offset : constant Row_Count :=
        (if View.Cursor > View.Top and then View.Cursor - View.Top <= View.Rows
         then View.Cursor - View.Top - 1 else 0);
      Target : constant Row_Count := (if Count = 0 then 0 else Natural'Max (1, Natural'Min (Cursor, Count)));
   begin
      View.Count := Count;
      View.Cursor := Target;
      View.Top := Natural'Min ((if Target > Offset + 1 then Target - Offset - 1 else 0),
                               Last_Top (Count, View.Rows));
      Reveal (View);
   end Place;

   procedure Scroll_To (View : in out Viewport; Top : Row_Count) is
   begin
      View.Top := Natural'Min (Top, Last_Top (View.Count, View.Rows));
   end Scroll_To;

   procedure Scroll (View : in out Viewport; By : Row_Delta) is
   begin
      Scroll_To (View, Integer'Min (Row_Count'Last, Integer'Max (0, View.Top + By)));
   end Scroll;

   procedure Move (View : in out Viewport; By : Row_Delta) is
   begin
      if View.Count > 0 then
         View.Cursor := Integer'Max (1, Integer'Min (View.Count, View.Cursor + By));
      end if;
      Reveal (View);
   end Move;

   procedure Go (View : in out Viewport; Row : Row_Index) is
   begin
      View.Cursor := Row;
      Reveal (View);
   end Go;
end Files_Viewport;
