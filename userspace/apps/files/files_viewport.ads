with Files_Limits; use Files_Limits;

--  Where a pane looks and which row its cursor is on (docs/files-app.md).
--  The view scrolls freely of the cursor (wheel, scrollbar); moving the
--  cursor brings it into view with the least scrolling. When the rows change
--  under it (a new order, a filter, more entries), the cursor follows its
--  entry and keeps its place on screen where the rows allow (Place). Pure
--  policy, proved (tests/files-app/files_proof.gpr).
package Files_Viewport with SPARK_Mode, Pure is
   subtype Page_Rows is Row_Index;

   type Viewport is record
      Count : Row_Count := 0;
      --  The cursor's row, 0 only when there are no rows.
      Cursor : Row_Count := 0;
      --  Rows above the first one drawn.
      Top : Row_Count := 0;
      Rows : Page_Rows := 1;
   end record;

   --  The furthest the view scrolls: the last row at the bottom.
   function Last_Top (Count : Row_Count; Rows : Page_Rows) return Row_Count is
     (if Count > Rows then Count - Rows else 0);
   function Valid (View : Viewport) return Boolean is
     (View.Cursor <= View.Count and then (View.Count = 0) = (View.Cursor = 0)
      and then View.Top <= Last_Top (View.Count, View.Rows));
   function Shows (View : Viewport; Row : Row_Index) return Boolean is
     (Row > View.Top and then Row - View.Top <= View.Rows);
   function Cursor_Shown (View : Viewport) return Boolean is
     (View.Cursor = 0 or else Shows (View, View.Cursor));

   --  The table changed height: the cursor stays; the view stays where the
   --  rows allow.
   procedure Fit (View : in out Viewport; Rows : Page_Rows)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Rows = Rows and then View.Count = View'Old.Count
                  and then View.Cursor = View'Old.Cursor
                  and then View.Top = Natural'Min (View'Old.Top, Last_Top (View.Count, Rows));
   --  New rows: the cursor on Cursor (clamped into them), at the same
   --  distance from the top of the view as before where the rows allow.
   procedure Place (View : in out Viewport; Count : Row_Count; Cursor : Row_Count)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Count = Count and then View.Rows = View'Old.Rows
                  and then View.Cursor = (if Count = 0 then 0 else Natural'Max (1, Natural'Min (Cursor, Count)))
                  and then Cursor_Shown (View);
   --  Scrolling: the view moves within the rows; the cursor stays, drawn
   --  or not.
   procedure Scroll_To (View : in out Viewport; Top : Row_Count)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Cursor = View'Old.Cursor
                  and then View.Top = Natural'Min (Top, Last_Top (View.Count, View.Rows));
   procedure Scroll (View : in out Viewport; By : Row_Delta)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Cursor = View'Old.Cursor;
   --  Brings the cursor into view, scrolling as little as needed.
   procedure Reveal (View : in out Viewport)
     with Pre => Valid (View),
          Post => Valid (View) and then Cursor_Shown (View)
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Cursor = View'Old.Cursor
                  and then (if Cursor_Shown (View'Old) then View.Top = View'Old.Top);
   --  The cursor by By rows (clamped), then into view.
   procedure Move (View : in out Viewport; By : Row_Delta)
     with Pre => Valid (View),
          Post => Valid (View) and then Cursor_Shown (View)
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then (if View.Count > 0 then
                              View.Cursor = Integer'Max (1, Integer'Min (View.Count, View'Old.Cursor + By)));
   procedure Go (View : in out Viewport; Row : Row_Index)
     with Pre => Valid (View) and then Row <= View.Count,
          Post => Valid (View) and then View.Cursor = Row and then Cursor_Shown (View)
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then (if Shows (View'Old, Row) then View.Top = View'Old.Top);
end Files_Viewport;
