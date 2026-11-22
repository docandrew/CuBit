--  Where the Logs table looks and which row it has selected
--  (docs/logs-app.md). The viewport scrolls freely: the wheel, the
--  scrollbar and arriving records leave the selection where it is, and only
--  moving the selection brings it into view. Following keeps the newest row
--  selected and in view until the person scrolls or moves away. Search
--  matches are found by row, with wrap-around, among rows that all stay
--  shown. Pure policy, proved (tests/log-viewer/log_viewport_proof.gpr).
package Log_Viewport with SPARK_Mode, Pure is
   --  Rows the table can hold: the Logs view's record ring.
   MAXIMUM_ROWS : constant := 2_048;
   subtype Row_Count is Natural range 0 .. MAXIMUM_ROWS;
   subtype Row_Index is Row_Count range 1 .. MAXIMUM_ROWS;
   --  Rows drawn at once.
   subtype Page_Rows is Row_Index;
   --  A move or scroll by this many rows; negative goes toward row 1.
   subtype Row_Delta is Integer range -MAXIMUM_ROWS .. MAXIMUM_ROWS;
   --  Which rows the search matches, by row.
   type Hit_Flags is array (Row_Index) of Boolean;

   type Viewport is record
      Count : Row_Count := 0;
      --  The selected row, 0 for none.
      Selected : Row_Count := 0;
      --  Rows above the first one drawn.
      Top : Row_Count := 0;
      Rows : Page_Rows := 20;
      Follow : Boolean := True;
   end record;

   --  The furthest the view scrolls: the last row at the bottom.
   function Last_Top (Count : Row_Count; Rows : Page_Rows) return Row_Count is
     (if Count > Rows then Count - Rows else 0);
   function Valid (View : Viewport) return Boolean is
     (View.Selected <= View.Count and then View.Top <= Last_Top (View.Count, View.Rows));
   --  Whether Row is drawn.
   function Shows (View : Viewport; Row : Row_Index) return Boolean is
     (Row > View.Top and then Row <= View.Top + View.Rows);
   function Selection_Shown (View : Viewport) return Boolean is
     (View.Selected = 0 or else Shows (View, View.Selected));

   --  The rows changed in number or the table in height: the selection and
   --  the view stay where they were as far as the rows allow.
   procedure Fit (View : in out Viewport; Count : Row_Count; Rows : Page_Rows)
     with Post => Valid (View) and then View.Count = Count and then View.Rows = Rows
                  and then View.Follow = View'Old.Follow
                  and then View.Selected = Natural'Min (View'Old.Selected, Count)
                  and then View.Top = Natural'Min (View'Old.Top, Last_Top (Count, Rows));
   --  Rebuilt rows, with the selection and the first drawn row as found again.
   procedure Place (View : in out Viewport; Count : Row_Count; Selected, Top : Row_Count)
     with Pre => Selected <= Count,
          Post => Valid (View) and then View.Count = Count and then View.Selected = Selected
                  and then View.Rows = View'Old.Rows and then View.Follow = View'Old.Follow
                  and then View.Top = Natural'Min (Top, Last_Top (Count, View.Rows));

   --  Scrolling: the view moves within the rows and the selection stays,
   --  drawn or not. A view that moved stops following.
   procedure Scroll_To (View : in out Viewport; Top : Row_Count)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Selected = View'Old.Selected
                  and then View.Top = Natural'Min (Top, Last_Top (View.Count, View.Rows))
                  and then View.Follow = (View'Old.Follow and then View.Top = View'Old.Top);
   procedure Scroll (View : in out Viewport; By : Row_Delta)
     with Pre => Valid (View),
          Post => Valid (View) and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Selected = View'Old.Selected
                  and then View.Top =
                    Integer'Min (Integer'Max (0, View'Old.Top + By), Last_Top (View.Count, View.Rows))
                  and then View.Follow = (View'Old.Follow and then View.Top = View'Old.Top);

   --  Brings the selection into view, scrolling as little as needed.
   procedure Reveal (View : in out Viewport)
     with Pre => Valid (View),
          Post => Valid (View) and then Selection_Shown (View)
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Selected = View'Old.Selected and then View.Follow = View'Old.Follow
                  and then (if Selection_Shown (View'Old) then View.Top = View'Old.Top);
   --  Moving the selection by hand: it stops following and comes into view.
   procedure Move (View : in out Viewport; By : Row_Delta)
     with Pre => Valid (View) and then View.Count > 0,
          Post => Valid (View) and then not View.Follow
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then View.Selected = Natural'Max (1, Natural'Min (View.Count, View'Old.Selected + By))
                  and then Shows (View, View.Selected);
   procedure Select_Row (View : in out Viewport; Row : Row_Index)
     with Pre => Valid (View) and then Row <= View.Count,
          Post => Valid (View) and then not View.Follow and then View.Selected = Row
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then Shows (View, Row)
                  and then (if Shows (View'Old, Row) then View.Top = View'Old.Top);
   --  A search match: selected, and centred when it was out of view so that
   --  the rows around it show too.
   procedure Show_Match (View : in out Viewport; Row : Row_Index)
     with Pre => Valid (View) and then Row <= View.Count,
          Post => Valid (View) and then not View.Follow and then View.Selected = Row
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then Shows (View, Row)
                  and then (if Shows (View'Old, Row) then View.Top = View'Old.Top);
   --  Following: Newest selected and in view.
   procedure Follow_Newest (View : in out Viewport; Newest : Row_Count)
     with Pre => Valid (View) and then Newest <= View.Count,
          Post => Valid (View) and then View.Follow and then View.Selected = Newest
                  and then View.Count = View'Old.Count and then View.Rows = View'Old.Rows
                  and then Selection_Shown (View);

   --  A row arrived at Row: the selection, and the rows drawn, stay on the
   --  same records (a row at or above the first drawn one arrives out of
   --  view).
   procedure Insert (View : in out Viewport; Row : Row_Index)
     with Pre => Valid (View) and then View.Count < MAXIMUM_ROWS and then Row <= View.Count + 1,
          Post => Valid (View) and then View.Count = View'Old.Count + 1
                  and then View.Rows = View'Old.Rows and then View.Follow = View'Old.Follow
                  and then View.Selected =
                    (if View'Old.Selected >= Row then View'Old.Selected + 1 else View'Old.Selected)
                  and then View.Top =
                    Natural'Min ((if View'Old.Top + 1 >= Row then View'Old.Top + 1 else View'Old.Top),
                                 Last_Top (View.Count, View.Rows));
   --  Row left: the same, the other way. A selected row that leaves passes
   --  the selection to the row before it.
   procedure Remove (View : in out Viewport; Row : Row_Index)
     with Pre => Valid (View) and then Row <= View.Count,
          Post => Valid (View) and then View.Count = View'Old.Count - 1
                  and then View.Rows = View'Old.Rows and then View.Follow = View'Old.Follow
                  and then View.Selected =
                    (if View'Old.Selected >= Row then View'Old.Selected - 1 else View'Old.Selected)
                  and then View.Top =
                    Natural'Min ((if View'Old.Top >= Row then View'Old.Top - 1 else View'Old.Top),
                                 Last_Top (View.Count, View.Rows));

   --  The nearest match after From (Forward) or before it, wrapping around
   --  the rows; From 0 starts before the first row (Forward) or after the
   --  last. From itself is found last, after a full turn. 0: no row matches.
   function Next_Match (Hits : Hit_Flags; Count : Row_Count; From : Row_Count; Forward : Boolean) return Row_Count
     with Pre => From <= Count,
          Post =>
            (if Next_Match'Result = 0 then (for all R in 1 .. Count => not Hits (R))
             else Next_Match'Result <= Count and then Hits (Next_Match'Result)
                  and then
                    (if Forward then
                       (if Next_Match'Result > From
                        then (for all R in From + 1 .. Next_Match'Result - 1 => not Hits (R))
                        else (for all R in From + 1 .. Count => not Hits (R))
                             and then (for all R in 1 .. Next_Match'Result - 1 => not Hits (R)))
                     else
                       (if Next_Match'Result < From
                        then (for all R in Next_Match'Result + 1 .. From - 1 => not Hits (R))
                        else (for all R in 1 .. From - 1 => not Hits (R))
                             and then (for all R in Next_Match'Result + 1 .. Count => not Hits (R)))));
   --  Matching rows, and those at or before Row.
   function Match_Total (Hits : Hit_Flags; Count : Row_Count) return Row_Count
     with Post => Match_Total'Result <= Count;
   function Match_Rank (Hits : Hit_Flags; Row : Row_Index) return Row_Count
     with Post => Match_Rank'Result <= Row and then (if Hits (Row) then Match_Rank'Result > 0);
end Log_Viewport;
