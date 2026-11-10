package body CuBit.UI.Boxes is
   use type BD.Policy;
   use type BD.Fit;
   package M renames CuBit.UI_Metrics;

   --  A sum of extents, saturating at the ceiling.
   function Plus (Left, Right : Natural) return Extent is
     (Extent (Long_Long_Integer'Min (Long_Long_Integer (BD.MAXIMUM_EXTENT),
                                     Long_Long_Integer (Left) + Long_Long_Integer (Right))));
   function Minus (Left, Right : Natural) return Extent is
     (if Left > Right then Natural'Min (Left - Right, BD.MAXIMUM_EXTENT) else 0);
   function To_Extent (Value : Natural) return Extent is (Natural'Min (Value, BD.MAXIMUM_EXTENT));

   function Text_Width (Text : String; Style : Text_Style) return Natural is
     (case Style is
        when Body_Text | Muted_Text => UI_Text_Width (Text),
        when Heading_Text => Emphasis_Text_Width (Text),
        when Code_Text => Code_Text_Width (Text));

   procedure Wrap (Text : String; Style : Text_Style; Width : Natural) is
      Start, Fit_Last, Cursor, Word_End : Natural;
   begin
      Start := Text'First;
      while Start <= Text'Last and then Text (Start) = ' ' loop
         Start := Start + 1;
      end loop;
      while Start <= Text'Last loop
         Fit_Last := Start - 1;
         Cursor := Start;
         loop
            Word_End := Cursor;
            while Word_End < Text'Last and then Text (Word_End + 1) /= ' ' loop
               Word_End := Word_End + 1;
            end loop;
            if Text_Width (Text (Start .. Word_End), Style) > Width then
               if Fit_Last < Start then
                  --  One word wider than the line: break it between characters,
                  --  at least one per line.
                  Fit_Last := Start;
                  while Fit_Last < Word_End
                    and then Text_Width (Text (Start .. Fit_Last + 1), Style) <= Width
                  loop
                     Fit_Last := Fit_Last + 1;
                  end loop;
               end if;
               exit;
            end if;
            Fit_Last := Word_End;
            Cursor := Word_End + 1;
            while Cursor <= Text'Last and then Text (Cursor) = ' ' loop
               Cursor := Cursor + 1;
            end loop;
            exit when Cursor > Text'Last;
         end loop;
         Line (Start, Fit_Last);
         Start := Fit_Last + 1;
         while Start <= Text'Last and then Text (Start) = ' ' loop
            Start := Start + 1;
         end loop;
      end loop;
   end Wrap;

   function Line_Count (Text : String; Style : Text_Style; Width : Natural) return Natural is
      Count : Natural := 0;
      procedure Counted (First, Last : Natural) is
         pragma Unreferenced (First, Last);
      begin
         Count := Count + 1;
      end Counted;
      procedure Count_Lines is new Wrap (Counted);
   begin
      Count_Lines (Text, Style, Width);
      return Count;
   end Line_Count;

   function Longest_Word (Text : String; Style : Text_Style) return Natural is
      Widest : Natural := 0;
      I : Natural := Text'First;
      Word_End : Natural;
   begin
      while I <= Text'Last loop
         if Text (I) = ' ' then
            I := I + 1;
         else
            Word_End := I;
            while Word_End < Text'Last and then Text (Word_End + 1) /= ' ' loop
               Word_End := Word_End + 1;
            end loop;
            Widest := Natural'Max (Widest, Text_Width (Text (I .. Word_End), Style));
            I := Word_End + 1;
         end if;
      end loop;
      return Widest;
   end Longest_Word;

   ---------------------------------------------------------------------------
   --  Building
   ---------------------------------------------------------------------------
   procedure Clear (T : in out Tree) is
   begin
      T.Used := 0;
      T.Text_Used := 0;
      T.Open := No_Node;
      T.Refused := False;
      T.Short := False;
   end Clear;

   function Overflowed (T : Tree) return Boolean is (T.Refused or else T.Open /= No_Node);
   function Fits (T : Tree) return Boolean is (not T.Short);

   --  A new node under the open container (the root when the tree is
   --  empty); No_Node when refused.
   procedure Add_Node (T : in out Tree; Kind : Node_Kind; Id : Widget_Id; Added : out Node_Ref) is
   begin
      Added := No_Node;
      if T.Used >= T.Capacity
        or else (T.Open = No_Node and then T.Used > 0)
        or else (T.Open /= No_Node and then T.Nodes (T.Open).Children >= BD.MAXIMUM_ITEMS)
      then
         T.Refused := True;
         return;
      end if;
      T.Used := T.Used + 1;
      Added := T.Used;
      T.Nodes (Added) := (Kind => Kind, Id => Id, Parent => T.Open, others => <>);
      if T.Open /= No_Node then
         declare
            P : Node renames T.Nodes (T.Open);
         begin
            if P.Last_Child = No_Node then
               P.First_Child := Added;
            else
               T.Nodes (P.Last_Child).Next := Added;
            end if;
            P.Last_Child := Added;
            P.Children := P.Children + 1;
         end;
      end if;
   end Add_Node;

   procedure Store (T : in out Tree; N : Node_Ref; Text : String) is
   begin
      if Text'Length > T.Text_Capacity - T.Text_Used then
         T.Refused := True;
         return;
      end if;
      T.Nodes (N).Text_First := T.Text_Used + 1;
      T.Nodes (N).Text_Last := T.Text_Used + Text'Length;
      T.Text (T.Text_Used + 1 .. T.Text_Used + Text'Length) := Text;
      T.Text_Used := T.Text_Used + Text'Length;
   end Store;

   function Text_Of (T : Tree; N : Node_Ref) return String is
     (T.Text (T.Nodes (N).Text_First .. T.Nodes (N).Text_Last));

   procedure Begin_Container
     (T : in out Tree; Kind : Container_Kind; Id : Widget_Id; Spacing, Row_Spacing, Margin : Extent;
      Columns : Positive; Align : Alignment; Width_Growth, Height_Growth : Growth)
   is
      N : Node_Ref;
   begin
      Add_Node (T, Kind, Id, N);
      if N /= No_Node then
         T.Nodes (N).Spacing := Spacing;
         T.Nodes (N).Row_Spacing := Row_Spacing;
         T.Nodes (N).Margin := Margin;
         T.Nodes (N).Columns := Natural'Min (Columns, BD.MAXIMUM_ITEMS);
         T.Nodes (N).Align := Align;
         T.Nodes (N).Width_Growth := Width_Growth;
         T.Nodes (N).Height_Growth := Height_Growth;
         T.Open := N;
      else
         --  Keep Begin/End balanced: a refused container's children go
         --  nowhere (the tree is already marked incomplete).
         T.Refused := True;
      end if;
   end Begin_Container;

   procedure Begin_Row
     (T : in out Tree; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_M; Margin : Extent := 0;
      Align : Alignment := Align_Center;
      Width_Growth, Height_Growth : Growth := From_Content) is
   begin
      Begin_Container (T, Row_Box, Id, Spacing, 0, Margin, 1, Align, Width_Growth, Height_Growth);
   end Begin_Row;

   procedure Begin_Column
     (T : in out Tree; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_S; Margin : Extent := 0;
      Align : Alignment := Align_Start;
      Width_Growth, Height_Growth : Growth := From_Content) is
   begin
      Begin_Container (T, Column_Box, Id, Spacing, 0, Margin, 1, Align, Width_Growth, Height_Growth);
   end Begin_Column;

   procedure Begin_Grid
     (T : in out Tree; Columns : Positive; Id : Widget_Id := No_Id;
      Spacing : Extent := CuBit.UI_Metrics.Space_L;
      Row_Spacing : Extent := CuBit.UI_Metrics.Space_XS; Margin : Extent := 0;
      Width_Growth, Height_Growth : Growth := From_Content) is
   begin
      Begin_Container (T, Grid_Box, Id, Spacing, Row_Spacing, Margin, Columns, Align_Start,
                       Width_Growth, Height_Growth);
   end Begin_Grid;

   procedure End_Box (T : in out Tree) is
   begin
      if T.Open = No_Node then
         T.Refused := True;
      else
         T.Open := T.Nodes (T.Open).Parent;
      end if;
   end End_Box;

   procedure Add_Text_Leaf (T : in out Tree; Kind : Node_Kind; Text : String; Style : Text_Style; Id : Widget_Id) is
      N : Node_Ref;
   begin
      Add_Node (T, Kind, Id, N);
      if N /= No_Node then
         T.Nodes (N).Style := Style;
         Store (T, N, Text);
      end if;
   end Add_Text_Leaf;

   procedure Add_Label
     (T : in out Tree; Text : String; Style : Text_Style := Body_Text; Id : Widget_Id := No_Id) is
   begin
      Add_Text_Leaf (T, Label, Text, Style, Id);
   end Add_Label;

   procedure Add_Wrapping_Label
     (T : in out Tree; Text : String; Style : Text_Style := Body_Text; Id : Widget_Id := No_Id) is
   begin
      Add_Text_Leaf (T, Wrapping_Label, Text, Style, Id);
   end Add_Wrapping_Label;

   procedure Add_Sized_Button (T : in out Tree; Label : String; Id : Widget_Id; Fill : Boolean; Insets : Natural) is
      N : Node_Ref;
   begin
      Add_Node (T, Button, Id, N);
      if N /= No_Node then
         Store (T, N, Label);
         declare
            W : constant Extent := To_Extent
              (Natural'Max (Button_Minimum_Width, Text_Width (Label, Body_Text) + Insets + 2 * M.Space_XS));
         begin
            T.Nodes (N).Width := (W, W, (if Fill then Expanding else Fixed), 1);
            T.Nodes (N).Height := BD.Fixed_Hint (M.Control_Height);
         end;
      end if;
   end Add_Sized_Button;

   procedure Add_Button
     (T : in out Tree; Label : String; Id : Widget_Id; Fill : Boolean := False) is
   begin
      Add_Sized_Button (T, Label, Id, Fill, 2 * BUTTON_TEXT_INSET);
   end Add_Button;

   procedure Add_Tab (T : in out Tree; Label : String; Id : Widget_Id) is
   begin
      Add_Sized_Button (T, Label, Id, True, TAB_TEXT_LEFT + TAB_TEXT_INSET);
   end Add_Tab;

   procedure Add_Block
     (T : in out Tree; Id : Widget_Id; Width, Height : Size_Hint; Scrolls : Boolean := False)
   is
      N : Node_Ref;
   begin
      Add_Node (T, Block, Id, N);
      if N /= No_Node then
         T.Nodes (N).Width := Width;
         T.Nodes (N).Height := Height;
         T.Nodes (N).Scrolls := Scrolls;
      end if;
   end Add_Block;

   procedure Add_Glue (T : in out Tree; Stretch : BD.Stretch_Factor := 1) is
      N : Node_Ref;
      Along_Row : constant Boolean := T.Open /= No_Node and then T.Nodes (T.Open).Kind = Row_Box;
      Grows : constant Size_Hint := (0, 0, Expanding, Stretch);
      Still : constant Size_Hint := (0, 0, Preferred, 1);
   begin
      Add_Node (T, Glue, No_Id, N);
      if N /= No_Node then
         T.Nodes (N).Width := (if Along_Row then Grows else Still);
         T.Nodes (N).Height := (if Along_Row then Still else Grows);
      end if;
   end Add_Glue;

   procedure Add_Gap (T : in out Tree; Length : Extent) is
      N : Node_Ref;
   begin
      Add_Node (T, Gap, No_Id, N);
      if N /= No_Node then
         T.Nodes (N).Width := BD.Fixed_Hint (Length);
         T.Nodes (N).Height := BD.Fixed_Hint (Length);
      end if;
   end Add_Gap;

   ---------------------------------------------------------------------------
   --  Measuring and arranging
   ---------------------------------------------------------------------------
   type Axis is (Horizontal, Vertical);

   function Hint_Of (T : Tree; N : Node_Ref; Along : Axis) return Size_Hint is
     (if Along = Horizontal then T.Nodes (N).Width else T.Nodes (N).Height);

   --  The hint of children laid end to end along an axis: sums of their
   --  minimums and preferred lengths, with spacing and margins; expanding
   --  if any child does.
   function Sum_Hint (Hints : BD.Hint_Array; Spacing, Margin : Extent) return Size_Hint is
      Minimum, Natural_Length : Extent := Plus (Margin, Margin);
      Kind : Policy := Fixed;
      Stretch : BD.Stretch_Factor := 1;
   begin
      for I in Hints'Range loop
         if I > Hints'First then
            Minimum := Plus (Minimum, Spacing);
            Natural_Length := Plus (Natural_Length, Spacing);
         end if;
         Minimum := Plus (Minimum, Hints (I).Minimum);
         Natural_Length := Plus (Natural_Length, Hints (I).Preferred);
         if Hints (I).Kind = Expanding then
            Kind := Expanding;
            Stretch := Natural'Max (Stretch, Hints (I).Stretch);
         elsif Hints (I).Kind = Preferred and then Kind = Fixed then
            Kind := Preferred;
         end if;
      end loop;
      if Kind = Fixed and then Minimum /= Natural_Length then
         Kind := Preferred;
      end if;
      return (Minimum, Natural_Length, Kind, Stretch);
   end Sum_Hint;

   --  The hint of children side by side across an axis: the largest.
   function Max_Hint (Hints : BD.Hint_Array; Margin : Extent) return Size_Hint is
      Result : Size_Hint := (0, 0, Fixed, 1);
   begin
      for I in Hints'Range loop
         Result := (if I = Hints'First then Hints (I) else BD.Merge (Result, Hints (I)));
      end loop;
      Result.Minimum := Plus (Result.Minimum, 2 * Margin);
      Result.Preferred := Plus (Result.Preferred, 2 * Margin);
      return Result;
   end Max_Hint;

   --  The children's hints along an axis, in order.
   procedure Child_Hints (T : Tree; N : Node_Ref; Along : Axis; Hints : out BD.Hint_Array) is
      C : Node_Ref := T.Nodes (N).First_Child;
   begin
      for I in Hints'Range loop
         Hints (I) := Hint_Of (T, C, Along);
         C := T.Nodes (C).Next;
      end loop;
   end Child_Hints;

   --  A grid's per-column (Horizontal) or per-row (Vertical) hints.
   procedure Track_Hints (T : Tree; N : Node_Ref; Along : Axis; Hints : out BD.Hint_Array) is
      Columns : constant Positive := T.Nodes (N).Columns;
      C : Node_Ref := T.Nodes (N).First_Child;
      Index : Natural := 0;
      Seen : array (Hints'Range) of Boolean := [others => False];
   begin
      Hints := [others => (0, 0, Fixed, 1)];
      while C /= No_Node loop
         declare
            Track : constant Positive :=
              (if Along = Horizontal then Index mod Columns + 1 else Index / Columns + 1);
            H : constant Size_Hint := Hint_Of (T, C, Along);
         begin
            Hints (Track) := (if Seen (Track) then BD.Merge (Hints (Track), H) else H);
            Seen (Track) := True;
         end;
         Index := Index + 1;
         C := T.Nodes (C).Next;
      end loop;
   end Track_Hints;

   function Track_Count (T : Tree; N : Node_Ref; Along : Axis) return BD.Item_Count is
     (if Along = Horizontal then Natural'Min (T.Nodes (N).Columns, T.Nodes (N).Children)
      else (T.Nodes (N).Children + T.Nodes (N).Columns - 1) / T.Nodes (N).Columns);

   procedure Set_Hint (T : in out Tree; N : Node_Ref; Along : Axis; Measured : Size_Hint) is
      Rule : constant Growth :=
        (if Along = Horizontal then T.Nodes (N).Width_Growth else T.Nodes (N).Height_Growth);
      Hint : Size_Hint := Measured;
   begin
      case Rule is
         when From_Content => null;
         when Shrink_Wrap =>
            if Hint.Kind = Expanding then
               Hint.Kind := Preferred;
            end if;
         when Fill =>
            Hint.Kind := Expanding;
      end case;
      if Along = Horizontal then
         T.Nodes (N).Width := Hint;
      else
         T.Nodes (N).Height := Hint;
      end if;
   end Set_Hint;

   --  Post-order: a node's hint along the axis from its children's. Leaf
   --  widths come from their text; heights need the arranged width.
   procedure Measure_Axis (T : in out Tree; N : Node_Ref; Along : Axis) is
      Item : Node renames T.Nodes (N);
   begin
      case Item.Kind is
         when Label =>
            if Along = Horizontal then
               Item.Width := BD.Fixed_Hint (To_Extent (Text_Width (Text_Of (T, N), Item.Style)));
            else
               Item.Height := BD.Fixed_Hint (M.Line_Height);
            end if;
         when Wrapping_Label =>
            declare
               Text : constant String := Text_Of (T, N);
            begin
               if Along = Horizontal then
                  declare
                     Whole : constant Extent := To_Extent (Text_Width (Text, Item.Style));
                     Least : constant Extent := Natural'Min
                       (Whole, To_Extent (Natural'Max (Wrap_Minimum_Width, Longest_Word (Text, Item.Style))));
                  begin
                     Item.Width := (Least, Natural'Max (Least, Natural'Min (Whole, Wrap_Preferred_Width)),
                                    Expanding, 1);
                  end;
               else
                  Item.Height := BD.Fixed_Hint
                    (To_Extent (Line_Count (Text, Item.Style, Item.Box.w) * M.Line_Height));
               end if;
            end;
         when Button | Block | Glue | Gap =>
            null;
         when Row_Box | Column_Box | Grid_Box =>
            declare
               C : Node_Ref := Item.First_Child;
            begin
               while C /= No_Node loop
                  Measure_Axis (T, C, Along);
                  C := T.Nodes (C).Next;
               end loop;
            end;
            if Item.Kind = Grid_Box then
               declare
                  Hints : BD.Hint_Array (1 .. Track_Count (T, N, Along));
               begin
                  Track_Hints (T, N, Along, Hints);
                  Set_Hint (T, N, Along, Sum_Hint
                    (Hints, (if Along = Horizontal then Item.Spacing else Item.Row_Spacing), Item.Margin));
               end;
            else
               declare
                  Hints : BD.Hint_Array (1 .. Item.Children);
                  Main : constant Axis := (if Item.Kind = Row_Box then Horizontal else Vertical);
               begin
                  Child_Hints (T, N, Along, Hints);
                  Set_Hint (T, N, Along,
                    (if Along = Main then Sum_Hint (Hints, Item.Spacing, Item.Margin)
                     else Max_Hint (Hints, Item.Margin)));
               end;
            end if;
      end case;
   end Measure_Axis;

   procedure Place (T : in out Tree; N : Node_Ref; Along : Axis; Offset, Length : Natural) is
   begin
      if Along = Horizontal then
         T.Nodes (N).Box.x := Offset;
         T.Nodes (N).Box.w := Length;
      else
         T.Nodes (N).Box.y := Offset;
         T.Nodes (N).Box.h := Length;
      end if;
   end Place;

   --  A child across a cell from Offset, Length long: its length (all of
   --  it when it expands) placed by Align.
   procedure Place_Across
     (T : in out Tree; C : Node_Ref; Along : Axis; Offset, Length : Natural; Align : Alignment)
   is
      Taken : Extent;
      Status : BD.Fit;
   begin
      BD.Cross (Hint_Of (T, C, Along), To_Extent (Length), Taken, Status);
      if Status = BD.Too_Small then
         T.Short := True;
      end if;
      declare
         Slack : constant Natural := Minus (Length, Taken);
      begin
         Place (T, C, Along, Offset + (case Align is when Align_Start => 0, when Align_Center => Slack / 2,
                                                       when Align_End => Slack), Taken);
      end;
   end Place_Across;

   --  Top-down: the node already has its place along the axis; place its
   --  children within it, then theirs.
   procedure Arrange_Axis (T : in out Tree; N : Node_Ref; Along : Axis) is
      Kind : constant Node_Kind := T.Nodes (N).Kind;
      Margin : constant Extent := T.Nodes (N).Margin;
      Start : constant Natural := (if Along = Horizontal then T.Nodes (N).Box.x else T.Nodes (N).Box.y) + Margin;
      Inner : constant Extent :=
        Minus ((if Along = Horizontal then T.Nodes (N).Box.w else T.Nodes (N).Box.h), 2 * Margin);
   begin
      if Kind not in Container_Kind or else T.Nodes (N).Children = 0 then
         return;
      end if;
      if Kind = Grid_Box then
         declare
            Hints : BD.Hint_Array (1 .. Track_Count (T, N, Along));
            Spans : BD.Span_Array (Hints'Range);
            Status : BD.Fit;
            Columns : constant Positive := T.Nodes (N).Columns;
            C : Node_Ref := T.Nodes (N).First_Child;
            Index : Natural := 0;
         begin
            Track_Hints (T, N, Along, Hints);
            BD.Distribute (Hints, To_Extent (Start), Inner,
              (if Along = Horizontal then T.Nodes (N).Spacing else T.Nodes (N).Row_Spacing), Spans, Status);
            if Status = BD.Too_Small then
               T.Short := True;
            end if;
            while C /= No_Node loop
               declare
                  Track : constant Positive :=
                    (if Along = Horizontal then Index mod Columns + 1 else Index / Columns + 1);
               begin
                  Place_Across (T, C, Along, Spans (Track).Offset, Spans (Track).Length,
                                (if Along = Horizontal then Align_Start else Align_Center));
               end;
               Arrange_Axis (T, C, Along);
               Index := Index + 1;
               C := T.Nodes (C).Next;
            end loop;
         end;
      elsif (Kind = Row_Box) = (Along = Horizontal) then
         --  Along the box's own axis: share the length out.
         declare
            Hints : BD.Hint_Array (1 .. T.Nodes (N).Children);
            Spans : BD.Span_Array (Hints'Range);
            Status : BD.Fit;
            C : Node_Ref := T.Nodes (N).First_Child;
         begin
            Child_Hints (T, N, Along, Hints);
            BD.Distribute (Hints, To_Extent (Start), Inner, T.Nodes (N).Spacing, Spans, Status);
            if Status = BD.Too_Small then
               T.Short := True;
            end if;
            for I in Spans'Range loop
               Place (T, C, Along, Spans (I).Offset, Spans (I).Length);
               Arrange_Axis (T, C, Along);
               C := T.Nodes (C).Next;
            end loop;
         end;
      else
         --  Across it: each child within the whole inner length.
         declare
            C : Node_Ref := T.Nodes (N).First_Child;
         begin
            while C /= No_Node loop
               Place_Across (T, C, Along, Start, Inner, T.Nodes (N).Align);
               Arrange_Axis (T, C, Along);
               C := T.Nodes (C).Next;
            end loop;
         end;
      end if;
   end Arrange_Axis;

   Root : constant Node_Ref := 1;

   procedure Lay_Out_Width (T : in out Tree; X, Width : Natural) is
   begin
      Measure_Axis (T, Root, Horizontal);
      Place (T, Root, Horizontal, X, Width);
      Arrange_Axis (T, Root, Horizontal);
      Measure_Axis (T, Root, Vertical);
   end Lay_Out_Width;

   procedure Measure (T : in out Tree; Minimum, Natural_Size : out Size) is
   begin
      Minimum := (0, 0);
      Natural_Size := (0, 0);
      if T.Used = 0 then
         return;
      end if;
      Measure_Axis (T, Root, Horizontal);
      Minimum.Width := T.Nodes (Root).Width.Minimum;
      Natural_Size.Width := T.Nodes (Root).Width.Preferred;
      Lay_Out_Width (T, 0, Minimum.Width);
      Minimum.Height := T.Nodes (Root).Height.Minimum;
      Lay_Out_Width (T, 0, Natural_Size.Width);
      Natural_Size.Height := Natural'Max (Minimum.Height, T.Nodes (Root).Height.Preferred);
   end Measure;

   procedure Arrange (T : in out Tree; Bounds : Rect) is
   begin
      T.Short := False;
      if T.Used = 0 then
         return;
      end if;
      Lay_Out_Width (T, Bounds.x, Bounds.w);
      if Bounds.w < T.Nodes (Root).Width.Minimum then
         T.Short := True;
      end if;
      if Bounds.h < T.Nodes (Root).Height.Minimum then
         T.Short := True;
      end if;
      Place (T, Root, Vertical, Bounds.y, Bounds.h);
      Arrange_Axis (T, Root, Vertical);
   end Arrange;

   function Bounds (T : Tree; Id : Widget_Id) return Rect is
   begin
      if Id /= No_Id then
         for N in 1 .. T.Used loop
            if T.Nodes (N).Id = Id then
               return T.Nodes (N).Box;
            end if;
         end loop;
      end if;
      return (others => 0);
   end Bounds;

   function Hit (T : Tree; X, Y : Natural) return Widget_Id is
   begin
      for N in 1 .. T.Used loop
         if T.Nodes (N).Kind in Button | Block and then T.Nodes (N).Id /= No_Id
           and then Point_In_Rect (X, Y, T.Nodes (N).Box)
         then
            return T.Nodes (N).Id;
         end if;
      end loop;
      return No_Id;
   end Hit;

   procedure Visit_Named (T : Tree) is
   begin
      for N in 1 .. T.Used loop
         if T.Nodes (N).Id /= No_Id then
            Visit (T.Nodes (N).Id, T.Nodes (N).Box);
         end if;
      end loop;
   end Visit_Named;

   function Scroll_Target (T : Tree; X, Y : Natural) return Widget_Id is
   begin
      for N in 1 .. T.Used loop
         if T.Nodes (N).Kind = Block and then T.Nodes (N).Scrolls and then T.Nodes (N).Id /= No_Id
           and then Point_In_Rect (X, Y, T.Nodes (N).Box)
         then
            return T.Nodes (N).Id;
         end if;
      end loop;
      return No_Id;
   end Scroll_Target;

   procedure Draw (T : Tree) is
   begin
      for N in 1 .. T.Used loop
         declare
            Item : Node renames T.Nodes (N);
         begin
            case Item.Kind is
               when Label =>
                  Draw_Text (Item.Box.x, Item.Box.y, Text_Of (T, N), Item.Style, Item.Id);
               when Wrapping_Label =>
                  declare
                     Text : constant String := Text_Of (T, N);
                     Row : Natural := 0;
                     procedure Line (First, Last : Natural) is
                     begin
                        Draw_Text (Item.Box.x, Item.Box.y + Row * M.Line_Height, Text (First .. Last),
                                   Item.Style, Item.Id);
                        Row := Row + 1;
                     end Line;
                     procedure Lines is new Wrap (Line);
                  begin
                     Lines (Text, Item.Style, Item.Box.w);
                  end;
               when Button =>
                  Draw_Button (Item.Box, Text_Of (T, N), Item.Id);
               when others =>
                  null;
            end case;
         end;
      end loop;
   end Draw;
end CuBit.UI.Boxes;
