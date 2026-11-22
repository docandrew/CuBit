package body CuBit.UI.Splits is
   package L renames Client_Split_Layout;

   procedure Lay (Area : Rect; Count : Part_Count; Lengths : L.Lengths; Parts, Dividers : out Rect_Table) is
      X : Natural := Area.x;
   begin
      Parts := [others => (others => 0)];
      Dividers := [others => (others => 0)];
      for I in 1 .. Count loop
         Parts (I) := (X, Area.y, Lengths (I), Area.h);
         X := X + Lengths (I);
         if I < Count then
            Dividers (I) := (X, Area.y, DIVIDER_WIDTH, Area.h);
            X := X + DIVIDER_WIDTH;
         end if;
      end loop;
   end Lay;

   procedure Track
     (Map : in out Controls.Control_Map; Area : Rect; Count : Part_Count; Shares : in out Weights;
      Minimum : Natural; Base : Controls.Control_ID; Parts, Dividers : out Rect_Table)
   is
      Lengths : L.Lengths;
      Value : Natural;
      Available : Boolean;
      Dragged : Boolean := False;
   begin
      L.Distribute (Natural'Min (Area.w, L.MAXIMUM_LENGTH), Count, DIVIDER_WIDTH, Shares, Lengths);
      Lay (Area, Count, Lengths, Parts, Dividers);
      for I in 1 .. Count - 1 loop
         declare
            Pair : constant Natural := Lengths (I) + Lengths (I + 1);
            Low : constant Natural := Natural'Min (Minimum, Pair / 2);
         begin
            Controls.Add_Horizontal_Drag
              (Map, Base + I - 1, Dividers (I), Area, Lengths (I), Low, Pair - Low, Parts (I).x);
            Controls.Take_Value (Map, Base + I - 1, Value, Available);
            if Available and then Pair <= L.MAXIMUM_LENGTH then
               L.Drag (Lengths, I, Natural'Min (Value, L.MAXIMUM_LENGTH), Minimum);
               Dragged := True;
            end if;
         end;
      end loop;
      if Dragged then
         Shares := Lengths;
         Lay (Area, Count, Lengths, Parts, Dividers);
      end if;
   end Track;

   procedure Draw_Dividers
     (C : Canvas; Map : Controls.Control_Map; Count : Part_Count; Dividers : Rect_Table; Colors : Theme;
      Base : Controls.Control_ID; Hot_X, Hot_Y : Natural; Hover : Boolean)
   is
   begin
      for I in 1 .. Count - 1 loop
         Draw_Vertical_Splitter
           (C, Dividers (I), Colors, Hover and then Point_In_Rect (Hot_X, Hot_Y, Dividers (I)),
            Controls.Is_Active (Map, Base + I - 1));
      end loop;
   end Draw_Dividers;
end CuBit.UI.Splits;
