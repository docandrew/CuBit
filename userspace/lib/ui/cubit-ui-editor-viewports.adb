package body CuBit.UI.Editor.Viewports with SPARK_Mode is
   procedure Initialize
     (Value : out Viewport; Visible_Lines : Positive)
   is
   begin
      Value :=
        (First => 1, Visible => Visible_Lines,
         First_Visible_Column => 1, Visible_Columns => 1);
   end Initialize;

   function First_Line (Value : Viewport) return Line_Number is (Value.First);
   function Line_Capacity (Value : Viewport) return Positive is (Value.Visible);
   function First_Column (Value : Viewport) return Positive is
     (Value.First_Visible_Column);
   function Column_Capacity (Value : Viewport) return Positive is
     (Value.Visible_Columns);

   function Last_Visible_Line
     (Value : Viewport; Document_Lines : Positive) return Line_Number
   is
   begin
      if Value.First > Document_Lines then
         return Document_Lines;
      elsif Value.Visible - 1 > Document_Lines - Value.First then
         return Document_Lines;
      else
         return Value.First + (Value.Visible - 1);
      end if;
   end Last_Visible_Line;

   procedure Set_Line_Capacity
     (Value : in out Viewport; Visible_Lines, Document_Lines : Positive)
   is
      Maximum_First : constant Positive :=
        (if Visible_Lines >= Document_Lines then 1
         else Document_Lines - Visible_Lines + 1);
   begin
      Value.Visible := Visible_Lines;
      Value.First := Positive'Min (Value.First, Maximum_First);
   end Set_Line_Capacity;

   procedure Ensure_Visible
     (Value : in out Viewport; Line, Document_Lines : Line_Number)
   is
   begin
      if Value.First > Document_Lines then
         Value.First := Document_Lines;
      end if;
      if Line < Value.First then
         Value.First := Line;
      elsif Line > Last_Visible_Line (Value, Document_Lines) then
         if Value.Visible - 1 < Line then
            Value.First := Line - Value.Visible + 1;
         else
            Value.First := 1;
         end if;
      end if;
   end Ensure_Visible;

   procedure Scroll_Lines
     (Value : in out Viewport; Amount : Integer;
      Document_Lines : Positive)
   is
      Maximum_First : constant Positive :=
        (if Value.Visible >= Document_Lines then 1
         else Document_Lines - Value.Visible + 1);
   begin
      if Amount < 0 then
         if Amount = Integer'First or else -Amount >= Value.First then
            Value.First := 1;
         else
            Value.First := Value.First - Natural (-Amount);
         end if;
      elsif Amount > 0 then
         if Amount >= Maximum_First - Natural'Min (Value.First, Maximum_First)
         then
            Value.First := Maximum_First;
         else
            Value.First := Value.First + Natural (Amount);
         end if;
      elsif Value.First > Maximum_First then
         Value.First := Maximum_First;
      end if;
   end Scroll_Lines;

   procedure Set_Column_Capacity
     (Value : in out Viewport; Visible_Columns, Document_Columns : Positive)
   is
      Maximum_First : constant Positive :=
        (if Visible_Columns >= Document_Columns then 1
         else Document_Columns - Visible_Columns + 1);
   begin
      Value.Visible_Columns := Visible_Columns;
      Value.First_Visible_Column :=
        Positive'Min (Value.First_Visible_Column, Maximum_First);
   end Set_Column_Capacity;

   procedure Ensure_Column_Visible
     (Value : in out Viewport; Column, Document_Columns : Positive)
   is
      Last_Visible : Positive;
   begin
      if Value.First_Visible_Column > Document_Columns then
         Value.First_Visible_Column := Document_Columns;
      end if;
      if Value.Visible_Columns - 1 >
        Document_Columns - Value.First_Visible_Column
      then
         Last_Visible := Document_Columns;
      else
         Last_Visible :=
           Value.First_Visible_Column + (Value.Visible_Columns - 1);
      end if;
      if Column < Value.First_Visible_Column then
         Value.First_Visible_Column := Column;
      elsif Column > Last_Visible then
         if Value.Visible_Columns - 1 < Column then
            Value.First_Visible_Column :=
              Column - Value.Visible_Columns + 1;
         else
            Value.First_Visible_Column := 1;
         end if;
      end if;
   end Ensure_Column_Visible;

   procedure Scroll_Columns
     (Value : in out Viewport; Amount : Integer;
      Document_Columns : Positive)
   is
      Maximum_First : constant Positive :=
        (if Value.Visible_Columns >= Document_Columns then 1
         else Document_Columns - Value.Visible_Columns + 1);
   begin
      if Amount < 0 then
         if Amount = Integer'First or else
           -Amount >= Value.First_Visible_Column
         then
            Value.First_Visible_Column := 1;
         else
            Value.First_Visible_Column :=
              Value.First_Visible_Column - Natural (-Amount);
         end if;
      elsif Amount > 0 then
         if Amount >= Maximum_First -
           Natural'Min (Value.First_Visible_Column, Maximum_First)
         then
            Value.First_Visible_Column := Maximum_First;
         else
            Value.First_Visible_Column :=
              Value.First_Visible_Column + Natural (Amount);
         end if;
      elsif Value.First_Visible_Column > Maximum_First then
         Value.First_Visible_Column := Maximum_First;
      end if;
   end Scroll_Columns;
end CuBit.UI.Editor.Viewports;
