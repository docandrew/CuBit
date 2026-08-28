package body CuBit.UI.Editor.Documents with SPARK_Mode is
   procedure Initialize
     (Value : out Document; Text : String; Result : out Edit_Result)
   is
   begin
      Value.Data := [others => ' '];
      Value.Last := 0;
      if Text'Length > Value.Capacity then
         Result := Capacity_Exceeded;
      else
         for Offset in 0 .. Text'Length - 1 loop
            pragma Loop_Invariant (Offset < Text'Length);
            Value.Data (Offset + 1) := Text (Text'First + Offset);
         end loop;
         Value.Last := Text'Length;
         Result := Applied;
      end if;
   end Initialize;

   function Length (Value : Document) return Natural is (Value.Last);
   function Capacity_Of (Value : Document) return Document_Capacity is
     (Value.Capacity);
   function Content (Value : Document) return String is
     (Value.Data (1 .. Value.Last));

   procedure Insert
     (Value : in out Document; Position : Document_Position;
      Text : String; Result : out Edit_Result)
   is
   begin
      if Text'Length > Value.Capacity - Value.Last then
         Result := Capacity_Exceeded;
         return;
      end if;
      if Text'Length > 0 then
         if Position <= Value.Last then
            Value.Data (Position + Text'Length .. Value.Last + Text'Length) :=
              Value.Data (Position .. Value.Last);
         end if;
         for Offset in 0 .. Text'Length - 1 loop
            pragma Loop_Invariant (Offset < Text'Length);
            Value.Data (Position + Offset) := Text (Text'First + Offset);
         end loop;
         Value.Last := Value.Last + Text'Length;
      end if;
      Result := Applied;
   end Insert;

   procedure Delete
     (Value : in out Document; Position : Document_Position; Count : Natural)
   is
   begin
      if Count > 0 then
         if Position + Count <= Value.Last then
            Value.Data (Position .. Value.Last - Count) :=
              Value.Data (Position + Count .. Value.Last);
         end if;
         Value.Last := Value.Last - Count;
      end if;
   end Delete;

   function Line_Count (Value : Document) return Positive is
      Count : Positive := 1;
   begin
      for Index in 1 .. Value.Last loop
         pragma Loop_Invariant (Count <= Index + 1);
         if Value.Data (Index) = ASCII.LF then Count := Count + 1; end if;
      end loop;
      return Count;
   end Line_Count;

   function Safe_Line_Length
     (Value : Document; Line : Positive) return Line_Character_Count
   with Pre => Value.Last <= Value.Capacity
   is
      Current : Positive := 1;
      Count : Line_Character_Count := 0;
   begin
      for Index in 1 .. Value.Last loop
         pragma Loop_Invariant (Current <= Index + 1);
         pragma Loop_Invariant (Count < Index);
         if Current = Line then
            exit when Value.Data (Index) = ASCII.LF;
            Count := Count + 1;
         elsif Value.Data (Index) = ASCII.LF then
            Current := Current + 1;
         end if;
      end loop;
      return Count;
   end Safe_Line_Length;

   function Line_Length
     (Value : Document; Line : Positive) return Line_Character_Count is
     (Safe_Line_Length (Value, Line));

   procedure Position_To_Line_Column
     (Value : Document; Position : Document_Position;
      Line, Column : out Positive)
   is
   begin
      Line := 1;
      Column := 1;
      for Index in 1 .. Position - 1 loop
         pragma Loop_Invariant (Line <= Index + 1);
         pragma Loop_Invariant (Column <= Index + 1);
         if Value.Data (Index) = ASCII.LF then
            Line := Line + 1;
            Column := 1;
         else
            Column := Column + 1;
         end if;
      end loop;
   end Position_To_Line_Column;

   function Safe_Line_Column_To_Position
     (Value : Document; Line, Column : Positive) return Document_Position
   with Pre => Value.Last <= Value.Capacity
   is
      Current_Line : Positive := 1;
      Current_Column : Positive := 1;
   begin
      for Index in 1 .. Value.Last loop
         pragma Loop_Invariant (Current_Line <= Index + 1);
         pragma Loop_Invariant (Current_Column <= Index + 1);
         if Current_Line = Line and then Current_Column = Column then
            return Index;
         end if;
         if Value.Data (Index) = ASCII.LF then
            Current_Line := Current_Line + 1;
            Current_Column := 1;
         else
            Current_Column := Current_Column + 1;
         end if;
      end loop;
      return Value.Last + 1;
   end Safe_Line_Column_To_Position;

   function Line_Column_To_Position
     (Value : Document; Line, Column : Positive) return Document_Position is
     (Safe_Line_Column_To_Position (Value, Line, Column));

   function Safe_Line_At_Position
     (Value : Document; Position : Document_Position) return Positive
   with Pre =>
     Value.Last <= Value.Capacity and then Position <= Value.Last + 1
   is
      Line : Positive := 1;
   begin
      for Index in 1 .. Position - 1 loop
         pragma Loop_Invariant (Line <= Index + 1);
         if Value.Data (Index) = ASCII.LF then Line := Line + 1; end if;
      end loop;
      return Line;
   end Safe_Line_At_Position;

   procedure Move_Vertically
     (Value : Document; Position : Document_Position;
      Preferred_Column : Display_Column; Direction : Vertical_Direction;
      Result : out Document_Position)
   is
      Line : constant Positive := Safe_Line_At_Position (Value, Position);
      Last_Line : constant Positive := Line_Count (Value);
      Target_Line : Positive;
      Target_Column : Positive;
   begin
      case Direction is
         when Up =>
            Target_Line := (if Line > 1 then Line - 1 else Line);
         when Down =>
            Target_Line := (if Line < Last_Line then Line + 1 else Line);
      end case;
      Target_Column := Positive'Min
         (Preferred_Column,
         Line_Character_Count'Succ
           (Safe_Line_Length (Value, Target_Line)));
      Result := Safe_Line_Column_To_Position
        (Value, Target_Line, Target_Column);
   end Move_Vertically;
end CuBit.UI.Editor.Documents;
