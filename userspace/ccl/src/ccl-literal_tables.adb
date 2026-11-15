package body CCL.Literal_Tables with SPARK_Mode is
   function Space (C : Character) return Boolean is (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);

   procedure Split
     (Literal : String; Shape : CCL.Types.Shapes.Row_Shape; Result : out Table)
   is
      Many : constant Boolean := Shape.Many;
      Columns : constant CCL.Types.Component_Count := Shape.Count;
      Position : Natural := Literal'First;
      Good : Boolean := True;

      --  From an opening '"' at Position, past its closing quote.
      procedure Skip_String is
      begin
         Position := Position + 1;
         while Position <= Literal'Last loop
            pragma Loop_Variant (Increases => Position);
            if Literal (Position) = '\' then
               Position := Position + (if Position < Literal'Last then 2 else 1);
            elsif Literal (Position) = '"' then
               Position := Position + 1;
               return;
            else
               Position := Position + 1;
            end if;
         end loop;
         Good := False;
      end Skip_String;

      --  From an opening bracket at Position, past its matching close.
      procedure Skip_Group is
         Depth : Natural := 0;
      begin
         while Position <= Literal'Last loop
            pragma Loop_Variant (Increases => Position);
            pragma Loop_Invariant (Depth <= Position - Literal'First);
            case Literal (Position) is
               when '"' =>
                  Skip_String;
                  exit when not Good;
               when '(' | '[' =>
                  Depth := Depth + 1;
                  Position := Position + 1;
               when ')' | ']' =>
                  Position := Position + 1;
                  if Depth <= 1 then return; end if;
                  Depth := Depth - 1;
               when others =>
                  Position := Position + 1;
            end case;
         end loop;
         Good := False;
      end Skip_Group;

      procedure Skip_Spaces is
      begin
         while Position <= Literal'Last and then Space (Literal (Position)) loop
            pragma Loop_Variant (Increases => Position);
            Position := Position + 1;
         end loop;
      end Skip_Spaces;

      --  One "(Type field ...)" at Position, its fields into row Row (when
      --  Row > 0; rows past Maximum_Rows are only counted).
      procedure Read_Row (Row : Row_Count) is
         Column : CCL.Types.Component_Count := 0;
         First : Positive;
      begin
         if Position > Literal'Last or else Literal (Position) /= '(' then
            Good := False;
            return;
         end if;
         Position := Position + 1;
         --  The type's name.
         while Position <= Literal'Last and then not Space (Literal (Position)) and then
           Literal (Position) /= ')'
         loop
            pragma Loop_Variant (Increases => Position);
            Position := Position + 1;
         end loop;
         loop
            pragma Loop_Variant (Increases => Position);
            Skip_Spaces;
            if Position > Literal'Last then
               Good := False;
               return;
            end if;
            exit when Literal (Position) = ')';
            First := Position;
            case Literal (Position) is
               when '"' => Skip_String;
               when '(' | '[' => Skip_Group;
               when others =>
                  while Position <= Literal'Last and then not Space (Literal (Position)) and then
                    Literal (Position) not in ')' | ']'
                  loop
                     pragma Loop_Variant (Increases => Position);
                     Position := Position + 1;
                  end loop;
            end case;
            if not Good or else Column = Columns then
               Good := False;
               return;
            end if;
            Column := Column + 1;
            if Row > 0 then
               Result.Cells (Row, Column) := (First, Position - 1);
            end if;
            exit when Position = First;  --  no progress: malformed
         end loop;
         Position := Position + 1;
         Good := Good and then Column = Columns;
      end Read_Row;
      --  An atom from Position: up to a space, a bracket or a parenthesis.
      procedure Skip_Atom is
      begin
         while Position <= Literal'Last and then not Space (Literal (Position)) and then
           Literal (Position) not in '(' | ')' | '[' | ']'
         loop
            pragma Loop_Variant (Increases => Position);
            Position := Position + 1;
         end loop;
      end Skip_Atom;

      --  One element of a list of a variant (Shape.Variant_Rows) at
      --  Position: its member's name, then its record's fields into the
      --  columns they fill in row Row (when Row > 0). A member without a
      --  payload is a bare atom, an empty row.
      procedure Read_Member_Row (Row : Row_Count) is
         Bare : constant Boolean := Position <= Literal'Last and then Literal (Position) /= '(';
         Name_First : Positive;
         Dot : Natural := 0;
         Member : CCL.Types.Component_Count := 0;
         Field : CCL.Types.Component_Count := 0;
         Column : CCL.Types.Component_Count;
         First : Positive;
      begin
         if Position > Literal'Last then
            Good := False;
            return;
         end if;
         if not Bare then
            Position := Position + 1;
         end if;
         Name_First := Position;
         Skip_Atom;
         for I in Name_First .. Position - 1 loop
            if Literal (I) = '.' then
               Dot := I;
            end if;
         end loop;
         for M in 1 .. Shape.Member_Count loop
            if Dot > 0 and then CCL.Types.Image (Shape.Members (M).Member) = Literal (Dot + 1 .. Position - 1) then
               Member := M;
            end if;
         end loop;
         if Member = 0 then
            Good := False;
            return;
         elsif Bare then
            Good := Shape.Members (Member).Fields = 0;
            return;
         end if;
         Skip_Spaces;
         if Position <= Literal'Last and then Literal (Position) = '(' then
            --  The payload record: its type's name, then its fields.
            Position := Position + 1;
            Skip_Atom;
            loop
               pragma Loop_Variant (Increases => Position);
               Skip_Spaces;
               if Position > Literal'Last then
                  Good := False;
                  return;
               end if;
               exit when Literal (Position) = ')';
               First := Position;
               case Literal (Position) is
                  when '"' => Skip_String;
                  when '(' | '[' => Skip_Group;
                  when others => Skip_Atom;
               end case;
               if not Good or else Position = First or else Field = Shape.Members (Member).Fields then
                  Good := False;
                  return;
               end if;
               Field := Field + 1;
               Column := (if Member = 1 then Field else Shape.Members (Member).Column_Of (Field));
               if Row > 0 and then Column in 1 .. Columns then
                  Result.Cells (Row, Column) := (First, Position - 1);
               end if;
            end loop;
            Position := Position + 1;
            Skip_Spaces;
         end if;
         Good := Good and then Field = Shape.Members (Member).Fields
           and then Position <= Literal'Last and then Literal (Position) = ')';
         if Good then
            Position := Position + 1;
         end if;
      end Read_Member_Row;
   begin
      Result := (Columns => Columns, others => <>);
      if Columns = 0 then return; end if;
      Skip_Spaces;
      if not Many then
         Read_Row (1);
         Result.Rows := 1;
         Result.Total := 1;
      elsif Position <= Literal'Last and then Literal (Position) = '[' then
         Position := Position + 1;
         loop
            pragma Loop_Variant (Increases => Position);
            Skip_Spaces;
            exit when not Good or else Position > Literal'Last or else Literal (Position) = ']';
            declare
               Before : constant Positive := Position;
            begin
               if Shape.Variant_Rows then
                  Read_Member_Row (if Result.Rows < Maximum_Rows then Result.Rows + 1 else 0);
               else
                  Read_Row (if Result.Rows < Maximum_Rows then Result.Rows + 1 else 0);
               end if;
               exit when not Good or else Position <= Before;
            end;
            if Result.Rows < Maximum_Rows then Result.Rows := Result.Rows + 1; end if;
            if Result.Total < Natural'Last then Result.Total := Result.Total + 1; end if;
         end loop;
         Good := Good and then Position <= Literal'Last and then Literal (Position) = ']';
      else
         Good := False;
      end if;
      Result.Complete := Good;
   end Split;
end CCL.Literal_Tables;
