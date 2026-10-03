package body CCL.Literal_Tables with SPARK_Mode is
   function Space (C : Character) return Boolean is (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);

   procedure Split
     (Literal : String; Many : Boolean; Columns : CCL.Types.Component_Count;
      Result : out Table)
   is
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
               Read_Row (if Result.Rows < Maximum_Rows then Result.Rows + 1 else 0);
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
