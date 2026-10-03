package body CCL.Presentations is
   use type CCL.Language.Interpretation_Status;

   --  The standard Image type: (Image width height id), all Integers.
   --  Rows of the standard Image type: (Image width height id), all Integers.
   function Image_Rows (Shape : CCL.Types.Shapes.Row_Shape) return Boolean is
     (CCL.Types.Image (Shape.Row_Type) = "Image" and then
      Shape.Count = 3 and then
      CCL.Types.Image (Shape.Fields (1).Identifier) = "width" and then Shape.Fields (1).Numeric and then
      CCL.Types.Image (Shape.Fields (2).Identifier) = "height" and then Shape.Fields (2).Numeric and then
      CCL.Types.Image (Shape.Fields (3).Identifier) = "id" and then Shape.Fields (3).Numeric);

   function Cell_Text
     (Outcome : CCL.Language.Interpretation_Result; Item : Presentation;
      Row : CCL.Literal_Tables.Row_Index; Column : CCL.Types.Component_Index) return String
   is
      Span : constant CCL.Literal_Tables.Span := Item.Cells.Cells (Row, Column);
   begin
      if Span.Last < Span.First or else Span.Last > Outcome.Literal.Length then
         return "";
      end if;
      return Outcome.Literal.Data (Span.First .. Span.Last);
   end Cell_Text;

   --  A canonical Integer literal's value (0 if it is not one). No 'Value:
   --  the native runtime has none. Image ids use all 63 bits.
   function Number (Text : String) return Long_Long_Integer is
      Negative : constant Boolean := Text'Length > 0 and then Text (Text'First) = '-';
      First : constant Natural := (if Negative then Text'First + 1 else Text'First);
      LIMIT : constant Long_Long_Integer := Long_Long_Integer'Last;
      Result : Long_Long_Integer := 0;
   begin
      if First > Text'Last then return 0; end if;
      for I in First .. Text'Last loop
         if Text (I) not in '0' .. '9' then return 0; end if;
         declare
            Digit : constant Long_Long_Integer :=
              Long_Long_Integer (Character'Pos (Text (I)) - Character'Pos ('0'));
         begin
            if Result > (LIMIT - Digit) / 10 then return 0; end if;
            Result := Result * 10 + Digit;
         end;
      end loop;
      return (if Negative then -Result else Result);
   end Number;

   --  A dimension as shown: within what the store can hold.
   function Side (Value : Long_Long_Integer) return Natural is
     (Natural (Long_Long_Integer'Max (0, Long_Long_Integer'Min
        (Value, CCL.Image_Store.Maximum_Side))));

   procedure Describe
     (Outcome : CCL.Language.Interpretation_Result; Result : out Presentation)
   is
   begin
      Result := (others => <>);
      if Outcome.Status /= CCL.Language.Succeeded then
         return;
      end if;
      Result.Kind := Text;
      if not Outcome.Has_Literal or else Outcome.Literal_Shape.Count = 0 then
         return;
      end if;
      Result.Shape := Outcome.Literal_Shape;
      CCL.Literal_Tables.Split
        (Outcome.Literal.Data (1 .. Outcome.Literal.Length), Result.Shape.Many,
         Result.Shape.Count, Result.Cells);
      if not Result.Cells.Complete then
         return;
      end if;
      Result.Kind := Table;
      if Image_Rows (Result.Shape) then
         for Row in 1 .. Result.Cells.Rows loop
            Result.Pictures (Row) :=
              (Image => CCL.Image_Store.Image_Id (Number (Cell_Text (Outcome, Result, Row, 3))),
               Width => Side (Number (Cell_Text (Outcome, Result, Row, 1))),
               Height => Side (Number (Cell_Text (Outcome, Result, Row, 2))));
         end loop;
         Result.Picture_Count := Result.Cells.Rows;
         if Result.Shape.Many then
            Result.Kind := Gallery;
         elsif Result.Picture_Count = 1 then
            Result.Kind := Picture;
            Result.Image := Result.Pictures (1).Image;
            Result.Image_Width := Result.Pictures (1).Width;
            Result.Image_Height := Result.Pictures (1).Height;
         end if;
      end if;
   end Describe;
end CCL.Presentations;
