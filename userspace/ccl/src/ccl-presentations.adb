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

   function Span_Text (Outcome : CCL.Language.Interpretation_Result; Span : Literal_Span) return String is
     (if Span.Last < Span.First or else Span.Last > Outcome.Literal.Length then ""
      else Outcome.Literal.Data (Span.First .. Span.Last));

   --  A lexicon entry's literal (CCL.Interfaces.Lexicon) as an article. The
   --  literal is the canonical one the VM wrote; anything else (a type of
   --  the same name from elsewhere, a different shape) is not an article.
   procedure Describe_Article
     (Outcome : CCL.Language.Interpretation_Result; Page : out Article_View; Found : out Boolean)
   is
      Length : constant Natural := Outcome.Literal.Length;
      P : Positive := 1;
      Ok : Boolean := True;
      Last_Part : Part_Name := Noun;
      Any_Part : Boolean := False;

      function At_End return Boolean is (P > Length);
      function Peek return Character is (if At_End then ASCII.NUL else Outcome.Literal.Data (P));
      procedure Skip_Space is
      begin
         while not At_End and then Peek = ' ' loop P := P + 1; end loop;
      end Skip_Space;
      procedure Expect (C : Character) is
      begin
         Skip_Space;
         if Ok and then Peek = C then P := P + 1; else Ok := False; end if;
      end Expect;
      --  A run of atom characters, compared with Text.
      function Atom return Literal_Span is
      begin
         Skip_Space;
         declare
            Start : constant Positive := P;
         begin
            while not At_End and then Peek not in ' ' | '(' | ')' | '[' | ']' | '"' loop P := P + 1; end loop;
            if P = Start then Ok := False; end if;
            return (First => Start, Last => P - 1);
         end;
      end Atom;
      function Atom_Is (Text : String) return Boolean is
         Span : constant Literal_Span := Atom;
      begin
         return Ok and then Span_Text (Outcome, Span) = Text;
      end Atom_Is;
      procedure Read_String (Span : out Literal_Span) is
      begin
         Span := (1, 0);
         Expect ('"');
         if not Ok then return; end if;
         Span.First := P;
         while not At_End and then Peek /= '"' loop
            if Peek = '\' then P := P + 1; end if;
            P := P + 1;
         end loop;
         Span.Last := Natural'Min (P, Length + 1) - 1;
         Expect ('"');
      end Read_String;
      procedure Read_Number (N : out Natural) is
         Span : constant Literal_Span := Atom;
         Value : constant Long_Long_Integer := Number (Span_Text (Outcome, Span));
      begin
         N := Natural (Long_Long_Integer'Max (0, Long_Long_Integer'Min (Value, Long_Long_Integer (Natural'Last))));
      end Read_Number;
      procedure Add_Line (Line : Article_Line) is
      begin
         if Page.Line_Count = Maximum_Article_Lines then Ok := False; return; end if;
         Page.Line_Count := Page.Line_Count + 1;
         Page.Lines (Page.Line_Count) := Line;
      end Add_Line;
      --  "(list-of String)" or ["a" "b" ...]: each string an Example line, or
      --  together one Words line of Role.
      procedure Read_List (As_Examples : Boolean; Role : Word_Role) is
         Line : Article_Line := (Kind => Words, Role => Role, First_Word => Page.Word_Count + 1, others => <>);
         Item : Literal_Span;
      begin
         Skip_Space;
         if Peek = '(' then
            Expect ('(');
            if not Atom_Is ("list-of") or else not Atom_Is ("String") then Ok := False; end if;
            Expect (')');
            return;
         end if;
         Expect ('[');
         loop
            Skip_Space;
            exit when not Ok or else At_End or else Peek = ']';
            Read_String (Item);
            exit when not Ok;
            if As_Examples then
               Add_Line ((Kind => Example, Text => Item, others => <>));
            elsif Page.Word_Count < Maximum_Article_Words then
               Page.Word_Count := Page.Word_Count + 1;
               Page.Words (Page.Word_Count) := Item;
               Line.Word_Count := Line.Word_Count + 1;
            else
               Ok := False;
            end if;
         end loop;
         Expect (']');
         if Ok and then Line.Word_Count > 0 then Add_Line (Line); end if;
      end Read_List;
      procedure Read_Part (Part : out Part_Name) is
         Span : constant Literal_Span := Atom;
         Text : constant String := Span_Text (Outcome, Span);
         PREFIX : constant String := "Part_Of_Speech.";
      begin
         Part := Noun;
         if Text'Length <= PREFIX'Length or else Text (Text'First .. Text'First + PREFIX'Length - 1) /= PREFIX then
            Ok := False;
            return;
         end if;
         declare
            Member : constant String := Text (Text'First + PREFIX'Length .. Text'Last);
         begin
            if Member = "Noun" then Part := Noun;
            elsif Member = "Verb" then Part := Verb;
            elsif Member = "Adjective" then Part := Adjective;
            elsif Member = "Adverb" then Part := Adverb;
            else Ok := False;
            end if;
         end;
      end Read_Part;
      --  One Word_Sense or Synonym_Group.
      procedure Read_Sense (Group : Boolean) is
         Part : Part_Name;
         Meaning : Literal_Span;
      begin
         Expect ('(');
         if not Atom_Is (if Group then "Synonym_Group" else "Word_Sense") then Ok := False; return; end if;
         Read_Part (Part);
         Read_String (Meaning);
         if not Ok then return; end if;
         if not Any_Part or else Part /= Last_Part then
            Add_Line ((Kind => Heading, Part => Part, others => <>));
            Last_Part := Part;
            Any_Part := True;
         end if;
         Page.Shown := Page.Shown + 1;
         Add_Line ((Kind => Numbered, Text => Meaning, Number => Page.Shown, others => <>));
         if Group then
            Read_List (False, Synonyms);
            Read_List (False, Antonyms);
            Read_List (False, Related);
            Read_List (False, Broader);
         else
            Read_List (True, Synonyms);
            Read_List (False, Synonyms);
            Read_List (False, Antonyms);
         end if;
         Expect (')');
      end Read_Sense;
      procedure Read_Senses (Group : Boolean) is
      begin
         Skip_Space;
         if Peek = '(' then
            Expect ('(');
            if not Atom_Is ("list-of") then Ok := False; end if;
            if Ok then
               declare
                  Ignore : constant Literal_Span := Atom;
               begin
                  null;
               end;
            end if;
            Expect (')');
            return;
         end if;
         Expect ('[');
         loop
            Skip_Space;
            exit when not Ok or else At_End or else Peek = ']';
            Read_Sense (Group);
         end loop;
         Expect (']');
      end Read_Senses;
      procedure Read_Not_Found is
         Word : Literal_Span;
      begin
         Expect ('(');
         if not Atom_Is ("Near_Spellings") then Ok := False; return; end if;
         Read_String (Word);
         Add_Line ((Kind => Missing, Text => Word, others => <>));
         Read_List (False, Suggestions);
         Expect (')');
      end Read_Not_Found;
      procedure Read_Entry (Group : Boolean) is
         Word : Literal_Span;
         First_Number : Natural;
      begin
         Expect ('(');
         if not Atom_Is (if Group then "Thesaurus_Entry" else "Word_Entry") then Ok := False; return; end if;
         Read_String (Word);
         Add_Line ((Kind => Headword, Text => Word, others => <>));
         Read_Senses (Group);
         Read_List (False, Forms);
         Read_Number (First_Number);
         Read_Number (Page.Total);
         Expect (')');
         --  Number the senses from the first shown.
         Page.First_Shown := First_Number;
         for L in 1 .. Page.Line_Count loop
            if Page.Lines (L).Kind = Numbered and then First_Number > 0 then
               Page.Lines (L).Number := Page.Lines (L).Number + First_Number - 1;
            end if;
         end loop;
      end Read_Entry;
      procedure Read_Synonym_List is
         Word : Literal_Span;
      begin
         Expect ('(');
         if not Atom_Is ("Synonym_List") then Ok := False; return; end if;
         Read_String (Word);
         Add_Line ((Kind => Headword, Text => Word, others => <>));
         Read_List (False, Synonyms);
         Read_List (False, Antonyms);
         Expect (')');
      end Read_Synonym_List;
      Choice : Literal_Span;
   begin
      Page := (others => <>);
      Found := False;
      if not Outcome.Has_Literal or else Length = 0 then return; end if;
      Expect ('(');
      Choice := Atom;
      if not Ok then return; end if;
      declare
         C : constant String := Span_Text (Outcome, Choice);
      begin
         if C = "Definition.Found" then
            Page.Command := Dict_Command;
            Read_Entry (Group => False);
         elsif C = "Thesaurus_Result.Found" then
            Page.Command := Thesaurus_Command;
            Read_Entry (Group => True);
         elsif C = "Synonyms.Found" then
            Page.Command := Synonym_Command;
            Read_Synonym_List;
         elsif C = "Definition.Not_Found" or else C = "Thesaurus_Result.Not_Found" or else C = "Synonyms.Not_Found" then
            Page.Command := (if C (C'First) = 'D' then Dict_Command
                             elsif C (C'First) = 'T' then Thesaurus_Command else Synonym_Command);
            Read_Not_Found;
         else
            return;
         end if;
      end;
      Expect (')');
      Skip_Space;
      Found := Ok and then At_End and then Page.Line_Count > 0;
      if not Found then Page := (others => <>); end if;
   end Describe_Article;

   procedure Describe
     (Outcome : CCL.Language.Interpretation_Result; Result : out Presentation)
   is
      Is_Article : Boolean;
   begin
      Result := (others => <>);
      if Outcome.Status /= CCL.Language.Succeeded then
         return;
      end if;
      Result.Kind := Text;
      Describe_Article (Outcome, Result.Page, Is_Article);
      if Is_Article then
         Result.Kind := Article;
         return;
      end if;
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
