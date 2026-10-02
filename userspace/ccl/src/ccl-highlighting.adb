with CCL.Language;

package body CCL.Highlighting with SPARK_Mode is
   function Space (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.CR | ASCII.LF);
   function Line_End (C : Character) return Boolean is
     (C in ASCII.CR | ASCII.LF);
   function Word_Character (C : Character) return Boolean is
     (not Space (C) and then C not in '(' | ')' | '"' | '#');

   function Is_Special_Form (Word : String) return Boolean is
     (Word = "define" or else Word = "type" or else Word = "let" or else
      Word = "if" or else Word = "match" or else Word = "fn" or else
      Word = "handler" or else Word = "field" or else Word = "list" or else
      Word = "list-of" or else Word = "->>" or else Word = "and" or else
      Word = "or" or else Word = "not");

   --  The core operators of CCL.Language plus its named built-ins.
   function Is_Operator (Word : String) return Boolean is
     (Word = "+" or else Word = "-" or else Word = "*" or else Word = "/" or else
      Word = "%" or else Word = "=" or else Word = "/=" or else Word = "<" or else
      Word = "<=" or else Word = ">" or else Word = ">=" or else
      Word = "add" or else Word = "subtract" or else Word = "multiply" or else
      Word = "divide" or else Word = "mod" or else Word = "modulo" or else
      Word = "equal" or else Word = "not-equal" or else Word = "less" or else
      Word = "less-equal" or else Word = "greater" or else
      Word = "greater-equal" or else Word = "at" or else Word = "concat" or else
      Word = "length" or else Word = "to-string" or else
      (for some Operation in CCL.Language.Builtin_Operation =>
         Operation /= CCL.Language.No_Builtin and then
         CCL.Language.Builtin_Name (Operation) = Word));

   function Is_Number (Word : String) return Boolean is
     (Word'Length > 0 and then
      (if Word (Word'First) = '-' then
         Word'Length > 1 and then
         (for all C of Word (Word'First + 1 .. Word'Last) => C in '0' .. '9')
       else (for all C of Word => C in '0' .. '9')));

   function Word_Class (Word : String; Head : Boolean) return Token_Class is
     (if Is_Number (Word) then Number
      elsif Word = "true" or else Word = "false" then Boolean_Literal
      elsif Is_Special_Form (Word) then Special_Form
      elsif Is_Operator (Word) then Operator
      elsif (for some C of Word => C = '.') then Host_Operation
      elsif Word'Length > 0 and then Word (Word'First) in 'A' .. 'Z' then Type_Name
      elsif Head then Call_Name
      else Name)
   with Pre => Word'Length > 0;

   procedure Classify (Source : String; Marks : out Mark_Map) is
      Depth : Natural := 0;
      Position : Natural := 0;  --  bytes classified so far
      Head : Boolean := False;  --  the next word is in operator position
      function At_Offset (Offset : Positive) return Character is
        (Source (Source'First + (Offset - 1)))
      with Pre => Offset <= Source'Length;
   begin
      Marks := [others => (Whitespace, 0)];
      while Position < Source'Length loop
         pragma Loop_Variant (Increases => Position);
         pragma Loop_Invariant (Position < Source'Length);
         declare
            First : constant Positive := Position + 1;
            C : constant Character := At_Offset (First);
            Last : Positive := First;
         begin
            if Space (C) then
               null;
            elsif C = '#' then
               while Last < Source'Length and then not Line_End (At_Offset (Last + 1)) loop
                  pragma Loop_Variant (Increases => Last);
                  pragma Loop_Invariant (Last in First .. Source'Length - 1);
                  Last := Last + 1;
               end loop;
               for I in First .. Last loop
                  Marks (I) := (Comment, 0);
               end loop;
            elsif C = '(' then
               Marks (First) := (Delimiter, Natural'Min (Depth, Maximum_Depth));
               if Depth < Source'Length then Depth := Depth + 1; end if;
               Head := True;
            elsif C = ')' then
               if Depth = 0 then
                  Marks (First) := (Mismatch, 0);
               else
                  Depth := Depth - 1;
                  Marks (First) := (Delimiter, Natural'Min (Depth, Maximum_Depth));
               end if;
               Head := False;
            elsif C = '"' then
               declare
                  Closed : Boolean := False;
                  Escaped : Boolean := False;
               begin
                  while Last < Source'Length and then not Closed and then
                    not Line_End (At_Offset (Last + 1))
                  loop
                     pragma Loop_Variant (Increases => Last);
                     pragma Loop_Invariant (Last in First .. Source'Length - 1);
                     Last := Last + 1;
                     if Escaped then
                        Escaped := False;
                     elsif At_Offset (Last) = '\' then
                        Escaped := True;
                     elsif At_Offset (Last) = '"' then
                        Closed := True;
                     end if;
                  end loop;
                  for I in First .. Last loop
                     Marks (I) := ((if Closed then Text_Literal else Unterminated), 0);
                  end loop;
               end;
               Head := False;
            else
               while Last < Source'Length and then Word_Character (At_Offset (Last + 1)) loop
                  pragma Loop_Variant (Increases => Last);
                  pragma Loop_Invariant (Last in First .. Source'Length - 1);
                  Last := Last + 1;
               end loop;
               declare
                  Class : constant Token_Class := Word_Class
                    (Source (Source'First + (First - 1) .. Source'First + (Last - 1)), Head);
               begin
                  for I in First .. Last loop
                     Marks (I) := (Class, 0);
                  end loop;
               end;
               Head := False;
            end if;
            Position := Last;
         end;
      end loop;
   end Classify;

   --  One pass shared by Balance and Open_Depth.
   procedure Scan
     (Source : String; Depth : out Natural; Stray, Open_Text, Content : out Boolean)
   is
      Quoted, Escaped, Comment : Boolean := False;
   begin
      Depth := 0;
      Stray := False;
      Content := False;
      for C of Source loop
         pragma Loop_Invariant (Depth <= Source'Length);
         if Comment then
            if Line_End (C) then Comment := False; end if;
         elsif Quoted then
            if Escaped then Escaped := False;
            elsif C = '\' then Escaped := True;
            elsif C = '"' then Quoted := False;
            elsif Line_End (C) then Stray := True; Quoted := False;
            end if;
         elsif C = '#' then Comment := True;
         elsif C = '"' then Quoted := True; Content := True;
         elsif C = '(' then
            if Depth < Source'Length then Depth := Depth + 1; end if;
            Content := True;
         elsif C = ')' then
            if Depth = 0 then Stray := True; else Depth := Depth - 1; end if;
            Content := True;
         elsif not Space (C) then
            Content := True;
         end if;
      end loop;
      Open_Text := Quoted;
   end Scan;

   function Balance (Source : String) return Form_State is
      Depth : Natural;
      Stray, Open_Text, Content : Boolean;
   begin
      Scan (Source, Depth, Stray, Open_Text, Content);
      return (if not Content then Empty
              elsif Stray or else Open_Text then Malformed
              elsif Depth > 0 then Open_Forms
              else Complete);
   end Balance;

   function Open_Depth (Source : String) return Natural is
      Depth : Natural;
      Stray, Open_Text, Content : Boolean;
   begin
      Scan (Source, Depth, Stray, Open_Text, Content);
      return (if Stray or else Open_Text then 0 else Depth);
   end Open_Depth;
end CCL.Highlighting;
