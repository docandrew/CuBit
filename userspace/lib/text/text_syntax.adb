package body Text_Syntax with SPARK_Mode is
   MAXIMUM_WORD : constant := 32;

   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) + 32) else C);

   function Ends_With (Name, Suffix : String) return Boolean is
     (Name'Length >= Suffix'Length and then Suffix'Length > 0 and then Name'Last >= Suffix'Length
      and then (for all K in 0 .. Suffix'Length - 1 =>
                  Lower (Name (Name'Last - K)) = Suffix (Suffix'Last - K)))
     with Pre => Suffix'Last < Positive'Last;

   function Language_Of (Name : String) return Language is
     (if Ends_With (Name, ".adb") or else Ends_With (Name, ".ads") or else Ends_With (Name, ".ada")
         or else Ends_With (Name, ".gpr") then Ada_Source
      elsif Ends_With (Name, ".ccl") or else Ends_With (Name, ".ccl-interface") then CCL_Source
      elsif Ends_With (Name, ".c") or else Ends_With (Name, ".h") or else Ends_With (Name, ".cc")
         or else Ends_With (Name, ".cpp") or else Ends_With (Name, ".hpp") then C_Source
      elsif Ends_With (Name, ".rs") then Rust_Source
      elsif Ends_With (Name, ".md") or else Ends_With (Name, ".markdown") then Markdown
      else Plain_Text);

   function Name_Of (Item : Language) return String is
     (case Item is
         when Plain_Text => "Plain text", when Ada_Source => "Ada/SPARK", when CCL_Source => "CCL",
         when C_Source => "C", when Rust_Source => "Rust", when Markdown => "Markdown");

   function Comment_Prefix (Item : Language) return String is
     (case Item is
         when Ada_Source => "--", when CCL_Source => "#", when C_Source | Rust_Source => "//",
         when Plain_Text | Markdown => "");

   function Is_Letter (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z' | '_');
   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Is_Word_Character (C : Character) return Boolean is
     (Is_Letter (C) or else Is_Digit (C) or else Character'Pos (C) >= 16#80#);

   function Is_Ada_Keyword (W : String) return Boolean is
     (W = "abort" or else W = "abs" or else W = "abstract" or else W = "accept" or else W = "access"
      or else W = "aliased" or else W = "all" or else W = "and" or else W = "array" or else W = "at"
      or else W = "begin" or else W = "body" or else W = "case" or else W = "constant" or else W = "declare"
      or else W = "delay" or else W = "delta" or else W = "digits" or else W = "do" or else W = "else"
      or else W = "elsif" or else W = "end" or else W = "entry" or else W = "exception" or else W = "exit"
      or else W = "for" or else W = "function" or else W = "generic" or else W = "goto" or else W = "if"
      or else W = "in" or else W = "interface" or else W = "is" or else W = "limited" or else W = "loop"
      or else W = "mod" or else W = "new" or else W = "not" or else W = "null" or else W = "of"
      or else W = "or" or else W = "others" or else W = "out" or else W = "overriding" or else W = "package"
      or else W = "parallel" or else W = "pragma" or else W = "private" or else W = "procedure"
      or else W = "protected" or else W = "raise" or else W = "range" or else W = "record" or else W = "rem"
      or else W = "renames" or else W = "requeue" or else W = "return" or else W = "reverse"
      or else W = "select" or else W = "separate" or else W = "some" or else W = "subtype"
      or else W = "synchronized" or else W = "tagged" or else W = "task" or else W = "terminate"
      or else W = "then" or else W = "type" or else W = "until" or else W = "use" or else W = "when"
      or else W = "while" or else W = "with" or else W = "xor");

   function Is_Ada_Type (W : String) return Boolean is
     (W = "boolean" or else W = "integer" or else W = "natural" or else W = "positive" or else W = "string"
      or else W = "character" or else W = "float" or else W = "duration" or else W = "long_integer"
      or else W = "wide_character" or else W = "wide_string" or else W = "unsigned_8"
      or else W = "unsigned_16" or else W = "unsigned_32" or else W = "unsigned_64" or else W = "address");

   --  SPARK aspects and pragmas worth seeing at a glance.
   function Is_Ada_Aspect (W : String) return Boolean is
     (W = "pre" or else W = "post" or else W = "spark_mode" or else W = "ghost" or else W = "loop_invariant"
      or else W = "loop_variant" or else W = "type_invariant" or else W = "dynamic_predicate"
      or else W = "static_predicate" or else W = "contract_cases" or else W = "global" or else W = "depends"
      or else W = "assert" or else W = "assume" or else W = "subprogram_variant" or else W = "pure"
      or else W = "inline" or else W = "import" or else W = "export" or else W = "convention"
      or else W = "initializes" or else W = "abstract_state" or else W = "refined_state");

   function Is_C_Keyword (W : String) return Boolean is
     (W = "auto" or else W = "break" or else W = "case" or else W = "const" or else W = "continue"
      or else W = "default" or else W = "do" or else W = "else" or else W = "enum" or else W = "extern"
      or else W = "for" or else W = "goto" or else W = "if" or else W = "inline" or else W = "register"
      or else W = "restrict" or else W = "return" or else W = "sizeof" or else W = "static"
      or else W = "struct" or else W = "switch" or else W = "typedef" or else W = "union"
      or else W = "volatile" or else W = "while" or else W = "_Static_assert" or else W = "_Alignof"
      or else W = "NULL" or else W = "true" or else W = "false");

   function Is_C_Type (W : String) return Boolean is
     (W = "void" or else W = "char" or else W = "short" or else W = "int" or else W = "long" or else W = "float"
      or else W = "double" or else W = "signed" or else W = "unsigned" or else W = "bool" or else W = "_Bool"
      or else W = "size_t" or else W = "ssize_t" or else W = "uint8_t" or else W = "uint16_t"
      or else W = "uint32_t" or else W = "uint64_t" or else W = "int8_t" or else W = "int16_t"
      or else W = "int32_t" or else W = "int64_t" or else W = "uintptr_t" or else W = "FILE");

   function Is_Rust_Keyword (W : String) return Boolean is
     (W = "as" or else W = "async" or else W = "await" or else W = "break" or else W = "const"
      or else W = "continue" or else W = "crate" or else W = "dyn" or else W = "else" or else W = "enum"
      or else W = "extern" or else W = "false" or else W = "fn" or else W = "for" or else W = "if"
      or else W = "impl" or else W = "in" or else W = "let" or else W = "loop" or else W = "match"
      or else W = "mod" or else W = "move" or else W = "mut" or else W = "pub" or else W = "ref"
      or else W = "return" or else W = "self" or else W = "static" or else W = "struct" or else W = "super"
      or else W = "trait" or else W = "true" or else W = "type" or else W = "unsafe" or else W = "use"
      or else W = "where" or else W = "while" or else W = "yield");

   function Is_Rust_Type (W : String) return Boolean is
     (W = "i8" or else W = "i16" or else W = "i32" or else W = "i64" or else W = "i128" or else W = "isize"
      or else W = "u8" or else W = "u16" or else W = "u32" or else W = "u64" or else W = "u128"
      or else W = "usize" or else W = "f32" or else W = "f64" or else W = "bool" or else W = "char"
      or else W = "str" or else W = "String" or else W = "Vec" or else W = "Option" or else W = "Result"
      or else W = "Box" or else W = "Self" or else W = "Some" or else W = "None" or else W = "Ok"
      or else W = "Err");

   function Is_CCL_Keyword (W : String) return Boolean is
     (W = "define" or else W = "fn" or else W = "let" or else W = "if" or else W = "match" or else W = "and"
      or else W = "or" or else W = "not" or else W = "type" or else W = "record" or else W = "enum"
      or else W = "interface" or else W = "cond" or else W = "do" or else W = "when" or else W = "lambda"
      or else W = "import" or else W = "true" or else W = "false" or else W = "field" or else W = "list");

   procedure Lex_Line
     (Item : Language; Text : String; Start : Line_State; Spans : out Span_Array; Count : out Span_Count;
      Finish : out Line_State)
   is
      --  A 1-based copy whose bounds every nested procedure knows.
      Last : constant Segment_Length := Text'Length;
      Line : constant String (1 .. Last) := Text;
      I : Positive := 1;
      State : Line_State := Start;
      --  The last emitted span's end (spans never overlap).
      Emitted_To : Natural := 0;

      --  The spans so far: in the line, each at least one byte, in order,
      --  none after Emitted_To.
      function Spans_Valid return Boolean is
        (Emitted_To <= Last
         and then (for all K in 1 .. Count => Spans (K).Length >= 1
                   and then Spans (K).First + Spans (K).Length - 1 <= Emitted_To)
         and then (for all K in 2 .. Count => Spans (K).First >= Spans (K - 1).First + Spans (K - 1).Length));

      procedure Emit (First, Final : Natural; Kind : Token_Kind)
        with Pre => Final <= Last and then Spans_Valid,
             Post => Spans_Valid and then (Emitted_To = Emitted_To'Old or else Emitted_To = Final)
                     and then Emitted_To >= Emitted_To'Old
      is
      begin
         if First > Emitted_To and then First >= 1 and then Final >= First and then Count < MAXIMUM_SPANS
           and then Kind /= Plain
         then
            Count := Count + 1;
            Spans (Count) := (First => First, Length => Final - First + 1, Kind => Kind);
            Emitted_To := Final;
         end if;
      end Emit;

      --  The end of the word starting at From.
      function Word_End (From : Positive) return Positive
        with Pre => From <= Last, Post => Word_End'Result in From .. Last
      is
         E : Positive := From;
      begin
         while E < Last and then Is_Word_Character (Line (E + 1)) loop
            pragma Loop_Invariant (E in From .. Last - 1);
            pragma Loop_Variant (Increases => E);
            E := E + 1;
         end loop;
         return E;
      end Word_End;

      --  The word as compared: lower case for Ada, as written otherwise.
      function Word (From, To : Positive; Fold : Boolean) return String
        with Pre => From <= To and then To <= Last
      is
         Result : String (1 .. Natural'Min (To - From + 1, MAXIMUM_WORD)) := [others => ' '];
      begin
         if To - From + 1 > MAXIMUM_WORD then
            return "";
         end if;
         for K in Result'Range loop
            Result (K) := (if Fold then Lower (Line (From + K - 1)) else Line (From + K - 1));
         end loop;
         return Result;
      end Word;

      --  The end of a number from From: digits, letters, '_', '.', '#'
      --  (Ada based literals), and an exponent's sign.
      function Number_End (From : Positive) return Positive
        with Pre => From <= Last, Post => Number_End'Result in From .. Last
      is
         E : Positive := From;
      begin
         while E < Last
           and then (Is_Word_Character (Line (E + 1)) or else Line (E + 1) = '#'
                     or else (Line (E + 1) = '.' and then E + 1 < Last and then Is_Digit (Line (E + 2)))
                     or else (Line (E + 1) in '+' | '-' and then Line (E) in 'e' | 'E'))
         loop
            pragma Loop_Invariant (E in From .. Last - 1);
            pragma Loop_Variant (Increases => E);
            E := E + 1;
         end loop;
         return E;
      end Number_End;

      --  Scan a quoted run from Scan (the first character after the
      --  opening quote, or the line's start for a run continued from the
      --  line before): Final is the closing quote, or Last when it does
      --  not close on this line. Backslash escapes (Escapes), or doubled
      --  quotes (Ada).
      procedure Scan_String
        (Scan : Positive; Quote : Character; Escapes : Boolean; Final : out Natural; Closed : out Boolean)
        with Pre => Scan <= Last + 1, Post => Final in Scan - 1 .. Last and then (if Closed then Final >= Scan)
      is
         E : Positive := Scan;
      begin
         Closed := False;
         Final := Last;
         while E <= Last loop
            pragma Loop_Invariant (E in Scan .. Last);
            pragma Loop_Variant (Increases => E);
            if Escapes and then Line (E) = '\' then
               exit when E >= Last - 1;
               E := E + 1;
            elsif Line (E) = Quote then
               if not Escapes and then E < Last and then Line (E + 1) = Quote then
                  exit when E >= Last - 1;
                  E := E + 1;
               else
                  Closed := True;
                  Final := E;
                  return;
               end if;
            end if;
            exit when E = Last;
            E := E + 1;
         end loop;
      end Scan_String;

      --  Scan a block comment from Scan: Final is its closing '/', or Last.
      --  Nested (Rust) comments count their depth in State.
      procedure Scan_Comment (Scan : Positive; Nested : Boolean; Final : out Natural; Closed : out Boolean)
        with Pre => Scan <= Last + 1, Post => Final in Scan - 1 .. Last and then (if Closed then Final >= Scan)
      is
         E : Positive := Scan;
      begin
         Closed := False;
         Final := Last;
         while E < Last loop
            pragma Loop_Invariant (E in Scan .. Last - 1);
            pragma Loop_Variant (Increases => E);
            if Line (E) = '*' and then Line (E + 1) = '/' then
               if State.Depth > 0 then
                  State.Depth := State.Depth - 1;
               end if;
               if State.Depth = 0 or else not Nested then
                  Closed := True;
                  Final := E + 1;
                  return;
               end if;
               exit when E + 1 >= Last;
               E := E + 1;
            elsif Nested and then Line (E) = '/' and then Line (E + 1) = '*' then
               if State.Depth < MAXIMUM_NESTING then
                  State.Depth := State.Depth + 1;
               end if;
               exit when E + 1 >= Last;
               E := E + 1;
            end if;
            E := E + 1;
         end loop;
      end Scan_Comment;

      --  Scan a raw string's body from Scan for '"' and State.Hashes '#'s.
      procedure Scan_Raw (Scan : Positive; Final : out Natural; Closed : out Boolean)
        with Pre => Scan <= Last + 1, Post => Final in Scan - 1 .. Last and then (if Closed then Final >= Scan)
      is
      begin
         Closed := False;
         Final := Last;
         for E in Scan .. Last loop
            if Line (E) = '"' and then State.Hashes <= Last - E
              and then (for all H in 1 .. State.Hashes => Line (E + H) = '#')
            then
               Closed := True;
               Final := E + State.Hashes;
               return;
            end if;
         end loop;
      end Scan_Raw;

      procedure Lex_Ada with Pre => I = 1 and then Last >= 1 and then Emitted_To = 0 and then Count = 0,
                             Post => Spans_Valid is
         --  The last significant character: a tick after a name or ')' is
         --  an attribute, otherwise a character literal.
         Previous : Character := ' ';
      begin
         while I <= Last loop
            pragma Loop_Invariant (I in 1 .. Last and then Emitted_To < I and then Spans_Valid);
            pragma Loop_Variant (Increases => I);
            declare
               C : constant Character := Line (I);
               Next : Positive := I + 1;
            begin
               if C = '-' and then I < Last and then Line (I + 1) = '-' then
                  Emit (I, Last, Comment);
                  Next := Last + 1;
               elsif C = '"' then
                  declare
                     Final : Natural;
                     Closed : Boolean;
                  begin
                     Scan_String (I + 1, '"', False, Final, Closed);
                     Emit (I, Natural'Max (Final, I), String_Text);
                     Next := Natural'Max (Final, I) + 1;
                  end;
               elsif C = ''' then
                  if I + 2 <= Last and then Line (I + 2) = ''' and then not Is_Word_Character (Previous)
                    and then Previous /= ')'
                  then
                     Emit (I, I + 2, Character_Text);
                     Next := I + 3;
                  elsif I < Last and then Is_Letter (Line (I + 1)) then
                     declare
                        E : constant Positive := Word_End (I + 1);
                     begin
                        Emit (I, E, Attribute);
                        Next := E + 1;
                     end;
                  end if;
               elsif Is_Digit (C) then
                  declare
                     E : constant Positive := Number_End (I);
                  begin
                     Emit (I, E, Number);
                     Next := E + 1;
                  end;
               elsif Is_Letter (C) then
                  declare
                     E : constant Positive := Word_End (I);
                     W : constant String := Word (I, E, Fold => True);
                  begin
                     if Is_Ada_Keyword (W) then
                        Emit (I, E, Keyword);
                     elsif Is_Ada_Type (W) then
                        Emit (I, E, Type_Name);
                     elsif Is_Ada_Aspect (W) then
                        Emit (I, E, Attribute);
                     end if;
                     Next := E + 1;
                  end;
               elsif C in ':' | '=' | '<' | '>' | '/' | '+' | '*' | '&' | '|' | '-' then
                  Emit (I, I, Operator);
               end if;
               if Next > I + 1 and then Next - 1 <= Last then
                  Previous := Line (Next - 1);
               elsif C /= ' ' then
                  Previous := C;
               end if;
               exit when Next > Last;
               I := Next;
            end;
         end loop;
      end Lex_Ada;

      --  C, Rust and CCL share the shape: comments, strings, numbers, words.
      procedure Lex_C_Like with Pre => I = 1 and then Last >= 1 and then Emitted_To = 0 and then Count = 0,
                             Post => Spans_Valid is
         Final : Natural;
         Closed : Boolean;
      begin
         --  What continues from the line before (CCL has no block comments
         --  or long strings: a stale state from another language resets).
         if Item = CCL_Source then
            State := INITIAL_STATE;
         end if;
         case State.Mode is
            when Block_Comment =>
               Scan_Comment (1, Item = Rust_Source, Final, Closed);
               Emit (1, Final, Comment);
               if not Closed or else Final >= Last then
                  if Closed then
                     State := INITIAL_STATE;
                  end if;
                  return;
               end if;
               State := INITIAL_STATE;
               I := Final + 1;
            when Long_String =>
               Scan_String (1, '"', True, Final, Closed);
               Emit (1, Final, String_Text);
               if not Closed or else Final >= Last then
                  if Closed then
                     State := INITIAL_STATE;
                  end if;
                  return;
               end if;
               State := INITIAL_STATE;
               I := Final + 1;
            when Raw_String =>
               Scan_Raw (1, Final, Closed);
               Emit (1, Final, String_Text);
               if not Closed or else Final >= Last then
                  if Closed then
                     State := INITIAL_STATE;
                  end if;
                  return;
               end if;
               State := INITIAL_STATE;
               I := Final + 1;
            when Continued_Directive | Normal | Fenced_Code =>
               null;
         end case;
         --  A directive: '#' first on the line, to its end (and on to the
         --  next line after a backslash).
         if Item = C_Source and then I = 1 then
            declare
               First : Natural := 0;
            begin
               for K in 1 .. Last loop
                  if Line (K) /= ' ' and then Line (K) /= ASCII.HT then
                     First := K;
                     exit;
                  end if;
               end loop;
               if State.Mode = Continued_Directive or else (First >= 1 and then Line (First) = '#') then
                  Emit (1, Last, Preprocessor);
                  State := (if Line (Last) = '\' then (Continued_Directive, 0, 0) else INITIAL_STATE);
                  return;
               end if;
            end;
         end if;
         while I <= Last loop
            pragma Loop_Invariant (I in 1 .. Last and then Emitted_To < I and then Spans_Valid);
            pragma Loop_Variant (Increases => I);
            declare
               C : constant Character := Line (I);
               Next : Positive := I + 1;
            begin
               if Item = CCL_Source and then C = '#' then
                  Emit (I, Last, Comment);
                  Next := Last + 1;
               elsif Item /= CCL_Source and then C = '/' and then I < Last and then Line (I + 1) = '/' then
                  Emit (I, Last, Comment);
                  Next := Last + 1;
               elsif Item /= CCL_Source and then C = '/' and then I < Last and then Line (I + 1) = '*' then
                  State := (Block_Comment, 1, 0);
                  Scan_Comment (I + 2, Item = Rust_Source, Final, Closed);
                  Emit (I, Natural'Max (Final, I + 1), Comment);
                  if Closed then
                     State := INITIAL_STATE;
                  end if;
                  Next := Natural'Max (Final, I + 1) + 1;
               elsif C = '"' then
                  Scan_String (I + 1, '"', True, Final, Closed);
                  Emit (I, Natural'Max (Final, I), String_Text);
                  if not Closed and then Item = Rust_Source then
                     State := (Long_String, 0, 0);
                  end if;
                  Next := Natural'Max (Final, I) + 1;
               elsif Item = Rust_Source and then C = 'r' and then I < Last and then Line (I + 1) in '"' | '#' then
                  --  A raw string: r"..." or r#"..."#.
                  declare
                     Hashes : Nesting := 0;
                     E : Positive := I + 1;
                  begin
                     while E < Last and then Line (E) = '#' and then Hashes < MAXIMUM_NESTING loop
                        pragma Loop_Invariant (E in I + 1 .. Last - 1);
                        pragma Loop_Variant (Increases => E);
                        Hashes := Hashes + 1;
                        E := E + 1;
                     end loop;
                     if Line (E) = '"' then
                        State := (Raw_String, 0, Hashes);
                        Scan_Raw (E + 1, Final, Closed);
                        Emit (I, Natural'Max (Final, E), String_Text);
                        if Closed then
                           State := INITIAL_STATE;
                        end if;
                        Next := Natural'Max (Final, E) + 1;
                     else
                        Next := Word_End (I) + 1;
                     end if;
                  end;
               elsif C = ''' and then Item /= CCL_Source then
                  --  'x' and '\n' are characters; Rust's 'a (a lifetime) is a label.
                  if I + 2 <= Last and then Line (I + 1) /= '\' and then Line (I + 2) = ''' then
                     Emit (I, I + 2, Character_Text);
                     Next := I + 3;
                  elsif I + 3 <= Last and then Line (I + 1) = '\' then
                     Scan_String (I + 1, ''', True, Final, Closed);
                     Emit (I, Natural'Max (Final, I), Character_Text);
                     Next := Natural'Max (Final, I) + 1;
                  elsif Item = Rust_Source and then I < Last and then Is_Letter (Line (I + 1)) then
                     declare
                        E : constant Positive := Word_End (I + 1);
                     begin
                        Emit (I, E, Label);
                        Next := E + 1;
                     end;
                  end if;
               elsif Item = Rust_Source and then C = '#' and then I < Last and then Line (I + 1) in '[' | '!' then
                  declare
                     E : Positive := I;
                  begin
                     while E < Last and then Line (E) /= ']' loop
                        pragma Loop_Invariant (E in I .. Last - 1);
                        pragma Loop_Variant (Increases => E);
                        E := E + 1;
                     end loop;
                     Emit (I, E, Attribute);
                     Next := E + 1;
                  end;
               elsif Is_Digit (C) then
                  declare
                     E : constant Positive := Number_End (I);
                  begin
                     Emit (I, E, Number);
                     Next := E + 1;
                  end;
               elsif Is_Letter (C) then
                  declare
                     E : Positive := Word_End (I);
                  begin
                     if Item = CCL_Source then
                        --  CCL names may hold '-' and '.' (setting keys, Enum.Literal).
                        while E < Last and then (Is_Word_Character (Line (E + 1)) or else Line (E + 1) in '-' | '.')
                        loop
                           pragma Loop_Invariant (E in I .. Last - 1);
                           pragma Loop_Variant (Increases => E);
                           E := E + 1;
                        end loop;
                     end if;
                     declare
                        W : constant String := Word (I, E, Fold => False);
                     begin
                        if (Item = C_Source and then Is_C_Keyword (W))
                          or else (Item = Rust_Source and then Is_Rust_Keyword (W))
                          or else (Item = CCL_Source and then Is_CCL_Keyword (W))
                        then
                           Emit (I, E, Keyword);
                        elsif (Item = C_Source and then Is_C_Type (W))
                          or else (Item = Rust_Source and then Is_Rust_Type (W))
                          or else (Item = CCL_Source and then C in 'A' .. 'Z')
                        then
                           Emit (I, E, Type_Name);
                        elsif Item = Rust_Source and then E < Last and then Line (E + 1) = '!' then
                           Emit (I, E + 1, Preprocessor);
                           E := E + 1;
                        elsif Item = CCL_Source and then E <= Last - 3 and then Line (E + 1) = ' '
                          and then Line (E + 2) = '=' and then Line (E + 3) = '>'
                        then
                           Emit (I, E, Label);
                        end if;
                     end;
                     Next := E + 1;
                  end;
               elsif C in '=' | '<' | '>' | '+' | '*' | '&' | '|' | '!' | '-' | '%' | '^' | '~' | '?' | ':' then
                  Emit (I, I, Operator);
               end if;
               exit when Next > Last;
               I := Next;
            end;
         end loop;
      end Lex_C_Like;

      procedure Lex_Markdown with Pre => I = 1 and then Last >= 1 and then Emitted_To = 0 and then Count = 0,
                             Post => Spans_Valid is
         Indent : Natural := 0;
         Fence : Boolean;
      begin
         while Indent < Last and then Line (Indent + 1) = ' ' loop
            pragma Loop_Invariant (Indent < Last);
            pragma Loop_Variant (Increases => Indent);
            Indent := Indent + 1;
         end loop;
         Fence := Indent + 3 <= Last and then Line (Indent + 1) = '`' and then Line (Indent + 2) = '`'
           and then Line (Indent + 3) = '`';
         if State.Mode = Fenced_Code then
            Emit (1, Last, Code);
            if Fence then
               State := INITIAL_STATE;
            end if;
            return;
         elsif Fence then
            Emit (1, Last, Code);
            State := (Fenced_Code, 0, 0);
            return;
         end if;
         if Indent < Last and then Line (Indent + 1) = '#' then
            Emit (1, Last, Heading);
            return;
         elsif Indent < Last and then Line (Indent + 1) = '>' then
            Emit (1, Last, Quote);
            return;
         elsif Indent + 1 < Last and then Line (Indent + 1) in '-' | '*' | '+' and then Line (Indent + 2) = ' ' then
            Emit (Indent + 1, Indent + 1, List_Marker);
            I := Indent + 2;
         end if;
         while I <= Last loop
            pragma Loop_Invariant (I in 1 .. Last and then Emitted_To < I and then Spans_Valid);
            pragma Loop_Variant (Increases => I);
            declare
               C : constant Character := Line (I);
               Next : Positive := I + 1;
               E : Positive := I;
            begin
               if C = '\' and then I < Last then
                  Emit (I, I + 1, Escape);
                  Next := I + 2;
               elsif C in '`' | '*' | '_' | '[' then
                  declare
                     Strong_Run : constant Boolean := C = '*' and then I < Last and then Line (I + 1) = '*';
                     Close : constant Character := (if C = '[' then ']' else C);
                  begin
                     E := (if Strong_Run then I + 1 else I);
                     while E < Last and then Line (E + 1) /= Close loop
                        pragma Loop_Invariant (E in I .. Last - 1);
                        pragma Loop_Variant (Increases => E);
                        E := E + 1;
                     end loop;
                     if E < Last then
                        E := E + 1;
                        if Strong_Run and then E < Last and then Line (E + 1) = '*' then
                           E := E + 1;
                        end if;
                        if C = '[' and then E < Last and then Line (E + 1) = '(' then
                           while E < Last and then Line (E) /= ')' loop
                              pragma Loop_Invariant (E in I .. Last - 1);
                              pragma Loop_Variant (Increases => E);
                              E := E + 1;
                           end loop;
                        end if;
                        Emit (I, E, (case C is when '`' => Code, when '[' => Link,
                                               when others => (if Strong_Run then Strong else Emphasis)));
                        Next := E + 1;
                     end if;
                  end;
               end if;
               exit when Next > Last;
               I := Next;
            end;
         end loop;
      end Lex_Markdown;
   begin
      Spans := [others => (others => <>)];
      Count := 0;
      Finish := INITIAL_STATE;
      if Last = 0 then
         Finish := (if Start.Mode = Continued_Directive then INITIAL_STATE else Start);
         return;
      end if;
      case Item is
         when Plain_Text => null;
         when Ada_Source => Lex_Ada;
         when C_Source | Rust_Source | CCL_Source =>
            Lex_C_Like;
         when Markdown => Lex_Markdown;
      end case;
      Finish := State;
   end Lex_Line;
end Text_Syntax;
