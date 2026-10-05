package body CCL.Language.Views with SPARK_Mode => On is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   Marker : constant String := BASIC_MARKER;

   function Detect (Source : String) return Surface is
     (if Source'Length >= Marker'Length and then
         Source (Source'First .. Source'First + Marker'Length - 1) = Marker
      then Basic else Lisp);

   procedure Convert
     (Source : String; From, Into : Surface;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out Conversion;
      Check : Boolean := True)
   is
      Input : Text;
      Lowered, Comments : Text;
      type Position_Map is array (Positive range 1 .. Maximum_View_Length)
        of Natural range 0 .. Maximum_View_Length + 1;
      Origins : Position_Map := [others => 0];
      Ends : Position_Map := [others => 0];
      Cursor : Positive range 1 .. Maximum_View_Length + 1 := 1;
      Failed, Full : Boolean := False;
      Analysis : Analysis_Result;
      Flat_Spans : Node_Spans := [others => (others => 0)];
      Canonical_Spans : Node_Spans := [others => (others => 0)];

      procedure Append (Buffer : in out Text; Value : String) is
      begin
         if Value'Length > Maximum_View_Length - Buffer.Length then
            Full := True;
         else
            Buffer.Data (Buffer.Length + 1 .. Buffer.Length + Value'Length) := Value;
            Buffer.Length := Buffer.Length + Value'Length;
         end if;
      end Append;

      procedure Put (Value : String; Origin : Natural) is
         Before : constant View_Length := Lowered.Length;
      begin
         Append (Lowered, Value);
         for I in Before + 1 .. Lowered.Length loop
            Origins (I) := Origin;
            Ends (I) := Natural'Min (Origin + 1, Maximum_View_Length + 1);
         end loop;
      end Put;

      function White (C : Character) return Boolean is
        (C in ' ' | ASCII.HT | ASCII.CR | ASCII.LF);

      function Name_Character (C : Character) return Boolean is
        (C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' |
          '-' | '_' | '.' | '?' | '+' | '=' | '*' | '/' | '%' | '<' | '>');

      function Basic_Name_Character (C : Character) return Boolean is
        (Name_Character (C) and then
         C not in '+' | '=' | '*' | '/' | '%' | '<' | '>');

      function Keyword (S : String) return Boolean is
        (S = "LET" or S = "IN" or S = "END" or S = "IF" or
         S = "THEN" or S = "ELSE" or S = "MOD" or S = "FUNCTION" or
         S = "AS" or S = "RETURN" or S = "TYPE" or S = "VARIANT" or S = "RECORD" or
         S = "MATCH" or S = "CASE" or S = "AND" or S = "OR");

      --  BASIC precedence, lowest first: OR, AND, comparisons, + and -,
      --  then *, / and MOD. A binary minus needs whitespace before it, since
      --  names may contain hyphens (a-b is a name; a - b subtracts).
      type Infix_Operator is (No_Operator, Disjunction, Conjunction, Equality,
                              Inequality, Less, Less_Equal, Greater,
                              Greater_Equal, Addition, Subtraction,
                              Multiplication, Division, Modulo);
      subtype Precedence is Natural range 0 .. 6;
      function Priority (Op : Infix_Operator) return Precedence is
        (case Op is when No_Operator => 0, when Disjunction => 1,
         when Conjunction => 2,
         when Equality | Inequality | Less | Less_Equal | Greater |
              Greater_Equal => 3,
         when Addition | Subtraction => 4, when others => 5);
      function Operator_For (Kind : Node_Kind) return Infix_Operator is
        (case Kind is when Equal_Form => Equality,
         when Not_Equal_Form => Inequality, when Less_Form => Less,
         when Less_Equal_Form => Less_Equal, when Greater_Form => Greater,
         when Greater_Equal_Form => Greater_Equal,
         when And_Form => Conjunction, when Or_Form => Disjunction,
         when Add_Form => Addition, when Subtract_Form => Subtraction,
         when Multiply_Form => Multiplication, when Divide_Form => Division,
         when Modulo_Form => Modulo, when others => No_Operator);
      function Spelling (Op : Infix_Operator; Style : Surface) return String is
        (case Op is when No_Operator => "", when Equality => "=",
         when Inequality => (if Style = Lisp then "/=" else "<>"),
         when Less => "<", when Less_Equal => "<=", when Greater => ">",
         when Greater_Equal => ">=",
         when Conjunction => (if Style = Lisp then "and" else "AND"),
         when Disjunction => (if Style = Lisp then "or" else "OR"),
         when Addition => "+", when Subtraction => "-",
         when Multiplication => "*", when Division => "/",
         when Modulo => (if Style = Lisp then "%" else "MOD"));

      procedure Skip is
         Start : Positive;
      begin
         while Cursor <= Input.Length loop
            if White (Input.Data (Cursor)) then
               Cursor := Cursor + 1;
            elsif Input.Data (Cursor) = '#' then
               Start := Cursor;
               while Cursor <= Input.Length and then
                 Input.Data (Cursor) not in ASCII.CR | ASCII.LF
               loop
                  Cursor := Cursor + 1;
               end loop;
               if Input.Data (Start .. Cursor - 1) /= Marker then
                  Append (Comments, Input.Data (Start .. Cursor - 1));
                  Append (Comments, String'(1 => ASCII.LF));
               end if;
            else
               exit;
            end if;
         end loop;
      end Skip;

      procedure Expect (Value : String) is
      begin
         Skip;
         if Value'Length <= Input.Length - (Cursor - 1) and then
           Input.Data (Cursor .. Cursor + Value'Length - 1) = Value and then
           (not Keyword (Value) or else Cursor + Value'Length > Input.Length or else
            not Basic_Name_Character (Input.Data (Cursor + Value'Length)))
         then
            Cursor := Cursor + Value'Length;
         else
            Failed := True;
         end if;
      end Expect;

      --  Backticks escape BASIC's contextual keyword or delimiter-bearing
      --  names. The Lisp analyzer still decides whether the name is legal.
      --  An integer literal: optional '-', then digits.
      procedure Number_Token (Value : out Text) is
         Start : Positive;
      begin
         Value := (others => <>);
         Skip;
         Start := Cursor;
         if Cursor <= Input.Length and then Input.Data (Cursor) = '-' then
            Cursor := Cursor + 1;
         end if;
         while Cursor <= Input.Length and then Input.Data (Cursor) in '0' .. '9' loop
            Cursor := Cursor + 1;
         end loop;
         if Cursor = Start then
            Failed := True;
            return;
         end if;
         Value.Length := Cursor - Start;
         Value.Data (1 .. Value.Length) := Input.Data (Start .. Cursor - 1);
      end Number_Token;

      procedure Name_Token (Value : out Text) is
         Start : Positive;
         Escaped : Boolean;
      begin
         Value := (others => <>);
         Skip;
         Escaped := Cursor <= Input.Length and then Input.Data (Cursor) = '`';
         if Escaped then Cursor := Cursor + 1; end if;
         Start := Cursor;
         while Cursor <= Input.Length loop
            exit when (if Escaped then Input.Data (Cursor) = '`'
              else White (Input.Data (Cursor)) or else
                Input.Data (Cursor) in '(' | ')' | '[' | ']' | ',' | '=' | '+' | '*' |
                  '/' | '%' | '<' | '>' | '#' | '"' | '`');
            Cursor := Cursor + 1;
         end loop;
         Append (Value, Input.Data (Start .. Cursor - 1));
         if Value.Length = 0 then Failed := True; end if;
         --  Never splice arbitrary quoted source into the Lisp reader.
         for C of Value.Data (1 .. Value.Length) loop
            if not Name_Character (C)
            then Failed := True; end if;
         end loop;
         if Escaped then Expect ("`"); end if;
      end Name_Token;

      procedure Expression (Depth : Natural; Minimum : Precedence := 1);
      procedure Basic_Type (Value : out Text; Depth : Natural := 0);

      procedure Primary (Depth : Natural) is
         Token, Binding : Text;
         Escaped : Boolean;
         Start, Finish : Positive;
      begin
         Skip;
         if Failed or Full then return; end if;
         if Depth > MAX_NESTING or else Cursor > Input.Length then
            Failed := True;
            return;
         end if;
         Start := Cursor;
         if Input.Data (Cursor) = '(' then
            declare
               Before : constant View_Length := Lowered.Length;
            begin
               Cursor := Cursor + 1;
               Expression (Depth + 1);
               Expect (")");
               if Lowered.Length > Before then
                  Origins (Before + 1) := Start;
                  Ends (Lowered.Length) := Cursor;
               end if;
            end;
         elsif Input.Data (Cursor) = '[' then
            --  [a, b, c] lowers to the canonical [a b c].
            Cursor := Cursor + 1;
            Put ("[", Start);
            loop
               Skip;
               exit when Failed or else Full;
               if Cursor > Input.Length then Failed := True; exit; end if;
               if Input.Data (Cursor) = ']' then
                  Cursor := Cursor + 1;
                  Put ("]", Cursor - 1);
                  exit;
               end if;
               Expression (Depth + 1);
               exit when Failed or else Full;
               Skip;
               if Cursor <= Input.Length and then Input.Data (Cursor) = ',' then
                  Cursor := Cursor + 1;
                  Put (" ", Cursor - 1);
               elsif Cursor > Input.Length or else Input.Data (Cursor) /= ']' then
                  Failed := True;
                  exit;
               end if;
            end loop;
         elsif Input.Data (Cursor) = '"' then
            Cursor := Cursor + 1;
            while Cursor <= Input.Length and then Input.Data (Cursor) /= '"' loop
               if Input.Data (Cursor) = '\' then
                  Cursor := Cursor + 1;
                  exit when Cursor > Input.Length;
               end if;
               Cursor := Cursor + 1;
            end loop;
            if Cursor > Input.Length then Failed := True; return; end if;
            Cursor := Cursor + 1;
            Put (Input.Data (Start .. Cursor - 1), Start);
            if Lowered.Length > 0 then Ends (Lowered.Length) := Cursor; end if;
         else
            Escaped := Input.Data (Start) = '`';
            Name_Token (Token);
            Finish := Cursor;
            Skip;
            if not Escaped and then Token.Data (1 .. Token.Length) = "LET" then
               Name_Token (Binding);
               Expect ("=");
               Put ("(let ((" & Binding.Data (1 .. Binding.Length) & " ", Start);
               Expression (Depth + 1);
               Expect ("IN");
               Put (")) ", Cursor);
               Expression (Depth + 1);
               Expect ("END");
               Put (")", Cursor - 1);
            elsif not Escaped and then Token.Data (1 .. Token.Length) = "MATCH" then
               Put ("(match ", Start);
               Expression (Depth + 1);
               loop
                  Skip;
                  exit when Failed or else Full or else Cursor + 3 > Input.Length or else
                    Input.Data (Cursor .. Cursor + 3) /= "CASE";
                  Expect ("CASE"); Name_Token (Token);
                  Put (" ((" & Token.Data (1 .. Token.Length), Cursor);
                  Skip;
                  if Cursor <= Input.Length and then Input.Data (Cursor) = '(' then
                     Expect ("("); Name_Token (Binding); Expect (")");
                     Put (" " & Binding.Data (1 .. Binding.Length), Cursor);
                  end if;
                  Put (") ", Cursor); Expect ("THEN");
                  Expression (Depth + 1); Put (")", Cursor);
               end loop;
               Expect ("END"); Put (")", Cursor - 1);
            elsif not Escaped and then Token.Data (1 .. Token.Length) = "IF" then
               Put ("(if ", Start);
               Expression (Depth + 1);
               Expect ("THEN");
               Put (" ", Cursor);
               Expression (Depth + 1);
               Expect ("ELSE");
               Put (" ", Cursor);
               Expression (Depth + 1);
               Expect ("END");
               Put (")", Cursor - 1);
            elsif not Escaped and then Token.Data (1 .. Token.Length) = "FUNCTION" and then
              Cursor <= Input.Length and then Input.Data (Cursor) = '('
            then
               --  FUNCTION(x AS T, ...) body lowers to (fn ((x T) ...) body).
               Cursor := Cursor + 1;
               Put ("(fn (", Start);
               Skip;
               if Cursor <= Input.Length and then Input.Data (Cursor) /= ')' then
                  loop
                     Name_Token (Binding);
                     Skip;
                     exit when Failed or else Full;
                     if Cursor + 1 <= Input.Length and then Input.Data (Cursor .. Cursor + 1) = "AS" and then
                       (Cursor + 2 > Input.Length or else not Basic_Name_Character (Input.Data (Cursor + 2)))
                     then
                        Expect ("AS"); Basic_Type (Token);
                        exit when Failed or else Full;
                        Put ("(" & Binding.Data (1 .. Binding.Length) & " " &
                             Token.Data (1 .. Token.Length) & ")", Cursor);
                     else
                        --  FUNCTION(x) body: the type is inferred where it is passed.
                        Put (Binding.Data (1 .. Binding.Length), Cursor);
                     end if;
                     Skip;
                     exit when Cursor > Input.Length or else Input.Data (Cursor) /= ',';
                     Cursor := Cursor + 1;
                     Put (" ", Cursor - 1);
                  end loop;
               end if;
               Expect (")");
               Put (") ", Cursor);
               Expression (Depth + 1);
               --  The span ends where the body ends, not after trailing space.
               declare
                  Body_End : constant Natural :=
                    (if Lowered.Length > 0 then Ends (Lowered.Length) else Cursor);
               begin
                  Put (")", Start);
                  if not Full and then Lowered.Length > 0 then
                     Ends (Lowered.Length) := Body_End;
                  end if;
               end;
            elsif not Escaped and then Keyword (Token.Data (1 .. Token.Length)) then
               Failed := True;
            elsif Cursor <= Input.Length and then Input.Data (Cursor) = '(' then
               Cursor := Cursor + 1;
               Put ("(" & Token.Data (1 .. Token.Length), Start);
               Skip;
               if Cursor <= Input.Length and then Input.Data (Cursor) /= ')' then
                  loop
                     Put (" ", Cursor);
                     Skip;
                     exit when Failed or else Full or else Cursor > Input.Length;
                     --  name => value names a record field, in both notations.
                     declare
                        Mark : constant Positive := Cursor;
                        Last : Natural := Cursor - 1;
                        After : Natural;
                     begin
                        while Last < Input.Length and then Basic_Name_Character (Input.Data (Last + 1)) loop
                           Last := Last + 1;
                        end loop;
                        After := Last + 1;
                        while After <= Input.Length and then White (Input.Data (After)) loop
                           After := After + 1;
                        end loop;
                        if Last >= Mark and then After < Input.Length and then
                          Input.Data (After .. After + 1) = "=>"
                        then
                           Put (Input.Data (Mark .. Last) & " => ", Mark);
                           Cursor := After + 2;
                        end if;
                     end;
                     Expression (Depth + 1);
                     Skip;
                     exit when Failed or else Full or else Cursor > Input.Length or else
                       Input.Data (Cursor) /= ',';
                     Cursor := Cursor + 1;
                  end loop;
               end if;
               Expect (")");
               Put (")", Cursor - 1);
            else
               Put (Token.Data (1 .. Token.Length), Start);
               --  End positions need the end of the token, not following trivia.
               if Lowered.Length > 0 then Ends (Lowered.Length) := Finish; end if;
            end if;
         end if;
      end Primary;

      --  Precedence climbing lowers directly into the existing Lisp reader.
      --  Wrapping a completed left operand retains its bytes and source map;
      --  no expression is evaluated, reordered, or reassociated here.
      procedure Expression (Depth : Natural; Minimum : Precedence := 1) is
         Before : constant View_Length := Lowered.Length;
         Start : Natural;
         Op : Infix_Operator;
         Width : Natural;
         Right_End : Natural;
         procedure Prefix (Value : String) is
         begin
            if Value'Length > Maximum_View_Length - Lowered.Length then
               Full := True;
               return;
            end if;
            for I in reverse Before + 1 .. Lowered.Length loop
               Lowered.Data (I + Value'Length) := Lowered.Data (I);
               Origins (I + Value'Length) := Origins (I);
               Ends (I + Value'Length) := Ends (I);
            end loop;
            Lowered.Data (Before + 1 .. Before + Value'Length) := Value;
            for I in Before + 1 .. Before + Value'Length loop
               Origins (I) := Start;
               Ends (I) := Start;
            end loop;
            Lowered.Length := Lowered.Length + Value'Length;
         end Prefix;
      begin
         Skip;
         Start := Cursor;
         Primary (Depth);
         loop
            exit when Failed or Full;
            Skip;
            exit when Cursor > Input.Length;
            Op := (case Input.Data (Cursor) is
              when '=' => Equality, when '+' => Addition,
              when '-' => Subtraction, when '<' => Less, when '>' => Greater,
              when '*' => Multiplication, when '/' => Division,
              when '%' => Modulo, when others => No_Operator);
            Width := 1;
            if Input.Length - (Cursor - 1) >= 2 and then
              Input.Data (Cursor .. Cursor + 1) in "<=" | ">=" | "<>"
            then
               Op := (case Input.Data (Cursor + 1) is
                 when '=' => (if Input.Data (Cursor) = '<' then Less_Equal
                              else Greater_Equal),
                 when others => Inequality);
               Width := 2;
            end if;
            declare
               function Word (W : String) return Boolean is
                 (Input.Length - (Cursor - 1) >= W'Length and then
                  Input.Data (Cursor .. Cursor + W'Length - 1) = W and then
                  (Cursor + W'Length > Input.Length or else
                   not Basic_Name_Character (Input.Data (Cursor + W'Length))));
            begin
               if Word ("MOD") then Op := Modulo; Width := 3;
               elsif Word ("AND") then Op := Conjunction; Width := 3;
               elsif Word ("OR") then Op := Disjunction; Width := 2;
               end if;
            end;
            exit when Priority (Op) < Minimum;
            Cursor := Cursor + Width;
            Prefix ("(" & Spelling (Op, Lisp) & " ");
            Put (" ", Cursor);
            Expression (Depth + 1, Priority (Op) + 1);
            exit when Failed or Full;
            Right_End := Ends (Lowered.Length);
            Put (")", Start);
            if not Full then Ends (Lowered.Length) := Right_End; end if;
         end loop;
         --  Pipelines bind loosest: a | f(x) | g lowers to (->> a (f x) g).
         if Minimum = 1 and then not Failed and then not Full then
            Skip;
            if Cursor <= Input.Length and then Input.Data (Cursor) = '|' then
               Prefix ("(->> ");
               loop
                  Skip;
                  exit when Cursor > Input.Length or else Input.Data (Cursor) /= '|';
                  Cursor := Cursor + 1;
                  Put (" ", Cursor);
                  Skip;
                  Primary (Depth + 1);
                  exit when Failed or Full;
               end loop;
               --  The pipeline ends where its last stage ends.
               Right_End := (if Lowered.Length > 0 then Ends (Lowered.Length) else Cursor);
               Put (")", Start);
               if not Full then Ends (Lowered.Length) := Right_End; end if;
            end if;
         end if;
      end Expression;

      --  A BASIC type after AS, lowered to its Lisp spelling: a name,
      --  FUNCTION(T, ...) AS R for (Function (T ...) R), or LIST(T) for
      --  (List T).
      procedure Basic_Type (Value : out Text; Depth : Natural := 0) is
         Part : Text;
      begin
         Value := (others => <>);
         Skip;
         if Depth > MAX_NESTING then Failed := True; return; end if;
         if Cursor + 7 <= Input.Length and then Input.Data (Cursor .. Cursor + 7) = "FUNCTION" and then
           (Cursor + 8 > Input.Length or else not Basic_Name_Character (Input.Data (Cursor + 8)))
         then
            Expect ("FUNCTION"); Expect ("(");
            Append (Value, "(Function (");
            Skip;
            if Cursor <= Input.Length and then Input.Data (Cursor) /= ')' then
               loop
                  Basic_Type (Part, Depth + 1);
                  exit when Failed or else Full;
                  Append (Value, Part.Data (1 .. Part.Length));
                  Skip;
                  exit when Cursor > Input.Length or else Input.Data (Cursor) /= ',';
                  Cursor := Cursor + 1;
                  Append (Value, " ");
               end loop;
            end if;
            Expect (")"); Expect ("AS");
            Basic_Type (Part, Depth + 1);
            Append (Value, ") " & Part.Data (1 .. Part.Length) & ")");
         elsif Cursor + 3 <= Input.Length and then Input.Data (Cursor .. Cursor + 3) = "LIST" and then
           (Cursor + 4 > Input.Length or else not Basic_Name_Character (Input.Data (Cursor + 4)))
         then
            Expect ("LIST"); Expect ("(");
            Basic_Type (Part, Depth + 1);
            Expect (")");
            Append (Value, "(List " & Part.Data (1 .. Part.Length) & ")");
         else
            Name_Token (Value);
         end if;
      end Basic_Type;

      procedure Program is
         Token, Type_Name : Text;
         Start : Positive;
         Variant, Record_Type : Boolean;
      begin
         loop
            Skip;
            exit when Failed or else Full;
            if Cursor + 3 <= Input.Length and then
              Input.Data (Cursor .. Cursor + 3) = "TYPE" and then
              (Cursor + 4 > Input.Length or else
               not Basic_Name_Character (Input.Data (Cursor + 4)))
            then
               Start := Cursor;
               Expect ("TYPE"); Name_Token (Token);
               Put ("(type " & Token.Data (1 .. Token.Length), Start);
               Expect ("="); Skip;
               if Cursor + 4 <= Input.Length and then Input.Data (Cursor .. Cursor + 4) = "RANGE" then
                  --  TYPE P = RANGE lo TO hi  ->  (type P (range lo hi))
                  Expect ("RANGE");
                  Put (" (range", Start);
                  Number_Token (Type_Name);
                  Put (" " & Type_Name.Data (1 .. Type_Name.Length), Cursor);
                  Expect ("TO");
                  Number_Token (Type_Name);
                  Put (" " & Type_Name.Data (1 .. Type_Name.Length), Cursor);
                  Put ("))", Cursor - 1);
                  if Lowered.Length > 0 then Ends (Lowered.Length) := Cursor; end if;
                  Put (" ", Cursor);
               else
                  Variant := Cursor + 6 <= Input.Length and then Input.Data (Cursor .. Cursor + 6) = "VARIANT";
                  Record_Type := Cursor + 5 <= Input.Length and then Input.Data (Cursor .. Cursor + 5) = "RECORD";
                  if Variant then Expect ("VARIANT"); end if;
                  if Record_Type then Expect ("RECORD"); end if;
                  Put ((if Record_Type then " (record" elsif Variant then " (variant" else " (enum"), Start);
                  Expect ("(");
                  Skip;
                  if Record_Type and then Cursor <= Input.Length and then Input.Data (Cursor) = ')' then
                     null;
                  else
                  loop
                     Name_Token (Token);
                     Put ((if Variant or Record_Type then " (" else " ") & Token.Data (1 .. Token.Length), Cursor);
                     if Variant or Record_Type then
                        Skip;
                        if Cursor + 1 <= Input.Length and then Input.Data (Cursor .. Cursor + 1) = "AS" then
                           Expect ("AS"); Basic_Type (Type_Name);
                           Put (" " & Type_Name.Data (1 .. Type_Name.Length), Cursor);
                        end if;
                        --  x AS T := default: a constant, so it is copied as written.
                        Skip;
                        if Record_Type and then Cursor + 1 <= Input.Length and then
                          Input.Data (Cursor .. Cursor + 1) = ":="
                        then
                           Cursor := Cursor + 2;
                           Skip;
                           declare
                              Mark : constant Positive := Cursor;
                           begin
                              while Cursor <= Input.Length and then Input.Data (Cursor) not in ',' | ')'
                                and then not White (Input.Data (Cursor))
                              loop
                                 Cursor := Cursor + 1;
                              end loop;
                              if Cursor = Mark then
                                 Failed := True;
                              else
                                 Put (" " & Input.Data (Mark .. Cursor - 1), Mark);
                              end if;
                           end;
                        end if;
                        Put (")", Cursor);
                     end if;
                     Skip;
                     exit when Failed or else Full or else Cursor > Input.Length or else Input.Data (Cursor) /= ',';
                     Cursor := Cursor + 1;
                  end loop;
                  end if;
                  --  The declaration ends at its closing parenthesis, as a
                  --  FUNCTION's does at END.
                  Expect (")"); Put ("))", Cursor - 1);
                  if Lowered.Length > 0 then Ends (Lowered.Length) := Cursor; end if;
                  Put (" ", Cursor);
               end if;
            else
            exit when Cursor + 7 > Input.Length or else
              Input.Data (Cursor .. Cursor + 7) /= "FUNCTION" or else
              (Cursor + 8 <= Input.Length and then
               Basic_Name_Character (Input.Data (Cursor + 8)));
            --  FUNCTION( ... begins an anonymous function: an expression.
            declare
               Peek : Positive := Cursor + 8;
            begin
               while Peek <= Input.Length and then White (Input.Data (Peek)) loop
                  Peek := Peek + 1;
               end loop;
               exit when Peek <= Input.Length and then Input.Data (Peek) = '(';
            end;
            Start := Cursor;
            Expect ("FUNCTION");
            Name_Token (Token);
            Put ("(define (" & Token.Data (1 .. Token.Length), Start);
            Expect ("("); Skip;
            if Cursor <= Input.Length and then Input.Data (Cursor) /= ')' then
               loop
                  Name_Token (Token); Expect ("AS"); Basic_Type (Type_Name);
                  Put (" (" & Token.Data (1 .. Token.Length) & " " &
                       Type_Name.Data (1 .. Type_Name.Length) & ")", Cursor);
                  Skip;
                  exit when Failed or else Full or else Cursor > Input.Length or else Input.Data (Cursor) /= ',';
                  Cursor := Cursor + 1;
               end loop;
            end if;
            Expect (")"); Expect ("AS"); Basic_Type (Type_Name);
            Put (") " & Type_Name.Data (1 .. Type_Name.Length) & " ", Cursor);
            Expect ("RETURN");
            Expression (1);
            Expect ("END");
            Put (")", Cursor - 1);
            if Lowered.Length > 0 then Ends (Lowered.Length) := Cursor; end if;
            Put (" ", Cursor);
            end if;
         end loop;
         Expression (0);
      end Program;

      function Type_Name (Kind : Static_Type) return String is
        (CCL.Types.Image (CCL.Types.Describe (Analysis.Tree.Types, Kind).Identifier));

      function Builtin
        (Kind : Node_Kind; Style : Surface; Compact : Boolean) return String is
        (case Kind is
           when Add_Form => (if Style = Lisp then "+" else "add"),
           when Subtract_Form => (if Style = Lisp then "-" else "subtract"),
           when Not_Equal_Form => (if Style = Lisp then "/=" else "not-equal"),
           when Less_Form => (if Style = Lisp then "<" else "less"),
           when Less_Equal_Form => (if Style = Lisp then "<=" else "less-equal"),
           when Greater_Form => (if Style = Lisp then ">" else "greater"),
           when Greater_Equal_Form =>
             (if Style = Lisp then ">=" else "greater-equal"),
           when And_Form => "and", when Or_Form => "or",
           when Multiply_Form => (if Style = Lisp then "*" else "multiply"),
           when Divide_Form => (if Style = Lisp then "/" else "divide"),
           --  Keep the internal encoding compact, but prefer the readable
           --  operation name in both editable source views.
           when Modulo_Form => (if Compact and Style = Lisp then "%" else "mod"),
           when Equal_Form => (if Style = Lisp then "=" else "equal"),
           when Not_Form => "not", when If_Form => "if",
           when String_Length_Form => "length", when String_Index_Form => "at",
           when String_Concat_Form => "concat", when To_String_Form => "to-string",
           when others => "");

      procedure Print
        (Ref : Node_Reference; Style : Surface; Buffer : in out Text;
         Spans : in out Node_Spans; Depth : Natural; Pretty : Boolean := False;
         Parent_Priority : Precedence := 0)
      is
         N : Node;
         Wrap : Boolean := False;
         Op : Infix_Operator;
         Grouped : Boolean;
         procedure Emit (S : String) is
         begin Append (Buffer, S); end Emit;
         procedure Identifier (Item : Name) is
            S : constant String := Item.Data (1 .. Item.Length);
         begin
            --  Escape only delimiter-bearing names and the BASIC keyword.
            if Style = Basic and then
              (Keyword (S) or else
               (for some C of S => C in '=' | '+' | '*' | '/' | '%' | '<' | '>'))
            then Emit ("`"); end if;
            Emit (S);
            if Style = Basic and then
              (Keyword (S) or else
               (for some C of S => C in '=' | '+' | '*' | '/' | '%' | '<' | '>'))
            then Emit ("`"); end if;
         end Identifier;
         procedure Child (Index : Node_Reference) is
         begin Print (Index, Style, Buffer, Spans, Depth + 1, Pretty); end Child;
         --  N ends a pipeline: print its first value, then each stage without
         --  the argument the pipeline supplies (a bare name when none remain).
         procedure Print_Pipeline is
            Stages : array (1 .. MAX_NESTING) of Node_Reference := [others => NO_NODE];
            Count : Natural := 0;
            Current : Node_Reference := Ref;
            S : Node;
            function Input_Of (Stage : Node) return Node_Reference is
              (if Stage.Kind in Builtin_Form | Function_Call then
                  (if Stage.Argument_Count >= 1 then Stage.Arguments (Stage.Argument_Count) else NO_NODE)
               else Stage.First);
         begin
            loop
               exit when Current >= Analysis.Tree.Length or else Count = MAX_NESTING;
               S := Analysis.Tree.Nodes (Current);
               exit when not S.Piped;
               Count := Count + 1;
               Stages (Count) := Current;
               Current := Input_Of (S);
            end loop;
            if Style = Lisp then Emit ("(->> "); end if;
            Child (Current);
            for I in reverse 1 .. Count loop
               S := Analysis.Tree.Nodes (Stages (I));
               Emit ((if Style = Lisp then " " else " | "));
               if I > 1 then Spans (Stages (I)).First := Buffer.Length + 1; end if;
               declare
                  Remaining : constant Natural :=
                    (if S.Kind in Builtin_Form | Function_Call and then S.Argument_Count >= 1
                     then S.Argument_Count - 1 else 0);
               begin
                  if Remaining > 0 and then Style = Lisp then Emit ("("); end if;
                  if S.Kind = Builtin_Form then Emit (Builtin_Name (S.Builtin));
                  elsif S.Kind = Function_Call then Identifier (S.Identifier);
                  else Emit (Builtin (S.Kind, Style, False));
                  end if;
                  if Remaining > 0 then
                     if Style = Basic then Emit ("("); end if;
                     for P in 1 .. Remaining loop
                        if Style = Lisp then Emit (" "); elsif P > 1 then Emit (", "); end if;
                        Child (S.Arguments (P));
                     end loop;
                     Emit (")");
                  end if;
               end;
               if I > 1 then Spans (Stages (I)).After_Last := Buffer.Length + 1; end if;
            end loop;
            if Style = Lisp then Emit (")"); end if;
         end Print_Pipeline;
         --  A type as written: a name, a list type (List Integer) /
         --  LIST(Integer), or a function type spelled from its parts,
         --  (Function (Integer) Integer) / FUNCTION(Integer) AS Integer.
         procedure Emit_Type (Kind : Static_Type; Level : Natural := 0) is
            D : CCL.Types.Description;
         begin
            if CCL.Types.Is_List (Analysis.Tree.Types, Kind) and then Level <= MAX_NESTING then
               Emit ((if Style = Lisp then "(List " else "LIST("));
               Emit_Type (CCL.Types.Element_Of (Analysis.Tree.Types, Kind), Level + 1);
               Emit (")");
               return;
            end if;
            if CCL.Types.Is_Stream (Analysis.Tree.Types, Kind) and then Level <= MAX_NESTING then
               Emit ((if Style = Lisp then "(Stream " else "STREAM("));
               Emit_Type (CCL.Types.Stream_Element (Analysis.Tree.Types, Kind), Level + 1);
               Emit (")");
               return;
            end if;
            if CCL.Types.Is_Task (Analysis.Tree.Types, Kind) and then Level <= MAX_NESTING then
               Emit ((if Style = Lisp then "(Task " else "TASK("));
               Emit_Type (CCL.Types.Task_Result (Analysis.Tree.Types, Kind), Level + 1);
               Emit (")");
               return;
            end if;
            if not CCL.Types.Is_Function (Analysis.Tree.Types, Kind) or else Level > MAX_NESTING then
               Emit (Type_Name (Kind));
               return;
            end if;
            D := CCL.Types.Describe (Analysis.Tree.Types, Kind);
            Emit ((if Style = Lisp then "(Function (" else "FUNCTION("));
            for P in 1 .. D.Count - 1 loop
               if P > 1 then Emit ((if Style = Lisp then " " else ", ")); end if;
               Emit_Type (D.Parts (P).Payload, Level + 1);
            end loop;
            Emit ((if Style = Lisp then ") " else ") AS "));
            Emit_Type (D.Parts (D.Count).Payload, Level + 1);
            if Style = Lisp then Emit (")"); end if;
         end Emit_Type;
         --  A field default as its constant is written: 3, true, Color.Red, [].
         procedure Emit_Default (Payload : Static_Type; Default : CCL.Types.Field_Default) is
         begin
            case Default.Kind is
               when CCL.Types.No_Default => null;
               when CCL.Types.Integer_Default =>
                  declare
                     Image : constant String := Default.Value'Image;
                  begin
                     Emit ((if Image (Image'First) = ' ' then Image (Image'First + 1 .. Image'Last) else Image));
                  end;
               when CCL.Types.Boolean_Default => Emit ((if Interfaces."=" (Default.Value, 1) then "true" else "false"));
               when CCL.Types.Alternative_Default =>
                  declare
                     Owner : constant CCL.Types.Description := CCL.Types.Describe (Analysis.Tree.Types, Payload);
                  begin
                     Identifier (Owner.Identifier); Emit (".");
                     if Default.Value in 1 .. Interfaces.Integer_64 (Owner.Count) then
                        Identifier (Owner.Parts (CCL.Types.Component_Index (Default.Value)).Identifier);
                     end if;
                  end;
               when CCL.Types.Empty_List_Default => Emit ("[]");
            end case;
         end Emit_Default;

         procedure New_Line (Level : Natural) is
         begin
            Emit (String'(1 => ASCII.LF));
            for I in 1 .. Level loop Emit ("  "); end loop;
         end New_Line;
         procedure Separator is
         begin
            if Wrap then
               if Style = Basic then Emit (","); end if;
               New_Line (Depth + 1);
            else Emit ((if Style = Lisp then " " else ", "));
            end if;
         end Separator;
      begin
         if Ref = NO_NODE or else Depth > MAX_NESTING or else Full then
            Failed := True;
            return;
         end if;
         N := Analysis.Tree.Nodes (Ref);
         if Pretty then
            declare
               Column : Natural := 0;
            begin
               for I in reverse 1 .. Buffer.Length loop
                  exit when Buffer.Data (I) = ASCII.LF;
                  Column := Column + 1;
               end loop;
               Wrap := N.Kind = If_Form or else
                 Column + Flat_Spans (Ref).After_Last - Flat_Spans (Ref).First > 80;
            end;
         end if;
         Spans (Ref).First := Buffer.Length + 1;
         Op := Operator_For (N.Kind);
         Grouped := Style = Basic and then Op /= No_Operator and then
           Priority (Op) < Parent_Priority;
         if Grouped then Emit ("("); end if;
         if Style = Basic and then Op /= No_Operator then
            Print (N.First, Style, Buffer, Spans, Depth + 1, Pretty, Priority (Op));
            Emit (" " & Spelling (Op, Basic));
            if Wrap then New_Line (Depth + 1); else Emit (" "); end if;
            --  Even associative operators retain explicit right grouping:
            --  overflow/fuel behavior and the AST must remain identical.
            Print (N.Second, Style, Buffer, Spans, Depth + 1, Pretty, Priority (Op) + 1);
         elsif N.Piped then
            Print_Pipeline;
         elsif Style = Basic and then N.Kind = If_Form then
            Emit ("IF ");
            Child (N.First);
            Emit (" THEN");
            New_Line (Depth + 1);
            Child (N.Second);
            New_Line (Depth); Emit ("ELSE");
            New_Line (Depth + 1);
            Child (N.Third);
            New_Line (Depth); Emit ("END");
         else
         case N.Kind is
            when Type_Definition =>
               declare
                  D : constant CCL.Types.Description :=
                    CCL.Types.Describe (Analysis.Tree.Types, N.Declared_Kind);
                  Enum : constant Boolean := CCL.Types.Is_Enumeration (Analysis.Tree.Types, N.Declared_Kind);
               begin
                  Emit ((if Style = Lisp then "(type " else "TYPE "));
                  Identifier (D.Identifier);
                  if D.Form = CCL.Types.Bounded then
                     --  (type P (range 1 10)) / TYPE P = RANGE 1 TO 10
                     declare
                        Low : constant String :=
                          CCL.Types.Low_Of (Analysis.Tree.Types, N.Declared_Kind)'Image;
                        High : constant String :=
                          CCL.Types.High_Of (Analysis.Tree.Types, N.Declared_Kind)'Image;
                        function Trim (S : String) return String is
                          (if S'Length >= 2 and then S (S'First) = ' '
                           then S (S'Last - (S'Length - 2) .. S'Last) else S);
                     begin
                        Emit ((if Style = Lisp then " (range " & Trim (Low) & " " & Trim (High) & "))"
                               else " = RANGE " & Trim (Low) & " TO " & Trim (High)));
                     end;
                     Spans (Ref).After_Last := Buffer.Length + 1;
                     if Pretty then New_Line (Depth); New_Line (Depth); else Emit (" "); end if;
                     Print (N.Second, Style, Buffer, Spans, Depth, Pretty);
                     return;
                  end if;
                  --  Canonicalize nullary sums to the enum shorthand.
                  Emit ((if Style = Lisp then
                           (if D.Form = CCL.Types.Product then " (record " elsif Enum then " (enum " else " (variant ")
                         else (if D.Form = CCL.Types.Product then " = RECORD (" elsif Enum then " = (" else " = VARIANT (")));
                  for I in 1 .. D.Count loop
                     if I > 1 then Emit ((if Style = Lisp then " " else ", ")); end if;
                     if not Enum and Style = Lisp then Emit ("("); end if;
                     Identifier (D.Parts (I).Identifier);
                     if D.Parts (I).Payload /= Unit_Type or D.Form = CCL.Types.Product then
                        Emit ((if Style = Lisp then " " else " AS "));
                        Emit_Type (D.Parts (I).Payload);
                     end if;
                     if CCL.Types."/=" (CCL.Types.Default_Of (Analysis.Tree.Types, N.Declared_Kind, I).Kind,
                                        CCL.Types.No_Default)
                     then
                        Emit ((if Style = Lisp then " " else " := "));
                        Emit_Default (D.Parts (I).Payload, CCL.Types.Default_Of (Analysis.Tree.Types, N.Declared_Kind, I));
                     end if;
                     if not Enum and Style = Lisp then Emit (")"); end if;
                  end loop;
                  Emit ((if Style = Lisp then "))" else ")"));
                  Spans (Ref).After_Last := Buffer.Length + 1;
                  if Pretty then New_Line (Depth); New_Line (Depth); else Emit (" "); end if;
                  Print (N.Second, Style, Buffer, Spans, Depth, Pretty);
                  return;
               end;
            when Match_Form =>
               declare
                  Arm : Node_Reference := N.Second;
                  A : Node;
               begin
                  Emit ((if Style = Lisp then "(match " else "MATCH "));
                  Child (N.First);
                  while Arm < Analysis.Tree.Length loop
                     A := Analysis.Tree.Nodes (Arm);
                     if Pretty then New_Line (Depth + 1); else Emit (" "); end if;
                     Spans (Arm).First := Buffer.Length + 1;
                     Emit ((if Style = Lisp then "((" else "CASE "));
                     Identifier (A.Pattern);
                     if A.Identifier.Length > 0 then
                        Emit ((if Style = Lisp then " " else "("));
                        Identifier (A.Identifier);
                        if Style = Basic then Emit (")"); end if;
                     end if;
                     Emit ((if Style = Lisp then ") " else " THEN "));
                     Child (A.First);
                     if Style = Lisp then Emit (")"); end if;
                     Spans (Arm).After_Last := Buffer.Length + 1;
                     Arm := A.Second;
                  end loop;
                  if Pretty then New_Line (Depth); else Emit (" "); end if;
                  Emit ((if Style = Lisp then ")" else "END"));
               end;
            when Match_Arm => Failed := True;
            when Function_Definition =>
               declare
                  Decl : constant Function_Declaration := Analysis.Tree.Functions (N.Function_Id);
               begin
                  Emit ((if Style = Lisp then "(define (" else "FUNCTION "));
                  Identifier (Decl.Identifier);
                  if Style = Basic then Emit ("("); end if;
                  for P in 1 .. Decl.Count loop
                     if Style = Lisp then Emit (" (");
                     elsif P > 1 then Emit (", "); end if;
                     Identifier (Decl.Parameters (P).Identifier);
                     Emit ((if Style = Lisp then " " else " AS "));
                     Emit_Type (Decl.Parameters (P).Kind);
                     if Style = Lisp then Emit (")"); end if;
                  end loop;
                  Emit ((if Style = Lisp then ") " else ") AS "));
                  Emit_Type (Decl.Result_Kind);
                  if Style = Basic then Emit (" RETURN"); end if;
                  if Pretty then New_Line (Depth + 1); else Emit (" "); end if;
                  Child (N.First);
                  if Style = Lisp then Emit (")");
                  else New_Line (Depth); Emit ("END"); end if;
                  Spans (Ref).After_Last := Buffer.Length + 1;
                  if Pretty then New_Line (Depth); New_Line (Depth); else Emit (" "); end if;
                  Print (N.Second, Style, Buffer, Spans, Depth, Pretty);
                  return;
               end;
            when Handler_Form =>
               Emit ((if Style = Lisp then "(handler " else "handler("));
               Identifier (N.Identifier);
               Emit (")");
            when Field_Form =>
               Emit ((if Style = Lisp then "(field " else "field("));
               Child (N.First);
               Emit ((if Style = Lisp then " " else ", "));
               Identifier (N.Identifier);
               Emit (")");
            when Record_Construct =>
               --  Positional fields, then named ones; defaulted fields stay
               --  left out, as the source wrote them.
               declare
                  Shape : constant CCL.Types.Description :=
                    CCL.Types.Describe (Analysis.Tree.Types, N.Declared_Kind);
                  Emitted : Boolean := False;
                  procedure Next is
                  begin
                     if Style = Lisp then Emit (" "); elsif Emitted then Emit (", "); end if;
                     Emitted := True;
                  end Next;
               begin
                  if Style = Lisp then Emit ("("); end if;
                  Identifier (N.Identifier);
                  if Style = Basic then Emit ("("); end if;
                  for P in 1 .. Shape.Count loop
                     if not N.Named_Fields (P) and then not N.Defaulted_Fields (P) then
                        Next;
                        Child (N.Components (P));
                     end if;
                  end loop;
                  for P in 1 .. Shape.Count loop
                     if N.Named_Fields (P) then
                        Next;
                        Identifier (Shape.Parts (P).Identifier); Emit (" => ");
                        Child (N.Components (P));
                     end if;
                  end loop;
                  Emit (")");
               end;
            when Builtin_Form =>
               if Style = Lisp then Emit ("("); end if;
               Emit (Builtin_Name (N.Builtin));
               if Style = Basic then Emit ("("); end if;
               for P in 1 .. N.Argument_Count loop
                  if Style = Lisp then Emit (" "); elsif P > 1 then Emit (", "); end if;
                  Child (N.Arguments (P));
               end loop;
               Emit (")");
            when Lambda_Form =>
               declare
                  Decl : constant Function_Declaration := Analysis.Tree.Functions (N.Function_Id);
               begin
                  Emit ((if Style = Lisp then "(fn (" else "FUNCTION("));
                  for P in 1 .. Decl.Count loop
                     if P > 1 then Emit ((if Style = Lisp then " " else ", ")); end if;
                     if Decl.Parameters (P).Declared then
                        if Style = Lisp then Emit ("("); end if;
                        Identifier (Decl.Parameters (P).Identifier);
                        Emit ((if Style = Lisp then " " else " AS "));
                        Emit_Type (Decl.Parameters (P).Kind);
                        if Style = Lisp then Emit (")"); end if;
                     else
                        Identifier (Decl.Parameters (P).Identifier);
                     end if;
                  end loop;
                  Emit (") ");
                  Child (N.First);
                  if Style = Lisp then Emit (")"); end if;
               end;
            when List_Construct =>
               if N.Element_Count = 0 and then N.Second = NO_NODE then
                  --  (list-of T) / list-of(T): the typed empty list.
                  Emit ((if Style = Lisp then "(list-of " else "list-of("));
                  Emit_Type (CCL.Types.Element_Of (Analysis.Tree.Types, N.Declared_Kind));
                  Emit (")");
               else
                  Emit ("[");
                  declare
                     Chunk : Node_Reference := Ref;
                     First_Element : Boolean := True;
                  begin
                     --  Long literals continue in chunks chained through Second.
                     for Step in 0 .. MAX_NESTING loop
                        exit when Chunk = NO_NODE or else Chunk >= Analysis.Tree.Length;
                        for P in 1 .. Analysis.Tree.Nodes (Chunk).Element_Count loop
                           if not First_Element then
                              Emit ((if Style = Lisp then " " else ", "));
                           end if;
                           First_Element := False;
                           Child (Analysis.Tree.Nodes (Chunk).Components (P));
                        end loop;
                        Chunk := Analysis.Tree.Nodes (Chunk).Second;
                     end loop;
                  end;
                  Emit ("]");
               end if;
            when Function_Call =>
               if Style = Lisp then Emit ("("); end if;
               Identifier (N.Identifier);
               if Style = Basic then Emit ("("); end if;
               for P in 1 .. N.Argument_Count loop
                  if Style = Lisp then Emit (" ");
                  elsif P > 1 then Emit (", "); end if;
                  Child (N.Arguments (P));
               end loop;
               Emit (")");
            when Integer_Literal =>
               declare
                  S : constant String := Interfaces.Integer_64'Image (N.Integer_Value);
               begin Emit ((if S (S'First) = ' ' then S (S'First + 1 .. S'Last) else S)); end;
            when Stream_Reference =>
               --  (stream T n) / stream(T, n).
               declare
                  S : constant String := Interfaces.Integer_64'Image (N.Integer_Value);
               begin
                  Emit ((if Style = Lisp then "(stream " else "stream("));
                  Emit_Type (CCL.Types.Stream_Element (Analysis.Tree.Types, N.Declared_Kind));
                  Emit ((if Style = Lisp then " " else ", "));
                  Emit (S (S'First + 1 .. S'Last));
                  Emit (")");
               end;
            when Boolean_Literal => Emit ((if N.Boolean_Value then "true" else "false"));
            when String_Literal =>
               Emit ("""");
               for I in N.Text_First .. N.Text_Last loop
                  case Analysis.Tree.Text_Data (I) is
                     when '"' => Emit ("\""");
                     when '\' => Emit ("\\");
                     when ASCII.LF => Emit ("\n");
                     when ASCII.CR => Emit ("\r");
                     when ASCII.HT => Emit ("\t");
                     when others => Emit (String'(1 => Analysis.Tree.Text_Data (I)));
                  end case;
               end loop;
               Emit ("""");
            when Name_Reference | Variant_Literal => Identifier (N.Identifier);
            when Let_Form =>
               Emit ((if Style = Lisp then "(let ((" else "LET "));
               Identifier (N.Identifier);
               Emit ((if Style = Lisp then " " else " = "));
               Child (N.First);
               if Pretty then
                  Emit ((if Style = Lisp then "))" else " IN"));
                  New_Line (Depth + 1);
               else Emit ((if Style = Lisp then ")) " else " IN" & ASCII.LF));
               end if;
               Child (N.Second);
               if Style = Lisp then Emit (")");
               elsif Pretty then New_Line (Depth); Emit ("END");
               else Emit (ASCII.LF & "END");
               end if;
            when Invalid_Node => Failed := True;
            when others =>
               if Style = Lisp then Emit ("("); end if;
               if N.Kind in Host_Import_Form | Variant_Construct then
                  Identifier (N.Identifier);
               else Emit (Builtin (N.Kind, Style, Compact => not Pretty)); end if;
               if Style = Basic then Emit ("("); end if;
               if N.First /= NO_NODE then
                  if Wrap and then N.Kind /= If_Form then New_Line (Depth + 1);
                  elsif Style = Lisp then Emit (" "); end if;
                  Child (N.First);
               end if;
               if N.Second /= NO_NODE then Separator; Child (N.Second); end if;
               if N.Third /= NO_NODE then Separator; Child (N.Third); end if;
               Emit (")");
         end case;
         end if;
         if Grouped then Emit (")"); end if;
         Spans (Ref).After_Last := Buffer.Length + 1;
      end Print;

   begin
      Result := (others => <>);
      if Source'Length > Maximum_View_Length then
         Result.Status := Capacity_Exceeded;
         return;
      end if;
      Append (Input, Source);
      if From = Basic then
         Program;
         Skip;
         Failed := Failed or Cursor <= Input.Length;
      else
         --  Strip only trivia, retaining exact token spelling and source
         --  positions. This also collects comments without inspecting strings.
         while Cursor <= Input.Length loop
            Skip;
            exit when Cursor > Input.Length;
            if Input.Data (Cursor) = '"' then
               declare
                  Start : constant Positive := Cursor;
               begin
                  Cursor := Cursor + 1;
                  while Cursor <= Input.Length loop
                     if Input.Data (Cursor) = '"' then Cursor := Cursor + 1; exit;
                     elsif Input.Data (Cursor) = '\' then
                        Cursor := Cursor + 1;
                        exit when Cursor > Input.Length;
                     end if;
                     Cursor := Cursor + 1;
                  end loop;
                  for I in Start .. Cursor - 1 loop Put (Input.Data (I .. I), I); end loop;
               end;
            else
               Put (Input.Data (Cursor .. Cursor), Cursor);
               Cursor := Cursor + 1;
            end if;
            --  Preserve a token separator, but not one per whitespace byte.
            if Cursor <= Input.Length and then
              (White (Input.Data (Cursor)) or else Input.Data (Cursor) = '#')
            then Put (" ", Cursor); end if;
         end loop;
      end if;
      if Full then Result.Status := Capacity_Exceeded; return; end if;
      if Failed then Result.Position := Cursor; Result.Diagnostic := Unexpected_Token; return; end if;
      if not Check and then From = Basic and then Into = Lisp then
         if Lowered.Length > MAX_SOURCE_LENGTH then
            Result.Status := Capacity_Exceeded;
         else
            Result.Canonical := Lowered;
            Result.Status := Converted;
         end if;
         return;
      end if;
      Analyze (Lowered.Data (1 .. Lowered.Length), Visible_Interfaces, Analysis);
      if Analysis.Status /= Analysis_Succeeded then
         Result.Diagnostic := Analysis.Diagnostic;
         Result.Position := (if Analysis.Diagnostic_Position in 1 .. Lowered.Length
           then Origins (Analysis.Diagnostic_Position) else Input.Length + 1);
         return;
      end if;
      for I in 0 .. Analysis.Tree.Length - 1 loop
         declare
            N : constant Node := Analysis.Tree.Nodes (I);
         begin
            if N.Source_Position in 1 .. Lowered.Length and then
              N.Source_End_Position in 2 .. Lowered.Length + 1
            then
               Result.Input_Nodes (I) := (Origins (N.Source_Position),
                 Ends (N.Source_End_Position - 1));
            end if;
         end;
      end loop;
      Print (Analysis.Tree.Root, Lisp, Result.Canonical, Canonical_Spans, 0);
      Flat_Spans := Canonical_Spans;
      if Into = Basic then Append (Result.Rendered, Marker & ASCII.LF); end if;
      Append (Result.Rendered, Comments.Data (1 .. Comments.Length));
      Print (Analysis.Tree.Root, Into, Result.Rendered, Result.Output_Nodes, 0, Pretty => True);
      if Full or Result.Canonical.Length > MAX_SOURCE_LENGTH then
         Result.Status := Capacity_Exceeded;
      elsif not Failed then Result.Status := Converted;
      end if;
   end Convert;
end CCL.Language.Views;
