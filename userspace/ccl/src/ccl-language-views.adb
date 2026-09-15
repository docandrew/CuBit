package body CCL.Language.Views with SPARK_Mode => On is
   Marker : constant String := "#!ccl basic";

   function Detect (Source : String) return Surface is
     (if Source'Length >= Marker'Length and then
         Source (Source'First .. Source'First + Marker'Length - 1) = Marker
      then Basic else Lisp);

   procedure Convert
     (Source : String; From, Into : Surface;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out Conversion)
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
          '-' | '_' | '.' | '?' | '+' | '=' | '*' | '/' | '%');

      function Basic_Name_Character (C : Character) return Boolean is
        (Name_Character (C) and then C not in '+' | '=' | '*' | '/' | '%');

      function Keyword (S : String) return Boolean is
        (S = "LET" or S = "IN" or S = "END" or S = "IF" or
         S = "THEN" or S = "ELSE" or S = "MOD");

      type Infix_Operator is (No_Operator, Equality, Addition, Multiplication,
                              Division, Modulo);
      subtype Precedence is Natural range 0 .. 4;
      function Priority (Op : Infix_Operator) return Precedence is
        (case Op is when No_Operator => 0, when Equality => 1,
         when Addition => 2, when others => 3);
      function Operator_For (Kind : Node_Kind) return Infix_Operator is
        (case Kind is when Equal_Form => Equality, when Add_Form => Addition,
         when Multiply_Form => Multiplication, when Divide_Form => Division,
         when Modulo_Form => Modulo, when others => No_Operator);
      function Spelling (Op : Infix_Operator; Style : Surface) return String is
        (case Op is when No_Operator => "", when Equality => "=",
         when Addition => "+", when Multiplication => "*", when Division => "/",
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
                Input.Data (Cursor) in '(' | ')' | ',' | '=' | '+' | '*' | '/' | '%' |
                  '#' | '"' | '`');
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
            elsif not Escaped and then Keyword (Token.Data (1 .. Token.Length)) then
               Failed := True;
            elsif Cursor <= Input.Length and then Input.Data (Cursor) = '(' then
               Cursor := Cursor + 1;
               Put ("(" & Token.Data (1 .. Token.Length), Start);
               Skip;
               if Cursor <= Input.Length and then Input.Data (Cursor) /= ')' then
                  loop
                     Put (" ", Cursor);
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
              when '*' => Multiplication, when '/' => Division,
              when '%' => Modulo, when others => No_Operator);
            Width := 1;
            if Input.Length - (Cursor - 1) >= 3 and then
              Input.Data (Cursor .. Cursor + 2) = "MOD" and then
              (Cursor + 3 > Input.Length or else not Basic_Name_Character (Input.Data (Cursor + 3)))
            then Op := Modulo; Width := 3;
            end if;
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
      end Expression;

      function Builtin
        (Kind : Node_Kind; Style : Surface; Compact : Boolean) return String is
        (case Kind is
           when Add_Form => (if Style = Lisp then "+" else "add"),
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
              (Keyword (S) or else (for some C of S => C in '=' | '+' | '*' | '/' | '%'))
            then Emit ("`"); end if;
            Emit (S);
            if Style = Basic and then
              (Keyword (S) or else (for some C of S => C in '=' | '+' | '*' | '/' | '%'))
            then Emit ("`"); end if;
         end Identifier;
         procedure Child (Index : Node_Reference) is
         begin Print (Index, Style, Buffer, Spans, Depth + 1, Pretty); end Child;
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
            when Integer_Literal =>
               declare
                  S : constant String := Interfaces.Integer_64'Image (N.Integer_Value);
               begin Emit ((if S (S'First) = ' ' then S (S'First + 1 .. S'Last) else S)); end;
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
            when Name_Reference => Identifier (N.Identifier);
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
               if N.Kind = Host_Import_Form then
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
         Expression (0);
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
