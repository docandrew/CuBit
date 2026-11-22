--  Golden vectors for the browser's CCL highlighter: what the native
--  CCL.Highlighting says of a set of sources, as JSON on standard output.
--  run.sh compares it with userspace/ccl/tools/ccl-observatory/
--  highlight-vectors.json, which highlight.test.mjs checks the JS port
--  against, so neither side can drift alone.
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Highlighting; use CCL.Highlighting;
with CCL.Language;

procedure Vectors is
   use type CCL.Language.Builtin_Operation;
   function Class_Name (Class : Token_Class) return String is
     (case Class is
         when Whitespace => "whitespace", when Comment => "comment",
         when Delimiter => "delimiter", when Mismatch => "mismatch",
         when Special_Form => "special-form", when Operator => "operator",
         when Host_Operation => "host-operation", when Type_Name => "type-name",
         when Call_Name => "call-name", when Name => "name", when Number => "number",
         when Boolean_Literal => "boolean", when Text_Literal => "text",
         when Unterminated => "unterminated");
   function Balance_Name (State : Form_State) return String is
     (case State is
         when Empty => "empty", when Complete => "complete",
         when Open_Forms => "open", when Malformed => "malformed");
   function Quoted (Text : String) return String is
      Result : String (1 .. Text'Length * 6 + 2);
      Last : Natural := 1;
      procedure Put_Char (C : Character) is
      begin
         Last := Last + 1; Result (Last) := C;
      end Put_Char;
   begin
      Result (1) := '"';
      for C of Text loop
         case C is
            when '"' => Put_Char ('\'); Put_Char ('"');
            when '\' => Put_Char ('\'); Put_Char ('\');
            when ASCII.LF => Put_Char ('\'); Put_Char ('n');
            when ASCII.HT => Put_Char ('\'); Put_Char ('t');
            when ASCII.CR => Put_Char ('\'); Put_Char ('r');
            when others => Put_Char (C);
         end case;
      end loop;
      Put_Char ('"');
      return Result (1 .. Last);
   end Quoted;
   First_Vector : Boolean := True;
   procedure Vector (Source : String) is
      Marks : Mark_Map (1 .. Source'Length);
   begin
      Classify (Source, Marks);
      Put ((if First_Vector then "  " else ",  ") & "{""source"": " & Quoted (Source) &
           ", ""balance"": """ & Balance_Name (Balance (Source)) & """, ""marks"": [");
      First_Vector := False;
      for I in Marks'Range loop
         Put ((if I > Marks'First then "," else "") & "[""" & Class_Name (Marks (I).Class) & """," &
              Natural'Image (Marks (I).Depth) & "]");
      end loop;
      Put_Line ("]}");
   end Vector;
   function Words return String is
      Result : String (1 .. 2_000);
      Last : Natural := 0;
      procedure Add (Word : String) is
      begin
         Result (Last + 1 .. Last + Word'Length + 3) := "(" & Word & " )";
         Last := Last + Word'Length + 3;
      end Add;
   begin
      for Form in Form_Word loop Add (Special_Form_Name (Form)); end loop;
      for Operator in Core_Operator loop Add (Core_Operator_Name (Operator)); end loop;
      for Operation in CCL.Language.Builtin_Operation loop
         if Operation /= CCL.Language.No_Builtin then Add (CCL.Language.Builtin_Name (Operation)); end if;
      end loop;
      return Result (1 .. Last);
   end Words;
begin
   Put_Line ("[");
   Vector ("(+ 20 22)");
   Vector ("(sort (list 5 3 9 1))");
   Vector ("(concat ""a \""b\"" c"" (to-string -42))");
   Vector ("# a comment" & ASCII.LF & "(logs.recent ""clock"")");
   Vector ("(define (twice (n Integer)) Integer (+ n n))");
   Vector ("(type Pair (record (a Integer) (s String)))");
   Vector ("(if true (f x) false)");
   Vector ("(+ 1 (* 2 (- 3 (/ 4 (% 5 6)))))");
   Vector ("(+ 1 2))");
   Vector ("(concat ""open");
   Vector ("(+ 1" & ASCII.LF & "  2");
   Vector ("  ");
   Vector ("-");
   Vector ("(image.plot (list 3 1 4))");
   Vector (Words);
   Put_Line ("]");
end Vectors;
