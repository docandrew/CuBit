with Text_Limits; use Text_Limits;

--  Highlighting (docs/text-editor.md, "Highlighting"): one lexer per
--  language, each a state machine run one line at a time. Given the state
--  at a line's start it gives the line's tokens and the state at the next
--  line's start. It never looks back and never backtracks, so lexing a
--  line costs O(its length) whatever the text, and the document keeps the
--  state every CHECKPOINT_LINES lines to resume anywhere.
--
--  Only constructs that span lines need state: C and Rust block comments
--  (Rust's nest), Rust multi-line and raw strings (with their hash count),
--  C preprocessor continuations, Markdown fenced code. Proved: no runtime
--  errors; spans lie within the line, in order, without overlap.
package Text_Syntax with SPARK_Mode is
   type Language is (Plain_Text, Ada_Source, CCL_Source, C_Source, Rust_Source, Markdown);

   type Token_Kind is
     (Plain, Keyword, Type_Name, Number, String_Text, Character_Text, Comment, Preprocessor, Attribute,
      Operator, Label, Heading, Emphasis, Strong, Code, Link, Quote, List_Marker, Escape);

   type Lexer_Mode is (Normal, Block_Comment, Long_String, Raw_String, Continued_Directive, Fenced_Code);
   MAXIMUM_NESTING : constant := 255;
   subtype Nesting is Natural range 0 .. MAXIMUM_NESTING;
   type Line_State is record
      Mode : Lexer_Mode := Normal;
      Depth : Nesting := 0;     --  nested block comments
      Hashes : Nesting := 0;    --  a raw string's closing hashes
   end record;
   INITIAL_STATE : constant Line_State := (Mode => Normal, Depth => 0, Hashes => 0);

   type Span is record
      First : Segment_Position := 1;   --  from the line's first byte, 1-based
      Length : Segment_Length := 0;
      Kind : Token_Kind := Plain;
   end record;
   MAXIMUM_SPANS : constant := 1_024;
   subtype Span_Count is Natural range 0 .. MAXIMUM_SPANS;
   subtype Span_Index is Span_Count range 1 .. MAXIMUM_SPANS;
   type Span_Array is array (Span_Index) of Span;

   --  The language of a file name, by its extension.
   function Language_Of (Name : String) return Language;
   function Name_Of (Item : Language) return String;
   --  The line comment prefix ("" when the language has none).
   function Comment_Prefix (Item : Language) return String;

   --  Lex one line (without its line end; at most one segment). Spans
   --  cover only highlighted runs; everything else is Plain. Past
   --  MAXIMUM_SPANS the rest of the line is plain.
   procedure Lex_Line
     (Item : Language; Text : String; Start : Line_State; Spans : out Span_Array; Count : out Span_Count;
      Finish : out Line_State)
     with Pre => Text'Length <= SEGMENT_BYTES and then Text'First = 1,
          Post => (for all K in 1 .. Count => Spans (K).First + Spans (K).Length - 1 <= Text'Length
                   and then Spans (K).Length >= 1)
                  and then (for all K in 2 .. Count => Spans (K).First >= Spans (K - 1).First + Spans (K - 1).Length);
end Text_Syntax;
