--  Lexical classification of CCL Lisp source for presentation: every byte of
--  the source gets a class and, for delimiters, a nesting depth. Front ends
--  (the console, the Workbench, the Observatory) colour from these classes;
--  none of them carries its own tokenizer. Classification is purely lexical
--  and never evaluates or resolves names against a catalog.
package CCL.Highlighting with SPARK_Mode is
   type Token_Class is
     (Whitespace,
      Comment,
      Delimiter,       --  ( and ), with a nesting depth
      Mismatch,        --  a ')' with no matching '('
      Special_Form,    --  define, let, if, match, fn, ...
      Operator,        --  built-in operations: + = concat sort-by ...
      Host_Operation,  --  qualified service names: logs.recent
      Type_Name,       --  capitalised names: Pair, LogEntry
      Call_Name,       --  other names in operator position
      Name,            --  other names
      Number,
      Boolean_Literal,
      Text_Literal,
      Unterminated);   --  a string that does not close on its line

   --  Depths beyond this all show as the deepest level.
   Maximum_Depth : constant := 255;
   subtype Nesting_Depth is Natural range 0 .. Maximum_Depth;

   type Mark is record
      Class : Token_Class := Whitespace;
      Depth : Nesting_Depth := 0;
   end record;
   type Mark_Map is array (Positive range <>) of Mark;

   procedure Classify (Source : String; Marks : out Mark_Map)
   with Pre => Marks'First = 1 and then Marks'Length = Source'Length;

   --  Whether the source is a complete entry, so a front end can submit on
   --  Enter or continue on a new line instead.
   type Form_State is
     (Empty,        --  only whitespace and comments
      Complete,     --  every form closed
      Open_Forms,   --  more '(' than ')': keep reading
      Malformed);   --  a stray ')' or an unterminated string: submit to
                    --  get the diagnostic
   function Balance (Source : String) return Form_State;

   --  The number of open forms at the end of the source (0 if malformed).
   function Open_Depth (Source : String) return Natural;
end CCL.Highlighting;
