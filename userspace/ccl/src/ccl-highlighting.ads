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

   --  The special forms and core operators of CCL.Language, named here so
   --  front ends can list them (completion) as well as colour them. The
   --  named built-ins are CCL.Language.Builtin_Operation.
   type Form_Word is
     (Define_Form, Type_Form, Let_Form, If_Form, Match_Form, Fn_Form,
      Handler_Form, Field_Form, List_Form, List_Of_Form, Stream_Form, Thread_Form,
      And_Form, Or_Form, Not_Form);
   function Special_Form_Name (Form : Form_Word) return String is
     (case Form is
         when Define_Form => "define", when Type_Form => "type",
         when Let_Form => "let", when If_Form => "if",
         when Match_Form => "match", when Fn_Form => "fn",
         when Handler_Form => "handler", when Field_Form => "field",
         when List_Form => "list", when List_Of_Form => "list-of",
         when Stream_Form => "stream",
         when Thread_Form => "->>", when And_Form => "and",
         when Or_Form => "or", when Not_Form => "not");
   type Core_Operator is
     (Plus, Minus, Times, Quotient, Remainder, Equal_Sign, Unequal_Sign,
      Less_Sign, Less_Equal_Sign, Greater_Sign, Greater_Equal_Sign,
      Add_Word, Subtract_Word, Multiply_Word, Divide_Word, Mod_Word,
      Modulo_Word, Equal_Word, Not_Equal_Word, Less_Word, Less_Equal_Word,
      Greater_Word, Greater_Equal_Word, At_Word, Concat_Word, Length_Word,
      To_String_Word);
   function Core_Operator_Name (Operator : Core_Operator) return String is
     (case Operator is
         when Plus => "+", when Minus => "-", when Times => "*",
         when Quotient => "/", when Remainder => "%", when Equal_Sign => "=",
         when Unequal_Sign => "/=", when Less_Sign => "<",
         when Less_Equal_Sign => "<=", when Greater_Sign => ">",
         when Greater_Equal_Sign => ">=", when Add_Word => "add",
         when Subtract_Word => "subtract", when Multiply_Word => "multiply",
         when Divide_Word => "divide", when Mod_Word => "mod",
         when Modulo_Word => "modulo", when Equal_Word => "equal",
         when Not_Equal_Word => "not-equal", when Less_Word => "less",
         when Less_Equal_Word => "less-equal", when Greater_Word => "greater",
         when Greater_Equal_Word => "greater-equal", when At_Word => "at",
         when Concat_Word => "concat", when Length_Word => "length",
         when To_String_Word => "to-string");

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
