with Text_Limits; use Text_Limits;

--  Search (docs/text-editor.md, "Search"). The document hands windows of
--  its bytes to these functions, a bounded slice per frame, so finding
--  never stalls typing.
--
--  Literal search: ASCII case folding and whole-word as options.
--
--  Regular expressions compile to a program for a Pike VM (a Thompson NFA
--  simulation): the time to match a line is linear in the line's length
--  times the program's, with no backtracking, so no pattern can hang the
--  editor. Supported: literals, '.', classes [a-z] [^...], \d \w \s \D \W \S,
--  escapes \. \\ \t \n, '*' '+' '?' (greedy, and lazy with a second '?'),
--  '|', groups, '^' '$' (line start and end), \b. A match never crosses a
--  line end. The leftmost match wins; among those that start there, the
--  one the pattern prefers (as a backtracking matcher would pick).
--
--  Proved: no runtime errors; matches lie within the text searched.
package Text_Search with SPARK_Mode is
   MAXIMUM_PATTERN : constant := 256;
   subtype Pattern_Length is Natural range 0 .. MAXIMUM_PATTERN;

   type Options is record
      Case_Sensitive : Boolean := False;
      Whole_Word : Boolean := False;
      Regular : Boolean := False;
   end record;

   --  The first literal match of Pattern in Text at or after From:
   --  Found, and the match's first index. Before and After are the bytes
   --  just outside Text (' ' when none), for whole-word tests at its edges.
   procedure Find_Literal
     (Text, Pattern : String; From : Positive; Settings : Options; Before, After : Character;
      Found : out Boolean; At_Index : out Positive)
     with Pre => Pattern'First = 1 and then Pattern'Length in 1 .. MAXIMUM_PATTERN
                 and then Text'First = 1 and then Text'Last >= 0 and then Text'Last < Positive'Last - MAXIMUM_PATTERN,
          Post => (if Found then At_Index >= From and then At_Index >= Text'First
                   and then At_Index <= Text'Last - Pattern'Length + 1);
   --  The last literal match that ends at or before Upto (an index in Text).
   procedure Find_Literal_Backward
     (Text, Pattern : String; Upto : Natural; Settings : Options; Before, After : Character;
      Found : out Boolean; At_Index : out Positive)
     with Pre => Pattern'First = 1 and then Pattern'Length in 1 .. MAXIMUM_PATTERN
                 and then Text'First = 1 and then Text'Last >= 0 and then Text'Last < Positive'Last - MAXIMUM_PATTERN,
          Post => (if Found then At_Index >= Text'First and then At_Index <= Text'Last - Pattern'Length + 1);

   type Program is private;
   type Compile_Result is (Compiled, Empty_Pattern, Too_Long, Unbalanced, Bad_Class, Bad_Repeat);

   procedure Compile (Pattern : String; Case_Sensitive : Boolean; Result : out Program; Status : out Compile_Result)
     with Pre => Pattern'First = 1 and then Pattern'Last >= 0 and then Pattern'Length <= MAXIMUM_PATTERN;

   --  The first match in Line (one line, without its line end) starting at
   --  or after From: Found, its first index and its length (0 for an
   --  empty match).
   procedure Match
     (Item : Program; Line : String; From : Positive; Found : out Boolean; First : out Positive;
      Length : out Natural)
     with Pre => Line'First = 1 and then Line'Last >= 0 and then Line'Length <= SEGMENT_BYTES,
          Post => (if Found then First in Line'First .. Line'Last + 1 and then Length <= Line'Last + 1 - First);
private
   MAXIMUM_PROGRAM : constant := 4 * MAXIMUM_PATTERN + 8;
   subtype Program_Count is Natural range 0 .. MAXIMUM_PROGRAM;
   subtype Program_Index is Program_Count range 1 .. MAXIMUM_PROGRAM;
   MAXIMUM_CLASSES : constant := 32;
   subtype Class_Count is Natural range 0 .. MAXIMUM_CLASSES;
   subtype Class_Index is Class_Count range 1 .. MAXIMUM_CLASSES;
   type Byte_Set is array (Character) of Boolean;
   type Class_Table is array (Class_Index) of Byte_Set;

   type Operation is (Literal, Any, In_Class, Split, Jump, Line_Start, Line_End, Word_Boundary, Matched);
   type Instruction is record
      Op : Operation := Matched;
      Value : Character := ' ';
      Class : Class_Index := 1;
      --  Split: X is preferred over Y. Jump: X.
      X, Y : Program_Index := 1;
   end record;
   type Instruction_Array is array (Program_Index) of Instruction;

   type Program is record
      Code : Instruction_Array := [others => (others => <>)];
      Length : Program_Count := 0;
      Classes : Class_Table := [others => [others => False]];
      Class_Used : Class_Count := 0;
      Fold : Boolean := False;
   end record;
end Text_Search;
