with Interfaces;

--  CCL's string built-ins, shared by the interpreter (CCL.Language) and the
--  bytecode VM (CCL.VM): one implementation on plain Strings. Each engine
--  only copies its operands out of its own text region and stores the
--  result, so the two cannot disagree about what a built-in does.
--
--  The subject string is the last operand of every built-in in source
--  ((replace old new s)); these procedures take it first.
package CCL.Text_Operations with SPARK_Mode => On is
   use Interfaces;

   --  The longest string either engine holds (CCL.Objects.Maximum_Text_Bytes).
   MAX_STRING : constant := 8_192;
   subtype String_Length is Natural range 0 .. MAX_STRING;

   --  The operations a compiled program can name; Text_Builtin's immediate.
   type Operation is
     (Upper, Lower, Trim, Reverse_Text, First_Chars, Last_Chars, Skip_Chars,
      Contains, Index_Of, Starts_With, Ends_With, Replace, Parse_Int);
   for Operation use
     (Upper => 0, Lower => 1, Trim => 2, Reverse_Text => 3, First_Chars => 4,
      Last_Chars => 5, Skip_Chars => 6, Contains => 7, Index_Of => 8,
      Starts_With => 9, Ends_With => 10, Replace => 11, Parse_Int => 12);

   --  Signatures, for the type checker and the bytecode verifier.
   type Operand_Kind is (Text_Operand, Integer_Operand, Boolean_Operand);
   subtype Operand_Count is Natural range 0 .. 2;
   type Operand_Kinds is array (1 .. 2) of Operand_Kind;
   type Signature is record
      --  Operands before the subject, in source order.
      Count : Operand_Count := 0;
      Operands : Operand_Kinds := [others => Text_Operand];
      Result : Operand_Kind := Text_Operand;
   end record;
   function Signature_Of (Item : Operation) return Signature is
     (case Item is
         when Upper | Lower | Trim | Reverse_Text => (0, [others => Text_Operand], Text_Operand),
         when First_Chars | Last_Chars | Skip_Chars =>
           (1, [Integer_Operand, Text_Operand], Text_Operand),
         when Contains | Starts_With | Ends_With => (1, [others => Text_Operand], Boolean_Operand),
         when Index_Of => (1, [others => Text_Operand], Integer_Operand),
         when Replace => (2, [others => Text_Operand], Text_Operand),
         when Parse_Int => (0, [others => Text_Operand], Integer_Operand));

   type Outcome is (Done, Text_Exhausted, Invalid_Number, Overflow);

   function Is_Blank (C : Character) return Boolean is
     (C in ' ' | ASCII.HT | ASCII.LF | ASCII.CR);

   --  Upper, Lower and Reverse_Text: Result has Subject's length.
   procedure Transform
     (Item : Operation; Subject : String; Result : out String)
   with Pre => Item in Upper | Lower | Reverse_Text and then
               Result'Length = Subject'Length;

   --  The bounds of Subject's slice for Trim, First_Chars, Last_Chars and
   --  Skip_Chars (N characters, clamped to 0 .. Subject'Length).
   procedure Slice
     (Item : Operation; Subject : String; N : Integer_64;
      Low : out Positive; High : out Natural)
   with Pre => Item in Trim | First_Chars | Last_Chars | Skip_Chars and then
               Subject'First = 1 and then Subject'Length <= MAX_STRING,
        Post => Low >= 1 and then High <= Subject'Length and then
                (High < Low or else Low <= Subject'Length);

   --  The first position of Pattern in Subject at or after From, or 0. An
   --  empty pattern is found at From.
   function Find (Subject, Pattern : String; From : Positive) return Natural
   with Pre => Subject'First = 1 and then Pattern'First = 1 and then
               Subject'Length <= MAX_STRING and then Pattern'Length <= MAX_STRING,
        Post => Find'Result = 0 or else
                (Find'Result >= From and then
                 Find'Result + Pattern'Length <= Subject'Length + 1);

   --  Contains, Starts_With and Ends_With.
   function Test (Item : Operation; Subject, Pattern : String) return Boolean
   with Pre => Item in Contains | Starts_With | Ends_With and then
               Subject'First = 1 and then Pattern'First = 1 and then
               Subject'Length <= MAX_STRING and then Pattern'Length <= MAX_STRING;

   --  Every occurrence of Pattern, left to right, by Replacement. An empty
   --  pattern changes nothing. Text_Exhausted when the result would exceed
   --  Result'Length.
   procedure Replace_All
     (Subject, Pattern, Replacement : String;
      Result : out String; Length : out String_Length; Status : out Outcome)
   with Pre => Subject'First = 1 and then Pattern'First = 1 and then
               Replacement'First = 1 and then Result'First = 1 and then
               Subject'Length <= MAX_STRING and then Pattern'Length <= MAX_STRING and then
               Replacement'Length <= MAX_STRING and then Result'Length = MAX_STRING,
        Post => (if Status = Done then Length <= Result'Length);

   --  split's scanner: the next piece of Subject from Position. With a
   --  separator, the pieces between separators, empty ones included, and
   --  always a last piece; with an empty separator, the runs of non-blank
   --  characters (words). Finished becomes True after the last piece;
   --  Found is False when there was none left.
   procedure Next_Piece
     (Subject, Separator : String; Position : in out Positive;
      Finished : in out Boolean; Low : out Positive; High : out Natural;
      Found : out Boolean)
   with Pre => Subject'First = 1 and then Separator'First = 1 and then
               Subject'Length <= MAX_STRING and then Separator'Length <= MAX_STRING and then
               Position <= Subject'Length + 1,
        Post => Position <= Subject'Length + 1 and then
                (if Found then High <= Subject'Length and then Low <= High + 1);

   --  An integer's decimal text (to-string): a sign when negative, then
   --  up to 19 digits.
   MAX_INTEGER_IMAGE : constant := 20;
   function Decimal_Image (Value : Integer_64) return String
   with Post => Decimal_Image'Result'Length in 1 .. MAX_INTEGER_IMAGE;

   --  Optional blanks and sign, then decimal digits: Invalid_Number or
   --  Overflow otherwise.
   procedure Parse_Integer (Subject : String; Value : out Integer_64; Status : out Outcome)
   with Pre => Subject'First = 1 and then Subject'Length <= MAX_STRING;
end CCL.Text_Operations;
