with Calc_Balls;
with Calc_Engine;
with Calc_Format;
with Calc_Syntax;
with Calc_Words;

--  The Calculator's history (docs/calculator.md, "History"): typed
--  expressions and their results, not strings. Each entry keeps the
--  expression's postfix terms (Calc_Syntax) with its source text, the
--  result as a Calc_Balls.Ball (exact, or approximate with its stated
--  bound; ans), the options it was evaluated with, and the texts the
--  window shows, made once when the entry is added (nothing is formatted
--  per frame). The newest entry is 1. Storage grows on the heap (CuAlloc)
--  up to the capacity its owner chose; beyond it the oldest entry goes.
package Calc_History is
   type History is limited private;
   subtype Entry_Count is Natural;
   subtype Entry_Index is Positive;

   --  The texts of one result. Read in place: a result can be far longer
   --  than a secondary stack (2^100000 has 30,103 digits), so they are
   --  never returned by copy.
   type Text_Access is access String;
   type Texts is record
      --  -5/6 (an exact result; empty for an approximate one).
      Fraction : Text_Access;
      --  The decimal (an approximate result: the digits its bound
      --  guarantees) and the bound ("2e-21"; empty when exact).
      Decimal, Bound : Text_Access;
      --  Programmer results: the word's bits in base 16, 8 and 2 (two's
      --  complement, padded to the word; unbounded: sign and magnitude).
      Hexadecimal, Octal, Binary : Text_Access;
      --  The bit field's bits, the most significant first: the word's, or
      --  for an unbounded word the low 64 bits of two's complement.
      Bits : Text_Access;
      Form : Calc_Format.Decimal_Form := Calc_Format.Integer_Form;
      Exact, Whole : Boolean := True;
      Programmer : Boolean := False;
      Word : Calc_Words.Word_Size := Calc_Words.Unbounded;
      --  Some step wrapped into the word.
      Wrapped : Boolean := False;
   end record;

   --  X's texts (Into's earlier texts are released).
   procedure Describe
     (X : Calc_Balls.Ball; Mode : Calc_Engine.Options; Wrapped : Boolean; Places : Calc_Format.Places_Count;
      Into : in out Texts);
   procedure Release (Item : in out Texts);

   procedure Create (H : out History; Capacity : Positive);
   procedure Destroy (H : in out History);

   function Count (H : History) return Entry_Count;
   function Capacity (H : History) return Positive;

   --  Add Text (whose terms Parse made) and its result, evaluated with
   --  Mode; Result is taken (left reclaimed).
   procedure Add
     (H : in out History; Text : String; Terms : Calc_Syntax.Term_Array; Result : in out Calc_Balls.Ball;
      Mode : Calc_Engine.Options; Wrapped : Boolean; Places : Calc_Format.Places_Count);

   type Text_View is not null access constant String;
   function Source (H : History; Index : Entry_Index) return Text_View
     with Pre => Index <= Count (H);
   --  The entry's texts (the strings stay the history's).
   function Shown (H : History; Index : Entry_Index) return Texts
     with Pre => Index <= Count (H);
   --  Its terms, the typed expression (re-evaluating them gives Value).
   function Term_Count (H : History; Index : Entry_Index) return Natural
     with Pre => Index <= Count (H);

   --  The newest result, for ans (none when the history is empty).
   function Has_Previous (H : History) return Boolean is (Count (H) > 0);
   procedure Copy_Latest (H : History; Into : in out Calc_Balls.Ball)
     with Pre => Has_Previous (H) and then Calc_Balls.Is_Reclaimed (Into);

   --  Re-format every entry at Places (the View menu changed them).
   procedure Reformat (H : in out History; Places : Calc_Format.Places_Count);

   procedure Clear (H : in out History)
     with Post => Count (H) = 0;

   --  Evaluate Text with ans = the newest result: the one evaluation path of
   --  the app (live preview and Enter alike).
   procedure Evaluate
     (H : History; Text : String; Bounds : Calc_Engine.Limits; Mode : Calc_Engine.Options;
      Result : in out Calc_Balls.Ball; Wrapped : out Boolean; Failure : out Calc_Engine.Failure_Kind;
      Position : out Calc_Syntax.Column)
     with Pre => Calc_Balls.Is_Reclaimed (Result) and then Text'Length <= Calc_Engine.MAXIMUM_TEXT;
private
   type Terms_Access is access Calc_Syntax.Term_Array;
   type Item is record
      Source : Text_Access;
      Terms : Terms_Access;
      Value : Calc_Balls.Ball;
      Mode : Calc_Engine.Options;
      Wrapped : Boolean := False;
      Shown : Texts;
   end record;
   type Item_Access is access Item;
   type Ring is array (Natural range <>) of Item_Access;
   type Ring_Access is access Ring;
   --  Items (First) is the oldest; Count follow it, wrapping.
   type History is limited record
      Items : Ring_Access;
      First : Natural := 0;
      Used : Natural := 0;
      Limit : Positive := 1;
   end record;
end Calc_History;
