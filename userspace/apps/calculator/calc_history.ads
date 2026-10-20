with Calc_Engine;
with Calc_Format;
with Calc_Numbers;
with Calc_Syntax;

--  The Calculator's history (docs/calculator.md, "History"): typed
--  expressions and their exact results, not strings. Each entry keeps the
--  expression's postfix terms (Calc_Syntax) with its source text, the
--  result as an exact Calc_Numbers.Number (ans), and the texts the window
--  shows, made once when the entry is added (nothing is formatted per
--  frame). The newest entry is 1. Storage grows on the heap (CuAlloc)
--  up to the capacity its owner chose; beyond it the oldest entry goes.
package Calc_History is
   type History is limited private;
   subtype Entry_Count is Natural;
   subtype Entry_Index is Positive;

   procedure Create (H : out History; Capacity : Positive);
   procedure Destroy (H : in out History);

   function Count (H : History) return Entry_Count;
   function Capacity (H : History) return Positive;

   --  Add Text (whose terms Parse made) and its result; Result is taken
   --  (left reclaimed). Places: the decimal text's places.
   procedure Add
     (H : in out History; Text : String; Terms : Calc_Syntax.Term_Array;
      Result : in out Calc_Numbers.Number; Places : Calc_Format.Places_Count);

   --  The texts, read in place: a result can be far longer than a
   --  secondary stack (2^100000 has 30,103 digits), so they are never
   --  returned by copy.
   type Text_View is not null access constant String;
   function Source (H : History; Index : Entry_Index) return Text_View
     with Pre => Index <= Count (H);
   function Fraction (H : History; Index : Entry_Index) return Text_View
     with Pre => Index <= Count (H);
   function Decimal (H : History; Index : Entry_Index) return Text_View
     with Pre => Index <= Count (H);
   function Form (H : History; Index : Entry_Index) return Calc_Format.Decimal_Form
     with Pre => Index <= Count (H);
   function Kind (H : History; Index : Entry_Index) return Calc_Numbers.Exactness
     with Pre => Index <= Count (H);
   function Is_Integer (H : History; Index : Entry_Index) return Boolean
     with Pre => Index <= Count (H);
   --  Its terms, the typed expression (re-evaluating them gives Value).
   function Term_Count (H : History; Index : Entry_Index) return Natural
     with Pre => Index <= Count (H);

   --  The newest result, for ans (none when the history is empty).
   function Has_Previous (H : History) return Boolean is (Count (H) > 0);
   procedure Copy_Latest (H : History; Into : in out Calc_Numbers.Number)
     with Pre => Has_Previous (H) and then Calc_Numbers.Is_Reclaimed (Into);

   --  Re-format every entry at Places (the View menu changed them).
   procedure Reformat (H : in out History; Places : Calc_Format.Places_Count);

   procedure Clear (H : in out History)
     with Post => Count (H) = 0;

   --  Evaluate Text with ans = the newest result: the one evaluation path of
   --  the app (live preview and Enter alike).
   procedure Evaluate
     (H : History; Text : String; Bounds : Calc_Engine.Limits;
      Result : in out Calc_Numbers.Number; Failure : out Calc_Engine.Failure_Kind;
      Position : out Calc_Syntax.Column)
     with Pre => Calc_Numbers.Is_Reclaimed (Result) and then Text'Length <= Calc_Engine.MAXIMUM_TEXT;
private
   type Text_Access is access String;
   type Terms_Access is access Calc_Syntax.Term_Array;
   type Item is record
      Source, Fraction, Decimal : Text_Access;
      Terms : Terms_Access;
      Value : Calc_Numbers.Number;
      Form : Calc_Format.Decimal_Form := Calc_Format.Integer_Form;
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
