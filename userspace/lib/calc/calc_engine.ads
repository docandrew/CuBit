with Calc_Balls; use Calc_Balls;
with Calc_Numbers;
with Calc_Syntax; use Calc_Syntax;
with Calc_Words;

--  The calculator's one evaluation entry (docs/calculator.md, "One core,
--  three faces"): text in, an exact number (or a ball with a stated error
--  bound) or a typed failure with its column out. The desktop app, the CCL
--  calls calc.evaluate and calc.evaluate-with, and any other client all
--  evaluate through here, with the limits and options they pass
--  explicitly, so the same text gives the same answer.
package Calc_Engine with SPARK_Mode is
   type Limits is record
      --  The most limbs (32 bits each) any numerator or denominator may
      --  have, a result's or a step's.
      Budget : Calc_Numbers.Size_Budget;
      --  Decimal digits approximations are computed to.
      Precision : Precision_Digits;
   end record;

   --  How values are kept: exact rationals (standard and scientific
   --  modes), or integers in a machine word (programmer mode: / divides
   --  toward zero, every step wraps into the word).
   type Number_Domain is (Rationals, Integers);
   type Options is record
      Domain : Number_Domain;
      Word : Calc_Words.Word_Size;
      Signed : Boolean;
      Angle : Angle_Unit;
   end record;

   type Failure_Kind is
     (No_Failure,
      --  The text (Calc_Syntax.Syntax_Error).
      Empty_Expression, Unexpected_Character, Expected_Operand, Expected_Operator,
      Unclosed_Parenthesis, Unopened_Parenthesis, Misplaced_Comma, Unknown_Name,
      Wrong_Argument_Count, Malformed_Number,
      --  The arithmetic.
      Division_By_Zero, Too_Large, Not_An_Integer, Outside_Domain, Indeterminate,
      --  rotl and rotr without a word size.
      Needs_Word_Size,
      --  ans with no previous result.
      No_Previous_Result);
   subtype Syntax_Failure is Failure_Kind range Empty_Expression .. Malformed_Number;
   subtype Arithmetic_Failure is Failure_Kind range Division_By_Zero .. No_Previous_Result;

   function Failure_Of (Error : Syntax_Error) return Failure_Kind is
     (case Error is
         when No_Error => No_Failure,
         when Calc_Syntax.Empty_Expression => Empty_Expression,
         when Calc_Syntax.Unexpected_Character => Unexpected_Character,
         when Calc_Syntax.Expected_Operand => Expected_Operand,
         when Calc_Syntax.Expected_Operator => Expected_Operator,
         when Calc_Syntax.Unclosed_Parenthesis => Unclosed_Parenthesis,
         when Calc_Syntax.Unopened_Parenthesis => Unopened_Parenthesis,
         when Calc_Syntax.Misplaced_Comma => Misplaced_Comma,
         when Calc_Syntax.Unknown_Name => Unknown_Name,
         when Calc_Syntax.Wrong_Argument_Count => Wrong_Argument_Count,
         when Calc_Syntax.Malformed_Number => Malformed_Number);
   function Failure_Of (State : Calc_Balls.Status) return Failure_Kind is
     (case State is
         when Calc_Balls.Done => No_Failure,
         when Calc_Balls.Division_By_Zero => Division_By_Zero,
         when Calc_Balls.Too_Large => Too_Large,
         when Calc_Balls.Not_An_Integer => Not_An_Integer,
         when Calc_Balls.Outside_Domain => Outside_Domain,
         when Calc_Balls.Indeterminate => Indeterminate);

   --  What a person reads (the app's status line, a CCL failure's message).
   function Message (Kind : Failure_Kind) return String is
     (case Kind is
         when No_Failure => "",
         when Empty_Expression => "nothing to evaluate",
         when Unexpected_Character => "this character is not part of an expression",
         when Expected_Operand => "a number, name or ( is missing here",
         when Expected_Operator => "an operator is missing here (write 2*3, not 2 3 or 2(3))",
         when Unclosed_Parenthesis => "this ( is never closed",
         when Unopened_Parenthesis => "this ) closes nothing",
         when Misplaced_Comma => "a comma separates a function's arguments only",
         when Unknown_Name => "unknown name (F1 lists the functions; constants: pi e ans)",
         when Wrong_Argument_Count => "wrong number of arguments for this function",
         when Malformed_Number => "malformed number",
         when Division_By_Zero => "division by zero",
         when Too_Large => "the result would be larger than this calculator's budget allows",
         when Not_An_Integer => "this needs integers (programmer mode keeps integers only)",
         when Outside_Domain => "outside this function's domain",
         when Indeterminate => "too close to call at this precision (a bound straddles the answer)",
         when Needs_Word_Size => "rotl and rotr need a word size (8, 16, 32 or 64 bits)",
         when No_Previous_Result => "ans: there is no previous result yet");

   --  Text short enough for the stack of terms and the literal digits.
   MAXIMUM_TEXT : constant := Calc_Numbers.Size_Budget'Last;

   --  Evaluate parsed terms (Calc_Syntax.Parse of Text). Previous is ans
   --  when Has_Previous. Position: the column of the failing term.
   --  Wrapped: some step wrapped into the word (programmer mode).
   procedure Evaluate_Terms
     (Text : String; Terms : Term_Array; Previous : Ball; Has_Previous : Boolean; Bounds : Limits;
      Mode : Options; Result : in out Ball; Wrapped : out Boolean; Failure : out Failure_Kind;
      Position : out Column)
     with Pre => Text'First = 1 and then Text'Length <= MAXIMUM_TEXT
                 and then Terms'First = 1 and then Terms'Length <= Text'Length and then Valid_Terms (Text, Terms)
                 and then Is_Reclaimed (Result),
          Post => (if Failure = No_Failure then Within (Result, Bounds.Budget) else Is_Reclaimed (Result));

   --  Parse and evaluate.
   procedure Evaluate
     (Text : String; Previous : Ball; Has_Previous : Boolean; Bounds : Limits; Mode : Options;
      Result : in out Ball; Wrapped : out Boolean; Failure : out Failure_Kind; Position : out Column)
     with Pre => Text'First = 1 and then Text'Length <= MAXIMUM_TEXT and then Is_Reclaimed (Result),
          Post => (if Failure = No_Failure then Within (Result, Bounds.Budget) else Is_Reclaimed (Result));
end Calc_Engine;
