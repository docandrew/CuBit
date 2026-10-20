with Calc_Numbers; use Calc_Numbers;
with Calc_Syntax; use Calc_Syntax;

--  The calculator's one evaluation entry (docs/calculator.md, "One core,
--  three faces"): text in, an exact (or marked approximate) number or a
--  typed failure with its column out. The desktop app, the CCL call
--  calc.evaluate and any other client all evaluate through here, with the
--  limits they pass explicitly, so the same text gives the same answer.
package Calc_Engine with SPARK_Mode is
   type Limits is record
      --  The most limbs (32 bits each) any numerator or denominator may
      --  have, a result's or a step's.
      Budget : Size_Budget;
      --  Decimal digits of an approximation (sqrt of a non-square, pi, e).
      Precision : Approximate_Digits;
   end record;

   type Failure_Kind is
     (No_Failure,
      --  The text (Calc_Syntax.Syntax_Error).
      Empty_Expression, Unexpected_Character, Expected_Operand, Expected_Operator,
      Unclosed_Parenthesis, Unopened_Parenthesis, Misplaced_Comma, Unknown_Name,
      Wrong_Argument_Count, Malformed_Number,
      --  The arithmetic (Calc_Numbers.Status).
      Division_By_Zero, Too_Large, Not_An_Integer, Fractional_Exponent, Outside_Domain,
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
   function Failure_Of (State : Status) return Failure_Kind is
     (case State is
         when Done => No_Failure,
         when Calc_Numbers.Division_By_Zero => Division_By_Zero,
         when Calc_Numbers.Too_Large => Too_Large,
         when Calc_Numbers.Not_An_Integer => Not_An_Integer,
         when Calc_Numbers.Fractional_Exponent => Fractional_Exponent,
         when Calc_Numbers.Outside_Domain => Outside_Domain);

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
         when Unknown_Name => "unknown name (functions: abs floor ceil round trunc sqrt gcd lcm num den; " &
                              "constants: pi e ans)",
         when Wrong_Argument_Count => "wrong number of arguments for this function",
         when Malformed_Number => "malformed number",
         when Division_By_Zero => "division by zero",
         when Too_Large => "the result would be larger than this calculator's budget allows",
         when Not_An_Integer => "this operation takes integers only",
         when Fractional_Exponent => "the exponent must be an integer (use sqrt for square roots)",
         when Outside_Domain => "outside this function's domain (a negative number)",
         when No_Previous_Result => "ans: there is no previous result yet");

   --  Text short enough for the stack of terms and the literal digits.
   MAXIMUM_TEXT : constant := Calc_Numbers.Size_Budget'Last;

   --  Evaluate parsed terms (Calc_Syntax.Parse of Text). Previous is ans
   --  when Has_Previous. Position: the column of the failing term.
   procedure Evaluate_Terms
     (Text : String; Terms : Term_Array; Previous : Number; Has_Previous : Boolean; Bounds : Limits;
      Result : in out Number; Failure : out Failure_Kind; Position : out Column)
     with Pre => Text'First = 1 and then Text'Length <= MAXIMUM_TEXT
                 and then Terms'First = 1 and then Terms'Length <= Text'Length and then Valid_Terms (Text, Terms)
                 and then Is_Reclaimed (Result),
          Post => (if Failure = No_Failure then Within (Result, Bounds.Budget) else Is_Reclaimed (Result));

   --  Parse and evaluate.
   procedure Evaluate
     (Text : String; Previous : Number; Has_Previous : Boolean; Bounds : Limits;
      Result : in out Number; Failure : out Failure_Kind; Position : out Column)
     with Pre => Text'First = 1 and then Text'Length <= MAXIMUM_TEXT and then Is_Reclaimed (Result),
          Post => (if Failure = No_Failure then Within (Result, Bounds.Budget) else Is_Reclaimed (Result));
end Calc_Engine;
