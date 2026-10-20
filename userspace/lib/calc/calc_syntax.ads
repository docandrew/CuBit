with Calc_Numbers; use Calc_Numbers;

--  The calculator's expression language (docs/calculator.md, "Expressions"):
--  text to a typed expression, the terms in evaluation (postfix) order.
--  An expression is data: the history keeps it, ans refers to the previous
--  result, and evaluating it again gives the same answer.
--
--    expression  sum
--    sum         product { (+ | -) product }
--    product     unary { (* | / | mod) unary }
--    unary       (- | +) unary | power
--    power       postfix [ ^ unary ]            right-associative: -2^2 = -4
--    postfix     primary { ! }
--    primary     number | ans | pi | e | name ( arguments ) | ( expression )
--    number      digits [ . digits ] [ (e | E) [+ | -] digits ], or . digits
--
--  There is no implicit multiplication: 2(3) is an error, not 6.
--  Parsing is iterative (shunting-yard): nesting is limited only by the
--  text's length, never by a stack depth.
--
--  Proved (level 2): no run-time errors; every term's spans lie inside the
--  text, and a literal's spans hold digits only.
package Calc_Syntax with SPARK_Mode is
   --  Column of a token in the text (1 = first character).
   subtype Column is Positive;

   type Term_Kind is (Literal, Previous_Result, Named_Constant, Unary, Binary);
   type Term is record
      Kind : Term_Kind := Literal;
      --  Where the term's token starts, for messages (the operator's
      --  column for an operation).
      Position : Column := 1;
      --  Literal: the whole and fraction digits (empty when Last < First)
      --  and the exponent.
      Whole_First, Fraction_First : Column := 1;
      Whole_Last, Fraction_Last : Natural := 0;
      Exponent : Decimal_Exponent := 0;
      Unary_Operation : Calc_Numbers.Unary_Operation := Negate;
      Binary_Operation : Calc_Numbers.Binary_Operation := Add;
      Value : Constant_Value := Pi;
   end record;
   type Term_Array is array (Positive range <>) of Term;

   type Syntax_Error is
     (No_Error,
      Empty_Expression,
      --  A character that starts no token.
      Unexpected_Character,
      --  A number, name or ( was due (after an operator, at the end).
      Expected_Operand,
      --  An operator, ) or the end was due (2 3, 2(3)).
      Expected_Operator,
      Unclosed_Parenthesis,
      Unopened_Parenthesis,
      --  A , outside a function's arguments.
      Misplaced_Comma,
      Unknown_Name,
      --  sqrt(1, 2), gcd(4).
      Wrong_Argument_Count,
      --  1e, 1.e5, a digitless exponent.
      Malformed_Number);

   --  Terms written by Parse, at most one per character.
   function Valid_Terms (Text : String; Terms : Term_Array) return Boolean is
     (for all T of Terms =>
        T.Position in Text'Range
        and then (if T.Kind = Literal then
                    (if T.Whole_Last >= T.Whole_First then
                       T.Whole_First in Text'Range and then T.Whole_Last in Text'Range
                       and then (for all I in T.Whole_First .. T.Whole_Last => Text (I) in '0' .. '9'))
                    and then (if T.Fraction_Last >= T.Fraction_First then
                       T.Fraction_First in Text'Range and then T.Fraction_Last in Text'Range
                       and then (for all I in T.Fraction_First .. T.Fraction_Last => Text (I) in '0' .. '9'))));

   procedure Parse
     (Text : String; Terms : out Term_Array; Count : out Natural;
      Error : out Syntax_Error; Error_Position : out Column)
     with Pre => Text'First = 1 and then Text'Last < Positive'Last
                 and then Terms'First = 1 and then Terms'Length >= Text'Length,
          Post => Count <= Text'Length
                  and then (if Error = No_Error then Count > 0
                            and then Valid_Terms (Text, Terms (1 .. Count)));

   --  The name a function or constant is written with.
   function Unary_Name (Item : Calc_Numbers.Unary_Operation) return String is
     (case Item is
         when Negate => "-", when Absolute => "abs", when Floor => "floor", when Ceiling => "ceil",
         when Round => "round", when Truncate => "trunc", when Factorial => "!",
         when Square_Root => "sqrt", when Numerator_Of => "num", when Denominator_Of => "den");
   function Binary_Name (Item : Calc_Numbers.Binary_Operation) return String is
     (case Item is
         when Add => "+", when Subtract => "-", when Multiply => "*", when Divide => "/",
         when Power => "^", when Modulo => "mod", when Greatest_Common_Divisor => "gcd",
         when Least_Common_Multiple => "lcm", when Round_To => "round");
   function Constant_Name (Item : Constant_Value) return String is
     (case Item is when Pi => "pi", when Euler => "e");
   PREVIOUS_RESULT_NAME : constant String := "ans";
end Calc_Syntax;
