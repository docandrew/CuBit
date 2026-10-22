with Interfaces;
with Calc_Naturals;
with Calc_Numbers;

--  The calculator's expression language (docs/calculator.md, "Expressions"):
--  text to a typed expression, the terms in evaluation (postfix) order.
--  An expression is data: the history keeps it, ans refers to the previous
--  result, and evaluating it again gives the same answer. One language for
--  every mode: a mode changes how values are kept (exact rationals or
--  machine words) and the angle unit, never what an expression means.
--
--    expression  or
--    or          xor { | xor }
--    xor         and { xor and }
--    and         shift { & shift }
--    shift       sum { (<< | >>) sum }
--    sum         product { (+ | -) product }
--    product     unary { (* | / | mod) unary }
--    unary       (- | + | ~) unary | power
--    power       postfix [ ^ unary ]            right-associative: -2^2 = -4
--    postfix     primary { ! }
--    primary     number | ans | pi | e | name ( arguments ) | ( expression )
--    number      digits [ . digits ] [ (e | E) [+ | -] digits ], or . digits,
--                or 0x hex digits, 0o octal digits, 0b binary digits
--
--  There is no implicit multiplication: 2(3) is an error, not 6.
--  Parsing is iterative (shunting-yard): nesting is limited only by the
--  text's length, never by a stack depth.
--
--  Proved (level 2): no run-time errors; every term's spans lie inside the
--  text, and a literal's spans hold digits of its base only.
package Calc_Syntax with SPARK_Mode is
   use type Interfaces.Unsigned_32;
   --  Column of a token in the text (1 = first character).
   subtype Column is Positive;

   type Operation is
     (--  Unary: prefix, postfix (!) and one-argument functions.
      Negate, Bit_Not, Absolute, Floor, Ceiling, Round, Truncate, Factorial, Square_Root, Numerator_Of,
      Denominator_Of, Sine, Cosine, Tangent, Arcsine, Arccosine, Arctangent, Natural_Log, Common_Log,
      Exponential,
      --  Binary: infix operators and two-argument functions.
      Add, Subtract, Multiply, Divide, Power, Modulo, Greatest_Common_Divisor, Least_Common_Multiple,
      Round_To, Root, Bit_And, Bit_Or, Bit_Xor, Shift_Left, Shift_Right, Rotate_Left, Rotate_Right);
   subtype Unary_Operation is Operation range Negate .. Exponential;
   subtype Binary_Operation is Operation range Add .. Rotate_Right;
   type Constant_Value is (Pi, Euler);

   subtype Literal_Base is Calc_Naturals.Digit_Base;
   DECIMAL : constant Literal_Base := 10;

   type Term_Kind is (Literal, Previous_Result, Named_Constant, Unary, Binary);
   type Term is record
      Kind : Term_Kind := Literal;
      --  Where the term's token starts, for messages (the operator's
      --  column for an operation).
      Position : Column := 1;
      --  Literal: its base; the whole and fraction digits (empty when
      --  Last < First; a fraction and an exponent in base 10 only).
      Base : Literal_Base := DECIMAL;
      Whole_First, Fraction_First : Column := 1;
      Whole_Last, Fraction_Last : Natural := 0;
      Exponent : Calc_Numbers.Decimal_Exponent := 0;
      Operator : Operation := Negate;
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
      --  1e, 1.e5, a digitless exponent, 0x without digits.
      Malformed_Number);

   --  Terms written by Parse, at most one per character.
   function Valid_Terms (Text : String; Terms : Term_Array) return Boolean is
     (for all T of Terms =>
        T.Position in Text'Range
        and then (if T.Kind = Unary then T.Operator in Unary_Operation)
        and then (if T.Kind = Binary then T.Operator in Binary_Operation)
        and then (if T.Kind = Literal then
                    (if T.Whole_Last >= T.Whole_First then
                       T.Whole_First in Text'Range and then T.Whole_Last in Text'Range
                       and then (for all I in T.Whole_First .. T.Whole_Last =>
                                   Calc_Naturals.Is_Digit (Text (I), T.Base)))
                    and then (if T.Fraction_Last >= T.Fraction_First then
                       T.Base = DECIMAL
                       and then T.Fraction_First in Text'Range and then T.Fraction_Last in Text'Range
                       and then (for all I in T.Fraction_First .. T.Fraction_Last => Text (I) in '0' .. '9'))));

   procedure Parse
     (Text : String; Terms : out Term_Array; Count : out Natural;
      Error : out Syntax_Error; Error_Position : out Column)
     with Pre => Text'First = 1 and then Text'Last < Positive'Last
                 and then Terms'First = 1 and then Terms'Length >= Text'Length,
          Post => Count <= Text'Length
                  and then (if Error = No_Error then Count > 0
                            and then Valid_Terms (Text, Terms (1 .. Count)));

   --  How an operation is written: its operator or function name.
   function Name (Item : Operation) return String is
     (case Item is
         when Negate => "-", when Bit_Not => "~", when Absolute => "abs", when Floor => "floor",
         when Ceiling => "ceil", when Round | Round_To => "round", when Truncate => "trunc",
         when Factorial => "!", when Square_Root => "sqrt", when Numerator_Of => "num",
         when Denominator_Of => "den", when Sine => "sin", when Cosine => "cos", when Tangent => "tan",
         when Arcsine => "asin", when Arccosine => "acos", when Arctangent => "atan",
         when Natural_Log => "ln", when Common_Log => "log", when Exponential => "exp",
         when Add => "+", when Subtract => "-", when Multiply => "*", when Divide => "/", when Power => "^",
         when Modulo => "mod", when Greatest_Common_Divisor => "gcd", when Least_Common_Multiple => "lcm",
         when Root => "root", when Bit_And => "&", when Bit_Or => "|", when Bit_Xor => "xor",
         when Shift_Left => "<<", when Shift_Right => ">>", when Rotate_Left => "rotl",
         when Rotate_Right => "rotr");
   function Constant_Name (Item : Constant_Value) return String is
     (case Item is when Pi => "pi", when Euler => "e");
   PREVIOUS_RESULT_NAME : constant String := "ans";
end Calc_Syntax;
