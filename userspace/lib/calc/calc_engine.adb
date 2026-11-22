with Interfaces;
with Calc_Limbs;
with Calc_Naturals;

package body Calc_Engine with SPARK_Mode is
   pragma Warnings (GNATprove, Off, "statement has no effect", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Free"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Release"" but not used", Reason => "frees end ownership");
   pragma Warnings (GNATprove, Off, "is set by ""Move"" but not used", Reason => "moves end ownership");

   use type Calc_Words.Word_Size;
   use type Calc_Numbers.Status;
   use type Interfaces.Unsigned_32;
   subtype Number is Calc_Numbers.Number;
   package N renames Calc_Numbers;
   type Ball_Array is array (Positive range <>) of Ball;

   ---------------------------------------------------------------------------
   --  Steps on exact operands (the integer operations).

   procedure Exact_Binary
     (Operation : N.Binary_Operation; A, B : Ball; Budget : N.Size_Budget; Made : in out Ball;
      Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (A, Budget) and then Within (B, Budget),
          Post => (if Failure = No_Failure then Within (Made, Budget) else Is_Reclaimed (Made))
   is
      Left, Right, Value : Number;
      State : N.Status;
   begin
      if not Is_Exact (A) or else not Is_Exact (B) then
         Failure := Not_An_Integer;
         return;
      end if;
      Midpoint (A, Left);
      Midpoint (B, Right);
      N.Apply (Operation, Left, Right, Budget, Value, State);
      N.Free (Left);
      N.Free (Right);
      Failure := (case State is
                     when N.Done => No_Failure, when N.Division_By_Zero => Division_By_Zero,
                     when N.Too_Large => Too_Large, when N.Not_An_Integer => Not_An_Integer,
                     when N.Outside_Domain => Outside_Domain, when N.Inexact => Indeterminate);
      if Failure = No_Failure then
         From_Exact (Value, Made);
      end if;
      N.Free (Value);
   end Exact_Binary;

   procedure Exact_Unary
     (Operation : N.Unary_Operation; X : Ball; Budget : N.Size_Budget; Made : in out Ball;
      Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (X, Budget),
          Post => (if Failure = No_Failure then Within (Made, Budget) else Is_Reclaimed (Made))
   is
      Operand, Value : Number;
      State : N.Status;
   begin
      if not Is_Exact (X) then
         Failure := Not_An_Integer;
         return;
      end if;
      Midpoint (X, Operand);
      N.Apply (Operation, Operand, Budget, Value, State);
      N.Free (Operand);
      Failure := (case State is
                     when N.Done => No_Failure, when N.Division_By_Zero => Division_By_Zero,
                     when N.Too_Large => Too_Large, when N.Not_An_Integer => Not_An_Integer,
                     when N.Outside_Domain => Outside_Domain, when N.Inexact => Indeterminate);
      if Failure = No_Failure then
         From_Exact (Value, Made);
      end if;
      N.Free (Value);
   end Exact_Unary;

   --  a mod b = a - b floor (a / b) for balls.
   procedure Ball_Modulo
     (A, B : Ball; Bounds : Limits; Made : in out Ball; Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (A, Bounds.Budget) and then Within (B, Bounds.Budget),
          Post => (if Failure = No_Failure then Within (Made, Bounds.Budget) else Is_Reclaimed (Made))
   is
      Ratio, Whole, Scaled : Ball;
      State : Calc_Balls.Status;
   begin
      Apply (Calc_Balls.Divide, A, B, Bounds.Precision, Bounds.Budget, Ratio, State);
      if State = Calc_Balls.Done then
         Round_To_Integer (Calc_Balls.Floor, Ratio, Bounds.Budget, Whole, State);
      end if;
      if State = Calc_Balls.Done then
         Apply (Calc_Balls.Multiply, B, Whole, Bounds.Precision, Bounds.Budget, Scaled, State);
      end if;
      if State = Calc_Balls.Done then
         Apply (Calc_Balls.Subtract, A, Scaled, Bounds.Precision, Bounds.Budget, Made, State);
      end if;
      Free (Ratio);
      Free (Whole);
      Free (Scaled);
      Failure := Failure_Of (State);
      if Failure /= No_Failure then
         Free (Made);
      end if;
   end Ball_Modulo;

   --  round (x, n) = round (x 10 ** n) / 10 ** n for a ball x, exact n.
   procedure Ball_Round_To
     (A, B : Ball; Bounds : Limits; Made : in out Ball; Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (A, Bounds.Budget) and then Within (B, Bounds.Budget),
          Post => (if Failure = No_Failure then Within (Made, Bounds.Budget) else Is_Reclaimed (Made))
   is
      Ten, Scale, Scaled, Whole : Ball;
      Ten_Value : Number;
      State : Calc_Balls.Status := Calc_Balls.Done;
   begin
      if not Is_Integer (B) then
         Failure := Not_An_Integer;
         return;
      end if;
      N.From_Integer (10, Ten_Value);
      From_Exact (Ten_Value, Ten);
      if not Within (Ten, Bounds.Budget) then
         State := Calc_Balls.Too_Large;
      end if;
      if State = Calc_Balls.Done then
         Apply (Calc_Balls.Power, Ten, B, Bounds.Precision, Bounds.Budget, Scale, State);
      end if;
      if State = Calc_Balls.Done then
         Apply (Calc_Balls.Multiply, A, Scale, Bounds.Precision, Bounds.Budget, Scaled, State);
      end if;
      if State = Calc_Balls.Done then
         Round_To_Integer (Calc_Balls.Round, Scaled, Bounds.Budget, Whole, State);
      end if;
      if State = Calc_Balls.Done then
         Apply (Calc_Balls.Divide, Whole, Scale, Bounds.Precision, Bounds.Budget, Made, State);
      end if;
      Free (Ten);
      Free (Scale);
      Free (Scaled);
      Free (Whole);
      N.Free (Ten_Value);
      Failure := Failure_Of (State);
      if Failure /= No_Failure then
         Free (Made);
      end if;
   end Ball_Round_To;

   function Elementary_Of (Operation : Unary_Operation) return Calc_Balls.Elementary is
     (case Operation is
         when Sine => Calc_Balls.Sine, when Cosine => Calc_Balls.Cosine, when Tangent => Calc_Balls.Tangent,
         when Arcsine => Calc_Balls.Arcsine, when Arccosine => Calc_Balls.Arccosine,
         when Arctangent => Calc_Balls.Arctangent, when Natural_Log => Calc_Balls.Natural_Log,
         when Common_Log => Calc_Balls.Common_Log, when Exponential => Calc_Balls.Exponential,
         when others => Calc_Balls.Square_Root);

   procedure Apply_Unary
     (Operation : Unary_Operation; X : Ball; Bounds : Limits; Mode : Options; Made : in out Ball;
      Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (X, Bounds.Budget),
          Post => (if Failure = No_Failure then Within (Made, Bounds.Budget) else Is_Reclaimed (Made))
   is
      State : Calc_Balls.Status := Calc_Balls.Done;
   begin
      Failure := No_Failure;
      case Operation is
         when Negate =>
            Negate (X, Made);
         when Absolute =>
            Absolute (X, Bounds.Precision, Bounds.Budget, Made, State);
         when Floor | Ceiling | Round | Truncate =>
            Round_To_Integer
              ((case Operation is
                   when Floor => Calc_Balls.Floor, when Ceiling => Calc_Balls.Ceiling,
                   when Round => Calc_Balls.Round, when others => Calc_Balls.Truncate),
               X, Bounds.Budget, Made, State);
         when Factorial => Exact_Unary (N.Factorial, X, Bounds.Budget, Made, Failure);
         when Numerator_Of => Exact_Unary (N.Numerator_Of, X, Bounds.Budget, Made, Failure);
         when Denominator_Of => Exact_Unary (N.Denominator_Of, X, Bounds.Budget, Made, Failure);
         when Bit_Not => Exact_Unary (N.Bit_Not, X, Bounds.Budget, Made, Failure);
         when Square_Root | Sine | Cosine | Tangent | Arcsine | Arccosine | Arctangent | Natural_Log
            | Common_Log | Exponential =>
            Calc_Balls.Evaluate (Elementary_Of (Operation), X, Mode.Angle, Bounds.Precision, Bounds.Budget, Made,
                                 State);
      end case;
      if Failure = No_Failure then
         Failure := Failure_Of (State);
      end if;
      if Failure /= No_Failure then
         Free (Made);
      elsif not Within (Made, Bounds.Budget) then
         Free (Made);  --  never: negation keeps the size
         Failure := Too_Large;
      end if;
   end Apply_Unary;

   procedure Apply_Binary
     (Operation : Binary_Operation; A, B : Ball; Bounds : Limits; Mode : Options; Made : in out Ball;
      Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then Within (A, Bounds.Budget) and then Within (B, Bounds.Budget),
          Post => (if Failure = No_Failure then Within (Made, Bounds.Budget) else Is_Reclaimed (Made))
   is
      State : Calc_Balls.Status := Calc_Balls.Done;
      In_Integers : constant Boolean := Mode.Domain = Integers;
   begin
      Failure := No_Failure;
      case Operation is
         when Add => Apply (Calc_Balls.Add, A, B, Bounds.Precision, Bounds.Budget, Made, State);
         when Subtract => Apply (Calc_Balls.Subtract, A, B, Bounds.Precision, Bounds.Budget, Made, State);
         when Multiply => Apply (Calc_Balls.Multiply, A, B, Bounds.Precision, Bounds.Budget, Made, State);
         when Power => Apply (Calc_Balls.Power, A, B, Bounds.Precision, Bounds.Budget, Made, State);
         when Divide =>
            if In_Integers then
               Exact_Binary (N.Integer_Quotient, A, B, Bounds.Budget, Made, Failure);
            else
               Apply (Calc_Balls.Divide, A, B, Bounds.Precision, Bounds.Budget, Made, State);
            end if;
         when Modulo =>
            if Is_Exact (A) and then Is_Exact (B) then
               Exact_Binary (N.Modulo, A, B, Bounds.Budget, Made, Failure);
            else
               Ball_Modulo (A, B, Bounds, Made, Failure);
            end if;
         when Round_To =>
            if Is_Exact (A) and then Is_Exact (B) then
               Exact_Binary (N.Round_To, A, B, Bounds.Budget, Made, Failure);
            else
               Ball_Round_To (A, B, Bounds, Made, Failure);
            end if;
         when Greatest_Common_Divisor => Exact_Binary (N.Greatest_Common_Divisor, A, B, Bounds.Budget, Made, Failure);
         when Least_Common_Multiple => Exact_Binary (N.Least_Common_Multiple, A, B, Bounds.Budget, Made, Failure);
         when Bit_And => Exact_Binary (N.Bit_And, A, B, Bounds.Budget, Made, Failure);
         when Bit_Or => Exact_Binary (N.Bit_Or, A, B, Bounds.Budget, Made, Failure);
         when Bit_Xor => Exact_Binary (N.Bit_Xor, A, B, Bounds.Budget, Made, Failure);
         when Shift_Left => Exact_Binary (N.Shift_Left, A, B, Bounds.Budget, Made, Failure);
         when Shift_Right => Exact_Binary (N.Shift_Right, A, B, Bounds.Budget, Made, Failure);
         when Root => Calc_Balls.Root (A, B, Bounds.Precision, Bounds.Budget, Made, State);
         when Rotate_Left | Rotate_Right =>
            if Mode.Word = Calc_Words.Unbounded then
               Failure := Needs_Word_Size;
            elsif not Is_Exact (A) or else not Is_Exact (B) then
               Failure := Not_An_Integer;
            else
               declare
                  Left, Right, Value : Number;
                  Outcome : N.Status;
               begin
                  Midpoint (A, Left);
                  Midpoint (B, Right);
                  Calc_Words.Rotate (Left, Right, Operation = Rotate_Left, Mode.Word, Mode.Signed, Bounds.Budget,
                                     Value, Outcome);
                  N.Free (Left);
                  N.Free (Right);
                  if Outcome = N.Done then
                     From_Exact (Value, Made);
                  else
                     Failure := (if Outcome = N.Too_Large then Too_Large else Not_An_Integer);
                  end if;
                  N.Free (Value);
               end;
            end if;
      end case;
      if Failure = No_Failure then
         Failure := Failure_Of (State);
      end if;
      if Failure /= No_Failure then
         Free (Made);
      end if;
   end Apply_Binary;

   --  Programmer mode: every value an integer in the word.
   procedure Into_Word
     (Made : in out Ball; Bounds : Limits; Mode : Options; Wrapped : in out Boolean; Failure : out Failure_Kind)
     with Pre => Within (Made, Bounds.Budget),
          Post => (if Failure = No_Failure then Within (Made, Bounds.Budget) else Is_Reclaimed (Made))
   is
      Value, Kept : Number;
      Changed : Boolean;
      Outcome : N.Status;
   begin
      Failure := No_Failure;
      if Mode.Domain /= Integers then
         return;
      end if;
      if not Is_Integer (Made) then
         Free (Made);
         Failure := Not_An_Integer;
         return;
      end if;
      Midpoint (Made, Value);
      Calc_Words.Wrap (Value, Mode.Word, Mode.Signed, Bounds.Budget, Kept, Changed, Outcome);
      N.Free (Value);
      Free (Made);
      if Outcome = N.Done then
         From_Exact (Kept, Made);
         Wrapped := Wrapped or else Changed;
      else
         Failure := Too_Large;
      end if;
      N.Free (Kept);
   end Into_Word;

   procedure Literal_Value
     (Text : String; T : Term; Budget : N.Size_Budget; Made : in out Ball; Failure : out Failure_Kind)
     with Pre => Is_Reclaimed (Made) and then T.Kind = Literal and then Text'Length <= MAXIMUM_TEXT
                 and then Valid_Terms (Text, [1 => T]),
          Post => (if Failure = No_Failure then Within (Made, Budget) else Is_Reclaimed (Made))
   is
      Value : Number;
      State : N.Status := N.Done;
      Digits_Value, One : Calc_Naturals.Magnitude;
   begin
      if T.Base = DECIMAL then
         N.From_Decimal (Text (T.Whole_First .. T.Whole_Last), Text (T.Fraction_First .. T.Fraction_Last),
                         T.Exponent, Budget, Value, State);
      else
         Calc_Naturals.From_Digits (Text (T.Whole_First .. T.Whole_Last), T.Base, Digits_Value);
         Calc_Naturals.Make (One, 1);
         if Calc_Naturals.Length (Digits_Value) < Calc_Limbs.MAXIMUM_LIMBS then
            N.From_Parts (False, Digits_Value, One, Budget, Value, State);
         else
            State := N.Too_Large;
         end if;
         Calc_Naturals.Free (Digits_Value);
         Calc_Naturals.Free (One);
      end if;
      Failure := (if State = N.Done then No_Failure else Too_Large);
      if Failure = No_Failure then
         From_Exact (Value, Made);
      end if;
      N.Free (Value);
   end Literal_Value;

   procedure Evaluate_Terms
     (Text : String; Terms : Term_Array; Previous : Ball; Has_Previous : Boolean; Bounds : Limits;
      Mode : Options; Result : in out Ball; Wrapped : out Boolean; Failure : out Failure_Kind;
      Position : out Column)
   is
      --  The operands so far, in postfix order: Values (1 .. Top).
      Values : Ball_Array (1 .. Terms'Length);
      Top : Natural := 0;
      Made : Ball;
      State : Calc_Balls.Status;

      --  Free every value still held.
      procedure Release
        with Global => (In_Out => (Values, Top), Proof_In => Terms),
             Pre => Top <= Values'Last
                    and then (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K)))),
             Post => (for all K in Values'Range => Is_Reclaimed (Values (K)))
      is
      begin
         while Top > 0 loop
            pragma Loop_Invariant (Top <= Values'Last);
            pragma Loop_Invariant (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K))));
            pragma Loop_Variant (Decreases => Top);
            Free (Values (Top));
            Top := Top - 1;
         end loop;
      end Release;
   begin
      Failure := No_Failure;
      Wrapped := False;
      Position := 1;
      for T of Terms loop
         pragma Loop_Invariant (Top <= Values'Last and then Failure = No_Failure and then Is_Reclaimed (Made));
         pragma Loop_Invariant (for all K in 1 .. Top => Within (Values (K), Bounds.Budget));
         pragma Loop_Invariant (for all K in Values'Range => (if K > Top then Is_Reclaimed (Values (K))));
         Position := T.Position;
         State := Calc_Balls.Done;
         case T.Kind is
            when Literal =>
               Literal_Value (Text, T, Bounds.Budget, Made, Failure);
            when Previous_Result =>
               if not Has_Previous then
                  Failure := No_Previous_Result;
               elsif not Within (Previous, Bounds.Budget) then
                  Failure := Too_Large;
               else
                  Copy (Previous, Made);
               end if;
            when Named_Constant =>
               Make_Constant ((if T.Value = Calc_Syntax.Pi then Calc_Balls.Pi else Calc_Balls.Euler),
                              Bounds.Precision, Bounds.Budget, Made, State);
               Failure := Failure_Of (State);
            when Unary =>
               if Top = 0 or else T.Operator not in Unary_Operation then
                  Failure := Expected_Operand;  --  never: Parse orders the terms
               else
                  Apply_Unary (T.Operator, Values (Top), Bounds, Mode, Made, Failure);
                  if Failure = No_Failure then
                     Free (Values (Top));
                     Top := Top - 1;
                  end if;
               end if;
            when Binary =>
               if Top < 2 or else T.Operator not in Binary_Operation then
                  Failure := Expected_Operand;  --  never: Parse orders the terms
               else
                  Apply_Binary (T.Operator, Values (Top - 1), Values (Top), Bounds, Mode, Made, Failure);
                  if Failure = No_Failure then
                     Free (Values (Top));
                     Free (Values (Top - 1));
                     Top := Top - 2;
                  end if;
               end if;
         end case;
         if Failure = No_Failure then
            Into_Word (Made, Bounds, Mode, Wrapped, Failure);
         end if;
         if Failure /= No_Failure then
            Free (Made);
            Release;
            return;
         end if;
         if Top = Values'Last then
            --  never: each term pushes one value after taking its operands
            Free (Made);
            Failure := Expected_Operand;
            Release;
            return;
         end if;
         Top := Top + 1;
         Move (Made, Values (Top));
      end loop;
      if Top /= 1 then
         Failure := (if Top = 0 then Empty_Expression else Expected_Operator);  --  never after Parse
         Release;
         return;
      end if;
      Move (Values (1), Result);
   end Evaluate_Terms;

   procedure Evaluate
     (Text : String; Previous : Ball; Has_Previous : Boolean; Bounds : Limits; Mode : Options;
      Result : in out Ball; Wrapped : out Boolean; Failure : out Failure_Kind; Position : out Column)
   is
      Terms : Term_Array (1 .. Text'Length);
      Count : Natural;
      Error : Syntax_Error;
   begin
      Wrapped := False;
      Parse (Text, Terms, Count, Error, Position);
      if Error /= No_Error then
         Failure := Failure_Of (Error);
         return;
      end if;
      Evaluate_Terms (Text, Terms (1 .. Count), Previous, Has_Previous, Bounds, Mode, Result, Wrapped, Failure,
                      Position);
   end Evaluate;
end Calc_Engine;
