------------------------------------------------------------------------------
--  Hosted tests of the calculator core (docs/calculator.md, "Tests").
--
--    calc_core_tests CASES             differential cases from calc_reference.py
--    calc_core_tests --eval TEXT [MODE] evaluate one expression
--    calc_core_tests --bench           timings of big-number and function work
--
--  Case lines (calc_reference.py): E exact results with their display, F
--  typed failures with their column, A approximations checked against
--  mpmath (the ball contains the reference; the shown digits are within
--  the stated bound), P programmer-mode words. Fixed checks cover what the
--  reference cannot: budgets, ans, deep nesting, long periods.
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Calc_Balls; use Calc_Balls;
with Calc_Engine; use Calc_Engine;
with Calc_Format; use Calc_Format;
with Calc_Limbs;
with Calc_Numbers;
with Calc_Syntax;
with Calc_Words;

procedure Calc_Core_Tests is
   use type Calc_Limbs.Ordering;
   use type Calc_Numbers.Status;
   use type Calc_Words.Word_Size;
   --  The tests' limits: about 39,000 decimal digits per numerator or
   --  denominator, and approximations to 50 digits.
   TEST_BUDGET : constant Calc_Numbers.Size_Budget := 4_096;
   TEST_PRECISION : constant Precision_Digits := 50;
   Bounds : constant Limits := (Budget => TEST_BUDGET, Precision => TEST_PRECISION);
   STANDARD_MODE : constant Options :=
     (Domain => Rationals, Word => Calc_Words.Unbounded, Signed => True, Angle => Radians);
   --  An approximation of 50 digits has a radius under this share of its
   --  size (10 ** -40): looser means a bound that is honest but useless.
   QUALITY_DIGITS : constant := 40;
   --  mpmath's reference is good to this many digits.
   REFERENCE_DIGITS : constant := 120;

   Passed, Failed : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Passed := Passed + 1;
      else
         Failed := Failed + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Upper (Text : String) return String is
     (Ada.Strings.Fixed.Translate (Text, Ada.Strings.Maps.Constants.Upper_Case_Map));

   function Field (Line : String; Index : Positive) return String is
      Start : Natural := Line'First;
      Seen : Natural := 1;
   begin
      for I in Line'Range loop
         if Line (I) = ASCII.HT then
            if Seen = Index then
               return Line (Start .. I - 1);
            end if;
            Seen := Seen + 1;
            Start := I + 1;
         end if;
      end loop;
      return (if Seen = Index then Line (Start .. Line'Last) else "");
   end Field;

   --  "std:degrees", "prog:8:signed", "prog:unbounded:unsigned".
   function Mode_Of (Text : String) return Options is
      Result : Options := STANDARD_MODE;
   begin
      if Text'Length >= 4 and then Text (Text'First .. Text'First + 3) = "prog" then
         Result.Domain := Integers;
         Result.Signed := Ada.Strings.Fixed.Index (Text, ":signed") > 0;
         Result.Word :=
           (if Ada.Strings.Fixed.Index (Text, ":8:") > 0 then Calc_Words.Bits_8
            elsif Ada.Strings.Fixed.Index (Text, ":16:") > 0 then Calc_Words.Bits_16
            elsif Ada.Strings.Fixed.Index (Text, ":32:") > 0 then Calc_Words.Bits_32
            elsif Ada.Strings.Fixed.Index (Text, ":64:") > 0 then Calc_Words.Bits_64
            else Calc_Words.Unbounded);
      else
         Result.Angle :=
           (if Ada.Strings.Fixed.Index (Text, "degrees") > 0 then Degrees
            elsif Ada.Strings.Fixed.Index (Text, "gradians") > 0 then Gradians else Radians);
      end if;
      return Result;
   end Mode_Of;

   procedure Eval
     (Text : String; Mode : Options; Result : in out Ball; Wrapped : out Boolean; Failure : out Failure_Kind;
      Position : out Calc_Syntax.Column; Previous : Ball; Has_Previous : Boolean := False;
      With_Limits : Limits := Bounds)
   is
      Based : constant String (1 .. Text'Length) := Text;
   begin
      Evaluate (Based, Previous, Has_Previous, With_Limits, Mode, Result, Wrapped, Failure, Position);
   end Eval;

   function Fraction_Of (X : Ball) return String is
      Mid : Calc_Numbers.Number;
   begin
      Midpoint (X, Mid);
      declare
         Into : String (1 .. Fraction_Bound (Mid));
         Last : Natural;
      begin
         Fraction_Text (Mid, Into, Last);
         Calc_Numbers.Free (Mid);
         return Into (1 .. Last);
      end;
   end Fraction_Of;

   procedure Show (X : Ball; Places : Places_Count; Text, Bound : out Unbounded_String; Form : out Decimal_Form) is
      Into : String (1 .. Ball_Bound (X, Places));
      Bound_Into : String (1 .. BOUND_TEXT);
      Last, Bound_Last : Natural;
   begin
      Ball_Text (X, Places, Into, Last, Bound_Into, Bound_Last, Form);
      Text := To_Unbounded_String (Into (1 .. Last));
      Bound := To_Unbounded_String (Bound_Into (1 .. Bound_Last));
   end Show;

   function Based_Of (X : Ball; Base : Display_Base; Word : Calc_Words.Word_Size) return String is
      Mid : Calc_Numbers.Number;
   begin
      Midpoint (X, Mid);
      declare
         Into : String (1 .. Base_Bound (Mid));
         Last : Natural;
      begin
         Base_Text (Mid, Base, Word, Into, Last);
         Calc_Numbers.Free (Mid);
         return Into (1 .. Last);
      end;
   end Based_Of;

   --  A decimal string (mpmath's) as an exact rational.
   procedure Exact_Of (Text : String; Result : in out Calc_Numbers.Number) is
      Negative : constant Boolean := Text'Length > 0 and then Text (Text'First) = '-';
      Body_Text : constant String := (if Negative then Text (Text'First + 1 .. Text'Last) else Text);
      E_At : constant Natural := Ada.Strings.Fixed.Index (Body_Text, "e");
      Mantissa : constant String := (if E_At = 0 then Body_Text else Body_Text (Body_Text'First .. E_At - 1));
      Exponent : constant Calc_Numbers.Decimal_Exponent :=
        (if E_At = 0 then 0 else Calc_Numbers.Decimal_Exponent'Value (Body_Text (E_At + 1 .. Body_Text'Last)));
      Dot : constant Natural := Ada.Strings.Fixed.Index (Mantissa, ".");
      Whole : constant String := (if Dot = 0 then Mantissa else Mantissa (Mantissa'First .. Dot - 1));
      Fraction : constant String := (if Dot = 0 then "" else Mantissa (Dot + 1 .. Mantissa'Last));
      Value : Calc_Numbers.Number;
      State : Calc_Numbers.Status;
   begin
      Calc_Numbers.From_Decimal (Whole, Fraction, Exponent, Calc_Numbers.Size_Budget'Last, Value, State);
      if Negative then
         Calc_Numbers.Apply (Calc_Numbers.Negate, Value, Calc_Numbers.Size_Budget'Last, Result, State);
         Calc_Numbers.Free (Value);
      else
         Calc_Numbers.Move (Value, Result);
      end if;
   end Exact_Of;

   --  |A - B| <= Bound, all exact.
   function Close (A, B, Bound : Calc_Numbers.Number) return Boolean is
      Difference, Size_Of : Calc_Numbers.Number;
      State : Calc_Numbers.Status;
      Order : Calc_Limbs.Ordering;
   begin
      Calc_Numbers.Apply (Calc_Numbers.Subtract, A, B, Calc_Numbers.Size_Budget'Last, Difference, State);
      Calc_Numbers.Apply (Calc_Numbers.Absolute, Difference, Calc_Numbers.Size_Budget'Last, Size_Of, State);
      Calc_Numbers.Compare (Size_Of, Bound, Order);
      Calc_Numbers.Free (Difference);
      Calc_Numbers.Free (Size_Of);
      return Order /= Calc_Limbs.Greater;
   end Close;

   procedure Run_Case (Line : String) is
      Kind : constant String := Field (Line, 1);
      Mode : constant Options := Mode_Of (Field (Line, 2));
      Text : constant String := Field (Line, 3);
      Previous, Result : Ball;
      Wrapped : Boolean;
      Failure : Failure_Kind;
      Position : Calc_Syntax.Column;
   begin
      Eval (Text, Mode, Result, Wrapped, Failure, Position, Previous);
      if Kind = "F" then
         Check (Failure'Image = Upper (Field (Line, 4)) and then Position = Positive'Value (Field (Line, 5)),
                Text & " (" & Field (Line, 2) & "): expected " & Field (Line, 4) & " at " & Field (Line, 5)
                & ", got " & Failure'Image & " at" & Position'Image);
      elsif Failure /= No_Failure then
         Check (False, Text & " (" & Field (Line, 2) & "): unexpected " & Failure'Image & " at" & Position'Image
                & " (" & Message (Failure) & ")");
      elsif Kind = "E" then
         declare
            Places : constant Places_Count := Positive'Value (Field (Line, 5));
            Shown, Bound : Unbounded_String;
            Form : Decimal_Form;
         begin
            Show (Result, Places, Shown, Bound, Form);
            Check (Is_Exact (Result), Text & ": not exact");
            Check (Fraction_Of (Result) = Field (Line, 4), Text & ": fraction " & Fraction_Of (Result) & " /= "
                   & Field (Line, 4));
            Check (To_String (Shown) = Field (Line, 6), Text & " at" & Places'Image & " places: " & To_String (Shown)
                   & " /= " & Field (Line, 6));
            Check (Upper (Form'Image) = Upper (Field (Line, 7)), Text & ": form " & Form'Image & " /= " & Field (Line, 7));
         end;
      elsif Kind = "A" then
         declare
            Places : constant Places_Count := Positive'Value (Field (Line, 5));
            Reference, Mid, Rad, Shown_Value, Bound_Value, Quality, Abs_Reference, One, Slack_Kept,
              Bound_Total : Calc_Numbers.Number;
            Shown, Bound : Unbounded_String;
            Form : Decimal_Form;
            State : Calc_Numbers.Status;
            Order : Calc_Limbs.Ordering;
         begin
            Exact_Of (Field (Line, 4), Reference);
            Midpoint (Result, Mid);
            --  The reference has 140 significant digits: allow for its own
            --  rounding (10 ** -120 of max (1, |x|)).
            declare
               Own, Slack, Ten, Exponent_Number, Abs_Ref, Unit, Larger : Calc_Numbers.Number;
               Order_Ref : Calc_Limbs.Ordering;
            begin
               Radius (Result, Own);
               Calc_Numbers.From_Integer (10, Ten);
               Calc_Numbers.From_Integer (-REFERENCE_DIGITS, Exponent_Number);
               Calc_Numbers.Apply (Calc_Numbers.Absolute, Reference, Calc_Numbers.Size_Budget'Last, Abs_Ref, State);
               Calc_Numbers.From_Integer (1, Unit);
               Calc_Numbers.Compare (Abs_Ref, Unit, Order_Ref);
               Calc_Numbers.Apply (Calc_Numbers.Power, Ten, Exponent_Number, Calc_Numbers.Size_Budget'Last, Larger, State);
               Calc_Numbers.Apply (Calc_Numbers.Multiply, Larger,
                                   (if Order_Ref = Calc_Limbs.Greater then Abs_Ref else Unit),
                                   Calc_Numbers.Size_Budget'Last, Slack, State);
               Calc_Numbers.Apply (Calc_Numbers.Add, Own, Slack, Calc_Numbers.Size_Budget'Last, Rad, State);
               Calc_Numbers.Copy (Slack, Slack_Kept);
               Calc_Numbers.Free (Own);
               Calc_Numbers.Free (Slack);
               Calc_Numbers.Free (Ten);
               Calc_Numbers.Free (Exponent_Number);
               Calc_Numbers.Free (Abs_Ref);
               Calc_Numbers.Free (Unit);
               Calc_Numbers.Free (Larger);
            end;
            --  The ball contains the reference.
            Check (Close (Mid, Reference, Rad), Text & " (" & Field (Line, 2) & "): the ball misses " & Field (Line, 4) & " (mid " & Fraction_Of (Result)'Length'Image
                   & " chars)");
            --  The shown digits are within the stated bound of it.
            Show (Result, Places, Shown, Bound, Form);
            if Length (Bound) > 0 then
               Exact_Of (To_String (Shown), Shown_Value);
               Exact_Of (To_String (Bound), Bound_Value);
               Calc_Numbers.Apply (Calc_Numbers.Add, Bound_Value, Slack_Kept, Calc_Numbers.Size_Budget'Last,
                                   Bound_Total, State);
               Check (Close (Shown_Value, Reference, Bound_Total),
                      Text & ": shown " & To_String (Shown) & " is not within " & To_String (Bound) & " of the reference");
               Calc_Numbers.Free (Shown_Value);
               Calc_Numbers.Free (Bound_Value);
            else
               --  Exact (sin^2 + cos^2 at an exact angle can be exact).
               Check (Is_Exact (Result), Text & ": no bound for an approximation");
            end if;
            --  And the bound is tight: under 10 ** -40 of max (1, |x|).
            Calc_Numbers.Apply (Calc_Numbers.Absolute, Reference, Calc_Numbers.Size_Budget'Last, Abs_Reference, State);
            Calc_Numbers.From_Integer (1, One);
            Calc_Numbers.Compare (Abs_Reference, One, Order);
            declare
               Scale_Value, Ten, Exponent_Number, Allowed : Calc_Numbers.Number;
            begin
               Calc_Numbers.From_Integer (10, Ten);
               Calc_Numbers.From_Integer (-QUALITY_DIGITS, Exponent_Number);
               Calc_Numbers.Apply (Calc_Numbers.Power, Ten, Exponent_Number, Calc_Numbers.Size_Budget'Last,
                                   Scale_Value, State);
               Calc_Numbers.Apply (Calc_Numbers.Multiply, Scale_Value,
                                   (if Order = Calc_Limbs.Greater then Abs_Reference else One),
                                   Calc_Numbers.Size_Budget'Last, Allowed, State);
               Calc_Numbers.Compare (Rad, Allowed, Order);
               Check (Order /= Calc_Limbs.Greater, Text & " (" & Field (Line, 2) & "): bound too loose: "
                      & To_String (Bound));
               Calc_Numbers.Free (Scale_Value);
               Calc_Numbers.Free (Ten);
               Calc_Numbers.Free (Exponent_Number);
               Calc_Numbers.Free (Allowed);
            end;
            Calc_Numbers.Free (Slack_Kept);
            Calc_Numbers.Free (Bound_Total);
            Calc_Numbers.Free (Reference);
            Calc_Numbers.Free (Mid);
            Calc_Numbers.Free (Rad);
            Calc_Numbers.Free (Quality);
            Calc_Numbers.Free (Abs_Reference);
            Calc_Numbers.Free (One);
         end;
      elsif Kind = "P" then
         Check (Fraction_Of (Result) = Field (Line, 4), Text & " (" & Field (Line, 2) & "): " & Fraction_Of (Result)
                & " /= " & Field (Line, 4));
         Check (Based_Of (Result, 16, Mode.Word) = Field (Line, 5), Text & ": hex " & Based_Of (Result, 16, Mode.Word)
                & " /= " & Field (Line, 5));
         Check (Based_Of (Result, 8, Mode.Word) = Field (Line, 6), Text & ": octal " & Based_Of (Result, 8, Mode.Word)
                & " /= " & Field (Line, 6));
         Check (Based_Of (Result, 2, Mode.Word) = Field (Line, 7), Text & ": binary " & Based_Of (Result, 2, Mode.Word)
                & " /= " & Field (Line, 7));
         Check (Wrapped = (Field (Line, 8) = "wrapped"), Text & " (" & Field (Line, 2) & "): wrapped " & Wrapped'Image);
      end if;
      Free (Result);
   end Run_Case;

   procedure Run_File (Name : String) is
      File : File_Type;
   begin
      Open (File, In_File, Name);
      while not End_Of_File (File) loop
         Run_Case (Get_Line (File));
      end loop;
      Close (File);
   end Run_File;

   procedure Fixed_Checks is
      Nothing, A, B : Ball;
      Failure : Failure_Kind;
      Wrapped : Boolean;
      Position : Calc_Syntax.Column;
      Shown, Bound : Unbounded_String;
      Form : Decimal_Form;
   begin
      --  ans carries the exact previous result.
      Eval ("1/3", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
      Eval ("ans * 3", STANDARD_MODE, B, Wrapped, Failure, Position, A, Has_Previous => True);
      Check (Failure = No_Failure and then Fraction_Of (B) = "1", "ans * 3 after 1/3");
      Free (B);
      Free (A);

      --  The budget is enforced before work.
      Eval ("2^4000", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing,
            With_Limits => (Budget => 64, Precision => 10));
      Check (Failure = Too_Large and then Is_Reclaimed (A), "2^4000 within 64 limbs is Too_Large");
      Eval ("2^2000", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing,
            With_Limits => (Budget => 64, Precision => 10));
      Check (Failure = No_Failure, "2^2000 within 64 limbs");
      Free (A);
      Eval ("1000!", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
      Check (Failure = No_Failure and then Fraction_Of (A)'Length = 2568, "1000! has 2568 digits");
      Free (A);

      --  An approximation states its bound; pi to 50 digits.
      Eval ("pi", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
      Show (A, 20, Shown, Bound, Form);
      Check (To_String (Shown) = "3.1415926535897932385" and then Form = Approximate_Decimal,
             "pi at 20 digits: " & To_String (Shown) & " +- " & To_String (Bound));
      Free (A);
      Eval ("sqrt(2) - sqrt(2)", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
      Check (Failure = No_Failure and then not Is_Exact (A), "sqrt(2) - sqrt(2) is a ball around zero");
      Free (A);

      --  1/9967 repeats every 9966 digits.
      Eval ("1/9967", STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
      Show (A, 10_000, Shown, Bound, Form);
      Check (Form = Repeating and then Length (Shown) = 2 + 9966 + 2, "1/9967 period 9966");
      Free (A);

      --  Deep nesting is limited by the text only.
      declare
         Depth : constant := 50_000;
         Deep : constant String := (1 .. Depth => '(') & "1" & (1 .. Depth => ')');
      begin
         Eval (Deep, STANDARD_MODE, A, Wrapped, Failure, Position, Nothing);
         Check (Failure = No_Failure and then Fraction_Of (A) = "1", "50,000 nested parentheses");
         Free (A);
      end;
   end Fixed_Checks;

   procedure Evaluate_One (Text : String; Mode : Options) is
      Nothing, Result : Ball;
      Failure : Failure_Kind;
      Wrapped : Boolean;
      Position : Calc_Syntax.Column;
      Shown, Bound : Unbounded_String;
      Form : Decimal_Form;
   begin
      Eval (Text, Mode, Result, Wrapped, Failure, Position, Nothing);
      if Failure /= No_Failure then
         Put_Line ("error at column" & Position'Image & ": " & Message (Failure));
      else
         Show (Result, 30, Shown, Bound, Form);
         if Is_Exact (Result) then
            Put_Line ("= " & Fraction_Of (Result));
            Put_Line ("  " & To_String (Shown) & "   (" & Form'Image & ")");
         else
            Put_Line ("~ " & To_String (Shown) & "   +- " & To_String (Bound) & "   (" & Form'Image & ")");
         end if;
         if Mode.Domain = Integers then
            Put_Line ("  hex " & Based_Of (Result, 16, Mode.Word) & "  oct " & Based_Of (Result, 8, Mode.Word)
                      & "  bin " & Based_Of (Result, 2, Mode.Word) & (if Wrapped then "  (wrapped)" else ""));
         end if;
      end if;
      Free (Result);
   end Evaluate_One;

   procedure Bench is
      procedure Measure (Text : String; Repeat : Positive; Mode : Options := STANDARD_MODE) is
         Nothing, Result : Ball;
         Failure : Failure_Kind;
         Wrapped : Boolean;
         Position : Calc_Syntax.Column;
         Based : constant String (1 .. Text'Length) := Text;
         Start : constant Ada.Real_Time.Time := Clock;
      begin
         for K in 1 .. Repeat loop
            Evaluate (Based, Nothing, False, (Budget => 65_536, Precision => TEST_PRECISION), Mode, Result,
                      Wrapped, Failure, Position);
            Free (Result);
         end loop;
         Put_Line (Text & ":" & Duration'Image (To_Duration (Clock - Start) / Repeat * 1_000_000) & " us");
      end Measure;
   begin
      Measure ("2/3 + 1/6", 100_000);
      Measure ("(1/3 + 1/7)^20 / 11", 10_000);
      Measure ("1000!", 20);
      Measure ("2^100000 mod 3^5000", 10);
      Measure ("sqrt(2)", 200);
      Measure ("pi", 100);
      Measure ("e", 100);
      Measure ("sin(1)", 100);
      Measure ("ln(2)", 100);
      Measure ("atan(1/3)", 100);
      Measure ("0xDEADBEEF xor 0xFFFF << 4", 10_000,
               (Domain => Integers, Word => Calc_Words.Bits_32, Signed => False, Angle => Radians));
   end Bench;
begin
   if Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (1) = "--eval" then
      Evaluate_One (Ada.Command_Line.Argument (2),
                    (if Ada.Command_Line.Argument_Count >= 3 then Mode_Of (Ada.Command_Line.Argument (3))
                     else STANDARD_MODE));
      return;
   elsif Ada.Command_Line.Argument_Count >= 1 and then Ada.Command_Line.Argument (1) = "--bench" then
      Bench;
      return;
   end if;
   Fixed_Checks;
   if Ada.Command_Line.Argument_Count >= 1 then
      Run_File (Ada.Command_Line.Argument (1));
   end if;
   Put_Line ("calculator core:" & Passed'Image & " checks passed," & Failed'Image & " failed");
   if Failed > 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
exception
   when E : others =>
      Put_Line ("FAIL: exception " & Ada.Exceptions.Exception_Information (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Calc_Core_Tests;
