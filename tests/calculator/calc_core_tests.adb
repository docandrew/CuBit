------------------------------------------------------------------------------
--  Hosted tests of the calculator core (docs/calculator.md, "Tests").
--
--    calc_core_tests CASES        differential cases from calc_reference.py
--    calc_core_tests --eval TEXT  evaluate one expression (exact and decimal)
--    calc_core_tests --bench      timings of big-number work
--
--  Each case line: expression, fraction, places, decimal, form; or
--  expression, !failure, column. An expression starting with ~ must come
--  out approximate. The checks also cover what the reference cannot: the
--  budget (Too_Large without allocating), ans, the text bounds.
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Calc_Engine; use Calc_Engine;
with Calc_Format; use Calc_Format;
with Calc_Numbers; use Calc_Numbers;
with Calc_Syntax;

procedure Calc_Core_Tests is
   --  The tests' limits: about 39,000 decimal digits per numerator or
   --  denominator, and approximations to 50 digits.
   TEST_BUDGET : constant Size_Budget := 4_096;
   TEST_PRECISION : constant Approximate_Digits := 50;
   Bounds : constant Limits := (Budget => TEST_BUDGET, Precision => TEST_PRECISION);

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

   function Fraction_Of (X : Number) return String is
      Into : String (1 .. Fraction_Bound (X));
      Last : Natural;
   begin
      Fraction_Text (X, Into, Last);
      return Into (1 .. Last);
   end Fraction_Of;

   function Decimal_Of (X : Number; Places : Places_Count; Form : out Decimal_Form) return String is
      Into : String (1 .. Decimal_Bound (X, Places));
      Last : Natural;
   begin
      Decimal_Text (X, Places, Into, Last, Form);
      return Into (1 .. Last);
   end Decimal_Of;

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

   procedure Run_Case (Line : String) is
      Raw : constant String := Field (Line, 1);
      Approximate_Expected : constant Boolean := Raw'Length > 0 and then Raw (Raw'First) = '~';
      Text : constant String := (if Approximate_Expected then Raw (Raw'First + 1 .. Raw'Last) else Raw);
      Expected : constant String := Field (Line, 2);
      Previous, Result : Number;
      Failure : Failure_Kind;
      Position : Calc_Syntax.Column;
      Based : constant String (1 .. Text'Length) := Text;
   begin
      Evaluate (Based, Previous, False, Bounds, Result, Failure, Position);
      if Expected'Length > 0 and then Expected (Expected'First) = '!' then
         Check (Failure'Image = Ada.Strings.Fixed.Translate (Expected (Expected'First + 1 .. Expected'Last),
                                                             Ada.Strings.Maps.Constants.Upper_Case_Map)
                and then Position = Positive'Value (Field (Line, 3)),
                Text & ": expected " & Expected & " at " & Field (Line, 3) & ", got " & Failure'Image
                & " at" & Position'Image);
      elsif Failure /= No_Failure then
         Check (False, Text & ": unexpected " & Failure'Image & " at" & Position'Image & " (" & Message (Failure) & ")");
      else
         declare
            Places : constant Places_Count := Positive'Value (Field (Line, 3));
            Form : Decimal_Form;
            Shown : constant String := Decimal_Of (Result, Places, Form);
         begin
            Check (Fraction_Of (Result) = Expected, Text & ": fraction " & Fraction_Of (Result) & " /= " & Expected);
            Check (Shown = Field (Line, 4), Text & " at" & Places'Image & " places: " & Shown & " /= " & Field (Line, 4));
            Check (Form'Image = Ada.Strings.Fixed.Translate (Field (Line, 5), Ada.Strings.Maps.Constants.Upper_Case_Map),
                   Text & ": form " & Form'Image & " /= " & Field (Line, 5));
            Check ((Kind (Result) = Approximate) = Approximate_Expected, Text & ": exactness " & Kind (Result)'Image);
         end;
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

   --  Evaluate Text with ans = Previous (when given).
   procedure Eval
     (Text : String; Result : in out Number; Failure : out Failure_Kind;
      Previous : Number; Has_Previous : Boolean := False; With_Limits : Limits := Bounds)
   is
      Position : Calc_Syntax.Column;
      Based : constant String (1 .. Text'Length) := Text;
   begin
      Evaluate (Based, Previous, Has_Previous, With_Limits, Result, Failure, Position);
   end Eval;

   procedure Fixed_Checks is
      Nothing, A, B, C : Number;
      Failure : Failure_Kind;
      Form : Decimal_Form;
   begin
      --  ans carries the exact previous result.
      Eval ("1/3", A, Failure, Nothing);
      Eval ("ans * 3", B, Failure, A, Has_Previous => True);
      Check (Failure = No_Failure and then Fraction_Of (B) = "1", "ans * 3 after 1/3");
      Free (B);
      Eval ("ans + ans", B, Failure, A, Has_Previous => True);
      Check (Fraction_Of (B) = "2/3", "ans + ans after 1/3");
      Free (A);
      Free (B);

      --  The budget is enforced before work: a small one refuses what a
      --  larger one computes, and nothing leaks either way.
      Eval ("2^4000", A, Failure, Nothing, With_Limits => (Budget => 64, Precision => 10));
      Check (Failure = Too_Large and then Is_Reclaimed (A), "2^4000 within 64 limbs is Too_Large");
      Eval ("2^2000", A, Failure, Nothing, With_Limits => (Budget => 64, Precision => 10));
      Check (Failure = No_Failure and then Numerator_Bits (A) = 2001, "2^2000 within 64 limbs");
      Free (A);
      Eval ("1000!", A, Failure, Nothing, With_Limits => (Budget => 64, Precision => 10));
      Check (Failure = Too_Large, "1000! within 64 limbs is Too_Large");
      Eval ("1000!", A, Failure, Nothing);
      Check (Failure = No_Failure and then Fraction_Of (A)'Length = 2568, "1000! has 2568 digits");
      Free (A);
      Eval ("sqrt(2)", A, Failure, Nothing, With_Limits => (Budget => 2, Precision => 100));
      Check (Failure = Too_Large, "sqrt(2) to 100 digits within 2 limbs is Too_Large");

      --  Huge but exact: (2^4096 + 1) / 2^4096 - 1 = 2^-4096.
      Eval ("(2^4096 + 1) / 2^4096 - 1", A, Failure, Nothing);
      Eval ("2^-4096", B, Failure, Nothing);
      Check (Failure = No_Failure and then Same (A, B), "(2^4096 + 1) / 2^4096 - 1 = 2^-4096");
      Free (A);
      Free (B);

      --  Big division: (10^500 + 7)^3 / (10^500 + 7) = (10^500 + 7)^2.
      Eval ("(10^500 + 7)^3 / (10^500 + 7)", A, Failure, Nothing);
      Eval ("(10^500 + 7)^2", B, Failure, Nothing);
      Check (Same (A, B), "(10^500+7)^3 / (10^500+7)");
      Free (A);
      Free (B);
      Eval ("(2^200 + 3^90) mod (7^40)", A, Failure, Nothing);
      Check (Fraction_Of (A) = "3327898286383320528859915629208778",
             "big mod: " & Fraction_Of (A));
      Free (A);

      --  Approximations stay marked through exact arithmetic.
      Eval ("sqrt(2) - sqrt(2)", A, Failure, Nothing);
      Check (Is_Zero (A) and then Kind (A) = Approximate, "sqrt(2) - sqrt(2) is an approximate zero");
      Free (A);
      Eval ("sqrt(9/4)", A, Failure, Nothing);
      Check (Kind (A) = Exact and then Fraction_Of (A) = "3/2", "sqrt(9/4) is exactly 3/2");
      Free (A);

      --  Many places: 1/7 to 10,000 places repeats with period 6.
      Eval ("1/7", A, Failure, Nothing);
      Check (Decimal_Of (A, 10_000, Form) = "0.(142857)" and then Form = Repeating, "1/7 at 10000 places");
      Free (A);
      --  A long period: 1/9967 repeats every 9966 digits.
      Eval ("1/9967", A, Failure, Nothing);
      declare
         Shown : constant String := Decimal_Of (A, 10_000, Form);
      begin
         Check (Form = Repeating and then Shown'Length = 2 + 9966 + 2, "1/9967 period 9966:" & Shown'Length'Image);
      end;
      Free (A);

      --  Deep nesting is limited by the text only, not by recursion.
      declare
         Depth : constant := 50_000;
         Deep : constant String := (1 .. Depth => '(') & "1" & (1 .. Depth => ')');
      begin
         Eval (Deep, A, Failure, Nothing);
         Check (Failure = No_Failure and then Fraction_Of (A) = "1", "50,000 nested parentheses");
         Free (A);
      end;
      declare
         Long : Unbounded_String := To_Unbounded_String ("0");
      begin
         for K in 1 .. 20_000 loop
            Append (Long, "+1");
         end loop;
         Eval (To_String (Long), A, Failure, Nothing);
         Check (Failure = No_Failure and then Fraction_Of (A) = "20000", "20,000 terms");
         Free (A);
      end;
      Free (C);
   end Fixed_Checks;

   procedure Evaluate_One (Text : String) is
      Nothing, Result : Number;
      Failure : Failure_Kind;
      Position : Calc_Syntax.Column;
      Form : Decimal_Form;
      Based : constant String (1 .. Text'Length) := Text;
   begin
      Evaluate (Based, Nothing, False, Bounds, Result, Failure, Position);
      if Failure /= No_Failure then
         Put_Line ("error at column" & Position'Image & ": " & Message (Failure));
      else
         Put_Line ((if Kind (Result) = Approximate then "~ " else "= ") & Fraction_Of (Result));
         declare
            Shown : constant String := Decimal_Of (Result, 30, Form);
         begin
            Put_Line ("  " & Shown & "   (" & Form'Image & ")");
         end;
      end if;
      Free (Result);
   end Evaluate_One;

   procedure Bench is
      procedure Measure (Text : String; Repeat : Positive) is
         Nothing, Result : Number;
         Failure : Failure_Kind;
         Position : Calc_Syntax.Column;
         Based : constant String (1 .. Text'Length) := Text;
         Start : constant Ada.Real_Time.Time := Clock;
      begin
         for K in 1 .. Repeat loop
            Evaluate (Based, Nothing, False, (Budget => 65_536, Precision => 1_000), Result, Failure, Position);
            Free (Result);
         end loop;
         Put_Line (Text & ":" & Duration'Image (To_Duration (Clock - Start) / Repeat * 1_000_000) & " us");
      end Measure;
   begin
      Measure ("2/3 + 1/6", 100_000);
      Measure ("(1/3 + 1/7)^20 / 11", 10_000);
      Measure ("1000!", 20);
      Measure ("2^100000 mod 3^5000", 10);
      Measure ("sqrt(2)", 50);
      Measure ("pi", 10);
      Measure ("e", 10);
   end Bench;
begin
   if Ada.Command_Line.Argument_Count >= 2 and then Ada.Command_Line.Argument (1) = "--eval" then
      Evaluate_One (Ada.Command_Line.Argument (2));
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
