with Interfaces; use Interfaces;
package body Calc_Syntax with SPARK_Mode is
   subtype Decimal_Exponent is Calc_Numbers.Decimal_Exponent;
   MAXIMUM_EXPONENT : constant := Calc_Numbers.MAXIMUM_EXPONENT;

   --  The functions, by name.
   type Callable is
     (Call_Abs, Call_Floor, Call_Ceil, Call_Round, Call_Trunc, Call_Sqrt, Call_Num, Call_Den, Call_Sin,
      Call_Cos, Call_Tan, Call_Asin, Call_Acos, Call_Atan, Call_Ln, Call_Log, Call_Exp, Call_Gcd, Call_Lcm,
      Call_Root, Call_Rotl, Call_Rotr);
   MAXIMUM_ARGUMENTS : constant := 2;
   subtype Argument_Count is Positive range 1 .. MAXIMUM_ARGUMENTS;
   --  Which argument counts a function takes.
   function Takes (Item : Callable; Count : Argument_Count) return Boolean is
     (case Item is
         when Call_Round => True,
         when Call_Gcd | Call_Lcm | Call_Root | Call_Rotl | Call_Rotr => Count = 2,
         when others => Count = 1);
   --  The operation a call with Count arguments is.
   function Operation_For (Item : Callable; Count : Argument_Count) return Operation is
     (case Item is
         when Call_Abs => Absolute, when Call_Floor => Floor, when Call_Ceil => Ceiling,
         when Call_Round => (if Count = 1 then Round else Round_To), when Call_Trunc => Truncate,
         when Call_Sqrt => Square_Root, when Call_Num => Numerator_Of, when Call_Den => Denominator_Of,
         when Call_Sin => Sine, when Call_Cos => Cosine, when Call_Tan => Tangent, when Call_Asin => Arcsine,
         when Call_Acos => Arccosine, when Call_Atan => Arctangent, when Call_Ln => Natural_Log,
         when Call_Log => Common_Log, when Call_Exp => Exponential, when Call_Gcd => Greatest_Common_Divisor,
         when Call_Lcm => Least_Common_Multiple, when Call_Root => Root, when Call_Rotl => Rotate_Left,
         when Call_Rotr => Rotate_Right)
     with Post => (if Count = 1 then Operation_For'Result in Unary_Operation
                   else Operation_For'Result in Binary_Operation)
                  or else not Takes (Item, Count);
   function Call_Name (Item : Callable) return String is
     (Name (Operation_For (Item, (if Takes (Item, 1) then 1 else 2))));

   type Entry_Kind is (Open_Group, Prefix, Infix);
   type Stack_Entry is record
      Kind : Entry_Kind := Open_Group;
      Position : Column := 1;
      --  Prefix: Negate or Bit_Not; Infix: a binary operation.
      Operator : Operation := Add;
      --  An Open_Group of a function call, its arguments so far.
      Is_Call : Boolean := False;
      Call : Callable := Call_Abs;
      Arguments : Natural := 1;
   end record;
   type Stack_Array is array (Positive range <>) of Stack_Entry;

   --  Operator precedence: | < xor < & < << >> < + - < * / mod < prefix - ~ < ^.
   type Precedence is range 1 .. 8;
   PREFIX_PRECEDENCE : constant Precedence := 7;
   function Precedence_Of (Item : Operation) return Precedence is
     (case Item is
         when Bit_Or => 1,
         when Bit_Xor => 2,
         when Bit_And => 3,
         when Shift_Left | Shift_Right => 4,
         when Add | Subtract => 5,
         when Power => 8,
         when others => 6);
   function Right_Associative (Item : Operation) return Boolean is (Item = Power);
   function Level_Of (Item : Stack_Entry) return Precedence is
     (if Item.Kind = Prefix then PREFIX_PRECEDENCE else Precedence_Of (Item.Operator));

   function Is_Digit (C : Character) return Boolean is (C in '0' .. '9');
   function Is_Letter (C : Character) return Boolean is (C in 'a' .. 'z' | 'A' .. 'Z');
   function Is_Space (C : Character) return Boolean is (C = ' ' or else C = ASCII.HT);
   function Lower (C : Character) return Character is
     (if C in 'A' .. 'Z' then Character'Val (Character'Pos (C) - Character'Pos ('A') + Character'Pos ('a')) else C);

   --  Text (First .. Last) spells Name, ignoring case.
   function Spells (Text : String; First, Last : Positive; Name : String) return Boolean is
     (Last - First + 1 = Name'Length
      and then (for all K in 0 .. Name'Length - 1 => Lower (Text (First + K)) = Name (Name'First + K)))
     with Pre => First in Text'Range and then Last in Text'Range and then First <= Last;

   DECIMAL_BASE : constant := 10;
   HEXADECIMAL : constant Literal_Base := 16;
   OCTAL : constant Literal_Base := 8;
   BINARY_BASE : constant Literal_Base := 2;
   --  The base a prefix letter (0x, 0o, 0b) names, or 10 for none.
   function Prefix_Base (C : Character) return Literal_Base is
     (case C is when 'x' | 'X' => HEXADECIMAL, when 'o' | 'O' => OCTAL, when 'b' | 'B' => BINARY_BASE,
                when others => DECIMAL);

   procedure Parse
     (Text : String; Terms : out Term_Array; Count : out Natural;
      Error : out Syntax_Error; Error_Position : out Column)
   is
      Stack : Stack_Array (1 .. Text'Length);
      Depth : Natural := 0;
      I : Positive := 1;
      Expect_Operand : Boolean := True;

      procedure Fail (Kind : Syntax_Error; Where : Column) is
      begin
         Error := Kind;
         Error_Position := Where;
      end Fail;

      --  Pop the stack's top operator into the terms.
      procedure Emit_Top
        with Pre => Terms'First = 1 and then Depth in 1 .. Stack'Last and then Count < Terms'Last
                    and then Stack (Depth).Position in Text'Range
                    and then Valid_Terms (Text, Terms (1 .. Count)),
             Post => Count = Count'Old + 1 and then Depth = Depth'Old - 1
                     and then Valid_Terms (Text, Terms (1 .. Count))
      is
         Top : constant Stack_Entry := Stack (Depth);
      begin
         Count := Count + 1;
         if Top.Kind = Prefix then
            Terms (Count) := (Kind => Unary, Position => Top.Position,
                              Operator => (if Top.Operator = Bit_Not then Bit_Not else Negate), others => <>);
         else
            Terms (Count) := (Kind => Binary, Position => Top.Position,
                              Operator => (if Top.Operator in Binary_Operation then Top.Operator else Add),
                              others => <>);
         end if;
         Depth := Depth - 1;
      end Emit_Top;
   begin
      Terms := [others => (others => <>)];
      Count := 0;
      Error := No_Error;
      Error_Position := 1;
      while I <= Text'Last and then Is_Space (Text (I)) loop
         I := I + 1;
      end loop;
      if I > Text'Last then
         Fail (Empty_Expression, 1);
         return;
      end if;
      I := 1;
      while I <= Text'Last loop
         pragma Loop_Invariant (Error = No_Error);
         pragma Loop_Invariant (Count + Depth < I);
         pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
         pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
         pragma Loop_Variant (Increases => I);
         declare
            C : constant Character := Text (I);
            Start : constant Column := I;
         begin
            if Is_Space (C) then
               I := I + 1;

            elsif Expect_Operand and then C = '0' and then I <= Text'Last - 2
              and then Prefix_Base (Text (I + 1)) /= DECIMAL
              and then Calc_Naturals.Is_Digit (Text (I + 2), Prefix_Base (Text (I + 1)))
            then
               --  A based number: 0x, 0o or 0b and its digits.
               declare
                  Base : constant Literal_Base := Prefix_Base (Text (I + 1));
                  Item : Term := (Kind => Literal, Position => Start, Base => Base, others => <>);
               begin
                  I := I + 2;
                  Item.Whole_First := I;
                  while I <= Text'Last and then Calc_Naturals.Is_Digit (Text (I), Base) loop
                     pragma Loop_Invariant
                       (I >= Item.Whole_First
                        and then (for all K in Item.Whole_First .. I - 1 => Calc_Naturals.Is_Digit (Text (K), Base)));
                     pragma Loop_Variant (Increases => I);
                     I := I + 1;
                  end loop;
                  Item.Whole_Last := I - 1;
                  Item.Fraction_First := I;
                  Item.Fraction_Last := I - 1;
                  if I <= Text'Last and then (Calc_Naturals.Is_Digit (Text (I), HEXADECIMAL) or else Text (I) = '.')
                  then
                     Fail (Malformed_Number, I);  --  0b102, 0x1.8
                     return;
                  end if;
                  Count := Count + 1;
                  Terms (Count) := Item;
                  Expect_Operand := False;
               end;

            elsif Expect_Operand and then (Is_Digit (C) or else C = '.') then
               --  A number: whole digits, a fraction, an exponent.
               declare
                  Item : Term := (Kind => Literal, Position => Start, others => <>);
                  Negative_Exponent : Boolean := False;
                  Exponent : Decimal_Exponent := 0;
               begin
                  Item.Whole_First := I;
                  while I <= Text'Last and then Is_Digit (Text (I)) loop
                     pragma Loop_Invariant (I >= Start and then (for all K in Start .. I - 1 => Is_Digit (Text (K))));
                     pragma Loop_Variant (Increases => I);
                     I := I + 1;
                  end loop;
                  Item.Whole_Last := I - 1;
                  Item.Fraction_First := I;
                  Item.Fraction_Last := I - 1;
                  if I <= Text'Last and then Text (I) = '.' then
                     I := I + 1;
                     Item.Fraction_First := I;
                     while I <= Text'Last and then Is_Digit (Text (I)) loop
                        pragma Loop_Invariant
                          (I >= Item.Fraction_First
                           and then (for all K in Item.Fraction_First .. I - 1 => Is_Digit (Text (K))));
                        pragma Loop_Variant (Increases => I);
                        I := I + 1;
                     end loop;
                     Item.Fraction_Last := I - 1;
                  end if;
                  if Item.Whole_Last < Item.Whole_First and then Item.Fraction_Last < Item.Fraction_First then
                     Fail (Malformed_Number, Start);
                     return;
                  end if;
                  --  An exponent: e or E, a sign, digits. An e without
                  --  digits is not part of the number.
                  if I < Text'Last and then (Text (I) = 'e' or else Text (I) = 'E')
                    and then (Is_Digit (Text (I + 1))
                              or else (I + 1 < Text'Last and then Text (I + 1) in '+' | '-'
                                       and then Is_Digit (Text (I + 2))))
                  then
                     I := I + 1;
                     if Text (I) in '+' | '-' then
                        Negative_Exponent := Text (I) = '-';
                        I := I + 1;
                     end if;
                     while I <= Text'Last and then Is_Digit (Text (I)) loop
                        pragma Loop_Invariant (I > Start and then Exponent in 0 .. MAXIMUM_EXPONENT);
                        pragma Loop_Variant (Increases => I);
                        --  Saturates: beyond the bound no budget holds it.
                        if Exponent <= (MAXIMUM_EXPONENT - DECIMAL_BASE) / DECIMAL_BASE then
                           Exponent := Exponent * DECIMAL_BASE
                             + Decimal_Exponent (Character'Pos (Text (I)) - Character'Pos ('0'));
                        else
                           Exponent := MAXIMUM_EXPONENT;
                        end if;
                        I := I + 1;
                     end loop;
                     Item.Exponent := (if Negative_Exponent then -Exponent else Exponent);
                  end if;
                  if I <= Text'Last and then (Is_Digit (Text (I)) or else Text (I) = '.') then
                     Fail (Malformed_Number, I);  --  1.2.3
                     return;
                  end if;
                  Count := Count + 1;
                  Terms (Count) := Item;
                  Expect_Operand := False;
               end;

            elsif Is_Letter (C) then
               --  A name: a constant, ans, a function call, or mod.
               while I <= Text'Last and then (Is_Letter (Text (I)) or else Is_Digit (Text (I))) loop
                  pragma Loop_Invariant (I >= Start);
                  pragma Loop_Variant (Increases => I);
                  I := I + 1;
               end loop;
               declare
                  Last : constant Column := I - 1;
                  Found : Boolean := False;
               begin
                  if not Expect_Operand then
                     if Spells (Text, Start, Last, Name (Modulo)) or else Spells (Text, Start, Last, Name (Bit_Xor))
                     then
                        declare
                           Keyword : constant Operation :=
                             (if Spells (Text, Start, Last, Name (Modulo)) then Modulo else Bit_Xor);
                        begin
                        while Depth > 0 and then Stack (Depth).Kind /= Open_Group
                          and then Level_Of (Stack (Depth)) >= Precedence_Of (Keyword)
                        loop
                           pragma Loop_Invariant (Count + Depth < Start);
                           pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
                           pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
                           pragma Loop_Variant (Decreases => Depth);
                           Emit_Top;
                        end loop;
                        Depth := Depth + 1;
                        Stack (Depth) := (Kind => Infix, Position => Start, Operator => Keyword, others => <>);
                        Expect_Operand := True;
                        end;
                     else
                        Fail (Expected_Operator, Start);
                        return;
                     end if;
                  elsif Spells (Text, Start, Last, PREVIOUS_RESULT_NAME) then
                     Count := Count + 1;
                     Terms (Count) := (Kind => Previous_Result, Position => Start, others => <>);
                     Expect_Operand := False;
                  else
                     for Item in Constant_Value loop
                        if Spells (Text, Start, Last, Constant_Name (Item)) then
                           Count := Count + 1;
                           Terms (Count) := (Kind => Named_Constant, Position => Start, Value => Item, others => <>);
                           Expect_Operand := False;
                           Found := True;
                           exit;
                        end if;
                     end loop;
                     if not Found then
                        for Item in Callable loop
                           if Spells (Text, Start, Last, Call_Name (Item)) then
                              --  A call: ( follows, perhaps after spaces.
                              while I <= Text'Last and then Is_Space (Text (I)) loop
                                 pragma Loop_Invariant (I > Start);
                                 pragma Loop_Variant (Increases => I);
                                 I := I + 1;
                              end loop;
                              if I > Text'Last or else Text (I) /= '(' then
                                 Fail (Expected_Operand, Start);
                                 return;
                              end if;
                              I := I + 1;
                              Depth := Depth + 1;
                              Stack (Depth) := (Kind => Open_Group, Position => Start, Is_Call => True,
                                                Call => Item, Arguments => 1, others => <>);
                              Found := True;
                              exit;
                           end if;
                        end loop;
                        if not Found then
                           Fail (Unknown_Name, Start);
                           return;
                        end if;
                     end if;
                  end if;
               end;

            elsif Expect_Operand then
               I := I + 1;
               case C is
                  when '(' =>
                     Depth := Depth + 1;
                     Stack (Depth) := (Kind => Open_Group, Position => Start, others => <>);
                  when '-' | '~' =>
                     Depth := Depth + 1;
                     Stack (Depth) := (Kind => Prefix, Position => Start,
                                       Operator => (if C = '-' then Negate else Bit_Not), others => <>);
                  when '+' =>
                     null;  --  a prefix plus changes nothing
                  when '*' | '/' | '^' | ')' | ',' | '!' | '&' | '|' | '<' | '>' =>
                     Fail (Expected_Operand, Start);
                     return;
                  when others =>
                     Fail (Unexpected_Character, Start);
                     return;
               end case;

            else
               I := I + 1;
               case C is
                  when '+' | '-' | '*' | '/' | '^' | '&' | '|' | '<' | '>' =>
                     if C in '<' | '>' then
                        --  << and >>: the second character must follow.
                        if I > Text'Last or else Text (I) /= C then
                           Fail (Unexpected_Character, Start);
                           return;
                        end if;
                        I := I + 1;
                     end if;
                     declare
                        Operator : constant Binary_Operation :=
                          (case C is
                              when '+' => Add, when '-' => Subtract, when '*' => Multiply,
                              when '/' => Divide, when '&' => Bit_And, when '|' => Bit_Or,
                              when '<' => Shift_Left, when '>' => Shift_Right, when others => Power);
                        Level : constant Precedence := Precedence_Of (Operator);
                     begin
                        while Depth > 0 and then Stack (Depth).Kind /= Open_Group
                          and then Level_Of (Stack (Depth)) >= Level
                          and then not (Right_Associative (Operator)
                                        and then Stack (Depth).Kind = Infix
                                        and then Precedence_Of (Stack (Depth).Operator) = Level)
                        loop
                           pragma Loop_Invariant (Count + Depth < Start);
                           pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
                           pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
                           pragma Loop_Variant (Decreases => Depth);
                           Emit_Top;
                        end loop;
                        Depth := Depth + 1;
                        Stack (Depth) := (Kind => Infix, Position => Start, Operator => Operator, others => <>);
                        Expect_Operand := True;
                     end;
                  when '!' =>
                     Count := Count + 1;
                     Terms (Count) := (Kind => Unary, Position => Start, Operator => Factorial, others => <>);
                  when ')' | ',' =>
                     while Depth > 0 and then Stack (Depth).Kind /= Open_Group loop
                        pragma Loop_Invariant (Count + Depth < Start);
                        pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
                        pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
                        pragma Loop_Variant (Decreases => Depth);
                        Emit_Top;
                     end loop;
                     if Depth = 0 then
                        Fail ((if C = ')' then Unopened_Parenthesis else Misplaced_Comma), Start);
                        return;
                     end if;
                     declare
                        Group : constant Stack_Entry := Stack (Depth);
                     begin
                        if C = ',' then
                           if not Group.Is_Call or else Group.Arguments >= MAXIMUM_ARGUMENTS then
                              Fail ((if Group.Is_Call then Wrong_Argument_Count else Misplaced_Comma), Start);
                              return;
                           end if;
                           Stack (Depth).Arguments := Group.Arguments + 1;
                           Expect_Operand := True;
                        else
                           Depth := Depth - 1;
                           if Group.Is_Call then
                              if Group.Arguments not in Argument_Count
                                or else not Takes (Group.Call, Group.Arguments)
                              then
                                 Fail (Wrong_Argument_Count, Group.Position);
                                 return;
                              end if;
                              Count := Count + 1;
                              Terms (Count) :=
                                (Kind => (if Group.Arguments = 1 then Unary else Binary), Position => Group.Position,
                                 Operator => Operation_For (Group.Call, Group.Arguments), others => <>);
                           end if;
                        end if;
                     end;
                  when others =>
                     Fail ((if Is_Digit (C) or else C = '(' or else C = '.' then Expected_Operator
                            else Unexpected_Character), Start);
                     return;
               end case;
            end if;
         end;
      end loop;
      if Expect_Operand or else Count = 0 then
         --  (Count = 0 never: an operand was read last.)
         Fail (Expected_Operand, Text'Last);
         return;
      end if;
      while Depth > 0 loop
         pragma Loop_Invariant (Count + Depth <= Text'Length);
         pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
         pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
         pragma Loop_Variant (Decreases => Depth);
         if Stack (Depth).Kind = Open_Group then
            Fail (Unclosed_Parenthesis, Stack (Depth).Position);
            return;
         end if;
         Emit_Top;
      end loop;
   end Parse;
end Calc_Syntax;
