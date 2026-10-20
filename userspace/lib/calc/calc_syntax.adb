with Interfaces; use Interfaces;
package body Calc_Syntax with SPARK_Mode is
   type Function_Kind is
     (Absolute_Function, Floor_Function, Ceiling_Function, Round_Function, Truncate_Function,
      Root_Function, Gcd_Function, Lcm_Function, Numerator_Function, Denominator_Function);
   function Function_Name (Item : Function_Kind) return String is
     (case Item is
         when Absolute_Function => Unary_Name (Absolute), when Floor_Function => Unary_Name (Floor),
         when Ceiling_Function => Unary_Name (Ceiling), when Round_Function => Unary_Name (Round),
         when Truncate_Function => Unary_Name (Truncate), when Root_Function => Unary_Name (Square_Root),
         when Gcd_Function => Binary_Name (Greatest_Common_Divisor),
         when Lcm_Function => Binary_Name (Least_Common_Multiple),
         when Numerator_Function => Unary_Name (Numerator_Of),
         when Denominator_Function => Unary_Name (Denominator_Of));
   MAXIMUM_ARGUMENTS : constant := 2;
   subtype Argument_Count is Positive range 1 .. MAXIMUM_ARGUMENTS;
   --  Which argument counts a function takes.
   function Takes (Item : Function_Kind; Count : Argument_Count) return Boolean is
     (case Item is
         when Round_Function => True,
         when Gcd_Function | Lcm_Function => Count = 2,
         when others => Count = 1);

   type Entry_Kind is (Open_Group, Prefix_Minus, Infix);
   type Stack_Entry is record
      Kind : Entry_Kind := Open_Group;
      Position : Column := 1;
      Operation : Binary_Operation := Add;
      --  An Open_Group of a function call, its arguments so far.
      Is_Call : Boolean := False;
      Call : Function_Kind := Absolute_Function;
      Arguments : Natural := 1;
   end record;
   type Stack_Array is array (Positive range <>) of Stack_Entry;

   --  Operator precedence: + - < * / mod < prefix - < ^.
   type Precedence is range 1 .. 4;
   PREFIX_PRECEDENCE : constant Precedence := 3;
   function Precedence_Of (Item : Binary_Operation) return Precedence is
     (case Item is
         when Add | Subtract => 1,
         when Multiply | Divide | Modulo => 2,
         when Power => 4,
         when others => 2);
   function Right_Associative (Item : Binary_Operation) return Boolean is (Item = Power);

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
         if Top.Kind = Prefix_Minus then
            Terms (Count) := (Kind => Unary, Position => Top.Position, Unary_Operation => Negate, others => <>);
         else
            Terms (Count) := (Kind => Binary, Position => Top.Position, Binary_Operation => Top.Operation,
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
                     if Spells (Text, Start, Last, Binary_Name (Modulo)) then
                        while Depth > 0 and then Stack (Depth).Kind /= Open_Group
                          and then (if Stack (Depth).Kind = Prefix_Minus then PREFIX_PRECEDENCE
                                    else Precedence_Of (Stack (Depth).Operation)) >= Precedence_Of (Modulo)
                        loop
                           pragma Loop_Invariant (Count + Depth < Start);
                           pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
                           pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
                           pragma Loop_Variant (Decreases => Depth);
                           Emit_Top;
                        end loop;
                        Depth := Depth + 1;
                        Stack (Depth) := (Kind => Infix, Position => Start, Operation => Modulo, others => <>);
                        Expect_Operand := True;
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
                        for Item in Function_Kind loop
                           if Spells (Text, Start, Last, Function_Name (Item)) then
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
                  when '-' =>
                     Depth := Depth + 1;
                     Stack (Depth) := (Kind => Prefix_Minus, Position => Start, others => <>);
                  when '+' =>
                     null;  --  a prefix plus changes nothing
                  when '*' | '/' | '^' | ')' | ',' | '!' =>
                     Fail (Expected_Operand, Start);
                     return;
                  when others =>
                     Fail (Unexpected_Character, Start);
                     return;
               end case;

            else
               I := I + 1;
               case C is
                  when '+' | '-' | '*' | '/' | '^' =>
                     declare
                        Operation : constant Binary_Operation :=
                          (case C is
                              when '+' => Add, when '-' => Subtract, when '*' => Multiply,
                              when '/' => Divide, when others => Power);
                        Level : constant Precedence := Precedence_Of (Operation);
                     begin
                        while Depth > 0 and then Stack (Depth).Kind /= Open_Group
                          and then (if Stack (Depth).Kind = Prefix_Minus then PREFIX_PRECEDENCE
                                    else Precedence_Of (Stack (Depth).Operation)) >= Level
                          and then not (Right_Associative (Operation)
                                        and then Stack (Depth).Kind = Infix
                                        and then Precedence_Of (Stack (Depth).Operation) = Level)
                        loop
                           pragma Loop_Invariant (Count + Depth < Start);
                           pragma Loop_Invariant (Valid_Terms (Text, Terms (1 .. Count)));
                           pragma Loop_Invariant (for all K in 1 .. Depth => Stack (K).Position in Text'Range);
                           pragma Loop_Variant (Decreases => Depth);
                           Emit_Top;
                        end loop;
                        Depth := Depth + 1;
                        Stack (Depth) := (Kind => Infix, Position => Start, Operation => Operation, others => <>);
                        Expect_Operand := True;
                     end;
                  when '!' =>
                     Count := Count + 1;
                     Terms (Count) := (Kind => Unary, Position => Start, Unary_Operation => Factorial, others => <>);
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
                              if Group.Arguments = 1 then
                                 Terms (Count) :=
                                   (Kind => Unary, Position => Group.Position,
                                    Unary_Operation =>
                                      (case Group.Call is
                                          when Absolute_Function => Absolute, when Floor_Function => Floor,
                                          when Ceiling_Function => Ceiling, when Round_Function => Round,
                                          when Truncate_Function => Truncate, when Root_Function => Square_Root,
                                          when Numerator_Function => Numerator_Of,
                                          when others => Denominator_Of),
                                    others => <>);
                              else
                                 Terms (Count) :=
                                   (Kind => Binary, Position => Group.Position,
                                    Binary_Operation =>
                                      (case Group.Call is
                                          when Gcd_Function => Greatest_Common_Divisor,
                                          when Lcm_Function => Least_Common_Multiple,
                                          when others => Round_To),
                                    others => <>);
                              end if;
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
